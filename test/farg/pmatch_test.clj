(ns farg.pmatch-test
  (:require [clojure.test :refer :all]
            [farg.pmatch :refer :all]))
  
(defn pm [x]
  (pmatch x
    ()
      'one
    ((-- ~expr) ~@more)
      (list 'two expr more)
    ((repeat ~n ~expr) ~@more)
      (list 'three n expr more)
    (bind ~v ~n)
      (list 'bound v n)
    (~f ~@r)
      (list 'cons f r)
    ~any
      (list 'catch-all any)))

(deftest test-pmatch
  (is (= 'one
         (pm ())))
  (is (= '(two a (b c))
         (pm '((-- a) b c))))
  (is (= '(three 5 expr (more))
         (pm '((repeat 5 expr) more))))
  (is (= '(bound x 4)
         (pm '(bind x 4))))
  (is (= '(cons a (b c d))
         (pm '(a b c d))))
  (is (= '(cons a ())
         (pm '(a))))
  (is (= '(catch-all xyz)
         (pm 'xyz))))

(defn pml [x]
  (pmatch-loop [x x]
    ()
      'one
    ((-- ~expr) ~@more)
      (list 'two expr more)
    ((repeat ~n ~expr) ~@more)
      (list 'three n expr more)
    (bind ~v ~n)
      (list 'bound v n)
    (~f ~@r)
      (list 'cons f r)
    ~any
      (list 'catch-all any)))

(deftest test-pmatch-loop-without-recur
  (is (= 'one
         (pml ())))
  (is (= '(two a (b c))
         (pml '((-- a) b c))))
  (is (= '(three 5 expr (more))
         (pml '((repeat 5 expr) more))))
  (is (= '(bound x 4)
         (pm '(bind x 4))))
  (is (= '(cons a (b c d))
         (pml '(a b c d))))
  (is (= '(cons a ())
         (pml '(a))))
  (is (= '(catch-all xyz)
         (pml 'xyz))))

(deftest test-pmatch-loop
  (is (= 22 (pmatch-loop [input '(:value 22)]
              (:value ~n) n)))
  (is (= 9 (pmatch-loop [input '((:value 2) (:value 3) (:value 4))
                         sum 0]
             ()
               sum
             ((:value ~n) ~@more)
               (pmatch-recur more (+ sum n))))))

(deftest test-grab-inner-outer-lists
  (is (= '[(a b c) (d e)]
         (pmatch '((a b c) d e)
           ((~@inner) ~@outer)
             [inner outer])))
  (is (= '[(a b c) (d e)]
         (pmatch-loop [input '((a b c) d e)]
           ((~@inner) ~@outer)
             [inner outer]))))

(deftest test-guard
  (let [f (fn [x]
            (pmatch x
              2 (guard (= x 3)) :never
              3 (guard (= x 3)) :three
              ~k (guard (keyword? k)) [:keyword k]
              ~n (guard (number? n)) [:number n]
              (~@r) (guard (= '(:rest1 :rest2) r)) [:r r]
              (~f ~@r) (guard (= '(:rest) r)) [:fr f r]
              (~a ~b) (guard (keyword? a)) [:pk a b]
              (~a ~b) [:pair a b]
              ~any [:any any]))]
    (is (= [:number 2] (f 2)))
    (is (= :three (f 3)))
    (is (= [:keyword :k] (f :k)))
    (is (= [:number 5] (f 5)))
    (is (= [:pk :x 'y] (f [:x 'y])))
    (is (= [:pair 'x 'y] (f ['x 'y])))
    (is (= [:r '(:rest1 :rest2)] (f '(:rest1 :rest2))))
    (is (= [:fr :f '(:rest)] (f '(:f :rest))))))

(defn docf [x]
  (pmatch x 
    (literal1 ~a ~b ~@more)
      [a b more]
    (~f 25) (guard (not= f 'xyz))
      (str f " is passed 25")
    (xyz ~n)
      (str "Last clause: xyz is passed " n)))

(deftest test-docstrings
  (is (= (docf '(literal1 100 200 300 400 500))
         '[100 200 (300 400 500)]))
  (is (= (docf '(fname 25))
         "fname is passed 25"))
  (is (= (docf '(xyz 25))
         "Last clause: xyz is passed 25"))
  (is (= (pmatch-loop [input '((tag1 1 2 3) (tag2 20 30) (tag1 10))
                       totals {}]
           ()
             totals
           ((~tag ~@numbers) ~@more)
             (pmatch-recur more (update totals tag
                                        (fnil #(apply + % numbers) 0))))
         '{tag1 16, tag2 50})))
