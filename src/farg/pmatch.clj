(ns farg.pmatch
  "pmatch, based on Oleg Kiselyov's original in Scheme

  Unlike clojure.core.match, pmatch makes it easy to parse Clojure code.
  pmatch is very useful in macros.

  This version of pmatch has none of the original's elegance. However, unlike
  the original, this version generates nearly minimal code. A straight
  translation of the original macro produced so much code that it was
  generating more than 64K of JVM bytecode for even fairly small sets of
  clauses.")

(defmacro loop-
  "Non-tail-recursive loop/recur. Call recur- inside the loop to recur."
  [bindings & body]
  (let [bindings (partition 2 bindings)
        names (vec (map first bindings))
        init-values (map second bindings)]
    `(letfn [(loop-# ~names (let [~'recur- loop-#] ~@body))]
       (loop-# ~@init-values))))

(defn- wrap-maybe-list [known-list? v then else]
  (if known-list?
      `~then
      `(if (sequential? ~v)
           ~then
           ~else)))

(defn unquote? [x]
  (and (sequential? x)
       (= 'clojure.core/unquote (first x))))

(defn unquote-splicing? [x]
  (and (sequential? x)
       (= 'clojure.core/unquote-splicing (first x))))

(defn- sym [unquote-form]
  (second unquote-form))

(defn- pat-type [pat]
  (cond
    (or (not (sequential? pat))
        (empty? pat))           :literal
    (unquote? pat)              :unq
    (unquote-splicing? pat)     :unqs
    :else                       :list))
;NICE Distinguishing between vectors and lists would go in place of that :else
;clause.

(defn- pmatch-line [target {:keys [pattern guard result]} fk]
  (loop- [[known-list? tar pat sk] [false target pattern nil]]
    (case (pat-type pat)
      :literal
        `(if (and (= '~pat ~tar) ~@guard)
             ~(if sk (recur- sk) result)
             ~fk)
      :unq
        `(let [~(sym pat) ~tar]
           (if (and ~@guard)
             ~(if sk (recur- sk) result)
             ~fk))
      :unqs
        (throw (IllegalArgumentException. (str "pmatch error: "
          "attempted to match unquoted-splicing form " pat
          " outside of a list")))
      :list
        (wrap-maybe-list known-list? tar
          (let [patf (first pat), patr (rest pat)
                tarf (gensym "first"), tarr (gensym "rest")]
            (case (pat-type patf)
              :unq
                (if (and (= 1 (count patr))
                         (= :unqs (pat-type (first patr))))
                    `(let [~(sym patf) (first ~tar)
                           ~(sym (first patr)) (rest ~tar)]
                       (if (and ~@guard)
                         ~(if sk (recur- sk) result)
                         ~fk))
                    `(let [~(sym patf) (first ~tar)
                           ~tarr       (rest ~tar)]
                       ~(recur- [true tarr patr sk])))
              :unqs
                (if (empty? patr)
                    `(let [~(sym patf) ~tar]
                       (if (and ~@guard)
                         ~(if sk (recur- sk) result)
                         ~fk))
                    (throw (IllegalArgumentException. (str "pmatch error: "
                      "unquoted-splicing form " patf " is followed by more "
                      "forms to match in the same list"))))
              `(let [~tarf (first ~tar), ~tarr (rest ~tar)]
                 ~(recur- [false tarf patf
                           [true tarr patr sk]]))))
          fk))))

(defn partition-lines
 ([lines]
  (partition-lines [] lines))
 ([line-maps lines]
  (let [[e1 e2 e3 & more] lines]
    (cond
      (empty? lines)
        line-maps
      (and (list? e2) (= 'guard (first e2)))
        (recur (conj line-maps (if (empty? (rest e2))
                                 {:pattern e1 :result e3}
                                 {:pattern e1 :guard (rest e2) :result e3}))
               more)
      (empty? (next lines))
        (throw (IllegalArgumentException. (str
          "pmatch: need result for pattern " e1)))
      :else
        (recur (conj line-maps {:pattern e1 :result e2})
               (drop 2 lines))))))

(defmacro pmatch [e & clauses]
  (let [target (gensym "target")
        clauses (partition-lines clauses)
        matchers
          (mapcat (fn [n clause]
                    [n (pmatch-line target clause `(recur ~(inc n)))])
                  (range)
                  clauses)]
    `(let [~target ~e]
       (loop [clause-num# 0]
         (case clause-num#
           ~@matchers
           ~(count clauses) (throw (IllegalArgumentException. (str
                              "pmatch failed to match " ~target))))))))

(defmacro pmatch-recur [& args] `(recur 0 ~@args))

(defmacro pmatch-loop [bindings & clauses]
  (when (not (vector? bindings))
        (throw (IllegalArgumentException.
          "The first argument to pmatch-loop must be a vector of bindings.")))
  (when (not (even? (count bindings)))
        (throw (IllegalArgumentException. (str
          "The first argument to pmatch-loop must be a binding vector "
          "containing an even number of expressions."))))
  (when (not (even? (count clauses)))
        (throw (IllegalArgumentException. (str
          "pmatch-loop requires an even number of clause expressions, "
          "coming in pairs each containing a pattern followed by a result."))))
  (let [target (first bindings)
        non-targets (map first (->> bindings (partition 2) rest))
        clauses (partition-lines clauses)
        matchers
          (mapcat (fn [n clause]
                    [n (pmatch-line target clause
                                    `(recur ~(inc n)
                                            ~target ~@non-targets))])
                  (range)
                  clauses)]
    `(loop [clause-num# 0, ~@bindings]
       (case clause-num#
         ~@matchers
         ~(count clauses) (throw (IllegalArgumentException. (str
                            "pmatch-loop failed to match " ~target)))))))
                                     ;TODO add line number

; TODO pmatch-recur really should be a local macro (defined only inside the
; loop generated by pmatch-loop).

; TODO Report apparent bug in clojure.tools.macro/macrolet
;
; An empty-list literal, (), in the generated code, appears to throw it off.
;
; (trace-ns 'clojure.tools.macro)
;
;TRACE t8247: | | | | | | | | | | | | | | (clojure.tools.macro/expand-args (if (clojure.core/= () elems) dock-map (recur 1 elems dock-map)) 1)
;TRACE t8248: | | | | | | | | | | | | | | | (clojure.tools.macro/expand-all (clojure.core/= () elems))
;TRACE t8249: | | | | | | | | | | | | | | | | (clojure.tools.macro/expand (clojure.core/= () elems))
;TRACE t8250: | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-1 (clojure.core/= () elems))
;TRACE t8251: | | | | | | | | | | | | | | | | | | (clojure.tools.macro/protected? clojure.core/=)
;TRACE t8251: | | | | | | | | | | | | | | | | | | => false
;TRACE t8252: | | | | | | | | | | | | | | | | | | (clojure.tools.macro/protected? clojure.core/=)
;TRACE t8252: | | | | | | | | | | | | | | | | | | => false
;TRACE t8253: | | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-symbol clojure.core/=)
;TRACE t8254: | | | | | | | | | | | | | | | | | | | (clojure.tools.macro/protected? clojure.core/=)
;TRACE t8254: | | | | | | | | | | | | | | | | | | | => false
;TRACE t8253: | | | | | | | | | | | | | | | | | | => clojure.core/=
;TRACE t8250: | | | | | | | | | | | | | | | | | => (clojure.core/= () elems)
;TRACE t8249: | | | | | | | | | | | | | | | | => (clojure.core/= () elems)
;TRACE t8255: | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-list (clojure.core/= () elems))
;TRACE t8256: | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-args (clojure.core/= () elems))
;TRACE t8257: | | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-args (clojure.core/= () elems) 1)
;TRACE t8258: | | | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-all ())
;TRACE t8259: | | | | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand ())
;TRACE t8260: | | | | | | | | | | | | | | | | | | | | | (clojure.tools.macro/expand-1 ())
;TRACE t8261: | | | | | | | | | | | | | | | | | | | | | | (clojure.tools.macro/protected? nil)
;:error-while-loading fargish.spec
;
;Error refreshing environment: java.lang.StringIndexOutOfBoundsException: String index out of range: 1, compiling:(fargish/spec.clj:71:3)
