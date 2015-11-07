;;   Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.lambder.deriva.core
  (:refer-clojure :include [=])
  (:use clojure.tools.macro com.lambder.deriva.utils)
  (:require [clojure.zip :as zip]
            [clojure.walk :refer (macroexpand-all)]))

(set! *warn-on-reflection* true)

(require 'clojure.tools.macro)
(in-ns 'clojure.tools.macro)

(defn expand-all-with [m-funs form]
  (binding [macro-fns m-funs]
    (expand-all form)))

(in-ns 'com.lambder.deriva.core)
(use 'clojure.tools.macro)

(defn expall [& args]
  (apply expand-all-with args))


;; Map of basic differentiation definitions.
;; New rules can be added or replaced with 'add-diff-rule'
(defonce rules-of-differentiation (atom {}))

(defn add-diff-rule
  "Adds or replaces differentiation rule."
  [oper-symbol arity diff-rule]
  (swap! rules-of-differentiation assoc [oper-symbol arity] diff-rule))

(defn normalize-expression [expression]
  (clojure.walk/prewalk
    (fn [node]
      (cond
        (and (seq? node) (= (first node) 'd)) (cons '∂ (rest node))
        (and (seq? node) (= (first node) 'dd)) (cons '∂∂ (rest node))
        (and (seq? node) (= (first node) 'ddd)) (cons '∂∂∂ (rest node))
        (and (seq? node) (< 3 (count node))) (let [oper (first node)
                                                   f-arg (second node)
                                                   r-args (drop 2 node)]
                                               (case oper
                                                 + `(~oper ~f-arg (~oper ~@r-args))
                                                 * `(~oper ~f-arg (~oper ~@r-args))
                                                 - (cons '- (cons f-arg (list (cons '+ r-args))))
                                                 / (cons '/ (cons f-arg (list (cons '* r-args))))
                                                 node))
        (= node 'add) '+
        (= node 'mul) '*
        (= node 'sub) '-
        (= node 'div) '/
        (= node 'eq) '=
        (= node 'gt) '>
        (= node 'lt) '<
        :else node
        ))
    expression))


;; Map of operation expansions definitions
;; Operation expansion definition defines with what code given expression should be replaced
;; E.g. expression *(sin x)*  should be replaced with *(Math/sin x)*
;; New definitions can be added or replaced with 'def-operation-expansion'
(defonce operation-expansions (atom {}))


(defn- replace-symbols [form sym1 sym2]
  "replaces all occurences of sym1 with sym2 in form"
  (clojure.walk/postwalk-replace
    {sym1 sym2}
    form))


(defmacro def-∂
  "Utility macro for adding differentiation rule.
  For example rule of differentiation for sin can be:
  
  (def-∂ sin [x] t
     (* (cos x) (∂ x t)))
     
  (def-∂ -
       ([a] t
         (- (∂ a t)))
       ([a b] t
         (- (∂ a t) (∂ b t)))) 
  "
  [oper & forms]
  (if (every? coll? forms)
    `(do
       ~@(for [[[& args] symbol-in-respect-to body] forms]
           (do
             (when (nil? body)
               (throw (Error. "body of the form must not be null"))) ;todo add more macro contract assertions
             (let [t (gensym symbol-in-respect-to)
                   gensyms (map gensym args)
                   arity (count args)]
               `(let
                  [~'the-fun (fn
                               [~t ~@gensyms]
                               (clojure.walk/postwalk-replace (merge (zipmap '~args (list ~@gensyms)) {'~symbol-in-respect-to ~t}) '~body)
                               )]
                  (add-diff-rule (quote ~oper) ~arity ~'the-fun))))))
    `(def-∂ ~oper ~forms)))



; `(add-diff-rule
;    ;TODO auto generate latex equasion e.g. : \dfrac {\partial \sin \left( x\right) } {\partial t}=\cos \left( x\right) .\dfrac {\partial x} {\partial t}
;    (quote ~oper)
;    (fn
;      [~@gensyms ~t]
;      `~(clojure.walk/postwalk-replace
;          {'~symbol-in-respect-to ~t}
;          (clojure.walk/postwalk-replace (zipmap '~args (list ~@gensyms)) '~body))
;      )))))


(add-diff-rule 'external :vararg (fn [symbol-in-respect-to external-function-instance & args]
                                   (->> args
                                        (cons symbol-in-respect-to)
                                        (cons external-function-instance)
                                        (cons 'external-∂))))

(add-diff-rule 'vector :vararg (fn [symbol-in-respect-to & exprs]
                                 (cons 'vector (map #(list '∂ % symbol-in-respect-to) exprs))))


(def-∂ when [x a b] t
       (when x (∂ a t) (∂ b t)))

(def-∂ max [x y] t
       (when (gt x y) (∂ x t) (∂ y t)))

(def-∂ min [x y] t
       (when (lt x y) (∂ x t) (∂ y t)))

(def-∂ abs [x] t
       (when (gt x (* -1 x)) (∂ x t) (∂ (* -1 x) t)))

;; \\dfrac {\\partial \\sin x} {\\partial x}=\\cos x
(def-∂ sin [x] t
       (* (cos x) (∂ x t)))

;; \\dfrac {\\partial \\cos x} {\\partial x}=\\-sin x
(def-∂ cos [x] t
       (* (* -1 (sin x)) (∂ x t)))

(def-∂ * [a b] t
       (+
         (* (∂ a t) b)
         (* (∂ b t) a)))

(add-diff-rule '* :vararg (fn [symbol-in-respect-to & exprs]
                            (list '+
                                  (list '* (list '∂ (first exprs) symbol-in-respect-to) (cons '* (rest exprs)))
                                  (list '* (list '∂ (cons '* (rest exprs)) symbol-in-respect-to) (first exprs)))))

(def-∂ / [a b] t
       (/
         (-
           (* (∂ a t) b)
           (* (∂ b t) a))
         (* b b)))


(def-∂ + [a b] t
       (+ (∂ a t) (∂ b t)))

(add-diff-rule '+ :vararg (fn [symbol-in-respect-to & exprs]
                            (cons '+ (map #(list '∂ % symbol-in-respect-to) exprs))))


(def-∂ -
       ([a] t
         (- (∂ a t)))
       ([a b] t
         (- (∂ a t) (∂ b t))))

(def-∂ neg [a] t
       (* -1 (∂ a t)))

(def-∂ pow [a b] t
       (* (pow a b)
          (+
            (*
              (∂ a t)
              (/ b a))
            (*
              (∂ b t)
              (ln a)))))

(def-∂ exp [a] t
       (* a (∂ a t)))

(def-∂ sqrt [a] t
       (*
         (* 0.5
            (/ 1 (sqrt a)))
         (∂ a t)))

(def-∂ sq [a] t
       (*
         (* 2 a)
         (∂ a t)))


(def-∂ log
       ; "TODO: add optimisations such as:  ∂(ln x)/∂x = 1/x, ∂(ln |x|)/∂x = |x|/x^2, "
       [x c] t
       (/ 1
          (* x
             (ln c))))


(def-∂ ln
       [x] t
       (*
         (/ 1 x)
         (∂ x t)))


; TODO implement tan sec csc cot arcsin arccos arctan arcsec arccsc sinh cosh .....

;; optimize EXAMPLES
; (optimize '(* 3 1 6) ) => (* 3 6)
; (optimize '(* 3 0) )   => 0
; (optimize '(* 3 1) )   => 3
; (optimize '(+ 3 0) )   => 3
; (optimize '(sin (+  1 0 2 (cos x)  (* y 0))) )        => (sin (+ 1 2 (cos x)))
; (optimize '(* 0 (sin (+  1 0 2 (cos x)  (* y 0))) ))  => 0
; (optimize
;   (derive-all
;     '(∂ (sin (+ (cos x)  (* y x))) y)))   =>     (* (cos (+ (cos x) (* y x))) x)
(defn optimize
  "removes unnecessary expressions such as '(* 1 x)' or '(+ 0 y)'"
  [expression]
  (clojure.walk/postwalk
    (fn [node]
      (if (seq? node)
        (let [[oper & args] node]
          (cond
            (= oper '+) #_:>> (let [without-zero (remove zer? args)]
                                (cond
                                  (empty? without-zero) #_:>> 0
                                  (= 1 (count without-zero)) #_:>> (first without-zero)
                                  :else #_:>> (cons oper without-zero)
                                  )
                                )

            (= oper '*) #_:>> (let [without-one (remove one? args)]
                                (cond
                                  (empty? without-one) #_:>> 1
                                  (= 1 (count without-one)) #_:>> (first without-one)
                                  (some zer? args) #_:>> 0
                                  :else #_:>> (cons oper without-one)
                                  ))

            :else #_:>> node
            ))
        node))
    expression))


;; derive-1 EXAMPLES
; (derive-1
;   '(∂ (sin (+ (cos x)  (* y x))) y)))     =>    (* (cos (+ (cos x) (* y x))) (∂ (+ (cos x) (* y x)) y))

(defn derive-1 [[∂ expression symbol-in-respect-to]]
  (cond
    (vector? expression) (list '∂ (cons 'vector expression) symbol-in-respect-to)
    (seq? expression) (let [[oper & args] expression
                            f (@rules-of-differentiation [oper (count args)])
                            f (or f (@rules-of-differentiation [oper :vararg]))
                            _ (if (nil? f) (throw (Exception. (str "Symbol " oper " with arity " (count args) " is not associated with any operation."))))
                            _ (println (str "derive-1 " (cons symbol-in-respect-to args)))
                            ]

                        (try
                          (apply f (cons symbol-in-respect-to args))
                          (catch clojure.lang.ArityException e
                            (throw (Exception. (str "function " oper " does not take " (.actual e) " parameters.  (" ∂ " " expression " " symbol-in-respect-to " )") e)))))
    :else (if (= expression symbol-in-respect-to) 1 0)))


;; derive-all EXAMPLES
; (derive-all
;   '(∂ y y))                               =>     1

; (derive-all
;   '(∂ (sin (+ (cos x)  (* y x))) y))      =>     (* (cos (+ (cos x) (* y x))) (+ (* (sin x) 0) (+ (* 1 x) (* 0 y))))


(defn derive-all [expression]
  (optimize
    (let [tree (zip/seq-zip expression)]
      (loop [loc tree]
        (if (zip/end? loc)
          (zip/root loc)
          (let [node (zip/node loc)]
            (cond
              (vector? node)              #_> (recur (zip/replace loc (cons 'vector node)))

              ;---------------------------------------------------------------------------------------------------------
              ; gradient derivative
              (and
                (seq? node)
                (> (count node) 3)
                (re-matches #"^(∂+)$" (str (first node))))

              #_____> (let [f (first node)
                            s (second node)
                            r (drop 2 node)]

                        ; example:
                        ; it transforms (∂ (+ x y) x y)  into (vector (∂ (+ x y) x) (∂ (+ x y) y))
                        ;
                        (recur (zip/replace loc (cons 'vector (map #(list f s %) r)))))
              ;---------------------------------------------------------------------------------------------------------
              ; partial multi-derivative
              (and
                (seq? node)
                (re-matches #"^∂(∂+)$" (str (first node))))

              #_____> (let [[_ match] (re-find #"^∂(∂+)$" (str (first node)))]
                        (recur (zip/replace loc
                                            (derive-1 (list (first node) (derive-all (list (symbol match) (second node) (last node))) (last node))))))
              ;---------------------------------------------------------------------------------------------------------
              ; partial single-derivative
              (and
                (seq? node)
                (= '∂ (first node)))  #_> (recur (zip/replace loc
                                                              (derive-1 (list (first node) (derive-all (second node)) (last node)))))
              ;---------------------------------------------------------------------------------------------------------
              ; some other kind of expression
              :else (recur (zip/next loc)))
            ))))))

;; partial-derivative EXAMPLES
; (partial-derivative 
;                     '(? (sin (+ (cos x)  (* y x))) y) 
;                     'x)
;
;         =>        (* (* (* -1 (sin (+ (cos x) (* y x)))) x) x)
;
(defn partial-derivative [expression variable]
  (optimize (derive-all (list '∂ expression variable))))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))


;; It translates base symbols to actual operation implementation
;
;  for example symbol +   represents clojure.core/+ function 
;              symbol sin represents java.lang.Math/sin function etc.
;
(defmacro def-operation-expansion [symb [& marcro-args] expansion]
  (assert-args
    (symbol? symb) "a symol for name")
  
  (if (and (= 2 (count marcro-args)) (= '& (first marcro-args)))
    (do
      #_(prn [">>>>>" symb marcro-args `(clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(zipmap (map #(list 'quote %) (rest marcro-args)) (map (fn [x] `(list ~x)) (rest marcro-args))) (quote ~expansion)))])
      `(swap! operation-expansions assoc
              (quote ~symb)
              (clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(zipmap (map #(list 'quote %) (rest marcro-args)) (map (fn [x] `(list ~x)) (rest marcro-args))) (quote ~expansion)))
              )
      )
    (do
      #_(prn [">>>>>>>" :normal])
      `(swap! operation-expansions assoc
              (quote ~symb)
              (clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(dissoc (zipmap (map #(list 'quote %) marcro-args) marcro-args) (list 'quote '&)) (quote ~expansion)))
              ))))


#_(defmacro def-operation-expansion [symb [& marcro-args] expansion]
  (assert-args
    (symbol? symb) "First argument to def-operation-expansion must be a symbol")
  (assert-args
    (or
      (= 0 (count (filter #{'&} marcro-args)))
      (and
        (= 1 (count (filter #{'&} marcro-args)))
        (= '& (second (reverse marcro-args))))) "Invalid parameter list")

  (let [arity (count marcro-args)]
    (cond
      (and (= 2 (count marcro-args)) (= '& (first marcro-args)))
      #_>>>>> (do
                (prn [">>>>>" symb marcro-args `(clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(zipmap (map #(list 'quote %) (rest marcro-args)) (map (fn [x] `(list ~x)) (rest marcro-args))) (quote ~expansion)))])
                `(swap! operation-expansions assoc
                        (quote ~symb)
                        (clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(zipmap (map #(list 'quote %) (rest marcro-args)) (map (fn [x] `(list ~x)) (rest marcro-args))) (quote ~expansion)))
                        ))

      (not-any? #{'&} marcro-args)
      #_>>>>> (do
                (prn [">>>>>>>" marcro-args])
                `(swap! operation-expansions assoc
                        [(quote ~symb) :variadic]
                        (clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(dissoc (zipmap (map #(list 'quote %) marcro-args) marcro-args) (list 'quote '&)) (quote ~expansion)))
                        ))

      :else
      #_>>>>> (do
                (prn [">>>>>>>" marcro-args])
                `(swap! operation-expansions assoc
                        [(quote ~symb) arity]
                        (clojure.core/fn [~@marcro-args] (clojure.walk/postwalk-replace ~(dissoc (zipmap (map #(list 'quote %) marcro-args) marcro-args) (list 'quote '&)) (quote ~expansion)))
                        ))

      )))



(def-operation-expansion sin [z] (java.lang.Math/sin z))
(def-operation-expansion cos [x] (java.lang.Math/cos x))
(def-operation-expansion + [x y] (clojure.core/+ x y))
(def-operation-expansion - [x y] (clojure.core/- x y))
(def-operation-expansion * [x y] (clojure.core/* x y))
(def-operation-expansion / [x y] (clojure.core// x y))
(def-operation-expansion exp [x] (Math/exp x))
(def-operation-expansion pow [x y] (Math/pow x y))

(def-operation-expansion neg [x] (clojure.core/* x -1))
(def-operation-expansion rem [x y] (clojure.core/rem x y))
(def-operation-expansion sq [x] (clojure.core/* x x))
(def-operation-expansion sqrt [x] (Math/sqrt x))
(def-operation-expansion ln [x] (Math/log x))
(def-operation-expansion log [x b] (/ (Math/log x) (Math/log b)))

(def-operation-expansion when [x a b] (if x a b))
(def-operation-expansion not [a] (clojure.core/not a))
(def-operation-expansion and [a b] (clojure.core/and a b))
(def-operation-expansion or [a b] (clojure.core/or a b))
(def-operation-expansion eq [a b] (clojure.core/== a b))
(def-operation-expansion gt [x y] (> x y))
(def-operation-expansion lt [x y] (< x y))
(def-operation-expansion max [a b] (clojure.core/max a b))
(def-operation-expansion min [a b] (clojure.core/min a b))
(def-operation-expansion abs [a] (Math/abs a))

(def-operation-expansion ident [x] x)

; (def-operation-expansion external [external-function-instance  args]
;   (. external-function-instance execute (vector args)))

; (def-operation-expansion external-∂ [external-function-instance t args]
;   (. external-function-instance gradient (vector [t args])))


(declare ^:dynamic array-type-idx)
(swap! operation-expansions assoc
       'vector
       (fn [& args]
         (let [array-type (array-type-idx `(~'vector ~@args))]
           (cond
             (pos? array-type) `(create-array ~array-type ~args) ;~(double-array-type array-type)
             :else `(create-array java.lang.Object ~args)))))



; TODO implement other functions such as   tan sec csc cot arcsin arccos arctan arcsec arccsc sinh cosh .....

(declare expression-comparator)

(defn expression-comparator [exp1 exp2]
  (cond
    (and (symbol? exp1) (symbol? exp2)) (compare exp1 exp2)
    (and (number? exp1) (number? exp2)) (compare exp1 exp2)
    (and (vector? exp1) (vector? exp2))
    (let [args-comparision (remove zero? (map expression-comparator exp1 exp2))]
      (if (and (= (count exp1) (count exp2)) (empty? args-comparision))
        0
        (first args-comparision)))
    (and (seq? exp1) (seq? exp2))
    (let [
          [oper1 & args1] exp1
          [oper2 & args2] exp2
          comp-opers (compare oper1 oper2)]
      (if (= 0 comp-opers)                                  ;; operators are equal
        (cond
          (= oper1 '+) (let [
                             sorted-args1 (sort-by identity expression-comparator args1)
                             sorted-args2 (sort-by identity expression-comparator args2)
                             args-comparision (remove zero? (map expression-comparator sorted-args1 sorted-args2))
                             ]
                         (if (and (= (count args1) (count args2)) (empty? args-comparision))
                           0
                           (first args-comparision)))

          (= oper1 '*) (let [
                             sorted-args1 (sort-by identity expression-comparator args1)
                             sorted-args2 (sort-by identity expression-comparator args2)
                             args-comparision (remove zero? (map expression-comparator sorted-args1 sorted-args2))
                             ]
                         (if (and (= (count args1) (count args2)) (empty? args-comparision))
                           0
                           (first args-comparision)))

          :else (let [args-comparision (remove zero? (map expression-comparator args1 args2))]
                  (if (and (= (count args1) (count args2)) (empty? args-comparision))
                    0
                    (first args-comparision))))
        comp-opers))

    (vector? exp1) 1
    (vector? exp2) -1
    (seq? exp1) 1
    (seq? exp2) -1
    (symbol? exp1) 1
    (symbol? exp2) -1
    (number? exp1) 1
    (number? exp2) -1
    :else (throw (Exception. (str (class exp1) " or " (class exp2) " not expected in expression.")))))




(defn build-index
  "builds a map of expressions to variables, in order to have duplicate expressions referenced by the same variable."
  ([form]
   (build-index form (sorted-map-by expression-comparator)))
  ([form index]
   (let [form (normalize-expression form)]
     (cond
       (vector? form) (reduce (fn [idx f] (build-index f idx)) (assoc index form (gensym)) form)
       (seq? form) (let [[oper & args] form]
                     (reduce
                       (fn [idx arg]
                         (build-index arg idx))
                       (assoc index form (gensym))
                       args))
       :else (assoc index form (gensym))))))



(defn exp2steps
  "Takes expression tree and converts it into steps of primitive oeprations.
  For example if we call it with `(+ x y (* x z))  expression, the result will be:
  
  (
  {:oper *,  :var GENERATED_SYMBOL_1,  :args (x z)}
  {:oper +,  :var GENERATED_SYMBOL_2,  :args (x y GENERATED_SYMBOL_1)}
  )
  
  "
  ([form]
   (cond
     (vector? form) (exp2steps form (build-index form))
     (seq? form) (exp2steps form (build-index form))
     :else [[(gensym) {:oper 'ident, :args [form]}]]
     ))
  ([form index]
   (exp2steps form [] index))
  ([form steps index]
   (let [form (normalize-expression form)]
     (cond
       (vector? form) (reduce (fn [st sub-form] (exp2steps sub-form st index))
                              (append steps [(index form) {:oper 'vector :args (map #(or (index %) %) form)}])
                              form)
       (seq? form) (let [[oper & args] form]
                     (reduce
                       (fn [st arg]
                         (exp2steps arg st index))
                       (append steps
                               [(index form)                ;; variable name
                                {:oper oper :args (map #(or (index %) %) args)}])
                       args))
       :else (append steps [(index form) {:oper (with-meta 'ident {:type 'double}), :args [form]}])

       ))))

(defn build-expression
  "takes variable symbol and computation step object 
  and creates vector which is then used in let binding.
  E.g.  (build-expression '[VAR_SYM_33 {:oper +, :args (x y)}])  gives:   '[VAR_SYM_33 (+ x y)]
  "
  [[v step]]
  [v `(~(:oper step) ~@(:args step))])




(defn array-type-index [function-form]
  (reduce
    (fn [idx [s [op & ex] :as step]]
      (let [e-types (map idx ex)
            e-same (apply = e-types)
            e-type (first e-types)]
        (if (and e-same (or (nil? e-type) (not (clojure.core/neg? e-type)))) ; nil - not processed yet, pos? - not flaged as -1 meaning different types
          (if e-type
            ; positive and all equal
            (if (= op 'vector)
              (assoc idx s (inc e-type))
              (assoc idx s e-type))
            ; not processed yet
            (assoc idx s 0))
          ; different types
          (assoc idx s -1))))
    {}
    (partition 2 function-form)))


(defn create-function-form
  "Takes sequence of symbols and an expression.
  It then build a function based on the expression
  and which arguments are maped to provided symbols."
  [arr [& args] expression]
  (let [expression (normalize-expression expression)
        expression (optimize (derive-all expression))
        steps (distinct (reverse (exp2steps expression)))
        terminator (first (last steps))
        operations (into [] (map build-expression steps))
        ]

    (let [function-form (vec (apply concat operations))
          array-type-index (array-type-index function-form)
          expressions-to-vars (apply hash-map (mapcat reverse (partition 2 function-form)))
          expressions-to-array-types (into {} (map (fn [[e v]] [e (array-type-index v)]) expressions-to-vars))
          ]
      (binding [array-type-idx expressions-to-array-types]
        (prn @operation-expansions)
        [(mapcat #(vector %1 (list 'aget arr %2)) args (range))
         (expand-all-with @operation-expansions function-form)
         function-form
         terminator]
        ))))



;(defn create-function
;  "Takes sequence of symbols and an expression.
;  It then build a function based on the expression
;  and which arguments are maped to provided symbols."
;  [[& args] expression]
;  (let [expression (normalize-expression expression)
;        [arr-destr steps function-form terminator] (create-function-form (gensym) args expression)]
;    (eval
;    `(fn [~@args]
;       (let [~@steps]
;         ~terminator))))
;)


(defn create-function
  "Takes sequence of symbols and an expression.
  It then build a function based on the expression
  and which arguments are maped to provided symbols.
  For example: 
  
      (def f (create-function '[x y] '(sin (+ x y))))
      (f 1 5) ;=> -0.27941549819892586
      (assert (= (f 2 3) (Math/sin 5)))  ; OK
      
      
      (def f (create-function '[x] '(∂ (sin x) x)))
      (assert (= (f 5) (Math/cos 5)))  ; OK
  "
  [[& args] expression]
  (let [expression (normalize-expression expression)
        [arr-destr steps function-form terminator] (create-function-form (gensym) args expression)]
    ;(eval
    `(let [~'function-type-tag (keyword (gensym))]
       (defmethod print-method ~'function-type-tag [~'x ^java.io.Writer ~'writer]
         (print-method
           (let [[~'args ~'body] (:view (meta ~'x))]
             (str "function: f" ~'args " = " (clojure.pprint/write ~'body :stream nil)))
           ~'writer))
       (with-meta
         (fn [~@args]
           (let [~@steps]
             ~terminator))
         {:type  ~'function-type-tag
          :steps (quote ~steps)
          :view  [(quote ~args) (quote ~expression)]}))
    ;)
    )
)




(defn error [^String msg]
  (throw (Exception. msg)))

(defmacro bind [expr-var & subs]
  "subs is a map of substitutions.
  The key is a symbol the value is expression"
  (if (zero? (mod (count subs) 2))
    (clojure.walk/postwalk-replace (apply hash-map subs) (deref (resolve expr-var)))
    (error "substitutions need to be even number of forms")))

(defmacro def-exp
  "macro defining named expressions. Expression can refer to other forms via special 'bind' operator
  
  (def-exp e1 (+ a b))                      =>  (assert (= e1 '(+ a b)))
  (def-exp e2 (+ c (bind e1 b (* x y z))))  =>  (assert (= e2 '(+ c (+ a (* x y z)))))
  
  "
  ([expr]
   `(quote ~(macroexpand-all expr)))
  ([name expr]
   `(def ~name (quote ~(macroexpand-all expr)))))


;
;    (def N '(+ a b))
;    (bind N a (- d1))                                   #_====> (+ (- d1) b)
;    (def-exp some-name (+ 234 (bind N a (- d1))))       #_====> (def some-name (+ 234 (+ (- d1) b)))  
;    


#_(
    (def a (make-array com.lambder.deriva.Expression 1))
    (aset a 0 (Deriva/sin \x))
    (def x (.function (Deriva/vector a) (char-array [\x])))
    (into [] (.execute x (double-array [3.2 2.1])))

    )



#_(

    (def-exp N
             (/ 1
                (+ 1
                   (exp (-
                          (* -0.07056 (pow x 3))
                          (* -1.5976 x))))))


    (def-exp d1 (/ (+ (ln (/ S K)) (* (+ r (/ (sq sigma) 2)) (- T t)))
                   (* sigma (sqrt t))))

    (def-exp d2 (- d1 (* sigma (sqrt t))))

    (def-exp call
             (-
               (* S (bind N x d1))
               (* (* K (bind N x d2)) (exp (- (* r (- T t)))))))

    (def-exp put
             (+
               (* K (exp (- (* r (- T t)))))
               (- S)
               (bind call)))




    (def-exp call-option-with-sensitivities-exp (∂ (bind call T 0.523) F K r t))

    (def call-option-with-sensitivities (create-function '[F K d1 d2] call-option-with-sensitivities-exp))

    )


#_(

    (def-exp exp (/ (+ (* (- (sq x) 1)    (- (sq y) 4)) (sq x) (sq y) -5) (sq (+  (sq x) (sq y) 1))))
    (def-exp ∂xy-exp (∂ (/ (+ (* (- (sq x) 1)   (- (sq y) 4)) (sq x) (sq y) -5) (sq (+  (sq x) (sq y) 1))) x y))
    (def f (create-function '[x y] ∂xy-exp))
    (meta f)
    )



; (def cl (-> (Thread/currentThread) (.getContextClassLoader)))
; (-> cl (.addURL (java.net.URL. "file:///C:/PROJECTS/Deriva/target/classes")))
; (import '(com.lambder.deriva Deriva))
; (def exp (Deriva/sin "x"))
; (def fun (.function exp (char-array [\x])))
; (.execute fun (double-array [(/ Math/PI 6)]))



; (def exp (Deriva/sin (Deriva/mul (Deriva/sq \x) (Deriva/sq \y))))
; (def deriv (Deriva/d exp "x" (into-array String ["y"])))
; (def fun (.function deriv (into-array String ["x" "y"])))
; (.execute fun (double-array [1.0 2.0]))

(defonce latex-conversions (atom {}))

(defn add-latex-conversion
  "Adds or replaces latex convertion for given symbol."
  [oper-symbol convertion]
  (swap! latex-conversions assoc oper-symbol convertion))

(add-latex-conversion '/ (fn [[a b]] (str "\\dfrac {" a "} {" b "}")))


(defn expr2latex [expression]
  (clojure.walk/postwalk
    (fn [node]
      (if (seq? node)
        (let [[oper & args] node
              conversion (@latex-conversions oper)]
          (conversion args))
        node))
    expression))
