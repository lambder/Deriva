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
  [oper-symbol diff-rule]
  (swap! rules-of-differentiation assoc oper-symbol diff-rule))


(defn normalize-expression [expression]
  (clojure.walk/postwalk-replace
    '{add +, mul *, sub -, div /, eq =, gt >, lt <, d ∂, dd ∂∂, ddd ∂∂∂}
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
  "
  [oper [& args] symbol-in-respect-to body]
  (let [t (gensym symbol-in-respect-to)
        gensyms (map gensym args)]
    `(let
       [~'the-fun (fn
                    [~t ~@gensyms]
                    `~(clojure.walk/postwalk-replace
                        {'~symbol-in-respect-to ~t}
                        (clojure.walk/postwalk-replace (zipmap '~args (list ~@gensyms)) '~body))
                    )]
       (add-diff-rule (quote ~oper) ~'the-fun)
       )))

; `(add-diff-rule
;    ;TODO auto generate latex equasion e.g. : \dfrac {\partial \sin \left( x\right) } {\partial t}=\cos \left( x\right) .\dfrac {\partial x} {\partial t}
;    (quote ~oper)
;    (fn
;      [~@gensyms ~t]
;      `~(clojure.walk/postwalk-replace
;          {'~symbol-in-respect-to ~t}
;          (clojure.walk/postwalk-replace (zipmap '~args (list ~@gensyms)) '~body))
;      )))))


(add-diff-rule 'external (fn [symbol-in-respect-to external-function-instance & args]
                           (->> args
                                (cons symbol-in-respect-to)
                                (cons external-function-instance)
                                (cons 'external-∂))))

(add-diff-rule 'vector (fn [symbol-in-respect-to & exprs]
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

(def-∂ / [a b] t
  (/
    (-
      (* (∂ a t) b)
      (* (∂ b t) a))
    (* b b)))

(def-∂ + [a b] t
  (+ (∂ a t) (∂ b t)))

(def-∂ - [a b] t
  (- (∂ a t) (∂ b t)))

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
                            f (@rules-of-differentiation oper)
                            _ (if (nil? f) (throw (Exception. (str "Symbol " oper " is not associated with any operation."))))
                            ;_ (println (str "derive-1 " (cons symbol-in-respect-to args)))
                            ]
                        
                        (apply f (cons symbol-in-respect-to args)))
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
              
              
              
              (and 
                (seq? node) 
                (re-matches #"^∂(∂+)$" (str (first node))))  #_> (let [[_ match] (re-find #"^∂(∂+)$" (str (first node)))]  
                                                                         (recur (zip/replace loc 
                                                                                             (derive-1 (list (first node) (derive-all (list (symbol match) (second node) (last node))) (last node))))))
              
              
              (and 
                (seq? node) 
                (= '∂ (first node)))  #_> (recur (zip/replace loc 
                                                                  (derive-1 (list (first node) (derive-all (second node)) (last node)))))
              
              :else (recur (zip/next loc)))          
            ))))))


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


(defmacro def-operation-expansion [symb [& args] expansion]  
  (assert-args
    (symbol? symb) "a symol for name")
  `(swap! operation-expansions assoc
          (quote ~symb)
          (clojure.core/fn [~@args] (clojure.walk/postwalk-replace ~(dissoc (zipmap (map #(list 'quote %) args) args) (list 'quote '&)) (quote ~expansion)))
          ))

(def-operation-expansion sin  [z]     (java.lang.Math/sin z))
(def-operation-expansion cos  [x]     (java.lang.Math/cos x))
(def-operation-expansion +    [x y]   (clojure.core/+ x y))
(def-operation-expansion -    [x y]   (clojure.core/- x y))
(def-operation-expansion *    [x y]   (clojure.core/* x y))
(def-operation-expansion /    [x y]   (clojure.core// x y))
(def-operation-expansion exp  [x]     (Math/exp x))
(def-operation-expansion pow  [x y]   (Math/pow x y))

(def-operation-expansion neg  [x]     (clojure.core/* x -1))
(def-operation-expansion rem  [x y]   (clojure.core/rem x y))
(def-operation-expansion sq   [x]     (clojure.core/* x x))
(def-operation-expansion sqrt [x]     (Math/sqrt x))
(def-operation-expansion ln   [x]     (Math/log x))
(def-operation-expansion log  [x b]   (/ (Math/log x) (Math/log b)))

(def-operation-expansion when [x a b] (if x a b))
(def-operation-expansion not  [a]     (clojure.core/not a))
(def-operation-expansion and  [a b]   (clojure.core/and a b))
(def-operation-expansion or   [a b]   (clojure.core/or a b))
(def-operation-expansion eq   [a b]   (clojure.core/== a b))
(def-operation-expansion gt   [x y]   (> x y))
(def-operation-expansion lt   [x y]   (< x y))
(def-operation-expansion max  [a b]   (clojure.core/max a b))
(def-operation-expansion min  [a b]   (clojure.core/min a b))
(def-operation-expansion abs  [a]     (Math/abs a))

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
             (pos? array-type) `(create-array ~array-type ~args)   ;~(double-array-type array-type)
             :else             `(create-array java.lang.Object ~args)))))           



; TODO implement other functions such as   tan sec csc cot arcsin arccos arctan arcsec arccsc sinh cosh .....

(declare expression-comparator)

(defn expression-comparator [exp1 exp2]
  (cond 
    (and (symbol? exp1) (symbol? exp2))  (compare exp1 exp2)
    (and (number? exp1) (number? exp2))  (compare exp1 exp2)
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
      (if (= 0 comp-opers) ;; operators are equal
        (cond
          (= oper1 '+)    (let [
                               sorted-args1 (sort-by identity expression-comparator args1)
                               sorted-args2 (sort-by identity expression-comparator args2)
                               args-comparision (remove zero? (map expression-comparator sorted-args1 sorted-args2))                     
                               ]
                           (if (and (= (count args1) (count args2)) (empty? args-comparision))
                             0
                             (first args-comparision)))
          
          (= oper1 '*)    (let [
                               sorted-args1 (sort-by identity expression-comparator args1)
                               sorted-args2 (sort-by identity expression-comparator args2)
                               args-comparision (remove zero? (map expression-comparator sorted-args1 sorted-args2))                     
                               ]
                           (if (and (= (count args1) (count args2)) (empty? args-comparision))
                             0
                             (first args-comparision)))
          
          :else           (let [args-comparision (remove zero? (map expression-comparator args1 args2))]
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
    :else (throw  (Exception. (str (class exp1) " or " (class exp2) " not expected in expression.")))))




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
                               [(index form) ;; variable name
                                {:oper oper :args (map #(or (index %) %) args)}])
                       args))
       :else (append steps [(index form) {:oper (with-meta 'ident {:type 'double}), :args [form]}])
       
       ))))

(defn build-expression 
  "takes variable symbol and computation step object 
  and creates vector which is then used in let binding.
  E.g.  [VAR_SYM_33 {:oper +, :args (x y)}]   = becomes =>   [VAR_SYM_33 (+ x y)]
  "
  [[v step]]
  [v `(~(:oper step) ~@(:args step))])




(defn array-type-index [function-form] 
  (reduce 
    (fn [idx [s [op & ex] :as step]]  
      (let [e-types (map idx ex)
            e-same (apply = e-types)
            e-type (first e-types)]
        (if (and e-same (or (nil? e-type) (not (clojure.core/neg? e-type))))  ; nil - not processed yet, pos? - not flaged as -1 meaning different types
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
        [(mapcat #(vector %1 (list 'aget arr %2)) args (range))
         (expand-all-with @operation-expansions function-form)
         ;function-form
         terminator]
        ))))



(defn create-function
  "Takes sequence of symbols and an expression.
  It then build a function based on the expression
  and which arguments are maped to provided symbols."
  [[& args] expression]
  (let [expression (normalize-expression expression)
        [arr-destr steps terminator] (create-function-form (gensym) args expression)]         
    (eval        
      `(fn [~@args]
         (let [~@steps]
           ~terminator)))))
  