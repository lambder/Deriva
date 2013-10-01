;;   Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.lambder.deriva.java
  (:use [com.lambder.deriva.core]
        [clojure.math.combinatorics]
        [clojure.string :only [join]]
        [clojure.pprint]
        [com.lambder.deriva.genclass]
        [com.lambder.deriva.utils])
  (:require [clojure.string :as s] [clojure.set :as set])
  (:refer-clojure :exclude [vector])
  (:import [com.lambder.deriva Expression Function]))

;;TODO delete it
(defn type-hint [type symbol]
  (with-meta symbol {:tag type}))

(defprotocol Describtable
  (describe [this]))

(defprotocol ExpressionTree
  (to-tree [this]))

(extend-type clojure.lang.Symbol
  ExpressionTree
  (to-tree [this] (symbol this)))

(extend-type Character
  ExpressionTree
  (to-tree [this] (symbol (str this))))

(extend-type String
  ExpressionTree
  (to-tree [this] (symbol this)))

(extend-type Number
  ExpressionTree
  (to-tree [this] this))

(extend-type clojure.lang.PersistentVector
  ExpressionTree
  (to-tree [this] (mapv to-tree this)))

(defn ExpressionImpl-expression-constructor [expr]
  [[]
   {:expression (clojure.walk/postwalk
                  (fn [form]
                    (to-tree form))
                  expr)}])


(defmacro expression-impl []
  `(gen-class2
     :name "com.lambder.deriva.ExpressionImpl"
     :implements ["com.lambder.deriva.Expression"]
     :init "expression-constructor"
     :constructors {[clojure.lang.Seqable] []}
     :prefix "ExpressionImpl-"
     :state ~'state
     :impl-ns com.lambder.deriva.java))

(expression-impl)

(defn ExpressionImpl-toString [this]
  (with-out-str (pprint (:expression (.state this)))))

(defn ExpressionImpl-describe [this]
  (describe (:expression (.state this))))
        
(import com.lambder.deriva.ExpressionImpl)


(defn- reify-function [this variables]
  (let [
        expression (:expression (.state this))
        e-args (gensym)
        b-args (gensym)
        symbols (map (comp symbol str) variables)        
        bind-f (fn [this args]
                 (let [new-expression (clojure.walk/postwalk-replace (zipmap symbols b-args) expression)]
                   (ExpressionImpl. new-expression)))        
        ]    
    (eval
      `(reify Function
         (~(type-hint 'double 'execute) [~'this ~(type-hint 'doubles 'e-args)]
            ~(binding [*unchecked-math* true]
               (let [execute-f (create-function-form 'e-args symbols expression)]                            
                 `(let ~(vec (concat (first execute-f) (second execute-f)))
                    ~(last execute-f))
                 )))))))


(defn- bind-expression-symbol [this symb rebind]
  (let [expression (:expression (.state this))
        rebind (if (char? rebind) (str rebind) rebind)
        rebind (if (string? rebind) (symbol rebind) rebind)
        symb (symbol (str symb))
        new-expression (clojure.walk/postwalk-replace {symb rebind} expression)]
    (ExpressionImpl.  new-expression)))


(defn #=(symbol "ExpressionImpl-function-char<>") [this variables]
  (reify-function this variables))

(defn #=(symbol "ExpressionImpl-function-String<>") [this variables]
  (reify-function this variables))

(defn #=(symbol "ExpressionImpl-bind-String-Expression") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-char-Expression") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-String-Number") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-char-Number") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-String-String") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-char-String") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-String-char") [this symb rebind]
  (bind-expression-symbol this symb rebind))
(defn #=(symbol "ExpressionImpl-bind-char-char") [this symb rebind]
  (bind-expression-symbol this symb rebind))

(extend-type clojure.lang.Seqable
  ExpressionTree
  (to-tree [this] this))

(extend-type ExpressionImpl
  ExpressionTree
  (to-tree [this] (:expression (.state this))))

(extend-type com.lambder.deriva.Expression
  ExpressionTree
  (to-tree [this] (.toTree this)))


(defn Vector1DImpl-constructor [expr]
  [[]
   {:expression (clojure.walk/postwalk
                  (fn [form]
                    (to-tree form))
                  expr)}])

(defn Vector2DImpl-constructor [expr]
  [[]
   {:expression (clojure.walk/postwalk
                  (fn [form]
                    (to-tree form))
                  expr)}])
(defn Vector3DImpl-constructor [expr]
  [[]
   {:expression (clojure.walk/postwalk
                  (fn [form]
                    (to-tree form))
                  expr)}])
(defn Vector4DImpl-constructor [expr]
  [[]
   {:expression (clojure.walk/postwalk
                  (fn [form]
                    (to-tree form))
                  expr)}])


(defn Vector1DImpl-describe [this]
  (describe (:expression (.state this))))
(defn Vector2DImpl-describe [this]
  (describe (:expression (.state this))))
(defn Vector3DImpl-describe [this]
  (describe (:expression (.state this))))
(defn Vector4DImpl-describe [this]
  (describe (:expression (.state this))))

(gen-class2
   :name "com.lambder.deriva.Vector1DImpl"
   :implements ["com.lambder.deriva.Vector1D"]
   :init "constructor"
   :constructors {[clojure.lang.Seqable] []}
   :prefix "Vector1DImpl-"
   :state state
   :impl-ns com.lambder.deriva.java)

(gen-class2
   :name "com.lambder.deriva.Vector2DImpl"
   :implements ["com.lambder.deriva.Vector2D"]
   :init "constructor"
   :constructors {[clojure.lang.Seqable] []}
   :prefix "Vector2DImpl-"
   :state state
   :impl-ns com.lambder.deriva.java)

(gen-class2
   :name "com.lambder.deriva.Vector3DImpl"
   :implements ["com.lambder.deriva.Vector3D"]
   :init "constructor"
   :constructors {[clojure.lang.Seqable] []}
   :prefix "Vector3DImpl-"
   :state state
   :impl-ns com.lambder.deriva.java)

(gen-class2
   :name "com.lambder.deriva.Vector4DImpl"
   :implements ["com.lambder.deriva.Vector4D"]
   :init "constructor"
   :constructors {[clojure.lang.Seqable] []}
   :prefix "Vector4DImpl-"
   :state state
   :impl-ns com.lambder.deriva.java)


(defn Vector1DImpl-toString [this]
  (with-out-str (pprint (:expression (.state this)))))
(defn Vector2DImpl-toString [this]
  (with-out-str (pprint (:expression (.state this)))))
(defn Vector3DImpl-toString [this]
  (with-out-str (pprint (:expression (.state this)))))
(defn Vector4DImpl-toString [this]
  (with-out-str (pprint (:expression (.state this)))))

(import com.lambder.deriva.Vector1DImpl)
(import com.lambder.deriva.Vector2DImpl)
(import com.lambder.deriva.Vector3DImpl)
(import com.lambder.deriva.Vector4DImpl)

(import com.lambder.deriva.Function)
(import com.lambder.deriva.Function1D)
(import com.lambder.deriva.Function2D)
(import com.lambder.deriva.Function3D)
(import com.lambder.deriva.Function4D)


(defn- reify-vector-d-function [d this variables]
  (let [
        expression (:expression (.state this))
        expression (normalize-expression expression)
        e-args (gensym)
        b-args (gensym)
        symbols (map (comp symbol str) variables)        
        bind-f (fn [this args]
                 (let [new-expression (clojure.walk/postwalk-replace (zipmap symbols b-args) expression)]
                   (ExpressionImpl. new-expression)))        
        ]
    (eval
      `(reify ~(symbol (str "com.lambder.deriva.Function" d "D"))
        (~(type-hint (symbol (str (str-times "[" d) "D")) 'execute) [~'this ~(type-hint 'doubles 'e-args)]
           ~(binding [*unchecked-math* true]
             (let [execute-f (create-function-form 'e-args symbols expression)]
              `(let ~(vec (concat (first execute-f) (second execute-f)))
                 ~(last execute-f)
                 ))))))))

(defn #=(symbol "Vector1DImpl-function-char<>") [this variables]
  (reify-vector-d-function 1 this variables))
(defn #=(symbol "Vector1DImpl-function-String<>") [this variables]
  (reify-vector-d-function 1 this variables))
(defn #=(symbol "Vector2DImpl-function-char<>") [this variables]
  (reify-vector-d-function 2 this variables))
(defn #=(symbol "Vector2DImpl-function-String<>") [this variables]
  (reify-vector-d-function 2 this variables))
(defn #=(symbol "Vector3DImpl-function-char<>") [this variables]
  (reify-vector-d-function 3 this variables))
(defn #=(symbol "Vector3DImpl-function-String<>") [this variables]
  (reify-vector-d-function 3 this variables))
(defn #=(symbol "Vector4DImpl-function-char<>") [this variables]
  (reify-vector-d-function 4 this variables))
(defn #=(symbol "Vector4DImpl-function-String<>") [this variables]
  (reify-vector-d-function 4 this variables))

(defn- bind-vector-d-symbol [this symb rebind]
  (let [expression (:expression (.state this))
        rebind (if (char? rebind) (str rebind) rebind)
        rebind (if (string? rebind) (symbol rebind) rebind)
        symb (symbol (str symb))
        new-expression (clojure.walk/postwalk-replace {symb rebind} expression)]
    (Vector1DImpl. new-expression)))

(defn #=(symbol "Vector1DImpl-bind-String-Expression") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-char-Expression") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-String-Number") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-char-Number") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-String-String") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-char-String") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-String-char") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))
(defn #=(symbol "Vector1DImpl-bind-char-char") [this symb rebind]
  (bind-vector-d-symbol this symb rebind))


(extend-type Vector1DImpl
  ExpressionTree
  (to-tree [this] (:expression (.state this))))

(extend-type com.lambder.deriva.Vector2D
  ExpressionTree
  (to-tree [this] (:expression (.state this))))

(extend-type com.lambder.deriva.Vector3D
  ExpressionTree
  (to-tree [this] (:expression (.state this))))

(extend-type com.lambder.deriva.Vector4D
  ExpressionTree
  (to-tree [this] (:expression (.state this))))


(extend-type clojure.lang.Seqable
  Describtable
  (describe [this] 
            (let [        
                  expression (optimize (derive-all this))
                  expression (normalize-expression expression)
                  expr_index (build-index expression)
                  inverted_expr_index (set/map-invert expr_index)
                  steps (distinct (reverse (exp2steps expression expr_index)))
                  df (fn [oper args]
                       (cond 
                         (= oper 'vector)     (str "[ " (s/join ", " args) " ];")  ;; TODO join with coma separator
                         (= oper 'ident)     (str (first args) ";")
                         (= 0 (count args))  (str oper ";")
                         (= 1 (count args))  (str oper "( " (first args) " );")
                         (= 2 (count args))  (str (first args) " " oper " " (second args) ";")                         
                         :else (str oper "( " args " );")
                         ))
                  d (map 
                      (fn [[s {oper :oper args :args}]]
                        (str "final double " s " = " (df oper args) "  //  " (with-out-str (pr (inverted_expr_index s))) "\n")) 
                      steps)
                  ]
              (apply str "Expression:\n" (with-out-str (pprint this)) "\ngets turned into:\n" (with-out-str (pprint expression)) "\n and into:\n" d))))


;; generation of static utility functions
(eval
  `(gen-class2
     :name "com.lambder.deriva.Deriva"
     :prefix "Deriva-"
     :methods [
               
               ~@(mapcat
                   (fn [a]
                     (map #(with-meta ['expression (cons 'java.lang.String %) 'com.lambder.deriva.Expression] {:static 'true}) (selections '(java.lang.Character java.lang.String java.lang.Number com.lambder.deriva.Expression) a)))
                   (range 0 3))
               
               ;; creates static 2-arity functions like:
               ;;
               ;;    (Expression add [String String])
               ;;
               ~@(mapcat
                   (fn [oper]
                     (map #(with-meta [oper % 'com.lambder.deriva.Expression] {:static 'true}) (selections '(java.lang.Character java.lang.String java.lang.Number com.lambder.deriva.Expression) 2)))
                   '(mul div add sub pow log gt lt and or max min eq))
               
               ;; creates static 1-arity functions like:
               ;;
               ;;    (Expression sin [String])
               ;;
               ~@(mapcat
                   (fn [oper]
                     (map #(with-meta [oper % 'com.lambder.deriva.Expression] {:static 'true}) (selections '(java.lang.Character java.lang.String java.lang.Number com.lambder.deriva.Expression) 1)))
                   '(sin cos tan ctg neg sq sqrt not abs ln exp))
               
               ~@(mapcat
                   (fn [oper]
                     (map #(with-meta [oper % 'com.lambder.deriva.Expression] {:static 'true}) (selections '(java.lang.Character java.lang.String java.lang.Number com.lambder.deriva.Expression) 3)))
                   '(when))
               

               ~(with-meta ['vector [#=(symbol "[Lcom.lambder.deriva.Expression;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true}) 
               ~(with-meta ['vector [#=(symbol "[Lcom.lambder.deriva.Vector1D;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true}) 
               ~(with-meta ['vector [#=(symbol "[Lcom.lambder.deriva.Vector2D;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true}) 
               ~(with-meta ['vector [#=(symbol "[Lcom.lambder.deriva.Vector3D;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})

               ~(with-meta ['d ['com.lambder.deriva.Bindable 'char] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;C)TT;"})
               ~(with-meta ['d ['com.lambder.deriva.Bindable 'java.lang.String] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;Ljava/lang/String;)TT;"})
               ~(with-meta ['dd ['com.lambder.deriva.Bindable 'char] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;C)TT;"})
               ~(with-meta ['dd ['com.lambder.deriva.Bindable 'java.lang.String] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;Ljava/lang/String;)TT;"})
               ~(with-meta ['ddd ['com.lambder.deriva.Bindable 'char] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;C)TT;"})
               ~(with-meta ['ddd ['com.lambder.deriva.Bindable 'java.lang.String] 'com.lambder.deriva.Bindable] {:static 'true :signature "<T::Lcom/lambder/deriva/Bindable;>(TT;Ljava/lang/String;)TT;"})
               
               ~(with-meta ['d ['com.lambder.deriva.Expression 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               ~(with-meta ['d ['com.lambder.deriva.Expression 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               
               ~(with-meta ['d ['com.lambder.deriva.Vector1D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               ~(with-meta ['d ['com.lambder.deriva.Vector1D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               
               ~(with-meta ['d ['com.lambder.deriva.Vector2D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               ~(with-meta ['d ['com.lambder.deriva.Vector2D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               
               ~(with-meta ['d ['com.lambder.deriva.Vector3D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})
               ~(with-meta ['d ['com.lambder.deriva.Vector3D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})
               

               ~(with-meta ['dd ['com.lambder.deriva.Expression 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               ~(with-meta ['dd ['com.lambder.deriva.Expression 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               
               ~(with-meta ['dd ['com.lambder.deriva.Vector1D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               ~(with-meta ['dd ['com.lambder.deriva.Vector1D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               
               ~(with-meta ['dd ['com.lambder.deriva.Vector2D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               ~(with-meta ['dd ['com.lambder.deriva.Vector2D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               
               ~(with-meta ['dd ['com.lambder.deriva.Vector3D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})
               ~(with-meta ['dd ['com.lambder.deriva.Vector3D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})
               
               
               ~(with-meta ['ddd ['com.lambder.deriva.Expression 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               ~(with-meta ['ddd ['com.lambder.deriva.Expression 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector1D] {:static 'true :varargs 'true})
               
               ~(with-meta ['ddd ['com.lambder.deriva.Vector1D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               ~(with-meta ['ddd ['com.lambder.deriva.Vector1D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector2D] {:static 'true :varargs 'true})
               
               ~(with-meta ['ddd ['com.lambder.deriva.Vector2D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               ~(with-meta ['ddd ['com.lambder.deriva.Vector2D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector3D] {:static 'true :varargs 'true})
               
               ~(with-meta ['ddd ['com.lambder.deriva.Vector3D 'java.lang.Character #=(symbol "[Ljava.lang.Character;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})
               ~(with-meta ['ddd ['com.lambder.deriva.Vector3D 'java.lang.String #=(symbol "[Ljava.lang.String;")] 'com.lambder.deriva.Vector4D] {:static 'true :varargs 'true})                                             
               ]
     ))

(defn Deriva-expression [oper [& args]]
  (com.lambder.deriva.ExpressionImpl. (cons oper args)))


(defmacro def-deriva-fun [& names]
  (cons
    'do
    (map
      (fn [oper]
        `(defn ~(symbol (str 'Deriva- oper))
           [& ~'args]
           (com.lambder.deriva.ExpressionImpl. (cons (quote ~oper) ~'args))))
      names)))


(def-deriva-fun ln log abs mul div add sub exp pow sin cos tan ctg neg sq sqrt gt lt when and or not max min eq)


(defn #=(symbol "Deriva-d-Bindable-char") [bindable in-respect]
  (cond
    (instance? com.lambder.deriva.Expression bindable) (com.lambder.deriva.ExpressionImpl. (list 'd bindable in-respect))
    (instance? com.lambder.deriva.Vector1D bindable) (com.lambder.deriva.Vector1DImpl. (list 'd bindable in-respect))
    (instance? com.lambder.deriva.Vector2D bindable) (com.lambder.deriva.Vector2DImpl. (list 'd bindable in-respect))
    (instance? com.lambder.deriva.Vector3D bindable) (com.lambder.deriva.Vector3DImpl. (list 'd bindable in-respect))
    (instance? com.lambder.deriva.Vector4D bindable) (com.lambder.deriva.Vector4DImpl. (list 'd bindable in-respect))
    :else (throw (Exception. (str "the `d` operator can't accept " (type bindable) " type.")))))

(defn #=(symbol "Deriva-d-Bindable-String") [bindable in-respect]
  (#=(symbol "Deriva-d-Bindable-char") bindable in-respect))

(defn #=(symbol "Deriva-dd-Bindable-char") [bindable in-respect]
  (cond
    (instance? com.lambder.deriva.Expression bindable) (com.lambder.deriva.ExpressionImpl. (list 'dd bindable in-respect))
    (instance? com.lambder.deriva.Vector1D bindable) (com.lambder.deriva.Vector1DImpl. (list 'dd bindable in-respect))
    (instance? com.lambder.deriva.Vector2D bindable) (com.lambder.deriva.Vector2DImpl. (list 'dd bindable in-respect))
    (instance? com.lambder.deriva.Vector3D bindable) (com.lambder.deriva.Vector3DImpl. (list 'dd bindable in-respect))
    (instance? com.lambder.deriva.Vector4D bindable) (com.lambder.deriva.Vector4DImpl. (list 'dd bindable in-respect))
    :else (throw (Exception. (str "the `dd` operator can't accept " (type bindable) " type.")))))

(defn #=(symbol "Deriva-dd-Bindable-String") [bindable in-respect]
  (#=(symbol "Deriva-dd-Bindable-char") bindable in-respect))

(defn #=(symbol "Deriva-ddd-Bindable-char") [bindable in-respect]
  (cond
    (instance? com.lambder.deriva.Expression bindable) (com.lambder.deriva.ExpressionImpl. (list 'ddd bindable in-respect))
    (instance? com.lambder.deriva.Vector1D bindable) (com.lambder.deriva.Vector1DImpl. (list 'ddd bindable in-respect))
    (instance? com.lambder.deriva.Vector2D bindable) (com.lambder.deriva.Vector2DImpl. (list 'ddd bindable in-respect))
    (instance? com.lambder.deriva.Vector3D bindable) (com.lambder.deriva.Vector3DImpl. (list 'ddd bindable in-respect))
    (instance? com.lambder.deriva.Vector4D bindable) (com.lambder.deriva.Vector4DImpl. (list 'ddd bindable in-respect))
    :else (throw (Exception. (str "the `ddd` operator can't accept " (type bindable) " type.")))))

(defn #=(symbol "Deriva-ddd-Bindable-String") [bindable in-respect]
  (#=(symbol "Deriva-ddd-Bindable-char") bindable in-respect))




(defn #=(symbol "Deriva-d-Expression-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector1D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector2D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector3D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-dd-Expression-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector1D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector2D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector3D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-ddd-Expression-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector1D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector2D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector3D-Character-Character<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-d-Expression-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector1D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector2D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-d-Vector3D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-dd-Expression-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector1D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector2D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-dd-Vector3D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'dd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-ddd-Expression-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector1DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector1D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector2DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector2D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector3DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))

(defn #=(symbol "Deriva-ddd-Vector3D-String-String<>") [bindable r rs]
  (com.lambder.deriva.Vector4DImpl. (mapv (fn[resp] (list 'ddd bindable resp)) (cons r rs))))



(defn #=(symbol "Deriva-vector-Expression<>") [arr]
  (com.lambder.deriva.Vector1DImpl. (vec arr)))

(defn #=(symbol "Deriva-vector-Vector1D<>") [arr]
  (com.lambder.deriva.Vector2DImpl. (vec arr)))

(defn #=(symbol "Deriva-vector-Vector2D<>") [arr]
  (com.lambder.deriva.Vector3DImpl. (vec arr)))

(defn #=(symbol "Deriva-vector-Vector3D<>") [arr]
  (com.lambder.deriva.Vector4DImpl. (vec arr)))







#_(
 (use 'com.lambder.deriva.java)
 (import 'com.lambder.deriva.Vector1DImpl)
 (import 'com.lambder.deriva.Deriva)
 
 (def a (make-array com.lambder.deriva.Expression 1))   
 (aset a 0 (Deriva/sin \x))

 (.function (Deriva/vector a) (char-array [\x]))   
 
 (type (to-tree (Deriva/vector a)) )  
   
)










