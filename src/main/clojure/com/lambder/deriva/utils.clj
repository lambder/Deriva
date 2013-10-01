;;   Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.


(ns com.lambder.deriva.utils
  (:use [clojure.pprint]))


(defn one? [a] (= 1 a))
(defn zer? [a] (= 0 a))

(defn append [seq e]
  (concat seq [e]))

(defn str-times [s n]
  (apply str (take n (repeat s))))


(defn double-array-class-name [^long dims]
  (str (str-times "[" dims) "D"))

(defn double-array-type [^long dims]
  (if (pos? dims)
  	(Class/forName (str (str-times "[" dims) "D"))
    Double/TYPE))

(defmacro classForName [n]
  (if (symbol? n)
    n
    `(Class/forName ~n)))

(defn hint-from-type [^Class type]
  (.getName type))


;; TODO benchamrk against clojure.core/into-array
(defmacro create-array [array-type [& sequence]]          
  (let [ar-sym (with-meta (gensym) {:tag (double-array-type array-type)})
        class-name (double-array-class-name array-type) 
        clazz (if (one? array-type) 'Double/TYPE (double-array-type (dec array-type)))
        clazz2 (hint-from-type (double-array-type (dec array-type)))
        ;p-clazz (if (one? array-type) double (hint-from-type (double-array-type (dec array-type))))        
        n (count sequence)
        ar (gensym)
        ]    
    `(let [~ar (make-array ~clazz ~(count sequence)) 
           ;~ar ~(vary-meta ar assoc :tag 'objects)
          ]           
        (do
          ~@(map 
              #(list 
                'clojure.core/aset 
                ;(with-meta ar {:tag "[D"})
                (with-meta ar {:tag (double-array-class-name array-type)})
                %1 
                (if (one? array-type) 
                   (list 'double %2)
                   (with-meta %2 {:tag (double-array-class-name (dec array-type))})))
              (range) sequence)
          ~ar))))
           	

; from http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
(defn array? [x] (-> x class .isArray))
(defn see [x] (if (array? x) (map see x) x))

; usage: (deep-aget dd doubles i j)
(defmacro deep-aget
  ([array hint idx]
    `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
       (deep-aget a# ~hint ~@idxs))))

; usage: (deep-aset dd doubles i j 42.0)
(defmacro deep-aset [array hint & idxsv]
  (let [hints '{doubles double ints int} ; writing a comprehensive map is left as an exercise to the reader
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~array ~'objects ~@idxs)
                        array)
        a-sym (with-meta (gensym "a") {:tag hint})]
      `(let [~a-sym ~nested-array]
         (aset ~a-sym ~idx ~v))))
; ~ from http://clj-me.cgrand.net/2009/10/15/multidim-arrays/

; (defmacro array [type [& dims] & elements]
;   `(doto (make-array ~type ~@dims)
;          (aset )
;     )
;   )
