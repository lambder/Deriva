(ns com.lambder.deriva.facts  
  (:use [com.lambder.deriva.core] [midje.sweet]))

  
(def simple-expression '(+ x (sin x)) )

(def simple-expression-2 '(pow z (+ x (* y x))))

(fact "deriva turns expressions into working functions"
	(let [x 3 y 4 z 7]
		((create-function ['x] simple-expression) x) => (+ x (Math/sin x))
	    ((create-function ['x 'y 'z] simple-expression-2) x y z) => (Math/pow z (+ x (* y x)))
	))


(fact "deriva can optimize expressions"
  (optimize '(+ 0 (+ 1 (* x 0)))) => 1
  (optimize '(+ y (* 1 (* x 1)))) => '(+ y x))



(fact "deriva can calculate derivatives"
  (derive-all '(∂ (sin x) x)) => '(cos x)
  (derive-all '(∂ (sin x) y)) => 0
  (derive-all '(∂ (sin (cos x)) x)) => '(* (cos (cos x)) (* -1 (sin x)))	)



(fact "deriva can compare expressions"
  (expression-comparator '(* x y) '(* y x)) => 0
  (expression-comparator '(+ x y) '(+ y x)) => 0)