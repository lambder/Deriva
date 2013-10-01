# Deriva

<img src="https://raw.github.com/lambder/Deriva/master/assets/PartialDerivative_800.gif"
 alt="Partial Derivative" title="Deriva" align="right" />

<!-- > "Derivatives are financial weapons of mass destruction."
> - [Warren Buffett] https://en.wikipedia.org/wiki/Warren_Buffett

> Warren Buffett Made $390 Million On Financial Weapons Of Mass Destruction
> - [Read more] http://www.businessinsider.com/warren-buffett-q2-derivatives-gains-2013-8#ixzz2fjpwJ8qt
 -->

Deriva automates algorithmic differentiation in Java and Clojure projects.

## Installation

### Leiningen

Add the following to your `:dependencies`:

```clj
[deriva "0.1.0-SNAPSHOT"]
```

### Maven

```xml
<dependency>
  <groupId>deriva</groupId>
  <artifactId>deriva</artifactId>
  <version>0.1.0-SNAPSHOT</version>
</dependency>
```


## Basic Java Usage

#### Simple example - sine function

To use Deriva java DSL simply add the following lines to your code:
```java
import com.lambder.deriva.*;
import static com.lambder.deriva.Deriva.*;
```


now we can define sine expression, simply as:

```java
Expression expr = sin('x');
```

Such expressions can be used as sub-expressions to build more complex math formulas. We’ll look at more complex example later on. Now let’s see how we can use an Expression. To execute the expression we need to create a function from it:

```java
Function fun = expr.function('x');
```

Here we see that we use the `'x'` symbol (represented here by a single character, but regular Strings will do as well) to define mapping of symbols to the arguments of a function. Placing a symbol in a given place indicates which argument it will map to. It will become more clear, when we see function invocation:



```java
double result = fun.execute(Math.PI / 6);
```

here `execute` takes `double` parameters (in our case one such parameter) and substitutes them into the underlying expression in accordance with the mapping defined when we called `function`. So in this case it replaces all occurrences of [ x ] with [ pi/6 ], making our original expression render [sin(pi/6)].

Now to derivatives. In order to calculate a derivative of a given expression in respect to a given symbol, in Java we do:

```java
Expression expr_d_1 = d(expr, 'x');
```

We can then use the expression, representing first order derivative on sine function [ ∂/(∂x) sin(x) ] , to obtain its value at point [t] by:

```java
double slope = expr_d_1.function('x').execute(t);
```

#### More fun - gradients of multivariate functions

The real benefits of algorithmic differentiation come when we work with multivariate functions. The code that calculates a gradient - that is, a vector of partial derivatives in respect to all variables - requires less operations than calculating these partial derivatives separately.

As an example lets take [ sin(x^2 y^2)  RR^2 => RR ] function.

To get its gradient we do:

```java
Expression expr = sin(mul(sq('x'), sq('y')));
Function1 fun = d(expr, 'x', 'y').function('x', 'y');  // (1)
double[] result = fun.execute(1.0, 2.0);
System.out.println(Arrays.toString(result));
```

which prints:

~~~ java
[-5.2291489669088955, -2.6145744834544478]
~~~

The result is n-long array of doubles, which elements are values of corresponding partial derivatives in order defined in `function` (1) call.

## Basic Clojure Usage

The namespace we are using is `com.lambder.deriva.core`

```clj
(use 'com.lambder.deriva.core)
```

### Simple expression:

```clj
(def f (function (sin x)))
(f 1) ;=> 0.8414709848078965
(def g (function (∂ (sin x) x))
(g 1) ;=> 0.5403023058681398
```


## More involved example - Black model[^3] with sensitivities

Black model as defined on wikipedia

[^3]: <http://en.wikipedia.org/wiki/Black_model>

call price : %% c = e^(-rT)(FN(d_1)-KN(d_2)) %%

put price: %% p = e^(-rT)(FN(-d_2)-KN(-d_1)) %%

where

[ d_1 = (ln(F//K)+(sigma^2//2)T)/(sigma*sqrt(T)) ]

[ d_2 = (ln(F//K)-(sigma^2//2)T)/(sigma*sqrt(T)) ]

[ N(x) = int_-oo^x (1/(2sqrt(pi))e^-(t^2/2) ) dt  ~~  1/(e^(-0.07056 * x^3 -1.5976*x) + 1)  ]

### in Java:

```java
import static com.lambder.deriva.Deriva.*;

public class Formulas {

  public static Expression black(final boolean isCall) {

    // Logistic aproximation of Cumulated Standard Normal Distribution
    // 1/( e^(-0.07056 * x^3 - 1.5976*x) + 1)
    Expression N =  div(1.0,
        add(
            exp(
                sub(
                    mul(
                        -0.07056,
                        pow('x', 3)),
                    mul(-1.5976, 'x'))),
            1.0));

    // ( F/K+T*σ^2/2 ) / σ*sqrt(T)
    Expression d1 = div(
        add(
            div('F', 'K'),
            mul(div(sq("sigma"), 2.0), 'T')),
        mul("sigma", sqrt('T')));

    // ( F/K-T*σ^2/2 ) / σ*sqrt(T)
    Expression d2 = div(
        sub(
            div('F', 'K'),
            mul(div(sq("sigma"), 2.0), 'T')),
        mul("sigma", sqrt('T')));

    // e^(-r*T) * ( F*N(d1)-K*N(d2) )
    Expression call = mul(
        exp(neg(mul('r', 'T'))),
        sub(
            mul('F', N.bind('x', d1)),
            mul('K', N.bind('x', d2))));

    // e^(-r*T) * ( F*N(-d2)-K*N(-d1) )
    Expression put = mul(
        exp(neg(mul('r', 'T'))),
        sub(
            mul('F', N.bind('x', neg(d2))),
            mul('K', N.bind('x', neg(d1)))));

    return isCall ? call : put;
  }

  // usage
  public static void main(String[] args) {
    // lets fix timeToExpiry to 0.523 and get only strike, forward and lognormalVol sensitivities
    Expression blackModel = black(true).bind('T', 0.523);
    Function1 fun = d(blackModel, 'F', 'K', 'r').function('F', 'K', 'r');
    fun.execute(12.3, 14.3, 0.03);
    fun.execute(12.3, 11.0, 0.03);
    fun.execute(12.3, 11.0, 0.02);
  }
}
```

### in Clojure:

```clj
(use 'com.lambder.deriva.core)

(def N
  '(/ 1
      (+ 1
         (exp (-
                (* -0.07056 (pow x 3))
                (* -1.5976 x)))))

(def d1 '(/ (+ (/ F K) (* T (/ (sq sigma) 2)))))

(def d2 '(/ (- (/ F K) (* T (/ (sq sigma) 2)))))

(def call
    `(*
        (exp (- (* r T))
        (-
          (* F ~(bind N x d1))
          (* K ~(bind N x d2)))))

(def put
    `(*
        (exp (- (* r T))
        (-
          (* F ~(bind N x (- d1)))
          (* K ~(bind N x (- d2))))))

(defn black-expression [call?] call put)


;; usage

(def black-model-with-sensitivities (∂ (bind (black-expression true) T 0.523) F K r))

(black-model-with-sensitivities 12.3 14.3 0.03)
(black-model-with-sensitivities 12.3 11.0 0.03)
(black-model-with-sensitivities 12.3 11.0 0.02)

```



## Q&A

1. Isn't it slow?   No. The generated code is actually very fast. As fast as Java originated byte-code can be.
There is no effort put in the performance of the code defining and transforming actual expressions,
but the resulting code is highly optimised. The intention is to have the definition in kind of initialisation
part of the application and run the generated code many times.

2. What are the applications?   Computation of sensitives of various financial models. Backpropagation in machine learning and I believe many more.

## The roadmap.

Derive is by no means finished product. It is under active development and I already have gathered requests for new functionalities:

1. Support for complex numbers
2. Support for jabobians
3. Using arbitrary java code in Deriva expressions

## License

Source Copyright © 2013 Daniel Kwiecinski.
Distributed under the Eclipse Public License, the same as Clojure
uses. See the file COPYING.
