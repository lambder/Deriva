/*
 * Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */
package com.lambder.deriva;

import org.junit.Test;

import static com.lambder.deriva.Deriva.*;


public class BlackFormula {

  public static Expression black(boolean isCall) {

    // Logistic approximation of Cumulative Standard Normal Distribution
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

  @Test
  public void testBlackFormula() {
    Expression blackModel = black(true).bind('T', 0.523);
    System.out.println(blackModel.describe());
    Function1D fun = d(blackModel, "F", "K", "r", "sigma").function("F", "K", "r", "sigma");
    fun.execute(12.3, 14.3, 0.03, 1);
    fun.execute(12.3, 11.0, 0.03, 1);
    fun.execute(12.3, 11.0, 0.02, 1);
  }
}
