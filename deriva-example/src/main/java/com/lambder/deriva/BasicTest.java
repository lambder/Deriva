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

import java.util.Arrays;


import static com.lambder.deriva.Deriva.*;
import static org.junit.Assert.assertEquals;


public class BasicTest {

  @Test
  public void testBasicExpressions() {

    Expression sine = sin('x');
    Expression square_of_sines = sq(sin('x'));

    Expression d_dx_sine = d(sine, 'x');

    assertEquals(sine.function('x').execute(4.5), Math.sin(4.5), 0);
    assertEquals(square_of_sines.function('x').execute(4.5), Math.pow(Math.sin(4.5), 2), 0);
    assertEquals(d_dx_sine.function('x').execute(4.5), Math.cos(4.5), 0);

    Vector1D v1 = vector(sine, square_of_sines);
    Function1D v1_f = v1.function('x');

    double[] v1_r = v1_f.execute(4.5);

    assertEquals(v1_r[0], Math.sin(4.5), 0);
    assertEquals(v1_r[1], Math.pow(Math.sin(4.5), 2), 0);

    Vector2D v2 = vector(vector(sine, square_of_sines), vector(add(3, 3)));
    double[][] v2_r = v2.function('x').execute(4.5);


    assertEquals(v2_r[0].length, 2);
    assertEquals(v2_r[1].length, 1);
    assertEquals(v2_r[0][0], Math.sin(4.5), 0);
    assertEquals(v2_r[0][1], Math.pow(Math.sin(4.5), 2), 0);
    assertEquals(v2_r[1][0], 6, 0);

    Vector2D v3 = d(v2, 'x');

    double[][] v3_r = v3.function('x').execute(4.5);

    System.out.println(v3.describe());

    assertEquals(v3_r[0].length, 2);
    assertEquals(v3_r[1].length, 1);
    assertEquals(v3_r[0][0], Math.cos(4.5), 0);
    assertEquals(v3_r[0][1], 2*Math.sin(4.5)*Math.cos(4.5), 0);
    assertEquals(v3_r[1][0], 0, 0);


    // multi-variate functions:
    Vector1D ex = vector(add(sin(mul(sq('x'), sq('y'))), ln(abs('z'))), sin('x'));
    Function1D fun = ex.function('x', 'y', 'z');

    double[] res = fun.execute(1, 2, 3);
    System.out.println(res);


    // binding

    Vector1D b_ex = ex.bind('x', sin('t'));
    System.out.println(b_ex.describe());


    Vector1D xxx = d(mul('x', 'y'), 'x', 'y');

    System.out.println(xxx.describe());
    double[] res3 = xxx.function('x', 'y').execute(3, 5);
    System.out.println(Arrays.toString(res3));


    System.out.println(Arrays.toString(d(vector(mul('x', 'y')), 'x', 'y').function('x', 'y').execute(3, 4)));
    System.out.println(Arrays.toString(d(vector(vector(mul('x', 'y'))), 'x', 'y').function('x', 'y').execute(3, 4)));
    System.out.println(Arrays.toString(d(vector(vector(vector(mul('x', 'y')))), 'x', 'y').function('x', 'y').execute(3, 4)));


    System.out.println(dd(sin('x'), 'x').describe());
    System.out.println(ddd(sin('x'), 'x').describe());
    System.out.println(ddd(sin(mul('x', 'y')), 'x', 'y').describe());


    // function with gradient second-order gradient and third order x-partial derivative
    Expression e = add(mul(sin('x'), cos('y')), sin('z'));
    Function2D f = vector(vector(e), d(e, 'x', 'y', 'z'), dd(e, 'x', 'y', 'z'), vector(ddd(e, 'x'))).function('x', 'y', 'z');
    double[][] r = f.execute(3123.12, 0534.54, 4324.534);
    System.out.println(r);

    Expression expr = sin(mul(sq('x'), sq('y')));
    Vector1D g_expr = vector(expr, d(expr, 'x'), d(expr, 'x'));
    System.out.println(g_expr.describe());

    Function funX = when(
        gt('x', 0),   // when x is greater than 0
        exp(          // e^(-1/x)
            neg(
                div(1, 'x'))),
        0)            // otherwise 0
        .function('x');

    funX.execute(2);

    System.out.println(when(gt(1, 4), 3.0, 'x').function('x').execute(5));
  }

}
