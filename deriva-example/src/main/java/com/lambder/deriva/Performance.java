/*
 * Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */
package com.lambder.deriva;

import static com.lambder.deriva.Deriva.*;
import com.lambder.deriva.*;

public class Performance {
  static final int ITERATIONS = 10000000;

  public static void main(String[] args) {

    testDeriva();
    testJava();

  }
  
  private static void testDeriva(){
    Expression expr = pow(add(sq(mul(sin('x'), cos('y'))), 'z'), 'z');
    Vector1D d_expr = vector(d(expr, 'x'), d(expr, 'y'), d(expr, 'z'));
    System.out.println(d_expr.toString());
    System.out.println(d_expr.describe());
    Function1D fun = d_expr.function('x', 'y', 'z');
    double res = 0;
    long start, end;
    for (int i = 0; i < ITERATIONS; i++) {
      res = res + fun.execute(100.0498384023, 1000.000000234, 1.000042320423)[1];
    }
    start = System.currentTimeMillis();
    for (int i = 0; i < ITERATIONS; i++) {
      res = res + fun.execute(100.0498384023, 1000.000000234, 1.000042320423)[1];
    }
    end = System.currentTimeMillis();
    System.out.println("Deriva took: "+(end-start)+" milliseconds to do "+ITERATIONS+" iterations."+res);
  }

  static class Calc {
    //public double[] execute(double x, double y, double z){
    public double[] execute(double... args){
      final double x = args[0];
      final double y = args[1];
      final double z = args[2];
      final double G__1624 = z;  //
      final double G__1623 = y;  //
      final double G__1622 = Math.cos(G__1623);  //
      final double G__1621 = x;  //
      final double G__1620 = Math.sin(G__1621);  //
      final double G__1619 = G__1620 * G__1622;  //
      final double G__1618 = G__1619 * G__1619;  //
      final double G__1617 = G__1618 + G__1624;  //
      final double G__1616 = Math.log(G__1617);  //
      final double G__1606 = G__1624 / G__1617;  //
      final double G__1605 = G__1606 + G__1616;  //
      final double G__1595 = Math.pow(G__1617,  G__1624);  //
      final double G__1594 = G__1595 * G__1605;  //
      final double G__1580 = Math.sin(G__1623);  //
      final double G__1579 = -1;  //
      final double G__1578 = G__1579 * G__1580;  //
      final double G__1577 = G__1578 * G__1620;  //
      final double G__1571 = 2;  //
      final double G__1570 = G__1571 * G__1619;  //
      final double G__1569 = G__1570 * G__1577;  //
      final double G__1568 = G__1569 * G__1606;  //
      final double G__1557 = G__1595 * G__1568;  //
      final double G__1543 = Math.cos(G__1621);  //
      final double G__1542 = G__1543 * G__1622;  //
      final double G__1534 = G__1570 * G__1542;  //
      final double G__1533 = G__1534 * G__1606;  //
      final double G__1522 = G__1595 * G__1533;  //
      return new double[]{ G__1522, G__1557, G__1594};  //
    }
  }

  private static void testJava(){
    Calc fun = new Calc();
    double res = 0;
    long start, end;
    for (int i = 0; i < ITERATIONS; i++) {
      res = res + fun.execute(100.0498384023, 1000.000000234, 1.000042320423)[1];
    }
    start = System.currentTimeMillis();
    for (int i = 0; i < ITERATIONS; i++) {
      res = res + fun.execute(100.0498384023, 1000.000000234, 1.000042320423)[1];
    }
    end = System.currentTimeMillis();
    System.out.println("Java took: "+(end-start)+" milliseconds to do "+ITERATIONS+" iterations."+res);
  }
}
