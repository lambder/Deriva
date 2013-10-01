/*
 * Copyright (c) 2013 Daniel Kwiecinski. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */
package com.lambder.deriva;

public interface Bindable<T extends Bindable> {

  T bind(String symbol, Expression expression);

  T bind(char symbol, Expression expression);

  T bind(String symbol, Number number);

  T bind(char symbol, Number number);

  T bind(String symbol, String otherSymbol);

  T bind(char symbol, String otherSymbol);

  T bind(String symbol, char otherSymbol);

  T bind(char symbol, char otherSymbol);

  String describe();

}