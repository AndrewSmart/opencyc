/* $Id: ComparableNumber.java 131140 2010-06-07 23:01:23Z nwinant $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util;

//// Internal Imports

//// External Imports

/** 
 * <P>ComparableNumber is a wrapper class for Numbers, and specifically exists
 * to allow comparisons of different kinds of numbers (floats, ints, etc.)
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Jun 7, 2010, 4:26:56 PM
 * Author     : nwinant
 * @version $Id: ComparableNumber.java 131140 2010-06-07 23:01:23Z nwinant $
 */
public class ComparableNumber extends Number implements Comparable<ComparableNumber> {

  //// Constructors

  /** Creates a new instance of ComparableNumber. */
  public ComparableNumber(final Number number) {
    this.number = number;
  }

  //// Public Area
  
  @Override
  public int intValue() {
    return number.intValue();
  }

  @Override
  public long longValue() {
    return number.longValue();
  }

  @Override
  public float floatValue() {
    return number.floatValue();
  }

  @Override
  public double doubleValue() {
    return number.doubleValue();
  }

  @Override
  public boolean equals(final Object obj) {
    final ComparableNumber num;
    try {
      num = (ComparableNumber) obj;
    } catch (ClassCastException c) {
      return false;
    }
    return number.equals(num.getNumber());
  }

  @Override
  public int hashCode() {
    int hash = 3;
    hash = 97 * hash + (this.number != null ? this.number.hashCode() : 0);
    return hash;
  }

  public int compareTo(final ComparableNumber o) {
    if (number.getClass().equals(o.getNumber().getClass())) {
      return ((Comparable) number).compareTo((Comparable) o.getNumber());
    } else {
      return new Double(number.doubleValue()).compareTo(o.getNumber().doubleValue());
    }
  }

  /**
   * @return the number
   */
  public Number getNumber() {
    return number;
  }
  

  //// Protected Area

  //// Private Area

  //// Internal Rep

  private final Number number;


  //// Main

}
