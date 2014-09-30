/* $Id: Money.java 131027 2010-05-25 00:30:23Z nwinant $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util;

//// Internal Imports

//// External Imports
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.math.BigDecimal;
import java.util.Currency;


/**
 * <P>Money is a Java representation of monetary amounts.
 *
 * @todo Add String-parsing constructor (so you can instantiate from strings like "$53.60")
 * @todo Expose BigDecimal arithmetic methods (add, subtract, divide, etc.)
 * @todo Add currency-appropriate rounding
 * @todo Improve serialization
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : May 24, 2010, 4:30:39 PM
 * Author     : nwinant
 * @version $Id: Money.java 131027 2010-05-25 00:30:23Z nwinant $
 */
public class Money implements Serializable, Comparable<Money> {

  //// Constructors

  /** Creates a new instance of Money. */
  public Money(BigDecimal quantity, Currency currency) {
    this.quantity = quantity;
    this.currency = currency;
  }

  /** Creates a new instance of Money, with the default currency. */
  public Money(BigDecimal quantity) {
    this(quantity, DEFAULT_CURRENCY);
  }


  //// Public Area
  
  /**
   * @return the quantity
   */
  public BigDecimal getQuantity() {
    return quantity;
  }

  /**
   * @return the currency
   */
  public Currency getCurrency() {
    return currency;
  }

  public boolean isSameCurrencyAs(Money m) {
    return getCurrency().equals(m.getCurrency());
  }
  
  public int compareTo(Money o) {
    if ((o == null) || (!isSameCurrencyAs(o)))
      throw new ClassCastException();
    return getQuantity().compareTo(o.getQuantity());
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof Money))
      return false;
    
    final Money m = (Money) obj;
    if (!isSameCurrencyAs(m))
      return false;

    return getQuantity().equals(m.getQuantity());
  }

  @Override
  public int hashCode() {
    int hash = 3;
    hash = 71 * hash + (this.quantity != null ? this.quantity.hashCode() : 0);
    hash = 71 * hash + (this.currency != null ? this.currency.hashCode() : 0);
    return hash;
  }

  @Override
  public String toString() {
    return getQuantity() + " " + getCurrency().getSymbol();
  }

  
  //// Protected Area


  //// Private Area


  //// Internal Rep

  public static final Currency DEFAULT_CURRENCY = Currency.getInstance("USD");
  private final BigDecimal quantity;
  private final Currency currency;

  
  //// Main

  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    final Logger logger = Logger.getLogger(Money.class.toString());
    logger.info("Starting");
    Money thisObj = null;
    try {
    } catch (Exception e) {
      logger.log(Level.SEVERE, e.getMessage(), e);
    } finally {
      if (thisObj != null) {
        // Clean up resource if necessary. E.g., call close() or flush().
      }
      logger.info("Finished.");
      System.exit(0);
    }
  }

}

