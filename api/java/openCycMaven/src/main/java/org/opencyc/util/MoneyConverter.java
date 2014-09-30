/* $Id: MoneyConverter.java 131054 2010-05-26 18:59:41Z baxter $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.util;

//// Internal Imports
//// External Imports
import java.util.Currency;
import java.util.HashMap;
import java.util.Map;

//// OpenCyc Imports
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycNaut;
import org.opencyc.cycobject.Guid;

/** 
 * <P>MoneyConverter is designed to convert java-style money amounts to their
 * corresponding CycL representations and vice versa.
 *
 * @todo Add more currencies. Currently only supports USD.
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : May 21, 2010, 2:03:06 PM
 * Author     : nwinant
 * @version $Id: MoneyConverter.java 131054 2010-05-26 18:59:41Z baxter $
 */
public class MoneyConverter extends DataTypeConverter<Money> {

  //// Constructors
  private MoneyConverter() {
    SHARED_INSTANCE = this;
  }


  //// Public Area

  /** Returns an instance of <code>MoneyConverter</code>.
   *
   * If an instance has already been created, the existing one will be returned.
   * Otherwise, a new one will be created.
   */
  private static MoneyConverter getInstance() {
    MoneyConverter moneyConverter = SHARED_INSTANCE;
    if (moneyConverter == null) {
      moneyConverter = new MoneyConverter();
    }
    return moneyConverter;
  }

  /** Try to parse <code>naut</code> into a Java <code>Money</code>.
   *
   * 
   * @param naut A money-denoting Cyc term.
   * @param shouldReportFailure If true, prints stack trace and returns null if the parse fails.
   * @return An equivalent Money object.
   */
  public static Money parseCycMoney(final CycNaut naut, final boolean shouldReportFailure) {
    return getInstance().parse(naut, shouldReportFailure);
  }

  /** Try to parse <code>naut</code> into a Java <code>Money</code>.
   *
   * Prints stack trace and returns null if the parse fails.
   */
  public static Money parseCycMoney(final CycNaut naut) {
    return getInstance().parse(naut);
  }

  public static boolean isCycMoney(final Object object) {
    return getInstance().isOfType(object);
  }

  public static CycNaut toCycMoney(final Money obj) throws ParseException {
    return getInstance().toCycTerm(obj);
  }

  //// Protected Area
  @Override
  protected Money parseDataType(final CycNaut naut) throws ParseException {
    final Currency currency = lookupCurrency(naut.getFunctor());
    if (currency == null) {
      throwParseException(naut, this.getClass().getName() + " does not recognize " + naut.getFunctor() + " as a currency.");
    } else if (naut.getArity() != 1) {
      throwParseException(naut, this.getClass().getName() + " can only parse Cyc terms with an arity of 1.");
    }

    return new Money(parseBigDecimal(naut.getArg(1), "amount"), currency);
  }

  /**
   * Convert Java object of type <code>Money</code> to <code>naut</code>.
   *
   * @param obj
   * @return 
   * @throws ParseException if the parse fails.
   */
  @Override
  protected CycNaut toCycTerm(final Money obj) throws ParseException {
    ensureCurrencyMapInitialized();
    final CycFort functor = lookupCycCurrencyTerm(obj.getCurrency());
    if (functor == null) {
      throwParseException("Cannot find Cyc UnitOfMoney for currency code " + obj.getCurrency().getCurrencyCode());
    }
    return new CycNaut(functor, obj.getQuantity());
  }

  //// Private Area
  public static CycFort lookupCycCurrencyTerm(final Currency curr) {
    ensureCurrencyMapInitialized();
    return CURRENCY_TO_CYC_CURRENCY_MAP.get(curr);
  }

  public static Currency lookupCurrency(final CycFort cycTerm) {
    ensureCurrencyMapInitialized();
    return lookupKeyByValue(CURRENCY_TO_CYC_CURRENCY_MAP, cycTerm);
  }

  private static void ensureCurrencyMapInitialized() {
    if (CURRENCY_TO_CYC_CURRENCY_MAP == null) {
      initializeCurrencyCycTermHash();
    }
  }

  /**
   * @todo: this needs to be fleshed out much, much more.
   * See http://en.wikipedia.org/wiki/ISO_4217
   */
  private static void initializeCurrencyCycTermHash() {
    CURRENCY_TO_CYC_CURRENCY_MAP = new HashMap<Currency, CycFort>();
    CURRENCY_TO_CYC_CURRENCY_MAP.put(Currency.getInstance("USD"), CURR_USD);
  }
  //// Internal Rep
  private static final CycConstant CURR_USD = new CycConstant("Dollar-UnitedStates",
          new Guid("bd58a636-9c29-11b1-9dad-c379636f7270"));
  private static Map<Currency, CycFort> CURRENCY_TO_CYC_CURRENCY_MAP = null;
  private static MoneyConverter SHARED_INSTANCE = null;


  //// Main
}
