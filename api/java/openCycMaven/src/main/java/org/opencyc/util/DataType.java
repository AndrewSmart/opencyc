package org.opencyc.util;

//// External Imports
import java.util.Date;

//// OpenCyc Imports
import org.opencyc.cycobject.CycNaut;

/**
 * <P>DataType is the closest reasonable Java class for a given CycTerm.
 *
 * @todo This needs to be fleshed out a lot more.
 *
 * @return
 */
public enum DataType {

  /**
   * Dates, i.e. Continuous conventional time periods like specific days, minutes,
   * or years, identified on some calendar.
   */
  DATE(Date.class) {

    @Override
    public Date convertCycToJava(Object term) {
      final Date result;
      term = CycNaut.convertIfPromising(term);
      if (term instanceof CycNaut) {
        result = DateConverter.parseCycDate((CycNaut) term);
        //        } else if (term instanceof CycList) {
        //          result = dc.parseCycDate((CycList) term);
      } else {
        result = (Date) term;
      }
      return result;
    }

    @Override
    protected Object convertTypedJavaToCyc(Object term) throws ParseException {
      return DateConverter.toCycDate((Date) term);
    }
  },

  /**
   * Money, which has a currency type and a numeric value, e.g. $27.
   */
  MONEY(Money.class) {

    @Override
    public Money convertCycToJava(Object term) {
      final Money result;
      term = CycNaut.convertIfPromising(term);
      if (term instanceof CycNaut) {
        result = MoneyConverter.parseCycMoney((CycNaut) term);
      } else {
        result = (Money) term;
      }
      return result;
    }

    @Override
    protected Object convertTypedJavaToCyc(Object term) throws ParseException {
      return MoneyConverter.toCycMoney((Money) term);
    }
  },

  /**
   * ComparableNumber, which is a wrapper class for Numbers, and which allows
   * for comparison of different kinds of numbers (floats, ints, etc.)
   */
  NUMBER(ComparableNumber.class) {

    @Override
    public ComparableNumber convertCycToJava(Object term) {
      if (term instanceof ComparableNumber)
        return (ComparableNumber) term;
      else
        return new ComparableNumber((Number) term);
    }

    @Override
    protected Object convertTypedJavaToCyc(Object term) throws ParseException {
      if (term instanceof ComparableNumber)
        return ((ComparableNumber) term).getNumber();
      else
        return (Number) term;
    }
  },

  STRING(String.class) {

    @Override
    public String convertCycToJava(Object term) {
      return term.toString();
    }

    @Override
    protected Object convertTypedJavaToCyc(Object term) throws ParseException {
      return (String) term;
    }
  };

  /** Define an instance by its defining class. */
  <E extends Comparable<E>>     DataType(Class<E> myClass) {
    this.myClass = myClass;
  }

  public abstract Object convertCycToJava(final Object term);

  public Object convertJavaToCyc(final Object term) throws ParseException {
    if (!myClass.isInstance(term)) {
      throw new IllegalArgumentException(
              this + " can only convert objects of type " + myClass.getName() + ". "
              + term + " is of type " + term.getClass().getName());
    } else {
      return convertTypedJavaToCyc(myClass.cast(term));
    }
  }

  protected abstract Object convertTypedJavaToCyc(final Object term) throws ParseException;

  public int compareObjectsOfSameClass(Object obj1, Object obj2) {
    return myClass.cast(obj1).compareTo(myClass.cast(obj2));
  }
  private Class<? extends Comparable> myClass;
}
