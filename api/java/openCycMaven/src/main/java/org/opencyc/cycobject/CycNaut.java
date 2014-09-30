/* $Id: CycNaut.java 131492 2010-07-30 21:24:11Z baxter $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.cycobject;
//// External Imports
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

//// Internal Imports
import org.opencyc.util.DateConverter;

/** 
 * <P>CycNaut is designed to...
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Jul 6, 2009, 10:05:20 AM
 * Author     : baxter
 * @version $Id: CycNaut.java 131492 2010-07-30 21:24:11Z baxter $
 */
public class CycNaut extends CycFormula implements CycNonAtomicTerm, CycDenotationalTerm {

  //// Constructors
  /** Creates a new instance of CycNaut. */
  public CycNaut(final Iterable<Object> terms) {
    super(terms);
  }

  public CycNaut(final CycDenotationalTerm functor, final Object... args) {
    super(functor, args);
  }

  /** Convert term to a CycNaut if it looks like it ought to be one. */
  static public Object convertIfPromising(final Object term) {
    if (term instanceof List && !(term instanceof CycNaut)) {
      final List<Object> termAsList = (List) term;
      if (!termAsList.isEmpty() && termAsList.get(0) instanceof CycConstant) {
        final CycConstant arg0 = (CycConstant) termAsList.get(0);
        if (Character.isUpperCase(arg0.getName().charAt(0))) {
          return new CycNaut(termAsList);
        }
      }
    }
    return term;
  }

  //// Public Area
  public CycFort getFunctor() {
    return (CycFort) getOperator();
  }

  public CycNaut getFormula() {
    return this;
  }

  @Override
  public List getArguments() {
    return getArgsUnmodifiable().subList(1, getArity() + 1);
  }

  @Override
  public CycNaut deepCopy() {
    return new CycNaut(super.deepCopy().getArgsUnmodifiable());
  }

  /**
   * Returns <tt>true</tt> some object equals this <tt>CycNart</tt>
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  @Override
  public boolean equalsAtEL(Object object) {
    if (!(object instanceof CycNonAtomicTerm)) {
      return false;
    }
    CycNonAtomicTerm thatNAT = (CycNonAtomicTerm) object;
    if (getFunctor().equalsAtEL(thatNAT.getFunctor())
            && getArity() == thatNAT.getArity()) {
      for (int argNum = 1; argNum <= getArity(); argNum++) {
        final Object arg = getArgument(argNum);
        final Object thatArg = thatNAT.getArgument(argNum);
        if (arg.equals(thatArg)) {
          continue;
        } else if (arg instanceof CycFormula
                && ((CycFormula) arg).equalsAtEL(thatArg)) {
          continue;
        } else if (arg instanceof CycDenotationalTerm
                && ((CycDenotationalTerm) arg).equalsAtEL(thatArg)) {
          continue;
        } else {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  /**
   * Returns a list representation of the OpenCyc NAT.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  @Override
  public CycList toCycList() {
    CycList cycList = new CycList();
    final CycFort functor = getFunctor();
    if (functor instanceof CycNonAtomicTerm) {
      cycList.add(((CycNonAtomicTerm) functor).toCycList());
    } else {
      cycList.add(functor);
    }
    for (final Object argument : this.getArguments()) {
      cycList.add(argument);
    }
    return cycList;
  }

  /**
   * Returns a list representation of the OpenCyc NART and expands any embedded NARTs as well.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  @Override
  public CycList toDeepCycList() {
    CycList cycList = new CycList();
    final CycFort functor = getFunctor();
    if (functor instanceof CycNonAtomicTerm) {
      cycList.add(((CycNonAtomicTerm) functor).toDeepCycList());
    } else {
      cycList.add(functor);
    }
    getArguments();
    for (final Object argument : this.getArguments()) {
      if (argument instanceof CycNonAtomicTerm) {
        cycList.add(((CycNonAtomicTerm) argument).toDeepCycList());
      } else {
        cycList.add(argument);
      }
    }
    return cycList;
  }

  public Object getArgument(int argnum) {
    return getArg(argnum);
  }

  @Override
  public Object cycListApiValue() {
    return super.cycListApiValue();
  }

  /** @return the Date denoted by this term in the default time zone, if it denotes a Date, null otherwise. */
  public Date asDate() {
    return asDate(TimeZone.getDefault());
  }

  /** @return the Date denoted by this term in <code>timeZone</code>, if it denotes a Date, null otherwise. */
  public Date asDate(final TimeZone timeZone) {
    return (isDate()) ? lookupOrComputeDate(timeZone) : null;
  }

  /** @return true iff this CycNaut is a standard CycL date. */
  public boolean isDate() {
    if (dateStatus == null) {
      computeDateStatus();
    }
    return dateStatus;
  }

  /** @return the Quantity denoted by this term, if it denotes a Quantity, null otherwise. */
  public CycQuantity asQuantity() {
    return (isQuantity()) ? quantity : null;
  }

  /** @return true iff this CycNaut is a standard CycL quantity. */
  public boolean isQuantity() {
    if (quantityStatus == null) {
      computeQuantityStatus();
    }
    return quantityStatus;
  }

  //// Private Area
  private Date lookupOrComputeDate(final TimeZone timeZone) {
    if (dates != null && dates.containsKey(timeZone)) {
      return dates.get(timeZone);
    } else {
      final Date date = DateConverter.parseCycDate(this, timeZone, false);
      if (date != null) {
        if (dates == null) {
          dates = new HashMap<TimeZone, Date>();
        }
        dates.put(timeZone, date);
      }
      return date;
    }
  }

  private void computeDateStatus() {
    lookupOrComputeDate(TimeZone.getDefault());
    dateStatus = (dates != null);
  }
  private Map<TimeZone, Date> dates = null;
  private Boolean dateStatus = null;

  private void computeQuantity() {
    quantity = CycQuantity.valueOf(this);
  }

  private void computeQuantityStatus() {
    computeQuantity();
    quantityStatus = (quantity != null);
  }
  private CycQuantity quantity = null;
  private Boolean quantityStatus = null;
  //// Protected Area
  //// Internal Rep
  //// Main
}
