/* $Id: DateConverter.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2007 Cycorp, Inc.  (Copyright is assigned to the United States Government under DFARS 252.227-7020).
 */
package org.opencyc.util;

//// External Imports
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

//// Internal Imports
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycNaut;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.Guid;

/**
 * <P>DateConverter is designed to convert java-style dates to their corresponding
 * CycL representations and vice versa.
 *
 * <P>Copyright (c) 2007 Cycorp, Inc.  (Copyright is assigned to the United States Government under DFARS 252.227-7020).
 *
 *
 * @author baxter
 * @date January 15, 2008, 5:33 PM
 * @version $Id: DateConverter.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DateConverter extends DataTypeConverter<Date> {

  public static final int DAY_GRANULARITY = TimeGranularity.DAY.intValue();
  public static final int HOUR_GRANULARITY = TimeGranularity.HOUR.intValue();
  public static final int MILLISECOND_GRANULARITY = TimeGranularity.MILLISECOND.intValue();
  public static final int MINUTE_GRANULARITY = TimeGranularity.MINUTE.intValue();
  public static final int MONTH_GRANULARITY = TimeGranularity.MONTH.intValue();
  public static final int SECOND_GRANULARITY = TimeGranularity.SECOND.intValue();
  public static final int WEEK_GRANULARITY = TimeGranularity.WEEK.intValue();
//  public static final int SEASON_GRANULARITY = TimeGranularity.SEASON.intValue();
  public static final int YEAR_GRANULARITY = TimeGranularity.YEAR.intValue();

  //// Constructors
  /** Creates a new instance of DateConverter. */
  private DateConverter() {
    SHARED_INSTANCE = this;
  }

  //// Public Area
  /** Returns an instance of <code>DateConverter</code>.
   *
   * If an instance has already been created, the existing one will be returned.
   * Otherwise, a new one will be created.
   * @return The singleton instance of this class.
   */
  public static DateConverter getInstance() {
    DateConverter dateConverter = SHARED_INSTANCE;
    if (dateConverter == null) {
      dateConverter = new DateConverter();
    }
    return dateConverter;
  }

  /** Try to parse <code>cycList</code> into a java <code>Date</code>
   *
   * If the parse fails, prints a stack trace iff <code>shouldReportFailure</code>
   * is non-null, and returns null.
   * 
   * The Cyc date is assumed to be in the default time zone.
   * @see TimeZone#getDefault
   * @deprecated Use CycNaut version.
   */
  static public Date parseCycDate(final CycList cycList, final boolean shouldReportFailure) {
    final Object naut = CycNaut.convertIfPromising(cycList);
    if (naut instanceof CycNaut) {
      return parseCycDate((CycNaut) naut, shouldReportFailure);
    } else if (shouldReportFailure) {
      new IllegalArgumentException(cycList + " cannot be interpreted as a NAUT").printStackTrace();
    }
    return null;
  }

  /** Try to parse <code>naut</code> into a java <code>Date</code>
   *
   * @param naut a date-denoting Cyc NAUT.
   * @param shouldReportFailure If true, and the parse fails, prints a stack trace.
   * @return the Date object corresponding to naut, or null if parse fails.
   * 
   * The Cyc date is assumed to be in the default time zone.
   * @see TimeZone#getDefault
   */
  static public Date parseCycDate(final CycNaut naut, final boolean shouldReportFailure) {
    return getInstance().parse(naut, shouldReportFailure);
  }

  /** Try to parse <code>cycList</code> into a java <code>Date</code> in a given time zone.
   *
   * If the parse fails, prints a stack trace iff <code>shouldReportFailure</code>
   * is non-null, and returns null.
   * @deprecated Use CycNaut version.
   */
  static public Date parseCycDate(final CycList cycList, final TimeZone timeZone, final boolean shouldReportFailure) {
    final Object naut = CycNaut.convertIfPromising(cycList);
    if (naut instanceof CycNaut) {
      return parseCycDate((CycNaut) naut, timeZone, shouldReportFailure);
    } else if (shouldReportFailure) {
      new IllegalArgumentException(cycList + " cannot be converted to a NAUT.").printStackTrace();
    }
    return null;
  }

  /** Try to parse <code>naut</code> into a java <code>Date</code> in a given time zone.
   *
   * If the parse fails, prints a stack trace iff <code>shouldReportFailure</code>
   * is non-null, and returns null.
   * @param naut 
   * @param timeZone 
   * @param shouldReportFailure 
   * @return
   */
  static public Date parseCycDate(final CycNaut naut, final TimeZone timeZone, final boolean shouldReportFailure) {
    try {
      return naut2Date(naut, timeZone);
    } catch (ParseException ex) {
      return getInstance().handleParseException(ex, shouldReportFailure);
    }
  }

  /** Try to parse <code>cycList</code> into a java <code>Date</code>
   *
   * Prints stack trace and returns null if the parse fails.
   * 
   * The Cyc date is assumed to be in the default time zone.
   * @see TimeZone#getDefault
   * @deprecated Use CycNaut version.
   */
  static public Date parseCycDate(final CycList cycList) {
    return getInstance().parse(cycList);
  }

  /** Try to parse <code>naut</code> into a java <code>Date</code>
   *
   * Prints stack trace and returns null if the parse fails.
   * @param naut 
   * @return the corresponding Date object.
   * 
   * The Cyc date is assumed to be in the default time zone.
   * @see TimeZone#getDefault
   */
  static public Date parseCycDate(final CycNaut naut) {
    return getInstance().parse(naut);
  }

  /** @return the precision of <tt>cycDate</tt> as a Calendar constant int.
   * @deprecated Use CycNaut version.
   */
  public static int getCycDatePrecision(CycList cycDate) {
    return getCycDatePrecision(new CycNaut(cycDate));
  }

  /**
   * @param cycDate a date-denoting Cyc NAUT.
   * @return the precision of <tt>cycDate</tt> as a Calendar constant int. */
  public static int getCycDatePrecision(CycNaut cycDate) {
    final Object fn = cycDate.getOperator();
    if (YEAR_FN.equals(fn)) {
      return YEAR_GRANULARITY;
    }
    if (MONTH_FN.equals(fn)) {
      return MONTH_GRANULARITY;
    }
    if (DAY_FN.equals(fn)) {
      return DAY_GRANULARITY;
    }
    if (HOUR_FN.equals(fn)) {
      return HOUR_GRANULARITY;
    }
    if (MINUTE_FN.equals(fn)) {
      return MINUTE_GRANULARITY;
    }
    if (SECOND_FN.equals(fn)) {
      return SECOND_GRANULARITY;
    }
    if (MILLISECOND_FN.equals(fn)) {
      return MILLISECOND_GRANULARITY;
    }
    return -1;
  }

  /** Convert the date in <code>date</code> to a CycL date term.
   *
   * @param date The date to be converted
   * @param granularity Indicates the desired granularity of the CycL term.
   *        Should be an <code>int</code> constant from this class,
   *        e.g. <code>DateConverter.YEAR_GRANULARITY</code> or <code>DateConverter.SECOND_GRANULARITY</code>.
   * @return The Cyc term corresponding to date.
   * @throws ParseException if date cannot be converted.
   **/
  public static CycNaut toCycDate(final Date date, final int granularity) throws ParseException {
    return date2Naut(date, granularity);
  }

  public static int guessGranularity(final Date date) {
    return TimeGranularity.guessGranularity(date).intValue();
  }

  public static int guessGranularity(final long millis) {
    return TimeGranularity.guessGranularity(millis).intValue();
  }

  /** Convert the date in <code>date</code> to a CycL date term.
   *
   * @param date The date to be converted
   * @return The Cyc term corresponding to date.
   * @throws ParseException if date cannot be converted.
   **/
  public static CycObject toCycDate(Date date) throws ParseException {
    return date2Naut(date, guessGranularity(date));
  }

  /** Convert the date in <code>calendar</code> to a CycL date term.
   *
   * @param calendar 
   * @param granularity Indicates the desired granularity of the CycL term.
   *        Should be an <code>int</code> constant from this class,
   *        e.g. <code>DateConverter.YEAR_GRANULARITY</code> or <code>DateConverter.SECOND_GRANULARITY</code>.
   * @return
   **/
  public static CycNaut toCycDate(final Calendar calendar, final int granularity) {
    return calendar2Naut(calendar, granularity);
  }

  //// Protected Area
  @Override
  protected Date parseDataType(final CycNaut naut) throws ParseException {
    final Calendar calendar = Calendar.getInstance();
    calendar.clear();
    updateCalendar(naut, calendar);
    return calendar.getTime();
  }

  @Override
  protected CycNaut toCycTerm(Date date) throws ParseException {
    return date2Naut(date, guessGranularity(date));
  }

  //// Private Area
  static private CycNaut date2Naut(Date date, final int granularity) throws ParseException {
    final Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    return calendar2Naut(calendar, granularity);
  }

  private static CycNaut calendar2Naut(final Calendar calendar, final int granularity) {
    CycNaut dateNaut = new CycNaut(YEAR_FN, calendar.get(YEAR_GRANULARITY));
//      if (granularity == WEEK_GRANULARITY) {
//        dateNaut = new CycNaut(WEEK_FN, calendar.get(WEEK_GRANULARITY), dateNaut);
//      } else
    if (granularity > YEAR_GRANULARITY) {
      dateNaut = new CycNaut(MONTH_FN, lookupMonth(calendar.get(MONTH_GRANULARITY)), dateNaut);
        if (granularity > MONTH_GRANULARITY) {
        dateNaut = new CycNaut(DAY_FN, calendar.get(DAY_GRANULARITY), dateNaut);
        if (granularity > DAY_GRANULARITY) {
          dateNaut = new CycNaut(HOUR_FN, calendar.get(HOUR_GRANULARITY), dateNaut);
          if (granularity > Calendar.HOUR) {
            dateNaut = new CycNaut(MINUTE_FN, calendar.get(MINUTE_GRANULARITY), dateNaut);
            if (granularity > MINUTE_GRANULARITY) {
              dateNaut = new CycNaut(SECOND_FN, calendar.get(SECOND_GRANULARITY), dateNaut);
              if (granularity > SECOND_GRANULARITY) {
                dateNaut = new CycNaut(MILLISECOND_FN, calendar.get(MILLISECOND_GRANULARITY), dateNaut);
              }
            }
          }
        }
      }
    }
    return dateNaut;
  }

  private static Date naut2Date(final CycNaut naut, final TimeZone timeZone) throws ParseException {
    return naut2Calendar(naut, timeZone).getTime();
  }

  private static Calendar naut2Calendar(final CycNaut naut, final TimeZone timeZone) throws ParseException {
    final Calendar calendar = Calendar.getInstance();
    calendar.clear();
    updateCalendar(naut, calendar);
    calendar.set(Calendar.ZONE_OFFSET, timeZone.getRawOffset());
    return calendar;
  }

  /** Set the time on <code>calendar</code> based on the CycL date <code>naut</code> */
  static private void updateCalendar(final CycNaut naut, final Calendar calendar) throws ParseException {
    final int arity = naut.getArity();
    final CycFort functor = naut.getFunctor();
    if (arity < 1 || arity > 2) {
      throwParseException(naut);
    }
    final Object arg1 = naut.getArg(1);
    if (arity == 1 && YEAR_FN.equals(functor)) {
      final Integer yearNum = parseInteger(arg1, "year number");
      calendar.set(YEAR_GRANULARITY, yearNum);
    } else if (arity == 1) {
      throwParseException(naut);
    } else {
      final Object arg2 = naut.getArg(2);
      if (!(arg2 instanceof CycNaut)) {
        throwParseException(arg2);
      }
      if (MONTH_FN.equals(functor)) {
        if (!(arg1 instanceof CycConstant)) {
          throw new ParseException(arg1 + " is not a valid CycL month.");
        }
        final int monthNum = lookupMonthNum((CycConstant) arg1);
        if (monthNum < Calendar.JANUARY || monthNum > Calendar.DECEMBER) {
          throw new ParseException(arg1 + " is not a valid CycL month.");
        }
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(MONTH_GRANULARITY, monthNum);
      } else if (DAY_FN.equals(functor)) {
        final Object dayNum = parseInteger(arg1, "day number");
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(DAY_GRANULARITY, (Integer) dayNum);
      } else if (HOUR_FN.equals(functor)) {
        final Object hourNum = parseInteger(arg1, "hour number");
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(HOUR_GRANULARITY, (Integer) hourNum);
      } else if (MINUTE_FN.equals(functor)) {
        final Object minuteNum = parseInteger(arg1, "minute number");
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(MINUTE_GRANULARITY, (Integer) minuteNum);
      } else if (SECOND_FN.equals(functor)) {
        final Object secondNum = Integer.valueOf(arg1.toString());
        if (!(secondNum instanceof Integer && (Integer) secondNum >= 0
                && (Integer) secondNum < TimeGranularity.SECONDS_IN_A_MINUTE)) {
          throw new ParseException(secondNum + " is not a valid second number.");
        }
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(SECOND_GRANULARITY, (Integer) secondNum);
      } else if (MILLISECOND_FN.equals(functor)) {
        final Object millisecondNum = parseInteger(arg1, "millisecond number");
        updateCalendar((CycNaut) arg2, calendar);
        calendar.set(MILLISECOND_GRANULARITY, (Integer) millisecondNum);
      } else {
        throwParseException(naut);
      }
    }
  }

  static public CycConstant lookupSeason(final String season) {
    if (season.equals("SU")) {
      return SUMMER;
    } else if (season.equals("SP")) {
      return SPRING;
    } else if (season.equals("FA")) {
      return FALL;
    } else if (season.equals("WI")) {
      return WINTER;
    } else return null;
  }

  static private CycConstant lookupMonth(final int month) {
    ensureMonthArrayInitialized();
    return CYC_MONTH_TERMS[month];
  }

  static private int lookupMonthNum(CycConstant cycMonth) {
    ensureMonthArrayInitialized();
    for (int monthNum = Calendar.JANUARY; monthNum <= Calendar.DECEMBER; monthNum++) {
      if (cycMonth.equals(CYC_MONTH_TERMS[monthNum])) {
        return monthNum;
      }
    }
    return -1;
  }

  private static void ensureMonthArrayInitialized() {
    if (CYC_MONTH_TERMS == null) {
      initializeCycMonthTerms();
    }
  }

  private static void initializeCycMonthTerms() {
    CYC_MONTH_TERMS = new CycConstant[12];
    CYC_MONTH_TERMS[Calendar.JANUARY] = JANUARY;
    CYC_MONTH_TERMS[Calendar.FEBRUARY] = FEBRUARY;
    CYC_MONTH_TERMS[Calendar.MARCH] = MARCH;
    CYC_MONTH_TERMS[Calendar.APRIL] = APRIL;
    CYC_MONTH_TERMS[Calendar.MAY] = MAY;
    CYC_MONTH_TERMS[Calendar.JUNE] = JUNE;
    CYC_MONTH_TERMS[Calendar.JULY] = JULY;
    CYC_MONTH_TERMS[Calendar.AUGUST] = AUGUST;
    CYC_MONTH_TERMS[Calendar.SEPTEMBER] = SEPTEMBER;
    CYC_MONTH_TERMS[Calendar.OCTOBER] = OCTOBER;
    CYC_MONTH_TERMS[Calendar.NOVEMBER] = NOVEMBER;
    CYC_MONTH_TERMS[Calendar.DECEMBER] = DECEMBER;
  }

  public static boolean isCycDate(Object object) {
    if (object instanceof CycList) {
      return parseCycDate((CycList) object, false) != null;
    } else if (object instanceof CycNaut) {
      return parseCycDate((CycNaut) object, false) != null;
    } else {
      return false;
    }
  }
  //// Internal Rep
  public static final CycConstant YEAR_FN = new CycConstant("YearFn",
          new Guid("bd58f29a-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant SEASON_FN = new CycConstant("SeasonFn",
          new Guid("c0fbe0cd-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant MONTH_FN = new CycConstant("MonthFn",
          new Guid("be00fd8d-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant DAY_FN = new CycConstant("DayFn",
          new Guid("be00ff5b-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant HOUR_FN = new CycConstant("HourFn",
          new Guid("be010082-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant MINUTE_FN = new CycConstant("MinuteFn",
          new Guid("be0100d7-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant SECOND_FN = new CycConstant("SecondFn",
          new Guid("be01010a-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant MILLISECOND_FN = new CycConstant("MillisecondFn",
          new Guid("8c3206d3-1616-11d8-99b1-0002b361bcfc"));
  public static final CycConstant JANUARY = new CycConstant("January",
          new Guid("bd58b833-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant FEBRUARY = new CycConstant("February",
          new Guid("bd58c2f7-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant MARCH = new CycConstant("March",
          new Guid("bd58c2bd-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant APRIL = new CycConstant("April",
          new Guid("bd58c279-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant MAY = new CycConstant("May",
          new Guid("bd58c232-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant JUNE = new CycConstant("June",
          new Guid("bd58c1f0-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant JULY = new CycConstant("July",
          new Guid("bd58c1ad-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant AUGUST = new CycConstant("August",
          new Guid("bd58c170-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant SEPTEMBER = new CycConstant("September",
          new Guid("bd58c131-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant OCTOBER = new CycConstant("October",
          new Guid("bd58c0ef-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant NOVEMBER = new CycConstant("November",
          new Guid("bd58c0a5-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant DECEMBER = new CycConstant("December",
          new Guid("bd58b8ba-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant SPRING = new CycConstant("CalendarSpring",
          new Guid("be011735-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant SUMMER = new CycConstant("CalendarSummer",
          new Guid("be011768-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant FALL = new CycConstant("CalendarAutumn",
          new Guid("be011790-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant WINTER = new CycConstant("CalendarWinter",
          new Guid("be0116f3-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant NTH_SPECIFIED_DATE_TYPE_OF_SUBSUMING_DATE_FN =
          new CycConstant("NthSpecifiedDateTypeOfSubsumingDateFn",
          new Guid("fa33c621-7b6f-4eeb-9801-3acb990b0c8f"));
  public static final CycConstant CALENDAR_WEEK = new CycConstant("CalendarWeek",
          new Guid("bd58c064-9c29-11b1-9dad-c379636f7270"));
  private static CycConstant[] CYC_MONTH_TERMS = null;
  private static DateConverter SHARED_INSTANCE = null;
  //// Main
}
