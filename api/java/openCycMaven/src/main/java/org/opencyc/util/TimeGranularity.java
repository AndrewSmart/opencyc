/* $Id: TimeGranularity.java 131068 2010-05-27 19:43:54Z baxter $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.util;

//// Internal Imports
import java.util.Calendar;
import java.util.Date;

//// External Imports
/** 
 * <P>TimeGranularity is designed to...
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : May 27, 2010, 1:27:34 PM
 * Author     : baxter
 * @version $Id: TimeGranularity.java 131068 2010-05-27 19:43:54Z baxter $
 */
public enum TimeGranularity {

  MILLISECOND(Calendar.MILLISECOND),
  SECOND(Calendar.SECOND),
  MINUTE(Calendar.MINUTE),
  HOUR(Calendar.HOUR_OF_DAY),
  DAY(Calendar.DAY_OF_MONTH),
  WEEK(Calendar.WEEK_OF_YEAR),
  MONTH(Calendar.MONTH),
  YEAR(Calendar.YEAR);

  //// Constructors
  /** Creates a new instance of TimeGranularity. */
  TimeGranularity(final int intValue) {
    this.intValue = intValue;
  }
  //// Public Area

  public int intValue() {
    return intValue;
  }

  public static TimeGranularity guessGranularity(final Date date) {
    return guessGranularity(date.getTime());
  }

  public static TimeGranularity guessGranularity(final long millis) {
    if (millis % MILLISECONDS_IN_A_SECOND != 0) {
      return MILLISECOND;
    } else if (millis % MILLISECONDS_IN_A_MINUTE != 0) {
      return SECOND;
    } else if (millis % MILLISECONDS_IN_AN_HOUR != 0) {
      return MINUTE;
    } else {
      return DAY;
    }
  }
  public static final int MILLISECONDS_IN_A_SECOND = 1000;
  public static final int SECONDS_IN_A_MINUTE = 60;
  public static final int MINUTES_IN_AN_HOUR = 60;
  public static final int MILLISECONDS_IN_A_MINUTE = MILLISECONDS_IN_A_SECOND * SECONDS_IN_A_MINUTE;
  public static final int MILLISECONDS_IN_AN_HOUR = MILLISECONDS_IN_A_MINUTE * MINUTES_IN_AN_HOUR;
  public static final long MILLISECONDS_IN_A_DAY = MILLISECONDS_IN_AN_HOUR * 24; // Except for start/end of daylight savings time, leap seconds, etc.
  public static final long MILLISECONDS_IN_A_WEEK = MILLISECONDS_IN_A_DAY * 7;
  public static final long MILLISECONDS_IN_A_NON_LEAP_YEAR = MILLISECONDS_IN_A_DAY * 365;
  public static final long MILLISECONDS_IN_A_LEAP_YEAR = MILLISECONDS_IN_A_DAY * 366;
  //// Protected Area
  //// Private Area
  //// Internal Rep
  private int intValue;
  //// Main
}
