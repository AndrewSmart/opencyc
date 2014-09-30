/* $Id: CachedValue.java 130515 2010-04-01 23:13:26Z tbrussea $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util;

//// Internal Imports

//// External Imports

/** 
 * <P>CachedValue is designed to...
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Apr 1, 2010, 3:32:56 PM
 * Author     : tbrussea
 * @version $Id: CachedValue.java 130515 2010-04-01 23:13:26Z tbrussea $
 */
public class CachedValue<V> {

  CachedValue(V payload, long keepAliveTimeMsecs) {
    if (payload == null) {
      throw new IllegalArgumentException("Cannot accept null values.");
    }
    if (keepAliveTimeMsecs < 0) {
      throw new IllegalArgumentException("Invalid keepAliveTime" + keepAliveTimeMsecs);
    }
    this.payload = payload;
    this.timeEntered = System.currentTimeMillis();
    this.keepAliveTimeMsecs = keepAliveTimeMsecs;
  }

  public V get() {
    return payload;
  }

  public long getEntryTime() {
    return timeEntered;
  }

  public boolean isExpired() {
    if (keepAliveTimeMsecs == 0) {
      return false;
    }
    return ((System.currentTimeMillis() - keepAliveTimeMsecs) >= timeEntered);
  }

  @Override
  public boolean equals(Object o) {
    if (o == null) { // fast fail
      return false;
    }
    if ((o == this) || (o == payload)) { // fast success
      return true;
    }
    if (!(o instanceof CachedValue)) { // cope with non-cached values
      return o.equals(payload);
    }
    return ((CachedValue) o).payload.equals(payload); // cope with cached values
  }

  @Override
  public String toString() {
    return "" + payload;
  }

  @Override
  public int hashCode() {
    return payload.hashCode();
  }

  private long timeEntered;
  private long keepAliveTimeMsecs;
  private V payload;
}
