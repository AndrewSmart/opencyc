/* $Id: Entry.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util;

//// Internal Imports

//// External Imports

/**
 * <P>Entry is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author mreimers
 * @date August 11, 2004, 1:00 PM
 * @version $Id: Entry.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface Entry {
  
  public Object getKey();
  public Object getValue();
  
}
