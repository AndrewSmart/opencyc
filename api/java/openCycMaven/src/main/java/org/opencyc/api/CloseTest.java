/* $Id: CloseTest.java 128323 2009-07-14 22:45:32Z tbrussea $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.api;

//// Internal Imports

//// External Imports

/** 
 * <P>CloseTest is designed to...
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Jul 9, 2009, 11:59:43 AM
 * Author     : tbrussea
 * @version $Id: CloseTest.java 128323 2009-07-14 22:45:32Z tbrussea $
 */
public class CloseTest {

  //// Constructors

  /** Creates a new instance of CloseTest. */
  public CloseTest() {
  }

  //// Public Area

  //// Protected Area

  //// Private Area

  //// Internal Rep

  //// Main

   //This main method should quit without needing to call exit because of lingering threads.
  public static void main(String[] args) {
    System.out.println("Starting");
    System.out.flush();
    CycAccess access = null;
    try {
      access = new CycAccess("localhost", 3660); // @hack
    } catch (Exception e) {
      e.printStackTrace();
    } finally {
      try {
        access.close();
      } catch (Exception e) {} // ignore
      System.out.println("Finished.");
      System.out.flush();
    }
  }

}
