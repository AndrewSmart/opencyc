/* $Id: CycNonAtomicTerm.java 131278 2010-06-28 13:51:40Z baxter $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.cycobject;

//// Internal Imports



//// External Imports
import java.util.List;

/** 
 * <P>CycNonAtomicTerm is designed to...
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Jul 6, 2009, 10:05:43 AM
 * Author     : baxter
 * @version $Id: CycNonAtomicTerm.java 131278 2010-06-28 13:51:40Z baxter $
 */
public interface CycNonAtomicTerm extends CycDenotationalTerm {
  
  abstract CycFort getFunctor();
  abstract List getArguments();
  abstract CycNaut getFormula();
  Object getArgument(final int argnum);

  /**
   * Returns a list representation of the OpenCyc NART.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  CycList toCycList();

  /**
   * Returns a list representation of the OpenCyc NART and expands any embedded NARTs as well.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  CycList toDeepCycList();

  public int getArity();
}
