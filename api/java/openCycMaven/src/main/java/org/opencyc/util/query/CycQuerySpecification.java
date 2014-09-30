/*
 * CycQuerySpecification.java
 *
 * Created on August 11, 2004, 10:49 AM
 */

package org.opencyc.util.query;

import org.opencyc.cycobject.CycList;

/**
 * @version $Id: CycQuerySpecification.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface CycQuerySpecification extends QuerySpecification {
  
  public CycList getQueryFormula();
}
