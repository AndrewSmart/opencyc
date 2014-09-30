/*
 * Query.java
 *
 * Created on August 10, 2004, 2:04 PM
 */

package org.opencyc.util.query;

import java.util.Set;

/**
 * @version $Id: QuerySpecification.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface QuerySpecification {
  
  public String getGloss();
  
  public Object getQuestion();
  
  public Set getConstraints();
  public Set getFilteredConstraints(Class constraintType);
  
  public Object clone();
}
