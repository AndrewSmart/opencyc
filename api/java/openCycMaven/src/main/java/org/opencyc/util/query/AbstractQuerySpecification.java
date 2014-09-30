/* $Id: AbstractQuerySpecification.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util.query;

//// Internal Imports

//// External Imports
import java.util.*;

/**
 * <P>AbstractQuerySpecification is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author mreimers
 * @date August 11, 2004, 1:56 PM
 * @version $Id: AbstractQuerySpecification.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class AbstractQuerySpecification implements QuerySpecification {
  protected Set constraints;
  protected Object id;
  protected Object question;
  
  //// Constructors
  
  /** Creates a new instance of AbstractQuerySpecification. */
  public AbstractQuerySpecification(Object question, Set constraints) {
    this.question = question;
    this.constraints = constraints;
  }
  
  public Set getConstraints() {
    return constraints;
  }
  
  public java.util.Set getFilteredConstraints(Class constraintType) {
    HashSet result = new HashSet();
    return result;
//    if(constraints == null)
//      return result;
//    Iterator it = constraints.iterator();
  }
  
  public String getGloss() {
    return "";
  }
  
  public Object getQuestion() {
    return null;
  }
  
  public Object clone() {
    return null;
  }
  
  public Object getId() { return this.id; }
  public void setId(Object id) {
    this.id = id;
  }
  
  public void addQueryListener(QueryListener listener) {
    
  }
  
  public void removeQueryListener(QueryListener listener) {
    
  }
  
  public void setQueryResultSet(QueryResultSet resultSet) {
    
  }
  
  public QueryResultSet getQueryResultSet() {
    return null;
  }
  
  public void setQueryStatus(QueryStatus queryStatus) {
    
  }
  
  public QueryStatus getQueryStatus() {
    return null;
  }
  
  public void revertQuerySpecification() {
    
  }
  
  public void setQuerySpecification(QuerySpecification querySpecification) {
    
  }
  
  public QuerySpecification getQuerySpecification() {
    return null;
  }
  
  public QuerySpecification getOriginalQuerySpecification() {
    return null;
  }
  //// Public Area
  
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  //// Main
  
}
