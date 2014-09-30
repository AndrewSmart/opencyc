/*
 * Query.java
 *
 * Created on August 11, 2004, 10:21 AM
 */

package org.opencyc.util.query;

/**
 * @version $Id: Query.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface Query {

  public Object getId();
  public QuerySpecification getOriginalQuerySpecification();
  public QuerySpecification getQuerySpecification();
  public void setQuerySpecification(QuerySpecification querySpecification);
  
  //public Query makeQuery(QuerySpecification querySpecification);
  
  /**
   * Revert this Query back to the original QuerySpecification.
   */
  public void revertQuerySpecification();
  
  public QueryStatus getQueryStatus();
  public void setQueryStatus(QueryStatus queryStatus);
  
  public QueryResultSet getQueryResultSet();
  public void setQueryResultSet(QueryResultSet resultSet);
  
  //public void notifyDataAvailable();
  //public void notifySpecificationChanged();
  //public void notifyStatusChanged();
  
  public void startQuery();
  public void stopQuery();
  public void pauseQuery();
  public void continueQuery();
  
  public void addQueryListener(QueryListener listener);
  public void removeQueryListener(QueryListener listener);
}
