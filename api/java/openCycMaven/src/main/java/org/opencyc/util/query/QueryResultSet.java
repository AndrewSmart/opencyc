/*
 * ResultSet.java
 *
 * Created on August 10, 2004, 2:04 PM
 */

package org.opencyc.util.query;

import java.util.Date;
import java.util.Iterator;

/**
 * @version $Id: QueryResultSet.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface QueryResultSet {
  public Iterator<QueryResult> getResultSetIterator();
  public Query getQuery();
  public Date getTimeStamp();
  public void addQueryResult(QueryResult queryResult);
  public Justification getJustificationForIndex(int i);
  
}
