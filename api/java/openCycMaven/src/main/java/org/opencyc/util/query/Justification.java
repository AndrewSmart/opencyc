/*
 * Justification.java
 *
 * Created on August 10, 2004, 2:06 PM
 */

package org.opencyc.util.query;

/**
 * @version $Id: Justification.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface Justification {
  public QueryResultSet getQueryResultSet();
  public int getQueryResultSetIndex();
  public String toPrettyString();
}
