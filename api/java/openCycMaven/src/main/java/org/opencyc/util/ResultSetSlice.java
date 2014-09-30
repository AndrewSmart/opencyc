package org.opencyc.util;

//  IMPORT_NON_CYCORP_PACKAGES

import java.sql.*;


/*****************************************************************************
 * A Cfaslable slice of a java.sql.ResultSet, used to send SubL portions of
 * a result set.
 * 
 * @version $Id: ResultSetSlice.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author
 *      Bjorn Aldag<BR>
 *      Copyright &copy; 2003 - 2006 Cycorp, Inc.  All rights reserved.
 *****************************************************************************/
public class ResultSetSlice {
  
  //// Instance Fields ////////////////////////////////////////////////////////
  
  /**
   * The java.sql.ResultSet underlying this ResultSetSlice.
   */
  private ResultSet rs;
  
  /**
   * The number of rows in the underlying result set.
   */
  private int rowCount;

  /**
   * The number of rows in this slice.
   */
  private int sliceRowCount;
  
  /**
   * The number of columns in the underlying result set.
   */
  private int columnCount;
  
  /**
   * The row id of the first row of this result slice.
   */
  private int first;
  
  /**
   * The row id of the last row of this result slice.
   */
  private int last;
  
  
  //// Constructors ///////////////////////////////////////////////////////////  
  
  /**
   * Creates a new ResultSetSlice that contains all rows from <code>lo</code> to 
   * <code>hi</code> (both inclusive) from the specified ResultSet. The 
   * current row of the underlying ResultSet is set to the row immediately 
   * before <code>lo</code>.
   * @param rs the ResultSet from which a slice is to be retrieved
   * @param lo the first row to be retrieved
   * @param hi the last row to be retrieved
   * @exception SQLException if an SQL error occurs.
   */
  public ResultSetSlice(java.sql.ResultSet rs, Integer lo, Integer hi) 
    throws SQLException {
    this.rs = rs;
    rowCount = rs.last() ? rs.getRow() : 0;
    columnCount = rs.getMetaData().getColumnCount();
    first = lo.intValue();
    last = Math.min(hi.intValue(), rowCount);
    sliceRowCount = last + 1 - first;
  }
  
  //// Instance Fields ////////////////////////////////////////////////////////

  /**
   * Returns the entire ResultSet that this object is a slice of.
   * @return the entire ResultSet that this object is a slice of.
   */
  public ResultSet resultSet() {return rs;}

  /**
   * Returns the number of rows of the ResultSet underlying this slice.
   * @return the number of rows of the ResultSet underlying this slice.
   */
  public int rowCount() {return rowCount;}

  /**
   * Returns the number of rows in this ResultSetSlice, not the total number
   * of rows in the underlying ResultSet.
   * @return the number of rows in this ResultSetSlice, not the total number
   * of rows in the underlying ResultSet.
   */
  public int sliceRowCount() {return sliceRowCount;}

  /**
   * Returns the number of columns of the ResultSet underlying this slice.
   * @return the number of columns of the ResultSet underlying this slice.
   */
  public int columnCount() {return columnCount;}

  /**
   * Returns the index of the first row of this ResultSetSlice, with respect
   * to the underlying ResultSet.
   * @return the index of the first row of this ResultSetSlice, with respect
   * to the underlying ResultSet.
   */
  public int first() {return first;}

  /**
   * Returns the index of the last row of this ResultSetSlice, with respect
   * to the underlying ResultSet.
   * @return the index of the last row of this ResultSetSlice, with respect
   * to the underlying ResultSet.
   */
  public int last() {return last;}


  //// Instance Methods //////////////////////////////////////////////////////////

  /**
   * Resets the current row of this ResultSetSlice to the one immediately before the
   * first row.
   */
  public void beforeFirst() throws SQLException {
    if (first == 1) {rs.beforeFirst();}
    else {rs.absolute(first - 1);}
  }

  /**
   * Returns the printed representation of this ResultSetSlice.
   * @return the printed representation of this ResultSetSlice.
   */
  public String toString() {
    StringBuffer string = new StringBuffer("(" + first + " (");
    try {
      beforeFirst();
      for (int row = first; row <= last; row++) {
	rs.next();
	string.append("(");
	for (int column = 1; column <= columnCount; column++) {
	  if (rs.getObject(column) == null) {string.append("NULL");}
	  else {string.append(rs.getObject(column).toString());}

	  if (! (column == columnCount)) {
	    string.append(" ");
	  }
	}
	string.append(")");
      }
    }
    catch (SQLException e) {
      throw new RuntimeException(e.getMessage());
    }
    string.append(") " + rowCount + ")");
    return new String(string);
  }
}
