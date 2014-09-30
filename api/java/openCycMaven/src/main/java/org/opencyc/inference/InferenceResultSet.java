 /* <p>Copyright 2010 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.opencyc.inference;

//// External Imports
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.cycobject.*;

/**
 * <P>InferenceResultSet provides a class for easy access and manipulation
 * of inference results. It tries to closely mimic the java.sql.ResultSet class.
 * One difference between java.sql.ResultSet and InferenceResultSet is that
 * InferenceResultSet is zero-based while java.sql.ResultSet is one-based. Example:
 *
 * <code><pre>
 *  System.out.println("Starting");
 *  CycAccess access = null;
 *  try {
 *    access = new CycAccess("localhost", 3660);
 *    ELMt inferencePSC = access.makeELMt("#$InferencePSC");
 *    String query = "(#$isa ?X #$Dog)";
 *    InferenceWorkerSynch worker = new DefaultInferenceWorkerSynch(query,
 *      inferencePSC, null, access, 500000);
 *    InferenceResultSet rs = worker.executeQuery();
 *    try {
 *      int indexOfX = rs.findColumn("?X");
 *      while (rs.next()) {
 *        CycObject curDog = rs.getCycObject(indexOfX);
 *        System.out.println("Got dog: " + curDog.cyclify());
 *      }
 *    } finally {
 *      rs.close();
 *    }
 *  } catch (Exception e) {
 *    e.printStackTrace();
 *  } finally {
 *    if (access != null) {
 *      access.close();
 *    }
 *  }
 *  System.out.println("Finished");
 *  System.out.flush();
 * </pre></code>
 *
 * <p>Copyright 2010 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * @author tbrussea
 * @date Mar 22, 2010, 11:55 AM
 * @version $Id: InferenceResultSet.java 139950 2012-05-07 18:15:54Z tbrussea $
 */
@SuppressWarnings("deprecation")
public final class InferenceResultSet extends AbstractResultSet implements ResultSet {

  //// Constructors

  /**
   * Creates a new <code>InferenceResultSet</code>.
   *
   * @param results The inference results as returned by "new-cyc-query".
   * @param inferenceWorker The inference worker that produced this result set.
   */
  public InferenceResultSet(List<Object> results, InferenceWorker inferenceWorker) {
    calcRows(results);
    this.inferenceWorker = inferenceWorker;
  }

  /**
   * Creates a new <code>InferenceResultSet</code>.
   *
   * @param results The inference results as returned by "new-cyc-query".
   */
  public InferenceResultSet(List<Object> results) {
    calcRows(results);
    this.inferenceWorker = null;
  }

  //// Public Area

  /**
   * Returns whether the <code>InferenceWorker</code> associated with this
   * <code>InferenceResultSet</code> object is finished working. Will return
   * <code>true</code> if there is no <code>InferenceWorker</code>.
   *
   * @return <code>true</code> if the <code>InferenceWorker</code> associated with this
   * <code>InferenceResultSet</code> or if the <code>InferenceWorker</code> is not set;
   * <code>false</code> if the <code>InferenceWorker</code> associated with this
   * <code>InferenceResultSet</code> might possibly produce more results.
   */
  public boolean isInferenceComplete() {
    if (inferenceWorker != null) {
      return inferenceWorker.isDone();
    }
    return true;
  }

  public InferenceIdentifier getInferenceIdentifier() {
    if (inferenceWorker != null) {
      return inferenceWorker.getInferenceIdentifier();
    }
    return null;
  }

  /**
   * Releases this <code>InferenceResultSet</code> object's server side
   * inference resources. Failure to close a result set may leave significant
   * resources hanging around the server until the client <code>CycAccess</code>
   * object is closed.
   * <P>
   * Calling the method <code>close</code> on a <code>InferenceResultSet</code>
   * object that is already closed is a no-op.
   *
   * @exception IOException if a communication error occurs
   * @exception TimeOutException if doesn't get a response from
   *            the server before an excessive amount of time has elapsed
   * @exception CycApiException if an internal server error occurs
   */
  @Override
  public synchronized void close() {
    if (isClosed()) {
      return;
    }
    setIsClosed(true);
    if (inferenceWorker != null) {
      try {
        inferenceWorker.releaseInferenceResources(MSECS_TO_WAIT_FOR_CLOSE);
      } catch (IOException ioe) {
        throw new RuntimeException(ioe.getMessage(), ioe);
      }
    }
  }

  /**
   * Returns, as an <code>Object</code>, the value at the current row and at the 
   * column identified by <code>col</code> which should be a 
   * <code>CycVariable</code> in the original query. Returns </code>null<code> 
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as an <code>Object</code>, at the current row and 
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   */
  public Object getObject(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException {
    return getObject(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycDenotationalTerm</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a 
   * <code>CycVariable</code> in the original query. Returns </code>null<code> 
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycDenotationalTerm</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycDenotationalTerm</code>
   */
  public CycDenotationalTerm getDenotationalTerm(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getDenotationalTerm(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>String</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>String</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>String</code>
   */
  public String getString(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getString(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>long</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>long</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>long</code>
   */
  public long getLong(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getLong(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>int</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>int</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>int</code>
   */
  public int getInt(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getInt(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>double</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>double</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>double</code>
   */
  public double getDouble(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getDouble(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>float</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>float</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>float</code>
   */
  public float getFloat(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getFloat(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>boolean</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>boolean</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>boolean</code>
   */
  public boolean getBoolean(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getBoolean(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycConstant</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycConstant</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycConstant</code>
   */
  public CycConstant getConstant(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getConstant(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycObject</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycObject</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycObject</code>
   */
  public CycObject getCycObject(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getCycObject(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycFort</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycFort</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycFort</code>
   */
  public CycFort getFort(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getFort(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycList</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycList</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycList</code>
   */
  public CycList<Object> getList(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getList(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>v</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycNart</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycNart</code>
   */
  public CycNart getNart(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getNart(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>CycNaut</code>, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>CycNaut</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>CycNaut</code>
   */
  public CycNaut getNaut(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getNaut(findColumnStrict(col));
  }

  /**
   * Returns, as a <code>java.util.Date</code> object, the value at the current row and at the
   * column identified by <code>col</code> which should be a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>
   * if no value is set for the current row and given column.
   *
   * <p/><strong>Note:</strong> Use the method <code>int colIindex = findColumn(col)<code> once
   * and the version of this method that takes an integer for maximum performance.
   *
   * @param col the name of the variable that represents the column of interest
   * @return the value, as a <code>java.util.Date</code>, at the current row and
   * at the column identified by the <code>col</code> which is a
   * <code>CycVariable</code> in the original query. Returns </code>null<code>,
   * if no value is set for the current row and given column.
   * @note Fails on dates that are not CycNauts using #$YearFn (i.e. it will not
   * work on skolemized dates, or other forms of dates that don't use the #$YearFn vocabulary).
   * @exception CycApiException if called on a closed result set
   * @exception IllegalArgumentException if <code>col</code> is not valid
   * @exception ArrayIndexOutOfBoundsException if the current cursor is not on a valid row
   * @exception ClassCastException if the value is not convertible to a
   * <code>java.util.Date</code>
   */
  public Date getDate(CycVariable col)
      throws CycApiException, IllegalArgumentException, ArrayIndexOutOfBoundsException, ClassCastException {
    return getDate(findColumnStrict(col));
  }

  
  
  /**
   * Returns the one-based column index for <code>col</code>.
   *
   * @param col the column variable to look up
   * @return the column index for the given <code>col</code> variable.
   * @exception IllegalArgumentException if called with an invalid <code>col</code>
   */
  public int findColumn(CycVariable col) throws IllegalArgumentException {
    return findColumn(col.toString());
  }

  //// Protected Area

  /**
   * Returns the one-based column index for the given <code>col</code>. This version differs
   * from the non-strict version in that it throws detailed error messages.
   *
   * @param col the index name to look up
   * @return the column index for the given <code>col</code
   * @exception IllegalArgumentException if called with an invalid <code>col</code>
   */
  protected int findColumnStrict(CycVariable col) throws IllegalArgumentException {
    if (col == null) {
      throw new IllegalArgumentException("Got null column name.");
    }
    return findColumnStrict(col.toString());
  }

  /**
   * Processes the results from "new-cyc-query" to store them conveniently in
   * this InferenceResultSet.
   *
   * @param results the results from "new-cyc-query"
   */
  @SuppressWarnings("unchecked")
  protected void calcRows(List results) {
    if (results.size() == 0){
      setTruthValue(Boolean.FALSE);
      return;
    }
    if (results.size() == 1) {
      Object val = results.get(0);
      if ((val instanceof CycSymbol) && (((CycSymbol)val).toString().equalsIgnoreCase("nil"))) {
        setTruthValue(Boolean.TRUE);
        return;
      }
    }
    for (List<CycList> bindingSet : (List<List>)results) {
      List<Object> row = addEmptyRow();
      for (CycList binding : bindingSet) {
        CycVariable colVar = (CycVariable)binding.get(0);
        int colIndex = possiblyAddColVar(colVar);
        String col = colVar.toString();
        Object val = binding.rest();
        row.set(colIndex, val);
      }
    }
  }

  /**
   * If the given column name isn't known, add it and make sure
   * current results have an extra column added. Return the new
   * (or existing) column index for the <code>colVar</code>.
   *
   * @return the column index of <code>colVar</code>
   * @param colVar the CycVariable which represents the column to add
   */
  protected int possiblyAddColVar(CycVariable colVar) {
    String col = colVar.toString();
    int colIndex = -1;
    List<String> columnNames = getColumnNamesUnsafe();
    if ((colIndex = columnNames.indexOf(col)) < 0) {
      columnNames.add(col);
      for (List<Object> row : getRS()) {
        row.add(null);
      }
      return columnNames.size() - 1;
    }
    return colIndex;
  }

  /**
   * Wait until the inference work completes.
   */
  @Override
  protected void waitTillProcessingDone() {
    if (inferenceWorker == null) {
      return;
    }
    if (inferenceWorker.isDone()) {
      return;
    }
    while (true) {
      synchronized (inferenceWorker) {
        try {
          inferenceWorker.wait(10);
        } catch (InterruptedException ie) {
          // @todo set warning
          return;
        }
        if (inferenceWorker.isDone()) {
          return;
        }
      }
    }
  }

  //// Private Area

  //// Internal Rep

  /** Maximum time to wait in milliseconds when closing the inference worker. */
  private static final long MSECS_TO_WAIT_FOR_CLOSE = 10000;

  /** The inference worker for whose results this InferenceResultSet represents.
   * The value may be null
   */
  private final InferenceWorker inferenceWorker;

  //// Main
  
  /**
   * Provides a working demonstration and sanity check main method.
   *
   * @param args the command line arguments (ignored)
   */
  public static void main(String[] args) {
    System.out.println("Starting");
    CycAccess access = null;
    try {
      access = new CycAccess("public1", 3660);
      String query = "(#$and (#$isa ?X #$Dog) (#$isa ?Y #$Cat))";
      InferenceWorkerSynch worker = new DefaultInferenceWorkerSynch(query,
        CycAccess.inferencePSC, null, access, 50000);
      InferenceResultSet rs = worker.executeQuery();
      try {
        int indexOfX = rs.findColumn("?X");
        int indexOfY = rs.findColumn("?Y");
        while (rs.next()) {
          CycObject curDog = rs.getCycObject(indexOfX);
          CycObject curCat = rs.getCycObject(indexOfY);
          System.out.println("Got dog/cat pair: " + curDog.cyclify() + " " + curCat.cyclify());
        }
        System.out.println("Result Set: " + rs);
      } finally {
        rs.close();
      }
    } catch (Exception e) {
      e.printStackTrace();
    } finally {
      if (access != null) {
        access.close();
      }
    }
    System.out.println("Finished");
    System.out.flush();
    System.exit(0);
  }

  public <T> T getObject(int columnIndex, Class<T> type) throws SQLException {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public <T> T getObject(String columnLabel, Class<T> type) throws SQLException {
    throw new UnsupportedOperationException("Not supported yet.");
  }
}
