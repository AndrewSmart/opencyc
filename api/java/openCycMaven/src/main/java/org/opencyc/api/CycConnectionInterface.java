package org.opencyc.api;

import java.io.IOException;
import org.opencyc.cycobject.CycList;
import org.opencyc.util.TimeOutException;
import org.opencyc.util.Timer;
import org.opencyc.util.UUID;

/**
 * Defines the interface for local and remote CycConnection objects<p>
 *
 * @version $Id: CycConnectionInterface.java 138557 2012-02-10 14:21:38Z daves $
 * @author Stephen L. Reed
 *
 * <p>Copyright 2001 Cycorp, Inc., license is open source GNU LGPL.
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

public interface CycConnectionInterface {
  
  /** Sends a message to Cyc and return the <tt>Boolean</tt> true as the first
   * element of an object array, and the cyc response Symbolic Expression as
   * the second element.  If an error occurs the first element is <tt>Boolean</tt>
   * false and the second element is the error message string.
   *
   * @param message the api command
   * @return an array of two objects, the first is an Integer response code, and the second is the
   * response object or error string.
   */
  public Object[] converse(Object message) throws IOException, CycApiException;
  
  /** Sends a message to Cyc and return the response code as the first
   * element of an object array, and the cyc response Symbolic Expression as
   * the second element, spending no less time than the specified timer allows
   * but throwing a <code>TimeOutException</code> at the first opportunity
   * where that time limit is exceeded.
   * If an error occurs the second element is the error message string.
   *
   * @param message the api command which must be a String or a CycList
   * @param timeout a <tt>Timer</tt> object giving the time limit for the api call
   * @return an array of two objects, the first is an Integer response code, and the second is the
   * response object or error string.
   */
  public Object[] converse(Object message, Timer timeout) 
  throws IOException, TimeOutException, CycApiException;
  
  /** Sends a message to Cyc and return the response code as the first
   * element of an object array, and the cyc response Symbolic Expression as
   * the second element, spending no less time than the specified timer allows
   * but throwing a <code>TimeOutException</code> at the first opportunity
   * where that time limit is exceeded.
   * If an error occurs the second element is the error message string.
   * The concurrent mode of Cyc server communication is supported by
   * Cyc's pool of transaction processor threads, each of which can
   * concurrently process an api request.
   *
   * The CFASL input and output streams are encoded in Base64 format.
   *
   * @param message the api command
   * @param timeout a <tt>Timer</tt> object giving the time limit for the api call
   * @return an array of two objects, the first is an Integer response code, and the second is the
   * response object or error string.
   */
  public Object[] converseBinary(final CycList message, final Timer timeout) 
  throws IOException, TimeOutException, CycApiException;
  
  /**
   * Send a message to Cyc spending no less time than the specified timer allows but throwing a <code>TimeOutException</code> 
   * at the first opportunity where that time limit is exceeded. The concurrent mode of Cyc server communication 
   * is supported by Cyc's pool of transaction processor threads, each of which can concurrently process an api request.  The
   * SubLWorker object notifies the caller when the api response is aschronously received.
   *
   * @param worker a <tt>SubLWorker</tt> object that notifies the caller when work is done
   * 
   * @throws IOException when a communication error occurs
   * @throws TimeOutException when the time limit is exceeded
   * @throws CycApiException when a Cyc api error occurs
   */
  public void converseBinary(SubLWorker worker) 
  throws IOException, TimeOutException, CycApiException;

    /** Returns connection information, suitable for diagnostics.
   *
   * @return connection information, suitable for diagnostics
   */
  public String connectionInfo();
  
  /** Closes the api sockets and streams. */
  public void close();
  
  /** Returns the trace value.
   *
   * @return the trace value
   */
  public int getTrace();
  
  /** Returns the connection type of the parent CycAccess object.
   *
   * @return the connection type of the parent CycAccess object
   */
  public int getConnectionType();
  
  /** Sets the trace value.
   *
   * @param trace the trace value
   */
  public void setTrace(int trace);
  
  /** Turns off the diagnostic trace of socket messages. */
  public void traceOff();
  
  /** Turns on the diagnostic trace of socket messages. */
  public void traceOn();
  
  /** Turns on the detailed diagnostic trace of socket messages. */
  public void traceOnDetailed();
  
  /** Returns the UUID that identifies this java api client connection.
   *
   * @return the UUID that identifies this java api client connection
   */
  public UUID getUuid();
  
  /** Returns the hostname of this connection.
   *
   * @return <code>String</code> denoting this hostname.
   */
  public String getHostName();
  
  /** Returns the base port of this connection.
   *
   * @return <code>int</code> of this connection's base port.
   */
  public int getBasePort();
  
  /**
   * @return the http port of the connected server.
   */
  public int getHttpPort();
  
  /**
   * Sets the client name of this api connection.
   *
   * @param myClientName the client name of this api connection
   */
  //public void setMyClientName(String myClientName);
  
  /**
   * Gets the client name of this api connection.
   *
   * @return the client name of this api connection
   */
  //public String getMyClientname();
  
  /** Cancels the communication associated with the given SubLWorker.
   *
   * @param worker the given SubLWorker
   */
  public void cancelCommunication(SubLWorker worker) throws IOException;
  
  /** Cancels the communication associated with the given SubLWorker.
   *
   * @param worker the given SubLWorker
   */
  public void abortCommunication(SubLWorker worker) throws IOException;
  
  /**
   * Send a message to Cyc and return the response code as the first element of an object array,
   * and the cyc response Symbolic Expression as the second element, spending no less time than
   * the specified timer allows but throwing a <code>TimeOutException</code> at the first
   * opportunity where that time limit is exceeded. If an error occurs the second element is the
   * error message string. The concurrent mode of Cyc server communication is supported by Cyc's
   * pool of transaction processor threads, each of which can concurrently process an api request.
   *
   * @param message the api command
   * @param timeout a <tt>Timer</tt> object giving the time limit for the api call
   * @param worker a <tt>SubLWorker</tt> object that notifies the caller when work is done
   *
   * @return an array of two objects, the first is an Boolean response code, and the second is the
   *         response object or error string.
   *
   * @throws IOException when a communication error occurs
   * @throws TimeOutException when the time limit is exceeded
   * @throws CycApiException when a Cyc api error occurs
   */
  //public void converseBinary(CycList message, Timer timeout, SubLWorker worker) throws IOException, TimeOutException, CycApiException;
}
