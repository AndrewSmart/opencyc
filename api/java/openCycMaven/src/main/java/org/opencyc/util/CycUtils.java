/*
 * CycUtils.java
 *
 * Created on March 21, 2002, 4:54 PM
 */

package org.opencyc.util;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.api.CycConnection;
import org.opencyc.api.CycIOException;
import org.opencyc.api.DefaultSubLWorkerSynch;
import org.opencyc.api.SubLWorkerSynch;

/**
 * This is a placeholder class for general cyc utilities.
 * All methods in this class are static.
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
 * @author  tbrussea
 * @version $Id: CycUtils.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class CycUtils {
    
  
  private static boolean useTiming = false;
  /*
   * Creates a new instance of CycUtils and hides it since no instances 
   * of this class need ever be made. All methods here are static. 
   */
  private CycUtils() {}
    
  /** 
   * Evaluates the given SubL expression given on the cyc image
   * provided by the CycAccess object given. Really just a thin wrapper
   * around "CycAccess.converseObject()" because I found that
   * to be a very non-intuitive method name.  Currently all
   * exceptions are caught and stack traces printed to standard
   * err. I expect that the API on this method may change in the near future
   * to throw appropriate exceptions.
   * @param connection The CycAccess object to use for communications
   * with the appropriate Cyc image.
   * @param subl The string that represents the SubL expression that
   * needs to be evaluated.
   * @return The value of evaluating the passed in subl expression or
   * null if an error occurred.
   * @deprecated use SubLWorker instead
   **/
  public static synchronized Object evalSubL(CycAccess connection, String subl) {
    Object result = null;
    try {
      if (CycConnection.inAWTEventThread()) {
        throw new RuntimeException("Unable to communicate with Cyc from the AWT event dispatch thread.");
      }
      long resultMsecs = 0;
      if(useTiming) {
        resultMsecs = System.currentTimeMillis();
      }
      result = connection.converseObject(subl);
      if(useTiming) {
        System.out.println("Finished call: " + subl);
        resultMsecs = System.currentTimeMillis() - resultMsecs;
        System.out.println("Call took: " + resultMsecs + " msecs.");
      }
    } catch (IOException e) {
      throw new CycIOException(e);
    }
    return result;
  }

  /** 
   * Evaluates the given SubL expression given on the cyc image
   * provided by the CycAccess object given. 
   * @param connection The CycAccess object to use for communications
   * with the appropriate Cyc image.
   * @param subl The string that represents the SubL expression that
   * needs to be evaluated.
   * @return The value of evaluating the passed in subl expression or
   * null if an error occurred.
   **/
  public static synchronized Object evalSubLWithWorker(final CycAccess connection, final String subl) 
    throws IOException, TimeOutException, CycApiException {
    final SubLWorkerSynch worker = new DefaultSubLWorkerSynch(subl, connection);
    return worker.getWork();
  }

  /** 
   * Resolve the value of the Symbol whose name is in the string
   * symbol.
   * @param connection The CycAccess object to use for communications
   * with the appropriate Cyc image.
   * @param symbol The string that represents the Symbol that
   * whose value is requested
   * @return The value of the symbol or null if an error occurred.
   **/
  public static Object getSymbolValue(CycAccess connection, 
                                      String symbol) {
    Object result = null;
    result = evalSubL( connection, "(SYMBOL-VALUE (QUOTE " + symbol + "))");
    return result;
  }
  
  /**
   * Evalutes the given subl expression on the given Cyc image in the 
   * background. When the evaluation is complete the CycWorkerListener
   * passed to this method is notified via an event callback.
   * @param conn The CycAccess object to use for communications
   * with the appropriate Cyc image.
   * @param subl The string that represents the SubL expression that
   * needs to be evaluated.
   * @param cwl The CycWorkerListener that should be notified of
   * the background tasks progress.
   * @return The CycWorker object that is doing the work. It will be
   * either already be started.
   * @see CycWorker
   * @see CycWorkerListener
   * @deprecated use SubLWorker instead
   */
  public static CycWorker evalSubLInBackground(final CycAccess conn,
					       final String subl,
					       final CycWorkerListener cwl) {
    CycWorker worker = new CycWorker() {
      public Object construct() throws Exception {
        return evalSubL(conn, subl); 
      }
    };
    if(cwl != null) { worker.addListener(cwl); }
    worker.start();
    return worker;
  }
  
  private static long SUBL_TIME_OFFSET;
  
  static {
    Calendar cal = Calendar.getInstance();
    cal.set(1900, Calendar.JANUARY, 1);
    long time = cal.getTime().getTime();
    cal.set(1970, Calendar.JANUARY, 1);
    SUBL_TIME_OFFSET = (cal.getTime().getTime() - time);
  }
  
  public static Date convertSubLTimeStampToDate(long timeStamp) {
    //@hack the (60*60*1000) is a complete hack and should be remved once
    //we can determine why out timestamps are off by 1 hour
    return new Date((long)(timeStamp * 1000) - SUBL_TIME_OFFSET + (60 * 60 * 1000));
  }
  
}
