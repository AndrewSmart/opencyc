
package org.opencyc.util;

//// Internal Imports
import org.opencyc.api.*;
import org.opencyc.cycobject.*;
import org.opencyc.inference.*;
import org.opencyc.util.TimeOutException;

//// External Imports
import java.io.*;
import java.util.*;
import java.util.logging.Logger;
import org.opencyc.util.OpenCycTaskInterruptedException;

/**
 * <P>CommUtils is designed to...
 *
 *  <p>Copyright 2007 Cycorp, Inc., license is open source GNU LGPL.
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
 * @date Tue Aug  7 15:50:28 CDT 2007
 * @version $Id: CommUtils.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public final class CommUtils {
  
  //// Constructors
  
  /** Creates a new instance of CommUtils. */
  private CommUtils() {}
  
  //// Public Area
  
  static public Object performApiCommand(String command, CycAccess cyc)  
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    return performApiCommand(command, cyc, "About to perform API command: ", 0);
  }
  
  static public Object performApiCommand(CycList command, CycAccess cyc)  
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    return performApiCommand(command, cyc, "About to perform API command: ", 
      0, CycConnection.NORMAL_PRIORITY);
  }
  
  static public Object performApiCommand(CycList command, CycAccess cyc, Integer priority)  
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    return performApiCommand(command, cyc, "About to perform API command: ", 0, priority);
  }
  
  static public Object performApiCommand(String command,
      CycAccess cyc, String taskDescription, long timeoutMsecs)  
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException  {
    SubLWorkerSynch worker = (SubLWorkerSynch)CommUtils.
      makeSubLWorker(command, cyc, false, timeoutMsecs, true);
    return performApiCommand(worker, taskDescription);
  }
  
  static public Object performApiCommand(CycList command, CycAccess cyc,
      String taskDescription, long timeoutMsecs, Integer priority)  
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException  {
    SubLWorkerSynch worker = (SubLWorkerSynch)CommUtils.
      makeSubLWorker(command, cyc, false, timeoutMsecs, true, priority);
    return performApiCommand(worker, taskDescription);
  }

  static public Object performApiCommand(SubLWorkerSynch command) 
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    return performApiCommand(command, "About to perform API command: ");
  }

  static public Object performApiCommand(SubLWorkerSynch command, String taskDescription) 
  throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    Object apiResult = null;
    Logger.getLogger(LOGGER_ID).info(taskDescription + "\n" + command);
    long startTime = System.currentTimeMillis();
    apiResult = command.getWork();
    long endTime = System.currentTimeMillis();
    Logger.getLogger(LOGGER_ID).info("Command took: " + (endTime - startTime) + "ms.");
    Logger.getLogger(LOGGER_ID).finest("" + apiResult);
    return apiResult;
  }
  
  static public List performInference(InferenceWorkerSynch command) 
  throws IOException, TimeOutException, CycApiException {
    return performInference(command, "About to start inference: ");
  }
  
  static public List performInference(InferenceWorkerSynch command, String taskDescription) 
  throws IOException, TimeOutException, CycApiException {
    List answers = null;
    Logger.getLogger(LOGGER_ID).info(taskDescription + "\n" + command);
    answers = command.performSynchronousInference();
    Logger.getLogger(LOGGER_ID).finest("Got answers: " + answers);
    return answers;
  }
  
  static public void startAsynchApiCommand(SubLWorker command) 
  throws IOException, TimeOutException, CycApiException {
    startAsynchApiCommand(command, "About to start API command: ");
  }
  
  static public void startAsynchApiCommand(SubLWorker command, String taskDescription) 
  throws IOException, TimeOutException, CycApiException {
    Logger.getLogger(LOGGER_ID).info(taskDescription + "\n" + command);
    command.start();
  }
  
  public static SubLWorker makeSubLWorker(String command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs) {
    return makeSubLWorker(command, cyc, asynch, timeoutMsecs, true, CycConnection.NORMAL_PRIORITY);
  }
  
  public static SubLWorker makeSubLWorker(String command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs, Integer priority) {
    return makeSubLWorker(command, cyc, asynch, timeoutMsecs, true, priority);
  }
  
  public static SubLWorker makeSubLWorker(String command, CycAccess cyc, boolean asynch, long timeoutMsecs,
      boolean wrapBookkeeping, Integer priority) {
    command = processCommand(command, cyc, wrapBookkeeping);
    return asynch? new DefaultSubLWorker(cyc.makeCycList(command), cyc, false, timeoutMsecs,priority) :
      new DefaultSubLWorkerSynch(cyc.makeCycList(command), cyc, timeoutMsecs,priority);
  }
  
  public static SubLWorker makeSubLWorker(CycList command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs) {
    return makeSubLWorker(command, cyc, asynch, timeoutMsecs, true);
  }
  
  public static SubLWorker makeSubLWorker(CycList command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs, Integer priority) {
    return makeSubLWorker(command, cyc, asynch, timeoutMsecs, true, priority);
  }
  
  public static SubLWorker makeSubLWorker(String command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs,
      boolean wrapBookkeeping) {
    command = processCommand(command, cyc, wrapBookkeeping);
    return asynch ? new DefaultSubLWorker(command, cyc, false, timeoutMsecs) : 
      new DefaultSubLWorkerSynch(command, cyc, timeoutMsecs);
  }
  
  public static SubLWorker makeSubLWorker(CycList command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs,
      boolean wrapBookkeeping) {
    command = processCommand(command, cyc, wrapBookkeeping);
    return asynch ? new DefaultSubLWorker(command, cyc, timeoutMsecs) : 
      new DefaultSubLWorkerSynch(command, cyc, timeoutMsecs);
  }
 
  public static SubLWorker makeSubLWorker(CycList command, 
      CycAccess cyc, boolean asynch, long timeoutMsecs,
      boolean wrapBookkeeping, Integer priority) {
    command = processCommand(command, cyc, wrapBookkeeping);
    return asynch ? new DefaultSubLWorker(command, cyc, timeoutMsecs, priority) : 
      new DefaultSubLWorkerSynch(command, cyc, timeoutMsecs, priority);
  }
    
  static public String getSubLForBoolean(boolean val) {
    return (val == false) ? "nil" : "T";
  }
  
  static public CycSymbol getCycSymbolForBoolean(boolean val) {
    return (val == false) ? CycObjectFactory.nil : CycObjectFactory.t;
  }
  
  static public boolean getBooleanFromSubLString(String val) {
    return (val.toUpperCase(Locale.ENGLISH).equals("NIL")) ? false : true;
  }
    
  // For compatibility with Cyccyc.converseBoolean
  static public boolean convertResponseToBoolean(Object response) {
    if (response.toString().toUpperCase(Locale.ENGLISH).equals("T")) {
      return true;
    } else {
      return false;
    }
  }
  
  // For compatibility with CycAccess.converseCycObject
  static public CycObject convertResponseToCycObject(Object response) {
    if (response.equals(CycObjectFactory.nil)) {
      return new CycList();
    } else {
      return (CycObject) response;
    }
  }
  
  // For compatibility with CycAccess.converseInt
  static public int convertResponseToInt(Object response) {
    return (new Integer(response.toString())).intValue();
  }
  
  // For compatibility with CycAccess.converseList
  static public CycList convertResponseToCycList(Object response, Object command) {  
    if (response.equals(CycObjectFactory.nil)) {
      return new CycList();
    } else {
      if (response instanceof CycList) {
        return (CycList) response;
      }
    }
    String request;
    if (command instanceof CycList) {
      request = ((CycList) command).cyclify();
    } else {
      request = (String) command;
    }
    throw new CycApiException(response.toString() + "\nrequest: " + request);
  }
  
  // For compatibility with CycAccess.converseString
  static public String convertResponseToString(Object response, Object command) {
    if (!(response instanceof String)) {
      throw new RuntimeException("Expected String but received (" 
        + response.getClass() + ") " + response + "\n in response to command " + command);
    }
    return (String) response;
  }

  static public String composeApiCommand(String function, Object... arguments) {
    StringBuilder apiCmd = new StringBuilder(256);
    apiCmd.append('(');
    apiCmd.append(function);
    for (Object arg : arguments) {
      apiCmd.append(" ");
      apiCmd.append(DefaultCycObject.stringApiValue(arg));
    }
    apiCmd.append(')');
    return apiCmd.toString();
  }
  
  static public String composeMultipleApiCommands(String... commands) {
    StringBuilder apiCmd = new StringBuilder(256);
    apiCmd.append("(LIST ");
    for (Object command : commands) {
      apiCmd.append(command);
    }
    apiCmd.append(')');
    return apiCmd.toString();
  }
  
  private static String processCommand(String command, CycAccess cyc, 
      boolean wrapBookkeeping) {
    if (wrapBookkeeping) {
      command = cyc.wrapBookkeeping(command);
    }
    return command;
  }
  
  private static CycList processCommand(CycList command, CycAccess cyc, 
      boolean wrapBookkeeping) {
    CycList result = command;
    if (wrapBookkeeping) {
      command = cyc.makeCycList(cyc.wrapBookkeeping("" + command.cyclify()));
    }
    return command;
  }
  
  //// Internal Rep
  
  public static String LOGGER_ID = CommUtils.class.toString();
  
  //// Main
  
}
