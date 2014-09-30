/* $Id: DefaultSubLWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */

package org.opencyc.api;

//// Internal Imports
import org.opencyc.util.*;

//// External Imports
import org.opencyc.cycobject.*;
import java.io.*;
import java.util.logging.*;
import javax.swing.event.EventListenerList;
import java.util.EventListener;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * <P>DefaultSubLWorker is designed to provide a default implentation of 
 * a class to handle a particular communication event with a Cyc server.
 * SubLWorkers by default handle communications asynchronously with callbacks through the
 * SubLWorkerListener interface. DefaultSubLWorker provides the default
 * implementation while SubLWorkerSynch and DefaultSubLWorkerSynch provide
 * synchronous communications capabilities -- and are somewhat easier to use
 * when synchronous communications are desired. Currently, SubLWorkers are one-shot,
 * i.e., a new SubLWorker needs to be created for every new communication.
 * SubLWorkers are cancelable, time-outable and provide means for incremental
 * return results. Note, all callbacks happen in a single communications thread --
 * if you need to do any significant work based on the results of a callback
 * you *must* do it in a separate thread or risk delaying or breaking other
 * communications with Cyc. The SubL command
 * (post-task-info-processor-partial-results <data>)
 * is used for sending back the results incrementally which will
 * cause notifySubLWorkerDataAvailable to be called but only if the worker was
 * created with expectIncrementalResults=true.
 *  
 * <P>Example usage: <code>
 * try {
 *   CycAccess access = new CycAccess("localhost", 3600);
 *   SubLWorker worker = new DefaultSubLWorker("(+ 1 1)", access);
 *   worker.addListener(new SubLWorkerListener() {
 *     public void notifySubLWorkerStarted(SubLWorkerEvent event) {
 *       System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
 *     }
 *     public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
 *       System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
 *     }
 *     public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
 *       System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
 *       System.exit(0);
 *     }
 *   });
 *   worker.start();
 * } catch (Exception e) {
 *   e.printStackTrace();
 * }
 * </code>
 * <p> To perform time-duration profiling of SubL commands:
 * <code>
 * DefaultSubLWorker.startProfiling();
 * .... perform commands to be timed.
 * DefaultSubLWorker.endProfiling(<report file path>);
 * </code>
 * <p>Copyright 2004-2009 Cycorp, Inc., license is open source GNU LGPL.
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
 * @date March 25, 2004, 2:01 PM
 * @version $Id: DefaultSubLWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultSubLWorker implements SubLWorker {
  
  //// Constructors
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   */
  public DefaultSubLWorker(String subLCommand, CycAccess access) {
    this(access.makeCycList(subLCommand), access);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   */  
  public DefaultSubLWorker(String subLCommand, CycAccess access, 
      long timeoutMsecs) {
    this(access.makeCycList(subLCommand), access, timeoutMsecs);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   */  
  public DefaultSubLWorker(String subLCommand, CycAccess access, 
      boolean expectIncrementalResults) {
    this(access.makeCycList(subLCommand), access, expectIncrementalResults);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   * @param timeoutMsec the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   */  
  public DefaultSubLWorker(String subLCommand, CycAccess access, 
      boolean expectIncrementalResults, long timeoutMsec) {
    this(access.makeCycList(subLCommand), access, 
      expectIncrementalResults, timeoutMsec, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access) {
    this(subLCommand, access, false);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      long timeoutMsecs) {
    this( subLCommand, access, false, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      boolean expectIncrementalResults) {
    this( subLCommand, access, expectIncrementalResults, 0, CycConnection.NORMAL_PRIORITY);
  }

  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      Integer priority) {
    this( subLCommand, access, false, 0, priority);
  }

  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      long timeoutMsecs, Integer priority) {
    this( subLCommand, access, false, timeoutMsecs, priority);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      boolean expectIncrementalResults, Integer priority) {
    this( subLCommand, access, expectIncrementalResults, 0, priority);
  }
 
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      boolean expectIncrementalResults, long timeoutMsecs) {
    this(subLCommand, access, expectIncrementalResults, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DerfaultSubLWorker.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */  
  public DefaultSubLWorker(CycList subLCommand, CycAccess access, 
      boolean expectIncrementalResults, long timeoutMsecs, Integer priority) {
    this.subLCommand = subLCommand;
    this.access = access;
    this.timeoutMsecs = timeoutMsecs;
    this.expectIncrementalResults = expectIncrementalResults;
    this.priority = priority;
    
    if (subLCommandProfiler != null)
      this.addListener(subLCommandProfiler);
  }
  
  //// Public Area
  
  /** Begins profiling SubL commands. */
  public static synchronized void startProfiling() {
    subLCommandProfiling = true;
    if (subLCommandProfiler != null) {
      Logger.getLogger("org.opencyc.api.DefaultSubLWorker").log(Level.INFO, "SubL command profiling already started.");
      return;
    }
    
    Logger.getLogger("org.opencyc.api.DefaultSubLWorker").log(Level.INFO, "Start of SubL command profiling.");
    subLCommandProfiler = new SubLCommandProfiler();
  }
  
  /** Ends the profiling SubL commands and creates the profile report.
   *
   * @param reportPath the profiling report path
   */
  public static synchronized void endProfiling(final String reportPath) throws IOException {
    if (reportPath == null)
      throw new NullPointerException("reportPath must not be null");
    if (reportPath.length() == 0)
      throw new IllegalArgumentException("reportPath must not be an empty string");
    
    subLCommandProfiling = false;
    if (subLCommandProfiler == null) {
      Logger.getLogger("org.opencyc.api.DefaultSubLWorker").log(Level.INFO, "SubL command profiling is not active.");
      return;
    }
    
    Logger.getLogger("org.opencyc.api.DefaultSubLWorker").log(Level.INFO, "End of SubL command profiling, writing report to " + reportPath);
    ((SubLCommandProfiler) subLCommandProfiler).report(reportPath);
    subLCommandProfiler = null;
  }
  
  /**
   * Returns the SubL command that will be evaluated to execute the
   * work requested by this SubLWorker.
   * @return the SubL command that will be evaluated to execute the
   * work requested by this SubLWorker
   */
  public CycList getSubLCommand() {
    return subLCommand;
  }
  
  /**
   * Sets the SubL command that will be evaluated to execute the
   * work requested by this SubLWorker.
   */
  public void setSubLCommand(final CycList command) {
    if (getStatus().equals(SubLWorkerStatus.NOT_STARTED_STATUS)) {
    subLCommand = command;
    } else {
      throw new UnsupportedOperationException("Worker has already started.");
    }
  }
  
  /**
   * The Cyc server that should do the work.
   * @return the Cyc server that should do the work
   */
  public CycAccess getCycServer() {
    return access;
  }
  
  /**
   * Return's the unique id for this communication. It typically won't
   * be valid until the start event has been sent out.
   * @return the unique id for this communication. It typically won't
   * be valid until the start event has been sent out.
   */
  public Integer getId() {
    return id;
  }
  
  /**
   * This call will start the Cyc server processing the worker's SubL command.
   * @throws IOException if communications with Cyc server fail
   * @throws TimeOutException if the communication with Cyc takes too long
   * @throws CycApiException all other errors
   */
  public synchronized void start() 
  throws IOException, TimeOutException, CycApiException {
    if (getStatus() != SubLWorkerStatus.NOT_STARTED_STATUS) { 
      throw new RuntimeException("This communication has already been started.");
    }
    setStatus(SubLWorkerStatus.WORKING_STATUS);
    
    CycConnectionInterface conInt = getCycServer().getCycConnection();
    conInt.converseBinary(this);    
  }
  
  /**
   * Attempts to terminate the work being processed by the Cyc server.
   * This method should be preferred to abort() in that it tries to use
   * the natural termination event messaging infracture.
   * @throws IOException if communications with the Cyc server fails
   */
  public void cancel() throws java.io.IOException {
    if (!(getStatus() == SubLWorkerStatus.WORKING_STATUS)) { return; }
    CycAccess cycAccess = getCycServer();
     synchronized (cycAccess) {
    cycAccess.getCycConnection().cancelCommunication(this);
     }
  }
  
  /**
   * Attempts to terminate the work being processed by the Cyc server.
   * This call bypasses the communications infrasture and results in
   * no new messages being sent back to the Java client.
   * @throws IOException throws IO exception if communication with Cyc server fails
   */
  public void abort() throws java.io.IOException {
    if (!(getStatus() == SubLWorkerStatus.WORKING_STATUS)) { return; }
    CycAccess cycAccess = getCycServer();
     synchronized (cycAccess) {
    cycAccess.getCycConnection().abortCommunication(this);
    }
  }
  
  public void cancelTask() {
    try {
     abort();
    } catch (java.io.IOException e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Return the task's priority. This is a value that meets the
   * constraints of SL:SET-PROCESS-PRIORITY.
   * @see CycConnection.MAX_PRIORITY
   * @see CycConnection.CRITICAL_PRIORITY
   * @see CycConnection.NORMAL_PRIORITY
   * @see CycConnection.BACKGROUND_PRIORITY
   * @see CycConnection.MIN_PRIORITY
   * @return the priority of the process
   */
  public Integer getPriority() {
    return priority;
  }
  
  /**
   * Returns the current status of this SubLWorker.
   * @return the current status of this SubLWorker
   */
  public SubLWorkerStatus getStatus() { 
    return status;
  }
  
  /**
   * Returns A boolean indicating whether communications with the Cyc server
   * on behalf of this SubLWorker have terminated
   * @return a boolean indicating whether communications with the Cyc server
   * on behalf of this SubLWorker have terminated
   */
  public boolean isDone() {
    SubLWorkerStatus status = getStatus();
    return !((status == SubLWorkerStatus.NOT_STARTED_STATUS) 
      || (status == SubLWorkerStatus.WORKING_STATUS));
  }
  
  /** 
   * Returns the number of msecs that this communication will wait, once
   * started, before throwing a TimeOutException. 0 msecs means to wait forever.
   * @return the number of msecs that this communication will wait before 
   * throwing a TimeOutException
   */
  public long getTimeoutMsecs() { 
    return timeoutMsecs;
  }
  
  /** Returns wether this communication should expect incremental results.
   * @return wether this communication should expect incremental results
   */
  public boolean expectIncrementalResults() {
    return expectIncrementalResults;
  }
  
  /**
   * Returns all the SubLWorkerListeners listening in on this
   * SubLWorker's events
   * @return all the SubLWorkerListeners listening in on this
   * SubLWorker's events
   */
  public Object[] getListeners() {
    return listeners.getListeners(listenerClass);
  }
  
  /**
   * Adds the given listener to this SubLWorker.
   * @param listener the listener that wishes to listen
   * for events sent out by this SubLWorker
   */
  public void addListener(SubLWorkerListener listener) {
    listeners.add(listenerClass, listener);
  }
  
  /** 
   * Removes the given listener from this SubLWorker.
   * @param listener the listener that no longer wishes
   * to receive events from this SubLWorker
   */
  public void removeListener(SubLWorkerListener listener) {
     listeners.remove(listenerClass, listener);
  }
  
  /** Removes all listeners from this SubLWorker. */
  public void removeAllListeners() { 
    Object[] listenerArray = listeners.getListenerList();
    for (int i = 0, size = listenerArray.length; i < size; i += 2) {
      listeners.remove((Class)listenerArray[i], 
        (EventListener)listenerArray[i+1]);
    }
  }
   
  /**
   * Returns a string representation of the SubLWorker.
   * @return a string representation of the SubLWorker
   */
  public String toString() {
    return toString(2);
  }
  
  /** Returns a string representation of the SubLWorker.
   * @return a string representation of the SubLWorker
   * @param indentLength the number of spaces to preceed each line of 
   * output String
   */
  public String toString(int indentLength) {
    StringBuffer nlBuff = new StringBuffer();
    nlBuff.append(System.getProperty("line.separator"));
    for (int i = 1; i < indentLength; i++) { nlBuff.append(" "); }
    String nl = nlBuff.toString();
    String sp = nl.substring(1);
    StringBuffer buf = new StringBuffer(sp + this.getClass().getName());
    buf.append(":").
      append(nl).append("Id: "). append(getId()).
      append(nl).append("Server: ").append(this.getCycServer().toString()).
      append(nl).append("Status: ").append(getStatus().getName()).
      append(nl).append("Incremental results: ").append(expectIncrementalResults()).
      append(nl).append("Timeout: ").append(getTimeoutMsecs()).append(" msecs").
      append(nl).append("Command: \n").append(getSubLCommand().toPrettyCyclifiedString("")).
      append(nl);
    return buf.toString();
  }
  
  /** 
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The start event that should be transmitted to
   * listeners of this SubLWorker
   */
  public void fireSubLWorkerStartedEvent(SubLWorkerEvent event) {
    if (event.getEventType() != SubLWorkerEvent.STARTING_EVENT_TYPE) {
      throw new RuntimeException("Got bad event type; " + 
        event.getEventType().getName());
    }
    setId(event.getId());
    synchronized(listeners) {
   Object[] curListeners = listeners.getListenerList();
    for (int i = curListeners.length-2; i >= 0; i -= 2) {
      if (curListeners[i] == listenerClass) {
        try {
          ((SubLWorkerListener)curListeners[i+1]).notifySubLWorkerStarted(event);
        } catch (Exception e) {
          Logger.getLogger(this.getClass().toString()).log(Level.WARNING, e.getMessage(), e);
        }
      }
    }
    }
  }
    
  /**
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The data available event that should be transmitted to
   * listeners of this SubLWorker
   */
  public void fireSubLWorkerDataAvailableEvent(SubLWorkerEvent event) {
    if (event.getEventType() != SubLWorkerEvent.DATA_AVAILABLE_EVENT_TYPE) {
      throw new RuntimeException("Got bad event type; " + 
        event.getEventType().getName());
    } 
    synchronized(listeners) {
   Object[] curListeners = listeners.getListenerList();
    for (int i = curListeners.length-2; i >= 0; i -= 2) {
      if (curListeners[i] == listenerClass) {
        try {
          //System.out.println("GOT DATA FOR SUBL CALL: " + event);
          ((SubLWorkerListener)curListeners[i+1]).notifySubLWorkerDataAvailable(event);
        } catch (Exception e) {
          Logger.getLogger(this.getClass().toString()).log(Level.WARNING, e.getMessage(), e);
        }
      }
    }
  }
  }
    
  /**
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The termination event that should be transmitted to
   * listeners of this SubLWorker
   */
  public void fireSubLWorkerTerminatedEvent(SubLWorkerEvent event) {
    if (event.getEventType() != SubLWorkerEvent.TERMINATION_EVENT_TYPE) {
      throw new RuntimeException("Got bad event type; " + 
        event.getEventType().getName());
    }
    setStatus(event.getStatus());
    synchronized(listeners) {
   Object[] curListeners = listeners.getListenerList();
    for (int i = curListeners.length-2; i >= 0; i -= 2) {
      if (curListeners[i] == listenerClass) {
        try {
          ((SubLWorkerListener)curListeners[i+1]).notifySubLWorkerTerminated(event);
        } catch (Exception e) {
          Logger.getLogger(this.getClass().toString()).log(Level.WARNING, e.getMessage(), e);
        }
      }
    }
  }
  }
  
  /**
   * Indicates whether this communication should be attempted even if
   * the current lease to the Cyc image has expired.
   */  
  public boolean shouldIgnoreInvalidLeases() {
    return shouldIgnoreInvalidLeases;
  }
  
  //// Protected Area
  
  protected void setShouldIgnoreInvalidLeases(boolean newVal) {
    this.shouldIgnoreInvalidLeases = newVal;
  }
  
  /** Sets the client-unique communication id for this message.
   * @param id the communication id
   */  
  protected void setId(Integer id) {
    this.id = id;
  }
  
  /** Sets the current status of this SubLWorker
   * @param status the new status value
   */  
  protected void setStatus(SubLWorkerStatus status) {
    
    this.status = status;
  }
  
  public BlockingQueue<CycConnection.TaskProcessorBinaryResponseHandler.NotificationTask> getNotificationQueue() {
    return notificationQueue;
  }
  
  //// Private Area
  
  //// Internal Rep
  
  private boolean shouldIgnoreInvalidLeases = false;
  
  private boolean expectIncrementalResults = false;
  
  private CycList subLCommand;
  
  private CycAccess access;
  
  private Integer id;
  
  private long timeoutMsecs = 0;
  
  private volatile SubLWorkerStatus status = 
    SubLWorkerStatus.NOT_STARTED_STATUS;
  
  private BlockingQueue<CycConnection.TaskProcessorBinaryResponseHandler.NotificationTask> notificationQueue 
      = new LinkedBlockingQueue<CycConnection.TaskProcessorBinaryResponseHandler.NotificationTask>();
  
  private Integer priority;
  
  /** This holds the list of registered SubLWorkerListener listeners. */
  private EventListenerList listeners = new EventListenerList();
  
  private static Class listenerClass = SubLWorkerListener.class;
  
  /** the indicator for whether API requests should be profiled */
  private static boolean subLCommandProfiling = false;
  
  /** the SubL command profiler that listens to each event when profiling is in effect */
  private static SubLWorkerListener subLCommandProfiler = null;
  
  
  //// Main
  
  /** Example usage for this class. The SubL command (post-task-info-processor-partial-results <data>)
   * is used for sending back the results incrementally which will cause notifySubLWorkerDataAvailable
   * to be called but only if the worker was created with expectIncrementalResults=true.
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    try {
      CycAccess access = new CycAccess("localhost", 3600);
      SubLWorker worker = new DefaultSubLWorker("(+ 1 1)", access);
      worker.addListener(new SubLWorkerListener() {
        public void notifySubLWorkerStarted(SubLWorkerEvent event) {
          System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
        }
        public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
          System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
        }
        public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
          System.out.println("Received SubL Worker Event: \n" + event.toString(2) + "\n");
          System.exit(0);
        }
      });
      worker.start();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
  
}
