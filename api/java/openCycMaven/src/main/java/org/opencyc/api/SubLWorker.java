/* $Id: SubLWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */
package org.opencyc.api;

//// Internal Imports
import org.opencyc.cycobject.*;
import org.opencyc.util.*;

//// External Imports
import java.io.*;
import java.util.concurrent.BlockingQueue;

/**
 * <P>SubLWorker is designed to provide a handle for a particular 
 * communication event with a Cyc server. SubLWorkers by default handle
 * communications asynchronously with callbacks through the 
 * SubLWorkerListener interface. DefaultSubLWorker provides the default
 * implementation while SubLWorkerSynch and DefaultSubLWorkerSynch provide
 * synchronous communications capabilities. Currently, SubLWorkers are one-shot,
 * i.e., a new SubLWorker needs to be created for every new communication.
 * SubLWorkers are cancelable, time-outable and provide means for incremental
 * return results. Note, all callbacks happen in a single communications thread --
 * if you need to do any significant work based on the results of a callback
 * you *must* do it in a separate thread or risk delaying or breaking other
 * communications with Cyc.
 *  
 * <P>Example usage: <pre>
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
 * </pre>
 * <p>Copyright 2004 Cycorp, Inc., license is open source GNU LGPL.
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
 * @see DefaultSubLWorker, SubLWorkerSynch, DefaultSubLWorkerSynch, SubLWorkerListener
 * @author tbrussea
 * @date March 17, 2004, 11:26 AM
 * @version $Id: SubLWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface SubLWorker extends Cancelable {
  
  /**
   * Returns the SubL command that will be evaluated to execute the
   * work requested by this SubLWorker.
   * @return the SubL command that will be evaluated to execute the
   * work requested by this SubLWorker
   */  
  CycList getSubLCommand();

  /**
   * The Cyc server that should do the work.
   * @return the Cyc server that should do the work
   */  
  CycAccess getCycServer();
  
  /**
   * Return's the unique id for this communication. It typically won't
   * be valid until the start event has been sent out.
   * @return the unique id for this communication. It typically won't
   * be valid until the start event has been sent out.
   */  
  Integer getId();
  
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
  Integer getPriority();
  
  /**
   * This call will start the Cyc server processing the worker's SubL command.
   * @throws IOException if communications with Cyc server fail
   * @throws TimeOutException if the communication with Cyc takes too long
   * @throws CycApiException all other errors
   */  
  void start() throws IOException, TimeOutException, CycApiException;
  
  /**
   * Attempts to terminate the work being processed by the Cyc server.
   * This method should be preferred to abort() in that it tries to use
   * the natural termination event messaging infracture.
   * @throws IOException if communications with the Cyc server fails
   */  
  void cancel() throws java.io.IOException;
  
  /**
   * Attempts to terminate the work being processed by the Cyc server.
   * This call bypasses the communications infrasture and results in
   * no new messages being sent back to the Java client.
   * @throws IOException
   */  
  void abort() throws java.io.IOException;
  
  /**
   * Returns the current status of this SubLWorker.
   * @return the current status of this SubLWorker
   */  
  SubLWorkerStatus getStatus();
  
  /**
   * Returns A boolean indicating whether communications with the Cyc server
   * on behalf of this SubLWorker have terminated
   * @return a boolean indicating whether communications with the Cyc server
   * on behalf of this SubLWorker have terminated
   */
  boolean isDone();
  
  /** 
   * Returns the number of msecs that this communication will wait, once
   * started, before throwing a TimeOutException. 0 msecs means to wait forever.
   * @return the number of msecs that this communication will wait before 
   * throwing a TimeOutException
   */  
  long getTimeoutMsecs();
  
  /** Returns wether this communication should expect incremental results.
   * @return wether this communication should expect incremental results
   */  
  boolean expectIncrementalResults();
  
  /**
   * Returns all the SubLWorkerListeners listening in on this
   * SubLWorker's events
   * @return all the SubLWorkerListeners listening in on this
   * SubLWorker's events
   */  
  Object[] getListeners();

  /**
   * Adds the given listener to this SubLWorker.
   * @param listener the listener that wishes to listen
   * for events sent out by this SubLWorker
   */  
  void addListener(SubLWorkerListener listener);
  
  /** 
   * Removes the given listener from this SubLWorker.
   * @param listener the listener that no longer wishes
   * to receive events from this SubLWorker
   */  
  void removeListener(SubLWorkerListener listener);
  
  /** Removes all listeners from this SubLWorker. */  
  void removeAllListeners();
  
  /**
   * Returns a string representation of the SubLWorker.
   * @return a string representation of the SubLWorker
   */  
  String toString();
  
  /**
   * Returns a string representation of the SubLWorker.
   * @param indentLength the number of spaces to preceed each line of 
   * output String
   * @return a string representation of the SubLWorker
   */  
  String toString(int indentLength);
  
  /**
   * Indicates whether this communication should be attempted even if
   * the current lease to the Cyc image has expired.
   */  
  boolean shouldIgnoreInvalidLeases();
  
  /** 
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The start event that should be transmitted to
   * listeners of this SubLWorker
   */  
  void fireSubLWorkerStartedEvent(SubLWorkerEvent event);
  
  /**
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The data available event that should be transmitted to
   * listeners of this SubLWorker
   */  
  void fireSubLWorkerDataAvailableEvent(SubLWorkerEvent event);
  
  /**
   * Public for implementation reasons only, this method should
   * only ever be called by subclasses of CycConnection.java.
   * @param event The termination event that should be transmitted to
   * listeners of this SubLWorker
   */  
  void fireSubLWorkerTerminatedEvent(SubLWorkerEvent event);
  
  public BlockingQueue<CycConnection.TaskProcessorBinaryResponseHandler.NotificationTask> getNotificationQueue();

}
