/*
 * CycWorker.java
 *
 * Created on March 21, 2002, 7:00 PM
 */
package org.opencyc.util;

import java.util.*;
import javax.swing.event.EventListenerList;

/**
 *
 * This is a convenient event callback wrapper around the
 * the SwingWorker class. It's inteded to allow easily
 * running tasks in the background. Users of this class
 * will be notified when the worker is started, interrupted
 * or finished with its work. If finished, the rusults of the
 * work will be made available as well.
 * <P>
 * Here is some example code:<P>
 * <CODE><PLAINTEXT>
 *   public static CycWorker evalSubLInBackground(CycAccess conn,
 *                                                String subl,
 *                                                CycWorkerListener cwl) {
 *       CycWorker worker = new CycWorker() {
 *           //The return val of construct() method is considered to be
 *           //the output for this worker and retrieved with
 *           //worker.getWork().
 *           public Object construct() { 
 *               return evalSubL(conn, subl); //Do work here.
 *           }
 *       };
 *       if(cwl != null) { worker.addListener(cwl); }
 *       worker.start();
 *       return worker;
 *   }
 *
 *   public class GKETermDialog implements CycWorkerListener {
 *
 *      private CycWorker specWorker = null;
 *
 *	public void notifyWorkerStarted(CycWorkerEvent evt) {}
 *    
 *	public void notifyWorkerInterrupted(CycWorkerEvent evt) {}
 *    
 *	public void notifyWorkerFinished(CycWorkerEvent evt) {
 *	    CycWorker worker = (CycWorker)evt.getSource();
 *	    System.out.println("WORKER FINISHED: " + worker);
 *	    System.out.println("WORKER OUTPUT: " + worker.getWork());
 *	}
 *   }
 * </PLAINTEXT></CODE>
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
 * @see CycWorkerListener
 * @see CycWorkerEvent
 * @see SwingWorker
 * @author tbrussea
 * @version $Id: CycWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public abstract class CycWorker extends SwingWorker {

  /** This CycWorkers listeners (instances of CycWorkerListener only).**/
  private EventListenerList listeners = new EventListenerList();
    
  /** Creates a new instance of CycWorker */
  public CycWorker() { }
    
  /**
   * Adds a new worker listener to this worker. Listeners will
   * be notified when the worker is started, interrupted or 
   * finished.
   * @param cwl The CycWorkerListener that wishes to listen to
   * this worker
   **/
  public void addListener(CycWorkerListener cwl) {
    listeners.add(CycWorkerListener.class, cwl);
  }
  
  /**
   * Removes a worker listener from this worker.
   * @param cwl The CycWorkerListener that no longer wishes to
   * be notified of this workers events.
   */
  public void removeListener(CycWorkerListener cwl) {
    listeners.remove(CycWorkerListener.class, cwl);
  }
  
  /**
   * Return a list of all CycWorkerListeners listening to
   * this CycWorker.
   * @return A non-null array of CycWorkerListener objects.
   */
  public Object[] getListeners() {
    return listeners.getListeners(CycWorkerListener.class);
  }
    
  final private static int CYC_WORKER_START=0;
  final private static int CYC_WORKER_INTERRUPT=1;
  final private static int CYC_WORKER_FINISHED=2;
    
  private void notifyStatChange(int eventType) {
    try {
      Object[] curListeners = listeners.getListenerList();
      CycWorkerEvent cwe = null;
      for (int i = curListeners.length-2; i>=0; i-=2) {
	if (curListeners[i]==CycWorkerListener.class) {
	  if (cwe == null) { cwe = new CycWorkerEvent(this); }
	  switch(eventType) {
	  case CYC_WORKER_START:
	    ((CycWorkerListener)curListeners[i+1]).
	      notifyWorkerStarted(cwe);
	    break;
	  case CYC_WORKER_INTERRUPT:
	    ((CycWorkerListener)curListeners[i+1]).
	      notifyWorkerInterrupted(cwe);
	    break;
	  case CYC_WORKER_FINISHED:
	    ((CycWorkerListener)curListeners[i+1]).
	      notifyWorkerFinished(cwe);
	    break;
	  }
	}
      }
    } catch (Exception e) { e.printStackTrace(); }
  }
  
  /** 
   * This method starts the backround processing of a particular
   * CycWorker. Appropriate listeners are notified when this
   * is called.
   */
  public void start() {
    notifyStatChange(CYC_WORKER_START);
    super.start();
  }
      
  /** 
   * This method interrupts a currently running CycWorker.
   * Appropriate listeners are notified when this is called.
   */       
  public void interrupt() {
    notifyStatChange(CYC_WORKER_INTERRUPT);
    super.interrupt();
  }
  
  /** 
   * This method should be called internally only...public for
   * Java related reasons only. It is automatically called
   * then the construct() method finishes.
   * Appropriate listeners are notified when this is called.
   */
  public void finished() {
    notifyStatChange(CYC_WORKER_FINISHED);
    super.finished();
  }
  
  /**
   * Exposed the get() method from SwingWorker class.
   * Returns the result of this CycWorker. Returns
   * null if worker not started or worker interrupted before 
   * processing could be completed. If processing is ongoing,
   * will block till finished.
   * @return Returns the result of this CycWorker or null if
   * CycWorker interrupted or not started. 
   **/
  public Object getWork() throws Exception { 
    if (getException() != null) {
      throw getException();
    }
    return super.get(); 
  }

  /**
   * @return the <tt>Thread</tt> doing the work of this worker.
   **/
  public Thread getThread() {
    return threadVar.get();
  }

}
