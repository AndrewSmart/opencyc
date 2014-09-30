/* $Id: CycWorkerQueue.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2007 Cycorp, Inc.  (Copyright is assigned to the United States Government under DFARS 252.227-7020).
 */

package org.opencyc.util;



//// External Imports
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * <P>CycWorkerQueue is designed to execute multiple CycWorker instances serially,
 * in first-in, first-out order. Each worker will run until its thread dies.
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  (Copyright is assigned to the United States Government under DFARS 252.227-7020).
 *
 * @author baxter
 * @version $Id: CycWorkerQueue.java 138070 2012-01-10 19:46:08Z sbrown $
 * @date March 27, 2008, 12:19 PM
 */
public class CycWorkerQueue {
  
  //// Constructors
  
  /**
   * Creates a new instance of CycWorkerQueue.
   */
  public CycWorkerQueue() {
    this("Cyc Worker Queue");
  }
  
  /**
   * Creates a new instance of CycWorkerQueue whose thread has the given name.
   */
  public CycWorkerQueue(final String name) {
    thread.setName(name);
    thread.start();
  }
  
  //// Public Area
  
  /** Set <tt>worker</tt> to be started as soon as all previously enqueued workers are done. */
  public void enqueue(CycWorker worker) {
    workerQueue.add(worker);
  }
  
  //// Protected Area
  
  //// Private Area
  private void processQueue() {
    while (true) {
      CycWorker worker = null;
      try {
        worker = getNextWorker();
      } catch (InterruptedException ie) {
        if (worker == null) {
          continue; //Go back to start of while loop and try again.
        }
      }
      worker.start();
      try {
        worker.getThread().join(); //Block until worker's thread is done.
      } catch (InterruptedException ex) {
        ex.printStackTrace();
      } catch (NullPointerException ex) {
        //Already done.
      }
    }
  }
  
  private CycWorker getNextWorker() throws InterruptedException {
    return workerQueue.take();
  }
  
  //// Internal Rep
  private BlockingQueue<CycWorker> workerQueue = new LinkedBlockingQueue<CycWorker>();
  private final Thread thread = new Thread() {
    public void run() {
      processQueue();
    }
  };
  
}
