/* $Id: DefaultSubLWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.api;

//// Internal Imports
import org.opencyc.cycobject.CycList;
import org.opencyc.util.*;

//// External Imports
import java.io.*;
import java.util.*;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

/**
 * <P>SubLWorkerSynch is designed to provide a handle for a particular
 * communication event with a Cyc server in a synchronous manner.
 * DefaultSubLWorkerSynch provides the default
 * implementation while SubLWorker and DefaultSubLWorker provide
 * asynchronous communications capabilities. Currently, SubLWorkerSynchs are one-shot,
 * i.e., a new SubLWorkerSynch needs to be created for every new communication.
 * SubLWorkerSynchs are cancelable, time-outable and provide means for incremental
 * return results.
 *
 * <P>Example usage: <code>
 * try {
 *    CycAccess access = new CycAccess("localhost", 3600);
 *    SubLWorkerSynch worker = new DefaultSubLWorkerSynch("(+ 1 1)", access);
 *    Object work = worker.getWork();
 *    System.out.println("Got worker: " + worker + "\nGot result: " + work + ".");
 *  } catch (Exception e) {
 *    e.printStackTrace();
 *  }
 * </code>
 *
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
 * @author tbrussea
 * @date March 25, 2004, 2:01 PM
 * @version $Id: DefaultSubLWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultSubLWorkerSynch
    extends DefaultSubLWorker
    implements SubLWorkerSynch, SubLWorkerListener {
  
  //// Constructors
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   */
  public DefaultSubLWorkerSynch(String subLCommand, CycAccess access) {
    this(access.makeCycList(subLCommand), access);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   */
  public DefaultSubLWorkerSynch(String subLCommand, CycAccess access,
      long timeoutMsecs) {
    this(access.makeCycList(subLCommand), access, timeoutMsecs);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   */
  public DefaultSubLWorkerSynch(String subLCommand, CycAccess access,
      boolean expectIncrementalResults) {
    this(access.makeCycList(subLCommand), access, expectIncrementalResults);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a String
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   * @param timeoutMsec the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */
  public DefaultSubLWorkerSynch(String subLCommand, CycAccess access,
      boolean expectIncrementalResults, long timeoutMsec) {
    this(access.makeCycList(subLCommand), access,
        expectIncrementalResults, timeoutMsec, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   */
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access) {
    this(subLCommand, access, false);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param timeoutMsecs the max time to wait in msecs for the work to
   * be completed before giving up (0 means to wait forever, and negative
   * values will cause an exception to be thrown). When communications time
   * out, an abort command is sent back to the Cyc server so processing will
   * stop there as well.
   */
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access,
      long timeoutMsecs) {
    this( subLCommand, access, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   */
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access,
      boolean expectIncrementalResults) {
    this( subLCommand, access, expectIncrementalResults, CycConnection.NORMAL_PRIORITY);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
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
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access,
      long timeoutMsecs, Integer priority) {
    this( subLCommand, access, false, timeoutMsecs, priority);
  }
  
  /** Creates a new instance of DefaultSubLWorkerSynch.
   * @param subLCommand the SubL command that does the work as a CycList
   * @param access the Cyc server that should process the SubL command
   * @param expectIncrementalResults boolean indicating wether to expect
   * incremental results
   * @param priority the priority at which the worker will be scheduled
   * on the CYC server side; 
   * @see getPriority()
   */
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access,
      boolean expectIncrementalResults, Integer priority) {
    this( subLCommand, access, expectIncrementalResults, 0, priority);
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
  public DefaultSubLWorkerSynch(CycList subLCommand, CycAccess access,
      boolean expectIncrementalResults, long timeoutMsecs, Integer priority) {
    super(subLCommand, access, expectIncrementalResults, timeoutMsecs, priority);
    addListener(this);
  }
  
  //// Public Area
  
  /** This method starts communications with the Cyc server, waits for the work
   * to be performed, then returns the resultant work.
   * @throws IOException thown when there is a problem with the communications
   * protocol with the CycServer
   * @throws TimeOutException thrown if the worker takes to long to return results
   * @throws CycApiException thrown if any other error occurs
   * @return The work produced by this SubLWorkerSynch
   */
  public Object getWork()
      throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException {
    if (getStatus() == SubLWorkerStatus.NOT_STARTED_STATUS) {
      start();
    }
    if (getStatus() == SubLWorkerStatus.WORKING_STATUS) {
      try {
        boolean gotResults = true;
        if (getTimeoutMsecs() > 0) {
          gotResults = sem.tryAcquire(getTimeoutMsecs(), TimeUnit.MILLISECONDS);
        } else {
          sem.acquire();
        }
        if ((!gotResults) || (getStatus() == SubLWorkerStatus.WORKING_STATUS)) {
          try {
            this.abort();
          } catch (IOException xcpt) {
            throw xcpt;
          } finally {
            this.fireSubLWorkerTerminatedEvent(new SubLWorkerEvent(this,
                SubLWorkerStatus.EXCEPTION_STATUS,
                new TimeOutException("Communications took more than: " + getTimeoutMsecs() + " msecs.\nWhile trying to execute: \n" + getSubLCommand().toPrettyCyclifiedString(""))));
          }
        }
      } catch (Exception xcpt) {
        setException(xcpt);
      }
    }
    if (getException() != null) {
      try {
        throw getException();
      } catch (IOException ioe) {
        throw ioe;
      } catch (TimeOutException toe) {
        throw toe;
      } catch (CycApiInvalidObjectException cae) {
        setException(new CycApiInvalidObjectException(cae.getMessage(), cae));
        throw (RuntimeException) getException();
      } catch (CycApiServerSideException cae) {
        setException(new CycApiServerSideException(cae.getMessage(), cae));
        throw (RuntimeException) getException();
      } catch (CycApiException cae) {
        setException(new CycApiException(cae.getMessage(), cae));
        throw (RuntimeException) getException();
      } catch (InterruptedException ie) {
        setException(new OpenCycTaskInterruptedException(ie));
        throw (RuntimeException) getException();
      } catch (RuntimeException re) {
        throw re;
      } catch (Exception xcpt) {
        setException(new RuntimeException(xcpt));
        throw (RuntimeException) getException();
      }
    }
    return work;
  }
  
  /** Ignore.
   * @param event the event object with details about this event
   */
  public void notifySubLWorkerStarted(SubLWorkerEvent event) {}
  
  /** Saves any  available work.
   * @param event the event object with details about this event
   */
  public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
    appendWork(event.getWork());
  }
  
  /** Make sure to save any applicable exceptions,
   * @param event the event object with details about this event
   */
  public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
    setException(event.getException());
    sem.release(Integer.MAX_VALUE);
  }
  
  /** Returns the exception thrown in the process of doing the work.
   * The value will be null if now exception has been thrown.
   * @return the exception thrown in the process of doing the work
   */
  public Exception getException() { return e; }
  
  //// Protected Area
  
  /** Sets the exception.
   * @param e The exception that was thrown while processing this worker
   */
  protected void setException(Exception e) {
    this.e = e;
  }
  
  /** Make sure to keep track of the resulting work, especially in the
   * case if incremental return results.
   * @param newWork The lastest batch of work.
   */
  protected void appendWork(Object newWork) {
    if (expectIncrementalResults()) {
      if (work == null) {
        work = new CycList();
      }
      if (newWork != CycObjectFactory.nil) {
        ((List)work).addAll((List)newWork);
      }
    } else {
      work = newWork;
    }
  }
  
  //// Private Area
  
  //// Internal Rep
  
  private final Semaphore sem = new Semaphore(0);
  volatile private Object work = null;
  volatile private Exception e = null;
  
  /** For tesing puposes only. */
  static SubLWorkerSynch testWorker;
  
  //// Main
  
  /** Test main method and example usage.
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    try {
      CycAccess access = new CycAccess("localhost", 3600);
      SubLWorkerSynch worker = new DefaultSubLWorkerSynch("(+ 1 1)", access);
      Object work = worker.getWork();
      System.out.println("Got worker: " + worker + "\nGot result: " + work + ".");
    } catch (Exception e) {
      e.printStackTrace();
    }
    try {
      final CycAccess access = new CycAccess("localhost", 3600);
      Thread workerThread = new Thread() {
        public void run() {
          try {
            System.out.println("Starting work.");
            testWorker = new DefaultSubLWorkerSynch("(do-assertions (a))", access);
            Object obj = testWorker.getWork();
            System.out.println("Finished work with " + testWorker.getStatus().getName()
            + ", received: " + obj);
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      };
      workerThread.start();
      Thread.currentThread().sleep(10000);
      System.out.println("About to cancel work.");
      testWorker.cancel();
      System.out.println("Canceled work.");
      
      System.out.println("\nGiving chance to get ready ....\n");
      Thread.currentThread().sleep(1000);
      
      System.out.println( "\nOk, second round ....\n\n");
      workerThread = new Thread() {
        public void run() {
          try {
            System.out.println("Starting work.");
            testWorker = new DefaultSubLWorkerSynch("(do-assertions (a))", access);
            Object obj = testWorker.getWork();
            System.out.println("Finished work with " + testWorker.getStatus().getName()
            + ", received: " + obj);
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      };
      workerThread.start();
      Thread.currentThread().sleep(10000);
      System.out.println("About to abort work.");
      testWorker.abort();
      System.out.println("Aborted work.");
      
      System.out.println("\nGiving chance to get ready ....\n");
      Thread.currentThread().sleep(1000);
      
      System.out.println( "\nOk, third round ....\n\n");
      workerThread = new Thread() {
        public void run() {
          long timeBefore = 0, timeAfter = 0;
          try {
            System.out.println("Starting work.");
            timeBefore = System.currentTimeMillis();
            testWorker = new DefaultSubLWorkerSynch("(do-assertions (a))", access, 500);
            Object obj = testWorker.getWork();
            timeAfter = System.currentTimeMillis();
            System.out.println("Finished work with " + testWorker.getStatus().getName()
            + " after " + (timeAfter - timeBefore)
            + " millisecs (should be about 500), received: " + obj);
          } catch (Exception e) {
            timeAfter = System.currentTimeMillis();
            System.out.println( "The current time is: " + (timeAfter - timeBefore)
            + " millisecs (should be about 500)");
            e.printStackTrace();
          }
        }
      };
      workerThread.start();
      Thread.currentThread().sleep(10000);
    } catch (Exception e) {
      e.printStackTrace();
    }
    System.exit(0);
  }
  
}
