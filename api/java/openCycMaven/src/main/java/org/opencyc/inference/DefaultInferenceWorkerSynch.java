/* $Id: DefaultInferenceWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.inference;

//// Internal Imports
import org.opencyc.api.*;
import org.opencyc.cycobject.*;
import org.opencyc.util.*;

//// External Imports
import java.util.*;
import java.io.*;
import org.opencyc.parser.CycLParserUtil;

/**
 * <P>DefaultInferenceWorkerSynch provides a synchronous version of the DefaultInferenceWorker.
 *
 * <p>Copyright 2005 Cycorp, Inc., license is open source GNU LGPL.
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
 * @author tbrussea, zelal
 * @date July 27, 2005, 11:55 AM
 * @version $Id: DefaultInferenceWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultInferenceWorkerSynch extends DefaultInferenceWorker implements InferenceWorkerSynch {
  
  //// Constructors
  
  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(String query, ELMt mt, 
      InferenceParameters queryProperties, CycAccess access, long timeoutMsecs) {
    this(makeCycLSentence(query, access), mt, queryProperties,
      null, null, false, access, timeoutMsecs);
  }
  
  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(CycList<Object> query, ELMt mt, 
      InferenceParameters queryProperties, CycAccess access, long timeoutMsecs) {
    super(query, mt, queryProperties, access, timeoutMsecs);
    init();
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(CycFormulaSentence query, ELMt mt,
      InferenceParameters queryProperties, CycAccess access, long timeoutMsecs) {
    super(query.getArgs(), mt, queryProperties, access, timeoutMsecs);
    init();
  }
  
  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param nlGenerationProperties the natural language generation properties
   * @param answerProcessingFunction the answer processing function
   * @param optimizeVariables the indicatior for whether variables are optimized
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(String query, ELMt mt, 
      InferenceParameters queryProperties, Map nlGenerationProperties, 
      CycSymbol answerProcessingFunction, boolean optimizeVariables, 
      CycAccess access, long timeoutMsecs) {
    this(makeCycLSentence(query, access), mt, queryProperties, nlGenerationProperties,
      answerProcessingFunction, optimizeVariables, access, timeoutMsecs);
  }
  
  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param nlGenerationProperties the natural language generation properties
   * @param answerProcessingFunction the answer processing function
   * @param optimizeVariables the indicatior for whether variables are optimized
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(CycList<Object> query, ELMt mt, 
      InferenceParameters queryProperties, Map nlGenerationProperties, 
      CycSymbol answerProcessingFunction, boolean optimizeVariables, 
      CycAccess access, long timeoutMsecs) {
    this(query, mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
      optimizeVariables, access, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param nlGenerationProperties the natural language generation properties
   * @param answerProcessingFunction the answer processing function
   * @param optimizeVariables the indicatior for whether variables are optimized
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   */
  public DefaultInferenceWorkerSynch(CycFormulaSentence query, ELMt mt,
      InferenceParameters queryProperties, Map nlGenerationProperties,
      CycSymbol answerProcessingFunction, boolean optimizeVariables,
      CycAccess access, long timeoutMsecs) {
    this(query, mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
      optimizeVariables, access, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param nlGenerationProperties the natural language generation properties
   * @param answerProcessingFunction the answer processing function
   * @param optimizeVariables the indicatior for whether variables are optimized
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   * @param priority the SubL task priority
   */
  public DefaultInferenceWorkerSynch(CycList query, ELMt mt, 
      InferenceParameters queryProperties, Map nlGenerationProperties, 
      CycSymbol answerProcessingFunction, boolean optimizeVariables, 
      CycAccess access, long timeoutMsecs, Integer priority) {
    super(query, mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
      optimizeVariables, access, timeoutMsecs, priority);
    init();
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query the query sentence
   * @param mt the inference microtheory
   * @param queryProperties the query properties
   * @param nlGenerationProperties the natural language generation properties
   * @param answerProcessingFunction the answer processing function
   * @param optimizeVariables the indicatior for whether variables are optimized
   * @param access the Cyc communications object
   * @param timeoutMsecs the timeout duration in milliseconds
   * @param priority the SubL task priority
   */
  public DefaultInferenceWorkerSynch(CycFormulaSentence query, ELMt mt,
      InferenceParameters queryProperties, Map nlGenerationProperties,
      CycSymbol answerProcessingFunction, boolean optimizeVariables,
      CycAccess access, long timeoutMsecs, Integer priority) {
    super(query, mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
      optimizeVariables, access, timeoutMsecs, priority);
    init();
  }
  
  //// Public Area

  /** Performs the synchronous inference operation. */
  public InferenceResultSet executeQuery()
  throws IOException, TimeOutException, CycApiException {
    List results = performSynchronousInference();
    return new InferenceResultSet(results, this);
  }
  
  /** Performs the synchronous inference operation. */
  public List performSynchronousInference()
  throws IOException, TimeOutException, CycApiException {
    if (getStatus() == SubLWorkerStatus.NOT_STARTED_STATUS) {
      start();
    }
    if (getStatus() == SubLWorkerStatus.WORKING_STATUS) {
      try {
        synchronized (lock) {
          lock.wait(getTimeoutMsecs());
          if (getStatus() == SubLWorkerStatus.WORKING_STATUS) {
            try {
              this.abort();
            } catch (IOException xcpt) {
              throw xcpt;
            } finally {
              this.fireSubLWorkerTerminatedEvent(new SubLWorkerEvent(this,
                SubLWorkerStatus.EXCEPTION_STATUS, 
                new TimeOutException("Communications took more than: " 
                + getTimeoutMsecs() + " msecs.\nWhile trying to execute inference: \n" 
                + getSubLCommand().toPrettyCyclifiedString(""))));
            }
          }
        }
      } catch (Exception xcpt) {
        throw new RuntimeException(xcpt);
      }
    }
    if (getException() != null) { 
      try {
        throw getException(); 
      } catch (IOException ioe) {
        throw ioe; 
      } catch (Exception xcpt) {
        if (xcpt instanceof RuntimeException) {
          throw (RuntimeException)xcpt;
        } else {
          throw new RuntimeException(xcpt);
        }
      }
    }
    return getAnswers();
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
  
  //// Private Area
  
  /** Initializes this object by registering an inference event listener. */
  private void init() {
    addInferenceListener(new InferenceWorkerListener () {
      public void notifyInferenceAnswersAvailable(InferenceWorker inferenceWorker, List newAnswers) {
      }

      public void notifyInferenceCreated(InferenceWorker inferenceWorker) {
      }

      public void notifyInferenceStatusChanged(InferenceStatus oldStatus, InferenceStatus newStatus, InferenceWorkerSuspendReason suspendReason, InferenceWorker inferenceWorker) {
      }

      public void notifyInferenceTerminated(InferenceWorker inferenceWorker, Exception e) {
        synchronized(lock) {
          lock.notify();
        }
      }
    });
    
  }
  
  private static CycFormulaSentence makeCycLSentence(String query, CycAccess access) {
    try {
      return CycLParserUtil.parseCycLSentence(query, true, access);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  
  //// Internal Rep
  
  /** the lock for the inference timeout */
  private final Object lock = new Object();
  
  /** the exception that was thrown while processing this worker */
  private Exception e = null;

  
  //// Main
  
  /** Provides a demonstration main method.
   *
   * @param args the command line arguments (ignored)
   */
  public static void main(String[] args) {
    System.out.println("Starting");
    CycAccess access = null;
    try {
      access = new CycAccess("localhost", 3660);
      String query = "(#$isa ?X #$Dog)";
      InferenceWorkerSynch worker = new DefaultInferenceWorkerSynch(query, 
        CycAccess.inferencePSC, null, access, 500000);
      InferenceResultSet rs = worker.executeQuery();
      try {
        int indexOfX = rs.findColumn("?X");
        while (rs.next()) {
          CycObject curDog = rs.getCycObject(indexOfX);
          System.out.println("Got dog: " + curDog.cyclify());
        }
        System.out.flush();
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
    System.exit(0);
  }
  
}
