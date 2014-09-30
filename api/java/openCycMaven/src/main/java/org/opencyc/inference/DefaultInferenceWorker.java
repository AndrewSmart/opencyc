/* $Id: DefaultInferenceWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// External Imports
import java.util.ArrayList;
import java.util.Collections;
import java.util.EventListener;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.swing.event.EventListenerList;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.api.CycConnection;
import org.opencyc.api.CycObjectFactory;
import org.opencyc.api.DefaultSubLWorker;
import org.opencyc.api.DefaultSubLWorkerSynch;
import org.opencyc.api.SubLWorkerEvent;
import org.opencyc.api.SubLWorkerListener;
import org.opencyc.api.SubLWorkerSynch;
import org.opencyc.cycobject.CycFormulaSentence;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.ELMt;
import org.opencyc.parser.CycLParserUtil;

/**
 * <P>DefaultInferenceWorker is designed to...
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
 * @version $Id: DefaultInferenceWorker.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultInferenceWorker extends DefaultSubLWorker implements InferenceWorker {

  //// Constructors
  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(String query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs) {
    this(makeCycLSentence(query, access), mt, queryProperties,
            access, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(CycList query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs) {
    this(query, mt, queryProperties, access, timeoutMsecs,
            CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(CycFormulaSentence query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs) {
    this(query.getArgs(), mt, queryProperties, access, timeoutMsecs);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   * @param priority
   */
  public DefaultInferenceWorker(String query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs, Integer priority) {
    this(makeCycLSentence(query, access), mt, queryProperties,
            DEFAULT_NL_GENERATION_PROPERTIES, null, false, access, timeoutMsecs, priority);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(CycList query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs, Integer priority) {
    super(access.makeCycList(createInferenceCommand(query, mt, queryProperties,
            DEFAULT_NL_GENERATION_PROPERTIES, null, false, access)), access, true, timeoutMsecs, priority);
    init();
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(CycFormulaSentence query, ELMt mt, InferenceParameters queryProperties,
          CycAccess access, long timeoutMsecs, Integer priority) {
    super(access.makeCycList(createInferenceCommandInternal(query, mt, queryProperties,
            DEFAULT_NL_GENERATION_PROPERTIES, null, false, access)), access, true, timeoutMsecs, priority);
    init();
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param nlGenerationProperties
   * @param answerProcessingFunction
   * @param optimizeVariables
   * @param access
   * @param timeoutMsecs
   */
  public DefaultInferenceWorker(String query, ELMt mt, InferenceParameters queryProperties,
          Map nlGenerationProperties, CycSymbol answerProcessingFunction,
          boolean optimizeVariables, CycAccess access, long timeoutMsecs) {
    this(makeCycLSentence(query, access), mt, queryProperties, nlGenerationProperties,
            answerProcessingFunction, optimizeVariables, access, timeoutMsecs,
            CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param nlGenerationProperties
   * @param answerProcessingFunction
   * @param optimizeVariables
   * @param access
   * @param timeoutMsecs
   * @param priority
   */
  public DefaultInferenceWorker(CycList query, ELMt mt, InferenceParameters queryProperties,
          Map nlGenerationProperties, CycSymbol answerProcessingFunction,
          boolean optimizeVariables, CycAccess access, long timeoutMsecs) {
    this(query, mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
            optimizeVariables, access, timeoutMsecs, CycConnection.NORMAL_PRIORITY);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param nlGenerationProperties
   * @param answerProcessingFunction
   * @param optimizeVariables
   * @param access
   * @param timeoutMsecs
   * @param priority
   */
  public DefaultInferenceWorker(CycFormulaSentence query, ELMt mt, InferenceParameters queryProperties,
          Map nlGenerationProperties, CycSymbol answerProcessingFunction,
          boolean optimizeVariables, CycAccess access, long timeoutMsecs) {
    this(query.getArgs(), mt, queryProperties, nlGenerationProperties, answerProcessingFunction,
            optimizeVariables, access, timeoutMsecs);
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param nlGenerationProperties
   * @param answerProcessingFunction
   * @param optimizeVariables
   * @param access
   * @param timeoutMsecs
   * @param priority
   */
  public DefaultInferenceWorker(CycList query, ELMt mt, InferenceParameters queryProperties,
          Map nlGenerationProperties, CycSymbol answerProcessingFunction,
          boolean optimizeVariables, CycAccess access, long timeoutMsecs, Integer priority) {
    super(access.makeCycList(createInferenceCommand(query, mt, queryProperties,
            nlGenerationProperties, answerProcessingFunction, optimizeVariables, access)),
            access, true, timeoutMsecs, priority);
    this.answerProcessingFunction = answerProcessingFunction;
    init();
  }

  /**
   * Creates a new instance of DefaultInferenceWorker.
   * @param query
   * @param mt
   * @param queryProperties
   * @param nlGenerationProperties
   * @param answerProcessingFunction
   * @param optimizeVariables
   * @param access
   * @param timeoutMsecs
   * @param priority
   */
  public DefaultInferenceWorker(CycFormulaSentence query, ELMt mt, InferenceParameters queryProperties,
          Map nlGenerationProperties, CycSymbol answerProcessingFunction,
          boolean optimizeVariables, CycAccess access, long timeoutMsecs, Integer priority) {
    super(access.makeCycList(createInferenceCommandInternal(query, mt, queryProperties,
            nlGenerationProperties, answerProcessingFunction, optimizeVariables, access)),
            access, true, timeoutMsecs, priority);
    this.answerProcessingFunction = answerProcessingFunction;
    init();
  }

  //// Public Area
  public void releaseInferenceResources(long timeoutMsecs)
          throws java.io.IOException, org.opencyc.util.TimeOutException, CycApiException {
    abort();
    SubLWorkerSynch subLWorker = new DefaultSubLWorkerSynch("(destroy-problem-store "
            + "(find-problem-store-by-id " + getProblemStoreId() + "))",
            getCycServer(), timeoutMsecs);
    subLWorker.getWork();
  }

  public static void releaseAllInferenceResourcesForClient(CycAccess cycAccess, long timeoutMsecs)
          throws java.io.IOException, org.opencyc.util.TimeOutException, CycApiException {
    SubLWorkerSynch subLWorker = new DefaultSubLWorkerSynch("(open-cyc-release-inference-resources-for-client)", cycAccess, timeoutMsecs);
    subLWorker.getWork();
  }

  /**
   * Returns all the InferenceWorkerListeners listening in on this
   * InferenceWorker's events
   * @return all the InferenceWorkerListeners listening in on this
   * InferenceWorker's events
   */
  public Object[] getInferenceListeners() {
    synchronized (inferenceListeners) {
      return inferenceListeners.getListeners(inferenceListenerClass);
    }
  }

  /**
   * Adds the given listener to this InferenceWorker.
   * @param listener the listener that wishes to listen
   * for events sent out by this InferenceWorker
   */
  public void addInferenceListener(InferenceWorkerListener listener) {
    synchronized (inferenceListeners) {
      inferenceListeners.add(inferenceListenerClass, listener);
    }
  }

  /**
   * Removes the given listener from this InferenceWorker.
   * @param listener the listener that no longer wishes
   * to receive events from this InferenceWorker
   */
  public void removeInferenceListener(InferenceWorkerListener listener) {
    synchronized (inferenceListeners) {
      inferenceListeners.remove(inferenceListenerClass, listener);
    }
  }

  /** Removes all listeners from this InferenceWorker. */
  public void removeAllInferenceListeners() {
    synchronized (inferenceListeners) {
      Object[] listenerArray = inferenceListeners.getListenerList();
      for (int i = 0, size = listenerArray.length; i < size; i += 2) {
        inferenceListeners.remove((Class) listenerArray[i],
                (EventListener) listenerArray[i + 1]);
      }
    }
  }

  //public void continueInference() {
  //  throw new UnsupportedOperationException("continueInference() needs to be implemented.");
  //}
  // with infinite patience
  public void interruptInference() {
    interruptInference(null);
  }

  // with said patience
  public void interruptInference(int patience) {
    interruptInference(new Integer(patience));
  }

  protected void interruptInference(Integer patience) {
    String command = createInferenceInterruptCommand(patience);
    DefaultSubLWorker newWorker = new DefaultSubLWorker(command, getCycServer(), true, 0);
    SubLWorkerListener listener = new SubLWorkerListener() {

      public void notifySubLWorkerStarted(SubLWorkerEvent event) {
      }

      public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
      }

      public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
//      System.out.println("Inference Interrupted "+event.getStatus()+" "+event.getWork());
      }
    };
    newWorker.addListener(listener);
    try {
      //System.out.println("running "+command);
      newWorker.start();
      //Object result = newWorker.getWork();
      //System.out.println(result);
    } catch (java.io.IOException ioe) {
      throw new RuntimeException("Failed to continue inference (IOException).");
    }
  }

  public void continueInference(InferenceParameters queryProperties) {
    String command = createInferenceContinuationCommand(queryProperties);
    DefaultSubLWorker newWorker = new DefaultSubLWorker(command, getCycServer(), true, getTimeoutMsecs());
    /*newWorker.addListener(new SubLWorkerListener() {
    public void notifySubLWorkerStarted(SubLWorkerEvent event) {}
    public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {}
    public void notifySubLWorkerTerminated(SubLWorkerEvent event) {}
    });*/
    newWorker.addListener(new SubLWorkerListener() {

      public void notifySubLWorkerStarted(SubLWorkerEvent event) {
        doSubLWorkerStarted(event);
      }

      public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
        doSubLWorkerDataAvailable(event);
      }

      public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
        doSubLWorkerTerminated(event);
      }
    });
    try {
      newWorker.start();
    } catch (java.io.IOException ioe) {
      throw new RuntimeException("Failed to continue inference (IOException).");
    }
    //throw new UnsupportedOperationException("continueInference() needs to be implemented.");
  }

  public void abort() throws java.io.IOException {
    //String command = createInferenceAbortionCommand();
    //DefaultSubLWorkerSynch newWorker = new DefaultSubLWorkerSynch(command, getCycServer(), false, getTimeoutMsecs());
    //newWorker.getWork();
    if (this.suspendReason == InferenceWorkerSuspendReason.INTERRUPT) {
      this.suspendReason = InferenceWorkerSuspendReason.ABORTED;
    }
    super.abort();
  }

  /**
   *
   * @param index
   * @return
   */
  public Object getAnswerAt(int index) {
    return answers.get(index);
  }

  /**
   *
   * @return
   */
  public int getAnswersCount() {
    return answers.size();
  }

  /**
   *
   * @return
   */
  public List getAnswers() {
    synchronized (answers) {
      return new CycList(answers);
    }
  }

  /**
   *
   * @param startIndex
   * @param endIndex
   * @return
   */
  public List getAnswers(int startIndex, int endIndex) {
    return new ArrayList(answers.subList(startIndex, endIndex));
  }

  /**
   *
   * @return
   */
  public int getInferenceId() {
    return inferenceId;
  }

  @Override
  public InferenceIdentifier getInferenceIdentifier() {
    return new InferenceIdentifier(getProblemStoreId(), getInferenceId(), getCycServer());
  }

  /**
   *
   * @return
   */
  public InferenceStatus getInferenceStatus() {
    return status;
  }

  /**
   *
   * @return
   */
  public int getProblemStoreId() {
    return problemStoreId;
  }

  /**
   * Returns a string representation of the InferenceWorker.
   * @return a string representation of the InferenceWorker
   */
  public String toString() {
    return toString(2);
  }

  /** Returns a string representation of the InferenceWorker.
   * @return a string representation of the InferenceWorker
   * @param indentLength the number of spaces to preceed each line of
   * output String
   */
  public String toString(int indentLength) {
    final String newline = System.getProperty("line.separator");
    final StringBuffer nlBuff = new StringBuffer();
    nlBuff.append(newline);
    for (int i = 1; i < indentLength; i++) {
      nlBuff.append(" ");
    }
    final String newlinePlusIndent = nlBuff.toString();
    nlBuff.append(super.toString(indentLength));
    nlBuff.append("Inference id: ").append(inferenceId).append(newlinePlusIndent);
    nlBuff.append("ProblemStore id: ").append(problemStoreId).append(newlinePlusIndent);
    nlBuff.append("Status: ").append(status).append(newlinePlusIndent);
    if (status == InferenceStatus.SUSPENDED) {
      nlBuff.append("Suspend reason: ").append(suspendReason).append(newlinePlusIndent);
    }
    nlBuff.append(getAnswersCount()).append(" answers").append(newlinePlusIndent);
    final int maxAnswersToShow = 10;
    if (getAnswersCount() > maxAnswersToShow) {
      nlBuff.append("First ").append(maxAnswersToShow).append(": ").append(newlinePlusIndent);
      for (int i = 0; i < maxAnswersToShow; i++) {
        nlBuff.append(answers.get(i)).append(newlinePlusIndent);
      }
    } else {
      for (Iterator i = answers.iterator(); i.hasNext();) {
        nlBuff.append(i.next()).append(newlinePlusIndent);
      }
    }
    return nlBuff.toString();
  }

  /**
   *
   * @return
   */
  public InferenceWorkerSuspendReason getSuspendReason() {
    return suspendReason;
  }
  //// Protected Area

  //// Private Area
  private void fireInferenceStatusChanged(final InferenceStatus oldStatus) throws RuntimeException {
    Object[] curListeners = getInferenceListeners();
    List<Exception> errors = new ArrayList<Exception>();
    for (int i = curListeners.length - 1; i >= 0; i -= 1) {
      try {
        ((InferenceWorkerListener) curListeners[i]).notifyInferenceStatusChanged(oldStatus, status, suspendReason, this);
      } catch (Exception e) {
        errors.add(e);
      }
    }
    if (errors.size() > 0) {
      throw new RuntimeException(errors.get(0)); // @hack
    }
  }

  private void init() {
    this.addListener(new SubLWorkerListener() {

      public void notifySubLWorkerStarted(SubLWorkerEvent event) {
        doSubLWorkerStarted(event);
      }

      public void notifySubLWorkerDataAvailable(SubLWorkerEvent event) {
        doSubLWorkerDataAvailable(event);
      }

      public void notifySubLWorkerTerminated(SubLWorkerEvent event) {
        doSubLWorkerTerminated(event);
      }
    });
  }

  private void doSubLWorkerStarted(SubLWorkerEvent event) {
    InferenceStatus oldStatus = status;
    status = InferenceStatus.STARTED;
    Object[] curListeners = getInferenceListeners();
    List<Exception> errors = new ArrayList<Exception>();
    for (int i = curListeners.length - 1; i >= 0; i -= 1) {
      try {
        ((InferenceWorkerListener) curListeners[i]).notifyInferenceStatusChanged(oldStatus, status, null, this);
      } catch (Exception e) {
        errors.add(e);
      }
    }
    if (errors.size() > 0) {
      throw new RuntimeException(errors.get(0)); // @hack
    }
  }

  private void doSubLWorkerDataAvailable(SubLWorkerEvent event) {
    Object obj = event.getWork();
    if ((obj == null) || (!(obj instanceof CycList))) {
      if (CycObjectFactory.nil.equals(obj)) {
        return;
      }
      throw new RuntimeException("Got invalid result from inference: " + obj);
    }
    final CycList data = (CycList) obj;
    if (data.size() < 2) {
      throw new RuntimeException("Got wrong number of arguments " + "from inference result: " + data);
    }
    Object obj2 = data.get(0);
    if ((obj2 == null) || (!(obj2 instanceof CycSymbol))) {
      throw new RuntimeException("Got bad result keyword " + "from inference result: " + obj2);
    }
    CycSymbol keyword = (CycSymbol) obj2;
    final String keywordName = keyword.toCanonicalString();
    if (keywordName.equals(":INFERENCE-START")) {
      handleInferenceInitializationResult(data);
    } else if (keywordName.equals(":INFERENCE-ANSWER")) {
      handleInferenceAnswerResult(data);
    } else if (keywordName.equals(":INFERENCE-STATUS")) {
      handleInferenceStatusChangedResult(data);
    }
  }

  private void doSubLWorkerTerminated(SubLWorkerEvent event) {
    Object[] curListeners = getInferenceListeners();
    List<Exception> errors = new ArrayList<Exception>();
    for (int i = curListeners.length - 1; i >= 0; i -= 1) {
      try {
        ((InferenceWorkerListener) curListeners[i]).notifyInferenceTerminated(this, event.getException());
      } catch (Exception e) {
        errors.add(e);
      }
    }
    if (errors.size() > 0) {
      throw new RuntimeException(errors.get(0)); // @hack
    }
  }

  private void handleInferenceInitializationResult(CycList data) {
    if (data.size() != 3) {
      throw new RuntimeException("Got wrong number of arguments " + "from inference result (expected 3): " + data);
    }
    Object problemStoreObj = data.get(1);
    Object inferenceObj = data.get(2);
    if ((problemStoreObj == null) || (!(problemStoreObj instanceof Number))) {
      throw new RuntimeException("Got bad inference problem store id: " + problemStoreObj);
    }
    if ((inferenceObj == null) || (!(inferenceObj instanceof Number))) {
      throw new RuntimeException("Got bad inference id: " + inferenceObj);
    }
    problemStoreId = ((Number) problemStoreObj).intValue();
    inferenceId = ((Number) inferenceObj).intValue();
    List<Exception> errors = new ArrayList<Exception>();
    Object[] curListeners = getInferenceListeners();
    for (int i = curListeners.length - 1; i >= 0; i -= 1) {
      try {
        ((InferenceWorkerListener) curListeners[i]).notifyInferenceCreated(this);
      } catch (Exception e) {
        errors.add(e);
      }
    }
    if (errors.size() > 0) {
      throw new RuntimeException(errors.get(0)); // @hack
    }
  }

  private void handleInferenceAnswerResult(CycList data) {
    if (data.size() != 2) {
      throw new RuntimeException("Got wrong number of arguments " + "from inference result (expected 2): " + data);
    }
    Object newAnswers = data.get(1);
    if ((newAnswers == null) || (!(newAnswers instanceof CycList))) {
      throw new RuntimeException("Got bad inference answers list: " + newAnswers);
    }
    answers.addAll((List) newAnswers);
    Object[] curListeners = getInferenceListeners();
    List<Exception> errors = new ArrayList<Exception>();
    for (int i = curListeners.length - 1; i >= 0; i -= 1) {
       try {
        ((InferenceWorkerListener) curListeners[i]).notifyInferenceAnswersAvailable(this, (List) newAnswers);
      } catch (Exception e) {
        errors.add(e);
      }
    }
    if (errors.size() > 0) {
      throw new RuntimeException(errors.get(0)); // @hack
    }
  }

  private void handleInferenceStatusChangedResult(CycList data) {
    // Expected format: (:INFERENCE-STATUS <STATUS-KEYWORD> <SUSPEND-REASON>)
    if (data.size() != 3) {
      throw new RuntimeException("Got wrong number of arguments " + "from inference status changed (expected 3): " + data);
    }
    Object statusObj = data.get(1);
    if ((statusObj == null) || (!(statusObj instanceof CycSymbol))) {
      throw new RuntimeException("Got bad inference status: " + statusObj);
    }
    InferenceStatus newStatus = InferenceStatus.findInferenceStatus((CycSymbol) statusObj);
    InferenceStatus oldStatus = status;
    status = newStatus;
    if (status == null) {
      throw new RuntimeException("Got bad inference status name: " + statusObj);
    }
    final Object cycSuspendReason = data.get(2);
    if (cycSuspendReason instanceof CycSymbol || cycSuspendReason == null) {
      suspendReason = InferenceWorkerSuspendReason.fromCycSymbol((CycSymbol) cycSuspendReason);
    } else if (cycSuspendReason instanceof CycList
            && InferenceWorkerSuspendReason.ERROR_SYMBOL.equals(((CycList) cycSuspendReason).get(0))) {
      suspendReason = InferenceWorkerSuspendReason.createFromErrorString((String) ((CycList) cycSuspendReason).get(1));
    } else {
      throw new RuntimeException("Unable to create InferenceWorkerSuspendReason from ("
              + cycSuspendReason.getClass().getName() + ") " + cycSuspendReason.toString());
    }
    fireInferenceStatusChanged(oldStatus);
  }

  /**
   * (define-api open-cyc-start-continuable-query (sentence mt &optional properties
   * (nl-generation-properties *default-open-cyc-nl-generation-properties*)
   * inference-answer-process-function
   * (incremental-results? *use-api-task-processor-incremental-results?*)
   * (optimize-query-sentence-variables? t))
   **/
  protected static String createInferenceCommand(CycList query, ELMt mt,
          InferenceParameters queryProperties, Map nlGenerationProperties,
          CycSymbol answerProcessingFunction, boolean optimizeVariables, CycAccess cycAccess) {
    return createInferenceCommandInternal(query, mt, queryProperties, nlGenerationProperties,
            answerProcessingFunction, optimizeVariables, cycAccess);
  }

  private static String createInferenceCommandInternal(CycObject query, ELMt mt,
          InferenceParameters queryProperties, Map nlGenerationProperties,
          CycSymbol answerProcessingFunction, boolean optimizeVariables, CycAccess cycAccess) {
    if (queryProperties == null) {
      queryProperties = new DefaultInferenceParameters(cycAccess);
    }
    if ((answerProcessingFunction != null) && (!answerProcessingFunction.shouldQuote())) {
      answerProcessingFunction = new CycSymbol(answerProcessingFunction.getPackageName(),
              answerProcessingFunction.getSymbolName());
    }
    String processingFnStr = ((answerProcessingFunction != null) ? answerProcessingFunction.stringApiValue() : "nil");
    queryProperties.put(new CycSymbol(":CONTINUABLE?"), Boolean.TRUE);
    return "(open-cyc-start-continuable-query " + query.stringApiValue() + " " + mt.stringApiValue() + " " + queryProperties.stringApiValue() + " " + CycList.convertMapToPlist(nlGenerationProperties).stringApiValue() + " " + processingFnStr + " t " + (optimizeVariables ? "t" : "nil") + ")";
  }

  /**
   * @param patience - seconds to wait; 0 -> no patience ; null -> inf patience
   **/
  protected static String createInferenceInterruptCommand(int problemStoreId, int inferenceId, Integer patience) {
    String patienceStr = patience == null ? "NIL" : patience.toString();
    return "(cdr (list (inference-interrupt (find-inference-by-ids "
            + problemStoreId + " " + inferenceId + ") " + patienceStr + ")))";
  }

  protected String createInferenceInterruptCommand(Integer patience) {
    return DefaultInferenceWorker.createInferenceInterruptCommand(
            problemStoreId, inferenceId, patience);
  }

  /**
   * (define-api open-cyc-continue-query (problem-store-id inference-id properties
   * &optional (nl-generation-properties *default-open-cyc-nl-generation-properties*)
   * inference-answer-process-function
   * (incremental-results? *use-api-task-processor-incremental-results?*))
   **/
  protected String createInferenceContinuationCommand(InferenceParameters queryProperties) {
    if (queryProperties == null) {
      queryProperties = new DefaultInferenceParameters(getCycServer());
    }
    if ((answerProcessingFunction != null) && (!answerProcessingFunction.shouldQuote())) {
      answerProcessingFunction = new CycSymbol(answerProcessingFunction.getPackageName(),
              answerProcessingFunction.getSymbolName());
    }
    String processingFnStr = ((answerProcessingFunction != null) ? answerProcessingFunction.stringApiValue() : "nil");
    queryProperties.put(new CycSymbol(":CONTINUABLE?"), Boolean.TRUE);
    return "(cdr (list (open-cyc-continue-query " + problemStoreId + " " + inferenceId + " " + queryProperties.stringApiValue() + " nil " + processingFnStr + " t)))";
  }

  protected String createInferenceAbortionCommand() {
    return "(cdr (list (inference-abort (find-inference-by-ids "
            + problemStoreId + " " + inferenceId + "))))";
  }
  
  //// Private
  
  private static CycFormulaSentence makeCycLSentence(String query, CycAccess access) {
    try {
      return CycLParserUtil.parseCycLSentence(query, true, access);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  //// Internal Rep
  
  private volatile int problemStoreId;
  private volatile int inferenceId;
  private volatile InferenceStatus status = InferenceStatus.NOT_STARTED;
  private List answers = Collections.synchronizedList(new ArrayList());
  /** This holds the list of registered SubLWorkerListener listeners. */
  final private EventListenerList inferenceListeners = new EventListenerList();
  private static Class inferenceListenerClass = InferenceWorkerListener.class;
  private volatile InferenceWorkerSuspendReason suspendReason = null;
  protected CycSymbol answerProcessingFunction;
  static private Map DEFAULT_NL_GENERATION_PROPERTIES = Collections.emptyMap();

  //// Main
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    try {
      CycAccess access = new CycAccess("localhost", 3600);
      InferenceWorker worker = new DefaultInferenceWorker("(#$isa ?X #$Dog)",
              CycAccess.inferencePSC, null, access, 10000);
      worker.addInferenceListener(new InferenceWorkerListener() {

        public void notifyInferenceCreated(InferenceWorker inferenceWorker) {
          System.out.println("GOT CREATED EVENT\n" + inferenceWorker);
        }

        public void notifyInferenceStatusChanged(InferenceStatus oldStatus, InferenceStatus newStatus,
                InferenceWorkerSuspendReason suspendReason, InferenceWorker inferenceWorker) {
          System.out.println("GOT STATUS CHANGED EVENT\n" + inferenceWorker);
        }

        public void notifyInferenceAnswersAvailable(InferenceWorker inferenceWorker, List newAnswers) {
          System.out.println("GOT NEW ANSWERS EVENT\n" + inferenceWorker);
        }

        public void notifyInferenceTerminated(InferenceWorker inferenceWorker, Exception e) {
          System.out.println("GOT TERMINATED EVENT\n" + inferenceWorker);
          if (e != null) {
            e.printStackTrace();
          }
          System.exit(0);
        }
      });
      worker.start();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
