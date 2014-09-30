/* $Id: KBQueryFactory.java 136863 2011-11-13 16:57:24Z baxter $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.api.CycObjectFactory;
import org.opencyc.api.SubLAPIHelper;
import org.opencyc.cycobject.CycDenotationalTerm;
import org.opencyc.cycobject.CycFormulaSentence;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.CycVariable;
import org.opencyc.cycobject.ELMt;

/**
 *
 * KBQueryFactory provides factory methods for inference workers where
 * the query information--the sentence, the ELMt and the inference parameters--
 * are backed by a KBQ term in the CYC knowledge base.
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author rck
 * @date 2010-07-08
 * @version $Id: KBQueryFactory.java 136863 2011-11-13 16:57:24Z baxter $
 */
public class KBQueryFactory {

  static private final KBQueryFactory INSTANCE = new KBQueryFactory();

  static public KBQueryFactory getInstance() {
    return INSTANCE;
  }
  //// SubL API entry points
  final static public CycSymbol KBQ_SENTENCE = CycObjectFactory.makeCycSymbol("KBQ-SENTENCE");
  final static public CycSymbol KBQ_ELMT = CycObjectFactory.makeCycSymbol("KBQ-MT");
  final static public CycSymbol KBQ_PROPERTIES = CycObjectFactory.makeCycSymbol("KBQ-QUERY-PROPERTIES");

  /**
   *
   * Create a new instance of InferenceWorker, either synchronous or asynchronous,
   * that will run the query in the ELMt and with the parameters specified by the
   * CYC query in the Cyc KB and uniquely denoted by the KBQ parameter.
   *
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  public InferenceWorker getInferenceWorker(CycAccess access, CycDenotationalTerm kbq,
          long timeoutMsecs, boolean sync) throws CycApiException, IOException {
    return getInferenceWorkerWithSubstitutions(access, kbq, NO_SUBSTITUTIONS, timeoutMsecs, sync);
  }

  /**
   *
   * Create a new instance of InferenceWorker, either synchronous or asynchronous,
   * that will run the query in the ELMt and with the parameters specified by the
   * CYC query in the Cyc KB and uniquely denoted by the KBQ parameter.
   *
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  static public InferenceWorker prepareKBQuery(CycAccess access, CycDenotationalTerm kbq, long timeoutMsecs, boolean sync) throws
          IOException, CycApiException {
    return prepareKBQueryTemplate(access, kbq, NO_SUBSTITUTIONS, timeoutMsecs, sync);
  }
  final static private Map<CycVariable, Object> NO_SUBSTITUTIONS = Collections.emptyMap();

  /**
   * Create a new instance of InferenceWorker, either synchronous or asynchronous,
   * that will run the query in the ELMt and with the parameters specified by the
   * CYC query in the Cyc KB and uniquely denoted by the KBQ parameter.
   * After the query sentence has been loaded, apply the substitutions spelled out
   * in the substitution map, equating variables with bindings.
   *
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param substitutions a mapping from CycVariable to Objects; each mapping is converted
   *  into an equalSymbols clause and conjoined to the query sentence
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  public InferenceWorker getInferenceWorkerWithSubstitutions(CycAccess access,
          CycDenotationalTerm kbq, Map<CycVariable, Object> substitutions, long timeoutMsecs,
          boolean sync) throws CycApiException, IOException {
    final CycFormulaSentence sentence = loadKBQSentence(access, kbq);
    final ELMt elmt = loadKBQELMt(access, kbq);
    final InferenceParameters properties = loadKBQProperties(access, kbq);
    if (substitutions != null) {
      sentence.applySubstitutionsDestructive(substitutions);
    }
    return (sync) ? new DefaultInferenceWorkerSynch(sentence, elmt, properties, access, timeoutMsecs)
            : new DefaultInferenceWorker(sentence, elmt, properties, access, timeoutMsecs);
  }

  /**
   * Create a new instance of InferenceWorker, either synchronous or asynchronous,
   * that will run the query in the ELMt and with the parameters specified by the
   * CYC query in the Cyc KB and uniquely denoted by the KBQ parameter.
   * After the query sentence has been loaded, apply the substitutions spelled out
   * in the substitution map, equating variables with bindings.
   *
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param substitutions a mapping from CycVariable to Objects; each mapping is converted
   *  into an equalSymbols clause and conjoined to the query sentence
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  static public InferenceWorker prepareKBQueryTemplate(CycAccess access, CycDenotationalTerm kbq,
          Map<CycVariable, Object> substitutions, long timeoutMsecs, boolean sync)
          throws IOException, CycApiException {
    return getInstance().getInferenceWorkerWithSubstitutions(access, kbq, substitutions, timeoutMsecs, sync);
  }

  /**
   * Similar to <code>getInferenceWorkerWithSubstitutions</code>, but performs a tree substitution on the query sentence.
   * 
   * @see KBQueryFactory#prepareKBQueryTemplate(org.opencyc.api.CycAccess, org.opencyc.cycobject.CycDenotationalTerm, java.util.Map, long, boolean) 
   * 
   * 
   * 
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param substitutions a mapping from CycObject to Objects
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  public InferenceWorker getInferenceWorkerWithTreeSubstitutions(CycAccess access,
          CycDenotationalTerm kbq, Map<CycObject, Object> substitutions, long timeoutMsecs,
          boolean sync) throws CycApiException, IOException {
    final CycFormulaSentence sentence = loadKBQSentence(access, kbq);
    final ELMt elmt = loadKBQELMt(access, kbq);
    final InferenceParameters properties = loadKBQProperties(access, kbq);
    CycFormulaSentence subsSentence = sentence.treeSubstitute(access, substitutions);
    return (sync) ? new DefaultInferenceWorkerSynch(subsSentence, elmt, properties, access, timeoutMsecs)
            : new DefaultInferenceWorker(subsSentence, elmt, properties, access, timeoutMsecs);
  }

  /**
   * Similar to <code>prepareKBQueryTemplate</code>, but performs a tree substitution on the query sentence.
   * 
   * @see KBQueryFactory#prepareKBQueryTemplate(org.opencyc.api.CycAccess, org.opencyc.cycobject.CycDenotationalTerm, java.util.Map, long, boolean) 
   * 
   * 
   * 
   * @param access the CycAccess object that specifies the CYC server that hosts the
   *  reified query and that will perform the inference (i.e. to which the inference worker
   *  is bound)
   * @param kbq the Cyc term that specifies the query
   * @param substitutions a mapping from CycObject to Objects
   * @param timeoutMsecs the timeout parameter for the inference worker in milli-seconds
   * @param sync if <tt>true</tt>, returns an instance of DefaultInferenceWorkerSynch; otherwise
   *  an instance of DefaultInferenceWorker
   * @return an instance of DefaultInferenceWorker, either synchronized or not, depending
   *  on the value of <tt>sync</tt>
   * @throws IOException if the converse operation throws an IOException
   * @throws CycApiException if the converse operation throws a CycApiException
   */
  static public InferenceWorker prepareKBQueryTreeTemplate(CycAccess access, CycDenotationalTerm kbq,
          Map<CycObject, Object> substitutions, long timeoutMsecs, boolean sync)
          throws IOException, CycApiException {
    return getInstance().getInferenceWorkerWithTreeSubstitutions(access, kbq, substitutions, timeoutMsecs, sync);
  }

  protected CycFormulaSentence loadKBQSentence(CycAccess access, CycDenotationalTerm kbq)
          throws CycApiException, IOException {
    try {
      final String command = SubLAPIHelper.makeSubLStmt(KBQ_SENTENCE, kbq);
      return access.converseSentence(command);
    } catch (CycApiException xcpt) {
      throw new CycApiException("Could not load query sentence for KBQ " + kbq.cyclify(), xcpt);
    }
  }

  protected ELMt loadKBQELMt(CycAccess access, CycDenotationalTerm kbq)
          throws CycApiException, IOException {
    try {
      final String command = SubLAPIHelper.makeSubLStmt(KBQ_ELMT, kbq);
      return access.makeELMt(access.converseCycObject(command));
    } catch (CycApiException xcpt) {
      throw new CycApiException("Could not load query MT for KBQ " + kbq.cyclify(), xcpt);
    }
  }

  protected InferenceParameters loadKBQProperties(CycAccess access, CycDenotationalTerm kbq)
          throws CycApiException, IOException {
    try {
      InferenceParameters properties = new DefaultInferenceParameters(access);
      final String command = SubLAPIHelper.makeSubLStmt(KBQ_PROPERTIES, kbq);
      properties.updateFromPlist(access.converseList(command));
      return properties;
    } catch (CycApiException xcpt) {
      throw new CycApiException("Could not load query inference properties for KBQ " + kbq.cyclify(), xcpt);
    }
  }
}
