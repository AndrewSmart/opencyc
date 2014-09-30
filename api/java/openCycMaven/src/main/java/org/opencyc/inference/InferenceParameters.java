/* $Id: InferenceParameters.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// Internal Imports
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

//// External Imports

//// OpenCyc Imports
import org.opencyc.api.CycObjectFactory;
import org.opencyc.api.CycAccess;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.inference.OpenCycInferenceParameterEnum.OpenCycInferenceMode;

/**
 * <P>InferenceParameters is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal
 * @date August 14, 2005, 2:41 PM
 * @version $Id: InferenceParameters.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface InferenceParameters extends Cloneable {

  public final static CycSymbol MAX_NUMBER = CycObjectFactory.makeCycSymbol(":MAX-NUMBER");
  public static final CycSymbol ALLOW_INDETERMINATE_RESULTS = CycObjectFactory.makeCycSymbol(":allow-indeterminate-results?");
  public static final CycSymbol ANSWER_LANGUAGE = CycObjectFactory.makeCycSymbol(":ANSWER-LANGUAGE");
  public static final CycSymbol CONDITIONAL_SENTENCE = CycObjectFactory.makeCycSymbol(":CONDITIONAL-SENTENCE?");
  public static final CycSymbol CONTINUABLE = CycObjectFactory.makeCycSymbol(":continuable?");
  public static final CycSymbol EL = CycObjectFactory.makeCycSymbol(":EL");
  public static final CycSymbol HL = CycObjectFactory.makeCycSymbol(":HL");
  public static final CycSymbol INTERMEDIATE_STEP_VALIDATION_LEVEL = CycObjectFactory.makeCycSymbol(":INTERMEDIATE-STEP-VALIDATION-LEVEL");
  public static final CycSymbol INFERENCE_MODE = CycObjectFactory.makeCycSymbol(":inference-mode");

  void clear();

  Object parameterValueCycListApiValue(final CycSymbol key, final Object val);

  Set<Entry<CycSymbol, Object>> entrySet();

  Integer getMaxNumber();
  final static CycSymbol MAX_TIME = CycObjectFactory.makeCycSymbol(":MAX-TIME");

  Integer getMaxTime();

  Set<CycSymbol> keySet();

  void putAll(InferenceParameters properties);

  void remove(CycSymbol property);

  Object put(CycSymbol parameterName, Object value);

  Object get(CycSymbol parameterName);

  boolean containsKey(CycSymbol key);

  void setMaxNumber(Integer max);

  void setMaxTime(Integer max);

  void setInferenceMode(OpenCycInferenceMode mode);

  String stringApiValue();

  Object cycListApiValue();

  Object clone();

  /* return a version of this that is a DefaultInferenceParameters.  May return the same object that was sent in.
   * 
   */
  public DefaultInferenceParameters toDefaultInferenceParameters (CycAccess cyc);

  /** Update from a plist of the type used by Cyc's inference engine.
   * @param plist
   */
  public void updateFromPlist(List plist);

  public boolean getAbductionAllowed();
  final static CycSymbol ABDUCTION_ALLOWED = CycObjectFactory.makeCycSymbol(":ABDUCTION-ALLOWED?");
  final static CycSymbol MAX_TRANSFORMATION_DEPTH = CycObjectFactory.makeCycSymbol(":MAX-TRANSFORMATION-DEPTH");

  void setMaxTransformationDepth(Integer i);

  Integer getMaxTransformationDepth();
  public static final CycSymbol LOAD_PROBLEM_STORE = CycObjectFactory.makeCycSymbol("LOAD-PROBLEM-STORE");
  final static CycSymbol PROBLEM_STORE = CycObjectFactory.makeCycSymbol(":PROBLEM-STORE");

  public void setProblemStorePath(String path);

  public boolean usesLoadedProblemStore();

  /**
   * Adjust these parameters to give the inference engine at least as extensive
   * resources as newParams.
   * @param newParams
   */
  public void makeAtLeastAsLooseAs(InferenceParameters newParams);
}
