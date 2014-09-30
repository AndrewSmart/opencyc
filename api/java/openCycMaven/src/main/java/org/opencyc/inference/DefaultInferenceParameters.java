/* $Id: DefaultInferenceParameters.java 140336 2012-06-08 19:55:09Z daves $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// External Imports
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.*;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycObjectFactory;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.DefaultCycObject;
import org.opencyc.inference.OpenCycInferenceParameterEnum.OpenCycInferenceMode;

/**
 * <P>DefaultInferenceParameters is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal
 * @date August 14, 2005, 2:46 PM
 * @version $Id: DefaultInferenceParameters.java 140336 2012-06-08 19:55:09Z daves $
 */
public class DefaultInferenceParameters extends SpecifiedInferenceParameters {

  //// Constructors
  /** Creates a new instance of DefaultInferenceParameters. */
  public DefaultInferenceParameters(CycAccess cycAccess) {
    this.cycAccess = cycAccess;
  }

  /** Creates a new instance of DefaultInferenceParameters. */
  public DefaultInferenceParameters(CycAccess cycAccess, boolean shouldReturnAnswersInHL) {
    this.cycAccess = cycAccess;
    if (shouldReturnAnswersInHL) {
      getAnswersInHL();
    } else {
      getAnswersInEL();
    }
  }

    /**
     * Creates a new instance of DefaultInferenceParameters.  
     * @param cycAccess the CycAccess object that these parameters are connected to.
     * @param params specified parameters to be added to the object.  This string should consist of a series of keywords
     * followed by the values for those keywords. The keywords can be found by looking for the #$sublIdentifier for the desired 
     * inference parameter in the Cyc KB.  For example, to limit a query to single-depth transformation and to allow at most 
     * 5 seconds per query, use the string ":max-transformation-depth 1 :max-time 5".
     */
    public DefaultInferenceParameters(CycAccess cycAccess, String params) {
        this.cycAccess = cycAccess;
        if (params == null && params.isEmpty()) {
            return;
        }
        CycList paramList = null;
        try {
            paramList = getParams(cycAccess, params);
        } catch (Exception ex) {
            throw new RuntimeException("Problem parsing '" + params + "' as inference parameter list.", ex);
        }
        Iterator<Object> paramIter = paramList.iterator();
        while (paramIter.hasNext()) {
            Object key = paramIter.next();
            Object value = paramIter.next();
            if (key instanceof CycSymbol) {
                put((CycSymbol)key, value);
            } else {
                throw new RuntimeException("'" + key + "' is not a valid inference parameter name.");
            }
        }
    }
    
    private static Map<String, CycList> paramCache = new HashMap<String, CycList>();

    private static CycList getParams(CycAccess cyc, String params) throws UnknownHostException, IOException {
        if (paramCache.containsKey(params)) {
            return paramCache.get(params);
        } else {
            CycList paramList = cyc.converseList("'(" + params + ")");
            paramCache.put(params, paramList);
            return paramList;
        }
    }

  
  public static DefaultInferenceParameters fromSpecifiedInferenceParameters(CycAccess cycAccess, SpecifiedInferenceParameters oldParameters) {
    return oldParameters.toDefaultInferenceParameters(cycAccess);
  }

  //// Public Area
  @Override
  public CycAccess getCycAccess() {
    return cycAccess;
  }

  public void getAnswersInHL() {
    put(ANSWER_LANGUAGE, HL);
  }

  public void getAnswersInEL() {
    put(ANSWER_LANGUAGE, EL);
  }

  @Override
  public void setMaxNumber(final Integer max) {
    put(MAX_NUMBER, max);
  }


  @Override
  public void setMaxTransformationDepth(Integer max) {
    put(MAX_TRANSFORMATION_DEPTH, max);
  }

  @Override
  public void setProblemStorePath(final String path) {
    final CycList problemStoreForm = CycList.makeCycList(LOAD_PROBLEM_STORE, path);
    put(PROBLEM_STORE, problemStoreForm);
  }



  private boolean getBoolean(CycSymbol paramName) {
    Object rawValue = get(paramName);
    if (rawValue instanceof Boolean) {
      return (Boolean) rawValue;
    } else {
      rawValue = getDefaultInferenceParameterDescriptions().getDefaultInferenceParameters().get(paramName);
      return (rawValue instanceof Boolean) ? (Boolean) rawValue : false;
    }
  }

  @Override
  public boolean getAbductionAllowed() {
    return getBoolean(ABDUCTION_ALLOWED);
  }


  @Override
  public void setInferenceMode(OpenCycInferenceMode mode) {
    put(INFERENCE_MODE, mode.getDescription());
  }

  public void setAllowIndeterminateResults(boolean b) {
    final CycSymbol value = b ? CycObjectFactory.t : CycObjectFactory.nil;
    put(ALLOW_INDETERMINATE_RESULTS, value);
  }

  public void setContinuable(boolean b) {
    final CycSymbol value = b ? CycObjectFactory.t : CycObjectFactory.nil;
    put(CONTINUABLE, value);
  }

  public void setConditionalSentence(boolean b) {
    final CycSymbol value = b ? CycObjectFactory.t : CycObjectFactory.nil;
    put(CONDITIONAL_SENTENCE, value);
  }

  public boolean getConditionalSentence() {
    return getBoolean(CONDITIONAL_SENTENCE);
  }

  public void setIntermediateStepValidationLevel(CycSymbol value) {
    put(INTERMEDIATE_STEP_VALIDATION_LEVEL, value);
  }

  public CycSymbol getIntermediateStepValidationLevel() {
    Object rawValue = get(INTERMEDIATE_STEP_VALIDATION_LEVEL);
    if (rawValue instanceof CycSymbol) {
      return (CycSymbol) rawValue;
    } else {
      rawValue = getDefaultInferenceParameterDescriptions().getDefaultInferenceParameters().get(INTERMEDIATE_STEP_VALIDATION_LEVEL);
      return (rawValue instanceof CycSymbol) ? (CycSymbol) rawValue : CycObjectFactory.nil;
    }
  }


  @Override
  public Object put(CycSymbol parameterName, Object value) {
    // @Hack if the value is :UNKNOWN the parameter will not be set and it is assumed that 
    // Cyc uses its own default for that particular parameter
    if (value instanceof CycSymbol && ((CycSymbol) value).toString().equals(":UNKNOWN")) {
      return null;
    }
    if (":PROBLEM-STORE".equals(parameterName.toString())) {
      if (value instanceof CycList || value instanceof CycSymbol || value instanceof Integer) {
        return map.put(parameterName, value);
      } else if (CycObjectFactory.nil.equals(value)) {
        return map.put(parameterName, value);
      } else {
        throw new RuntimeException("Got invalid value " + value + " (" + value.getClass().getSimpleName() + ")"
                + " for parameter " + parameterName);
      }
    }
    InferenceParameter param = getInferenceParameter(parameterName);
    value = param.canonicalizeValue(value);
    if (!param.isValidValue(value)) {
      throw new RuntimeException("Got invalid value " + value + " for parameter " + parameterName);
    }
    return map.put(parameterName, value);
  }

  @Override
  public String stringApiValue() {
    if (size() <= 0) {
      return CycObjectFactory.nil.stringApiValue();
    }
    StringBuilder buf = new StringBuilder("(LIST ");
    for (Iterator<CycSymbol> iter = keySet().iterator(); iter.hasNext();) {
      CycSymbol key = iter.next();
      buf.append(DefaultCycObject.stringApiValue(key));
      buf.append(" ");
      final Object val = get(key);
      buf.append(parameterValueStringApiValue(key, val));
      if (iter.hasNext()) {
        buf.append(" ");
      }
    }
    buf.append(")");
    return buf.toString();
  }

  @Override
  public Object cycListApiValue() {
    if (size() <= 0) {
      return CycList.EMPTY_CYC_LIST;
    }
    CycList<Object> cycList = new CycList<Object>();
    for (Iterator<CycSymbol> iter = keySet().iterator(); iter.hasNext();) {
      CycSymbol key = iter.next();
      cycList.add(key);
      final Object val = get(key);
      cycList.add(parameterValueCycListApiValue(key, val));
    }
    return cycList;
  }

  /* @return the CycList API value for val qua value for key. */
  @Override
  public Object parameterValueCycListApiValue(final CycSymbol key, final Object val) {
    final InferenceParameter param = getInferenceParameter(key);
    return param.parameterValueCycListApiValue(val);
  }

  @Override
  public Object clone() {
    DefaultInferenceParameters copy = new DefaultInferenceParameters(cycAccess);
    Iterator<CycSymbol> iterator = this.keySet().iterator();
    while (iterator.hasNext()) {
      CycSymbol key = iterator.next();
      Object value = this.get(key); // note: this might should be cloned
      copy.put(key, value);
    }
    return copy;
  }

  @Override
  public DefaultInferenceParameters toDefaultInferenceParameters(CycAccess cyc) {
    if (this.getCycAccess() == cyc) {
      return this;
    } else {
      DefaultInferenceParameters copy = new DefaultInferenceParameters(cycAccess);
      Iterator<CycSymbol> iterator = this.keySet().iterator();
      while (iterator.hasNext()) {
        CycSymbol key = iterator.next();
        Object value = this.get(key); // note: this might should be cloned
        copy.put(key, value);
      }
      return copy;
    }
  }


  public static Object getInfiniteValue() {
    return null;
  }

  public static boolean isInfiniteValue(final Object value) {
    return null == value;
  }

  //// Protected Area
  //// Private Area
  private int size() {
    return map.size();
  }

  private InferenceParameterDescriptions getDefaultInferenceParameterDescriptions() {
    if (defaultInferenceParameterDescriptions == null) {
      initializeDefaultInferenceParameterDescriptions();
    }
    return defaultInferenceParameterDescriptions;
  }

  private void initializeDefaultInferenceParameterDescriptions() {
    defaultInferenceParameterDescriptions =
            DefaultInferenceParameterDescriptions.getDefaultInferenceParameterDescriptions(cycAccess);
  }

  private InferenceParameter getInferenceParameter(CycSymbol parameterName) throws RuntimeException {
    InferenceParameterDescriptions descriptions = getDefaultInferenceParameterDescriptions();
    if (descriptions == null) {
      throw new RuntimeException("Cannot find inference parameter descriptions");
    }
    InferenceParameter param = (InferenceParameter) descriptions.get(parameterName);
    if (param == null) {
      throw new RuntimeException("No parameter found by name " + parameterName);
    }
    return param;
  }

  private static boolean isProblemStoreSpecification(final CycSymbol key, final Object val) {
    return (":PROBLEM-STORE".equals(key.toString())) && (val instanceof List);
  }

  /* @return the string API value for val qua value for key. */
  private String parameterValueStringApiValue(final CycSymbol key, final Object val) {
    final Object cycListApiValue = parameterValueCycListApiValue(key, val);
    if (isProblemStoreSpecification(key, cycListApiValue)) {
      return problemStoreStringApiValue((List) cycListApiValue);
    }
    if (cycListApiValue instanceof CycObject) {
      return ((CycObject) cycListApiValue).stringApiValue();
    } else {
      return (DefaultCycObject.stringApiValue(cycListApiValue));
    }
  }

  
  @Override
	public String toString() {
		final int maxLen = 10;
		StringBuilder builder = new StringBuilder();
		builder.append("DefaultInferenceParameters [");
		if (cycAccess != null)
			builder.append("cycAccess=").append(cycAccess).append(", ");
		if (defaultInferenceParameterDescriptions != null)
			builder.append("defaultInferenceParameterDescriptions=").append(
					toString(defaultInferenceParameterDescriptions.entrySet(), maxLen));
		builder.append("]");
		return builder.toString();
	}

	private String toString(Collection<?> collection, int maxLen) {
		StringBuilder builder = new StringBuilder();
		builder.append("[");
		int i = 0;
		for (Iterator<?> iterator = collection.iterator(); iterator.hasNext() && i < maxLen; i++) {
			if (i > 0)
				builder.append(", ");
			builder.append(iterator.next());
		}
		builder.append("]");
		return builder.toString();
	}

//@hack -- Only way to pass a problem store is to pass a form that evaluates to one:
	private static String problemStoreStringApiValue(final List val) {
    final StringBuffer buf = new StringBuffer("(");
    for (Iterator i = ((List) val).iterator(); i.hasNext();) {
      final Object item = i.next();
      if (item instanceof String) {
        buf.append(DefaultCycObject.stringApiValue(item));
      } else {
        buf.append(item);
      }
      buf.append(" ");
    }
    buf.append(")");
    return buf.toString();
  }
  //// Internal Rep
  private final CycAccess cycAccess;
  private InferenceParameterDescriptions defaultInferenceParameterDescriptions = null;
  //// Main

  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    try {
      System.out.println("Starting...");
      CycAccess cycAccess = new CycAccess("public1", 3600);
      InferenceParameters parameters = new DefaultInferenceParameters(cycAccess);
      parameters.put(new CycSymbol(":MAX-NUMBER"), new Integer(10));
      parameters.put(new CycSymbol(":PROBABLY-APPROXIMATELY-DONE"), new Double(0.5));
      parameters.put(new CycSymbol(":ABDUCTION-ALLOWED?"), Boolean.TRUE);
      parameters.put(new CycSymbol(":EQUALITY-REASONING-METHOD"), new CycSymbol(":CZER-EQUAL"));
      try {
        parameters.put(new CycSymbol(":MAX-NUMBER"), new CycSymbol(":BINDINGS"));
        System.out.println("Failed to catch exception.");
      } catch (Exception e) {
      } // ignore
      try {
        parameters.put(new CycSymbol(":PROBABLY-APPROXIMATELY-DONE"), new CycSymbol(":BINDINGS"));
        System.out.println("Failed to catch exception.");
      } catch (Exception e) {
      } // ignore
      try {
        parameters.put(new CycSymbol(":ABDUCTION-ALLOWED?"), new CycSymbol(":BINDINGS"));
        System.out.println("Failed to catch exception.");
      } catch (Exception e) {
      } // ignore
      try {
        parameters.put(new CycSymbol(":EQUALITY-REASONING-METHOD"), new Double(0.5));
        System.out.println("Failed to catch exception.");
      } catch (Exception e) {
      } // ignore
      System.out.println("PARAMETERS: " + parameters.stringApiValue());
    } catch (Exception e) {
      e.printStackTrace();
    } finally {
      System.out.println("Exiting...");
      System.exit(0);
    }
  }
}
