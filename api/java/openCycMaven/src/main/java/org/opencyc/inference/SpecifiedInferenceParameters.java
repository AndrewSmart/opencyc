package org.opencyc.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycObjectFactory;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.inference.OpenCycInferenceParameterEnum.OpenCycInferenceMode;

/**
 * <P>SpecifiedInferenceParameters is designed to be used when you want to carry
 * around inference parameters, but do not have access to a CycAccess instance.  It will not
 * perform value canonicalization or other useful checks on the names or values
 * of the inference parameters.  When the time comes to actually run the query, this
 * can be converted into a DefaultInferenceParameters object by providing a CycAccess and
 * calling the toDefaultInferenceParameters method.
 *
 * <P>Copyright (c) 2011 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author daves
 * @date March 15, 2011
 * @version $Id: SpecifiedInferenceParameters.java 136308 2011-10-14 09:54:09Z bbouldin $
 */

public class SpecifiedInferenceParameters implements InferenceParameters {


  public Object clone() {
    SpecifiedInferenceParameters copy = new SpecifiedInferenceParameters();
    Iterator<CycSymbol> iterator = this.keySet().iterator();
    while (iterator.hasNext()) {
      CycSymbol key = iterator.next();
      Object value = this.get(key); // note: this might should be cloned
      copy.put(key, value);
    }
    return copy;
  }

  public DefaultInferenceParameters toDefaultInferenceParameters (CycAccess cyc) {
    DefaultInferenceParameters copy = new DefaultInferenceParameters(cyc);
    Iterator<CycSymbol> iterator = this.keySet().iterator();
    while (iterator.hasNext()) {
      CycSymbol key = iterator.next();
      Object value = this.get(key); // note: this might should be cloned
      copy.put(key, value);
    }
    return copy;
  }

  public void clear() {
    map.clear();
  }

  public Object parameterValueCycListApiValue(CycSymbol key, Object val) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public Set<Entry<CycSymbol, Object>> entrySet() {
    return Collections.unmodifiableSet(map.entrySet());
  }

  public Integer getMaxNumber() {
    final Object rawValue = get(MAX_NUMBER);
    if (rawValue instanceof Integer) {
      return (Integer) rawValue;
    } else {
      return null;
    }
  }


  @Override
  public Integer getMaxTime() {
    final Object rawValue = get(MAX_TIME);
    if (rawValue instanceof Integer) {
      return (Integer) rawValue;
    } else {
      return null;
    }
  }

  public Set<CycSymbol> keySet() {
    return Collections.unmodifiableSet(map.keySet());
  }

  public Object get(CycSymbol parameterName) {
    return map.get(parameterName);
  }

  public void putAll(InferenceParameters properties) {
    for (final CycSymbol key : properties.keySet()) {
      put(key, properties.get(key));
    }
  }

  public void remove(CycSymbol property) {
    map.remove(property);
  }

  public Object put(CycSymbol parameterName, Object value) {
    return map.put(parameterName, value);
  }

  public boolean containsKey(CycSymbol key) {
    return map.containsKey(key);
  }

  public void setMaxNumber(Integer max) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public void setMaxTime(Integer max) {
    put(MAX_TIME, max);
  }

  public void setInferenceMode(OpenCycInferenceMode mode) {
    put(CycObjectFactory.makeCycSymbol(":inference-mode"), mode.getDescription().getValue());
  }

  public String stringApiValue() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public Object cycListApiValue() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public CycAccess getCycAccess() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public void updateFromPlist(final List plist) {
    for (int i = 0; i < plist.size(); i++) {
      final CycSymbol paramKey = (CycSymbol) plist.get(i++);
      final Object paramValue = plist.get(i);
      put(paramKey, paramValue);
    }
  }
  public boolean getAbductionAllowed() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public void setMaxTransformationDepth(Integer i) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  public Integer getMaxTransformationDepth() {
    final Object rawValue = get(MAX_TRANSFORMATION_DEPTH);
    if (rawValue instanceof Integer) {
      return (Integer) rawValue;
    } else {
      return null;
    }
  }

  public void setProblemStorePath(String path) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean usesLoadedProblemStore() {
    final Object value = get(PROBLEM_STORE);
    return (value instanceof CycList
            && LOAD_PROBLEM_STORE.equals(((CycList) value).first()));
  }

    @Override
  public void makeAtLeastAsLooseAs(final InferenceParameters newParams) {
    if (newParams.getMaxTransformationDepth() == null || newParams.getMaxTransformationDepth() > this.getMaxTransformationDepth()) {
      setMaxTransformationDepth(newParams.getMaxTransformationDepth());
    }
    if (newParams.getMaxTime() == null || newParams.getMaxTime() > this.getMaxTime()) {
      setMaxTime(newParams.getMaxTime());
    }
    if (newParams.getMaxNumber() == null || newParams.getMaxNumber() > this.getMaxNumber()) {
      setMaxNumber(newParams.getMaxNumber());
    }
    //@TODO -- Add more as needed.
  }

  public void setBrowsable(boolean b) {
    final CycSymbol value = b ? CycObjectFactory.t : CycObjectFactory.nil;
    put(CycObjectFactory.makeCycSymbol(":browsable?"), value);
  }

  @Override
  public boolean equals(Object rhs) {
    // Can't guarantee that this works on subclasses right now, so use reflection
    //  to verify that we're not looking at subclasses.
    // TODO:  Generalize this to work on subclasses as well, or override .equals
    //  in those classes in some appropriate fashion
    return this.getClass().equals(SpecifiedInferenceParameters.class)
            && rhs.getClass().equals(SpecifiedInferenceParameters.class)
            && map.equals(((SpecifiedInferenceParameters) rhs).map);
  }

  @Override
  public int hashCode() {
    return map.hashCode();
  }

  final Map<CycSymbol, Object> map = new HashMap<CycSymbol, Object>();

	@Override
	public String toString() {
		final int maxLen = 10;
		StringBuilder builder = new StringBuilder();
		builder.append("SpecifiedInferenceParameters [");
		if (map != null)
			builder.append("map=").append(toString(map.entrySet(), maxLen));
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

  
  
}
