/* $Id: DefaultEnumerationInferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// External Imports
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

//// Internal Imports
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycSymbol;

/**
 * <P>DefaultBooleanInferenceParameter is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal
 * @date August 9, 2005, 9:09 PM
 * @version $Id: DefaultEnumerationInferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultEnumerationInferenceParameter extends AbstractInferenceParameter implements EnumerationInferenceParameter {

  //// Constructors
  /** Creates a new instance of DefaultBooleanInferenceParameter. */
  public DefaultEnumerationInferenceParameter(Map propertyMap) {
    super(propertyMap);
    for (int i = 0, size = REQUIRED_SYMBOLS.length; i < size; i++) {
      if (propertyMap.get(REQUIRED_SYMBOLS[i]) == null) {
        throw new RuntimeException("Expected key not found in map " +
                REQUIRED_SYMBOLS[i] +
                " for inference parameter " + propertyMap.get(AbstractInferenceParameter.ID_SYMBOL));
      }
    }
    Object potentialValuesObj = DefaultInferenceParameterValueDescription.verifyObjectType(propertyMap, POTENTIAL_VALUES_SYMBOL, List.class);
    init((List) potentialValuesObj);
  }

  DefaultEnumerationInferenceParameter(Object defaultValue, CycSymbol keyword,
          CycFort id, String shortDescription, String longDescription,
          CycSymbol isBasicParameter, CycSymbol isQueryStaticParameter, CycList alternateValue,
          List<InferenceParameterValueDescription> potentialValues) {
    super(defaultValue, keyword, id, shortDescription, longDescription, isBasicParameter,
            isQueryStaticParameter, alternateValue);
    this.potentialValues.addAll(potentialValues);
    this.potentialValues = Collections.unmodifiableList(this.potentialValues);
  }

  //// Public Area
  public boolean isValidValue(Object potentialValue) {
    if (isAlternateValue(potentialValue)) {
      return true;
    }
    if (potentialValues.contains(potentialValue)) {
      return true;
    }
    return false;
  }

  public List<InferenceParameterValueDescription> getPotentialValues() {
    return potentialValues;
  }

  @Override
  public String getPrettyRepresentation(Object value) {
    Iterator<InferenceParameterValueDescription> iterator =
            getPotentialValues().iterator();
    while (iterator.hasNext()) {
      InferenceParameterValueDescription description = iterator.next();
      if (description.getValue().equals(value)) {
        return description.getShortDescription();
      }
    }
    return super.getPrettyRepresentation(value);
  }

  @Override
  public String toString() {
    String str = super.toString() + " values={";
    Iterator iterator = getPotentialValues().iterator();
    while (iterator.hasNext()) {
      Object value = iterator.next();
      str += value.toString();
      if (iterator.hasNext()) {
        str += ",";
      } else {
        str += "}";
      }
    }
    return str;
  }

 
  public Object parameterValueCycListApiValue(final Object val) {
    return val;
  }

  //// Protected Area
  //// Private Area
  private void init(List potentialValues) {
    if (potentialValues == null) {
      throw new IllegalArgumentException("Got null potentialValues");
    }
    for (Iterator iter = potentialValues.iterator(); iter.hasNext();) {
      Object potentialValueObj = iter.next();
      if (!(potentialValueObj instanceof CycList)) {
        throw new RuntimeException("Expected a CycList; got " + potentialValueObj);
      }
      InferenceParameterValueDescription potentialValue =
              new DefaultInferenceParameterValueDescription(DefaultInferenceParameterDescriptions.parsePropertyList((CycList) potentialValueObj));
      this.potentialValues.add(potentialValue);
    }
    this.potentialValues = Collections.unmodifiableList(this.potentialValues);
  }
  //// Internal Rep
  private List<InferenceParameterValueDescription> potentialValues =
          new ArrayList<InferenceParameterValueDescription>() {

            @Override
            public boolean contains(Object obj) {
              for (Iterator iter = iterator(); iter.hasNext();) {
                if (iter.next().equals(obj)) {
                  return true;
                }
              }
              return false;
            }
          };
  private final static CycSymbol POTENTIAL_VALUES_SYMBOL = new CycSymbol(":POTENTIAL-VALUES");
  private final static CycSymbol[] REQUIRED_SYMBOLS = {POTENTIAL_VALUES_SYMBOL};

  //// Main
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
  }
}
