/* $Id: DefaultBooleanInferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;


//// External Imports
import java.util.Map;

//// Internal Imports
import org.opencyc.api.CycObjectFactory;
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
 * @version $Id: DefaultBooleanInferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class DefaultBooleanInferenceParameter extends AbstractInferenceParameter implements BooleanInferenceParameter {

  //// Constructors
  /** Creates a new instance of DefaultBooleanInferenceParameter. */
  public DefaultBooleanInferenceParameter(Map propertyMap) {
    super(propertyMap);
  }

  protected DefaultBooleanInferenceParameter(Boolean defaultValue, CycSymbol keyword,
          CycFort id, String shortDescription, String longDescription,
          CycSymbol isBasicParameter, CycSymbol isQueryStaticParameter, CycList alternateValue) {
    super(defaultValue, keyword, id, shortDescription, longDescription, isBasicParameter,
            isQueryStaticParameter, alternateValue);
  }

  //// Public Area
  public boolean isValidValue(Object potentialValue) {
    if (isAlternateValue(potentialValue)) {
      return true;
    } else if (potentialValue instanceof Boolean) {
      return true;
    } else {
      return false;
    }
  }

  @Override
  public Object canonicalizeValue(final Object value) {
    if (isValidValue(value)) {
      return value;
    } else if (value == null) {
      throw new RuntimeException("Got invalid boolean value " + value);
    } else if (value.toString().equals(CycObjectFactory.nil.toString())) {
      return false;
    } else if (value.toString().equals(CycObjectFactory.t.toString())) {
      return true;
    } else {
      throw new RuntimeException("Got invalid boolean value " + value);
    }
  }

  @Override
  public String getPrettyRepresentation(Object value) {
    if (value instanceof Boolean) {
      return (Boolean) value ? "Yes" : "No";
    } else {
      return super.getPrettyRepresentation(value);
    }
  }


  public Object parameterValueCycListApiValue(Object val) {
    if (((Boolean) val).booleanValue()) {
      return (CycObjectFactory.t);
    } else {
      return (CycObjectFactory.nil);
    }
  }

//// Protected Area
//// Private Area
//// Internal Rep
//// Main
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
  }
}
