/* $Id: InferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// Internal Imports
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycSymbol;


//// External Imports
/**
 * <P>InferenceParameter is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author tbrussea
 * @date August 2, 2005, 10:25 AM
 * @version $Id: InferenceParameter.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface InferenceParameter {

  /* @return the CycList API value for val qua value for this parameter. */
  Object parameterValueCycListApiValue(Object val);

  Object canonicalizeValue(Object value);

  CycFort getId();

  CycSymbol getKeyword();

  String getShortDescription();

  String getLongDescription();

  String getPrettyRepresentation(Object value);

  InferenceParameterValueDescription getAlternateValue();

  boolean isAlternateValue(Object value);

  boolean isValidValue(Object potentialValue);

  boolean isBasicParameter();

  boolean isQueryStaticParameter();

  Object getDefaultValue();
}
