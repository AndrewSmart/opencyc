/* $Id: InferenceParameterDescriptions.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.inference;

//// External Imports
import java.util.Map;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.cycobject.CycSymbol;

/**
 * <P>InferenceParameterDescriptions is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author tbrussea
 * @date August 2, 2005, 10:21 AM
 * @version $Id: InferenceParameterDescriptions.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface InferenceParameterDescriptions extends Map<CycSymbol, InferenceParameter> {
  
  public String stringApiValue();
  public CycAccess getCycAccess();
  public InferenceParameters getDefaultInferenceParameters();

}
