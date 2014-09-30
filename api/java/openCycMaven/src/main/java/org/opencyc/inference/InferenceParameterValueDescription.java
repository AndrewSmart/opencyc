/* $Id: InferenceParameterValueDescription.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.inference;

//// Internal Imports
import org.opencyc.cycobject.*;

//// External Imports

/**
 * <P>InferenceParameterValueDescription is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal
 * @date August 14, 2005, 12:47 PM
 * @version $Id: InferenceParameterValueDescription.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface InferenceParameterValueDescription {

  public void setValue(Object infiniteValue);
  Object getValue();
  String getShortDescription();
  String getLongDescription();
}
