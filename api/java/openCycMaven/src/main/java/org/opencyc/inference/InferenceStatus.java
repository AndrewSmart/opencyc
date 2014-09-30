/* $Id: InferenceStatus.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.inference;

//// Internal Imports
import org.opencyc.cycobject.*;

//// External Imports
import java.util.*;

/**
 * <P>InferenceStatus is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal, tbrussea
 * @date July 27, 2005, 12:23 PM
 * @version $Id: InferenceStatus.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public final class InferenceStatus extends CycSymbol {

  private InferenceStatus(String name) { 
    super(name);
  }
  
  public static InferenceStatus findInferenceStatus(CycSymbol symbol) {
    return (InferenceStatus)inferenceStatuses.get(symbol);
  }
  
  // should probably try to gracefully find a way to get these from the KB. (*inference-statuses*)
  public final static InferenceStatus NOT_STARTED = new InferenceStatus(":NOT-STARTED");
  public final static InferenceStatus STARTED = new InferenceStatus(":STARTED");
  public final static InferenceStatus NEW = new InferenceStatus(":NEW");
  public final static InferenceStatus PREPARED = new InferenceStatus(":PREPARED");
  public final static InferenceStatus READY = new InferenceStatus(":READY");
  public final static InferenceStatus RUNNING = new InferenceStatus(":RUNNING");
  public final static InferenceStatus SUSPENDED = new InferenceStatus(":SUSPENDED");
  public final static InferenceStatus DEAD = new InferenceStatus(":DEAD");
  public final static InferenceStatus TAUTOLOGY = new InferenceStatus(":TAUTOLOGY");
  public final static InferenceStatus CONTRADICTION = new InferenceStatus(":CONTRADICTION");
  public final static InferenceStatus ILL_FORMED = new InferenceStatus(":ILL-FORMED");
  public final static InferenceStatus FORMATTING = new InferenceStatus(":FORMATTING");
  
  private static HashMap inferenceStatuses = new HashMap();
  
  static {
    inferenceStatuses.put(NOT_STARTED, NOT_STARTED);
    inferenceStatuses.put(STARTED, STARTED);
    inferenceStatuses.put(NEW, NEW);
    inferenceStatuses.put(PREPARED, PREPARED);
    inferenceStatuses.put(READY, READY);
    inferenceStatuses.put(RUNNING, RUNNING);
    inferenceStatuses.put(SUSPENDED, SUSPENDED);
    inferenceStatuses.put(DEAD, DEAD);
    inferenceStatuses.put(TAUTOLOGY, TAUTOLOGY);
    inferenceStatuses.put(CONTRADICTION, CONTRADICTION);
    inferenceStatuses.put(ILL_FORMED, ILL_FORMED);
    inferenceStatuses.put(FORMATTING, FORMATTING);
  }
}
