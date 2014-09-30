/*
 * CycQuery.java
 *
 * Created on August 11, 2004, 10:48 AM
 */

package org.opencyc.util.query;

import org.opencyc.inference.InferenceStatus;
/**
 * @version $Id: CycQuery.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  mreimers
 */
public interface CycQuery extends Query {
  
  public InferenceStatus getInferenceStatus();
}
