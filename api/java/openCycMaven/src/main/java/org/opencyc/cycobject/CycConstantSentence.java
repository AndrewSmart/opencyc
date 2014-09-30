/* $Id: CycConstantSentence.java 133723 2011-03-03 21:55:50Z mwitbrock $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.cycobject;

//// External Imports

/** 
 * <P>CycConstantSentence is designed to be an object that represents Sentences that
 * are single terms (the only known instance of this is #$False.
 *
 * <P>Copyright (c) 2011 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : March 3, 2011, 10:05:43 AM
 * Author     : daves
 * @version $Id: CycConstantSentence.java 133723 2011-03-03 21:55:50Z mwitbrock $
 *
 */
public class CycConstantSentence extends CycConstant implements CycSentence {

  /**
   * Create and return a new CycSentence whose arguments are terms.
   * CycList arguments will be converted to CycNauts or CycSentences.
   * @param terms
   */
  public CycConstantSentence(CycConstant constant) {
    super(constant.getName(), constant.getGuid());
    //@todo should this throw an exception if it's not #$False?
  }


  @Override
  public boolean isConditionalSentence() {
    return false;
  }

  public boolean isConjunction() {
    return false;
  }
 
  @Override
  public boolean isLogicalConnectorSentence() {
    return false;
  }

  @Override
  public boolean isExistential() {
    return false;
  }

  @Override
  public boolean isUniversal() {
    return false;
  }

}
