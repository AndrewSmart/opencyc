/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.opencyc.cycobject;

/**
 *
 * @author baxter
 */
public interface CycSentence extends CycObject {

  boolean isConditionalSentence();

  boolean isConjunction();

  boolean isLogicalConnectorSentence();

  boolean isExistential();

  boolean isUniversal();

}
