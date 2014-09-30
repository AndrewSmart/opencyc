/*
 * An identifier for an individual answer within a specific Cyc inference.
 */
package org.opencyc.inference;

import org.opencyc.cycobject.CycList;

/**
 *
 * @author baxter
 */
public class InferenceAnswerIdentifier {

  public InferenceAnswerIdentifier(InferenceIdentifier inferenceID, int answerID) {
    this.inferenceID = inferenceID;
    this.answerID = answerID;
  }

  @Override
  public String toString() {
    return "Answer " + answerID + " for " + inferenceID;
  }

  public String stringApiValue() {
    return cycListApiValue().stringApiValue();
  }

  public CycList<Integer> cycListApiValue() {
    return CycList.makeCycList(inferenceID.getProblemStoreID(), inferenceID.getInferenceID(), answerID);
  }

  public int getAnswerID() {
    return answerID;
  }

  public InferenceIdentifier getInferenceID() {
    return inferenceID;
  }

  public static boolean possiblyInferenceAnswerSignature(Object obj) {
    if (obj instanceof CycList) {
      final CycList cycList = (CycList) obj;
      if (cycList.size() == 3) {
        try {
          return (Integer.valueOf(cycList.get(0).toString()) >= 0
                  && Integer.valueOf(cycList.get(1).toString()) >= 0
                  && Integer.valueOf(cycList.get(2).toString()) >= 0);
        } catch (NumberFormatException e) {
          return false;
        }
      }
    }
    return false;
  }
  private InferenceIdentifier inferenceID;
  private int answerID;
}
