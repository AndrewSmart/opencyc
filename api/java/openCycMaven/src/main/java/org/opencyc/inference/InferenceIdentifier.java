/*
 * An object that identifies an inference object in a Cyc image.
 */
package org.opencyc.inference;

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import static org.opencyc.api.SubLAPIHelper.makeNestedSubLStmt;
import static org.opencyc.api.SubLAPIHelper.makeSubLStmt;

/**
 *
 * @author baxter
 */
public class InferenceIdentifier {

  private int problemStoreID;

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final InferenceIdentifier other = (InferenceIdentifier) obj;
    if (this.problemStoreID != other.problemStoreID) {
      return false;
    }
    if (this.inferenceID != other.inferenceID) {
      return false;
    }
    if (this.cyc != other.cyc && (this.cyc == null || !this.cyc.equals(other.cyc))) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 71 * hash + this.problemStoreID;
    hash = 71 * hash + this.inferenceID;
    hash = 71 * hash + (this.cyc != null ? this.cyc.hashCode() : 0);
    return hash;
  }
  private int inferenceID;
  private CycAccess cyc;

  public CycAccess getCycAccess() {
    return cyc;
  }

  public int getInferenceID() {
    return inferenceID;
  }

  public int getProblemStoreID() {
    return problemStoreID;
  }

  public Integer getFirstProofId(Integer answerId) {
    Integer proofId = null;
    try {
      proofId = cyc.converseInt(makeSubLStmt("proof-suid", makeNestedSubLStmt("inference-answer-justification-first-proof",
              makeNestedSubLStmt("inference-answer-first-justification",
              makeNestedSubLStmt("find-inference-answer-by-ids", getProblemStoreID(), getInferenceID(), answerId)))));
    } catch (UnknownHostException ex) {
      logSevereException(ex);
    } catch (IOException ex) {
      logSevereException(ex);
    } catch (CycApiException ex) {
      logSevereException(ex);
    }
    return proofId;
  }

  private static void logSevereException(Exception ex) {
    Logger.getLogger(InferenceIdentifier.class.getName()).log(Level.SEVERE, null, ex);
  }

  @Override
  public String toString() {
    return "Inference " + inferenceID + " in Problem Store " + problemStoreID;
  }

  public String stringApiValue() {
    return "(find-inference-by-ids " + Integer.toString(problemStoreID) + " " + Integer.toString(inferenceID) + ")";
  }

  public InferenceIdentifier(int problemStoreID, int inferenceID, CycAccess cyc) {
    this.problemStoreID = problemStoreID;
    this.inferenceID = inferenceID;
    this.cyc = cyc;
  }

  public InferenceIdentifier(int problemStoreID, int inferenceID) {
    this(problemStoreID, inferenceID, null);
  }

  public void close() {
    try {
      getCycAccess().converseVoid("(destroy-inference-and-problem-store " + stringApiValue() + ")");
    } catch (UnknownHostException ex) {
      logSevereException(ex);
    } catch (IOException ex) {
      logSevereException(ex);
    } catch (CycApiException ex) {
      logSevereException(ex);
    }
  }

  public String toXML() {
    return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            + "<inferenceIdentifier>"
            + "<problemStore id=\"" + problemStoreID + "\"/>"
            + "<inference id=\"" + inferenceID + "\"/>"
            + "</inferenceIdentifier>";
  }
}
