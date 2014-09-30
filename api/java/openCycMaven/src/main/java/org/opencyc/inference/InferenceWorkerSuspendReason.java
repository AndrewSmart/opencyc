/* $Id: InferenceWorkerSuspendReason.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

//// Internal Imports
import java.util.HashMap;
import java.util.Map;
import static org.opencyc.api.CycObjectFactory.makeCycSymbol;
import org.opencyc.cycobject.CycSymbol;

//// External Imports
/**
 * <P>InferenceWorkerSuspendReason is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author bklimt
 * @date October 31, 2005, 10:29 AM
 * @version $Id: InferenceWorkerSuspendReason.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public final class InferenceWorkerSuspendReason {

  private static final Map<CycSymbol, InferenceWorkerSuspendReason> SYMBOL_MAP =
          new HashMap<CycSymbol, InferenceWorkerSuspendReason>();

  //// Constructors
  /** Creates a new instance of InferenceWorkerSuspendReason. */
  private InferenceWorkerSuspendReason(String description, final String statusString,
          final boolean isExceptional, final String cycSymbolName,
          final boolean isContinuable, final boolean allowStop,
          final boolean allowReset) {
    this.description = description;
    this.inferenceStatusString = addHTMLFontColor(statusString, isExceptional);
    this.isContinuable = isContinuable;
    this.allowStop = allowStop;
    this.allowReset = allowReset;
    SYMBOL_MAP.put(makeCycSymbol(cycSymbolName), this);
  }
  //// Public Area

  /** Create a new error reason. */
  public static InferenceWorkerSuspendReason createFromErrorString(final String errorString) {
    final InferenceWorkerSuspendReason reason = new InferenceWorkerSuspendReason("Error: " + errorString,
            "Error", true, ":ERROR", true, true, true);
    reason.setErrorFlag(true);
    return reason;
  }

  /** Was inference suspended because of an error? */
  public boolean isError() {
    return isError;
  }

  public static InferenceWorkerSuspendReason fromCycSymbol(final CycSymbol symbol) {
    final InferenceWorkerSuspendReason reason = SYMBOL_MAP.get(symbol);
    if (reason != null) {
      return reason;
    } else if (symbol == null) {
      return UNKNOWN;
    } else {
      throw new IllegalArgumentException("Unable to find InferenceWorkerSuspendReason from " + symbol);
    }
  }

  @Override
  public String toString() {
    return description;
  }

  /** A string to describe the status of the inference. */
  public String getInferenceStatusString() {
    return inferenceStatusString;
  }

  /** Can an inference suspended for this reason be reset? */
  public boolean allowReset() {
    return allowReset;
  }

  /** Can an inference suspended for this reason be stopped? */
  public boolean allowStop() {
    return allowStop;
  }

  /** Can an inference suspended for this reason be continued? */
  public boolean isContinuable() {
    return isContinuable;
  }
  //// Suspend reasons referenced from other packages: 
  public static final InferenceWorkerSuspendReason MAX_TIME =
          new InferenceWorkerSuspendReason("Max time reached", "Timed out", false,
          ":MAX-TIME", true, false, true);
  public static final InferenceWorkerSuspendReason MAX_NUMBER =
          new InferenceWorkerSuspendReason("Max results reached", "Result limit",
          false, ":MAX-NUMBER", true, false, true);
  public static final InferenceWorkerSuspendReason MAX_STEPS =
          new InferenceWorkerSuspendReason("Max steps performed", "Inference Step Limit",
          false, ":MAX-STEP", false, false, false);
  public static final InferenceWorkerSuspendReason EXHAUST =
          new InferenceWorkerSuspendReason("Exhausted", "Finished", false, ":EXHAUST",
          false, false, false);

  static {
    // This one has two corresponding Cyc symbols:
    SYMBOL_MAP.put(makeCycSymbol(":EXHAUST-TOTAL"), EXHAUST);
  }
  public static final InferenceWorkerSuspendReason ABORTED =
          new InferenceWorkerSuspendReason("Aborted", "Aborted", true, ":ABORT",
          false, false, false);
  public static final InferenceWorkerSuspendReason INTERRUPT =
          new InferenceWorkerSuspendReason("Interrupted", "Paused", true, ":INTERRUPT",
          true, true, true);
  //// Protected Area
  //// Private Area

  private void setErrorFlag(boolean b) {
    isError = b;
  }

  private static String addHTMLFontColor(final String text, final boolean isExceptional) {
    return "<html><body><font color=" + (isExceptional ? "red" : "green") + ">" + text + "</font></body></html>";
  }
  //// Assorted suspend reasons not directly referenced in java code:

  static {
    new InferenceWorkerSuspendReason("Max depth reached", "Depth limit", false,
            ":LOOK-NO-DEEPER-FOR-ADDITIONAL-ANSWERS", true, true, false);
    new InferenceWorkerSuspendReason("Max problem count reached", "Problem limit",
            false, ":MAX-PROBLEM-COUNT", true, true, false);
    new InferenceWorkerSuspendReason("Max proof count reached", "Proof limit",
            false, ":MAX-PROOF-COUNT", true, true, false);

    new InferenceWorkerSuspendReason("Probably approximately done", "P.A.D.",
            false, ":PROBABLY-APPROXIMATELY-DONE", true, true, false);

  }
  static CycSymbol ERROR_SYMBOL = makeCycSymbol(":ERROR");
  private static final InferenceWorkerSuspendReason UNKNOWN =
          new InferenceWorkerSuspendReason("Unknown", "Unknown", true, "NIL", true, true, false);
  //// Internal Rep
  private final String description;
  private boolean isError = false; // true iff inference suspended because of an error
  private final boolean isContinuable;
  private final boolean allowStop;
  private final boolean allowReset;
  private final String inferenceStatusString;
  //// Main
}
