/* $Id: SubLInteractor.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.api;

//// Internal Imports
import java.io.IOException;
//// External Imports
import java.util.Arrays;
import java.util.List;
import org.opencyc.util.OpenCycTaskInterruptedException;
import org.opencyc.util.TimeOutException;

/** 
 * <P>SubLInteractor is designed to...
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Oct 8, 2008, 1:19:31 PM
 * Author     : baxter
 * @version $Id: SubLInteractor.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class SubLInteractor {

  private CycAccess cycAccess;
  private DefaultSubLWorkerSynch worker;
  private Exception ex;
  private int timeoutMsecs = 0;

  //// Constructors
  /** Creates a new instance of SubLInteractor. */
  SubLInteractor(CycAccess cycAccess) {
    this.cycAccess = cycAccess;
  }

  //// Protected Area
  CycAccess getCycAccess() {
    return cycAccess;
  }

  void cancelLastCommand() {
    try {
      worker.abort();
    } catch (Exception ex) {
    }
  }

  void quit() {
    cancelLastCommand();
  }

  void setTimeoutMsecs(int msecs) {
    timeoutMsecs = msecs;
  }

  /** @return List of the Objects return values from evaluating text. */
  List submitSubL(String text) throws Exception {
    ex = null;
    Object result = null;
    try {
      worker = new DefaultSubLWorkerSynch("(multiple-value-list " + text + ")", cycAccess, timeoutMsecs);
      result = worker.getWork();
    } catch (IOException iOException) {
      ex = iOException;
    } catch (TimeOutException timeOutException) {
      ex = timeOutException;
    } catch (CycApiException cycApiException) {
      ex = cycApiException;
    } catch (OpenCycTaskInterruptedException openCycTaskInterruptedException) {
      ex = openCycTaskInterruptedException;
    } catch (RuntimeException e) {
      ex = e;
    } catch (Exception e) {
      ex = e;
    } catch (Throwable t) {
      System.out.println(t);
    }
    if (ex == null) {
      if (result instanceof List) {
      return (List) result;
      } else {
        final Object[] results = {result};
        return Arrays.asList(results);
      }
    } else {
      throw ex;
    }
  }
}
