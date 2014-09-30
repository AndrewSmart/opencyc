/* $Id: SubLAPIHelper.java 133920 2011-03-25 18:25:34Z tbrussea $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.api;

//// Internal Imports
//// External Imports
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.DefaultCycObject;
import org.opencyc.cycobject.Guid;

/** 
 * <P>SubLAPIHelper is designed to...
 *
 * <P>Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Nov 4, 2010, 11:33:24 AM
 * Author     : tbrussea
 * @version $Id: SubLAPIHelper.java 133920 2011-03-25 18:25:34Z tbrussea $
 */
public class SubLAPIHelper {

  private static String getAPIString(Object param) {
    return (param instanceof AsIsTerm) ? ((AsIsTerm) param).toString() : DefaultCycObject.stringApiValue(param);
  }

  //// Constructors
  public SubLAPIHelper(String hostName, int port, CycObject user) throws UnknownHostException, IOException {
    this(new CycAccess(hostName, port), user);
    madeCycAccess = true;
  }

  public SubLAPIHelper(CycAccess access, CycObject user) {
    if (access == null) {
      throw new RuntimeException("Got invalid access: " + access);
    }
    this.access = access;
    this.user = user;
  }

  //// Public Area
  public synchronized void close() {
    if (madeCycAccess) {
      try {
        getCycAccess().close();
      } catch (Exception e) {
      } // ignore
    }
    setCycAccess(null);
  }

  public CycAccess getCycAccess() {
    return access;
  }

  private synchronized void setCycAccess(CycAccess access) {
    this.access = access;
    madeCycAccess = false;
  }

  private synchronized void recreateCycAccess() throws UnknownHostException, IOException {
    String hosName = getCycAccess().getHostName();
    int port = getCycAccess().getBasePort();
    setCycAccess(new CycAccess(hosName, port));
    madeCycAccess = true;
  }

  private CycObject getUser() {
    return user;
  }

  public String getHost() {
    return getCycAccess().getHostName();
  }

  public int getPort() {
    return getCycAccess().getBasePort();
  }
  static public final CycSymbol THE_CYCLIST = CycObjectFactory.makeCycSymbol("*the-cyclist*");

  public String wrapCommandWithUser(String command) {
    return wrapVariableBinding(command, THE_CYCLIST, getUser());
  }

  public static String wrapVariableBinding(String command, CycSymbol variable, Object value) {
    try {
      // @todo consider setting *ke-purpose*
      return "(clet ((" + DefaultCycObject.cyclifyWithEscapeChars(variable, true) + " " + getAPIString(value) + ")) " + command + ")";
    } catch (Exception e) {
      return command;
    }
  }

  public SubLWorker makeAsynchSubLWorker(String command) {
    return makeAsynchSubLWorker(command, getMaxTimeoutMSecs());
  }

  public SubLWorker makeAsynchSubLWorker(String command, long timeoutMsecs) {
    command = wrapCommandWithUser(command);
    SubLWorker worker = new DefaultSubLWorker(command, getCycAccess(), timeoutMsecs);
    return worker;
  }

  public Object executeCommandSynchronously(String command) throws IOException {
    return executeCommandSynchronously(command, getMaxTimeoutMSecs());
  }

  public Object executeCommandSynchronously(String command, long timeoutMsecs) throws IOException {
    return executeCommandSynchronouslyInt(command, timeoutMsecs, 0);
  }

  public Object executeCommandSynchronouslyInt(String command, long timeoutMsecs, int currentTry)
          throws IOException {
    command = wrapCommandWithUser(command);
    synchronized (System.out) { // @hack for tomcat in netbeans issue
      logger.log(Level.INFO, "About to execute command: {0}\nto server: {1}:{2}",
              new Object[]{command, getCycAccess().getHostName(), getCycAccess().getBasePort()});
    }
    Object result = null;
    try {
      final SubLWorkerSynch worker = new DefaultSubLWorkerSynch(command, getCycAccess(), timeoutMsecs);
      result = worker.getWork();
    } catch (CycApiServerSideException csse) {
      throw new CycApiServerSideException(csse.getMessage());
    } catch (CycApiClosedConnectionException expt) {
      if (currentTry < 2) {
        recreateCycAccess();
        result = executeCommandSynchronouslyInt(command, timeoutMsecs, currentTry + 1);
      } else {
        throw expt;
      }
    }
    synchronized (System.out) { // @hack for tomcat in netbeans issue
      logger.log(Level.FINE, "Got result: {0}", DefaultCycObject.cyclify(result));
    }
    return result;
  }

  public void setMaxTimeoutMSecs(long maxTimeoutMSecs) {
    this.maxTimeoutMSecs = maxTimeoutMSecs;
  }

  public long getMaxTimeoutMSecs() {
    return maxTimeoutMSecs;
  }

  /** @return an executable SubL statment string applying function to params. */
  public static String makeSubLStmt(CycSymbol function, Object... params) {
    return makeSubLStmt(function.getSymbolName(), params);
  }

  /** @return an executable SubL statment string applying function to params. */
  public static String makeSubLStmt(String functionName, Object... params) {
    StringBuilder buf = new StringBuilder(2048);
    buf.append("(").append(functionName);
    for (Object param : params) {
      buf.append(" ").append(getAPIString(param));
    }
    buf.append(")");
    return buf.toString();
  }

  /** @return a SubL statment applying function to params, suitable for using as an argument to {@link makeSubLStmt}. */
  public static AsIsTerm makeNestedSubLStmt(CycSymbol function, Object... params) {
    return makeNestedSubLStmt(function.getSymbolName(), params);
  }

  /** @return a SubL statment applying function to params, suitable for using as an argument to {@link makeSubLStmt}. */
  public static AsIsTerm makeNestedSubLStmt(String functionName, Object... params) {
    return new AsIsTerm(makeSubLStmt(functionName, params));
  }

  public static class AsIsTerm {

    public AsIsTerm(Object obj) {
      this.obj = obj;
    }

    @Override
    public String toString() {
      return "" + obj;
    }

    public Object getObj() {
      return obj;
    }
    protected Object obj;
  }

  public static final class QuotedAsIsTerm extends AsIsTerm {

    public QuotedAsIsTerm(Object obj) {
      super(obj);
    }

    @Override
    public String toString() {
      return "(quote " + obj + ")";
    }
  }

  public static final class QuotedTerm extends AsIsTerm {

    public QuotedTerm(Object obj) {
      super(obj);
    }

    @Override
    public String toString() {
      return "(quote " + DefaultCycObject.stringApiValue(obj) + ")";
    }
  }

  public static void setLoggingLevel(Level newLevel) {
    logger.setLevel(newLevel);
  }
  //// Protected Area
  //// Private Area
  //// Internal Rep
  private volatile CycAccess access;
  private final CycObject user;
  private volatile boolean madeCycAccess = false;
  private volatile long maxTimeoutMSecs = DEFAULT_MAX_TIMEOUT_MSECS;
  public static long DEFAULT_MAX_TIMEOUT_MSECS = 10000;
  private static final Logger logger = Logger.getLogger(SubLAPIHelper.class.toString());
  public static final Level DEFAULT_LOGGING_LEVEL = Level.WARNING;

  static {
    logger.addHandler(new ConsoleHandler());
    logger.setUseParentHandlers(false);
    setLoggingLevel(DEFAULT_LOGGING_LEVEL);
  }

  //// Main
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    logger.info("Starting");
    SubLAPIHelper thisObj = null;
    try {
      CycConstant admin = new CycConstant("Administrator", new Guid("bd58aedc-9c29-11b1-9dad-c379636f7270"));
      thisObj = new SubLAPIHelper("localhost", 3660, admin);
      setLoggingLevel(Level.FINE);
      String command = thisObj.makeSubLStmt("asdlfksjd", 1, 2, 3, 4);
      Object result = thisObj.executeCommandSynchronously(command);
      command = thisObj.makeSubLStmt("list", new QuotedAsIsTerm("a"),
              new QuotedAsIsTerm("b"), new QuotedAsIsTerm("c"), admin);
      result = thisObj.executeCommandSynchronously(command);
    } catch (Exception e) {
      logger.log(Level.SEVERE, e.getMessage(), e);
    } finally {
      if (thisObj != null) {
        thisObj.close();
      }
      logger.info("Finished.");
      System.exit(0);
    }
  }
}
