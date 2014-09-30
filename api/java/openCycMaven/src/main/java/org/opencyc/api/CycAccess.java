package org.opencyc.api;

//// External Imports
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.text.CharacterIterator;
import java.text.SimpleDateFormat;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

//// Internal Imports
import static org.opencyc.api.CycObjectFactory.makeCycSymbol;
import static org.opencyc.api.SubLAPIHelper.makeNestedSubLStmt;
import static org.opencyc.api.SubLAPIHelper.makeSubLStmt;
import org.opencyc.cycobject.CycFormulaSentence;
import org.opencyc.util.StringUtils;
import org.opencyc.cycobject.CycAssertion;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycDenotationalTerm;
import org.opencyc.cycobject.CycFormula;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycListParser;
import org.opencyc.cycobject.CycNart;
import org.opencyc.cycobject.CycNaut;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSentence;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.CycVariable;
import org.opencyc.cycobject.DefaultCycObject;
import org.opencyc.cycobject.ELMt;
import org.opencyc.cycobject.ELMtConstant;
import org.opencyc.cycobject.ELMtCycNaut;
import org.opencyc.cycobject.ELMtNart;
import org.opencyc.cycobject.Guid;
import org.opencyc.inference.DefaultInferenceParameterDescriptions;
import org.opencyc.inference.DefaultInferenceParameters;
import org.opencyc.inference.DefaultInferenceWorkerSynch;
import org.opencyc.inference.InferenceParameterDescriptions;
import org.opencyc.inference.InferenceParameters;
import org.opencyc.inference.InferenceResultSet;
import org.opencyc.inference.InferenceWorkerSynch;
import org.opencyc.soap.SOAPBinaryCycConnection;
import org.opencyc.util.DateConverter;
import org.opencyc.util.LRUCache;
import org.opencyc.util.Log;
import org.opencyc.util.Pair;
import org.opencyc.util.PasswordManager;
import org.opencyc.util.TimeOutException;

/**
 * Provides wrappers for the OpenCyc API.
 *
 * <p>
 * Collaborates with the <tt>CycConnection</tt> class which manages the api connections.
 * </p>
 *
 * @version $Id: CycAccess.java 140169 2012-05-24 20:58:48Z daves $
 * @author Stephen L. Reed <p><p><p><p><p>
 */
public class CycAccess {

  private static final CycSymbol CYC_QUERY = makeCycSymbol("cyc-query");
  private static final CycSymbol EL_WFF = makeCycSymbol("el-wff?");
  private static final CycSymbol EQUALS_EL = makeCycSymbol("equals-el?");
  private static final CycSymbol FPRED_VALUE_IN_MT = makeCycSymbol("fpred-value-in-mt");
  private static final CycSymbol SOME_PRED_VALUE_IN_ANY_MT = makeCycSymbol("some-pred-value-in-any-mt");
  private static final CycSymbol SOME_PRED_VALUE_IN_RELEVANT_MTS = makeCycSymbol("some-pred-value-in-relevant-mts");
  private static final CycSymbol WITH_ALL_MTS = makeCycSymbol("with-all-mts");

  //// Constructors
  /**
   * Constructs a new CycAccess object.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycAccess()
          throws UnknownHostException, IOException, CycApiException {
    this(CycConnection.DEFAULT_HOSTNAME,
            CycConnection.DEFAULT_BASE_PORT);
  }

  /**
   * Constructs a new CycAccess object.
   *
   * @param conn the Cyc connection object (in persistent, binary mode)
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycAccess(CycConnectionInterface conn)
          throws IOException, CycApiException {
    hostName = conn.getHostName();
    port = conn.getBasePort();
    persistentConnection = PERSISTENT_CONNECTION;
    cycConnection = conn;
    commonInitialization();
  }

  /**
   * Constructs a new CycAccess object for a SOAP connection.
   *
   * @param endpointURL the SOAP XML endpoint URL which indicates the Cyc API web services host
   * @param hostName the name of the computer hosting the Cyc server
   * @param basePort the Cyc server base listening port
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycAccess(URL endpointURL,
          String hostName,
          int basePort)
          throws IOException, CycApiException {
    this.hostName = hostName;
    this.port = basePort;
    isSOAPConnection = true;
    this.persistentConnection = PERSISTENT_CONNECTION;
    cycConnection = new SOAPBinaryCycConnection(endpointURL, hostName, basePort, this);
    commonInitialization();
  }

  /**
   * Constructs a new CycAccess object given a host name, port, communication mode, and messaging mode
   *
   * @param hostName the host name
   * @param basePort the base (HTML serving) TCP socket port number
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycAccess(String hostName,
          int basePort)
          throws UnknownHostException, IOException, CycApiException {
    this.hostName = hostName;
    this.port = basePort;
    this.persistentConnection = PERSISTENT_CONNECTION;

    cycConnection = new CycConnection(hostName,
            port,
            this);
    commonInitialization();
  }

  public static CycAccess getNewCycAccessInteractively() {
    CycAccess cycAccess = null;
    String hostname = "localhost";
    final Integer[] ports = {3600, 3620, 3600, 3660, 3680};
    Integer port = ports[0];
    while (cycAccess == null) {
      hostname = JOptionPane.showInputDialog("Specify host machine:", hostname);
      if (hostname == null) {
        return null;
      }
      port = (Integer) JOptionPane.showInputDialog(
              null, "Specify port:", "Specify Port", JOptionPane.QUESTION_MESSAGE, null,
              ports, port);
      if (port == null) {
        return null;
      }
      try {
        cycAccess = new CycAccess(hostname, port);
      } catch (Exception ex) {
        final int result = JOptionPane.showConfirmDialog(null,
                "Got Exception trying to connect to " + hostname + ":" + port + ":\n" + ex.getLocalizedMessage()
                + "\nTry again?", "Exception", JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
        if (result != JOptionPane.OK_OPTION) {
          return null;
        }
      }
    }
    return cycAccess;
  } //// Public Area
  /**
   * Dictionary of CycAccess instances, indexed by thread so that the application does not have to
   * keep passing around a CycAccess object reference.
   */
  public static final Map<Thread, CycAccess> cycAccessInstances = new HashMap<Thread, CycAccess>();
  /**
   * Shared CycAccess instance when thread synchronization is entirely handled by the application.
   * Use of the CycAccess.current() method returns this reference if the lookup by process thread
   * fails.
   */
  public static CycAccess sharedCycAccessInstance = null;
  /** Value indicating that the OpenCyc api should use one TCP socket for the entire session. */
  public static final int PERSISTENT_CONNECTION = 2;
  /** Value indicating that the OpenCyc api should use one TCP socket for the entire session. */
  public static final int XML_SOAP_CONNECTION = 3;
  /**
   * Parameter indicating whether the OpenCyc api should use one TCP socket for the entire session,
   * or if the socket is created and then closed for each api call, or if an XML SOAP service
   * provides the message transport.
   */
  public int persistentConnection = PERSISTENT_CONNECTION;
  /** Default value for isLegacyMode is no compatibility with older versions of the OpenCyc api. */
  public static final boolean DEFAULT_IS_LEGACY_MODE = false;
  /** the indicator that API request forms should be logged to a file api-requests.lisp in the working directory */
  public boolean areAPIRequestsLoggedToFile = false;
  public FileWriter apiRequestLog = null;
  /** Convenient reference to #$BaseKb. */
  public static final ELMt baseKB = ELMtConstant.makeELMtConstant(
          new CycConstant("BaseKB", new Guid("bd588111-9c29-11b1-9dad-c379636f7270")));
  /** Convenient reference to #$isa. */
  public static final CycConstant isa = new CycConstant("isa", new Guid("bd588104-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$genls. */
  public static final CycConstant genls = new CycConstant("genls", new Guid("bd58810e-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$genlMt. */
  public static final CycConstant genlMt = new CycConstant("genlMt", new Guid("bd5880e5-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$comment. */
  public static final CycConstant comment = new CycConstant("comment", new Guid("bd588109-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$Collection. */
  public static final CycConstant collection = new CycConstant("Collection", new Guid("bd5880cc-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$BinaryPredicate. */
  public static final CycConstant binaryPredicate = new CycConstant("BinaryPredicate", new Guid("bd588102-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$elementOf. */
  public static final CycConstant elementOf = new CycConstant("elementOf", new Guid("c0659a2b-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$numericallyEquals. */
  public static final CycConstant numericallyEqual = new CycConstant("numericallyEquals", new Guid("bd589d90-9c29-11b1-9dad-c379636f7270"));
  /************************* constants needed by CycL parser *********/
  /** Convenient reference to #$True. */
  public static final CycConstant trueConst = new CycConstant("True", new Guid("bd5880d9-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$False. */
  public static final CycConstant falseConst = new CycConstant("False", new Guid("bd5880d8-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$not. */
  public static final CycConstant not = new CycConstant("not", new Guid("bd5880fb-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$and. */
  public static final CycConstant and = new CycConstant("and", new Guid("bd5880f9-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$or. */
  public static final CycConstant or = new CycConstant("or", new Guid("bd5880fa-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$xor. */
  public static final CycConstant xorConst = new CycConstant("xor", new Guid("bde7f9f2-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$equiv. */
  public static final CycConstant equivConst = new CycConstant("equiv", new Guid("bda887b6-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$implies. */
  public static final CycConstant impliesConst = new CycConstant("implies", new Guid("bd5880f8-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$forAll. */
  public static final CycConstant forAllConst = new CycConstant("forAll", new Guid("bd5880f7-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$thereExists. */
  public static final CycConstant thereExistsConst = new CycConstant("thereExists", new Guid("bd5880f6-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$thereExistExactly. */
  public static final CycConstant thereExistExactlyConst = new CycConstant("thereExistExactly", new Guid("c10ae7b8-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$thereExistAtMost. */
  public static final CycConstant thereExistAtMostConst = new CycConstant("thereExistAtMost", new Guid("c10af932-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$thereExistAtLeast. */
  public static final CycConstant thereExistAtLeastConst = new CycConstant("thereExistAtLeast", new Guid("c10af5e7-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$ExapndSubLFn. */
  public static final CycConstant expandSubLFnConst = new CycConstant("ExpandSubLFn", new Guid("c0b2bc13-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$SubLQuoteFn. */
  public static final CycConstant sublQuoteFnConst = new CycConstant("SubLQuoteFn", new Guid("94f07021-8b0d-11d7-8701-0002b3a8515d"));
  /** Convenient reference to #$PlusFn. */
  public static final CycConstant plusFn = new CycConstant("PlusFn", new Guid("bd5880ae-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$different. */
  public static final CycConstant different = new CycConstant("different", new Guid("bd63f343-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$Thing. */
  public static final CycConstant thing = new CycConstant("Thing", new Guid("bd5880f4-9c29-11b1-9dad-c379636f7270"));
  /** Convenient reference to #$MtSpace. */
  public static final CycConstant mtSpace = new CycConstant("MtSpace", new Guid("abb96eb5-e798-11d6-8ac9-0002b3a333c3"));
  /** Convenient reference to #$CurrentWorldDataCollectorMt-NonHomocentric. */
  public static final ELMt currentWorldDataMt = ELMtConstant.makeELMtConstant(
          new CycConstant("CurrentWorldDataCollectorMt-NonHomocentric", new Guid("bf192b1e-9c29-11b1-9dad-c379636f7270")));
  /** Convenient reference to #$InferencePSC. */
  public static final ELMt inferencePSC = ELMtConstant.makeELMtConstant(
          new CycConstant("InferencePSC", new Guid("bd58915a-9c29-11b1-9dad-c379636f7270")));
  /** Convenient reference to #$AnytimePSC. */
  public static final ELMt anytimePSC = ELMtConstant.makeELMtConstant(
          new CycConstant("AnytimePSC", new Guid("28392742-b00f-41d8-98de-8399027785de")));
  /** Convenient reference to #$EverythingPSC. */
  public static final ELMt everythingPSC = ELMtConstant.makeELMtConstant(  
          new CycConstant("EverythingPSC", new Guid("be7f041b-9c29-11b1-9dad-c379636f7270")));
  /** Convenient reference to #$UniversalVocabularyMt. */
  public static final ELMt universalVocabularyMt = ELMtConstant.makeELMtConstant(
          new CycConstant("UniversalVocabularyMt", new Guid("dff4a041-4da2-11d6-82c0-0002b34c7c9f")));
  /** Convenient reference to #$bookkeepingMt. */
  public static final ELMt bookkeepingMt = ELMtConstant.makeELMtConstant(
          new CycConstant("BookkeepingMt", new Guid("beaed5bd-9c29-11b1-9dad-c379636f7270")));

  /**
   * Returns a string representation of this object.
   *
   * @return a string representation of this object
   */
  @Override
  public String toString() {
    if (cycConnection == null) {
      return "CycAccess: no valid connection";
    }
    return cycConnection.connectionInfo();
  }

  /**
   * Returns the CycAccess that is to be used for this thread.
   * @return 
   * @throws RuntimeException when there is no CycAccess for this thread.
   */
  public static CycAccess getCurrent() {
      return currentCyc.get();
  }
  
  /**
   * 
   * @param access set the CycAccess object that will be used as the default CycAccess object for this thread.
   */
  public static CycAccess setCurrent(CycAccess access) {
      currentCyc.set(access);
      return access;
  }

  /**
   * Specify that this thread should use a CycAccess object pointing to <code>hostname</code> and <code>port</code>.
   * This may use an existing CycAccess object, or if one can't be found, it will create a new CycAccess object 
   * and make that new CycAccess object the default one for this thread.
   * @param hostName the name of the machine where the desired Cyc Server resides.
   * @param port the port number where the desired Cyc Server resides.
   */
    public static CycAccess setCurrent(String hostName, int port) throws UnknownHostException, IOException {
        String key = hostName + ":" + port;
        if (currentCycAccesses.containsKey(key)) {
            setCurrent(currentCycAccesses.get(key));
        } else {
            CycAccess cyc = new CycAccess(hostName, port);
            setCurrent(cyc);
            currentCycAccesses.put(key, cyc);
        }
        return getCurrent();
    }

  /**
   * Returns the <tt>CycAccess</tt> object for this thread.
   *
   * @return the <tt>CycAccess</tt> object for this thread
   * @throws RuntimeException when there is no CycAcess object for this thread
   * @deprecated Replaced by {@link #getCurrent()}
   */
  @Deprecated
  public static CycAccess current() {
    CycAccess cycAccess = cycAccessInstances.get(Thread.currentThread());

    if (cycAccess == null) {
      if (sharedCycAccessInstance != null) {
        return sharedCycAccessInstance;
      } else {
        throw new RuntimeException("No CycAccess object for this thread");
      }
    }

    return cycAccess;
  }

  /**
   * Returns true if there is a CycAccess object for this thread.
   *
   * @return true if there is a CycAccess object for this thread.
   * @deprecated  Check to see if the value of {@link getCurrent()} is null
   */
  @Deprecated
  public static boolean hasCurrent() {
    CycAccess cycAccess = cycAccessInstances.get(Thread.currentThread());

    if (cycAccess == null) {
      /*if (sharedCycAccessInstance != null) {
      return true;
      } else*/ {
        return false;
      }
    }
    return true;
  }

  /**
   * Sets the shared <tt>CycAccess</tt> instance.
   *
   * @param sharedCycAccessInstance shared<tt>CycAccess</tt> instance
   * @deprecated  Use {@link setCurrent(CycAccess)} instead.
   */
  @Deprecated
  public static void setSharedCycAccessInstance(CycAccess sharedCycAccessInstance) {
    CycAccess.sharedCycAccessInstance = sharedCycAccessInstance;
  }

  /** Returns the Cyc api services lease manager.
   *
   *@return the Cyc api services lease manager
   */
  public CycLeaseManager getCycLeaseManager() {
    return cycLeaseManager;
  }

  /**
   * Turns on the diagnostic trace of socket messages.
   */
  public void traceOn() {
    cycConnection.traceOn();
    trace = CycConnection.API_TRACE_MESSAGES;
  }

  /**
   * Turns on the detailed diagnostic trace of socket messages.
   */
  public void traceOnDetailed() {
    if (cycConnection != null) {
      cycConnection.traceOnDetailed();
    }

    trace = CycConnection.API_TRACE_DETAILED;
  }

  /**
   * Turns off the diagnostic trace of socket messages.
   */
  public void traceOff() {
    cycConnection.traceOff();
    trace = CycConnection.API_TRACE_NONE;
  }

  /**
   * gets the hostname of the connection
   *
   * @return the hostname of the connection
   */
  public String getHostName() {
    return cycConnection.getHostName();
  }

  /**
   * gets the baseport of the connection
   *
   * @return the baseport of the connection
   */
  public int getBasePort() {
    return cycConnection.getBasePort();
  }


  /**
   * @return the http of server the connection is connected to.
   */
  public int getHttpPort() {
    return cycConnection.getHttpPort();
  }

  /** 
   * Returns the browser URL for the Cyc image that this CycAccess is connected to,
   * as the URL would be seen from the machine where the CycAccess is running.  Note that if
   * there is a firewall, port-forwarding or other indirection between the browser and the machine where
   * this CycAccess is situated, the URL returned may not be functional.
   */
  public String getBrowserUrl() {
    return "http://" + getHostName() + ":" + getHttpPort() + "/cgi-bin/cg?cb-start";
  }
  
  /**
   * Returns the CycConnection object.
   *
   * @return the CycConnection object
   */
  public CycConnectionInterface getCycConnection() {
    return cycConnection;
  }
  /** Indicates whether the connection is closed */
  private volatile boolean isClosed = false;

  /** Returns whether the connection is closed
   * @return whether the connection is closed
   */
  public boolean isClosed() {
    return isClosed;
  }

  /**
   * Closes the CycConnection object. Modified by APB to be able to handle multiple calls to
   * close() safely.
   */
  public synchronized void close() {
    if (isClosed) {
      return;
    }
    isClosed = true;
    if (cycLeaseManager != null) {
      cycLeaseManager.interrupt();
    }
    if (cycConnection != null) {
      cycConnection.close();
    }
    if (areAPIRequestsLoggedToFile) {
      try {
        apiRequestLog.close();
      } catch (IOException e) {
        System.err.println("error when closing apiRequestLog: " + e.getMessage());
      }
    }
    cycAccessInstances.remove(Thread.currentThread());
    // make sure it's not every used again for setting as the 'current' CycAccess.
    for (Map.Entry<String, CycAccess> entry : currentCycAccesses.entrySet()) {
      if (entry.getValue().equals(this)) {
         currentCycAccesses.remove(entry.getKey());
      }
    }
    /*if (sharedCycAccessInstance == null || sharedCycAccessInstance.equals(this)) {
    final Iterator iter = cycAccessInstances.values().iterator();
    if (iter.hasNext())
    sharedCycAccessInstance = (CycAccess) iter.next();
    else
    sharedCycAccessInstance = null;
    }*/
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as an object.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public Object converseObject(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      return response[1];
    } else {
      throw new ConverseException(command, response);
    }
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as a list.  The symbol
   * nil is returned as the empty list.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList converseList(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (response[1].equals(CycObjectFactory.nil)) {
        return new CycList();
      } else {
        if (response[1] instanceof CycList) {
          return (CycList) response[1];
        }
      }
    }
    throw new ConverseException(command, response);
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as a Cyc sentence.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycFormulaSentence converseSentence(Object command)
          throws UnknownHostException, IOException, CycApiException {
    return new CycFormulaSentence(converseList(command));
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as a CycObject.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycObject converseCycObject(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (response[1].equals(CycObjectFactory.nil)) {
        return new CycList();
      } else {
        return (CycObject) response[1];
      }
    } else {
      throw new ConverseException(command, response);
    }
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as a String.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws IOException if a data communication error occurs
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws CycApiException if the api request results in a cyc server error
   * @throws RuntimeException if the return from Cyc is not a string
   */
  public String converseString(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (!(response[1] instanceof String)) {
        throw new RuntimeException("Expected String but received (" + response[1].getClass()
                + ") " + response[1] + "\n in response to command " + command);
      }

      return (String) response[1];
    } else {
      throw new ConverseException(command, response);
    }
  }

  static public class ConverseException extends CycApiException {

    private ConverseException(final Object command, final Object[] response) {
      super(response[1].toString() + "\nrequest: " + makeRequestString(command));
      if (response[1] instanceof CycApiException) {
        this.initCause((CycApiException) response[1]);
      }
    }

    private static String makeRequestString(final Object command) {
      if (command instanceof CycList) {
        return ((CycList) command).cyclify();
      } else {
        return command.toString();
      }

    }
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as a boolean.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean converseBoolean(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (response[1].toString().equals("T")) {
        return true;
      } else {
        return false;
      }
    } else {
      throw new ConverseException(command, response);
    }
  }

  /**
   * Converses with Cyc to perform an API command whose result is returned as an int.
   *
   * @param command the command string or CycList
   *
   * @return the result of processing the API command
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int converseInt(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      return Integer.valueOf(response[1].toString());
    } else {
      throw new ConverseException(command, response);
    }
  }

  /**
   * Converses with Cyc to perform an API command whose result is void.
   *
   * @param command the command string or CycList
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void converseVoid(final Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    response = converse(command);

    if (response[0].equals(Boolean.FALSE)) {
      throw new ConverseException(command, response);
    }
  }

  /** Try to enhance <code>urlString</code> to log <code>cyclist</code> in and redirect to
   * the page it would otherwise go to directly.
   */
  public String maybeAddLoginRedirect(final String urlString, final CycFort cyclist,
          final CycDenotationalTerm applicationTerm) {
    // sample urlString: cg?CB-CF&236134
    final int questionMarkPos = urlString.indexOf('?');
    if ((cyclist instanceof CycConstant) && (questionMarkPos >= 0)) {
      final String cgiProgram = urlString.substring(0, questionMarkPos);
      final String originalQueryString = urlString.substring(questionMarkPos + 1);
      final StringBuilder stringBuilder = new StringBuilder(cgiProgram);
      stringBuilder.append("?cb-login-handler");
      stringBuilder.append("&new_login_name=").append(((CycConstant) cyclist).getName());
      maybeAddPassword(cyclist, applicationTerm, stringBuilder);
      stringBuilder.append("&redirect-handler=").append(originalQueryString);
      return stringBuilder.toString();
    } else {
      return urlString;
    }
  }

  private String doubleURLEncode(final String password) throws UnsupportedEncodingException {
    return urlEncode(urlEncode(password));
  }

  private String urlEncode(final String password) throws UnsupportedEncodingException {
    return URLEncoder.encode(password, UTF8);
  }

  /** Add a piece to the URL being string-built to specify cyclist's (encrypted) password */
  private void maybeAddPassword(final CycFort cyclist, final CycDenotationalTerm applicationTerm,
          final StringBuilder stringBuilder) {
    if (cyclist instanceof CycConstant) {
      final PasswordManager passwordManager = new PasswordManager(this);
      try {
        if (passwordManager.isPasswordRequired()) {
          final String password = passwordManager.lookupPassword((CycConstant) cyclist, applicationTerm);
          if (password != null) {
            // @hack -- Cyc decodes '+' characters twice, so we encode twice:
            final String urlEncodedPassword = doubleURLEncode(password);
            stringBuilder.append("&new_login_hashed_password=").append(urlEncodedPassword);
          }
        }
      } catch (IOException ex) {
        // Ignore: User may have to supply password to browser.
      }
    }
  }

  /**
   * Sets the print-readable-narts feature on.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void setReadableNarts()
          throws UnknownHostException, IOException, CycApiException {
    converseVoid("(csetq *print-readable-narts t)");
  }

  /**
   * Gets a known CycConstant by using its constant name.
   *
   * @param constantName the name of the constant to be instantiated
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise throw an exception
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant getKnownConstantByName(String constantName)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant cycConstant = getConstantByName(constantName);

    if (cycConstant == null) {
      throw new CycApiException("Expected constant not found " + constantName);
    }

    return cycConstant;
  }

  /**
   * Gets a known CycFort by using its constant name or NART string.
   *
   * @param fortName the name of the FORT to be instantiated
   * @return the complete <tt>CycFort</tt> if found, otherwise throw an exception
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort getKnownFortByName(String fortName)
          throws UnknownHostException, IOException, CycApiException {
    CycFort fort = null;
    if (fortName.contains(")")) {
      final CycList terms = new CycListParser(this).read(fortName);
      fort = getCycNartFromCons(terms);
    } else {
      fort = getKnownConstantByName(fortName);
    }
    return fort;
  }

  public List findConstantsForNames(List constantNames)
          throws UnknownHostException, IOException, CycApiException {
    if ((constantNames == null) || (constantNames.size() <= 0)) {
      return null;
    }
    StringBuffer command = new StringBuffer("(MAPCAR (QUOTE FIND-CONSTANT) (LIST");
    for (Iterator iter = constantNames.iterator(); iter.hasNext();) {
      command.append(" \"");
      String curConstName = StringUtils.escapeDoubleQuotes("" + iter.next());
      command.append(curConstName);
      command.append("\"");
    }
    command.append("))");
    final Object result = converseCycObject("" + command);
    if (!(result instanceof CycList)) {
      return null;
    }
    return (CycList) result;
  }

  public List findConstantsForGuids(List constantGuids)
          throws UnknownHostException, IOException, CycApiException {
    if ((constantGuids == null) || (constantGuids.size() <= 0)) {
      return null;
    }
    List result = new ArrayList();
    // @ToDo this is very inefficient...we need to find a way to do this
    // with a single round trip to Cyc. --Tony
    for (Iterator iter = constantGuids.iterator(); iter.hasNext();) {
      CycConstant item = (CycConstant) iter.next();
      try {
        result.add(getConstantByGuid(item.getGuid()));
      } catch (Exception e) {
        result.add(CycObjectFactory.nil);
      }
    }
    return result;
  }

  /**
   * Gets a CycConstant by using its constant name.
   *
   * @param constantName the name of the constant to be instantiated
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise return null
   *
   * @throws IOException if a data communication error occurs
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant getConstantByName(final String constantName)
          throws UnknownHostException, IOException, CycApiException {
    String name = constantName;
    if (constantName.startsWith("#$")) {
      name = name.substring(2);
    }
    CycConstant answer = CycObjectFactory.getCycConstantCacheByName(name);
    if (answer != null) {
      return answer;
    }
    final String command = makeSubLStmt("find-constant", name);
    final Object answerObject = converseObject(command);
    if (answerObject instanceof CycConstant) {
      answer = (CycConstant) answerObject;
      CycObjectFactory.addCycConstantCache(answer);
      return answer;
    }
    return null;
  }

  /**
   * Gets the Guid for the given constant name, raising an exception if the constant does not
   * exist.
   *
   * @param constantName the name of the constant object for which the Guid is sought
   *
   * @return the Guid for the given CycConstant
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public Guid getConstantGuid(String constantName)
          throws UnknownHostException, IOException, CycApiException {
    final String command =
            makeSubLStmt("guid-to-string",
            makeNestedSubLStmt("constant-external-id",
            makeNestedSubLStmt("find-constant", constantName)));
    return CycObjectFactory.makeGuid(converseString(command));
  }

  /**
   * Gets a <tt>CycAssertion</tt> by using its ID.
   *
   * @param id the id of the <tt>CycAssertion</tt> sought
   *
   * @return the <tt>CycAssertion</tt> if found or <tt>null</tt> if not found
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycAssertion getAssertionById(Integer id)
          throws UnknownHostException, IOException, CycApiException {
    final String command = makeSubLStmt("find-assertion-by-id", id);
    Object obj = converseObject(command);

    if (obj.equals(new CycSymbol("NIL"))) {
      return null;
    } else if (!(obj instanceof CycAssertion)) {
      throw new RuntimeException(obj + " is not a CycAssertion");
    } else {
      return (CycAssertion) obj;
    }
  }

  /**
   * Gets the name for the given constant guid.
   *
   * @param guid the guid of the constant object for which the name is sought
   *
   * @return the name for the given CycConstant
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getConstantName(Guid guid)
          throws UnknownHostException, IOException, CycApiException {
    // Optimized for the binary api.

    String command = makeSubLStmt("constant-name",
            makeNestedSubLStmt("find-constant-by-external-id",
            makeNestedSubLStmt("string-to-guid"), guid.toString()));

    return converseString(command);
  }

  /**
   * Gets a known CycConstant by using its GUID string.
   *
   * @param guidString the globally unique ID string of the constant to be instantiated
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise throw an exception
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant getKnownConstantByGuid(String guidString)
          throws UnknownHostException, IOException, CycApiException {
    Guid guid = CycObjectFactory.makeGuid(guidString);

    return getKnownConstantByGuid(guid);
  }

  /**
   * Gets a known CycConstant by using its GUID.
   *
   * @param guid the globally unique ID of the constant to be instantiated
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise throw an exception
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant getKnownConstantByGuid(Guid guid)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant cycConstant = getConstantByGuid(guid);

    if (cycConstant == null) {
      throw new CycApiException("Expected constant not found " + guid);
    }

    return cycConstant;
  }

  /**
   * Gets a CycConstant by using its GUID.
   *
   * @param guid the GUID from which to find the constant
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise return <tt>null</tt>
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant getConstantByGuid(Guid guid)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant answer = CycObjectFactory.getCycConstantCacheByGuid(
            guid);
    if (answer != null) {
      return answer;
    }
    final String command =
            makeSubLStmt("find-constant-by-external-id",
            makeNestedSubLStmt("string-to-guid", guid.getGuidString()));
    final Object answerObject = converseObject(command);
    if (answerObject instanceof CycConstant) {
      answer = (CycConstant) answerObject;
      CycObjectFactory.addCycConstantCache(answer);
      return answer;
    }
    return null;
  }

  /**
   * Adds #$ to string for all CycConstants mentioned in the string that don't already have them
   * @param str the String that will have #$'s added to it.
   *
   * @return a copy of str with #$'s added where appropriate.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String cyclifyString(String str)
          throws UnknownHostException, IOException, CycApiException {
    final String command =
            makeSubLStmt("cyclify-string", str);
    final String answer = converseString(command);
    return answer;
  }

  /**
   * Makes a known CycConstant by using its GUID and name, adding it to the cache.
   *
   * @param guidString the known GUID string from which to make the constant
   * @param constantName the known name to associate with the constant
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise return <tt>null</tt>
   */
  public CycConstant makeConstantWithGuidName(String guidString,
          String constantName) {
    return makeConstantWithGuidName(CycObjectFactory.makeGuid(
            guidString),
            constantName);
  }

  /**
   * Makes a known CycConstant by using its GUID and name, adding it to the cache.
   *
   * @param guid the known GUID from which to make the constant
   * @param constantName the known name to associate with the constant
   *
   * @return the complete <tt>CycConstant</tt> if found, otherwise return <tt>null</tt>
   */
  public CycConstant makeConstantWithGuidName(Guid guid,
          String constantName) {
    CycConstant answer = CycObjectFactory.getCycConstantCacheByGuid(guid);
    if (answer != null) {
      return answer;
    }
    answer = new CycConstant(constantName, guid);
    CycObjectFactory.addCycConstantCache(answer);

    return answer;
  }

  /**
   * Gets the CycNart object from a Cons object that lists the names of its functor and its
   * arguments.
   *
   * @param elCons the given list which names the functor and arguments
   *
   * @return a CycNart object from a Cons object that lists the names of its functor and its
   *         arguments
   */
  public CycNart getCycNartFromCons(CycList elCons) {
    return new CycNart(elCons);
  }

  /**
   * Returns true if CycConstant BINARYPREDICATE relates CycFort ARG1 and CycFort ARG2.
   *
   * @param binaryPredicate the predicate
   * @param arg1 the first argument related by the predicate
   * @param arg2 the second argument related by the predicate
   *
   * @return true if CycConstant BINARYPREDICATE relates CycFort ARG1 and CycFort ARG2 otherwise
   *         false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean predicateRelates(CycConstant binaryPredicate,
          CycFort arg1,
          CycFort arg2)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    final String command =
            makeSubLStmt("pred-u-v-holds-in-any-mt", binaryPredicate, arg1, arg2);
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (response[1] == null) {
        return false;
      } else if (response[1].toString().equals("T")) {
        return true;
      } else {
        return false;
      }
    } else {
      throw new CycApiException(response[1].toString());
    }
  }

  /**
   * Returns true if CycConstant BINARYPREDICATE relates CycFort ARG1 and CycFort ARG2.
   *
   * @param binaryPredicate the predicate
   * @param arg1 the first argument related by the predicate
   * @param arg2 the second argument related by the predicate
   * @param mt the relevant mt
   *
   * @return true if CycConstant BINARYPREDICATE relates CycFort ARG1 and CycFort ARG2 otherwise
   *         false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean predicateRelates(CycConstant binaryPredicate,
          CycFort arg1,
          CycFort arg2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};
    final String command =
            makeSubLStmt("pred-u-v-holds", binaryPredicate, arg1, arg2, makeELMt(mt));
    response = converse(command);

    if (response[0].equals(Boolean.TRUE)) {
      if (response[1] == null) {
        return false;
      } else if (response[1].toString().equals("T")) {
        return true;
      } else {
        return false;
      }
    } else {
      throw new CycApiException(response[1].toString());
    }
  }

  private String getGeneratedPhrase(CycObject cycObject, boolean precise, CycObject languageMt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    final NLFormat nlf = NLFormat.getInstance(this);
    nlf.setPrecise(precise);
    if (languageMt != null) {
      nlf.setFormatLanguageMt(languageMt);
    }
    return nlf.format(cycObject);
  }

  /**
   * Gets the default generated phrase for a CycFort (intended for predicates).
   *
   * @param cycObject the predicate term for paraphrasing
   *
   * @return the default generated phrase for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getGeneratedPhrase(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    return getGeneratedPhrase(cycObject, true, null);
  }

  /**
   * Gets the default generated phrase for a CycFort (intended for predicates),
   * with precise paraphrasing off.
   *
   * @param cycObject the predicate term for paraphrasing
   *
   * @return the default generated phrase for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  /*
  public String getImpreciseGeneratedPhrase(CycObject cycObject)
  throws UnknownHostException, IOException, CycApiException {
  //// Preconditions
  if (cycObject == null) {
  throw new NullPointerException("cycObject must not be null");
  }
  if (!(cycObject instanceof CycConstant
  || cycObject instanceof CycNart
  || cycObject instanceof CycList)) {
  throw new IllegalArgumentException("cycObject must be a CycConstant, CycNart or CycList " + cycObject.cyclify());
  }
  String command = "(with-precise-paraphrase-off (generate-phrase "
  + cycObject.stringApiValue() + "))";
  return converseString(command);
  }
   */
  /**
   * Gets the default generated phrase for a CycFort (intended for predicates)
   * from a particular MT with precise paraphrasing off.
   *
   * @param cycObject the predicate term for paraphrasing
   *
   * @return the default generated phrase for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  /*
  public String getImpreciseGeneratedPhrase(CycObject cycObject, CycObject mt)
  throws UnknownHostException, IOException, CycApiException {
  //// Preconditions
  if (cycObject == null) {
  throw new NullPointerException("cycObject must not be null");
  }
  if (!(cycObject instanceof CycConstant
  || cycObject instanceof CycNart
  || cycObject instanceof CycList)) {
  throw new IllegalArgumentException("cycObject must be a CycConstant, CycNart or CycList " + cycObject.cyclify());
  }
  String command = "(with-precise-paraphrase-off (generate-phrase "
  + cycObject.stringApiValue() + " :DEFAULT nil " + mt.stringApiValue() + "))";
  return converseString(command);
  }
   */
  /**
   * Gets the paraphrase for a Cyc assertion.
   *
   * @param assertion the assertion formula
   *
   * @return the paraphrase for a Cyc assertion
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getParaphrase(CycList assertion)
          throws UnknownHostException, IOException, CycApiException {
    return getGeneratedPhrase(assertion);
  }

  /**
   * Gets the paraphrase for a Cyc sentence.
   *
   * @param sentence the sentence
   *
   * @return the paraphrase
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getParaphrase(CycFormulaSentence sentence)
          throws UnknownHostException, IOException, CycApiException {
    return getGeneratedPhrase(sentence);
  }

  /**
   * Gets the imprecise paraphrase for a Cyc assertion.
   *
   * @param assertionString the assertion formula
   *
   * @return the imprecise paraphrase for a Cyc assertion
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getImpreciseParaphrase(String assertionString)
          throws UnknownHostException, IOException, CycApiException {
    CycFormulaSentence assertion = this.makeCycSentence(assertionString);
    return getGeneratedPhrase(assertion, false, null);
  }

  /**
   * Gets the imprecise paraphrase for a Cyc assertion.
   *
   * @param assertion the assertion formula
   *
   * @return the imprecise paraphrase for a Cyc assertion
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getImpreciseParaphrase(CycList assertion)
          throws UnknownHostException, IOException, CycApiException {
    return getGeneratedPhrase(assertion, false, null);
  }

  /**
   * Gets the concise paraphrase for a Cyc sentence.
   *
   * @param sentence the assertion formula
   *
   * @return the imprecise paraphrase for a Cyc assertion
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getImpreciseParaphrase(CycFormulaSentence sentence)
          throws UnknownHostException, IOException, CycApiException {
    return getGeneratedPhrase(sentence, false, null);
  }

  /**
   * Gets the comment for a CycFort.  Embedded quotes are replaced by spaces.
   *
   * @param cycObject the term for which the comment is sought
   *
   * @return the comment for the given CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getComment(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (cycObject instanceof CycList) {
      return null;
    }
    String script = "(clet ((comment-string \n" + "         (with-all-mts (comment "
            + cycObject.stringApiValue() + ")))) \n" + "  (fif comment-string \n"
            + "       (string-substitute \" \" \"\\\"\" comment-string) \n"
            + "       \"\"))";

    return converseString(script);
  }

  /**
   * Gets the comment for a CycFort in the relevant mt. Embedded quotes are replaced by spaces.
   *
   * @param cycFort the term for which the comment is sought
   * @param mt the relevant mt from which the comment is visible
   *
   * @return the comment for the given CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public String getComment(final CycObject cycObject,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    String script =
            "(clet ((comment-string \n"
            + "         (comment " + cycObject.stringApiValue() + " " + makeELMt(mt).stringApiValue() + "))) \n"
            + "  (fif comment-string \n"
            + "       (string-substitute \" \" \"\\\"\" comment-string) \n"
            + "       \"\"))";

    return converseString(script);
  }

  /**
   * Gets the list of the isas for the given CycFort.
   *
   * @param cycObject the term for which its isas are sought
   *
   * @return the list of the isas for the given CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getIsas(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(remove-duplicates (with-all-mts (isa " + cycObject.stringApiValue()
            + ")))");
  }

  /**
   * Gets the list of the isas for the given CycFort.
   *
   * @param cycObject the term for which its isas are sought
   * @param mt the relevant mt
   *
   * @return the list of the isas for the given CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getIsas(final CycObject cycObject,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(isa " + cycObject.stringApiValue()
            + " " + makeELMt(mt).stringApiValue()
            + ")");
  }

  /**
   * Gets the list of the directly asserted true genls for the given CycFort collection.
   *
   * @param cycObject the given term
   *
   * @return the list of the directly asserted true genls for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenls(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(remove-duplicates (with-all-mts (genls " + cycObject.stringApiValue()
            + ")))");
  }

  /**
   * Gets the list of the directly asserted true genls for the given CycFort collection.
   *
   * @param cycObject the given term
   * @param mt the relevant mt
   *
   * @return the list of the directly asserted true genls for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenls(final CycObject cycObject,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(genls " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the minimum (most specific) genls for a CycFort collection.
   *
   * @param cycFort the given collection term
   *
   * @return a list of the minimum (most specific) genls for a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinGenls(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (min-genls "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets a list of the minimum (most specific) genls for a CycFort collection.
   *
   * @param cycFort the collection
   * @param mt the microtheory in which to look
   *
   * @return a list of the minimum (most specific) genls for a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinGenls(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(min-genls " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the directly asserted true specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the directly asserted true specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSpecs(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (specs " + cycFort.stringApiValue()
            + ")))");
  }

  /**
   * Gets the list of the directly asserted true specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   * @param mt the microtheory in which to look
   *
   * @return the list of the directly asserted true specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSpecs(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(specs " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the least specific specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the least specific specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMaxSpecs(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (max-specs "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the least specific specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   * @param mt the microtheory in which to look
   *
   * @return the list of the least specific specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMaxSpecs(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(max-specs " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the direct genls of the direct specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the direct genls of the direct specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenlSiblings(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (genl-siblings "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the direct genls of the direct specs for the given CycFort collection.
   *
   * @param cycFort the given collection
   * @param mt the microtheory in which to look
   *
   * @return the list of the direct genls of the direct specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenlSiblings(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(genl-siblings " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the siblings (direct specs of the direct genls) for the given CycFort
   * collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the siblings (direct specs of the direct genls) for the given CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSiblings(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return getSpecSiblings(cycFort);
  }

  /**
   * Gets the list of the siblings (direct specs of the direct genls) for the given CycFort
   * collection.
   *
   * @param cycFort the given collection
   * @param mt the microtheory in which to look
   *
   * @return the list of the siblings (direct specs of the direct genls) for the given CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSiblings(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return getSpecSiblings(cycFort,
            mt);
  }

  /**
   * Gets the list of the siblings (direct specs of the direct genls) for the given CycFort
   * collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the siblings (direct specs of the direct genls) for the given CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSpecSiblings(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (spec-siblings "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the siblings (direct specs of the direct genls) for the given CycFort
   * collection.
   *
   * @param cycFort the given collection
   * @param mt the microtheory in which to look
   *
   * @return the list of the siblings (direct specs of the direct genls) for the given CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSpecSiblings(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(spec-siblings " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect genls for the given CycFort collection.
   *
   * @param cycFort the collection
   *
   * @return the list of all of the direct and indirect genls for a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenls(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-genls-in-any-mt " + cycFort.stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect genls for a CycFort collection given a
   * relevant microtheory.
   *
   * @param cycObject the collection
   * @param mt the relevant mt
   *
   * @return the list of all of the direct and indirect genls for a CycFort collection given a
   *         relevant microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenls(CycObject cycObject,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(all-genls " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of all of the direct and indirect specs for a CycFort collection.
   *
   * @param cycFort the collection
   *
   * @return the list of all of the direct and indirect specs for the given collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecs(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-specs "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the direct and indirect specs for the given collection in the given
   * microtheory.
   *
   * @param cycFort the collection
   * @param mt the microtheory
   *
   * @return the list of all of the direct and indirect specs for the given collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecs(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-specs " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect genls for a CycFort SPEC which are also specs
   * of CycFort GENL.
   *
   * @param spec the given collection
   * @param genl the more general collection
   *
   * @return the list of all of the direct and indirect genls for a CycFort SPEC which are also
   *         specs of CycFort GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenlsWrt(CycFort spec,
          CycFort genl)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-genls-wrt "
            + spec.stringApiValue() + " " + genl.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the direct and indirect genls for a CycFort SPEC which are also specs
   * of CycFort GENL.
   *
   * @param spec the given collection
   * @param genl the more general collection
   * @param mt the relevant mt
   *
   * @return the list of all of the direct and indirect genls for a CycFort SPEC which are also
   *         specs of CycFort GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenlsWrt(CycFort spec,
          CycFort genl,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-genls-wrt " + spec.stringApiValue() + " " + genl.stringApiValue()
            + " " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the dependent specs for a CycFort collection.  Dependent specs are
   * those direct and indirect specs of the collection such that every path connecting the spec to
   * a genl of the collection passes through the collection.  In a typical taxomonmy it is
   * expected that all-dependent-specs gives the same result as all-specs.
   *
   * @param cycFort the given collection
   *
   * @return the list of all of the dependent specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllDependentSpecs(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-dependent-specs "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the dependent specs for a CycFort collection.  Dependent specs are
   * those direct and indirect specs of the collection such that every path connecting the spec to
   * a genl of the collection passes through the collection.  In a typical taxomonmy it is
   * expected that all-dependent-specs gives the same result as all-specs.
   *
   * @param cycFort the given collection
   * @param mt the relevant mt
   *
   * @return the list of all of the dependent specs for the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllDependentSpecs(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-dependent-specs " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list with the specified number of sample specs of the given CycFort collection.
   * Attempts to return leaves that are maximally differet with regard to their all-genls.
   *
   * @param cycFort the given collection
   * @param numberOfSamples the maximum number of sample specs returned
   *
   * @return the list with the specified number of sample specs of the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSampleLeafSpecs(CycFort cycFort,
          int numberOfSamples)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (sample-leaf-specs " + cycFort.stringApiValue() + " "
            + numberOfSamples + "))");
  }

  /**
   * Gets the list with the specified number of sample specs of the given CycFort collection.
   * Attempts to return leaves that are maximally differet with regard to their all-genls.
   *
   * @param cycFort the given collection
   * @param numberOfSamples the maximum number of sample specs returned
   * @param mt the relevant mt
   *
   * @return the list with the specified number of sample specs of the given CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getSampleLeafSpecs(CycFort cycFort,
          int numberOfSamples,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(sample-leaf-specs " + cycFort.stringApiValue() + " " + numberOfSamples
            + " " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns the single most specific collection from the given list of collectons.
   *
   * @param collections the given collections
   *
   * @return the single most specific collection from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycFort getMinCol(final CycList collections)
          throws UnknownHostException, IOException, CycApiException {
    return (CycFort) converseObject("(with-all-mts (min-col " + collections.stringApiValue()
            + "))");
  }

  /**
   * Returns the single most specific collection from the given list of collectons.
   *
   * @param collections the given collections
   * @param mt the relevant mt
   *
   * @return the single most specific collection from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycFort getMinCol(final CycList collections, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (collections == null) {
      throw new NullPointerException("collections must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return (CycFort) converseObject("(with-inference-mt-relevance " + makeELMt(mt).stringApiValue()
            + " (min-col " + collections.stringApiValue() + "))");
  }

  /**
   * Returns the most general collections from the given list of collectons.
   *
   * @param collections the given collections
   *
   * @return the most general collections from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMaxCols(final CycList collections)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    assert collections != null : "collections cannot be null";

    return converseList("(with-all-mts (max-cols " + collections.stringApiValue()
            + "))");
  }

  /**
   * Returns the most general collections from the given list of collectons.
   *
   * @param collections the given collections
   * @param mt the inference microtheory
   *
   * @return the most general collections from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMaxCols(final CycList collections, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (collections == null) {
      throw new NullPointerException("collections must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(with-inference-mt-relevance " + makeELMt(mt).stringApiValue() + " (max-cols " + collections.stringApiValue()
            + "))");
  }

  /**
   * Returns the most specific collections from the given list of collectons.
   *
   * @param collections the given collections
   *
   * @return the most specific collections from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinCols(final CycList collections)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    assert collections != null : "collections cannot be null";

    return converseList("(with-all-mts (min-cols " + collections.stringApiValue()
            + "))");
  }

  /**
   * Returns the most specific collections from the given list of collectons.
   *
   * @param collections the given collections
   * @param mt the inference microtheory
   *
   * @return the most specific collections from the given list of collectons
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinCols(final CycList collections, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (collections == null) {
      throw new NullPointerException("collections must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(with-inference-mt-relevance " + makeELMt(mt).stringApiValue()
            + " (min-cols " + collections.stringApiValue() + "))");
  }

  /**
   * Returns true if CycFort SPEC is a spec of CycFort GENL.
   *
   * @param spec the considered spec collection
   * @param genl the considered genl collection
   *
   * @return true if CycFort SPEC is a spec of CycFort GENL, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isSpecOf(CycObject spec,
          CycObject genl)
          throws UnknownHostException, IOException, CycApiException {
    return isGenlOf(genl,
            spec);
  }

  /**
   * Returns true if CycFort SPEC is a spec of CycFort GENL.
   *
   * @param spec the considered spec collection
   * @param genl the considered genl collection
   * @param mt the relevant mt
   *
   * @return true if CycFort SPEC is a spec of CycFort GENL, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isSpecOf(CycObject spec,
          CycObject genl,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return isGenlOf(genl,
            spec,
            mt);
  }

  /**
   * Returns true if CycFort GENL is a genl of CycFort SPEC.
   *
   * @param genl the collection for genl determination
   * @param spec the collection for spec determination
   *
   * @return <tt>true</tt> if CycFort GENL is a genl of CycFort SPEC
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlOf(CycObject genl,
          CycObject spec)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(genl-in-any-mt? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort GENL is a genl of CycFort SPEC, implements a cache to avoid asking the
   * same question twice from the KB.
   *
   * @param genl the collection for genl determination
   * @param spec the collection for spec determination
   *
   * @return <tt>true</tt> if CycFort GENL is a genl of CycFort SPEC
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlOf_Cached(CycObject genl,
          CycObject spec)
          throws UnknownHostException, IOException, CycApiException {
    final Pair args = new Pair(genl, spec);
    Boolean isGenlOf = isGenlOfCache.get(args);
    if (isGenlOf != null) {
      return isGenlOf.booleanValue();
    }
    final boolean answer = isGenlOf(genl, spec);
    isGenlOfCache.put(args, answer);
    return answer;
  }

  /**
   * Returns true if CycFort GENL is a genl of CycFort SPEC in MT.
   *
   * @param genl the collection for genl determination
   * @param spec the collection for spec determination
   * @param mt the microtheory for spec determination
   *
   * @return <tt>true</tt> if CycFort GENL is a genl of CycFort SPEC in MT
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlOf(CycObject genl,
          CycObject spec,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(genl? " + spec.stringApiValue() + " " + genl.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort GENLPRED is a genl-pred of CycFort SPECPRED in MT.
   *
   * @param genlPred the predicate for genl-pred determination
   * @param specPred the predicate for spec-pred determination
   * @param mt the microtheory for subsumption determination
   *
   * @return <tt>true</tt> if CycFort GENLPRED is a genl-pred of CycFort SPECPRED in MT
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlPredOf(CycFort genlPred,
          CycFort specPred,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(genl-predicate? " + specPred.stringApiValue() + " "
            + genlPred.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort GENLPRED is a genl-pred of CycFort SPECPRED in any MT.
   *
   * @param genlPred the predicate for genl-pred determination
   * @param specPred the predicate for spec-pred determination
   *
   * @return <tt>true</tt> if CycFort GENLPRED is a genl-pred of CycFort SPECPRED in any MT
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlPredOf(CycFort genlPred,
          CycFort specPred)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (genl-predicate? " + specPred.stringApiValue() + " "
            + genlPred.stringApiValue() + "))");
  }

  /**
   * Returns true if CycFort GENLPRED is a genl-inverse of CycFort SPECPRED in MT.
   *
   * @param genlPred the predicate for genl-inverse determination
   * @param specPred the predicate for spec-inverse determination
   * @param mt the microtheory for inverse subsumption determination
   *
   * @return <tt>true</tt> if CycFort GENLPRED is a genl-inverse of CycFort SPECPRED in MT
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlInverseOf(CycFort genlPred,
          CycFort specPred,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(genl-inverse? " + specPred.stringApiValue() + " "
            + genlPred.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort GENLPRED is a genl-inverse of CycFort SPECPRED in any MT.
   *
   * @param genlPred the predicate for genl-inverse determination
   * @param specPred the predicate for spec-inverse determination
   *
   * @return <tt>true</tt> if CycFort GENLPRED is a genl-inverse of CycFort SPECPRED in any MT
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlInverseOf(CycFort genlPred,
          CycFort specPred)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (genl-inverse? " + specPred.stringApiValue() + " "
            + genlPred.stringApiValue() + "))");
  }

  /**
   * Returns true if CycFort GENLMT is a genl-mt of CycFort SPECPRED in mt-mt (currently
   * #$UniversalVocabularyMt).
   *
   * @param genlMt the microtheory for genl-mt determination
   * @param specMt the microtheory for spec-mt determination
   *
   * @return <tt>true</tt> if CycFort GENLMT is a genl-mt of CycFort SPECPRED in mt-mt (currently
   *         #$UniversalVocabularyMt)
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isGenlMtOf(CycObject genlMt,
          CycObject specMt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(genl-mt? " + makeELMt(specMt).stringApiValue() + " "
            + makeELMt(genlMt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are tacitly coextensional via
   * mutual genls of each other.
   *
   * @param collection1 the first given collection
   * @param collection2 the second given collection
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are tacitly coextensional via
   *         mutual genls of each other, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areTacitCoextensional(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (tacit-coextensional? " + collection1.stringApiValue()
            + " " + collection2.stringApiValue() + "))");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are tacitly coextensional via
   * mutual genls of each other.
   *
   * @param collection1 the first given collection
   * @param collection2 the second given collection
   * @param mt the relevant mt
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are tacitly coextensional via
   *         mutual genls of each other, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areTacitCoextensional(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(tacit-coextensional? " + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are asserted coextensional.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are asserted coextensional
   *         otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areAssertedCoextensional(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant coExtensional = this.getKnownConstantByGuid(
            "bd59083a-9c29-11b1-9dad-c379636f7270");

    if (predicateRelates(coExtensional,
            collection1,
            collection2)) {
      return true;
    } else if (predicateRelates(coExtensional,
            collection2,
            collection1)) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are asserted coextensional.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are asserted coextensional
   *         otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areAssertedCoextensional(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant coExtensional = this.getKnownConstantByGuid(
            "bd59083a-9c29-11b1-9dad-c379636f7270");

    if (predicateRelates(coExtensional,
            collection1,
            collection2,
            mt)) {
      return true;
    } else if (predicateRelates(coExtensional,
            collection2,
            collection1,
            mt)) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 intersect with regard to all-specs.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 intersect with regard to all-specs
   *         otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areIntersecting(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (collections-intersect? "
            + collection1.stringApiValue() + " " + collection2.stringApiValue()
            + "))");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 intersect with regard to all-specs.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 intersect with regard to all-specs
   *         otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areIntersecting(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(collections-intersect? " + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are in a hierarchy.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are in a hierarchy, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areHierarchical(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (hierarchical-collections? "
            + collection1.stringApiValue() + " " + collection2.stringApiValue()
            + "))");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are in a hierarchy.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are in a hierarchy, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areHierarchical(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(hierarchical-collections? " + collection1.stringApiValue()
            + collection2.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the justifications of why CycFort SPEC is a SPEC of CycFort GENL.
   * getWhyGenl("Dog", "Animal") --> "(((#$genls #$Dog #$CanineAnimal) :TRUE) (#$genls
   * #$CanineAnimal #$NonPersonAnimal) :TRUE) (#$genls #$NonPersonAnimal #$Animal) :TRUE))
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   *
   * @return the list of the justifications of why CycFort SPEC is a SPEC of CycFort GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyGenl(CycFort spec,
          CycFort genl)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (why-genl? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + "))");
  }

  /**
   * Gets the list of the justifications of why CycFort SPEC is a SPEC of CycFort GENL.
   * getWhyGenl("Dog", "Animal") --> "(((#$genls #$Dog #$CanineAnimal) :TRUE) (#$genls
   * #$CanineAnimal #$NonPersonAnimal) :TRUE) (#$genls #$NonPersonAnimal #$Animal) :TRUE))
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   * @param mt the relevant mt
   *
   * @return the list of the justifications of why CycFort SPEC is a SPEC of CycFort GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyGenl(CycFort spec,
          CycFort genl,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(why-genl? " + spec.stringApiValue() + " " + genl.stringApiValue()
            + " " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort SPEC is a SPEC of CycFort
   * GENL. getWhyGenlParaphrase("Dog", "Animal") --> "a dog is a kind of canine" "a canine is a
   * kind of non-human animal" "a non-human animal is a kind of animal"
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   *
   * @return the English parapharse of the justifications of why CycFort SPEC is a SPEC of CycFort
   *         GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyGenlParaphrase(CycFort spec,
          CycFort genl)
          throws UnknownHostException, IOException, CycApiException {
    CycList listAnswer = converseList("(with-all-mts (why-genl? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + "))");
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    CycList iter = listAnswer;

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort SPEC is a SPEC of CycFort
   * GENL. getWhyGenlParaphrase("Dog", "Animal") --> "a dog is a kind of canine" "a canine is a
   * kind of non-human animal" "a non-human animal is a kind of animal"
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   * @param mt the relevant mt
   *
   * @return the English parapharse of the justifications of why CycFort SPEC is a SPEC of CycFort
   *         GENL
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyGenlParaphrase(CycFort spec,
          CycFort genl,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList listAnswer = converseList("(why-genl? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    CycList iter = listAnswer;

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the list of the justifications of why CycFort COLLECTION1 and a CycFort COLLECTION2
   * intersect. see getWhyGenl
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return the list of the justifications of why CycFort COLLECTION1 and a CycFort COLLECTION2
   *         intersect
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyCollectionsIntersect(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (why-collections-intersect? "
            + collection1.stringApiValue() + " " + collection2.stringApiValue() + "))");
  }

  /**
   * Gets the list of the justifications of why CycFort COLLECTION1 and a CycFort COLLECTION2
   * intersect. see getWhyGenl
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return the list of the justifications of why CycFort COLLECTION1 and a CycFort COLLECTION2
   *         intersect
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyCollectionsIntersect(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(why-collections-intersect? " + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort COLLECTION1 and a CycFort
   * COLLECTION2 intersect. see getWhyGenlParaphrase
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return the English parapharse of the justifications of why CycFort COLLECTION1 and a CycFort
   *         COLLECTION2 intersect
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyCollectionsIntersectParaphrase(CycFort collection1,
          CycFort collection2)
          throws UnknownHostException, IOException,
          CycApiException {
    CycList listAnswer = converseList("(with-all-mts (why-collections-intersect? "
            + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + "))");
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    CycList iter = listAnswer;

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();


      //Log.current.println("assertion: " + assertion);
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort COLLECTION1 and a CycFort
   * COLLECTION2 intersect. see getWhyGenlParaphrase
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return the English parapharse of the justifications of why CycFort COLLECTION1 and a CycFort
   *         COLLECTION2 intersect
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyCollectionsIntersectParaphrase(CycFort collection1,
          CycFort collection2,
          CycObject mt)
          throws UnknownHostException, IOException,
          CycApiException {
    CycList listAnswer = converseList("(with-all-mts (why-collections-intersect? "
            + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    CycList iter = listAnswer;

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();


      //Log.current.println("assertion: " + assertion);
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the list of the collection leaves (most specific of the all-specs) for a CycFort
   * collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the collection leaves (most specific of the all-specs) for a CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getCollectionLeaves(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (collection-leaves " + cycFort.stringApiValue() + "))");
  }

  /**
   * Gets the list of the collection leaves (most specific of the all-specs) for a CycFort
   * collection.
   *
   * @param cycFort the given collection
   * @param mt the relevant mt
   *
   * @return the list of the collection leaves (most specific of the all-specs) for a CycFort
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getCollectionLeaves(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(collection-leaves " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the collections asserted to be disjoint with a CycFort collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the collections asserted to be disjoint with a CycFort collection
   *
   * @throws IOException if a communication error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getLocalDisjointWith(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (local-disjoint-with " + cycFort.stringApiValue() + "))");
  }

  /**
   * Gets the list of the collections asserted to be disjoint with a CycFort collection.
   *
   * @param cycFort the given collection
   * @param mt the relevant mt
   *
   * @return the list of the collections asserted to be disjoint with a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getLocalDisjointWith(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(local-disjoint-with " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are disjoint.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are disjoint, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areDisjoint(CycObject collection1,
          CycObject collection2)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (disjoint-with? " + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + "))");
  }

  /**
   * Returns true if CycFort COLLECION1 and CycFort COLLECTION2 are disjoint.
   *
   * @param collection1 the first collection
   * @param collection2 the second collection
   * @param mt the relevant mt
   *
   * @return true if CycFort COLLECION1 and CycFort COLLECTION2 are disjoint, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean areDisjoint(CycObject collection1,
          CycObject collection2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(with-all-mts (disjoint-with? " + collection1.stringApiValue() + " "
            + collection2.stringApiValue() + " " + makeELMt(
            mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the most specific collections (having no subsets) which contain a CycFort
   * term.
   *
   * @param cycFort the given term
   *
   * @return the list of the most specific collections (having no subsets) which contain a CycFort
   *         term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinIsas(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (min-isa " + cycFort.stringApiValue() + "))");
  }

  /**
   * Gets the list of the most specific collections (having no subsets) which contain a CycFort
   * term.
   *
   * @param cycFort the given term
   * @param mt the relevant mt
   *
   * @return the list of the most specific collections (having no subsets) which contain a CycFort
   *         term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getMinIsas(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(min-isa " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the asserted instances of a CycFort collection.
   *
   * @param cycFort the given collection
   *
   * @return the list of the instances (who are individuals) of a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInstances(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    CycList result = converseList("(with-all-mts (instances " + cycFort.stringApiValue() + "))");
    return result;
  }

  /**
   * Gets the list of the asserted instances of a CycFort collection.
   *
   * @param cycFort the given collection
   * @param mt the relevant mt
   *
   * @return the list of the instances (who are individuals) of a CycFort collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInstances(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(instances " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the instance siblings of a CycFort, for all collections of which it is an
   * instance.
   *
   * @param cycFort the given term
   *
   * @return the list of the instance siblings of a CycFort, for all collections of which it is an
   *         instance
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInstanceSiblings(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (instance-siblings " + cycFort.stringApiValue() + "))");
  }

  /**
   * Gets the list of the instance siblings of a CycFort, for all collections of which it is an
   * instance.
   *
   * @param cycFort the given term
   * @param mt the relevant mt
   *
   * @return the list of the instance siblings of a CycFort, for all collections of which it is an
   *         instance
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if an error is returned by the Cyc server
   */
  public CycList getInstanceSiblings(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(instance-siblings " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the collections of which the CycFort is directly and indirectly an instance.
   *
   * @param cycFort the given term
   *
   * @return the list of the collections of which the CycFort is directly and indirectly an
   *         instance
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllIsa(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(all-isa-in-any-mt " + cycFort.stringApiValue() + ")";
    CycList result = converseList(command);
    return result;
  }

  /**
   * Gets the list of the collections of which the CycFort is directly and indirectly an instance.
   *
   * @param cycFort the given term
   * @param mt the relevant mt
   *
   * @return the list of the collections of which the CycFort is directly and indirectly an
   *         instance
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllIsa(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-isa " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of all the direct and indirect instances (individuals) for a CycFort collection.
   *
   * @param cycFort the collection for which all the direct and indirect instances (individuals)
   *        are sought
   *
   * @return the list of all the direct and indirect instances (individuals) for the given
   *         collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllInstances(CycObject cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-instances-in-all-mts " + cycFort.stringApiValue() + ")");
  }

  /**
   * Gets a list of all the direct and indirect instances for a CycFort collection in
   * the given microtheory.
   *
   * @param cycFort the collection for which all the direct and indirect instances are sought
   * @param mt the relevant mt
   *
   * @return the list of all the direct and indirect instances for the
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error given collection
   */
  public CycList getAllInstances(CycObject cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-instances " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of all the direct and indirect quoted instances for a CycFort collection in
   * the given microtheory.
   *
   * @param cycFort the collection for which all the direct and indirect quoted instances are sought
   * @param mt the relevant mt
   *
   * @return the list of all the direct and indirect quoted instances for the CycFort collection in
   * the given microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error given collection
   */
  public CycList getAllQuotedInstances(final CycObject cycFort, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return getAllQuotedInstances(cycFort, mt, 0);
  }

  /**
   * Gets a list of all the direct and indirect quoted instances for a CycFort collection in
   * the given microtheory.
   *
   * @param cycFort the collection for which all the direct and indirect quoted instances are sought
   * @param mt the relevant mt
   *
   * @return the list of all the direct and indirect quoted instances for the CycFort collection in
   * the given microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error given collection
   * @throws TimeOutException if the calculation times out
   */
  public CycList getAllQuotedInstances(final CycObject cycFort, final CycObject mt,
          final long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    final CycVariable queryVariable = CycObjectFactory.makeCycVariable("?QUOTED-INSTANCE");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByName("quotedIsa"), queryVariable, cycFort);
    return queryVariable(queryVariable, query, makeELMt(mt), null, timeoutMsecs);
  }

  /**
   * Returns true if CycFort TERM is a instance of CycFort COLLECTION, defaulting to all
   * microtheories.
   *
   * @param term the term
   * @param collectionName the name of the collection
   *
   * @return <tt>true</tt> if CycFort TERM is a instance of the CycFort named by COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isa(CycObject term,
          String collectionName)
          throws UnknownHostException, IOException, CycApiException {
    return isa(term,
            getKnownConstantByName(collectionName));
  }

  /**
   * Returns true if CycFort TERM is a instance of CycFort COLLECTION, defaulting to all
   * microtheories.
   *
   * @param term the term
   * @param collection the collection
   *
   * @return <tt>true</tt> if CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isa(CycObject term,
          CycFort collection)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(isa-in-any-mt? " + term.stringApiValue() + " "
            + collection.stringApiValue() + ")");
  }

  /**
   * Returns true if CycFort TERM is a instance of CycFort COLLECTION, using the given microtheory.
   * Method implementation optimised for the binary api.
   *
   * @param term the term
   * @param collection the collection
   * @param mt the microtheory in which the ask is performed
   *
   * @return <tt>true</tt> if CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isa(CycObject term,
          CycObject collection,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    final String command = "(isa? " + term.stringApiValue() + " "
            + collection.stringApiValue() + " " + makeELMt(mt).stringApiValue() + ")";

    return converseBoolean(command);
  }

  /**
   * Returns true if the quoted CycFort TERM is a instance of CycFort COLLECTION, in any microtheory.
   * Method implementation optimised for the binary api.
   *
   * @param term the term
   * @param collection the collection
   *
   * @return <tt>true</tt> if the quoted CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isQuotedIsa(final CycObject term, final CycObject collection)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    return isQuotedIsa(term, collection, 0);
  }

  /**
   * Returns true if the quoted CycFort TERM is a instance of CycFort COLLECTION, in any microtheory.
   * Method implementation optimised for the binary api.
   *
   * @param term the term
   * @param collection the collection
   * @param timeoutMsecs the time in milliseconds to wait before giving up, set to
   * zero to wait  forever.
   *
   * @return <tt>true</tt> if the quoted CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @throws TimeOutException if the calculation times out
   */
  public boolean isQuotedIsa(final CycObject term, final CycObject collection, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    //// Preconditions
    if (term == null) {
      throw new NullPointerException("term must not be null");
    }
    if (collection == null) {
      throw new NullPointerException("collection must not be null");
    }
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByName("quotedIsa"), term, collection);
    return isQueryTrue(query, inferencePSC, null, timeoutMsecs);
  }

  /**
   * Returns true if the quoted CycFort TERM is a instance of CycFort COLLECTION,
   * in the given inference microtheory.
   *
   * @param term the term
   * @param collection the collection
   * @param mt the inference microtheory
   * set to zero in order to wait forever
   *
   * @return <tt>true</tt> if the quoted CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isQuotedIsa(final CycObject term, final CycObject collection, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    return isQuotedIsa(term, collection, mt, 0);
  }

  /**
   * Returns true if the quoted CycFort TERM is a instance of CycFort COLLECTION,
   * in the given inference microtheory.
   *
   * @param term the term
   * @param collection the collection
   * @param mt the inference microtheory
   * @param timeoutMsecs the time in milliseconds to wait before giving up,
   * set to zero in order to wait forever
   *
   * @return <tt>true</tt> if the quoted CycFort TERM is a instance of CycFort COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @throws TimeOutException if the calculation times out
   */
  public boolean isQuotedIsa(final CycObject term, final CycObject collection, final CycObject mt, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    if (term == null) {
      throw new NullPointerException("Term must not be null.");
    }
    if (collection == null) {
      throw new NullPointerException("Collection must not be null.");
    }
    if (mt == null) {
      throw new NullPointerException("Microtheory must not be null.");
    }
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByName("quotedIsa"), term, collection);
    return isQueryTrue(query, makeELMt(mt), null, timeoutMsecs);
  }

  /**
   * Gets the list of the justifications of why CycFort TERM is an instance of CycFort COLLECTION.
   * getWhyIsa("Brazil", "Country") --> "(((#$isa #$Brazil #$IndependentCountry) :TRUE) (#$genls
   * #$IndependentCountry #$Country) :TRUE))
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   *
   * @return the list of the justifications of why CycFort TERM is an instance of CycFort
   *         COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyIsa(CycObject spec,
          CycObject genl)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(with-all-mts (why-isa? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + "))");
  }

  /**
   * Gets the list of the justifications of why CycFort TERM is an instance of CycFort COLLECTION.
   * getWhyIsa("Brazil", "Country") --> "(((#$isa #$Brazil #$IndependentCountry) :TRUE) (#$genls
   * #$IndependentCountry #$Country) :TRUE))
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   * @param mt the relevant mt
   *
   * @return the list of the justifications of why CycFort TERM is an instance of CycFort
   *         COLLECTION
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getWhyIsa(CycObject spec,
          CycObject genl,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(why-isa? " + spec.stringApiValue() + " " + genl.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort TERM is an instance of
   * CycFort COLLECTION. getWhyGenlParaphase("Brazil", "Country") --> "Brazil is an independent
   * country" "an  independent country is a kind of country"
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   *
   * @return the English parapharse of the justifications of why CycFort TERM is an instance of
   *         CycFort COLLECTION
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyIsaParaphrase(CycObject spec,
          CycObject genl)
          throws IOException, CycApiException {
    String command = "(with-all-mts (why-isa? " + spec.stringApiValue() + " "
            + genl.stringApiValue() + "))";
    CycList listAnswer = converseList(command);
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the English parapharse of the justifications of why CycFort TERM is an instance of
   * CycFort COLLECTION. getWhyGenlParaphase("Brazil", "Country") --> "Brazil is an independent
   * country" "an  independent country is a kind of country"
   *
   * @param spec the specialized collection
   * @param genl the more general collection
   * @param mt the relevant mt
   *
   * @return the English parapharse of the justifications of why CycFort TERM is an instance of
   *         CycFort COLLECTION
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public List getWhyIsaParaphrase(CycFort spec,
          CycFort genl,
          CycObject mt)
          throws IOException, CycApiException {
    String command = "(why-isa? " + spec.stringApiValue() + " " + genl.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")";
    CycList listAnswer = converseList(command);
    List answerPhrases = new ArrayList();

    if (listAnswer.size() == 0) {
      return answerPhrases;
    }

    for (int i = 0; i < listAnswer.size(); i++) {
      CycList assertion = (CycList) ((CycList) listAnswer.get(
              i)).first();
      answerPhrases.add(getParaphrase(assertion));
    }

    return answerPhrases;
  }

  /**
   * Gets the list of the genlPreds for a CycConstant predicate.
   *
   * @param predicate the given predicate term
   *
   * @return the list of the more general predicates for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenlPreds(final CycObject predicate)
          throws UnknownHostException, IOException, CycApiException {
    if (predicate instanceof CycList) {
      final String script =
              "(clet ((canonicalized-predicate (canonicalize-term " + predicate.stringApiValue() + ")))"
              + "  (pif (fort-p canonicalized-predicate)"
              + "    (remove-duplicates (with-all-mts (genl-predicates canonicalized-predicate)))"
              + "    nil))";
      return converseList(script);
    } else {
      return converseList("(remove-duplicates (with-all-mts (genl-predicates "
              + predicate.stringApiValue() + ")))");
    }
  }

  /**
   * Gets the list of the genlPreds for a CycConstant predicate.
   *
   * @param predicate the given predicate term
   * @param mt the relevant mt
   *
   * @return the list of the more general predicates for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getGenlPreds(final CycObject predicate,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    if (predicate instanceof CycList) {
      final String script =
              "(clet ((canonicalized-predicate (canonicalize-term " + predicate.stringApiValue() + ")))"
              + "  (pif (fort-p canonicalized-predicate)"
              + "    (remove-duplicates (with-all-mts (genl-predicates canonicalized-predicate " + makeELMt(mt).stringApiValue() + ")))"
              + "    nil))";
      return converseList(script);
    } else {
      return converseList("(genl-predicates " + predicate.stringApiValue() + " " + makeELMt(mt).stringApiValue() + ")");
    }
  }

  /**
   * Gets the list of all of the genlPreds for a CycConstant predicate, using an upward closure.
   *
   * @param predicate the predicate for which all the genlPreds are obtained
   *
   * @return a list of all of the genlPreds for a CycConstant predicate, using an upward closure
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenlPreds(CycConstant predicate)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-genl-predicates "
            + predicate.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the genlPreds for a CycConstant predicate, using an upward closure.
   *
   * @param predicate the predicate for which all the genlPreds are obtained
   * @param mt the relevant mt
   *
   * @return a list of all of the genlPreds for a CycConstant predicate, using an upward closure
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllGenlPreds(CycConstant predicate,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-genl-predicates " + predicate.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect specs-preds for the given predicate in all
   * microtheories.
   *
   * @param cycFort the predicate
   *
   * @return the list of all of the direct and indirect spec-preds for the given predicate in all
   *         microtheories.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecPreds(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-spec-predicates "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the direct and indirect specs-preds for the given predicate in the
   * given microtheory.
   *
   * @param cycFort the predicate
   * @param mt the microtheory
   *
   * @return the list of all of the direct and indirect spec-preds for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecPreds(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-spec-predicates " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect specs-inverses for the given predicate in all
   * microtheories.
   *
   * @param cycFort the predicate
   *
   * @return the list of all of the direct and indirect spec-inverses for the given predicate in
   *         all microtheories.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecInverses(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (all-spec-inverses "
            + cycFort.stringApiValue() + ")))");
  }

  /**
   * Gets the list of all of the direct and indirect specs-inverses for the given predicate in the
   * given microtheory.
   *
   * @param cycFort the predicate
   * @param mt the microtheory
   *
   * @return the list of all of the direct and indirect spec-inverses for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecInverses(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-spec-inverses " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of all of the direct and indirect specs-mts for the given microtheory in mt-mt
   * (currently #$UniversalVocabularyMt).
   *
   * @param mt the microtheory
   *
   * @return the list of all of the direct and indirect specs-mts for the given microtheory in
   *         mt-mt (currently #$UniversalVocabularyMt)
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getAllSpecMts(CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(all-spec-mts " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg1Isas for a CycConstant predicate.
   *
   * @param cycObject the predicate for which argument 1 contraints are sought.
   *
   * @return the list of the arg1Isas for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg1Isas(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(remove-duplicates (with-all-mts (arg1-isa "
            + cycObject.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the arg1Isas for a CycConstant predicate given an mt.
   *
   * @param predicate the predicate for which argument 1 contraints are sought.
   * @param mt the relevant microtheory
   *
   * @return the list of the arg1Isas for a CycConstant predicate given an mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg1Isas(final CycObject cycObject,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(arg1-isa " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg2Isas for a CycConstant predicate.
   *
   * @param cycObject the predicate for which argument 2 contraints are sought.
   *
   * @return the list of the arg1Isas for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg2Isas(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(remove-duplicates (with-all-mts (arg2-isa "
            + cycObject.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the arg2Isas for a CycConstant predicate given an mt.
   *
   * @param cycObject the predicate for which argument 2 contraints are sought.
   * @param mt the relevant microtheory
   *
   * @return the list of the arg2Isas for a CycConstant predicate given an mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg2Isas(CycObject cycObject,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseList("(arg2-isa " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg3Isas for a CycConstant predicate.
   *
   * @param predicate the predicate for which argument 3 contraints are sought.
   *
   * @return the list of the arg1Isas for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg3Isas(CycFort predicate)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (arg3-isa "
            + predicate.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the arg3Isas for a CycConstant predicate given an mt.
   *
   * @param predicate the predicate for which argument 3 contraints are sought.
   * @param mt the relevant microtheory
   *
   * @return the list of the arg1Isas for a CycConstant predicate given an mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg3Isas(CycFort predicate,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(arg3-isa " + predicate.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg4Isas for a CycConstant predicate.
   *
   * @param predicate the predicate for which argument 4 contraints are sought.
   *
   * @return the list of the arg4Isas for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg4Isas(CycFort predicate)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (arg4-isa "
            + predicate.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the arg4Isas for a CycConstant predicate given an mt.
   *
   * @param predicate the predicate for which argument 4 contraints are sought.
   * @param mt the relevant microtheory
   *
   * @return the list of the arg4Isas for a CycConstant predicate given an mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg4Isas(CycFort predicate,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(arg4-isa " + predicate.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the argNIsas for a CycConstant predicate.
   *
   * @param predicate the predicate for which argument N contraints are sought.
   * @param argPosition the argument position of argument N
   *
   * @return the list of the argNIsas for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArgNIsas(CycFort predicate,
          int argPosition)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(remove-duplicates \n" + "  (with-all-mts \n" + "    (argn-isa "
            + predicate.stringApiValue() + " " + Integer.toString(
            argPosition) + ")))";

    return converseList(command);
  }

  /**
   * Gets the list of the argNIsas for a CycConstant predicate given an mt.
   *
   * @param predicate the predicate for which argument contraints are sought.
   * @param argPosition the argument position of argument N
   * @param mt the relevant microtheory
   *
   * @return the list of the arg1Isas for a CycConstant predicate given an mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArgNIsas(CycFort predicate,
          int argPosition,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(remove-duplicates \n" + "  (with-all-mts \n" + "    (argn-isa \n"
            + "      " + predicate.stringApiValue() + "      "
            + Integer.toString(argPosition) + "      " + makeELMt(
            mt).stringApiValue()
            + ")))";

    return converseList(command);
  }

  /**
   * Gets the list of the interArgIsa1-2 isa constraint pairs for the given predicate.  Each item
   * of the returned list is a pair (arg1-isa arg2-isa) which means that when (#$isa arg1
   * arg1-isa) holds, (#$isa arg2 arg2-isa) must also hold for (predicate arg1 arg2 ..) to be well
   * formed.
   *
   * @param predicate the predicate for interArgIsa1-2 contraints are sought.
   *
   * @return the list of the interArgIsa1-2 isa constraint pairs for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInterArgIsa1_2s(CycFort predicate)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(remove-duplicates \n" + "  (with-all-mts \n" + "    (inter-arg-isa1-2 "
            + predicate.stringApiValue() + ")))";

    return converseList(command);
  }

  /**
   * Gets the list of the interArgIsa1-2 isa constraint pairs for the given predicate.  Each item
   * of the returned list is a pair (arg1-isa arg2-isa) which means that when (#$isa arg1
   * arg1-isa) holds, (#$isa arg2 arg2-isa) must also hold for (predicate arg1 arg2 ..) to be well
   * formed.
   *
   * @param predicate the predicate for interArgIsa1-2 contraints are sought.
   * @param mt the relevant inference microtheory
   *
   * @return the list of the interArgIsa1-2 isa constraint pairs for the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInterArgIsa1_2s(CycFort predicate,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(remove-duplicates \n" + "  (with-all-mts \n" + "    (inter-arg-isa1-2 "
            + "      " + predicate.stringApiValue() + "      "
            + makeELMt(mt).stringApiValue() + ")))";

    return converseList(command);
  }

  /**
   * Gets the list of the interArgIsa1-2 isa constraints for arg2, given the predicate and arg1.
   *
   * @param predicate the predicate for interArgIsa1-2 contraints are sought.
   * @param arg1 the argument in position 1
   *
   * @return the list of the interArgIsa1-2 isa constraints for arg2, given the predicate and arg1
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInterArgIsa1_2_forArg2(CycFort predicate,
          CycFort arg1)
          throws UnknownHostException, IOException, CycApiException {
    CycList result = new CycList();
    ListIterator constraintPairs = getInterArgIsa1_2s(predicate).listIterator();

    while (constraintPairs.hasNext()) {
      CycList pair = (CycList) constraintPairs.next();

      if (pair.first().equals(arg1)) {
        result.add(pair.second());
      }
    }

    return result;
  }

  /**
   * Gets the list of the interArgIsa1-2 isa constraints for arg2, given the predicate and arg1.
   *
   * @param predicate the predicate for interArgIsa1-2 contraints are sought.
   * @param arg1 the argument in position 1
   * @param mt the relevant inference microtheory
   *
   * @return the list of the interArgIsa1-2 isa constraints for arg2, given the predicate and arg1
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getInterArgIsa1_2_forArg2(CycFort predicate,
          CycFort arg1,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList result = new CycList();
    ListIterator constraintPairs = getInterArgIsa1_2s(predicate,
            mt).listIterator();

    while (constraintPairs.hasNext()) {
      CycList pair = (CycList) constraintPairs.next();

      if (pair.first().equals(arg1)) {
        result.add(pair.second());
      }
    }

    return result;
  }

  /**
   * Gets the list of the resultIsa for a CycConstant function.
   *
   * @param function the given function term
   *
   * @return the list of the resultIsa for a CycConstant function
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getResultIsas(CycFort function)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (result-isa "
            + function.stringApiValue() + ")))");
  }

  /**
   * Gets the list of the resultIsa for a CycConstant function.
   *
   * @param function the given function term
   * @param mt the relevant mt
   *
   * @return the list of the resultIsa for a CycConstant function
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getResultIsas(CycFort function,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(result-isa " + function.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets the list of the argNGenls for a CycConstant predicate.
   *
   * @param predicate the given predicate term
   * @param argPosition the argument position for which the genls argument constraints are sought
   *        (position 1 = first argument)
   *
   * @return the list of the argNGenls for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArgNGenls(CycFort predicate,
          int argPosition)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(remove-duplicates (with-all-mts (argn-genl "
            + predicate.stringApiValue() + " " + argPosition + ")))");
  }

  /**
   * Gets the list of the argNGenls for a CycConstant predicate.
   *
   * @param predicate the given predicate term
   * @param argPosition the argument position for which the genls argument constraints are sought
   *        (position 1 = first argument)
   * @param mt the relevant mt
   *
   * @return the list of the argNGenls for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArgNGenls(CycFort predicate,
          int argPosition,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(argn-genl " + predicate.stringApiValue() + " " + argPosition + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg1Formats for a CycConstant predicate.
   *
   * @param cycObject the given predicate term
   *
   * @return a list of the arg1Formats for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg1Formats(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(with-all-mts (arg1-format " + cycObject.stringApiValue() + "))");
  }

  /**
   * Gets a list of the arg1Formats for a CycConstant predicate.
   *
   * @param cycObject the given predicate term
   * @param mt the relevant mt
   *
   * @return a list of the arg1Formats for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg1Formats(CycObject cycObject,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    return converseList("(arg1-format " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the arg2Formats for a CycConstant predicate.
   *
   * @param cycObject the given predicate term
   *
   * @return a list of the arg2Formats for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg2Formats(final CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(with-all-mts (arg2-format " + cycObject.stringApiValue() + "))");
  }

  /**
   * Gets a list of the arg2Formats for a CycConstant predicate.
   *
   * @param cycObject the given predicate term
   * @param mt the relevant mt
   *
   * @return a list of the arg2Formats for a CycConstant predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg2Formats(final CycObject cycObject,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    return converseList("(arg2-format " + cycObject.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the disjointWiths for a CycFort.
   *
   * @param cycObject the given collection term
   *
   * @return a list of the disjointWiths for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getDisjointWiths(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseList("(remove-duplicates (with-all-mts (local-disjoint-with "
            + cycObject.stringApiValue() + ")))");
  }

  /**
   * Gets a list of the disjointWiths for a CycFort.
   *
   * @param cycFort the given collection term
   * @param mt the relevant mt
   *
   * @return a list of the disjointWiths for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getDisjointWiths(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList("(local-disjoint-with " + cycFort.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Gets a list of the coExtensionals for a CycFort.  Limited to 120 seconds.
   *
   * @param cycObject the given collection term
   *
   * @return a list of the coExtensionals for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getCoExtensionals(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    return getCoExtensionals(cycObject, 0);
  }

  /**
   * Gets a list of the coExtensionals for a CycFort.  Limited to 120 seconds.
   *
   * @param cycObject the given collection term
   * @param timeoutMsecs the time to wait before giving up
   *
   * @return a list of the coExtensionals for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @throws TimeOutException if the calculation times out
   */
  public CycList getCoExtensionals(CycObject cycObject, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    return getCoExtensionals(cycObject, inferencePSC, timeoutMsecs);
  }

  /**
   * Gets a list of the coExtensionals for a CycFort.  Limited to 120 seconds.
   *
   * @param cycObject the given collection term
   * @param mt the microtheory
   *
   * @return a list of the coExtensionals for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getCoExtensionals(CycObject cycObject, CycObject mt)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    return getCoExtensionals(cycObject, mt, 0);
  }

  /**
   * Gets a list of the coExtensionals for a CycFort.
   *
   * @param cycFort the given collection term
   * @param mt the relevant mt for inference
   *
   * @return a list of the coExtensionals for a CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getCoExtensionals(CycObject cycObject,
          CycObject mt, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    CycList answer = null;
    try {
      final String queryString =
              "(#$and"
              + "  (#$different  " + cycObject.cyclify() + " ?X) "
              + "  (#$or (#$coExtensional " + cycObject.cyclify() + " ?X) "
              + "    (#$coextensionalSetOrCollections " + cycObject.cyclify() + " ?X)))";
      final CycFormulaSentence query = makeCycSentence(queryString);
      final CycVariable queryVariable = CycObjectFactory.makeCycVariable("?X");
      answer = queryVariable(queryVariable, query, makeELMt(mt), null, timeoutMsecs);
    } catch (IOException e) {
      Log.current.println("getCoExtensionals - ignoring:\n" + e.getMessage());
      return new CycList();
    }

    return canonicalizeList(answer);
  }

  /**
   * Returns true if the given term is a microtheory.
   *
   * @param cycFort the constant for determination as a microtheory
   *
   * @return <tt>true</tt> iff cycConstant is a microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isMicrotheory(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(isa-in-any-mt? " + cycFort.stringApiValue() + " #$Microtheory)");
  }

  /**
   * Returns true if the given term is a Collection.
   *
   * @param cycObject the given term
   *
   * @return true if the given term is a Collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCollection(final CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseBoolean("(isa-in-any-mt? " + cycObject.stringApiValue() + " #$Collection)");
  }

  /**
   * Returns true if the given term is a Collection.
   *
   * @param cycObject the given term
   * @param mt the inference microtheory
   *
   * @return true if the given term is a Collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCollection(final CycObject cycObject, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseBoolean("(isa? " + cycObject.stringApiValue() + " #$Collection " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns true if the given object is a Collection.
   *
   * @param term the given term
   *
   * @return true if the given term is a Collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCollection(final Object obj)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (obj == null) {
      throw new NullPointerException("term must not be null");
    }
    if (obj instanceof CycObject) {
      return isCollection((CycObject) obj);
    } else {
      return false;
    }
  }

  /**
   * Returns true if the given term is a collection, implemented by a cache to avoid asking the same
   * question twice from the KB.
   *
   * @param cycObject the given term
   *
   * @return true if the given term is a collection
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCollection_Cached(CycObject cycObject)
          throws IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    Boolean isCollection = isCollectionCache.get(cycObject);

    if (isCollection != null) {
      return isCollection.booleanValue();
    }

    final boolean answer = isCollection(cycObject);
    isCollectionCache.put(cycObject, answer);

    return answer;
  }

  public boolean isCollection_Cached(Object term)
          throws IOException, CycApiException {
    if (term instanceof CycObject) {
      return isCollection_Cached((CycObject) term);
    } else {
      return false;
    }
  }

  /**
   * Returns true if the given term is an Individual.
   *
   * @param cycObject the given term
   *
   * @return true if the given term is an Individual
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isIndividual(final CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseBoolean("(isa-in-any-mt? " + cycObject.stringApiValue() + " #$Individual)");
  }

  /**
   * Returns true if the given term is an Individual.
   *
   * @param cycObject the given term
   * @param mt the inference microtheory
   *
   * @return true if the given term is an Individual
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isIndividual(final CycObject cycObject, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseBoolean("(isa? " + cycObject.stringApiValue() + " #$Individual " + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns true if the given is a Function.
   *
   * @param cycFort the given term
   *
   * @return true if the given is a Function
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isFunction(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(isa-in-any-mt? " + cycFort.stringApiValue()
            + " #$Function-Denotational)");
  }

  /**
   * Returns true if the given term is an evaluatable predicate.
   *
   * @param predicate the given term
   *
   * @return true if true if the given term is an evaluatable predicate, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isEvaluatablePredicate(CycFort predicate)
          throws UnknownHostException, IOException, CycApiException {
    final String command = makeSubLStmt("with-all-mts", makeNestedSubLStmt("evaluatable-predicate?", predicate));
    return converseBoolean(command);
  }

  /**
   * Returns true if cycObject is a Predicate.
   *
   * @param cycObject the term for determination as a predicate
   *
   * @return true if cycObject is a Predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isPredicate(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseBoolean("(isa-in-any-mt? " + cycObject.stringApiValue() + " #$Predicate)");
  }

  /**
   * Returns true if cycObject is a Predicate.
   *
   * @param cycObject the term for determination as a predicate
   * @param mt the inference microtheory
   *
   * @return true if cycObject is a Predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isPredicate(final CycObject cycObject, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseBoolean("(isa? " + cycObject.stringApiValue() + " #$Predicate " + makeELMt(mt).stringApiValue() + ")");
  }

  public boolean isPredicate(final Object object, final CycObject mt) throws UnknownHostException, IOException, CycApiException {
    if (object == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    if (object instanceof CycObject) {
      return isPredicate((CycObject) object, mt);
    } else {
      return false;
    }
  }

  public boolean isPredicate(final Object object) throws UnknownHostException, IOException, CycApiException {
    if (object == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    if (object instanceof CycObject) {
      return isPredicate((CycObject) object);
    } else {
      return false;
    }
  }

  /**
   * Returns true if the given term is a UnaryPredicate.
   *
   * @param cycObject the given term
   *
   * @return true if true if the given term is a UnaryPredicate, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isUnaryPredicate(CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseBoolean("(isa-in-any-mt? " + cycObject.stringApiValue()
            + " #$UnaryPredicate)");
  }

  /**
   * Returns true if the given term is a UnaryPredicate.
   *
   * @param cycObject the given term
   * @param mt the inference microtheory
   *
   * @return true if true if the given term is a UnaryPredicate, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isUnaryPredicate(CycObject cycObject, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    return converseBoolean("(isa? " + cycObject.stringApiValue()
            + " #$UnaryPredicate " + makeELMt(mt).stringApiValue() + ")");
  }

  protected void verifyPossibleDenotationalTerm(CycObject cycObject) throws IllegalArgumentException {
    if (!(cycObject instanceof CycDenotationalTerm || cycObject instanceof CycList)) {
      throw new IllegalArgumentException("cycObject must be a Cyc denotational term " + cycObject.cyclify());
    }
  }

  /**
   * Returns true if the cyc object is a BinaryPredicate.
   *
   * @param cycObject the given cyc object
   *
   * @return true if cycObject is a BinaryPredicate, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBinaryPredicate(final CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    return converseBoolean("(binary-predicate? " + cycObject.stringApiValue() + ")");
  }

  /**
   * Returns true if the cyc object is a BinaryPredicate.
   *
   * @param cycObject the given cyc object
   * @param mt the inference microtheory
   *
   * @return true if cycObject is a BinaryPredicate, otherwise false
   *
   * @obsolete mt is not necessary.
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBinaryPredicate(final CycObject cycObject, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return isBinaryPredicate(cycObject);
  }

  /**
   * Returns true if the candidate name uses valid CycConstant characters.
   *
   * @param candidateName the candidate name
   *
   * @return true if the candidate name uses valid CycConstant characters
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isValidConstantName(String candidateName)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(new-constant-name-spec-p \"" + candidateName + "\")");
  }

  /**
   * Returns true if the candidate name is an available CycConstant name, case insensitive.
   *
   * @param candidateName the candidate name
   *
   * @return true if the candidate name uses valid CycConstant characters
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isConstantNameAvailable(String candidateName)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(constant-name-available \"" + candidateName + "\")");
  }

  /**
   * Returns true if term is a quotedCollection, in any microtheory
   *
   * @param cycFort the given CycFort term
   *
   * @return true if term is a quotedCollection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @deprecated
   */
  public boolean isQuotedCollection(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    throw new CycApiException("quotedCollection is no longer supported, see Quote");
  }

  /**
   * Returns true if term is a quotedCollection is a quotedCollection.
   *
   * @param cycFort the given CycFort term
   * @param mt the microtheory in which the query is made
   *
   * @return true if term is a quotedCollection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @deprecated
   */
  public boolean isQuotedCollection(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    throw new CycApiException("quotedCollection is no longer supported, see Quote");
  }

  /** @return true iff expression is free of all variables.
   * @throws IOException if a data communication error occurs
   */
  public boolean isGround(CycObject expression) throws IOException {
    return converseBoolean("(ground? " + DefaultCycObject.stringApiValue(expression) + ")");
  }

  /**
   * Returns true if cycConstant is a PublicConstant.
   *
   * @param cycConstant the given constant
   *
   * @return true if cycConstant is a PublicConstant
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isPublicConstant(CycConstant cycConstant)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(isa-in-any-mt? " + cycConstant.stringApiValue()
            + " #$PublicConstant)");
  }

  /**
   * Gets a list of the public Cyc constants.
   *
   * @return a list of the public Cyc constants
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getPublicConstants()
          throws UnknownHostException, IOException, CycApiException {
    // #$PublicConstant
    return getKbSubset(getKnownConstantByGuid("bd7abd90-9c29-11b1-9dad-c379636f7270"));
  }

  /**
   * Gets a list of the elements of the given CycKBSubsetCollection.
   *
   * @param cycKbSubsetCollection the given CycKBSubsetCollection
   *
   * @return a list of the elements of the given CycKBSubsetCollection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getKbSubset(CycFort cycKbSubsetCollection)
          throws UnknownHostException, IOException, CycApiException {
    CycList answer = converseList("(ask-template '?X '(#$isa ?X "
            + cycKbSubsetCollection.stringApiValue() + ") #$EverythingPSC)");

    return canonicalizeList(answer);
  }

  /**
   * Renames the given constant.
   *
   * @param cycConstant the constant term to be renamed
   * @param newName the new constant name
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void rename(final CycConstant cycConstant, final String newName)
          throws UnknownHostException, IOException, CycApiException {
    String command = wrapBookkeeping("(ke-rename-now " + cycConstant.stringApiValue() + "\"" + newName + "\")");
    Object result = converseObject(command);
    if (result.equals(CycObjectFactory.nil)) {
      throw new CycApiException(newName + " is an invalid new name for " + cycConstant.cyclify());
    }
    CycObjectFactory.removeCaches(cycConstant);
    cycConstant.setName(newName);
    CycObjectFactory.addCycConstantCache(cycConstant);
  }

  /**
   * Kills a Cyc constant.  If CYCCONSTANT is a microtheory, then all the contained assertions are
   * deleted from the KB, the Cyc Truth Maintenance System (TML) will automatically delete any
   * derived assertions whose sole support is the killed term(s).
   *
   * @param cycConstant the constant term to be removed from the KB
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void kill(CycConstant cycConstant)
          throws UnknownHostException, IOException, CycApiException {
    String command = wrapBookkeeping("(ke-kill-now " + cycConstant.stringApiValue() + ")");
    converseBoolean(command);
    CycObjectFactory.removeCaches(cycConstant);
  }

  /**
   * Kills a Cyc constant without issuing a transcript operation. If CYCCONSTANT is a microtheory,
   * then all the contained assertions are deleted from the KB, the Cyc Truth Maintenance System
   * (TMS) will automatically delete any derived assertions whose sole support is the killed
   * term(s).
   *
   * @param cycConstant the constant term to be removed from the KB
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void killWithoutTranscript(CycConstant cycConstant)
          throws UnknownHostException, IOException, CycApiException {
    String command = wrapBookkeeping("(cyc-kill " + cycConstant.stringApiValue() + ")");
    converseBoolean(command);
    CycObjectFactory.removeCaches(cycConstant);
  }

  /**
   * Kills the given Cyc constants.  If CYCCONSTANT is a microtheory, then all the contained
   * assertions are deleted from the KB, the Cyc Truth Maintenance System (TMS) will automatically
   * delete any derived assertions whose sole support is the killed term(s).
   *
   * @param cycConstants the list of constant terms to be removed from the KB
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void kill(CycConstant[] cycConstants)
          throws UnknownHostException, IOException, CycApiException {
    for (int i = 0; i < cycConstants.length; i++) {
      kill(cycConstants[i]);
    }
  }

  /**
   * Kills the given Cyc constants.  If CYCCONSTANT is a microtheory, then all the contained
   * assertions are deleted from the KB, the Cyc Truth Maintenance System (TMS) will automatically
   * delete any derived assertions whose sole support is the killed term(s).
   *
   * @param cycConstants the list of constant terms to be removed from the KB
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void kill(List cycConstants)
          throws UnknownHostException, IOException, CycApiException {
    for (int i = 0; i < cycConstants.size(); i++) {
      kill((CycConstant) cycConstants.get(i));
    }
  }

  /**
   * Kills a Cyc NART (Non Atomic Reified Term).  If CYCFORT is a microtheory, then all the
   * contained assertions are deleted from the KB, the Cyc Truth Maintenance System (TMS) will
   * automatically delete any derived assertions whose sole support is the killed term(s).
   *
   * @param cycFort the NART term to be removed from the KB
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public synchronized void kill(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    if (cycFort instanceof CycConstant) {
      kill((CycConstant) cycFort);
    } else {
      String command = wrapBookkeeping("(ke-kill-now " + cycFort.stringApiValue() + ")");
      converseBoolean(command);
    }
  }

  /**
   * Returns the value of the Cyclist.
   *
   * @return the value of the Cyclist
   */
  public CycFort getCyclist() {
    return cyclist;
  }

  /**
   * Sets the value of the Cyclist, whose identity will be attached via #$myCreator bookkeeping
   * assertions to new KB entities created in this session.
   *
   * @param cyclistName the name of the cyclist term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void setCyclist(String cyclistName)
          throws UnknownHostException, IOException, CycApiException {
    if (cyclistName == null || "".equals(cyclistName)) {
      throw new CycApiException("Invalid cyclist name specified.");
    }
    Object term = getHLCycTerm(cyclistName);
    CycFort newCyclist = null;
    if (term instanceof CycFort) {
      newCyclist = (CycFort) term;
    } else {
      // see if it is a blank name
      newCyclist = find(cyclistName);
    }
    if (newCyclist == null) {
      throw new CycApiException("Cannot interpret " + cyclistName + " as a cyclist.");
    }
    setCyclist(newCyclist);
  }

  /**
   * Sets the value of the Cyclist, whose identity will be attached via #$myCreator bookkeeping
   * assertions to new KB entities created in this session.
   *
   * @param cyclist the cyclist term
   */
  public void setCyclist(CycFort cyclist) {
    this.cyclist = cyclist;
  }

  /**
   * Returns the value of the project (KE purpose).
   *
   * @return he value of the project (KE purpose)
   */
  public CycFort getKePurpose() {
    return project;
  }

  /**
   * Sets the value of the KE purpose, whose project name will be attached via #$myCreationPurpose
   * bookkeeping assertions to new KB entities created in this session.
   *
   * @param projectName the string name of the KE Purpose term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void setKePurpose(String projectName)
          throws UnknownHostException, IOException, CycApiException {
    setKePurpose((CycFort) getHLCycTerm(projectName));
  }

  /**
   * Sets the value of the KE purpose, whose project name will be attached via #$myCreationPurpose
   * bookkeeping assertions to new KB entities created in this session.
   *
   * @param project the KE Purpose term
   */
  public void setKePurpose(CycFort project) {
    this.project = project;
  }

  /**
   * Asserts the given sentence, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithTranscript(CycList sentence,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscript(sentence.stringApiValue(), mt);
  }

  /**
   * Asserts the given sentence, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithTranscript(String sentence,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertSentence(sentence, makeELMt(mt), false, true);
  }

  /**
   * Asserts the given sentence with bookkeeping, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithTranscriptAndBookkeeping(String sentence, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscriptAndBookkeeping(makeCycSentence(sentence), mt);
  }

  /**
   * Asserts the given sentence with bookkeeping, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithTranscriptAndBookkeeping(CycFormulaSentence sentence, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscriptAndBookkeepingInternal(sentence, mt);
  }

  /**
   * Asserts the given sentence with bookkeeping, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithTranscriptAndBookkeeping(CycList sentence, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscriptAndBookkeepingInternal(sentence, mt);
  }

  /**
   * Asserts the given sentence with bookkeeping, and then places it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  private void assertWithTranscriptAndBookkeepingInternal(CycObject sentence, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertSentence(sentence.stringApiValue(), makeELMt(mt), true, true);
  }

  /**
   * Asserts the given sentence with bookkeeping and without placing it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithBookkeepingAndWithoutTranscript(CycList sentence, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithBookkeepingAndWithoutTranscript(sentence.stringApiValue(), mt);
  }

  /**
   * Asserts the given sentence with bookkeeping and without placing it on the transcript queue.
   *
   * @param sentence the given sentence for assertion
   * @param mt the microtheory in which the assertion is placed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertWithBookkeepingAndWithoutTranscript(String sentence, CycObject mt)
          throws UnknownHostException, IOException,
          CycApiException {
    assertSentence(sentence, makeELMt(mt), true, false);
  }

  public void assertSentence(String sentence, ELMt mt, boolean bookkeeping, boolean transcript)
          throws UnknownHostException, IOException, CycApiException {
    assertSentence(sentence, mt, bookkeeping, transcript, null);
  }

  public void assertSentence(String sentence, ELMt mt, boolean bookkeeping, boolean transcript,
          CycFort template)
          throws UnknownHostException, IOException, CycApiException {
    String command = "(multiple-value-list (" + (transcript ? "ke-assert-now" : "cyc-assert") + "\n"
            + sentence + "\n" + mt.stringApiValue() + "))";
    if (bookkeeping) {
      command = wrapBookkeeping(command);
    } else {
      command = wrapCyclistAndPurpose(command);
    }
    if (template != null) {
      String loadRules = "(" + "creation-template-allowable-rules" + " " + template.stringApiValue() + ")";
      command = wrapDynamicBinding(command, "*forward-inference-allowed-rules*", loadRules);
    }
    CycList<Object> results = converseList(command);
    boolean statusOk = !results.get(0).equals(CycObjectFactory.nil);
    if (!statusOk) {
      throw new CycApiException("Assertion failed in mt: " + mt.cyclify()
              + "\n" + sentence + "\nbecause: \n" + results.get(1));
    }
  }

  /**
   * Unasserts the given sentence with bookkeeping and without placing it on the transcript queue.
   *
   * @param sentence the given sentence for unassertion
   * @param mt the microtheory from which the assertion is removed
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void unassertWithBookkeepingAndWithoutTranscript(CycList sentence,
          CycObject mt)
          throws UnknownHostException, IOException,
          CycApiException {
    String command = wrapBookkeeping("(cyc-unassert " + sentence.stringApiValue()
            + makeELMt(mt).stringApiValue() + ")");
    boolean unassertOk = converseBoolean(command);

    if (!unassertOk) {
      throw new CycApiException("Could not unassert from mt: " + makeELMt(
              mt) + "\n  "
              + sentence.cyclify());
    }
  }

  /**
   * Finds a Cyc constant in the KB with the specified name
   *
   * @param constantName the name of the new constant
   *
   * @return the constant term or null if the argument name is null or if the term is not found
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant find(String constantName)
          throws UnknownHostException, IOException, CycApiException {
    if (constantName == null) {
      return null;
    }

    return getConstantByName(constantName);
  }

  /**
   * Finds or creates a Cyc constant in the KB with the specified name.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param constantName the name of the new constant
   *
   * @return the new constant term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant findOrCreate(String constantName)
          throws UnknownHostException, IOException, CycApiException {
    return makeCycConstant(constantName);
  }

  /**
   * Creates a new permanent Cyc constant in the KB with the specified name.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param constantName the name of the new constant
   *
   * @return the new constant term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant createNewPermanent(String constantName)
          throws UnknownHostException, IOException, CycApiException {
    return makeCycConstant(constantName);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycDenotationalTerm predicate,
          CycDenotationalTerm arg1,
          CycDenotationalTerm arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <CycFort>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    assertWithTranscriptAndBookkeeping(sentence,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate, which is a string
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycFort predicate,
          CycFort arg1,
          String arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <String>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    assertWithTranscriptAndBookkeeping(sentence,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate, which is a CycList
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycConstant predicate,
          CycFort arg1,
          CycList arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <List>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    assertWithTranscriptAndBookkeeping(sentence, mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate, which is an int
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycConstant predicate,
          CycFort arg1,
          int arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <int>)
    assertGaf(mt, predicate, arg1, arg2);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate, which is an Integer
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycFort predicate,
          CycFort arg1,
          Integer arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <int>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    assertWithTranscriptAndBookkeeping(sentence,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the binary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate, which is a Double
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycFort predicate,
          CycFort arg1,
          Double arg2)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <int>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    assertWithTranscriptAndBookkeeping(sentence,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT. The operation and its
   * bookkeeping info will be added to the KB transcript for replication and archive.
   *
   * @param mt the microtheory in which the assertion is made
   * @param predicate the ternary predicate of the assertion
   * @param arg1 the first argument of the predicate
   * @param arg2 the second argument of the predicate
   * @param arg3 the third argument of the predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycObject mt,
          CycConstant predicate,
          CycFort arg1,
          CycFort arg2,
          CycFort arg3)
          throws UnknownHostException, IOException, CycApiException {
    // (predicate <CycFort> <CycFort> <CycFort>)
    CycList sentence = new CycList();
    sentence.add(predicate);
    sentence.add(arg1);
    sentence.add(arg2);
    sentence.add(arg3);
    assertWithTranscriptAndBookkeeping(sentence,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param gaf the gaf in the form of a CycList
   * @param mt the microtheory in which the assertion is made
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycList gaf,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscriptAndBookkeeping(gaf,
            mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param gaf the gaf in the form of a CycFormulaSentence
   * @param mt the microtheory in which the assertion is made
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGaf(CycFormulaSentence gaf, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertWithTranscriptAndBookkeepingInternal(gaf, mt);
  }

  /**
   * Asserts a ground atomic formula (gaf) in the specified microtheory MT.  The operation is performed at the HL level
   * and does not perform wff-checking, nor forward inference, nor bookkeeping assertions, nor transcript recording.  The advantage of
   * this method is that it is fast.
   *
   * @param gaf the gaf in the form of a CycList
   * @param mt the microtheory in which the assertion is made
   * @param strength the assertion strength (e.g. :default or :monotonic)
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertHLGaf(final CycList gaf, final CycObject mt, final CycSymbol strength)
          throws UnknownHostException, IOException, CycApiException {
    // (HL-ADD-ARGUMENT '(:ASSERTED-ARGUMENT <strength>) '(NIL ((<gaf>)) <mt> :FORWARD NIL)
    final CycList command = new CycList();
    command.add(makeCycSymbol("hl-add-argument"));
    final CycList command1 = new CycList();
    command1.add(makeCycSymbol(":asserted-argument"));
    command1.add(strength);
    command.addQuoted(command1);
    final CycList command2 = new CycList();
    command2.add(CycObjectFactory.nil);
    final CycList command3 = new CycList();
    final CycList canonicalGaf = new CycList();
    final int gaf_size = gaf.size();
    for (int i = 0; i < gaf_size; i++) {
      final Object obj = gaf.get(i);
      if (obj instanceof CycNart) {
        canonicalGaf.add(((CycNart) obj).getFormula());
      } else {
        canonicalGaf.add(obj);
      }
    }
    command3.add(canonicalGaf);
    command2.add(command3);
    command.addQuoted(command2);
    command.add(mt);
    command.add(makeCycSymbol(":forward"));
    command.add(CycObjectFactory.nil);
    converseCycObject(command);
  }

  /**
   * Unasserts the given ground atomic formula (gaf) in the specified microtheory MT.
   *
   * @param gaf the gaf in the form of a CycList
   * @param mt the microtheory in which the assertion is made
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void unassertGaf(CycList gaf,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = wrapBookkeeping("(ke-unassert-now " + gaf.stringApiValue()
            + makeELMt(mt).stringApiValue() + ")");
    converseVoid(command);
  }

  /**
   * Unasserts the given ground atomic formula (gaf) in the specified microtheory MT.
   *
   * @param gaf the gaf in the form of a CycList
   * @param mt the microtheory in which the assertion is made
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void unassertGaf(CycFormulaSentence gaf, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = wrapBookkeeping("(ke-unassert-now " + gaf.stringApiValue()
            + makeELMt(mt).stringApiValue() + ")");
    converseVoid(command);
  }

  /**
   * Assert a nameString for the specified CycConstant in the specified lexical microtheory. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param cycConstantName the name of the given term
   * @param nameString the given name string for the term
   * @param mtName the name of the microtheory in which the name string is asserted
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertNameString(String cycConstantName,
          String nameString,
          String mtName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(getKnownConstantByName(mtName)),
            getKnownConstantByGuid("c0fdf7e8-9c29-11b1-9dad-c379636f7270"),
            getKnownConstantByName(cycConstantName),
            nameString);
  }

  /**
   * Assert a comment for the specified CycConstant in the specified microtheory MT.  The operation
   * will be added to the KB transcript for replication and archive.
   *
   * @param cycConstantName the name of the given term
   * @param comment the comment string
   * @param mtName the name of the microtheory in which the comment is asserted
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertComment(String cycConstantName,
          String comment,
          String mtName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(getKnownConstantByName(mtName)),
            CycAccess.comment,
            getKnownConstantByName(cycConstantName),
            comment);
  }

  /**
   * Assert a comment for the specified CycFort in the specified microtheory. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param cycFort the given term
   * @param comment the comment string
   * @param mt the comment assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertComment(CycFort cycFort,
          String comment,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    ELMt elmt = makeELMt(mt);
    assertGaf(elmt,
            CycAccess.comment,
            cycFort,
            comment);
  }

  /**
   * Assert a name string for the specified CycFort in the specified microtheory. The operation
   * will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the given term
   * @param nameString the name string
   * @param mt the name string assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertNameString(CycFort cycFort,
          String nameString,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    ELMt elmt = makeELMt(mt);
    assertGaf(elmt,
            this.getKnownConstantByGuid("c0fdf7e8-9c29-11b1-9dad-c379636f7270"),
            cycFort,
            nameString);
  }

  /**
   * Assert a paraphrase format for the specified CycFort in the #$EnglishParaphraseMt. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param relation the given term
   * @param genFormatString the genFormat string
   * @param genFormatList the genFormat argument substitution sequence
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  @Deprecated
  public void assertGenFormat(CycFort relation,
          String genFormatString,
          CycList genFormatList)
          throws UnknownHostException, IOException, CycApiException {
    // (#$genFormat <relation> <genFormatString> <genFormatList>)
    CycList sentence = new CycList();
    sentence.add(getKnownConstantByGuid("beed06de-9c29-11b1-9dad-c379636f7270"));
    sentence.add(relation);
    sentence.add(genFormatString);

    if (genFormatList.size() == 0) {
      sentence.add(CycObjectFactory.nil);
    } else {
      sentence.add(genFormatList);
    }

    assertGaf(sentence,
            // #$EnglishParaphraseMt
            makeELMt(getKnownConstantByGuid("bda16220-9c29-11b1-9dad-c379636f7270")));
  }

  /**
   * Create a microtheory MT, with a comment, isa MT-TYPE and CycFort genlMts. An existing
   * microtheory with the same name is killed first, if it exists.
   *
   * @param mtName the name of the microtheory term
   * @param comment the comment for the new microtheory
   * @param isaMt the type of the new microtheory
   * @param genlMts the list of more general microtheories
   *
   * @return the new microtheory term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant createMicrotheory(String mtName,
          String comment,
          CycFort isaMt,
          List genlMts)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant mt = getConstantByName(mtName);

    if (mt != null) {
      kill(mt);
    }

    mt = createNewPermanent(mtName);
    assertComment(mt,
            comment,
            baseKB);
    assertGaf(universalVocabularyMt,
            isa,
            mt,
            isaMt);

    Iterator iterator = genlMts.iterator();

    while (true) {
      if (!iterator.hasNext()) {
        break;
      }

      CycFort aGenlMt = (CycFort) iterator.next();
      assertGaf(universalVocabularyMt,
              genlMt,
              mt,
              aGenlMt);
    }

    return mt;
  }

  /**
   * Create a microtheory MT, with a comment, isa MT-TYPE and CycFort genlMts.
   *
   * @param mt the microtheory term
   * @param comment the comment for the new microtheory
   * @param isaMt the type of the new microtheory
   * @param genlMts the list of more general microtheories
   *
   * @return the new microtheory term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void createMicrotheory(CycFort mt,
          String comment,
          CycFort isaMt,
          List genlMts)
          throws UnknownHostException, IOException, CycApiException {
    assertComment(mt,
            comment,
            baseKB);
    assertGaf(universalVocabularyMt,
            isa,
            mt,
            isaMt);
    Iterator iterator = genlMts.iterator();
    while (true) {
      if (!iterator.hasNext()) {
        break;
      }
      final CycList gaf = new CycList(3);
      gaf.add(genlMt);
      gaf.add(mt);
      gaf.add(iterator.next());
      assertGaf(gaf, universalVocabularyMt);
    }
  }

  /**
   * Create a microtheory MT, with a comment, isa MT-TYPE and CycFort genlMts. An existing
   * microtheory with the same name is killed first, if it exists.
   *
   * @param mtName the name of the microtheory term
   * @param comment the comment for the new microtheory
   * @param isaMtName the type (as a string) of the new microtheory
   * @param genlMts the list of more general microtheories (as strings)
   *
   * @return the new microtheory term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant createMicrotheory(String mtName,
          String comment,
          String isaMtName,
          List genlMts)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant mt = getConstantByName(mtName);

    if (mt != null) {
      kill(mt);
    }

    mt = createNewPermanent(mtName);
    assertComment(mt,
            comment,
            baseKB);
    assertIsa(mtName,
            isaMtName);

    Iterator iterator = genlMts.iterator();

    while (true) {
      if (!iterator.hasNext()) {
        break;
      }

      String genlMtName = (String) iterator.next();
      assertGenlMt(mtName,
              genlMtName);
    }

    return mt;
  }

  /**
   * Create a microtheory system for a new mt.  Given a root mt name, create a theory ROOTMt,
   * create a vocabulary ROOTVocabMt, and a data ROOTDataMt.  Establish genlMt links for the
   * theory mt and data mt.  Assert that the theory mt is a genlMt of the
   * WorldLikeOursCollectorMt. Assert that the data mt is a genlMt of the collector
   * CurrentWorldDataMt.
   *
   * @param mtRootName the root name of the microtheory system
   * @param comment the root comment of the microtheory system
   * @param genlMts the list of more general microtheories
   *
   * @return an array of three elements consisting of the theory mt, vocabulary mt, and the data mt
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant[] createMicrotheorySystem(String mtRootName,
          String comment,
          List genlMts)
          throws UnknownHostException, IOException, CycApiException {
    //traceOn();
    CycConstant[] mts = {null, null, null};
    String theoryMtName = mtRootName + "Mt";
    String vocabMtName = mtRootName + "VocabMt";
    String vocabMtComment = "The #$VocabularyMicrotheory for #$" + theoryMtName;
    String dataMtName = mtRootName + "DataMt";
    String dataMtComment = "The #$DataMicrotheory for #$" + theoryMtName;
    CycConstant worldLikeOursMt = getKnownConstantByGuid(
            "bf4c781d-9c29-11b1-9dad-c379636f7270");
    CycConstant genlMt_Vocabulary = getKnownConstantByGuid(
            "c054a49e-9c29-11b1-9dad-c379636f7270");

    CycConstant theoryMicrotheory = getKnownConstantByGuid(
            "be5275a8-9c29-11b1-9dad-c379636f7270");
    CycConstant theoryMt = createMicrotheory(theoryMtName,
            comment,
            theoryMicrotheory,
            genlMts);
    CycConstant vocabularyMicrotheory = getKnownConstantByGuid(
            "bda19dfd-9c29-11b1-9dad-c379636f7270");
    CycConstant vocabMt = createMicrotheory(vocabMtName,
            vocabMtComment,
            vocabularyMicrotheory,
            new ArrayList());
    CycConstant dataMicrotheory = getKnownConstantByGuid(
            "be5275a8-9c29-11b1-9dad-c379636f7270");
    CycConstant dataMt = createMicrotheory(dataMtName,
            dataMtComment,
            dataMicrotheory,
            new ArrayList());
    assertGaf(baseKB,
            genlMt_Vocabulary,
            theoryMt,
            vocabMt);
    assertGaf(baseKB,
            genlMt,
            dataMt,
            theoryMt);
    assertGaf(baseKB,
            genlMt,
            worldLikeOursMt,
            theoryMt);
    assertGaf(baseKB,
            genlMt,
            currentWorldDataMt,
            dataMt);
    mts[0] = theoryMt;
    mts[1] = vocabMt;
    mts[2] = dataMt;

    //traceOff();
    return mts;
  }

  /**
   * Assert that the specified CycConstant is a collection in the UniversalVocabularyMt. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the given collection term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsaCollection(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            isa,
            cycFort,
            collection);
  }

  /**
   * Assert that the specified CycConstant is a collection in the specified defining microtheory
   * MT. The operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the given collection term
   * @param mt the assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsaCollection(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    ELMt elmt = makeELMt(mt);
    assertGaf(elmt,
            isa,
            cycFort,
            collection);
  }

  /**
   * Assert that the genlsCollection is a genls of specCollection, in the specified defining
   * microtheory MT. The operation will be added to the KB transcript for replication and archive.
   *
   * @param specCollectionName the name of the more specialized collection
   * @param genlsCollectionName the name of the more generalized collection
   * @param mtName the assertion microtheory name
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenls(String specCollectionName,
          String genlsCollectionName,
          String mtName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(getKnownConstantByName(mtName)),
            genls,
            getKnownConstantByName(specCollectionName),
            getKnownConstantByName(genlsCollectionName));
  }

  /**
   * Assert that the genlsCollection is a genls of specCollection, in the UniversalVocabularyMt The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param specCollectionName the name of the more specialized collection
   * @param genlsCollectionName the name of the more generalized collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenls(String specCollectionName,
          String genlsCollectionName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            genls,
            getKnownConstantByName(specCollectionName),
            getKnownConstantByName(genlsCollectionName));
  }

  /**
   * Assert that the genlsCollection is a genls of specCollection, in the UniveralVocabularyMt. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param specCollection the more specialized collection
   * @param genlsCollection the more generalized collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenls(CycFort specCollection,
          CycFort genlsCollection)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            genls,
            specCollection,
            genlsCollection);
  }

  /**
   * Assert that the genlsCollection is a genls of specCollection, in the specified defining
   * microtheory MT. The operation will be added to the KB transcript for replication and archive.
   *
   * @param specCollection the more specialized collection
   * @param genlsCollection the more generalized collection
   * @param mt the assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenls(CycFort specCollection,
          CycFort genlsCollection,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    ELMt elmt = makeELMt(mt);
    assertGaf(elmt,
            genls,
            specCollection,
            genlsCollection);
  }

  /**
   * Assert that the more general predicate is a genlPreds of the more specialized predicate,
   * asserted in the UniversalVocabularyMt The operation will be added to the KB transcript for
   * replication and archive.
   *
   * @param specPredName the name of the more specialized predicate
   * @param genlPredName the name of the more generalized predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenlPreds(String specPredName,
          String genlPredName)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant genlPreds = getKnownConstantByGuid("bd5b4951-9c29-11b1-9dad-c379636f7270");
    assertGaf(universalVocabularyMt,
            genlPreds,
            getKnownConstantByName(specPredName),
            getKnownConstantByName(genlPredName));
  }

  /**
   * Assert that the more general predicate is a genlPreds of the more specialized predicate,
   * asserted in the UniversalVocabularyMt The operation will be added to the KB transcript for
   * replication and archive.
   *
   * @param specPred the more specialized predicate
   * @param genlPred the more generalized predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenlPreds(CycFort specPred,
          CycFort genlPred)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant genlPreds = getKnownConstantByGuid("bd5b4951-9c29-11b1-9dad-c379636f7270");
    assertGaf(universalVocabularyMt,
            genlPreds,
            specPred,
            genlPred);
  }

  /**
   * Assert that term1 is conceptually related to term2 in the specified microtheory. The operation
   * will be added to the KB transcript for replication and archive.
   *
   * @param term1 the first symbol
   * @param term2 the second symbol
   * @param mt the microtheory in which the assertion is made
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertConceptuallyRelated(CycFort term1,
          CycFort term2,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant conceptuallyRelated = getKnownConstantByGuid(
            "bd58803e-9c29-11b1-9dad-c379636f7270");
    assertGaf(makeELMt(mt),
            conceptuallyRelated,
            term1,
            term2);
  }

  /**
   * Assert that the more general micortheory is a genlMt of the more specialized microtheory,
   * asserted in the UniversalVocabularyMt The operation will be added to the KB transcript for
   * replication and archive.
   *
   * @param specMtName the name of the more specialized microtheory
   * @param genlsMtName the name of the more generalized microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenlMt(String specMtName,
          String genlsMtName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            genlMt,
            getKnownConstantByName(specMtName),
            getKnownConstantByName(genlsMtName));
  }

  /**
   * Assert that the more general micortheory is a genlMt of the more specialized microtheory,
   * asserted in the UniversalVocabularyMt The operation will be added to the KB transcript for
   * replication and archive.
   *
   * @param specMt the more specialized microtheory
   * @param genlsMt the more generalized microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertGenlMt(CycFort specMt,
          CycFort genlsMt)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt, genlMt, specMt, genlsMt);
  }

  /**
   * Assert that the cycFort is a collection in the UniversalVocabularyMt. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param cycFortName the collection element name
   * @param collectionName the collection name
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsa(String cycFortName,
          String collectionName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            isa,
            getKnownConstantByName(cycFortName),
            getKnownConstantByName(collectionName));
  }

  /**
   * Assert that the cycFort is a collection, in the specified defining microtheory MT. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFortName the collection element name
   * @param collectionName the collection name
   * @param mtName the assertion microtheory name
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsa(String cycFortName,
          String collectionName,
          String mtName)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(getKnownConstantByName(mtName)),
            isa,
            getKnownConstantByName(cycFortName),
            getKnownConstantByName(collectionName));
  }

  /**
   * Assert that the cycFort is a collection, in the specified defining microtheory MT. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the collection element
   * @param aCollection the collection
   * @param mt the assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsa(CycFort cycFort,
          CycFort aCollection,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(mt),
            isa,
            cycFort,
            aCollection);
  }

  /**
   * Assert that the cycFort term itself is a collection, in the given mt. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param cycFort the collection element
   * @param aCollection the collection
   * @param mt the assertion microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertQuotedIsa(CycFort cycFort, CycFort aCollection, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(makeELMt(mt),
            getKnownConstantByGuid("055544a2-4371-11d6-8000-00a0c9da2002"),
            cycFort,
            aCollection);
  }

  /**
   * Assert that the cycFort is a collection, in the UniversalVocabularyMt. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param cycFort the collection element
   * @param aCollection the collection
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsa(CycDenotationalTerm instance,
          CycDenotationalTerm aCollection)
          throws UnknownHostException, IOException, CycApiException {
    assertGaf(universalVocabularyMt,
            isa,
            instance,
            aCollection);
  }

  /**
   * Assert that the specified CycConstant is a #$BinaryPredicate in the specified defining
   * microtheory. The operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the given term
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsaBinaryPredicate(CycFort cycFort)
          throws UnknownHostException, IOException, CycApiException {
    assertIsa(cycFort,
            binaryPredicate,
            universalVocabularyMt);
  }

  /**
   * Assert that the specified CycConstant is a #$BinaryPredicate in the specified defining
   * microtheory. The operation will be added to the KB transcript for replication and archive.
   *
   * @param cycFort the given term
   * @param mt the defining microtheory
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertIsaBinaryPredicate(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    assertIsa(cycFort,
            binaryPredicate,
            makeELMt(mt));
  }

  /**
   * Constructs a new CycNaut object by parsing a string.
   *
   * @param string the string in CycL external (EL). For example: (#$MotherFn #$GeorgeWashington)
   *
   * @return the new CycNaut object from parsing the given string
   *
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycNaut makeCycNaut(String string)
          throws CycApiException {
    return new CycNaut(makeCycList(string));
  }

  /**
   * Constructs a new CycFormulaSentence object by parsing a string.
   *
   * @param string the string in CycL external (EL). For example: (#$isa #$Dog #$TameAnimal)
   *
   * @return the new CycFormulaSentence object from parsing the given string
   *
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycFormulaSentence makeCycSentence(String string)
          throws CycApiException {
    return new CycFormulaSentence(makeCycList(string));
  }

  /**
   * Constructs a new CycFormulaSentence object by parsing a string from a string that
   * may or may not include all appropriate "#$"s.
   *
   * @param string the string in CycL external (EL). For example: (#$isa Dog TameAnimal)
   *
   * @return the new CycFormulaSentence object from parsing the given string
   *
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycFormulaSentence makeCyclifiedSentence(String string)
          throws CycApiException, UnknownHostException, IOException {
    String cyclified = cyclifyString(string);
    return makeCycSentence(cyclified);
  }

  /**
   * Constructs a new CycList object by parsing a string.
   *
   * @param string the string in CycL external (EL). For example: (#$isa #$Dog #$TameAnimal)
   *
   * @return the new CycList object from parsing the given string
   *
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList<Object> makeCycList(String string)
          throws CycApiException {
    return (new CycListParser(this)).read(string);
  }

  /**
   * Constructs a new ELMt object by the given CycObject.
   *
   * @param cycObject the given CycObject from which the ELMt is derived
   *
   * @return the new ELMt object by the given CycObject
   *
   * @throws IOException if a communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   * @throws IllegalArgumentException if the cycObject is not the correct type of thing for
   * making into an ELMt
   */
  public ELMt makeELMt(Object object)
          throws IOException, CycApiException {
    if (object instanceof ELMt) {
      return (ELMt) object;
    } else if (object instanceof CycList) {
      return canonicalizeHLMT((CycList) object);
    } else if (object instanceof CycNaut) {
      return canonicalizeHLMT((CycNaut) object);
    } else if (object instanceof CycConstant) {
      return ELMtConstant.makeELMtConstant((CycConstant) object);
    } else if (object instanceof CycNart) {
      return ELMtNart.makeELMtNart((CycNart) object);
    } else if (object instanceof String) {
      String elmtString = object.toString().trim();
      if (elmtString.startsWith("(")) {
        @SuppressWarnings("unchecked")
        CycList<Object> elmtCycList = makeCycList(elmtString);
        return makeELMt(elmtCycList);
      } else {
        return makeELMt(getKnownConstantByName(elmtString));
      }
    } else {
      throw new IllegalArgumentException("Can't make an ELMt from " + object
              + " class: " + object.getClass().getSimpleName());
    }
  }

  /**
   * Returns the canonical Heuristic Level Microtheory (HLMT) given a list  representation.
   *
   * @param cycList the given CycList NART/NAUT representation
   *
   * @return the canonical Heuristic Level Microtheory (HLMT) given a list  representation
   *
   * @throws IOException if a communication error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public ELMt canonicalizeHLMT(CycList cycList)
          throws IOException, CycApiException {
    ELMt mt;
    String command = makeSubLStmt("canonicalize-hlmt", cycList);
    final CycObject result = converseCycObject(command);
    if (result instanceof CycDenotationalTerm) {
      mt = makeELMt(result);
    } else if (result instanceof List) {
      mt = ELMtCycNaut.makeELMtCycNaut((List) result);
    } else {
      throw new CycApiException("Can't canonicalize " + cycList);
    }
    return mt;
  }

  /**
   * Returns the canonical Heuristic Level Microtheory (HLMT) given a list  representation.
   *
   * @param naut the given NAUT representation
   *
   * @return the canonical Heuristic Level Microtheory (HLMT) given a list  representation
   *
   * @throws IOException if a communication error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public ELMt canonicalizeHLMT(CycNaut naut)
          throws IOException, CycApiException {
    return canonicalizeHLMT(naut.toCycList());
  }

  /**
   * Wraps the given api command string with the binding environment for bookkeeping assertions.
   *
   * @param command the given command string
   *
   * @return the given api command string with the binding environment for bookkeeping assertions
   */
  public String wrapBookkeeping(String command) {
    final String projectName = (project == null) ? "nil" : project.stringApiValue();
    final String cyclistName = (cyclist == null) ? "nil" : cyclist.stringApiValue();

    String wrappedCommand = "(with-bookkeeping-info (new-bookkeeping-info " + cyclistName
            + " (the-date) " + projectName + " (the-second))\n"
            + wrapCyclistAndPurpose(command, cyclistName, projectName)
            + "\n)";

    return wrappedCommand;
  }

  public String wrapCyclistAndPurpose(String command) {
    final String projectName = (project == null) ? "nil" : project.stringApiValue();
    final String cyclistName = (cyclist == null) ? "nil" : cyclist.stringApiValue();
    return wrapCyclistAndPurpose(command, cyclistName, projectName);
  }

  public String wrapCyclistAndPurpose(String command, String cyclistName, String projectName) {
    return "(clet ((*require-case-insensitive-name-uniqueness* nil)\n"
            + "       (*the-cyclist* " + cyclistName + ")\n"
            + "       (*ke-purpose* " + projectName + "))\n"
            + "    " + command
            + "\n)";
  }

  public String wrapDynamicBinding(String command, String symbolName, String apiValue) {
    return "(clet ((" + symbolName + " " + apiValue + "))\n"
            + "  " + command + "\n)";
  }

  /**
   * Returns a new <tt>CycConstant</tt> object using the constant name, recording bookkeeping
   * information and archiving to the Cyc transcript.
   *
   * @param name Name of the constant. If prefixed with "#$", then the prefix is removed for
   *        canonical representation.
   *
   * @return a new <tt>CycConstant</tt> object using the constant name, recording bookkeeping
   *         information and archiving to the Cyc transcript
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant makeCycConstant(String name)
          throws UnknownHostException, IOException, CycApiException {
    String constantName = name;

    if (constantName.startsWith("#$")) {
      constantName = constantName.substring(2);
    }

    CycConstant cycConstant = getConstantByName(name);

    if (cycConstant != null) {
      return cycConstant;
    }

    String command = wrapBookkeeping("(ke-create-now \"" + constantName + "\")");
    Object object = converseObject(command);

    if (object instanceof CycConstant) {
      cycConstant = (CycConstant) object;
    } else {
      throw new CycApiException("Cannot create new constant for " + name);
    }
    CycObjectFactory.addCycConstantCache(cycConstant);

    return cycConstant;
  }

  /**
   * Returns a new unique <tt>CycConstant</tt> object using the constant start name prefixed by
   * TMP-, recording bookkeeping information and archiving to the Cyc transcript.  If
   * the start name begins with #$ that portion of the start name is ignored.
   *
   * @param startName the starting name of the constant which will be made unique using a suffix.
   *
   * @return a new <tt>CycConstant</tt> object using the constant starting name, recording
   *         bookkeeping information and archiving to the Cyc transcript
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant makeUniqueCycConstant(final String startName)
          throws UnknownHostException, IOException, CycApiException {
    String constantName = startName;

    if (constantName.startsWith("#$")) {
      constantName = constantName.substring(2);
    }
    String suffix = "";
    int suffixNum = 0;
    while (true) {
      String command = "(constant-name-available \"" + startName + suffix + "\")";
      if (converseBoolean(command)) {
        break;
      }
      if (suffix.length() == 0) {
        suffixNum = ((int) (9 * Math.random())) + 1;
      } else {
        suffixNum = (suffixNum * 10) + ((int) (10 * Math.random()));
      }
      suffix = String.valueOf(suffixNum);
    }
    return makeCycConstant(startName + suffix);
  }

  /**
   * Returns a new unique <tt>CycConstant</tt> object using the constant start name and prefix,
   * recording bookkeeping information and but without archiving to the Cyc transcript. If the
   * start name begins with #$ that portion of the start name is ignored.
   *
   * @param startName the starting name of the constant which will be made unique using a suffix.
   * @param prefix the prefix
   *
   * @return a new <tt>CycConstant</tt> object using the constant starting name, recording
   *         bookkeeping information and archiving to the Cyc transcript
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant makeUniqueCycConstant(String startName,
          String prefix)
          throws UnknownHostException, IOException, CycApiException {
    String constantName = startName;

    if (constantName.startsWith("#$")) {
      constantName = constantName.substring(2);
    }

    String command = wrapBookkeeping("(gentemp-constant \"" + constantName + "\" \"" + prefix
            + "\")");
    CycConstant cycConstant = (CycConstant) converseObject(
            command);
    CycObjectFactory.addCycConstantCache(cycConstant);

    return cycConstant;
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   * @deprecated use <code>executeQuery</code>
   */
  public CycList askNewCycQuery(final CycList query,
          final CycObject mt,
          final InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    final String script =
            "(new-cyc-query " + query.stringApiValue() + " " + makeELMt(mt).stringApiValue() + " " + queryPropertiesToString(queryProperties) + ")";
    return converseList(script);
  }

  /**
   * Run a synchronous query against Cyc.
   *
   * </p><strong>Note: </strong> One should put this call in a try/finally statement
   * and the finally statement should explicitly close the result set. Failure to do so
   * could cause garbage to accumulate on the server.
   *
   * @param query the query in CycList form to ask
   * @param mt the microtheory in which the query should be asked
   * @param queryProperties the query properties to use when asking the query
   * @param timeoutMsecs the amount of time in milliseconds to wait before
   * giving up. A zero for this value means to wait forever.
   * @return the <code>InferenceResultSet</code> which provides a convenient
   * mechanism for walking over results similar to java.sql.ResultSet
   * @throws IOException if a communication error occurs
   * @throws CycApiException if an internal error occurs
   * @throws TimeOutException if the query times out
   */
  public InferenceResultSet executeQuery(final CycList query,
          final ELMt mt, final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws IOException, CycApiException, TimeOutException {
    InferenceWorkerSynch worker =
            new DefaultInferenceWorkerSynch(query, mt, queryProperties, this, timeoutMsecs);
    InferenceResultSet rs = worker.executeQuery();
    return rs;
  }

  /**
   * Run a synchronous query against Cyc.
   *
   * </p><strong>Note: </strong> One should put this call in a try/finally statement
   * and the finally statement should explicitly close the result set. Failure to do so
   * could cause garbage to accumulate on the server.
   *
   * @param query the query in CycFormulaSentence form to ask
   * @param mt the microtheory in which the query should be asked
   * @param queryProperties the query properties to use when asking the query
   * @return the <code>InferenceResultSet</code> which provides a convenient
   * mechanism for walking over results similar to java.sql.ResultSet
   * @throws IOException if a communication error occurs
   * @throws CycApiException if an internal error occurs
   * @throws TimeOutException if the query times out
   */
  public InferenceResultSet executeQuery(final CycFormulaSentence query,
          final ELMt mt, final InferenceParameters queryProperties)
          throws IOException, CycApiException, TimeOutException {
    return executeQuery(query, mt, queryProperties, 0);
  }

  /**
   * Run a synchronous query against Cyc.
   *
   * </p><strong>Note: </strong> One should put this call in a try/finally statement
   * and the finally statement should explicitly close the result set. Failure to do so
   * could cause garbage to accumulate on the server.
   *
   * @param query the query in CycFormulaSentence form to ask
   * @param mt the microtheory in which the query should be asked
   * @param queryProperties the query properties to use when asking the query
   * @param timeoutMsecs the amount of time in milliseconds to wait before
   * giving up. A zero for this value means to wait forever.
   * @return the <code>InferenceResultSet</code> which provides a convenient
   * mechanism for walking over results similar to java.sql.ResultSet
   * @throws IOException if a communication error occurs
   * @throws CycApiException if an internal error occurs
   * @throws TimeOutException if the query times out
   */
  public InferenceResultSet executeQuery(final CycFormulaSentence query,
          final CycObject mt, final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws IOException, CycApiException, TimeOutException {
    InferenceWorkerSynch worker =
            new DefaultInferenceWorkerSynch(query, (ELMt) mt, queryProperties, this, timeoutMsecs);
    InferenceResultSet rs = worker.executeQuery();
    return rs;
  }

  /**
   * Run a synchronous query against Cyc.
   * 
   * </p><strong>Note: </strong> One should put this call in a try/finally statement
   * and the finally statement should explicitly close the result set. Failure to do so
   * could cause garbage to accumulate on the server.
   * 
   * @param query the query in String form to ask
   * @param mt the microtheory in which the query should be asked (as a String, CycObject or ELMt)
   * @param queryProperties the query properties to use when asking the query
   * @param timeoutMsecs the amount of time in milliseconds to wait before 
   * giving up. A zero for this value means to wait forever.
   * @return the <code>InferenceResultSet</code> which provides a convenient 
   * mechanism for walking over results similar to java.sql.ResultSet
   * @throws IOException if a communication error occurs
   * @throws CycApiException if an internal error occurs
   * @throws TimeOutException if the query times out
   */
  public InferenceResultSet executeQuery(final String query,
          final Object mt, final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws IOException, CycApiException, TimeOutException {
    InferenceWorkerSynch worker =
            new DefaultInferenceWorkerSynch(query, makeELMt(mt), queryProperties, this, timeoutMsecs);
    InferenceResultSet rs = worker.executeQuery();
    return rs;
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns an XML stream according
   * to the specifications in the CycList xmlSpec.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values
   * @param xmlSpec the specification of elements, attributes, sort order and bindings for the XML that the method returns
   *
   * @return the binding list from the query in the XML format specified by xmlSpec
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public String queryResultsToXMLString(CycList query,
          CycObject mt,
          InferenceParameters queryProperties,
          CycList xmlSpec)
          throws UnknownHostException, IOException, CycApiException {
    return queryResultsToXMLStringInternal(query, mt, queryProperties, xmlSpec);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns an XML stream according
   * to the specifications in the CycList xmlSpec.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values
   * @param xmlSpec the specification of elements, attributes, sort order and bindings for the XML that the method returns
   *
   * @return the binding list from the query in the XML format specified by xmlSpec
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public String queryResultsToXMLString(CycFormulaSentence query,
          CycObject mt,
          InferenceParameters queryProperties,
          CycList xmlSpec)
          throws UnknownHostException, IOException, CycApiException {
    return queryResultsToXMLStringInternal(query, mt, queryProperties, xmlSpec);

  }

  private String queryResultsToXMLStringInternal(CycObject query,
          CycObject mt,
          InferenceParameters queryProperties,
          CycList xmlSpec)
          throws UnknownHostException, IOException, CycApiException {
    String xmlSpecString = (xmlSpec == null) ? ":default" : xmlSpec.stringApiValue();
    final String script =
            "(query-results-to-xml-string " + query.stringApiValue() + " " + makeELMt(mt).stringApiValue() + " " + queryPropertiesToString(queryProperties) + " " + xmlSpecString + ")";
    return converseString(script);
  }

  /**
   * Returns true if the  Cyc query (with inference parameters) is proven true.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords
   * and values, or null if the defaults are to used
   *
   * @return true if the query is proven true.
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error or if there are open variables in the query
   */
  public boolean isQueryTrue(CycList query, CycObject mt, InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    return isQueryTrue(query, mt, queryProperties, 0);
  }

  /**
   * Returns true if the  Cyc query (with inference parameters) is proven true.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords
   * and values, or null if the defaults are to used
   *
   * @return true if the query is proven true.
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error or if there are open variables in the query
   */
  public boolean isQueryTrue(CycFormulaSentence query, CycObject mt, InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    return isQueryTrue(query, mt, queryProperties, 0);
  }

  /**
   * Returns true if the  Cyc query (with inference parameters) is proven true.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords 
   * and values, or null if the defaults are to used
   * @param timeoutMsecs the time in milliseconds to wait before giving up,
   * set to zero in order to wait forever
   *
   * @return true if the query is proven true.
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error or if there are open variables in the query
   * @throws TimeOutException if the calculation takes too long
   */
  public boolean isQueryTrue(CycList query, CycObject mt, InferenceParameters queryProperties, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    InferenceResultSet rs = executeQuery(query, makeELMt(mt), queryProperties, timeoutMsecs);
    try {
      return rs.getTruthValue();
    } finally {
      rs.close();
    }
  }

  /**
   * Returns true if the  Cyc query (with inference parameters) is proven true.
   *
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords
   * and values, or null if the defaults are to used
   * @param timeoutMsecs the time in milliseconds to wait before giving up,
   * set to zero in order to wait forever
   *
   * @return true if the query is proven true.
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error or if there are open variables in the query
   * @throws TimeOutException if the calculation takes too long
   */
  public boolean isQueryTrue(CycFormulaSentence query, CycObject mt, InferenceParameters queryProperties, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    InferenceResultSet rs = executeQuery(query, makeELMt(mt), queryProperties, timeoutMsecs);
    try {
      return rs.getTruthValue();
    } finally {
      rs.close();
    }
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param variable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final CycList query, final CycObject mt,
          final InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    return queryVariable(queryVariable, query, mt, queryProperties, 0);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param queryVariable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final CycFormulaSentence query, final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return queryVariable(queryVariable, query, mt, new DefaultInferenceParameters(this));
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param queryVariable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final CycFormulaSentence query, final CycObject mt,
          final InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    final InferenceResultSet rs = executeQuery(query, makeELMt(mt), queryProperties);
    return queryVariableLow(queryVariable, rs);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param queryVariable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory (given in String, CycObject or ELMt form)
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final String query, final Object mt,
          final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    final InferenceResultSet rs = executeQuery(query, mt, queryProperties, timeoutMsecs);
    return queryVariableLow(queryVariable, rs);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param variable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   * @param timeoutMsecs the amount of time in milliseconds to wait before giving up, set to
   * zero in order to wait forever.
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   * @throws TimeOutException if the calculation takes too long
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final CycList query, final CycObject mt,
          final InferenceParameters queryProperties, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    InferenceResultSet rs = null;
    try {
      rs = executeQuery(query, makeELMt(mt), queryProperties, timeoutMsecs);
      return queryVariableLow(queryVariable, rs);
    } finally {
      return new CycList<Object>();
    }
  }

  public CycList<Object> queryVariableLow(final CycVariable queryVariable,
          final InferenceResultSet rs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    CycList result = new CycList();
    try {
      if (rs.getCurrentRowCount() == 0) {
        return result;
      }
      int colIndex = rs.findColumn(queryVariable);
      if (colIndex < 0) {
        throw new CycApiException("Unable to find variable: " + queryVariable);
      }
      while (rs.next()) {
        result.add(rs.getObject(colIndex));
      }
      return result;
    } finally {
      if (rs != null) {
        rs.close();
      }
    }
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param variable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   * @param timeoutMsecs the amount of time in milliseconds to wait before giving up, set to
   * zero in order to wait forever.
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   * @throws TimeOutException if the calculation takes too long
   */
  public CycList<Object> queryVariable(final CycVariable queryVariable,
          final CycFormulaSentence query, final CycObject mt,
          final InferenceParameters queryProperties, long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException, TimeOutException {
    InferenceResultSet rs = executeQuery(query, makeELMt(mt), queryProperties, timeoutMsecs);
    return queryVariableLow(queryVariable, rs);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable.
   *
   * @param variable the unbound variable for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to used
   * @param inferenceProblemStoreName the problem store name
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList queryVariable(final CycVariable queryVariable,
          final CycList query,
          final CycObject mt,
          final InferenceParameters queryProperties,
          final String inferenceProblemStoreName)
          throws UnknownHostException, IOException, CycApiException {
    if (queryVariable == null) {
      throw new NullPointerException("queryVariables must not be null");
    }
    if (query == null) {
      throw new NullPointerException("query must not be null");
    }
    if (query.isEmpty()) {
      throw new IllegalArgumentException("query must not be empty");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    if (inferenceProblemStoreName == null) {
      throw new NullPointerException("inferenceProblemStoreName must not be null");
    }
    if (inferenceProblemStoreName.length() == 0) {
      throw new IllegalArgumentException("inferenceProblemStoreName must not be an empty list");
    }
    final InferenceParameters tempQueryProperties = (queryProperties == null) ? getHLQueryProperties() : queryProperties;
    tempQueryProperties.put(makeCycSymbol(":problem-store"), makeCycSymbol("problem-store", false));
    final String script =
            "(clet ((problem-store (find-problem-store-by-name \"" + inferenceProblemStoreName + "\")))"
            + "  (query-variable " + queryVariable.stringApiValue() + " "
            + query.stringApiValue() + " " + makeELMt(mt).stringApiValue() + " " + queryPropertiesToString(tempQueryProperties) + "))";
    return converseList(script);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable list.
   *
   * @param variables the list of unbound variables for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to be used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariables(final CycList<CycVariable> queryVariables,
          final CycList<Object> query,
          final CycObject mt,
          final InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    // @todo use inference property timeout rather than 0 if given
    return queryVariablesLow(queryVariables, query, mt, queryProperties, 0);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable list.
   *
   * @param variables the list of unbound variables for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to be used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList<Object> queryVariables(final CycList<CycVariable> queryVariables,
          final CycFormulaSentence query,
          final CycObject mt,
          final InferenceParameters queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    return queryVariablesLow(queryVariables, query, mt, queryProperties, 0);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable list.
   *
   * @param variables the list of unbound variables for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to be used
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  @SuppressWarnings("unchecked")
  public CycList<Object> queryVariables(final CycList<CycVariable> queryVariables,
          final String query,
          final Object mt,
          final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    final String command = makeSubLStmt("query-template", queryVariables, query, makeELMt(mt), queryProperties);
    SubLWorkerSynch worker = new DefaultSubLWorkerSynch(command, this, timeoutMsecs);
    if (CycObjectFactory.nil.equals(worker.getWork())) {
      return new CycList<Object>();
    }
    return (CycList<Object>) worker.getWork();
  }

  private CycList<Object> queryVariablesLow(final CycList<CycVariable> queryVariables,
          final CycObject query,
          final CycObject mt,
          final InferenceParameters queryProperties,
          final long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    final String command = makeSubLStmt("query-template", queryVariables, query, makeELMt(mt), queryProperties);
    SubLWorkerSynch worker = new DefaultSubLWorkerSynch(command, this, timeoutMsecs);
    if (CycObjectFactory.nil.equals(worker.getWork())) {
      return new CycList<Object>();
    }
    return (CycList<Object>) worker.getWork();
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable list.
   *
   * @param queryVariables the list of unbound variables for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to be used
   * @param inferenceProblemStoreName the problem store name
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList queryVariables(final CycList queryVariables,
          final CycList query,
          final CycObject mt,
          final InferenceParameters queryProperties,
          final String inferenceProblemStoreName)
          throws UnknownHostException, IOException, CycApiException {
    if (query.isEmpty()) {
      throw new IllegalArgumentException("query must not be empty");
    }
    return queryVariablesInternal(queryVariables, query, mt, queryProperties, inferenceProblemStoreName, 0);
  }

  /**
   * Asks a Cyc query (new inference parameters) and returns the binding list for the given variable list.
   *
   * @param queryVariables the list of unbound variables for which bindings are sought
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values, or null if the defaults are to be used
   * @param inferenceProblemStoreName the problem store name
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList queryVariables(final CycList<CycVariable> queryVariables,
          final CycFormulaSentence query,
          final CycObject mt,
          final InferenceParameters queryProperties,
          final String inferenceProblemStoreName)
          throws UnknownHostException, IOException, CycApiException {
    return queryVariablesInternal(queryVariables, query, mt, queryProperties, inferenceProblemStoreName, 0);
  }

  private CycList queryVariablesInternal(final CycList queryVariables,
          final CycObject query,
          final CycObject mt,
          final InferenceParameters queryProperties,
          final String inferenceProblemStoreName,
          final long timeoutMsecs)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (queryVariables == null) {
      throw new NullPointerException("queryVariables must not be null");
    }
    if (queryVariables.isEmpty()) {
      throw new IllegalArgumentException("queryVariables must not be empty");
    }
    if (query == null) {
      throw new NullPointerException("query must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
    }
    if (inferenceProblemStoreName == null) {
      throw new NullPointerException("inferenceProblemStoreName must not be null");
    }
    if (inferenceProblemStoreName.length() == 0) {
      throw new IllegalArgumentException("inferenceProblemStoreName must not be an empty list");
    }
    final InferenceParameters tempQueryProperties = (queryProperties == null) ? getHLQueryProperties() : queryProperties;
    tempQueryProperties.put(makeCycSymbol(":problem-store"), makeCycSymbol("problem-store", false));
    final String script =
            "(clet ((problem-store (find-problem-store-by-name \"" + inferenceProblemStoreName + "\")))"
            + "  (query-template " + queryVariables.stringApiValue() + " "
            + query.stringApiValue() + " " + makeELMt(mt).stringApiValue() + " " + queryPropertiesToString(tempQueryProperties) + "))";
    SubLWorkerSynch worker = new DefaultSubLWorkerSynch(script, this, timeoutMsecs);
    if (CycObjectFactory.nil.equals(worker.getWork())) {
      return new CycList<Object>();
    }
    return (CycList) worker.getWork();
  }

  /**
   * Asks a Cyc query and returns the binding list. Properties:
   * @param query the query expression
   * @param mt the inference microtheory
   * @param maxTransformationDepth the Integer maximum transformation depth or nil for no limit
   * @param maxNumber the Integer maximum number of returned bindings or nil for no limit
   * @param maxTimeSeconds the Integer maximum number of seconds inference duration or nil for no
   *        limit
   * @param maxProofDepth the Integer maximum number of levels in the proof tree or nil for no
   *        limit
   *
   * @return the binding list of answers for the given query and inference property settings
   *
   * @throws IOException if a communication error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList askCycQuery(CycList query,
          CycObject mt,
          Object maxTransformationDepth,
          Object maxNumber,
          Object maxTimeSeconds,
          Object maxProofDepth)
          throws UnknownHostException, IOException, CycApiException {
    HashMap queryProperties = new HashMap();
    queryProperties.put(makeCycSymbol(
            ":max-transformation-depth"),
            maxTransformationDepth);
    queryProperties.put(makeCycSymbol(
            ":max-number"),
            maxNumber);
    queryProperties.put(makeCycSymbol(
            ":max-time"),
            maxTimeSeconds);
    queryProperties.put(makeCycSymbol(
            ":max-proof-depth"),
            maxProofDepth);
    queryProperties.put(makeCycSymbol(
            ":forget-extra-results?"),
            CycObjectFactory.t);

    return askCycQuery(query,
            mt,
            queryProperties);
  }

  /**
   * Asks a Cyc query and returns the binding list.
   * @deprecated use <code>executeQuery</code>
   * @param query the query expression
   * @param mt the inference microtheory
   * @param queryProperties queryProperties the list of query property keywords and values
   *
   * @return the binding list resulting from the given query
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList askCycQuery(CycList query, CycObject mt, HashMap queryProperties)
          throws UnknownHostException, IOException, CycApiException {
    CycList parameterList = new CycList();
    Iterator iter = queryProperties.entrySet().iterator();

    if (iter.hasNext()) {
      while (iter.hasNext()) {
        Entry mapEntry = (Entry) iter.next();
        CycSymbol queryParameterKeyword = (CycSymbol) mapEntry.getKey();
        parameterList.add(queryParameterKeyword);

        Object queryParameterValue = mapEntry.getValue();
        parameterList.add(queryParameterValue);
      }
    }
    String command = makeSubLStmt(CYC_QUERY, query, makeELMt(mt), parameterList);

    return converseList(command);
  }

  /**
   * Returns a list of bindings for a query with a single unbound variable.
   *
   * @param query the query to be asked in the knowledge base
   * @param variable the single unbound variable in the query for which bindings are sought
   * @param mt the microtheory in which the query is asked
   *
   * @return a list of bindings for the query
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList queryVariable(final CycList query,
          final CycVariable variable,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder queryBuffer = new StringBuilder();
    queryBuffer.append("(clet ((*cache-inference-results* nil) ");
    queryBuffer.append("       (*compute-inference-results* nil) ");
    queryBuffer.append("       (*unique-inference-result-bindings* t) ");
    queryBuffer.append("       (*generate-readable-fi-results* nil)) ");
    queryBuffer.append("  (without-wff-semantics ");
    queryBuffer.append("    (ask-template ").append(variable.stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(query.stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(makeELMt(
            mt).stringApiValue()).append(" ");
    queryBuffer.append("                  0 nil nil nil)))");

    CycList answer = converseList(queryBuffer.toString());

    return canonicalizeList(answer);
  }

  /**
   * Returns a list of bindings for a query with a single unbound variable.
   *
   * @deprecated use queryVariable
   * @param query the query to be asked in the knowledge base
   * @param variable the single unbound variable in the query for which bindings are sought
   * @param mt the microtheory in which the query is asked
   *
   * @return a list of bindings for the query
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList askWithVariable(CycList query,
          CycVariable variable,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder queryBuffer = new StringBuilder();
    queryBuffer.append("(clet ((*cache-inference-results* nil) ");
    queryBuffer.append("       (*compute-inference-results* nil) ");
    queryBuffer.append("       (*unique-inference-result-bindings* t) ");
    queryBuffer.append("       (*generate-readable-fi-results* nil)) ");
    queryBuffer.append("  (without-wff-semantics ");
    queryBuffer.append("    (ask-template ").append(variable.stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(query.stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(makeELMt(
            mt).stringApiValue()).append(" ");
    queryBuffer.append("                  0 nil nil nil)))");

    CycList answer = converseList(queryBuffer.toString());

    return canonicalizeList(answer);
  }

  /**
   * Returns a list of bindings for a query with unbound variables.  The bindings each consist of a
   * list in the order of the unbound variables list parameter, in which each bound term is the
   * binding for the corresponding variable.
   *
   * @deprecated use queryVariables
   * @param query the query to be asked in the knowledge base
   * @param variables the list of unbound variables in the query for which bindings are sought
   * @param mt the microtheory in which the query is asked
   *
   * @return a list of bindings for the query
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList askWithVariables(CycList query,
          List variables,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder queryBuffer = new StringBuilder();
    queryBuffer.append("(clet ((*cache-inference-results* nil) ");
    queryBuffer.append("       (*compute-inference-results* nil) ");
    queryBuffer.append("       (*unique-inference-result-bindings* t) ");
    queryBuffer.append("       (*generate-readable-fi-results* nil)) ");
    queryBuffer.append("  (without-wff-semantics ");
    queryBuffer.append("    (ask-template ").append((new CycList(variables)).stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(query.stringApiValue()).append(" ");
    queryBuffer.append("                  ").append(mt.stringApiValue()).append(" ");
    queryBuffer.append("                  0 nil nil nil)))");

    CycList bindings = converseList(queryBuffer.toString());
    CycList canonicalBindings = new CycList();
    Iterator iter = bindings.iterator();

    while (iter.hasNext()) {
      canonicalBindings.add(this.canonicalizeList((CycList) iter.next()));
    }
    return canonicalBindings;
  }

  /**
   * Returns <tt>true</tt> iff the query is true in the knowledge base.
   *
   * @deprecated
   * @param query the query to be asked in the knowledge base
   * @param mt the microtheory in which the query is asked
   *
   * @return <tt>true</tt> iff the query is true in the knowledge base
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isQueryTrue(CycList query, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt(CYC_QUERY, canonicalizeList(query), makeELMt(mt));
    CycList response = converseList(command);

    return response.size() > 0;
  }

  /**
   * Returns <tt>true</tt> iff the query is true in the knowledge base, implements a cache to avoid
   * asking the same question twice from the KB.
   *
   * @deprecated
   * @param query the query to be asked in the knowledge base
   * @param mt the microtheory in which the query is asked
   *
   * @return <tt>true</tt> iff the query is true in the knowledge base
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isQueryTrue_Cached(CycList query,
          CycObject mt)
          throws IOException, CycApiException {
    Boolean isQueryTrue = askCache.get(query);

    if (isQueryTrue != null) {
      return isQueryTrue.booleanValue();
    }

    final boolean answer = isQueryTrue(query, makeELMt(mt));
    askCache.put(query, answer);

    return answer;
  }

  /**
   * Returns the count of the instances of the given collection.
   *
   * @param collection the collection whose instances are counted
   * @param mt microtheory (including its genlMts) in which the count is determined
   *
   * @return the count of the instances of the given collection
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int countAllInstances(CycFort collection,
          CycObject mt)
          throws IOException, CycApiException {
    return this.converseInt("(count-all-instances " + collection.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns the count of the instances of the given collection, implements a cache to avoid asking
   * the same question twice from the KB.
   *
   * @param collection the collection whose instances are counted
   * @param mt microtheory (including its genlMts) in which the count is determined
   *
   * @return the count of the instances of the given collection
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int countAllInstances_Cached(CycFort collection, CycObject mt)
          throws IOException, CycApiException {
    Integer countAllInstances = countAllInstancesCache.get(collection);

    if (countAllInstances != null) {
      return countAllInstances;
    }

    final int answer = countAllInstances(collection, makeELMt(mt));
    countAllInstancesCache.put(collection, answer);

    return answer;
  }

  /**
   * Gets a list of the backchaining implication rules which might apply to the given rule.
   *
   * @param predicate the predicate for which backward chaining implication rules are sought
   * @param formula the literal for which backward chaining implication rules are sought
   * @param mt the microtheory (and its genlMts) in which the search for backchaining implication
   *        rules takes place
   *
   * @return a list of the backchaining implication rules which might apply to the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getBackchainImplicationRules(CycConstant predicate,
          CycList formula, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder command = new StringBuilder();

    if (makeELMt(mt).equals(inferencePSC) || makeELMt(mt).equals(
            everythingPSC)) {
      command.append("(clet (backchain-rules formula) ");
      command.append("  (with-all-mts ");
      command.append("    (do-predicate-rule-index (rule ").
              append(predicate.stringApiValue()).
              append(" :pos nil :backward) ");
      command.append("       (csetq formula (assertion-el-formula rule)) ");
      command.append("       (pwhen (cand (eq (first formula) #$implies) ");
      command.append("                    (unify-el-possible ").
              append(formula.stringApiValue()).
              append(" ");
      command.append("                                          (third formula))) ");
      command.append("         (cpush formula backchain-rules)))) ");
      command.append("   backchain-rules)");
    } else {
      command.append("(clet (backchain-rules formula) ");
      command.append("  (with-inference-mt-relevance ").
              append(makeELMt(mt).stringApiValue()).
              append(" ");
      command.append("    (do-predicate-rule-index (rule ").
              append(predicate.stringApiValue()).
              append(" :pos nil :backward) ");
      command.append("       (csetq formula (assertion-el-formula rule)) ");
      command.append("       (pwhen (cand (eq (first formula) #$implies) ");
      command.append("                    (unify-el-possible ").
              append(formula.stringApiValue()).
              append(" ");
      command.append("                                          (third formula))) ");
      command.append("         (cpush formula backchain-rules)))) ");
      command.append("   backchain-rules)");
    }

    //this.traceOn();
    return converseList(command.toString());
  }

  /**
   * Gets a list of the forward chaining implication rules which might apply to the given rule.
   *
   * @param predicate the predicate for which forward chaining implication rules are sought
   * @param formula the literal for which forward chaining implication rules are sought
   * @param mt the microtheory (and its genlMts) in which the search for forward chaining rules
   *        takes place
   *
   * @return a list of the forward chaining implication rules which might apply to the given
   *         predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getForwardChainRules(CycConstant predicate,
          CycList formula, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder command = new StringBuilder();
    if (makeELMt(mt).equals(inferencePSC) || makeELMt(mt).equals(
            everythingPSC)) {
      command.append("(clet (backchain-rules formula) ");
      command.append("  (with-all-mts ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" :pos nil :forward) ");
      command.append("       (csetq formula (assertion-el-formula rule)) ");
      command.append("       (pwhen (cand (eq (first formula) #$implies) ");
      command.append("                    (unify-el-possible ").append(formula.stringApiValue()).append(" ");
      command.append("                                          (third formula))) ");
      command.append("         (cpush formula backchain-rules)))) ");
      command.append("   backchain-rules)");
    } else {
      command.append("(clet (backchain-rules formula) ");
      command.append("  (with-inference-mt-relevance ").append(makeELMt(mt).stringApiValue()).append(" ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" :pos nil :forward) ");
      command.append("       (csetq formula (assertion-el-formula rule)) ");
      command.append("       (pwhen (cand (eq (first formula) #$implies) ");
      command.append("                    (unify-el-possible ").append(formula.stringApiValue()).append(" ");
      command.append("                                          (third formula))) ");
      command.append("         (cpush formula backchain-rules)))) ");
      command.append("   backchain-rules)");
    }

    return converseList(command.toString());
  }

  /**
   * Gets a list of the backchaining implication rules which might apply to the given predicate.
   *
   * @param predicate the predicate for which backchaining rules are sought
   * @param mt the microtheory (and its genlMts) in which the search for backchaining rules takes
   *        place
   *
   * @return a list of the backchaining implication rules which might apply to the given predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getBackchainRules(CycConstant predicate, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder command = new StringBuilder();
    if (makeELMt(mt).equals(inferencePSC) || makeELMt(mt).equals(
            everythingPSC)) {
      command.append("(clet (backchain-rules) ");
      command.append("  (with-all-mts ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" ");
      command.append("                                :sense :pos ");
      command.append("                                :done nil ");
      command.append("                                :direction :backward) ");
      command.append("       (pwhen (eq (first (assertion-el-formula rule)) #$implies) ");
      command.append("         (cpush (assertion-el-formula rule) backchain-rules)))) ");
      command.append("   backchain-rules)");
    } else {
      command.append("(clet (backchain-rules) ");
      command.append("  (with-inference-mt-relevance ").append(makeELMt(mt).stringApiValue()).append(" ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" ");
      command.append("                                :sense :pos ");
      command.append("                                :done nil ");
      command.append("                                :direction :backward) ");
      command.append("       (pwhen (eq (first (assertion-el-formula rule)) #$implies) ");
      command.append("         (cpush (assertion-el-formula rule) backchain-rules)))) ");
      command.append("   backchain-rules)");
    }

    //this.traceOn();
    return converseList(command.toString());
  }

  /**
   * Gets a list of the forward chaining implication rules which might apply to the given
   * predicate.
   *
   * @param predicate the predicate for which forward chaining rules are sought
   * @param mt the microtheory (and its genlMts) in which the search for forward chaining rules
   *        takes place
   *
   * @return a list of the forward chaining implication rules which might apply to the given
   *         predicate
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getForwardChainRules(CycConstant predicate,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    StringBuilder command = new StringBuilder();

    if (makeELMt(mt).equals(inferencePSC) || makeELMt(mt).equals(
            everythingPSC)) {
      command.append("(clet (backchain-rules) ");
      command.append("  (with-all-mts ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" ");
      command.append("                                :sense :pos ");
      command.append("                                :done nil ");
      command.append("                                :direction :forward) ");
      command.append("       (pwhen (eq (first (assertion-el-formula rule)) #$implies) ");
      command.append("         (cpush (assertion-el-formula rule) backchain-rules)))) ");
      command.append("   backchain-rules)");
    } else {
      command.append("(clet (backchain-rules) ");
      command.append("  (with-inference-mt-relevance ").append(makeELMt(mt).stringApiValue()).append(" ");
      command.append("    (do-predicate-rule-index (rule ").append(predicate.stringApiValue()).append(" ");
      command.append("                                :sense :pos ");
      command.append("                                :done nil ");
      command.append("                                :direction :forward) ");
      command.append("       (pwhen (eq (first (assertion-el-formula rule)) #$implies) ");
      command.append("         (cpush (assertion-el-formula rule) backchain-rules)))) ");
      command.append("   backchain-rules)");
    }

    return converseList(command.toString());
  }

  /**
   * Gets the value of a given KB symbol.  This is intended mainly for test case setup.
   *
   * @param cycSymbol the KB symbol which will have a value bound
   *
   * @return the value assigned to the symbol
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public Object getSymbolValue(CycSymbol cycSymbol)
          throws UnknownHostException, IOException, CycApiException {
    return converseObject("(symbol-value " + cycSymbol.stringApiValue() + ")");
  }

  /**
   * Sets a KB symbol to have the specified value.  This is intended mainly for test case setup. If
   * the symbol does not exist at the KB, then it will be created and assigned the value.
   *
   * @param cycSymbol the KB symbol which will have a value bound
   * @param value the value assigned to the symbol
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void setSymbolValue(CycSymbol cycSymbol, Object value)
          throws UnknownHostException, IOException, CycApiException {
    converseVoid(makeSubLStmt("csetq", new SubLAPIHelper.AsIsTerm(cycSymbol), value));
  }

  /**
   * Returns <tt>true</tt> iff <tt>CycList</tt> represents a well formed formula.
   *
   * @param cycList the candidate well-formed-formula
   *
   * @return true iff cycList represents a well formed formula
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isWellFormedFormula(CycList cycList)
          throws UnknownHostException, IOException, CycApiException {
    return isWellFormedFormulaInternal(cycList);
  }

  /**
   * Returns <tt>true</tt> iff <tt>CycList</tt> represents a well formed formula.
   *
   * @param formula the candidate well-formed-formula
   *
   * @return true iff cycList represents a well formed formula
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isWellFormedFormula(CycFormula formula)
          throws UnknownHostException, IOException, CycApiException {
    return isWellFormedFormulaInternal(formula);
  }

  private boolean isWellFormedFormulaInternal(CycObject cycList)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean(makeSubLStmt(WITH_ALL_MTS, makeNestedSubLStmt(EL_WFF, cycList)));
  }

  /**
   * Returns <tt>true</tt> iff backchain inference on the given predicate is required.
   *
   * @param predicate the <tt>CycConstant</tt> predicate for which backchaining required status is
   *        sought
   * @param mt microtheory (including its genlMts) in which the backchaining required status is
   *        sought
   *
   * @return <tt>true</tt> iff backchain inference on the given predicate is required
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBackchainRequired(CycConstant predicate,
          CycObject mt)
          throws IOException, CycApiException {
    CycConstant backchainRequired = getKnownConstantByGuid(
            "beaa3d29-9c29-11b1-9dad-c379636f7270");

    return hasSomePredicateUsingTerm(backchainRequired,
            predicate, 1, makeELMt(mt));
  }

  /**
   * Returns <tt>true</tt> iff backchain inference on the given predicate is encouraged.
   *
   * @param predicate the <tt>CycConstant</tt> predicate for which backchaining encouraged status
   *        is sought
   * @param mt microtheory (including its genlMts) in which the backchaining encouraged status is
   *        sought
   *
   * @return <tt>true</tt> iff backchain inference on the given predicate is encouraged
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBackchainEncouraged(CycConstant predicate,
          CycObject mt)
          throws IOException, CycApiException {
    CycConstant backchainEncouraged = getKnownConstantByGuid(
            "c09d1cea-9c29-11b1-9dad-c379636f7270");

    return hasSomePredicateUsingTerm(backchainEncouraged,
            predicate, 1, makeELMt(mt));
  }

  /**
   * Returns <tt>true</tt> iff backchain inference on the given predicate is discouraged.
   *
   * @param predicate the <tt>CycConstant</tt> predicate for which backchaining discouraged status
   *        is sought
   * @param mt microtheory (including its genlMts) in which the backchaining discouraged status is
   *        sought
   *
   * @return <tt>true</tt> iff backchain inference on the given predicate is discouraged
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBackchainDiscouraged(CycConstant predicate,
          CycObject mt)
          throws IOException, CycApiException {
    CycConstant backchainDiscouraged = getKnownConstantByGuid(
            "bfcbce14-9c29-11b1-9dad-c379636f7270");

    return hasSomePredicateUsingTerm(backchainDiscouraged,
            predicate, 1, makeELMt(mt));
  }

  /**
   * Returns <tt>true</tt> iff backchain inference on the given predicate is forbidden.
   *
   * @param predicate the <tt>CycConstant</tt> predicate for which backchaining forbidden status is
   *        sought
   * @param mt microtheory (including its genlMts) in which the backchaining forbidden status is
   *        sought
   *
   * @return <tt>true</tt> iff backchain inference on the given predicate is forbidden
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isBackchainForbidden(CycConstant predicate,
          CycObject mt)
          throws IOException, CycApiException {
    CycConstant backchainForbidden = getKnownConstantByGuid(
            "bfa4e9d2-9c29-11b1-9dad-c379636f7270");

    return hasSomePredicateUsingTerm(backchainForbidden,
            predicate, 1, makeELMt(mt));
  }

  /**
   * Returns <tt>true</tt> iff the predicate has the irreflexive property: (#$isa ?PRED
   * #$IrreflexsiveBinaryPredicate).
   *
   * @param predicate the <tt>CycConstant</tt> predicate for which irreflexive status is sought
   * @param mt microtheory (including its genlMts) in which the irreflexive status is sought
   *
   * @return <tt>true</tt> iff the predicate has the irreflexive property: (#$isa ?PRED
   *         #$IrreflexsiveBinaryPredicate)
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isIrreflexivePredicate(CycConstant predicate,
          CycObject mt)
          throws IOException, CycApiException {
    CycConstant irreflexiveBinaryPredicate = getKnownConstantByGuid(
            "bd654be7-9c29-11b1-9dad-c379636f7270");

    return this.isa(predicate,
            irreflexiveBinaryPredicate,
            makeELMt(mt));
  }

  /**
   * Returns <tt>true</tt> iff any ground formula instances exist having the given predicate, and
   * the given term in the given argument position.
   *
   * @param term the term present at the given argument position
   * @param predicate the <tt>CycConstant</tt> predicate for the formula
   * @param argumentPosition the argument position of the given term in the ground formula
   * @param mt microtheory (including its genlMts) in which the existence is sought
   *
   * @return <tt>true</tt> iff any ground formula instances exist having the given predicate, and
   *         the given term in the given argument position
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean hasSomePredicateUsingTerm(CycConstant predicate,
          CycFort term,
          Integer argumentPosition,
          CycObject mt)
          throws IOException, CycApiException {
    String command;
    if (makeELMt(mt).equals(inferencePSC) || makeELMt(mt).equals(everythingPSC)) {
      command = makeSubLStmt(SOME_PRED_VALUE_IN_ANY_MT, term, predicate, argumentPosition);
    } else {
      command = makeSubLStmt(SOME_PRED_VALUE_IN_RELEVANT_MTS, term, predicate,
              makeELMt(mt), argumentPosition);
    }

    //this.traceOn();
    return converseBoolean(command);
  }

  /**
   * Returns the count of the assertions indexed according to the given pattern, using the best
   * index (from among the predicate and argument indices).  The formula can contain variables.
   *
   * @param formula the formula whose indexed instances are counted
   * @param mt microtheory (including its genlMts) in which the count is determined
   *
   * @return the count of the assertions indexed according to the given pattern, using the best
   *         index (from among the predicate and argument indices)
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int countUsingBestIndex(CycList formula,
          CycObject mt)
          throws IOException, CycApiException {
    String command = makeSubLStmt("with-inference-mt-relevance", makeELMt(mt),
            makeNestedSubLStmt("best-index-count", formula, CycObjectFactory.t, CycObjectFactory.t));
    return converseInt(command);
  }

  /**
   * Imports a MUC (Message Understanding Conference) formatted symbolic expression into cyc via
   * the function which parses the expression and creates assertions for the contained concepts
   * and relations between them.
   *
   * @param mucExpression the MUC (Message Understanding Conference) formatted symbolic expression
   * @param mtName the name of the microtheory in which the imported assertions will be made
   *
   * @return the number of assertions imported from the input MUC expression
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int importMucExpression(CycList mucExpression,
          String mtName)
          throws IOException, CycApiException {
    String command = makeSubLStmt("convert-netowl-sexpr-to-cycl-assertions",
            mucExpression, mtName);
    return converseInt(command);
  }

  /**
   * Returns a list of parsing expressions, each consisting of a parsing span expression, and a
   * list of parsed terms.
   * <pre>
   * (RKF-PHRASE-READER "penguins" #$RKFEnglishLexicalMicrotheoryPSC #$InferencePSC)
   * ==>
   * (((0) (#$Penguin #$PittsburghPenguins)))
   * </pre>
   *
   * @param text the phrase to be parsed
   * @param parsingMt the microtheory in which lexical info is asked
   * @param domainMt the microtherory in which the info about candidate terms is asked
   *
   * @return a parsing expression consisting of a parsing span expression, and a list of parsed
   *         terms
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList rkfPhraseReader(String text, String parsingMt, String domainMt)
          throws IOException, CycApiException {
    return rkfPhraseReader(text,
            getKnownConstantByName(parsingMt),
            getKnownConstantByName(domainMt));
  }

  /**
   * Returns a list of parsing expressions, each consisting of a parsing span expression, and a
   * list of parsed terms.
   * <pre>
   * (RKF-PHRASE-READER "penguins" #$RKFEnglishLexicalMicrotheoryPSC #$InferencePSC)
   * ==>
   * (((0) (#$Penguin #$PittsburghPenguins)))
   * </pre>
   *
   * @param text the phrase to be parsed
   * @param parsingMt the microtheory in which lexical info is asked
   * @param domainMt the microtherory in which the info about candidate terms is asked
   *
   * @return a parsing expression consisting of a parsing span expression, and a list of parsed
   *         terms
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList rkfPhraseReader(String text,
          CycFort parsingMt,
          CycFort domainMt)
          throws IOException, CycApiException {
    String command = makeSubLStmt("rkf-phrase-reader", text, parsingMt, domainMt);
    return converseList(command);
  }

  /**
   * Returns a list of disambiguation expressions, corresponding to each of the terms in the given
   * list of objects.
   * <pre>
   * (GENERATE-DISAMBIGUATION-PHRASES-AND-TYPES (QUOTE (#$Penguin #$PittsburghPenguins)))
   * ==>
   * ((#$Penguin "penguin" #$Bird "bird")
   *  (#$PittsburghPenguins "the Pittsburgh Penguins" #$IceHockeyTeam "ice hockey team"))
   * </pre>
   *
   * @param objects the list of terms to be disambiguated
   *
   * @return a list of disambiguation expressions, corresponding to each of the terms in the given
   *         list of objects
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList generateDisambiguationPhraseAndTypes(CycList objects)
          throws IOException, CycApiException {
    String command = makeSubLStmt(
            makeCycSymbol("generate-disambiguation-phrases-and-types"), objects);
    return converseList(command);
  }

  public CycList phraseStructureParse(String str)
          throws IOException, CycApiException {
    String command = makeSubLStmt(
            makeCycSymbol("ps-get-cycls-for-phrase"), str);
    return converseList(command);
  }

  /**
   * Returns the arity of the given predicate.
   *
   * @param predicate the given predicate whose number of arguments is sought
   *
   * @return the arity of the given predicate, or zero if the argument is not a predicate
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public int getArity(CycFort predicate)
          throws IOException, CycApiException {
    String command = makeSubLStmt(WITH_ALL_MTS,
            makeNestedSubLStmt("arity", predicate));
    Object object = this.converseObject(command);
    if (object instanceof Integer) {
      return (Integer) object;
    } else {
      return 0;
    }
  }

  /**
   * Returns the list of arg2 values of binary gafs, given the predicate and arg1, looking in all
   * microtheories.
   *
   * @param predicate the given predicate for the gaf pattern
   * @param arg1 the given first argument of the gaf
   *
   * @return the list of arg2 values of the binary gafs
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getArg2s(CycFort predicate,
          Object arg1)
          throws IOException, CycApiException {

    CycVariable variable = CycObjectFactory.makeCycVariable("?arg2");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(predicate, arg1, variable);

    return queryVariable(variable, query, inferencePSC);
  }

  /**
   * Returns the single (first) arg2 value of a binary gaf, given the predicate and arg0, looking
   * in all microtheories.  Return null if none found.
   *
   * @param predicate the given predicate for the gaf pattern
   * @param arg1 the given first argument of the gaf
   *
   * @return the single (first) arg2 value of the binary gaf(s)
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public Object getArg2(CycFort predicate,
          Object arg1)
          throws IOException, CycApiException {
    CycList arg2s = getArg2s(predicate, arg1);
    if (arg2s.isEmpty()) {
      return null;
    } else {
      return arg2s.first();
    }
  }

  /**
   * Returns true if formula is well-formed in the relevant mt.
   *
   * @param formula the given EL formula
   * @param mt the relevant mt
   *
   * @return true if formula is well-formed in the relevant mt, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isFormulaWellFormed(CycList formula,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return isFormulaWellFormedInternal(formula, mt);
  }

  /**
   * Returns true if formula is well-formed in the relevant mt.
   *
   * @param formula the given EL formula
   * @param mt the relevant mt
   *
   * @return true if formula is well-formed in the relevant mt, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isFormulaWellFormed(CycFormula formula,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return isFormulaWellFormedInternal(formula, mt);
  }

  private boolean isFormulaWellFormedInternal(CycObject formula,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(el-formula-ok? " + formula.stringApiValue() + " "
            + makeELMt(mt).stringApiValue() + ")");
  }

  /**
   * Returns true if formula is well-formed Non Atomic Reifable Term.
   *
   * @param formula the given EL formula
   *
   * @return true if formula is well-formed Non Atomic Reifable Term, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCycLNonAtomicReifableTerm(CycList formula)
          throws UnknownHostException, IOException, CycApiException {
    return isCycLNonAtomicReifableTermInternal(formula);
  }

  /**
   * Returns true if formula is well-formed Non Atomic Reifable Term.
   *
   * @param formula the given EL formula
   *
   * @return true if formula is well-formed Non Atomic Reifable Term, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCycLNonAtomicReifableTerm(CycFormula formula)
          throws UnknownHostException, IOException, CycApiException {
    return isCycLNonAtomicReifableTermInternal(formula);
  }

  private boolean isCycLNonAtomicReifableTermInternal(CycObject formula)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(cycl-nart-p " + formula.stringApiValue() + ")");
  }

  /**
   * Returns true if formula is well-formed Non Atomic Un-reifable Term.
   *
   * @param formula the given EL formula
   *
   * @return true if formula is well-formed Non Atomic Un-reifable Term, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCycLNonAtomicUnreifableTerm(CycList formula)
          throws UnknownHostException, IOException, CycApiException {
    return isCycLNonAtomicUnreifableTermInternal(formula);
  }

  /**
   * Returns true if formula is well-formed Non Atomic Un-reifable Term.
   *
   * @param formula the given EL formula
   *
   * @return true if formula is well-formed Non Atomic Un-reifable Term, otherwise false
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isCycLNonAtomicUnreifableTerm(CycFormula formula)
          throws UnknownHostException, IOException, CycApiException {
    return isCycLNonAtomicUnreifableTermInternal(formula);
  }

  private boolean isCycLNonAtomicUnreifableTermInternal(CycObject formula)
          throws UnknownHostException, IOException, CycApiException {
    return converseBoolean("(cycl-naut-p " + formula.stringApiValue() + ")");
  }

  /**
   * Creates a new Collector microtheory and links it more general mts.
   *
   * @param mtName the name of the new collector microtheory
   * @param comment the comment for the new collector microtheory
   * @param genlMts the list of more general microtheories
   *
   * @return the new microtheory
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createCollectorMt(String mtName,
          String comment,
          List genlMts)
          throws IOException, CycApiException {
    CycConstant collectorMt = getKnownConstantByName("CollectorMicrotheory");

    return createMicrotheory(mtName,
            comment,
            collectorMt,
            genlMts);
  }

  /**
   * Asserts each of the given list of forts to be instances of the given collection in the
   * UniversalVocabularyMt
   *
   * @param fortNames the list of forts
   * @param collectionName
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertIsas(List fortNames,
          String collectionName)
          throws IOException, CycApiException {
    List forts = new ArrayList();

    for (int i = 0; i < forts.size(); i++) {
      Object fort = forts.get(i);

      if (fort instanceof String) {
        forts.add(getKnownConstantByName((String) fort));
      } else if (fort instanceof CycFort) {
        forts.add(fort);
      } else {
        throw new CycApiException(fort + " is neither String nor CycFort");
      }

      assertIsas(forts,
              getKnownConstantByName(collectionName));
    }
  }

  /**
   * Asserts each of the given list of forts to be instances of the given collection in the
   * UniversalVocabularyMt
   *
   * @param forts the list of forts
   * @param collection
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertIsas(List forts,
          CycFort collection)
          throws IOException, CycApiException {
    for (int i = 0; i < forts.size(); i++) {
      assertIsa((CycFort) forts.get(i),
              collection);
    }
  }

  /**
   * Creates a new spindle microtheory in the given spindle system.
   *
   * @param spindleMtName the name of the new spindle microtheory
   * @param comment the comment for the new spindle microtheory
   * @param spindleHeadMtName the name of the spindle head microtheory
   * @param spindleCollectorMtName the name of the spindle head microtheory
   *
   * @return the new spindle microtheory in the given spindle system
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createSpindleMt(String spindleMtName,
          String comment,
          String spindleHeadMtName,
          String spindleCollectorMtName)
          throws IOException, CycApiException {
    return createSpindleMt(spindleMtName,
            comment,
            getKnownConstantByName(spindleHeadMtName),
            getKnownConstantByName(spindleCollectorMtName));
  }

  /**
   * Creates a new spindle microtheory in the given spindle system.
   *
   * @param spindleMtName the name of the new spindle microtheory
   * @param comment the comment for the new spindle microtheory
   * @param spindleHeadMt the spindle head microtheory
   * @param spindleCollectorMt the spindle head microtheory
   *
   * @return the new spindle microtheory in the given spindle system
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createSpindleMt(String spindleMtName,
          String comment,
          CycFort spindleHeadMt,
          CycFort spindleCollectorMt)
          throws IOException, CycApiException {
    CycConstant spindleMt = getKnownConstantByName("SpindleMicrotheory");
    List genlMts = new ArrayList();
    genlMts.add(spindleHeadMt);

    CycConstant mt = this.createMicrotheory(spindleMtName,
            comment,
            spindleMt,
            genlMts);
    assertGaf(universalVocabularyMt,
            genlMt,
            spindleCollectorMt,
            mt);

    return mt;
  }

  /**
   * Creates a new binary predicate term.
   *
   * @param predicateName the name of the new binary predicate
   * @param predicateTypeName the type of binary predicate, for example
   *        #$TransitiveBinaryPredicate, which when null defaults to #$BinaryPredicate
   * @param comment the comment for the new binary predicate, or null
   * @param arg1IsaName the argument position one type constraint, or null
   * @param arg2IsaName the argument position two type constraint, or null
   * @param arg1FormatName the argument position one format constraint, or null
   * @param arg2FormatName the argument position two format constraint, or null
   * @param genlPredsName the more general binary predicate of which this new predicate is a
   *        specialization, that when null defaults to #$conceptuallyRelated
   * @param genFormatList the paraphrase generation list string, or null
   * @param genFormatString the paraphrase generation string, or null
   *
   * @return the new binary predicate term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createBinaryPredicate(String predicateName,
          String predicateTypeName,
          String comment,
          String arg1IsaName,
          String arg2IsaName,
          String arg1FormatName,
          String arg2FormatName,
          String genlPredsName,
          String genFormatString,
          String genFormatList)
          throws IOException, CycApiException {
    return createBinaryPredicate(predicateName,
            find(predicateTypeName),
            comment,
            find(arg1IsaName),
            find(arg2IsaName),
            find(arg1FormatName),
            find(arg2FormatName),
            find(genlPredsName),
            genFormatString,
            makeCycList(genFormatList));
  }

  /**
   * Creates a new binary predicate term.
   *
   * @param predicateName the name of the new binary predicate
   * @param predicateType the type of binary predicate, for example #$TransitiveBinaryPredicate,
   *        which when null defaults to #$BinaryPredicate
   * @param comment the comment for the new binary predicate, or null
   * @param arg1Isa the argument position one type constraint, or null
   * @param arg2Isa the argument position two type constraint, or null
   * @param arg1Format the argument position one format constraint, or null
   * @param arg2Format the argument position two format constraint, or null
   * @param genlPreds the more general binary predicate of which this new predicate is a
   *        specialization, that when null defaults to #$conceptuallyRelated
   * @param genFormatList the paraphrase generation list string, or null
   * @param genFormatString the paraphrase generation string, or null
   *
   * @return the new binary predicate term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createBinaryPredicate(String predicateName,
          CycFort predicateType,
          String comment,
          CycFort arg1Isa,
          CycFort arg2Isa,
          CycFort arg1Format,
          CycFort arg2Format,
          CycFort genlPreds,
          String genFormatString,
          CycList genFormatList)
          throws IOException, CycApiException {
    CycConstant predicate = findOrCreate(predicateName);

    if (predicateType == null) {
      assertIsa(predicate,
              binaryPredicate);
    } else {
      assertIsa(predicate,
              predicateType);
    }

    if (comment != null) {
      assertComment(predicate,
              comment,
              baseKB);
    }

    if (arg1Isa != null) {
      assertArgIsa(predicate,
              1,
              arg1Isa);
    }

    if (arg2Isa != null) {
      assertArgIsa(predicate,
              2,
              arg2Isa);
    }

    if (arg1Format != null) {
      assertArgFormat(predicate,
              1,
              arg1Format);
    }

    if (arg2Format != null) {
      assertArgFormat(predicate,
              2,
              arg2Format);
    }

    if (genlPreds == null) {
      assertGenlPreds(predicate,
              // #$conceptuallyRelated
              getKnownConstantByGuid("bd58803e-9c29-11b1-9dad-c379636f7270"));
    } else {
      assertGenlPreds(predicate,
              genlPreds);
    }

    if ((genFormatString != null) && (genFormatList != null)) {
      assertGenFormat(predicate,
              genFormatString,
              genFormatList);
    }

    return predicate;
  }

  /**
   * Creates a new KB subset collection term.
   *
   * @param constantName the name of the new KB subset collection
   * @param comment the comment for the new KB subset collection
   *
   * @return the new KB subset collection term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createKbSubsetCollection(String constantName,
          String comment)
          throws IOException, CycApiException {
    CycConstant kbSubsetCollection = getKnownConstantByName(
            "KBSubsetCollection");
    CycConstant cycConstant = getConstantByName(constantName);

    if (cycConstant == null) {
      cycConstant = createNewPermanent(constantName);
    }

    assertIsa(cycConstant,
            kbSubsetCollection);
    assertComment(cycConstant,
            comment,
            baseKB);
    assertGenls(cycConstant,
            thing);

    CycFort variableOrderCollection = getKnownConstantByGuid(
            "36cf85d0-20a1-11d6-8000-0050dab92c2f");
    assertIsa(cycConstant,
            variableOrderCollection,
            baseKB);

    return cycConstant;
  }

  /**
   * Creates a new collection term.
   *
   * @param collectionName the name of the new collection
   * @param comment the comment for the collection
   * @param commentMtName the name of the microtheory in which the comment is asserted
   * @param isaName the name of the collection of which the new collection is an instance
   * @param genlsName the name of the collection of which the new collection is a subset
   *
   * @return the new collection term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant createCollection(String collectionName,
          String comment,
          String commentMtName,
          String isaName,
          String genlsName)
          throws IOException, CycApiException {
    CycConstant cycCollection = findOrCreate(collectionName);
    assertComment(cycCollection,
            comment,
            getKnownConstantByName(commentMtName));
    assertIsa(cycCollection,
            getKnownConstantByName(isaName));
    assertGenls(cycCollection,
            getKnownConstantByName(genlsName));

    return cycCollection;
  }

  /**
   * Creates a new collection term.
   *
   * @param collectionName the name of the new collection
   * @param comment the comment for the collection
   * @param commentMt the microtheory in which the comment is asserted
   * @param isa the collection of which the new collection is an instance
   * @param genls the collection of which the new collection is a subset
   *
   * @return the new collection term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollection(String collectionName,
          String comment,
          CycFort commentMt,
          CycFort isa,
          CycFort genls)
          throws IOException, CycApiException {
    return createCollection(findOrCreate(collectionName),
            comment,
            commentMt,
            isa,
            genls);
  }

  /**
   * Creates a new collection term.
   *
   * @param collection the new collection
   * @param comment the comment for the collection
   * @param commentMt the microtheory in which the comment is asserted
   * @param isa the collection of which the new collection is an instance
   * @param genls the collection of which the new collection is a subset
   *
   * @return the new collection term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollection(CycFort collection,
          String comment,
          CycFort commentMt,
          CycFort isa,
          CycFort genls)
          throws IOException, CycApiException {
    assertComment(collection,
            comment,
            commentMt);
    assertIsa(collection,
            isa);
    assertGenls(collection,
            genls);

    return collection;
  }

  /**
   * Creates a new individual term.
   *
   * @param IndividualName the name of the new individual term
   * @param comment the comment for the individual
   * @param commentMt the microtheory in which the comment is asserted
   * @param isa the collection of which the new individual is an instance
   *
   * @return the new individual term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createIndividual(String IndividualName,
          String comment,
          String commentMt,
          String isa)
          throws IOException, CycApiException {
    return createIndividual(IndividualName,
            comment,
            getKnownConstantByName(commentMt),
            getKnownConstantByName(isa));
  }

  /**
   * Creates a new individual term.
   *
   * @param IndividualName the name of the new individual term
   * @param comment the comment for the individual
   * @param commentMt the microtheory in which the comment is asserted
   * @param isa the collection of which the new individual is an instance
   *
   * @return the new individual term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createIndividual(String IndividualName,
          String comment,
          CycDenotationalTerm commentMt,
          CycDenotationalTerm isa)
          throws IOException, CycApiException {
    CycFort individual = findOrCreate(IndividualName);
    assertComment(individual,
            comment,
            commentMt);
    assertIsa(individual,
            isa);

    return individual;
  }

  /**
   * Creates a new individual-denoting reifiable unary function term.
   *
   * @param unaryFunction the new collection
   * @param comment the comment for the unary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1Isa the kind of objects this unary function takes as its argument
   * @param resultIsa the kind of object represented by this reified term
   *
   * @return the new individual-denoting reifiable unary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createIndivDenotingUnaryFunction(String unaryFunction,
          String comment,
          String commentMt,
          String arg1Isa,
          String resultIsa)
          throws IOException, CycApiException {
    return createIndivDenotingUnaryFunction(findOrCreate(
            unaryFunction),
            comment,
            getKnownConstantByName(
            commentMt),
            getKnownConstantByName(
            arg1Isa),
            getKnownConstantByName(
            resultIsa));
  }

  /**
   * Creates a new individual-denoting reifiable unary function term.
   *
   * @param unaryFunction the new collection
   * @param comment the comment for the unary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1Isa the kind of objects this unary function takes as its argument
   * @param resultIsa the kind of object represented by this reified term
   *
   * @return the new individual-denoting reifiable unary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createIndivDenotingUnaryFunction(CycFort unaryFunction,
          String comment,
          CycFort commentMt,
          CycFort arg1Isa,
          CycFort resultIsa)
          throws IOException, CycApiException {
    assertComment(unaryFunction,
            comment,
            commentMt);


    // (#$isa unaryFunction #$UnaryFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd58af89-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$ReifiableFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd588002-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$IndividualDenotingFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd58fad9-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$Function-Denotational)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd5c40b0-9c29-11b1-9dad-c379636f7270"));
    assertArgIsa(unaryFunction,
            1,
            arg1Isa);
    assertResultIsa(unaryFunction,
            resultIsa);

    return unaryFunction;
  }

  /**
   * Creates a new collection-denoting reifiable unary function term.
   *
   * @param unaryFunction the new collection
   * @param comment the comment for the unary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1Isa the isa type constraint for the argument
   * @param arg1GenlName the genls type constraint for the argument if it is a collection
   * @param resultIsa the isa object represented by this reified term
   * @param resultGenlName the genls object represented by this reified term
   *
   * @return the new collection-denoting reifiable unary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollectionDenotingUnaryFunction(String unaryFunction,
          String comment,
          String commentMt,
          String arg1Isa,
          String arg1GenlName,
          String resultIsa,
          String resultGenlName)
          throws IOException, CycApiException {
    CycFort arg1Genl;

    if (arg1GenlName != null) {
      arg1Genl = getKnownConstantByName(arg1GenlName);
    } else {
      arg1Genl = null;
    }

    CycFort resultGenl;

    if (resultGenlName != null) {
      resultGenl = getKnownConstantByName(resultGenlName);
    } else {
      resultGenl = null;
    }

    return createCollectionDenotingUnaryFunction(findOrCreate(
            unaryFunction),
            comment,
            getKnownConstantByName(
            commentMt),
            getKnownConstantByName(
            arg1Isa),
            arg1Genl,
            getKnownConstantByName(
            resultIsa),
            resultGenl);
  }

  /**
   * Creates a new collection-denoting reifiable unary function term.
   *
   * @param unaryFunction the new collection
   * @param comment the comment for the unary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1Isa the isa type constraint for the argument
   * @param arg1Genl the genls type constraint for the argument if it is a collection
   * @param resultIsa the isa object represented by this reified term
   * @param resultGenl the genls object represented by this reified term
   *
   * @return the new collection-denoting reifiable unary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollectionDenotingUnaryFunction(CycFort unaryFunction,
          String comment,
          CycFort commentMt,
          CycFort arg1Isa,
          CycFort arg1Genl,
          CycFort resultIsa,
          CycFort resultGenl)
          throws IOException, CycApiException {
    assertComment(unaryFunction,
            comment,
            commentMt);


    // (#$isa unaryFunction #$UnaryFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd58af89-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$ReifiableFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd588002-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$CollectionDenotingFunction)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd58806a-9c29-11b1-9dad-c379636f7270"));


    // (#$isa unaryFunction #$Function-Denotational)
    assertIsa(unaryFunction,
            getKnownConstantByGuid("bd5c40b0-9c29-11b1-9dad-c379636f7270"));
    assertArgIsa(unaryFunction,
            1,
            arg1Isa);

    if (arg1Genl != null) {
      assertArg1Genl(unaryFunction,
              arg1Genl);
    }

    assertResultIsa(unaryFunction,
            resultIsa);

    if (resultGenl != null) {
      assertResultGenl(unaryFunction,
              resultGenl);
    }

    return unaryFunction;
  }

  /**
   * Creates a new collection-denoting reifiable binary function term.
   *
   * @param binaryFunction the new collection
   * @param comment the comment for the binary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1IsaName the collection of which the new binary function is an instance
   * @param arg2GenlsName the kind of objects this binary function takes as its first argument, or
   *        null
   * @param arg2IsaName the kind of objects this binary function takes as its second argument, or
   *        null
   * @param arg1GenlsName the general collections this binary function takes as its first argument,
   *        or null
   * @param resultIsa the kind of object represented by this reified term
   *
   * @return the new collection-denoting reifiable binary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollectionDenotingBinaryFunction(String binaryFunction,
          String comment,
          String commentMt,
          String arg1IsaName,
          String arg2IsaName,
          String arg1GenlsName,
          String arg2GenlsName,
          String resultIsa)
          throws IOException, CycApiException {
    CycFort arg1Isa = null;
    CycFort arg2Isa = null;
    CycFort arg1Genls = null;
    CycFort arg2Genls = null;

    if (arg1IsaName != null) {
      arg1Isa = getKnownConstantByName(arg1IsaName);
    }

    if (arg2IsaName != null) {
      arg1Isa = getKnownConstantByName(arg2IsaName);
    }

    if (arg1GenlsName != null) {
      arg1Genls = getKnownConstantByName(arg1GenlsName);
    }

    if (arg2GenlsName != null) {
      arg2Genls = getKnownConstantByName(arg2GenlsName);
    }

    return createCollectionDenotingBinaryFunction(findOrCreate(
            binaryFunction),
            comment,
            getKnownConstantByName(
            commentMt),
            arg1Isa,
            arg2Isa,
            arg1Genls,
            arg2Genls,
            getKnownConstantByName(
            resultIsa));
  }

  /**
   * Creates a new collection-denoting reifiable binary function term.
   *
   * @param binaryFunction the new collection
   * @param comment the comment for the binary function
   * @param commentMt the microtheory in which the comment is asserted
   * @param arg1Isa the kind of objects this binary function takes as its first argument, or null
   * @param arg2Isa the kind of objects this binary function takes as its first argument, or null
   * @param arg1Genls the general collections this binary function takes as its first argument, or
   *        null
   * @param arg2Genls the general collections this binary function takes as its second argument, or
   *        null
   * @param resultIsa the kind of object represented by this reified term
   *
   * @return the new collection-denoting reifiable binary function term
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort createCollectionDenotingBinaryFunction(CycFort binaryFunction,
          String comment,
          CycFort commentMt,
          CycFort arg1Isa,
          CycFort arg2Isa,
          CycFort arg1Genls,
          CycFort arg2Genls,
          CycFort resultIsa)
          throws IOException, CycApiException {
    assertComment(binaryFunction,
            comment,
            commentMt);


    // (#$isa binaryFunction #$BinaryFunction)
    assertIsa(binaryFunction,
            getKnownConstantByGuid("c0e7247c-9c29-11b1-9dad-c379636f7270"));


    // (#$isa binaryFunction #$ReifiableFunction)
    assertIsa(binaryFunction,
            getKnownConstantByGuid("bd588002-9c29-11b1-9dad-c379636f7270"));


    // (#$isa binaryFunction #$CollectionDenotingFunction)
    assertIsa(binaryFunction,
            getKnownConstantByGuid("bd58806a-9c29-11b1-9dad-c379636f7270"));


    // (#$isa binaryFunction #$Function-Denotational)
    assertIsa(binaryFunction,
            getKnownConstantByGuid("bd5c40b0-9c29-11b1-9dad-c379636f7270"));

    if (arg1Isa != null) {
      assertArgIsa(binaryFunction,
              1,
              arg1Isa);
    }

    if (arg2Isa != null) {
      assertArgIsa(binaryFunction,
              2,
              arg2Isa);
    }

    if (arg1Genls != null) {
      assertArg1Genl(binaryFunction,
              arg1Genls);
    }

    if (arg2Genls != null) {
      assertArg2Genl(binaryFunction,
              arg2Genls);
    }

    assertResultIsa(binaryFunction,
            resultIsa);

    return binaryFunction;
  }

  /**
   * Assert an argument isa contraint for the given relation and argument position. The operation
   * will be added to the KB transcript for replication and archive.
   *
   * @param relation the given relation
   * @param argPosition the given argument position
   * @param argNIsa the argument constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArgIsa(CycFort relation,
          int argPosition,
          CycFort argNIsa)
          throws UnknownHostException, IOException, CycApiException {
    // (#$argIsa relation argPosition argNIsa)
    CycList sentence = new CycList();
    sentence.add(getKnownConstantByGuid("bee22d3d-9c29-11b1-9dad-c379636f7270"));
    sentence.add(relation);
    sentence.add(argPosition);
    sentence.add(argNIsa);
    assertGaf(sentence, universalVocabularyMt);
  }

  /**
   * Assert an argument one genls contraint for the given relation. The operation will be added to
   * the KB transcript for replication and archive.
   *
   * @param relation the given relation
   * @param argGenl the argument constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArg1Genl(CycFort relation,
          CycFort argGenl)
          throws UnknownHostException, IOException, CycApiException {
    // (#$arg1Genl relation argGenl)
    CycList sentence = new CycList();
    sentence.add(getKnownConstantByGuid("bd588b1d-9c29-11b1-9dad-c379636f7270"));
    sentence.add(relation);
    sentence.add(argGenl);
    assertGaf(sentence,
            universalVocabularyMt);
  }

  /**
   * Assert an argument two genls contraint for the given relation. The operation will be added to
   * the KB transcript for replication and archive.
   *
   * @param relation the given relation
   * @param argGenl the argument constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArg2Genl(CycFort relation,
          CycFort argGenl)
          throws UnknownHostException, IOException, CycApiException {
    // (#$arg2Genl relation argGenl)
    CycList sentence = new CycList();
    sentence.add(getKnownConstantByGuid("bd58dcda-9c29-11b1-9dad-c379636f7270"));
    sentence.add(relation);
    sentence.add(argGenl);
    assertGaf(sentence,
            universalVocabularyMt);
  }

  /**
   * Assert an argument three genls contraint for the given relation. The operation will be added
   * to the KB transcript for replication and archive.
   *
   * @param relation the given relation
   * @param argGenl the argument constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArg3Genl(CycFort relation,
          CycFort argGenl)
          throws UnknownHostException, IOException, CycApiException {
    // (#$arg3Genl relation argGenl)
    CycList sentence = new CycList();
    sentence.add(getKnownConstantByGuid("bd58b8c3-9c29-11b1-9dad-c379636f7270"));
    sentence.add(relation);
    sentence.add(argGenl);
    assertGaf(sentence,
            universalVocabularyMt);
  }

  /**
   * Assert the isa result contraint for the given denotational function. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param denotationalFunction the given denotational function
   * @param resultIsa the function's isa result constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertResultIsa(CycFort denotationalFunction,
          CycFort resultIsa)
          throws UnknownHostException, IOException, CycApiException {
    // (#$resultIsa denotationalFunction resultIsa)
    assertGaf(universalVocabularyMt,
            getKnownConstantByGuid("bd5880f1-9c29-11b1-9dad-c379636f7270"),
            denotationalFunction,
            resultIsa);
  }

  /**
   * Assert the genls result contraint for the given denotational function. The operation will be
   * added to the KB transcript for replication and archive.
   *
   * @param denotationalFunction the given denotational function
   * @param resultGenl the function's genls result constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertResultGenl(CycFort denotationalFunction,
          CycFort resultGenl)
          throws UnknownHostException, IOException, CycApiException {
    // (#$resultGenl denotationalFunction resultGenls)
    assertGaf(universalVocabularyMt,
            getKnownConstantByGuid("bd58d6ab-9c29-11b1-9dad-c379636f7270"),
            denotationalFunction,
            resultGenl);
  }

  /**
   * Returns true if this KB is OpenCyc.
   *
   * @return true if this KB is OpenCyc, otherwise false
   *
   * @throws IOException if a data communication error occurs
   * @throws UnknownHostException if cyc server host not found on the network
   */
  public boolean isOpenCyc()
          throws UnknownHostException, IOException {
    boolean answer;

    try {
      answer = converseBoolean("(cyc-opencyc-feature)");
    } catch (CycApiException e) {
      answer = false;
    }

    return answer;
  }

  /**
   * Imports an OWL ontology.
   *
   * @param uri the URI of the ontology
   * @param prefix the prefix to append to names of new constants created for terms in the ontology
   *
   * @return the CycFort representing the imported ontology
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort importOwlOntology(final String uri, final String prefix)
          throws IOException, CycApiException {
    final String sourceFile = uri;
    return importOwlOntology(uri, prefix, sourceFile);
  }

  /**
   * Imports an OWL ontology.
   *
   * @param uri the URI of the ontology
   * @param prefix the prefix to append to names of new constants created for terms in the ontology
   * @param sourceFile the path for the file from which to import the ontology
   *
   * @return the CycFort representing the imported ontology
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort importOwlOntology(final String uri, final String prefix, final String sourceFile)
          throws IOException, CycApiException {
    final CycFort ontology = null;
    return importOwlOntology(uri, prefix, sourceFile, ontology);
  }

  /**
   * Imports an OWL ontology.
   *
   * @param uri the URI of the ontology
   * @param prefix the prefix to append to names of new constants created for terms in the ontology
   * @param sourceFile the path for the file from which to import the ontology, if not from the URI
   * @param ontology the existing CycFort representing the ontology, if we are to update an already-imported ontology
   *
   * @return the CycFort representing the imported ontology
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort importOwlOntology(final String uri, final String prefix,
          final String sourceFile, final CycFort ontology)
          throws IOException, CycApiException {
    final CycFort quotedIsa = null;
    return importOwlOntology(uri, prefix, sourceFile, ontology, quotedIsa);
  }

  /**
   * Imports an OWL ontology.
   *
   * @param uri the URI of the ontology
   * @param prefix the prefix to append to names of new constants created for terms in the ontology
   * @param sourceFile the path for the file from which to import the ontology, if not from the URI
   * @param ontology the existing CycFort representing the ontology, if we are to update an already-imported ontology
   * @param quotedIsa the collection of which newly created terms should be quoted instances. If null, Cyc will create one.
   *
   * @return the CycFort representing the imported ontology
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort importOwlOntology(final String uri, final String prefix,
          final String sourceFile, final CycFort ontology, final CycFort quotedIsa)
          throws IOException, CycApiException {
    final CycFort currentCyclist = getCyclist();
    return importOwlOntology(uri, prefix, sourceFile, ontology, quotedIsa, currentCyclist);
  }

  /**
   * Imports an OWL ontology.
   *
   * @param uri the URI of the ontology
   * @param prefix the prefix to append to names of new constants created for terms in the ontology
   * @param sourceFile the path for the file from which to import the ontology, if not from the URI
   * @param ontology the existing CycFort representing the ontology, if we are to update an already-imported ontology
   * @param quotedIsa the collection of which newly created terms should be quoted instances. If null, Cyc will create one.
   * @param cyclist the cyclist to attribute the new terms and assertions to
   *
   * @return the CycFort representing the imported ontology
   *
   * @throws IOException if a communications error occurs
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycFort importOwlOntology(final String uri, final String prefix,
          final String sourceFile, final CycFort ontology, final CycFort quotedIsa,
          final CycFort cyclist)
          throws IOException, CycApiException {
    final String sourceFileString = ((sourceFile != null) && (sourceFile.length() > 0)) ? "\"" + sourceFile + "\"" : "nil";
    final String ontologyString = (ontology == null) ? "nil" : CycFort.stringApiValue(ontology);
    final String quotedIsaString = (quotedIsa == null) ? "nil" : CycFort.stringApiValue(quotedIsa);
    final String cyclistString = (cyclist == null) ? "nil" : CycFort.stringApiValue(cyclist);
    final Object object = converseObject("(import-owl-ontology " + "\"" + uri + "\" " + "\"" + prefix + "\" " + sourceFileString + " " + ontologyString + " " + quotedIsaString + " " + cyclistString + ")");
    if (object instanceof CycFort) {
      return (CycFort) object;
    } else {
      return null;
    }
  }

  /**
   * Returns a constant whose name differs from the given name only by case. Used because Cyc by
   * default requires constant names to be unique by case.
   *
   * @param name the name used to lookup an existing constant
   *
   * @return a constant whose name differs from the given name only by case, otherwise null if none
   *         exists
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycConstant constantNameCaseCollision(String name)
          throws UnknownHostException, IOException, CycApiException {
    Object object = converseObject("(constant-name-case-collision \"" + name + "\")");

    if (object instanceof CycConstant) {
      return (CycConstant) object;
    } else {
      return null;
    }
  }

  /**
   * Returns the list of applicable binary predicates which are elements of any of the given list
   * of KBSubsetCollections.
   *
   * @param kbSubsetCollections the list of KBSubsetCollections
   *
   * @return the list of applicable binary predicates which are elements of any of the given list
   *         of KBSubsetCollections
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getApplicableBinaryPredicates(CycList kbSubsetCollections)
          throws UnknownHostException, IOException, CycApiException {
    CycList result = new CycList();

    for (int i = 0; i < kbSubsetCollections.size(); i++) {
      CycFort kbSubsetCollection = (CycFort) kbSubsetCollections.get(
              i);
      String query = "(#$and \n" + "  (#$isa ?binary-predicate " + kbSubsetCollection.stringApiValue()
              + ") \n" + "  (#$isa ?binary-predicate #$BinaryPredicate))";
      result.addAllNew(queryVariable(CycObjectFactory.makeCycVariable("?binary-predicate"),
              makeCycSentence(query), inferencePSC, new DefaultInferenceParameters(this)));
    }

    return result;
  }

  /**
   * Returns the list of gafs in which the predicate is a element of the given list of predicates
   * and in which the given term appears in the first argument position.
   *
   * @param cycObject the given term
   * @param predicates the given list of predicates
   * @param mt the relevant inference microtheory
   *
   * @return the list of gafs in which the predicate is a element of the given list of predicates
   *         and in which the given term appears in the first argument position
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getGafsForPredicates(final CycObject cycObject,
          final List predicates,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (predicates == null) {
      throw new NullPointerException("predicates must not be null");
    }
    if (predicates == null) {
      throw new NullPointerException("predicates must not be null");
    }
    final CycList result = new CycList();

    for (int i = 0; i < predicates.size(); i++) {
      result.addAllNew(getGafs(cycObject,
              (CycFort) predicates.get(
              i),
              makeELMt(mt)));
    }

    return result;
  }

  /**
   * Returns the list of gafs in which the predicate is the given predicate and in which the given
   * term appears in the first argument position.
   *
   * @param cycFort the given term
   * @param predicate the given predicate
   * @param mt the relevant inference microtheory
   *
   * @return the list of gafs in which the predicate is a element of the given list of predicates
   *         and in which the given term appears in the first argument position
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getGafs(final CycObject cycFort,
          final CycObject predicate,
          final CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    if (cycFort == null) {
      throw new NullPointerException("cycFort must not be null");
    }
    if (predicate == null) {
      throw new NullPointerException("predicate must not be null");
    }
    if (mt == null) {
      throw new NullPointerException("mt must not be null");
      // TODO handle the case where the cycObject is a NAUT,
      //getGafsForNaut
    }
    final CycList gafs = new CycList();
    final String command = "(with-inference-mt-relevance " + makeELMt(mt).stringApiValue() + "\n"
            + "  (pred-values-in-relevant-mts " + cycFort.stringApiValue() + " "
            + predicate.stringApiValue() + "))";
    final CycList values = converseList(command);

    for (int i = 0; i < values.size(); i++) {
      final CycList gaf = new CycList();
      gaf.add(predicate);
      gaf.add(cycFort);
      gaf.add(values.get(i));
      gafs.add(gaf);
    }

    return gafs;
  }

  /**
   * Returns the list of gafs in which the predicate is a element of the given list of predicates
   * and in which the given term appears in the first argument position.
   *
   * @param cycObject the given term
   * @param predicates the given list of predicates
   *
   * @return the list of gafs in which the predicate is a element of the given list of predicates
   *         and in which the given term appears in the first argument position
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getGafsForPredicates(final CycObject cycObject,
          final List predicates)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (predicates == null) {
      throw new NullPointerException("predicates must not be null");
    }
    final CycList result = new CycList();
    for (int i = 0; i < predicates.size(); i++) {
      result.addAllNew(getGafs(cycObject, (CycObject) predicates.get(i)));
    }

    return result;
  }

  /**
   * Returns the list of gafs in which the predicate is the given predicate and in which the given
   * term appears in the first argument position.
   *
   * @param cycObject the given term
   * @param predicate the given predicate
   *
   * @return the list of gafs in which the predicate is a element of the given list of predicates
   *         and in which the given term appears in the first argument position
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getGafs(final CycObject cycObject,
          final CycObject predicate)
          throws UnknownHostException, IOException, CycApiException {
    //// Preconditions
    if (cycObject == null) {
      throw new NullPointerException("cycObject must not be null");
    }
    verifyPossibleDenotationalTerm(cycObject);
    if (cycObject instanceof CycList) {
      return getGafsForNaut((CycList) cycObject, predicate);
    }
    final CycList gafs = new CycList();
    // TODO handle the case where the cycObject is a NAUT,
    //getGafsForNaut
    final String command = "(with-all-mts \n" + "  (pred-values-in-relevant-mts (canonicalize-term "
            + cycObject.stringApiValue() + ") " + "(canonicalize-term "
            + predicate.stringApiValue() + ")))";
    final CycList values = converseList(command);

    for (int i = 0; i < values.size(); i++) {
      final CycList gaf = new CycList();
      gaf.add(predicate);
      gaf.add(cycObject);
      gaf.add(values.get(i));
      gafs.add(gaf);
    }

    return gafs;
  }

  /**
   * Returns the list of gafs in which the predicate is the given predicate and in which the given
   * term appears in the first argument position.
   *
   * @param cycObject the given term
   * @param predicate the given predicate
   *
   * @return the list of gafs in which the predicate is a element of the given list of predicates
   *         and in which the given term appears in the first argument position
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getGafsForNaut(final CycList naut,
          final CycObject predicate)
          throws UnknownHostException, IOException, CycApiException {
    final String command =
            "(clet (assertions) "
            + "  (do-gaf-arg-index-naut (assertion " + naut.stringApiValue() + ")"
            + "    (pwhen (equal (formula-arg1 assertion) " + predicate.stringApiValue() + ")"
            + "      (cpush assertion assertions))) "
            + "  assertions)";
    final CycList gafs = converseList(command);

    //// Postconditions
    assert gafs != null : "gafs cannot be null";

    return gafs;
  }

  /**
   * Returns the list of tuples gathered from assertions in given microtheory in which the
   * predicate is the given predicate, in which the given term appears in the indexArg position
   * and in which the list of gatherArgs determines the assertion arguments returned as each
   * tuple.
   *
   * @param term the term in the index argument position
   * @param predicate the given predicate
   * @param indexArg the argument position in which the given term appears
   * @param gatherArgs the list of argument Integer positions which indicate the assertion
   *        arguments to be returned as each tuple
   * @param mt the relevant inference microtheory
   *
   * @return the list of tuples gathered from assertions in given microtheory in which the
   *         predicate is the given predicate, in which the given term appears in the indexArg
   *         position and in which the list of gatherArgs determines the assertion arguments
   *         returned as each tuple
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getPredicateValueTuplesInMt(CycFort term, CycFort predicate,
          int indexArg, CycList gatherArgs, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList tuples = new CycList();
    String command = makeSubLStmt("pred-value-tuples-in-mt", term, predicate, indexArg,
            gatherArgs, makeELMt(mt));
    return converseList(command);
  }

  /**
   * Assert an argument contraint for the given relation and argument position. The operation will
   * be added to the KB transcript for replication and archive.
   *
   * @param relation the given relation
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArg1FormatSingleEntry(CycFort relation)
          throws UnknownHostException, IOException, CycApiException {
    // (#$arg1Format relation SingleEntry)
    assertArgFormat(relation, 1, getKnownConstantByGuid("bd5880eb-9c29-11b1-9dad-c379636f7270"));
  }

  /**
   * Assert an argument format contraint for the given relation and argument position. The
   * operation will be added to the KB transcript for replication and archive.
   *
   * @param relation the given relation
   * @param argPosition the given argument position
   * @param argNFormat the argument format constraint
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public void assertArgFormat(CycFort relation, int argPosition, CycFort argNFormat)
          throws UnknownHostException, IOException, CycApiException {
    // (#$argFormat relation argPosition argNFormat)
    CycFormulaSentence sentence = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByGuid("bd8a36e1-9c29-11b1-9dad-c379636f7270"), relation,
            argPosition, argNFormat);
    assertGaf(sentence, baseKB);
  }

  /**
   * Asserts that the given DAML imported term is mapped to the given Cyc term.
   *
   * @param cycTerm the mapped Cyc term
   * @param informationSource the external indexed information source
   * @param externalConcept the external concept within the information source
   * @param mt the assertion microtheory
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertSynonymousExternalConcept(String cycTerm,
          String informationSource,
          String externalConcept,
          String mt)
          throws UnknownHostException, IOException, CycApiException {
    assertSynonymousExternalConcept(getKnownConstantByName(cycTerm),
            getKnownConstantByName(informationSource),
            externalConcept,
            getKnownConstantByName(mt));
  }

  /**
   * Asserts that the given DAML imported term is mapped to the given Cyc term.
   *
   * @param cycTerm the mapped Cyc term
   * @param informationSource the external indexed information source
   * @param externalConcept the external concept within the information source
   * @param mt the assertion microtheory
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertSynonymousExternalConcept(CycFort cycTerm,
          CycFort informationSource,
          String externalConcept,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycFormulaSentence gaf = CycFormulaSentence.makeCycFormulaSentence(
            // #$synonymousExternalConcept
            getKnownConstantByGuid("c0e2af4e-9c29-11b1-9dad-c379636f7270"), cycTerm, informationSource, externalConcept);
    assertGaf(gaf, makeELMt(mt));
  }

  /**
   * Gets the list of mappings from the specified information source given the inference
   * microtheory.  Each returned list item is the pair consisting of external concept string and
   * synonymous Cyc term.
   *
   * @param informationSource the external indexed information source
   * @param mt the assertion microtheory
   *
   * @return list of mappings from the specified information source given the inference
   * microtheory
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getSynonymousExternalConcepts(String informationSource, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getSynonymousExternalConcepts(getKnownConstantByName(informationSource),
            getKnownConstantByName(mt));
  }

  /**
   * Gets the list of mappings from the specified information source given the inference
   * microtheory.  Each returned list item is the pair consisting of external concept string and
   * synonymous Cyc term.
   *
   * @param informationSource the external indexed information source
   * @param mt the assertion microtheory
   *
   * @return the list of mappings from the specified information source given the inference
   * microtheory
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getSynonymousExternalConcepts(CycFort informationSource, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList variables = new CycList();
    CycVariable cycTermVar = CycObjectFactory.makeCycVariable("?cyc-term");
    variables.add(cycTermVar);

    CycVariable externalConceptVar = CycObjectFactory.makeCycVariable("?externalConcept");
    variables.add(externalConceptVar);

    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            // #$synonymousExternalConcept
            getKnownConstantByGuid("c0e2af4e-9c29-11b1-9dad-c379636f7270"),
            cycTermVar, informationSource, externalConceptVar);

    return queryVariables(variables, query, makeELMt(mt), new DefaultInferenceParameters(this));
  }

  /**
   * Asserts a preferred name string for the given term using lexical singular count noun
   * assumptions.
   *
   * @param cycTerm the Cyc term
   * @param phrase the preferred phrase for this term
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertGenPhraseCountNounSingular(CycFort cycTerm,
          String phrase)
          throws UnknownHostException, IOException, CycApiException {
    // (#$genPhrase <term> #$CountNoun #$singular <phrase>) in #$EnglishParaphaseMt
    final CycFormulaSentence gaf = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByGuid("bd5fb28e-9c29-11b1-9dad-c379636f7270"),
            cycTerm,
            getKnownConstantByGuid("bd588078-9c29-11b1-9dad-c379636f7270"),
            getKnownConstantByGuid("bd6757b8-9c29-11b1-9dad-c379636f7270"),
            phrase);

    ELMt elmt = makeELMt(getKnownConstantByGuid("bda16220-9c29-11b1-9dad-c379636f7270"));
    assertGaf(gaf, elmt);
  }

  /**
   * Asserts a preferred name string for the given term using lexical singular count noun
   * assumptions.
   *
   * @param cycTerm the Cyc term
   * @param phrase the preferred phrase for this term
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void assertGenPhraseCountNounPlural(CycFort cycTerm,
          String phrase)
          throws UnknownHostException, IOException, CycApiException {
    CycList gaf = new CycList();


    // (#$genPhrase <term> #$CountNoun #$plural <phrase>) in
    // #$EnglishParaphaseMt
    gaf.add(getKnownConstantByGuid("bd5fb28e-9c29-11b1-9dad-c379636f7270"));
    gaf.add(cycTerm);
    gaf.add(getKnownConstantByGuid("bd588078-9c29-11b1-9dad-c379636f7270"));
    gaf.add(getKnownConstantByGuid("bd5a6853-9c29-11b1-9dad-c379636f7270"));
    gaf.add(phrase);

    ELMt elmt = makeELMt(getKnownConstantByGuid("bda16220-9c29-11b1-9dad-c379636f7270"));
    assertGaf(gaf,
            elmt);
  }

  /**
   * Gets the list of name strings for the given CycFort.
   *
   * @param cycFort the given FORT
   * @param mt the relevant inference microtheory
   *
   * @return the list of name strings for the given CycFort
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public CycList getNameStrings(CycFort cycFort,
          CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    // (#$nameString <cycFort> ?name-string)

    CycVariable variable = CycObjectFactory.makeCycVariable("?name-string");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            getKnownConstantByGuid("c0fdf7e8-9c29-11b1-9dad-c379636f7270"), cycFort, variable);

    return queryVariable(variable, query, makeELMt(mt));
  }

  /**
   * Ensures that the given term meets the given isa and genl wff constraints in the
   * UniversalVocabularyMt.
   *
   * @param cycFort the given term
   * @param isaConstraintName the given isa type constraint, or null
   * @param genlsConstraintName the given genls type constraint, or null
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void ensureWffConstraints(String cycFort,
          String isaConstraintName,
          String genlsConstraintName)
          throws UnknownHostException, IOException, CycApiException {
    CycConstant cycConstant = find(cycFort);
    CycConstant isaConstraint = null;
    CycConstant genlsConstraint = null;

    if (isaConstraintName != null) {
      isaConstraint = find(isaConstraintName);
    }

    if (genlsConstraintName != null) {
      genlsConstraint = find(genlsConstraintName);
    }

    ensureWffConstraints(cycConstant, isaConstraint, genlsConstraint);
  }

  /**
   * Ensures that the given term meets the given isa and genl wff constraints in the
   * UniversalVocabularyMt.
   *
   * @param cycFort the given term
   * @param isaConstraint the given isa type constraint, or null
   * @param genlsConstraint the given genls type constraint, or null
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void ensureWffConstraints(CycFort cycFort, CycFort isaConstraint, CycFort genlsConstraint)
          throws UnknownHostException, IOException, CycApiException {
    if ((isaConstraint != null)
            && (!isa(cycFort, isaConstraint, universalVocabularyMt))) {
      assertIsa(cycFort, isaConstraint);
    }

    if ((genlsConstraint != null)
            && (!isSpecOf(cycFort, genlsConstraint, universalVocabularyMt))) {
      assertGenls(cycFort, genlsConstraint);
    }
  }

  /**
   * Returns the list of arg2 terms from binary gafs having the specified predicate and arg1
   * values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the list of arg2 terms from gafs having the specified predicate and arg1 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getArg2s(String predicate, String arg1, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg2s(getKnownConstantByName(predicate),
            getKnownConstantByName(arg1),
            getKnownConstantByName(mt));
  }

  /**
   * Returns the list of arg2 terms from binary gafs having the specified predicate and arg1
   * values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the list of arg2 terms from gafs having the specified predicate and arg1 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getArg2s(String predicate, CycFort arg1, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg2s(getKnownConstantByName(predicate), arg1, makeELMt(mt));
  }

  /**
   * Returns the list of arg2 terms from binary gafs having the specified predicate and arg1
   * values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the list of arg2 terms from gafs having the specified predicate and arg1 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getArg2s(CycFort predicate, CycFort arg1, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {

    CycVariable variable = CycObjectFactory.makeCycVariable("?arg2");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            predicate, arg1, variable);

    return queryVariable(variable, query, makeELMt(mt));
  }

  /**
   * Returns the first arg2 term from binary gafs having the specified predicate and arg1 values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the first arg2 term from gafs having the specified predicate and arg1 values or null
   *         if none
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getArg2(String predicate, String arg1, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg2(getKnownConstantByName(predicate),
            getKnownConstantByName(arg1),
            getKnownConstantByName(mt));
  }

  /**
   * Returns the first arg2 term from binary gafs having the specified predicate and arg1 values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the first arg2 term from gafs having the specified predicate and arg1 values or null
   *         if none
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getArg2(String predicate, CycFort arg1, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg2(getKnownConstantByName(predicate), arg1, makeELMt(mt));
  }

  /**
   * Returns the first arg2 term from binary gafs having the specified predicate and arg1 values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the first arg2 term from gafs having the specified predicate and arg1 values or null
   *         if none
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getArg2(CycFort predicate, CycFort arg1, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {

    CycVariable variable = CycObjectFactory.makeCycVariable("?arg2");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            predicate, arg1, variable);
    final DefaultInferenceParameters params = new DefaultInferenceParameters(this);
    params.setMaxNumber(1);
    CycList answer = queryVariable(variable, query, makeELMt(mt), params);

    if (answer.size() > 0) {
      return answer.get(0);
    } else {
      return null;
    }
  }

  /**
   * Returns the first arg2 ground or non-term from assertions having the specified predicate and
   * arg1 values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the first arg2 ground or non-term from assertions having the specified predicate and
   *         arg1 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getAssertionArg2(String predicate, String arg1, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getAssertionArg2(getKnownConstantByName(predicate),
            getKnownConstantByName(arg1),
            getKnownConstantByName(mt));
  }

  /**
   * Returns the first arg2 ground or non-term from assertions having the specified predicate and
   * arg1 values.
   *
   * @param predicate the given predicate
   * @param arg1 the given arg1 term
   * @param mt the inference microtheory
   *
   * @return the first arg2 ground or non-term from assertions having the specified predicate and
   *         arg1 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getAssertionArg2(CycFort predicate, CycFort arg1, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return this.converseObject(makeSubLStmt(FPRED_VALUE_IN_MT, arg1, predicate, makeELMt(mt)));
  }

  /**
   * Returns the first arg1 term from gafs having the specified predicate and arg2 values.
   *
   * @param predicate the given predicate
   * @param arg2 the given arg2 term
   * @param mt the inference microtheory
   *
   * @return the first arg1 term from gafs having the specified predicate and arg2 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getArg1(String predicate, String arg2, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg1(getKnownConstantByName(predicate),
            getKnownConstantByName(arg2),
            getKnownConstantByName(mt));
  }

  /**
   * Returns the first arg1 term from gafs having the specified predicate and arg2 values.
   *
   * @param predicate the given predicate
   * @param arg2 the given arg2 term
   * @param mt the inference microtheory
   *
   * @return the first arg1 term from gafs having the specified predicate and arg2 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getArg1(CycFort predicate, CycDenotationalTerm arg2, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList answer = getArg1s(predicate, arg2, makeELMt(mt));

    if (answer.size() > 0) {
      return answer.get(0);
    } else {
      return null;
    }
  }

  /**
   * Returns the list of arg1 terms from gafs having the specified predicate and arg2 values.
   *
   * @param predicate the given predicate
   * @param arg2 the given arg2 term
   * @param mt the inference microtheory
   *
   * @return the list of arg1 terms from gafs having the specified predicate and arg2 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getArg1s(String predicate, String arg2, String mt)
          throws UnknownHostException, IOException, CycApiException {
    return getArg1s(getKnownConstantByName(predicate),
            getKnownConstantByName(arg2),
            getKnownConstantByName(mt));
  }

  /**
   * Returns the list of arg1 terms from gafs having the specified predicate and arg2 values.
   *
   * @param predicate the given predicate
   * @param arg2 the given arg2 term
   * @param mt the inference microtheory
   *
   * @return the list of arg1 terms from gafs having the specified predicate and arg2 values
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getArg1s(CycFort predicate, CycDenotationalTerm arg2, CycObject mt)
          throws UnknownHostException, IOException, CycApiException {

    CycVariable variable = CycObjectFactory.makeCycVariable("?arg1");
    final CycFormulaSentence query = CycFormulaSentence.makeCycFormulaSentence(
            predicate, variable, arg2);

    return queryVariable(variable, query, makeELMt(mt));
  }

  /**
   * Returns the Cyc image ID.
   *
   * @return the Cyc image ID string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public String getCycImageID()
          throws UnknownHostException, IOException, CycApiException {
    return converseString(CYC_IMAGE_ID_EXPRESSION);
  }
  static private final String CYC_IMAGE_ID_EXPRESSION = makeSubLStmt("cyc-image-id");

  /**
   * Returns the list of assertions contained in the given mt.
   *
   * @param mt the given microtheory
   *
   * @return the list of assertions contained in the given mt
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getAllAssertionsInMt(CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    return converseList(makeSubLStmt("gather-mt-index", mt));
  }

  /**
   * Unasserts all assertions from the given mt, with a transcript record of the unassert
   * operation.
   *
   * @param mt the microtheory from which to delete all its assertions
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void unassertMtContentsWithTranscript(CycObject mt)
          throws UnknownHostException, IOException, CycApiException {
    CycList assertions = getAllAssertionsInMt(mt);
    Iterator iter = assertions.iterator();

    while (iter.hasNext()) {
      CycAssertion assertion = (CycAssertion) iter.next();
      String command = wrapBookkeeping(makeSubLStmt("ke-unassert-now", assertion, makeELMt(mt)));
      converseVoid(command);
    }
  }

  /**
   * Unasserts all assertions from the given mt, without a transcript record of the unassert
   * operation.
   *
   * @param mt the microtheory from which to delete all its assertions
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void unassertMtContentsWithoutTranscript(CycObject mt)
          throws UnknownHostException, IOException,
          CycApiException {
    CycList assertions = getAllAssertionsInMt(mt);
    Iterator iter = assertions.iterator();

    while (iter.hasNext()) {
      CycAssertion assertion = (CycAssertion) iter.next();
      String command = makeSubLStmt("cyc-unassert", assertion, makeELMt(mt));
      converseVoid(command);
    }
  }

  /**
   * Unasserts all assertions from the given mt having the given predicate and arg1, without a
   * transcript record of the unassert operation.
   *
   * @param predicate the given predicate or null to match all predicates
   * @param arg1 the given arg1
   * @param mt the microtheory from which to delete the matched assertions
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public void unassertMatchingAssertionsWithoutTranscript(CycFort predicate,
          Object arg1,
          CycObject mt)
          throws UnknownHostException, IOException,
          CycApiException {
    CycList assertions = getAllAssertionsInMt(mt);
    Iterator iter = assertions.iterator();

    while (iter.hasNext()) {
      CycAssertion assertion = (CycAssertion) iter.next();
      CycList sentence = assertion.getFormula();

      if (sentence.size() < 2) {
        continue;
      }

      if (!(arg1.equals(sentence.second()))) {
        continue;
      }

      if ((predicate != null) && (!(predicate.equals(sentence.first())))) {
        continue;
      }

      String command = "(cyc-unassert " + assertion.stringApiValue()
              + makeELMt(mt).stringApiValue() + "))";
      converseVoid(command);
    }
  }

  /**
   * Returns the list of Cyc terms whose denotation matches the given English string.
   *
   * @param denotationString the given English denotation string
   *
   * @return the list of Cyc terms whose denotation matches the given English string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getDenotsOfString(String denotationString)
          throws UnknownHostException, IOException, CycApiException {
    return new CycList(NLFormat.getInstance(this).parseObjects(denotationString));
  }

  /**
   * Returns the list of Cyc terms whose denotation matches the given English string and which are
   * instances of any of the given collections.
   *
   * @param denotationString the given English denotation string
   * @param collections the given list of collections
   *
   * @return the list of Cyc terms whose denotation matches the given English string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getDenotsOfString(String denotationString, CycList collections)
          throws UnknownHostException, IOException, CycApiException {
    CycList terms = getDenotsOfString(denotationString);
    CycList result = new CycList();
    Iterator collectionsIterator = collections.iterator();

    while (collectionsIterator.hasNext()) {
      CycFort oneCollection = (CycFort) collectionsIterator.next();
      Iterator termsIter = terms.iterator();

      while (termsIter.hasNext()) {
        CycFort term = (CycFort) termsIter.next();

        if (this.isa(term, oneCollection)) {
          result.add(term);
        }
      }
    }

    return result;
  }

  /**
   * Returns the list of Cyc terms whose denotation matches the given English multi-word string.
   *
   * @param multiWordDenotationString the given English denotation multi-word string
   *
   * @return the list of Cyc terms whose denotation matches the given English multi-word string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getMWSDenotsOfString(CycList multiWordDenotationString)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("mws-denots-of-string", multiWordDenotationString);
    return converseList(command);
  }

  /**
   * Returns the list of Cyc terms whose denotation matches the given English multi-word string and
   * which are instances of any of the given collections.
   *
   * @param multiWordDenotationString the given English denotation string
   * @param collections the given list of collections
   *
   * @return the list of Cyc terms whose denotation matches the given English multi-word string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList getMWSDenotsOfString(CycList multiWordDenotationString,
          CycList collections)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("mws-denots-of-string", multiWordDenotationString);

    CycList terms = converseList(command);
    CycList result = new CycList();
    Iterator collectionsIterator = collections.iterator();

    while (collectionsIterator.hasNext()) {
      CycFort oneCollection = (CycFort) collectionsIterator.next();
      Iterator termsIter = terms.iterator();

      while (termsIter.hasNext()) {
        CycFort term = (CycFort) termsIter.next();

        if (this.isa(term, oneCollection)) {
          result.add(term);
        }
      }
    }

    return result;
  }

  /**
   * Returns true if the given symbol is defined as an api function.
   *
   * @param symbolName the candidate api function symbol name
   *
   * @return true if the given symbol is defined as an api function
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isFunctionBound(String symbolName)
          throws UnknownHostException, IOException, CycApiException {
    CycSymbol cycSymbol = makeCycSymbol(
            symbolName);

    return isFunctionBound(cycSymbol);
  }

  /**
   * Returns true if the given symbol is defined as an api function.
   *
   * @param cycSymbol the candidate api function symbol
   *
   * @return rue if the given symbol is defined as an api function
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  public boolean isFunctionBound(CycSymbol cycSymbol)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("boolean", makeNestedSubLStmt("fboundp"), cycSymbol);
    return converseBoolean(command);
  }

  /**
   * Returns the Heuristic Level (HL) object represented by the given string.
   *
   * @param string the string which represents a number, quoted string, constant, naut or nart
   *
   * @return the Heuristic Level (HL) object represented by the given string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getHLCycTerm(String string)
          throws UnknownHostException, IOException, CycApiException {
    return converseObject("(canonicalize-term  '" + string + ")");
  }

  /**
   * Returns the Epistimological Level (EL) object represented by the given string.
   *
   * @param string the string which represents a number, quoted string, constant, naut or nart
   *
   * @return the Epistimological Level (EL)object represented by the given string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public Object getELCycTerm(String string)
          throws UnknownHostException, IOException, CycApiException {
    return converseObject("(identity '" + string + ")");
  }

  /** Returns the external ID for the given Cyc object.
   *
   * @deprecated Should be replaced in favor of {@link #org.opencyc.cycObject.DefaultCycObject.toCompactExternalId(obj, access)}
   * @param cycObject the Cyc object
   * @return the external ID string
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  @Deprecated
  public String getExternalIDString(final CycObject cycObject)
          throws UnknownHostException, IOException, CycApiException {
    return DefaultCycObject.toCompactExternalId(cycObject, this);
  }

  public static boolean isPossibleExternalIDString(final Object obj) {
    return is64BitString(obj);
  }

  private static boolean is64BitString(final Object obj) {
    if (obj instanceof String) {
      return is64Bit((String) obj);
    } else {
      return false;
    }
  }

  private static boolean is64Bit(final String string) {
    final StringCharacterIterator i = new StringCharacterIterator(string);
    for (char c = i.first(); c != CharacterIterator.DONE; c = i.next()) {
      if (!is64Bit(c)) {
        return false;
      }
    }
    return true;
  }

  private static boolean is64Bit(final char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || //0-51
            (c >= '0' && c <= '9') || //52-61
            (c == '+') || (c == '/') || //62-63 original Base64 standard
            (c == '-') || (c == '_'); //62-63 modified Base64 for URL
  }

  /**
   * Returns a random constant.
   *
   * @return a random constant
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycConstant getRandomConstant()
          throws UnknownHostException, IOException, CycApiException {
    return (CycConstant) converseObject("(random-constant)");
  }

  /**
   * Returns a random nart (Non-Atomic Reified Term).
   *
   * @return a random nart (Non-Atomic Reified Term)
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycNart getRandomNart()
          throws UnknownHostException, IOException, CycApiException {
    return (CycNart) converseObject("(random-nart)");
  }

  /**
   * Returns a random assertion.
   *
   * @return a random assertion
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycAssertion getRandomAssertion()
          throws UnknownHostException, IOException, CycApiException {
    return (CycAssertion) converseObject("(random-assertion)");
  }

  /**
   * Returns the given list with EL NARTS transformed to CycNart objects.
   *
   * @param cycList the given list
   *
   * @return the given list with EL NARTS transformed to CycNart objects
   *
   * @throws IOException if a communications error occurs
   * @throws UnknownHostException if the Cyc server cannot be found
   * @throws CycApiException if the Cyc server returns an error
   */
  public CycList canonicalizeList(CycList cycList)
          throws UnknownHostException, IOException, CycApiException {
    CycList canonicalList = new CycList();
    Iterator iter = cycList.iterator();

    while (iter.hasNext()) {
      Object obj = iter.next();

      if (obj instanceof CycList) {
        canonicalList.add(getHLCycTerm(((CycList) obj).cyclify()));
      } else if (obj instanceof CycNart) {
        canonicalList.add(getHLCycTerm(((CycNart) obj).cyclify()));
      } else {
        canonicalList.add(obj);
      }
    }

    return canonicalList;
  }

  /**
   * Gets the assertion date for the given assertion, or zero if the date is not available.
   *
   * @return the assertion date for the given assertion
   */
  public Long getAssertionDate(CycAssertion cycAssertion)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("asserted-when", cycAssertion);
    Object date = converseObject(command);
    if (date instanceof Integer) {
      return ((Integer) date).longValue();
    }
    if (date instanceof Long) {
      return (Long) date;
    }
    if (date.equals(CycObjectFactory.nil)) {
      return 0l;
    } else {
      throw new CycApiException("unexpected type of date returned " + date.toString());
    }
  }

  /**
   * Returns true if the given HL formula and microtheory correspond to a valid
   * assertion in that microtheory.
   *
   * @param gaf the given assertion formula
   * @param mt the candidate assertion microtheory
   */
  public boolean isGafValidAssertion(CycList gaf, ELMt mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("find-gaf", gaf, mt);
    Object response = converseObject(command);
    return !response.equals(CycObjectFactory.nil);
  }

  /**
   * Returns true if the given HL formula and microtheory correspond to a valid
   * assertion in that microtheory.
   *
   * @param gaf the given assertion formula
   * @param mt the candidate assertion microtheory
   */
  public boolean isGafValidAssertion(CycFormulaSentence gaf, ELMt mt)
          throws UnknownHostException, IOException, CycApiException {
    return isGafValidAssertion(gaf.getArgs(), mt);
  }

  /**
   * Returns true if the given HL formula and microtheory correspond to a valid
   * assertion in that microtheory.
   *
   * @param hlFormula the given HL formula
   * @param mt the candidate assertion microtheory
   */
  public boolean isAssertionValid(CycList hlFormula, CycFort mt)
          throws UnknownHostException, IOException, CycApiException {
    String command = makeSubLStmt("find-assertion", hlFormula, mt);
    Object response = converseObject(command);
    return !response.equals(CycObjectFactory.nil);
  }

  /** Asserts that the given term is dependent upon the given independent term.  If the latter is
   * killed, then the truth maintenance kills the dependent term.
   *
   * @param dependentTerm the dependent term
   * @param independentTerm the independent term
   * @param mt the assertion microtheory
   */
  public void assertTermDependsOn(final CycFort dependentTerm, final CycFort independentTerm, final CycFort mt) throws IOException, CycApiException {
    // assert (#$termDependsOn <dependentTerm> <independentTerm>) in #$UniversalVocabularyMt
    assertGaf(mt, getKnownConstantByGuid("bdf02d74-9c29-11b1-9dad-c379636f7270"), dependentTerm, independentTerm);
  }

  /** Asserts that the given term is defined in the given mt.  If the mt is
   * subsequently killed, then the truth maintenance kills the dependent term.
   *
   * @param dependentTerm the dependent term
   * @param mt the defining microtheory
   */
  public void assertDefiningMt(final CycFort dependentTerm, final CycFort mt) throws IOException, CycApiException {
    // assert (#$definingMt <dependentTerm> <mt>) in #$BaseKB
    assertGaf(baseKB, getKnownConstantByGuid("bde5ec9c-9c29-11b1-9dad-c379636f7270"), dependentTerm, mt);
  }

  /** Returns the XML datetime string corresponding to the given CycL date
   *
   * @param date the date naut
   * @return the XML datetime string corresponding to the given CycL date
   * @deprecated use DateConverter.
   */
  static public String xmlDatetimeString(final CycList date) throws IOException, CycApiException {
    try {
      final CycNaut dateNaut = (CycNaut) CycNaut.convertIfPromising(date);
      final Date javadate = DateConverter.parseCycDate(dateNaut, TimeZone.getDefault(), false);
      int cycDatePrecision = DateConverter.getCycDatePrecision(dateNaut);
      if (cycDatePrecision > DateConverter.DAY_GRANULARITY) {
        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").format(javadate);
      } else {
        return new SimpleDateFormat("yyyy-MM-dd").format(javadate);
      }
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Returns whether or not we have a valid lease with the Cyc server.
   * @return whether or not we have a valid lease with the Cyc server
   */
  public boolean hasValidLease() {
    return (cycLeaseManager == null) ? true : cycLeaseManager.hasValidLease();
  }

  /** Initializes the query properties. */
  private void initializeQueryProperties() {
    defaultQueryProperties.put(makeCycSymbol(":allowed-rules"), makeCycSymbol(":all"));
    defaultQueryProperties.put(makeCycSymbol(":result-uniqueness"), makeCycSymbol(":bindings"));
    defaultQueryProperties.put(makeCycSymbol(":allow-hl-predicate-transformation?"), false);
    defaultQueryProperties.put(makeCycSymbol(":allow-unbound-predicate-transformation?"), false);
    defaultQueryProperties.put(makeCycSymbol(":allow-evaluatable-predicate-transformation?"), false);
    defaultQueryProperties.put(makeCycSymbol(":intermediate-step-validation-level"), makeCycSymbol(":all"));
    defaultQueryProperties.put(makeCycSymbol(":negation-by-failure?"), false);
    defaultQueryProperties.put(makeCycSymbol(":allow-indeterminate-results?"), true);
    defaultQueryProperties.put(makeCycSymbol(":allow-abnormality-checking?"), true);
    defaultQueryProperties.put(makeCycSymbol(":disjunction-free-el-vars-policy"), makeCycSymbol(":compute-intersection"));
    defaultQueryProperties.put(makeCycSymbol(":allowed-modules"), makeCycSymbol(":all"));
    defaultQueryProperties.put(makeCycSymbol(":completeness-minimization-allowed?"), true);
    defaultQueryProperties.put(makeCycSymbol(":direction"), makeCycSymbol(":backward"));
    defaultQueryProperties.put(makeCycSymbol(":equality-reasoning-method"), makeCycSymbol(":czer-equal"));
    defaultQueryProperties.put(makeCycSymbol(":equality-reasoning-domain"), makeCycSymbol(":all"));
    defaultQueryProperties.put(makeCycSymbol(":max-problem-count"), Long.valueOf(100000));
    defaultQueryProperties.put(makeCycSymbol(":transformation-allowed?"), false);
    defaultQueryProperties.put(makeCycSymbol(":add-restriction-layer-of-indirection?"), true);
    defaultQueryProperties.put(makeCycSymbol(":evaluate-subl-allowed?"), true);
    defaultQueryProperties.put(makeCycSymbol(":rewrite-allowed?"), false);
    defaultQueryProperties.put(makeCycSymbol(":abduction-allowed?"), false);
    defaultQueryProperties.put(makeCycSymbol(":removal-backtracking-productivity-limit"), Long.valueOf(2000000));
    // dynamic query properties
    defaultQueryProperties.put(makeCycSymbol(":max-number"), null);
    defaultQueryProperties.put(makeCycSymbol(":max-time"), Integer.valueOf(120));
    defaultQueryProperties.put(makeCycSymbol(":max-transformation-depth"), Integer.valueOf(0));
    defaultQueryProperties.put(makeCycSymbol(":block?"), false);
    defaultQueryProperties.put(makeCycSymbol(":max-proof-depth"), null);
    defaultQueryProperties.put(makeCycSymbol(":cache-inference-results?"), false);
    defaultQueryProperties.put(makeCycSymbol(":answer-language"), makeCycSymbol(":el"));
    defaultQueryProperties.put(makeCycSymbol(":continuable?"), false);
    defaultQueryProperties.put(makeCycSymbol(":browsable?"), false);
    defaultQueryProperties.put(makeCycSymbol(":productivity-limit"), Long.valueOf(2000000));

    final CycList<CycSymbol> queryPropertiesList = new CycList(defaultQueryProperties.keySet());
    final String command = makeSubLStmt("mapcar", makeCycSymbol("query-property-p"), queryPropertiesList);
    try {
      CycList results = converseList(command);
      for (int i = 0, size = results.size(); i < size; i++) {
        if (results.get(i).equals(CycObjectFactory.nil)) {
          final CycSymbol badProperty = queryPropertiesList.get(i);
          System.err.println(badProperty + " is not a query-property-p");
          defaultQueryProperties.remove(badProperty);
        }
      }
    } catch (Exception e) {
      System.err.println(e.getMessage());
    }
    queryPropertiesInitialized = true;
  }

  /** Migrate to this once inference parameter definitions are included in OpenCyc KB. */
  private void initializeQueryPropertiesNew() {
    synchronized (defaultQueryProperties) {
      defaultQueryProperties.clear();
      try {
        final InferenceParameterDescriptions desc = DefaultInferenceParameterDescriptions.loadInferenceParameterDescriptions(this, 10000);
        final InferenceParameters defaults = desc.getDefaultInferenceParameters();
        final CycList allQueryProperties = converseList(makeSubLStmt("ALL-QUERY-PROPERTIES"));
        for (final Object property : allQueryProperties) {
          if (property instanceof CycSymbol && defaults.containsKey((CycSymbol) property)) {
            final Object value = defaults.get((CycSymbol) property);
            defaultQueryProperties.put((CycSymbol) property, value);
          }
        }
      } catch (UnknownHostException ex) {
        Logger.getLogger(CycAccess.class.getName()).log(Level.SEVERE, null, ex);
      } catch (IOException ex) {
        Logger.getLogger(CycAccess.class.getName()).log(Level.SEVERE, null, ex);
      } catch (CycApiException ex) {
        Logger.getLogger(CycAccess.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
    queryPropertiesInitialized = true;
  }

  /** Returns a clone of the default HL query propoerties.
   *
   * @return the default HL query propoerties
   */
  public InferenceParameters getHLQueryProperties() {
    synchronized (defaultQueryProperties) {
      if (!queryPropertiesInitialized) {
        initializeQueryProperties();
      }
      return (InferenceParameters) defaultQueryProperties.clone();
    }
  }

  /** Returns a query properties string for the given query properties if present, otherwise
   * returns a query properties string for the default query properties.
   *
   * @param queryProperties the given query properties or null if the defaults are to be used
   *
   * @return a query properties string for the given query properties if present, otherwise
   * returns a query properties string for the default query properties
   */
  public String queryPropertiesToString(final InferenceParameters queryProperties) {
    final InferenceParameters tempQueryProperties = (queryProperties == null) ? getHLQueryProperties() : queryProperties;
    final CycList parameterList = new CycList();
    for (final Iterator<Entry<CycSymbol, Object>> iter = tempQueryProperties.entrySet().iterator(); iter.hasNext();) {
      Entry<CycSymbol, Object> mapEntry = iter.next();
      CycSymbol queryParameterKeyword = mapEntry.getKey();
      parameterList.add(queryParameterKeyword);
      final Object rawValue = mapEntry.getValue();
      parameterList.add(tempQueryProperties.parameterValueCycListApiValue(queryParameterKeyword, rawValue));
    }
    return parameterList.stringApiValue();
  }

  /** Initializes a named inference problem store.
   *
   * @param name the unique problem store name
   * @param queryProperties the given query properties or null if the defaults are to be used
   */
  public void initializeNamedInferenceProblemStore(final String name, final InferenceParameters queryProperties) throws IOException, CycApiException {
    //// Preconditions
    if (name == null) {
      throw new NullPointerException("name must not be null");
    }
    if (name.length() == 0) {
      throw new IllegalArgumentException("name must not be an empty string");
    }
    final InferenceParameters tempQueryProperties = (queryProperties == null) ? getHLQueryProperties() : queryProperties;
    final String command =
            "(progn "
            + "  (find-or-create-problem-store-by-name \"" + name + "\" (filter-plist " + queryPropertiesToString(tempQueryProperties) + "'problem-store-property-p)) "
            + "  nil)";
    converseVoid(command);
  }

  /** Destroys the named problem store.
   *
   * @param name the unique problem store name
   */
  public void destroyInferenceProblemStoreByName(final String name) throws IOException, CycApiException {
    //// Preconditions
    if (name == null) {
      throw new NullPointerException("name must not be null");
    }
    if (name.length() == 0) {
      throw new IllegalArgumentException("name must not be an empty string");
    }
    final String command =
            "(destroy-problem-store-by-name \"" + name + "\")";
    converseVoid(command);
  }

  public CycSentence getSimplifiedSentence(CycFormulaSentence sentence) throws UnknownHostException, IOException {
    return sentence.getSimplifiedSentence(this);
  }

  public CycSentence getSimplifiedSentence(CycFormulaSentence sentence, ELMt mt) throws UnknownHostException, IOException {
    return sentence.getSimplifiedSentence(this, mt);
  }

  /** Determines whether the two input objects are equal EL expressions. */
  public boolean equalsEL(Object obj1, Object obj2) throws IOException {
    String command = makeSubLStmt(EQUALS_EL, obj1, obj2);
    // execute the SubL function-call and access the response
    Object response = converseObject(command);
    return !response.equals(CycObjectFactory.nil);
  }

  /** Determines whether the two input queries are equal EL expressions. */
  public boolean queriesEqualAtEL(Object obj1, Object obj2) throws IOException {
    String command = makeSubLStmt("queries-equal-at-el?", obj1, obj2);
    // execute the SubL function-call and access the response
    Object response = converseObject(command);
    return !response.equals(CycObjectFactory.nil);
  }
  //// Protected Area

  /**
   * Provides common local and remote CycAccess object initialization.
   *
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  private final void commonInitialization()
          throws IOException, CycApiException {
    if (Log.current == null) {
      Log.makeLog("cyc-api.log");
    }

    if (areAPIRequestsLoggedToFile) {
      apiRequestLog = new FileWriter("api-requests.lisp");
    }
    cycAccessInstances.put(Thread.currentThread(), this);

    /*if (sharedCycAccessInstance == null) {
    sharedCycAccessInstance = this;
    }*/
    cycImageID = getCycImageID();
    try {
      DefaultInferenceParameterDescriptions.loadInferenceParameterDescriptions(this, 0);
    } catch (Exception e) {
      Logger logger = Logger.getLogger("org.opencyc.api.CycAccess");
      logger.warning("Could not load inference parameter descriptions.");
      Throwable curr = e;
      while (curr != null) {
        logger.warning(curr.getLocalizedMessage());
        curr = curr.getCause();
      }
    }
    if (!isSOAPConnection) // if the communication mode is SOAP, then there is a lease manager, but it is never started
    {
      try {
        // wait for the sockets to initialize
        Thread.sleep(500);
      } catch (java.lang.InterruptedException e) {
      }
      cycLeaseManager = new CycLeaseManager(this);
      cycLeaseManager.start();
    }
  }

  /**
   * Converses with Cyc to perform an API command.  Creates a new connection for this command if
   * the connection is not persistent.
   *
   * @param command the command string or CycList
   *
   * @return the result as an object array of two objects
   * @see org.opencyc.api.CycConnectionInterface.converse
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  // @note Used by BOAPI.
  public Object[] converseRaw(Object command)
          throws UnknownHostException, IOException, CycApiException {
    return converse(command);
  }

  /**
   * Converses with Cyc to perform an API command.  Creates a new connection for this command if
   * the connection is not persistent.
   *
   * @param command the command string or CycList
   *
   * @return the result as an object array of two objects
   * @see CycConnectionInterface.converse
   *
   * @throws UnknownHostException if cyc server host not found on the network
   * @throws IOException if a data communication error occurs
   * @throws CycApiException if the api request results in a cyc server error
   */
  protected Object[] converse(Object command)
          throws UnknownHostException, IOException, CycApiException {
    Object[] response = {null, null};

    if (trace > CycConnection.API_TRACE_NONE || areAPIRequestsLoggedToFile) {
      final CycList commandCycList = (command instanceof CycList) ? (CycList) command : makeCycList((String) command);
      final String prettyCommandCycList = commandCycList.toPrettyCyclifiedString("");
      final String escapedCommandCycList = commandCycList.toPrettyEscapedCyclifiedString("");
      if (areAPIRequestsLoggedToFile) {
        apiRequestLog.write(escapedCommandCycList);
        apiRequestLog.write('\n');
      }
      if (trace > CycConnection.API_TRACE_NONE) {
        Log.current.println(prettyCommandCycList + "\n--> cyc");
      }
    }

    if (!isSOAPConnection) {
//      if ((previousAccessedMilliseconds + MAX_UNACCESSED_MILLIS) < System.currentTimeMillis()) {
//        Log.current.println("Re-establishing a stale Cyc connection.");
//        reEstablishCycConnection();
//      }
//      else
      if (!((CycConnection) getCycConnection()).isValidBinaryConnection()) {
        Log.current.println("Re-establishing an invalid Cyc connection.");
        reEstablishCycConnection();
      }
    }
    response = cycConnection.converse(command);
    previousAccessedMilliseconds = System.currentTimeMillis();

    if (trace > CycConnection.API_TRACE_NONE) {
      String responseString;

      if (response[1] instanceof CycList) {
        responseString = ((CycList) response[1]).toPrettyString("");
      } else if (response[1] instanceof CycFort) {
        responseString = ((CycFort) response[1]).cyclify();
      } else {
        responseString = response[1].toString();
      }
      Log.current.println("cyc --> " + responseString);
    }

    return response;
  }

  /** Re-estabishes a stale binary CycConnection. */
  protected void reEstablishCycConnection() throws UnknownHostException, IOException, CycApiException {
    previousAccessedMilliseconds = System.currentTimeMillis();
    cycConnection.close();
    cycConnection = new CycConnection(hostName,
            port,
            this);
    if (!(cycImageID.equals(getCycImageID()))) {
      Log.current.println("New Cyc image detected, resetting caches.");
      CycObjectFactory.resetCaches();
    }
  }

  /**
   * Returns a with-bookkeeping-info macro expresssion.
   *
   * @return a with-bookkeeping-info macro expresssion
   */
  protected String withBookkeepingInfo() {
    String projectName = "nil";

    if (project != null) {
      projectName = project.stringApiValue();
    }

    String cyclistName = "nil";

    if (cyclist != null) {
      cyclistName = cyclist.stringApiValue();
    }

    return "(with-bookkeeping-info (new-bookkeeping-info " + cyclistName + " (the-date) "
            + projectName + "(the-second)) ";
  }
  //// Private Area

  private CycConstant makePrefetchedConstant(String guidStr, HashMap constantInfoDictionary) {
    Guid guid = CycObjectFactory.makeGuid(guidStr);
    CycConstant prefetchedConstant = makeConstantWithGuidName(guid,
            (String) constantInfoDictionary.get(guid));
    CycObjectFactory.addCycConstantCache(prefetchedConstant);
    return prefetchedConstant;
  }

  /** @return true iff <tt>focalTerm</tt> is known to have a fact sheet accessible to this CycAccess */
  public boolean termKnownToHavePrecachedFactSheet(final CycObject focalTerm) {
    return termsKnownToHavePrecachedFactSheets.contains(focalTerm);
  }

  /** Record the information that <tt>focalTerm</tt> is known to have a fact sheet accessible to this CycAccess */
  public void noteTermHasPrecachedFactSheet(final CycObject focalTerm) {
    termsKnownToHavePrecachedFactSheets.add(focalTerm);
  }  //// Internal Rep
  /** the Cyc server host name */
  protected String hostName;
  /** the Cyc server host tcp port number */
  protected int port;
  /** the Cyc server OK response code */
  protected static final int OK_RESPONSE_CODE = 200;
  /** the parameter that, when true, causes a trace of the messages to and from the server */
  protected int trace = CycConnection.API_TRACE_NONE;
  //protected int trace = CycConnection.API_TRACE_MESSAGES;
  //protected int trace = CycConnection.API_TRACE_DETAILED;
  private static final String UTF8 = "UTF-8";
  /** the current Cyc Cyclist (user) */
  private CycFort cyclist = null;
  /** the current Cyc project */
  private CycFort project = null;
  /** Least Recently Used Cache of ask results. */
  private final Map<CycList, Boolean> askCache = new LRUCache<CycList, Boolean>(500, 5000, true);
  /** Least Recently Used Cache of countAllInstances results. */
  private final Map<CycFort, Integer> countAllInstancesCache = new LRUCache<CycFort, Integer>(500, 5000, true);
  /** Least Recently Used Cache of isCollection results. */
  private final Map<CycObject, Boolean> isCollectionCache = new LRUCache<CycObject, Boolean>(500, 5000, true);
  /** Least Recently Used Cache of isGenlOf results. */
  private final Map<Pair, Boolean> isGenlOfCache = new LRUCache<Pair, Boolean>(500, 5000, true);
  /**
   * Reference to <tt>CycConnection</tt> object which manages the api connection to the OpenCyc
   * server.
   */
  protected CycConnectionInterface cycConnection;
  /** default query properties, initialized by initializeQueryProperties(). */
  private final InferenceParameters defaultQueryProperties = new DefaultInferenceParameters(this);
  private boolean queryPropertiesInitialized = false;
  /** the timestamp for the previous access to Cyc, used to re-establish too-long unused connections */
  private long previousAccessedMilliseconds = System.currentTimeMillis();
  /** the maximum time that the CycAccess connection is allowed to be unused before
   * establishing a fresh connection (ten hours)
   */
  protected static final long MAX_UNACCESSED_MILLIS = 36000000;
  /** the Cyc image ID used for detecting new Cyc images that cause the constants cache to be reset */
  private String cycImageID;
  /** the Cyc lease manager that acquires Cyc api service leases */
  private CycLeaseManager cycLeaseManager;
  /** The indicator that this CycAccess object is using a SOAP connection to communicate with Cyc */
  private boolean isSOAPConnection = false;
  final private Set<CycObject> termsKnownToHavePrecachedFactSheets = new HashSet<CycObject>();
  
  private static ThreadLocal<CycAccess> currentCyc = new ThreadLocal<CycAccess>();
  private static Map<String, CycAccess> currentCycAccesses = Collections.synchronizedMap(new HashMap<String, CycAccess>());
}
