/* $Id: CycLeaseManager.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2005 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.api;

//// External Imports
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EventListener;
import java.util.EventObject;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

//// Internal Imports
import org.opencyc.cycobject.CycList;
import org.opencyc.util.TimeOutException;

/**
 * <P>CycLeaseManager manages api service leases between a Cyc image (server) and application client.
 *
 * <P>Copyright (c) 2003 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 * @author reed
 *  date June 27, 2005, 10:20 AM
 * @version $Id: CycLeaseManager.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class CycLeaseManager extends Thread {

  //// Constructors
  /** Creates a new instance of CycLeaseManager. 
   *
   * @param cycAccess the Cyc api services client
   */
  public CycLeaseManager(final CycAccess cycAccess) {
    //// Preconditions
    if (cycAccess == null) {
      throw new InvalidParameterException("cycAccess must not be null");
    }

    logger = Logger.getLogger("org.opencyc.api.CycLeaseManager");
    this.cycAccess = cycAccess;
  }

  //// Public Area
  /** Adds a Cyc lease manager listener.
   *
   * @param cycLeaseManagerListener a listener for Cyc lease manager events
   */
  public synchronized void addListener(final CycLeaseManagerListener cycLeaseManagerListener) {
    //// Preconditions
    if (cycLeaseManagerListener == null) {
      throw new InvalidParameterException("cycLeaseManagerListener must not be null");
    }
    if (listeners.contains(cycLeaseManagerListener)) {
      throw new InvalidParameterException("listener must not be currently registered");
    }
    assert listeners != null : "listeners must not be null";
    if (cycAccess.getCycConnection() instanceof CycConnection) {
      assert cycAccess.getCycLeaseManager().isAlive() : "the CycLeaseManager thread has died because a lease timed-out, errored or was denied";
    }
    // otherwise, this is a SOAP client connection and leasing is never started

    listeners.add(cycLeaseManagerListener);
  }

  /** Removes a Cyc lease manager listener.
   *
   * @param cycLeaseManagerListener a listener for Cyc lease manager events
   */
  public synchronized void removeListener(final CycLeaseManagerListener cycLeaseManagerListener) {
    //// Preconditions
    if (cycLeaseManagerListener == null) {
      throw new InvalidParameterException("cycLeaseManagerListener must not be null");
    }
    assert listeners != null : "listeners must not be null";
    if (cycAccess.getCycConnection() instanceof CycConnection) {
      assert cycAccess.getCycLeaseManager().isAlive() : "the CycLeaseManager thread has died because a lease timed-out, errored or was denied";
    }
    // otherwise, this is a SOAP client connection and leasing is never started

    listeners.remove(cycLeaseManagerListener);
  }

  /**
   * Returns whether or not we have a valid lease with the Cyc server.
   * @return whether or not we have a valid lease with the Cyc server
   */
  public boolean hasValidLease() {
    return hasValidLease;
  }

  /** Removes all Cyc lease manager listeners.
   *
   * @param cycLeaseManagerListener a listener for Cyc lease manager events
   */
  public void removeAllListeners() {
    //// Preconditions
    assert listeners != null : "listeners must not be null";
    if (cycAccess.getCycConnection() instanceof CycConnection) {
      assert cycAccess.getCycLeaseManager().isAlive() : "the CycLeaseManager thread has died because a lease timed-out, errored or was denied";
    }
    // otherwise, this is a SOAP client connection and leasing is never started

    listeners.clear();
  }
  /** the Cyc api services lease request timeout in milliseconds */
  public static long CYC_API_SERVICES_LEASE_REQUEST_TIMEOUT_MILLIS = 120000;

  /** Performs periodic Cyc api service lease acquistion, and notifies listeners if the lease fails or is denied. */
  public void run() {

    //// Preconditions
    assert listeners != null : "listeners must not be null";
    assert cycAccess != null : "cycAccess must not be null";

    /*
    try {
    Thread.currentThread().sleep(Long.MAX_VALUE);
    } catch (InterruptedException ie) {
    } finally {
    if (true) {
    return;
    }
    }
     */


    Thread.currentThread().setName("Cyc API services lease manager");
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY);
    while (!cycAccess.isClosed()) {
      // SubL side gets a lease request for twice the value of the lease duration
      final String script = "(with-immediate-execution (acquire-api-services-lease " +
              (getLeaseDurationMilliseconds() * 2) + " \"" +
              cycAccess.getCycConnection().getUuid().toString() + "\"))";
      final CycList scriptList = cycAccess.makeCycList(script);
      String results = null;
      isLeaseRequestPending = true;
      logger.fine("Requesting API services lease");
      try {
        DefaultSubLWorkerSynch worker = new DefaultSubLWorkerSynch(scriptList, cycAccess,
                CYC_API_SERVICES_LEASE_REQUEST_TIMEOUT_MILLIS);
        worker.setShouldIgnoreInvalidLeases(true);
        results = (String) worker.getWork();
        logger.finest(results);
      } catch (TimeOutException toe) {
        isLeaseRequestPending = false;
        logger.fine("Cyc communications timeout encountered when attempting " + "to renew the API services lease.\n" + toe.getMessage());
        notifyListeners(CYC_DOES_NOT_RESPOND_TO_LEASE_REQUEST);
        try {
          Thread.sleep(getLeaseDurationMilliseconds());
        } catch (InterruptedException ex) {
        }
        continue;
      } catch (Exception e) {
        isLeaseRequestPending = false;
        logger.fine("Cyc communications error encountered when attempting " + "to renew the API services lease.\n" + e.getMessage());
        notifyListeners(CYC_COMMUNICATION_ERROR);
        try {
          Thread.sleep(getLeaseDurationMilliseconds());
        } catch (InterruptedException ex) {
        }
        continue;
      }
      //cycAccess.getCycConnection().traceOff();
      isLeaseRequestPending = false;
      if (results.equals("api services lease denied")) {
        logger.severe("The request to renew the API services lease was denied by the Cyc server.");
        notifyListeners(CYC_DENIES_THE_LEASE_REQUEST);
      } else {
        String currentImageID = extractImageID(results);
        if (cycImageID != null && !cycImageID.equals(currentImageID)) {
          logger.info("The Cyc server image ID has changed.");
          notifyListeners(CYC_IMAGE_ID_HAS_CHANGED);
        } else {
          logger.fine("API services lease renewed");
          notifyListeners(LEASE_SUCCESSFULLY_RENEWED);
        }
        cycImageID = currentImageID;
      }
      try {
        Thread.sleep(getLeaseDurationMilliseconds());
      } catch (InterruptedException e) {
      }
    }
  }

  /** Immediately renews the current lease by interrupting the sleep of this leasing thread. */
  public void immediatelyRenewLease() {
    logger.finest("immedidately renewing the lease");
    interrupt();
    try {
      // give time for Cyc to renew the lease
      sleep(250);
    } catch (InterruptedException e) {
    }
  }

  /** Returns the indicator whether a lease request is currently pending.
   *
   * @return the indicator whether a lease request is currently pending 
   */
  public boolean isLeaseRequestPending() {
    return isLeaseRequestPending;
  }

  /** Gets the lease duration milliseconds.
   *
   * @return the lease duration milliseconds
   */
  public long getLeaseDurationMilliseconds() {
    return leaseDurationMilliseconds;
  }

  /** Sets the lease duration milliseconds.
   *
   * @param leaseDurationMillieconds the lease duration milliseconds
   */
  public void setLeaseDurationMilliseconds(final long leaseDurationMilliseconds) {
    //// Preconditions
    if (leaseDurationMilliseconds < 2000) {
      throw new InvalidParameterException("leaseDurationMilliseconds must be at least 2000");
    }

    this.leaseDurationMilliseconds = leaseDurationMilliseconds;
  }

  /** Class to contain the Cyc lease manager event reason */
  public static class CycLeaseManagerReason {

    /** Creates a new CycLeaseManagerReason instance.
     *
     * @param reason the Cyc lease manager event reason
     */
    public CycLeaseManagerReason(final String reason) {
      //// Preconditions
      if (reason == null || reason.length() == 0) {
        throw new InvalidParameterException("reason must be a non-empty string");
      }

      this.reason = reason;
    }

    /** Gets the Cyc lease manager event reason.
     *
     * @return the Cyc lease manager event reason
     */
    public String getReason() {
      return reason;
    }
    /** the Cyc lease manager event reason */
    private final String reason;

    public boolean isGood() {
      return this == LEASE_SUCCESSFULLY_RENEWED;
    }
  };
  /** the Cyc lease manager reason when Cyc denies the api service lease request */
  public static final CycLeaseManagerReason CYC_DENIES_THE_LEASE_REQUEST =
          new CycLeaseManagerReason("CYC_DENIES_THE_LEASE_REQUEST");
  /** the Cyc lease manager reason when the api service request resulted in a communication error condition */
  public static final CycLeaseManagerReason CYC_COMMUNICATION_ERROR =
          new CycLeaseManagerReason("CYC_COMMUNICATION_ERROR");
  /** the Cyc lease manager reason when Cyc does not respond to the api service lease request */
  public static final CycLeaseManagerReason CYC_DOES_NOT_RESPOND_TO_LEASE_REQUEST =
          new CycLeaseManagerReason("CYC_DOES_NOT_RESPOND_TO_LEASE_REQUEST");
  /** the Cyc lease manager reason when Cyc has been restarted with a new image id (invalidating any cached constant ids, and
   * possibly with inconsistent knowledge state) */
  public static final CycLeaseManagerReason CYC_IMAGE_ID_HAS_CHANGED =
          new CycLeaseManagerReason("CYC_IMAGE_ID_HAS_CHANGED");
  /** the Cyc lease manager reason when a lease has been successfully renewed */
  public static final CycLeaseManagerReason LEASE_SUCCESSFULLY_RENEWED =
          new CycLeaseManagerReason("LEASE_SUCCESSFULLY_RENEWED");

  /** Class that provides a Cyc lease event. */
  public class CycLeaseEventObject extends EventObject {

    /** Constructs a new CycLeaseEventObject instance.
     *
     * @param source the object on which the Event initially occurred
     * @param reason the Cyc lease manager event reason
     */
    public CycLeaseEventObject(final Object source, final CycLeaseManagerReason cycLeaseManagerReason) {
      super(source);
      //// Preconditions
      if (cycLeaseManagerReason == null) {
        throw new InvalidParameterException("cycLeaseManagerReason must not be null");
      }

      this.cycLeaseManagerReason = cycLeaseManagerReason;
    }

    /** Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
      return "CycLeaseEvent (" + cycLeaseManagerReason.getReason() + ")";
    }

    public CycLeaseManagerReason getReason() {
      return cycLeaseManagerReason;
    }
    final CycLeaseManagerReason cycLeaseManagerReason;
  }

  /** Defines the interface for Cyc API services event listeners. */
  public interface CycLeaseManagerListener extends EventListener {

    /** Notifies the listener of the given Cyc API services lease event.
     *
     * @param evt the the given Cyc API services lease event
     */
    void notifyCycLeaseEvent(CycLeaseEventObject evt);
  };

  public String getCycImageId() {
    return cycImageID;
  }

  //// Protected Area
  //// Private Area
  private String extractImageID(final String leaseDescription) {
    assert leaseDescription.startsWith("api services lease granted by ");
    String currentImageID = leaseDescription.substring(30);
    final int firstSpaceIndex = currentImageID.indexOf(" ");
    if (firstSpaceIndex > 0) {
      currentImageID = currentImageID.substring(0, firstSpaceIndex);
    }
    return currentImageID;
  }

  /** Notifies the api service lease event listeners that an event happened with the given reason.
   *
   * @param cycLeaseManagerReason the Cyc lease manager event reason
   */
  private void notifyListeners(final CycLeaseManagerReason cycLeaseManagerReason) {
    //// Preconditions
    assert cycLeaseManagerReason != null : "cycLeaseManagerReason must not be null";
    assert listeners != null : "listeners must not be null";
    assert cycAccess != null : "cycAccess must not be null";
    if ((cycLeaseManagerReason == CycLeaseManager.CYC_COMMUNICATION_ERROR) || (cycLeaseManagerReason == CycLeaseManager.CYC_DENIES_THE_LEASE_REQUEST) || (cycLeaseManagerReason == CycLeaseManager.CYC_DOES_NOT_RESPOND_TO_LEASE_REQUEST)) {
      hasValidLease = false;
    } else {
      hasValidLease = true;
    }
    final int listeners_size = listeners.size();
    for (int i = 0; i < listeners_size; i++) {
      final CycLeaseManagerListener cycLeaseManagerListener = (CycLeaseManagerListener) listeners.get(i);
      cycLeaseManagerListener.notifyCycLeaseEvent(new CycLeaseEventObject(cycAccess, cycLeaseManagerReason));
    }
  }
  //// Internal Rep
  /** the logger */
  private final Logger logger;
  /** the Cyc api services client */
  private final CycAccess cycAccess;
  /** the Cyc image ID from the previous lease */
  private String cycImageID = null;
  /** the list of registered listeners */
  private final List listeners = Collections.synchronizedList(new ArrayList());
  /** the indicator whether a lease request is currently pending */
  private volatile boolean isLeaseRequestPending = false;
  /** the lease duration milliseconds */
  private long leaseDurationMilliseconds = 120000;
  /** assume we have a valid lease until told otherwise */
  private volatile boolean hasValidLease = true;

  //// Main
  /** Executes a test of the CycLeaseManager.
   *
   * @param args the command line arguments (unused)
   */
  public static void main(String[] args) {
    try {
      Logger.getAnonymousLogger().info("Starting.");
      final CycAccess cycAccess = new CycAccess("public1.cyc.com", 3600);
      Logger.getAnonymousLogger().info("Connected to: " + cycAccess.getHostName() + ":" + cycAccess.getBasePort());
      //      cycAccess.traceOn();
      for (int i = 0; i < 2; i++) {
        Thread.sleep(2000);
        final String script = "(sleep 1)";
        Logger.getAnonymousLogger().info("About to talk to Cyc: " + script);
        cycAccess.converseVoid(script);
        Logger.getAnonymousLogger().info("Finished talking to Cyc.");
      }
      Logger.getAnonymousLogger().info("About to close CycAccess.");
      cycAccess.close();
      Logger.getAnonymousLogger().info("Closed CycAccess.");
    } catch (Exception e) {
      Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage(), e);
      Logger.getAnonymousLogger().info("Finished.");
      System.exit(1);
    }
    Logger.getAnonymousLogger().info("Finished.");
    // @note if the  main method hangs, then there is an issue with threads
    // lingering that should be investigated.
  }
}
