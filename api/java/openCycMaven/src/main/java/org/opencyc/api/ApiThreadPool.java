/* $Id: ApiThreadPool.java 128353 2009-07-17 22:52:39Z tbrussea $
 *
 * Copyright (c) 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.api;

//// Internal Imports

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;


/**
 * <P>ApiThreadPool is designed to...
 *
 * <P>Copyright (c) 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author rck
 * @date September 14, 2006, 3:01 PM
 * @version $Id: ApiThreadPool.java 128353 2009-07-17 22:52:39Z tbrussea $
 */
public class ApiThreadPool extends ThreadPoolExecutor {
  
  //// Constructors
  
  /** Creates a new instance of ApiThreadPool. */
  public ApiThreadPool() {
    super(MIN_THREADS, MAX_THREADS, KEEP_ALIVE_TIME, KEEP_ALIVE_UNITS,
      DEFAULT_WORK_QUEUE, DEFAULT_THREAD_FACTORY);
  }
  
  public static synchronized ApiThreadPool getDefaultPool() {
    if (apiThreadPool == null) {
      apiThreadPool = new ApiThreadPool();
    }
    return apiThreadPool;
  }
  
  public static ThreadGroup getDefaultThreadGroup() {
    return defaultThreadGroup;
  }
  
  /** ensure we only run the right type of runnables **/
  public void execute(Runnable runnable) {
    super.execute(runnable);
  }
  
  //// Protected Area
  
  //// Private Area
  static private class ApiThread extends Thread {
    ApiThread(ThreadGroup threadGroup, Runnable command, String name) {
      super(threadGroup,command,name);
    }
  }
  
  //// Internal Rep
  private static int threadNum = 1;
  private static final ThreadGroup defaultThreadGroup = new ThreadGroup("OpenCYC API Thread Group");

  private static final int MIN_THREADS = 4;
  private static final int MAX_THREADS = 50;
  private static final BlockingQueue<Runnable> DEFAULT_WORK_QUEUE = new SynchronousQueue<Runnable>();
  private static final int KEEP_ALIVE_TIME = 60;
  private static final TimeUnit KEEP_ALIVE_UNITS = TimeUnit.SECONDS;
  private static final ThreadFactory DEFAULT_THREAD_FACTORY = new ThreadFactory() {
      public Thread newThread(Runnable command) {
        return new ApiThread(defaultThreadGroup, command, "SubL Thread #" + threadNum++);
      }
    };

  private static ApiThreadPool apiThreadPool = null;
  
  //// Main

  public static void main(String[] args) {
    System.out.println("Starting.");
    System.out.flush();
    final Semaphore sem = new Semaphore(0);
    for (int i = 0; i < 10; i++) {
      System.out.println("Queing proc " + i);
      System.out.flush();
      final int threadNum = i;
      getDefaultPool().execute(new Runnable() {
        public void run() {
          try {
            System.out.println("Starting proc " + threadNum);
            System.out.flush();
            Thread.currentThread().sleep(threadNum + 5000);
            sem.release();
            System.out.println("Finished proc " + threadNum);
            System.out.flush();
          } catch (Exception e) {} // dont care
        }
      });
    }
    try {
      sem.acquire(10);
    } catch (Exception e) {} // dont care
    System.out.println("Quitting.");
    System.exit(0);
  }
  
}
