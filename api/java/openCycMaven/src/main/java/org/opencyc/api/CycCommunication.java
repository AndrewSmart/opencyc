package org.opencyc.api;

import java.io.IOException;

import org.opencyc.util.CycUtils;
import org.opencyc.util.CycWorker;
import org.opencyc.util.CycWorkerListener;


public class CycCommunication extends CycWorker {

  int timeOutMS = 0;
  String subL = "";
  //need to make CycAccess TRUELY ASYNCHRONOUS!!!!!!!!
  CycAccess conn = null;

  /** @deprecated use SubLWorker */
  public CycCommunication(CycAccess theConn, String subLExp, 
			  int theTimeOutMS, CycWorkerListener cwl) {
    conn = theConn;
    subL = subLExp;
    timeOutMS = theTimeOutMS;
    this.addListener(cwl);
  }
    
  /** @deprecated use SubLWorker */
  public CycCommunication(CycAccess theConn, String subLExp, 
			  int theTimeOutMS) {
    conn = theConn;
    subL = subLExp;
    timeOutMS = theTimeOutMS;
  }

  //Need to modify this to handle timeouts!!!!!!!!!!!
  public Object construct() throws IOException, CycApiException { 
    return CycUtils.evalSubLWithWorker(conn, subL); 
  }

  /**Blocking call.*/
  public Object getReply() throws Exception { return getWork(); }

  public boolean isDone() { 
    if(null == threadVar.get()) { return true; }
    return false;
  }

  public void waitTillDone() {
    while (true) {  
      Thread t = threadVar.get();
      if (t == null) {
	return;
      }
      try {
	t.join();
      } catch (InterruptedException e) {
	Thread.currentThread().interrupt(); // propagate
	return;
      }
    }
  }
  
  //Need to updated this to send kill message to KB as well as
  //kill background thread!!!!!!!!
  public void interrupt() { super.interrupt(); }

}
  
