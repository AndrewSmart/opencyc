/* $Id: SubLWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 */

package org.opencyc.api;

//// Internal Imports
import org.opencyc.cycobject.*;
import org.opencyc.api.*;
import org.opencyc.util.*;

//// External Imports
import java.util.*;
import java.io.*;

/**
 * <P>SubLWorkerSynch is designed to provide a handle for a particular 
 * communication event with a Cyc server in a synchronous manner. 
 * DefaultSubLWorkerSynch provides the default
 * implementation while SubLWorker and DefaultSubLWorker provide
 * asynchronous communications capabilities. Currently, SubLWorkerSynchs are one-shot,
 * i.e., a new SubLWorkerSynch needs to be created for every new communication.
 * SubLWorkerSynchs are cancelable, time-outable and provide means for incremental
 * return results.
 *  
 * <P>Example usage: <pre>
 * try {
 *    CycAccess access = new CycAccess("localhost", 3600);
 *    SubLWorkerSynch worker = new DefaultSubLWorkerSynch("(+ 1 1)", access);
 *    Object work = worker.getWork();
 *    System.out.println("Got worker: " + worker + "\nGot result: " + work + ".");
 *  } catch (Exception e) {
 *    e.printStackTrace();
 *  }
 * </pre>
 *
 * <p>Copyright 2004 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * @author tbrussea
 * @date March 17, 2004, 11:26 AM
 * @version $Id: SubLWorkerSynch.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface SubLWorkerSynch extends SubLWorker {
  
  /** This method starts communications with the Cyc server, waits for the work
   * to be performed, then returns the resultant work.
   * @throws IOException thown when there is a problem with the communications
   * protocol with the CycServer
   * @throws TimeOutException thrown if the worker takes to long to return results
   * @throws CycApiException thrown if any other error occurs
   * @return The work produced by this SubLWorkerSynch
   */  
  Object getWork() throws IOException, TimeOutException, CycApiException, OpenCycTaskInterruptedException;
  
}
