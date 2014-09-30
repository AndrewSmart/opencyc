/*
 * CycWorkerListener.java
 *
 * Created on March 22, 2002, 9:25 AM
 */
package org.opencyc.util;

import java.util.*;

/**
 * This is an interface to be used by classes that wishes to listen
 * to a CycWorker's events.
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
 * @see CycWorker
 * @see CycWorkerEvent
 * @see SwingWorker
 * @author  tbrussea
 * @version $Id: CycWorkerListener.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public interface CycWorkerListener extends EventListener {
    
  /** 
   * Called when the CycWorker is started. 
   * @param evt The CycWorkerEvent object.
   **/
  void notifyWorkerStarted(CycWorkerEvent evt);
    
  /** 
   * Called when the CycWorker is interrupted. 
   * @param evt The CycWorkerEvent object.
   **/
  void notifyWorkerInterrupted(CycWorkerEvent evt);
    
  /** 
   * Called when the CycWorker is finished. 
   * @param evt The CycWorkerEvent object.
   **/
  void notifyWorkerFinished(CycWorkerEvent evt);
}
