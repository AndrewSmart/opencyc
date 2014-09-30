/*
 * CycWorkerEvent.java
 *
 * Created on March 22, 2002, 9:33 AM
 */
package org.opencyc.util;

import java.util.*;

/**
 * This is an event object for CycWorker events. It is currently
 * an unmodified subclass of EventObject, but more functionality may
 * be added in the future.
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
 * @author  tbrussea
 * @see CycWorker
 * @see CycWorkerListener
 * @see SwingWorker
 * @version $Id: CycWorkerEvent.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class CycWorkerEvent extends EventObject {
    
    /** 
     * Creates a new instance of CycWorkerEvent.
     * @param source The CycWorker that is generating this event.
     **/
    public CycWorkerEvent(Object source) {
        super(source);
    }

}
