/* $Id: SubLWorkerStatus.java 138070 2012-01-10 19:46:08Z sbrown $
 */

package org.opencyc.api;

//// Internal Imports

//// External Imports

/**
 * <P>SubLWorkerStatus is designed to be a type-safe enumeration
 * of the status of a SubLWorker.
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
 * @date March 26, 2004, 10:40 AM
 * @version $Id: SubLWorkerStatus.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class SubLWorkerStatus {
  
  //// Constructors
  
  /** Creates a new instance of SubLWorkerStatus.
   * @param name the name of this status
   */
  private SubLWorkerStatus(String name) { this.name = name; }
  
  //// Public Area
  
  /** Returns the name of this status as a String.
   * @return the name of this status as a String
   */  
  public String getName() { return name; }
  
   /** Returns the name of this status as a String.
   * @return the name of this status as a String
   */  
  public String toString() { return getName(); }
  
  /** Indicates that the SubLWorker has not yet been started. */
  public static final SubLWorkerStatus NOT_STARTED_STATUS =
    new SubLWorkerStatus("Not started");

  /** Indicates that the SubLWorker is currently processing
   * the SubL task.*/
  public static final SubLWorkerStatus WORKING_STATUS =
    new SubLWorkerStatus("Working");

  /** Indicates that the SubLWorker was canceled. */
  public static final SubLWorkerStatus CANCELED_STATUS =
    new SubLWorkerStatus("Canceled");

  /** Indicates that the SubLWorker was aborted. */
  public static final SubLWorkerStatus ABORTED_STATUS =
    new SubLWorkerStatus("Aborted");

  /** Indicates that the SubLWorker has finished processing 
   the SubL task normally. */
  public static final SubLWorkerStatus FINISHED_STATUS =
    new SubLWorkerStatus("Finished");

  /** Indicates that the SubLWorker has finished processing 
   the SubL task abnormally because an exception was thrown. */
  public static final SubLWorkerStatus EXCEPTION_STATUS =
    new SubLWorkerStatus("Exception");
    
  
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  private String name;
  
  //// Main
  
}
