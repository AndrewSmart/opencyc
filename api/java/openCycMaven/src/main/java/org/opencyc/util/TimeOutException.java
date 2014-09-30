package  org.opencyc.util;

/**
 * Implements an exception class for notification when a Cyc server 
 * communication has timed out. When this is thrown, the outstanding
 * task on the Cyc server is arborted.
 *
 * @version $Id: TimeOutException.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Eric E. Allen<br>
 *
 * <p>Copyright 2001 Cycorp, Inc., license is open source GNU LGPL.
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
 */
public class TimeOutException extends RuntimeException {
  
  /**
   * Construct a TimeOutException object with no 
   * specified message.
   */
  public TimeOutException () {
    super();
  }
  
  /**
  * Construct a TimeOutException object with a 
  * specified message.
  * @param message a message describing the exception.
  */
  public TimeOutException(String message) {
    super(message);
  }
  
  /**
   * Construct a TimeOutException object with a 
   * specified message and throwable.
   * @param message the message string
   * @param t the throwable that caused this exception
   */
  public TimeOutException(String message, Throwable t) {
    super(message, t);
  }
    
  /**
   * Construct a TimeOutException object with the 
   * specified throwable.
   * @param t the throwable that caused this exception
   */
  public TimeOutException(Throwable t) {
    super(t);
  }
  
}
