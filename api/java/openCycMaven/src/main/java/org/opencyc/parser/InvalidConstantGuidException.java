package org.opencyc.parser;

//// Internal Imports
import org.opencyc.cycobject.*;

//// External Imports
import java.util.*;

/**
 * <P>Thrown whenever the CycL parser tries to parse a constant
 * by its Guid, and the Cyc server does not already have a constant
 * by that Guid defined.
 *
 * @version $Id: InvalidConstantGuidException.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau
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
public class InvalidConstantGuidException extends Exception {
  
  //// Constructors
  
  /** Creates a new instance of InvalidConstantName. */
  public InvalidConstantGuidException(Guid invalidConstantGuid) {
    this.invalidConstantGuids = new ArrayList();
    this.invalidConstantGuids.add(invalidConstantGuid);
  }
  
  public InvalidConstantGuidException() {
  }
  
  //// Public Area
  
  public String getMessage() {
    StringBuffer buf = new StringBuffer("Invalid constant GUID(s): ");
    if (getInvalidConstantNames() != null) {
      for (Iterator iter = getInvalidConstantNames().iterator(); iter.hasNext(); ) {
        buf.append("#G").append(iter.next());
        if (iter.hasNext()) {
          buf.append(", ");
        }
      }
    }
    return buf.toString();
  }
  
  public void addInvalidConstantGuid(Guid invalidConstantGuid) {
    if (invalidConstantGuids == null) {
      invalidConstantGuids = new ArrayList();
    }
    invalidConstantGuids.add(invalidConstantGuid);
  }
  
  public List getInvalidConstantNames() { 
    return invalidConstantGuids;
  }
  
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  private List invalidConstantGuids;
  
  //// Main
  
}
