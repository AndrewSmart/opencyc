package org.opencyc.parser;

//// Internal Imports

//// External Imports
import java.util.*;

/**
 * <P>Thrown whenever the CycL parser tries to parse a constant
 * by its name, and the Cyc server does not already have a constant
 * by that name defined.
 *
 * @version $Id: InvalidConstantNameException.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class InvalidConstantNameException extends Exception {
  
  //// Constructors
  
  /** Creates a new instance of InvalidConstantName. */
  public InvalidConstantNameException(String invalidConstantName) {
    this.invalidConstantNames = new ArrayList();
    this.invalidConstantNames.add(invalidConstantName);
  }
  
  public InvalidConstantNameException() {
  }
  
  //// Public Area
  
  public String getMessage() {
    StringBuffer buf = new StringBuffer("Invalid constant name(s): ");
    if (getInvalidConstantNames() != null) {
      for (Iterator iter = invalidConstantNames.iterator(); iter.hasNext(); ) {
        buf.append("#$").append(iter.next());
        if (iter.hasNext()) {
          buf.append(", ");
        }
      }
    }
    return buf.toString();
  }
  
  public void addInvalidConstantName(String constantName) {
    if (invalidConstantNames == null) {
      invalidConstantNames = new ArrayList();
    }
    invalidConstantNames.add(constantName);
  }
  
  public List getInvalidConstantNames() { 
    return invalidConstantNames;
  }
  
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  private List invalidConstantNames;
  
  //// Main
  
}
