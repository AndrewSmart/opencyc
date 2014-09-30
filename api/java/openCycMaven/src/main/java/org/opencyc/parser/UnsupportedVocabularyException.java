package org.opencyc.parser;

//// Internal Imports
import org.opencyc.cycobject.CycConstant;

//// External Imports

/**
 * <P>Provides 
 *
 * @version $Id: UnsupportedVocabularyException.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class UnsupportedVocabularyException extends Exception {
  
  //// Constructors
  
  /** Creates a new instance of InvalidConstantName. */
  public UnsupportedVocabularyException(CycConstant invalidConstant) {
    this.invalidConstant = invalidConstant;
  }
  
  //// Public Area
  
  public String getMessage() {
    return "The following vocabulary is not supported: '" + invalidConstant.cyclify() + "'.";
  }
  
  public CycConstant getInvalidVocabulary() { return invalidConstant; }
  
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  private CycConstant invalidConstant;
  
  //// Main
  
}
