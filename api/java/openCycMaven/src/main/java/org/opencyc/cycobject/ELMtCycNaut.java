package org.opencyc.cycobject;

import java.util.List;

/**
 * Provides the container for the ELMt NAUT (Epistemlogical Level Microtheory
 * Non Atomic Un-reified Term).<p>
 *
 * @version $Id: ELMtCycNaut.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau
 *
 * <p>Copyright 2003 Cycorp, Inc., license is open source GNU LGPL.
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

public class ELMtCycNaut extends CycNaut implements ELMt {
  
  /** Creates a new instance of ELMtCycNaut */
  private ELMtCycNaut(List terms) {
    super(terms);
  }
  
  /**
   * Returns a new ELMtCycNaut.  Note, use the
   * factory method in the CycAccess to create these.
   */
  public static ELMtCycNaut makeELMtCycNaut(List terms) {
    return new ELMtCycNaut(terms);
  }
  /**
   * Returns this object in a form suitable for use as an <tt>String</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>String</tt> api expression value
   */
  //@ToDo uncomment this when (list* issues has been resolved in cyclify() of CycList
  /*public String stringApiValue() {
    return "'" + super.stringApiValue();
  }*/

}
