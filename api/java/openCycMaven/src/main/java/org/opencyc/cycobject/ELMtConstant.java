package org.opencyc.cycobject;

import org.opencyc.api.CycAccess;
import org.opencyc.api.CycObjectFactory;

/**
 * Provides the container for the ELMt CycConstant (Epistemlogical Level Microtheory
 * Constant).<p>
 *
 * @version $Id: ELMtConstant.java 138070 2012-01-10 19:46:08Z sbrown $
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

public class ELMtConstant extends CycConstant implements ELMt {
  
  static final long serialVersionUID = -2405506745680227189L;
  
  /** Privately creates a new instance of ELMtConstant 
   * deprecated
   */
  private ELMtConstant(CycConstant cycConstant) {
    super(cycConstant.getName(), cycConstant.getGuid());
  }
    
  /**
   * Returns a new ELMtConstant given a CycConstant.  Note, use the
   * factory method in the CycAccess to create these.
   */
  public static ELMtConstant makeELMtConstant(CycConstant cycConstant) {
    CycObjectFactory.removeCaches(cycConstant);
    ELMtConstant elmtConstant = new ELMtConstant(cycConstant);
    CycObjectFactory.addCycConstantCache(cycConstant);
    return elmtConstant;
  }
}
