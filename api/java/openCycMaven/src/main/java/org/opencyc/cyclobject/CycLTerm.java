package org.opencyc.cyclobject;

import org.opencyc.xml.XMLWriter;

import java.io.Serializable;
import java.io.IOException;

import java.util.List;

/*****************************************************************************
 * KB comment for #$CycLTerm as of 2002/05/07:<p>
 *
 * The collection of all syntactically well-formed expressions in the CycL
 * language that can be used as terms, i.e. that can be combined with other
 * expressions to form non-atomic terms or formulas.  Since the grammar of the
 * CycL language allows any CycL expression to be used as a term, #$CycLTerm and
 * #$CycLExpression are coextensional collections.  Note that, as with most
 * #$CycLExpressionTypes, #$CycLTerm, is a #$quotedCollection (q.v.).<p>
 * 
 * @version $Id: CycLTerm.java 138070 2012-01-10 19:46:08Z sbrown $
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
 *****************************************************************************/
public interface CycLTerm extends Serializable {
   
  boolean isOpen();
  boolean isAtom();
  boolean isAtomic();
  boolean isFormula();
  boolean isSentence();
  boolean isDenotational();
  boolean isConstant();
  boolean isLiteral();
  boolean isCharacterLiteral();
  boolean isStringLiteral();
  boolean isRealNumberLiteral();
  boolean isSymbolLiteral();
  boolean isAssertion();
  boolean isIndexed();
  boolean isReified();
  boolean isReifiable();
  boolean isRepresented();
  boolean isAskable();
  boolean isAssertible();
  boolean isVariable();
  boolean isGAF();
  boolean isEL();
  boolean isHL();

  abstract String cyclify();

  abstract String toString();

  abstract String toXMLString() throws IOException;

  abstract void toXML(XMLWriter xmlWriter, int indent, boolean relative) 
    throws IOException;
  
  boolean equals(Object object);

  int hashCode();

}
