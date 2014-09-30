package org.opencyc.cycobject;

//// External Imports
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.api.CycObjectFactory;
import static org.opencyc.api.SubLAPIHelper.makeSubLStmt;
import org.opencyc.xml.XMLStringWriter;
import org.opencyc.xml.XMLWriter;

/**
 * Provides the behavior and attributes of OpenCyc assertions.<p>
 * <p>
 * Assertions are communicated over the binary API using their Id number (an int).
 * The associated formula, microtheory, truth-value, direction, and remaining attributes are
 * is fetched later.
 *
 * @version $Id: CycAssertion.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stephen L. Reed
 * @author Dan Lipofsky
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
public class CycAssertion extends DefaultCycObject {

  /**
   * The name of the XML tag for this object.
   */
  public static final String cycAssertionXMLTag = "assertion";
  /**
   * The default indentation for printing objects to XML
   */
  public static int indentLength = 2;
  private static final CycSymbol ASSERTION_EL_FORMULA = CycObjectFactory.makeCycSymbol("assertion-el-formula");
  /** the assertion in HL form */
  private final CycList<CycFormulaSentence> negLits = new CycList<CycFormulaSentence>();
  private final CycList<CycFormulaSentence> posLits = new CycList<CycFormulaSentence>();
  private final CycList<CycList<CycFormulaSentence>> hlFormula =
          new CycList<CycList<CycFormulaSentence>>(negLits, posLits);
  /** the assertion mt */
  private CycObject mt;
  /**
   * When true, indicates that the assertion is invalid.
   */
  private boolean isInvalid = false;

  /** Constructs an assertion object. */
  private CycAssertion() {
  }

  /**
   * Constructs an assertion object given its HL formula (in conjunctive normal form) and assertion mt.
   *
   * @param hlFormula the assertion in HL (conjunctive normal) form
   * @param mt the assertion mt
   */
  public CycAssertion(CycList hlFormula, CycObject mt) {
    //// Preconditions
    assert hlFormula != null : "hlFormula cannot be null";
    assert hlFormula.size() == 2 : "hlFormula must be a doubleton";
    assert mt != null : "mt cannot be null";
    {
      Object newNegLits = hlFormula.get(0);
      if (CycObjectFactory.nil.equals(newNegLits)) {
        newNegLits = Collections.emptyList();
      }
      assert (newNegLits instanceof Iterable) : "hlFormula must contain lists of literals";
      for (final Object lit : (Iterable) newNegLits) {
        if (lit instanceof CycFormulaSentence) {
          this.negLits.add((CycFormulaSentence) lit);
        } else {
          this.negLits.add(new CycFormulaSentence((Iterable<? extends Object>) lit));
        }
      }
    }
    {
      Object newPosLits = hlFormula.get(1);
      if (CycObjectFactory.nil.equals(newPosLits)) {
        newPosLits = Collections.emptyList();
      }
      assert (newPosLits instanceof Iterable) : "hlFormula must contain lists of literals";
      for (final Object lit : (Iterable) newPosLits) {
        if (lit instanceof CycFormulaSentence) {
          this.posLits.add((CycFormulaSentence) lit);
        } else {
          this.posLits.add(new CycFormulaSentence((Iterable<? extends Object>) lit));
        }
      }
    }
    this.mt = mt;
  }

  /**
   * Constructs a GAF assertion object from a single (positive) literal and assertion mt.
   *
   * @param posLit the GAF literal
   * @param mt the assertion mt
   */
  public CycAssertion(CycFormulaSentence posLit, CycObject mt) {
    //// Preconditions
    assert mt != null : "mt cannot be null";
    this.posLits.add(posLit);
    this.mt = mt;
  }

  /** Constructs a the singleton invalid <tt>CycAssertion</tt> object. 
   * This should only be called from CycObjectFactory.
   *
   * @return the invalid cyc assertion
   */
  public static CycAssertion makeInvalidAssertion() {
    final CycAssertion cycAssertion = new CycAssertion();
    cycAssertion.isInvalid = true;
    return cycAssertion;
  }

  /**
   * Indicates whether the object is equal to this object.
   *
   * @return <tt>true</tt> if the object is equal to this object, otherwise
   * returns <tt>false</tt>
   */
  @Override
  public boolean equals(Object object) {
    if (!(object instanceof CycAssertion)) {
      return false;
    }
    CycAssertion that = (CycAssertion) object;
    if (this.isInvalid && that.isInvalid) {
      return true;
    } else if (!this.mt.equals(that.mt)) {
      return false;
    } else {
      return this.hlFormula.equals(that.hlFormula);
    }
  }

  /**
   * Returns a <tt>String</tt> representation of the <tt>CycAssertion</tt>.
   *
   * @return a <tt>String</tt> representation of the <tt>CycAssertion</tt>
   */
  @Override
  public String toString() {
    if (isInvalid) {
      return "INVALID-ASSERTION";
    } else {
      return hlFormula.cyclify();
    }
  }

  /**
   * Returns a cyclified string representation of the CycL assertion.
   * A cyclified string representation with escape chars is one where
   * constants have been prefixed with #$ and Strings have had an escape
   * character inserted before each character that needs to be escaped in SubL.
   *
   * @return a cyclified <tt>String</tt> with escape characters.
   */
  @Override
  public String cyclifyWithEscapeChars() {
    if (isInvalid) {
      return "INVALID-ASSERTION";
    } else {
      return hlFormula.cyclifyWithEscapeChars();
    }
  }

  /**
   * Returns this object in a form suitable for use as an <tt>String</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>String</tt> api expression value
   */
  @Override
  public String stringApiValue() {
    if (isInvalid) {
      return "INVALID-ASSERTION";
    } else {
      return "(find-assertion " + hlFormula.stringApiValue() + " " + mt.stringApiValue() + ")";
    }
  }

  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  @Override
  public Object cycListApiValue() {
    return this;
  }

  /**
   * Returns the EL formula for this assertion.
   *
   * @return the EL formula for this assertion
   */
  public CycFormulaSentence getELFormula(final CycAccess access) throws CycApiException, IOException, UnknownHostException {
    if (isGaf()) {
      return getGaf();
    } else {
      return getFormulaFromCyc(access);
    }
  }

  /**
   * Returns the HL formula (in conjunctive normal form) for this assertion.
   *
   * @return the HL formula for this assertion
   */
  public CycList getFormula() {
    return hlFormula;
  }

  private CycFormulaSentence getFormulaFromCyc(CycAccess access) throws CycApiException, IOException, UnknownHostException {
    String command = makeSubLStmt(ASSERTION_EL_FORMULA, this);
    return access.converseSentence(command);
  }

  /**
   * Returns the Ground Atomic Formula (gaf) for this assertion.
   *
   * @return the Ground Atomic Formula (gaf) for this assertion
   */
  public CycFormulaSentence getGaf() {
    if (!isGaf()) {
      return null;
    }
    //// Preconditions
    assert !hlFormula.isEmpty() : "hlFormula cannot be empty";
    assert negLits.isEmpty() : ((CycList) hlFormula).cyclify() + " negativeLiterals must be nil";
    if (posLits.size() == 1) {
      return posLits.first();
    }
    return null;
  }

  /**
   * Returns the Ground Atomic Formula (gaf) for this assertion.
   *
   * @param cycAccess the Cyc communications object
   * @return the Ground Atomic Formula (gaf) for this assertion
   * @deprecated cycAccess is not necessary.
   */
  public CycFormulaSentence getGaf(final CycAccess cycAccess) {
    return getGaf();
  }

  /** @return true iff this assertion's formula is both ground and atomic. */
  public boolean isGaf() {
    if (negLits.isEmpty()) {
      if (posLits.size() == 1) {
        return noVars(posLits.first());
      }
    }
    return false;
  }

  private boolean noVars(final Object term) {
    if (term instanceof CycVariable) {
      return false;
    } else if (term instanceof CycList) {
      for (final Object arg : (CycList) term) {
        if (!noVars(arg)) {
          return false;
        }
      }
      return true;
    } else {
      return true;
    }
  }

  /**
   * Returns the microtheory for this assertion.
   *
   * @return the microtheory for this assertion
   */
  public CycObject getMt() {
    return mt;
  }

  /**
   * Returns the XML representation of this object.
   *
   * @return the XML representation of this object
   */
  public String toXMLString() throws IOException {
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    toXML(xmlStringWriter, 0, false);
    return xmlStringWriter.toString();
  }

  /**
   * Prints the XML representation of the CycAssertion to an <code>XMLWriter</code>
   *
   * @param xmlWriter an <tt>XMLWriter</tt>
   * @param indent an int that specifies by how many spaces to indent
   * @param relative a boolean; if true indentation is relative, otherwise absolute
   */
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative)
          throws IOException {
    xmlWriter.printXMLStartTag(cycAssertionXMLTag, indent, relative, true);
    hlFormula.toXML(xmlWriter, indent, relative);
    mt.toXML(xmlWriter, indent, relative);
    xmlWriter.printXMLEndTag(cycAssertionXMLTag);
  }

  /**
   * Returns a list of all constants refered to by this CycObject.
   * For example, a CycConstant will return a List with itself as the
   * value, a nart will return a list of its functor and all the constants refered
   * to by its arguments, a CycList will do a deep search for all constants,
   * a symbol or variable will return the empty list.
   * @return a list of all constants refered to by this CycObject
   **/
  public List getReferencedConstants() {
    List result = null;
    if (getFormula() != null) {
      result = DefaultCycObject.getReferencedConstants(getFormula());
      if (getMt() != null) {
        result.addAll(getMt().getReferencedConstants());
      }
      return result;
    }
    if (getMt() != null) {
      result = DefaultCycObject.getReferencedConstants(getMt());
    }
    return (result == null) ? new ArrayList() : result;
  }

  public int compareTo(Object o) {
    if (!(o instanceof CycAssertion)) {
      return toString().compareTo(o.toString());
    }
    CycAssertion cao = (CycAssertion) o;
    int ret = this.getMt().compareTo(cao.getMt());
    if (ret != 0) {
      return ret;
    }
    return this.getFormula().compareTo(cao.getFormula());
  }
}







