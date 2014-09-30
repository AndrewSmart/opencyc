package org.opencyc.cycobject;

//// External Imports
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.List;
import java.util.ListIterator;

//// Internal Imports
import org.opencyc.api.CycObjectFactory;
import org.opencyc.xml.XMLStringWriter;
import org.opencyc.xml.XMLWriter;

/**
 * This class implements the behavior and attributes of a
 * an OpenCyc NART (Non Atomic Reified Term).
 *
 * @version $Id: CycNart.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stefano Bertolo
 * @author Stephen L. Reed
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
public class CycNart extends CycFort implements CycNonAtomicTerm, Serializable {

  static final long serialVersionUID = -1344072553770319121L;
  /**
   * XML serialization tags.
   */
  public static final String natXMLtag = "nat";
  public static final String functorXMLtag = "functor";
  public static final String argXMLtag = "arg";
  /**
   * XML serialization indentation.
   */
  public static int indentLength = 2;
  private CycNaut formula;

  /**
   * Constructs a new incomplete <tt>CycNart</tt> object.
   */
  public CycNart() {
  }

  /**
   * Constructs a new unary <tt>CycNart</tt> object from the CycNaut formula
   *
   */
  public CycNart(final CycNaut formula) {
    this.formula = formula;
  }

  /**
   * Constructs a new unary <tt>CycNart</tt> object from the functor and
   * argument.
   *
   * @param functor a <tt>CycFort</tt> which is the functor of this
   * <tt>CycNart</tt> object.
   * @param argument an <tt>Object</tt> most typically a <tt>CycConstant</tt>
   * which is the single argument of this <tt>CycNart</tt> object.
   */
  public CycNart(CycFort functor, Object... arguments) {
    this.formula = new CycNaut(functor, arguments);
  }

  /**
   * Constructs a new <tt>CycNart</tt> object from the <tt>CycList</tt> object.
   *
   * @param cycList a list representation of the <tt>CycNart</tt>
   */
  public CycNart(CycList cycList) {
    if (cycList.size() == 0) {
      throw new RuntimeException("Cannot make a CycNart from an empty CycList");
    }
    if (!(cycList.first() instanceof CycFort)) {
      throw new RuntimeException("CycNart functor must be a CycFort " + cycList.cyclify());
    }
    this.formula = new CycNaut(cycList);
  }

  /** Constructs a the singleton invalid <tt>CycNart</tt> object. 
   * This should only be called from CycObjectFactory.
   *
   * @return the invalid nart
   */
  public static CycNart makeInvalidNart() {
    final CycNart cycNart = new CycNart();
    cycNart.isInvalid = true;
    return cycNart;
  }

  /**
   * Returns the given object if it is a <tt>CycNart</tt>, otherwise the object is expected to be
   * a <tt>CycList</tt> and a <tt>CycNart</tt> object is returned using the given
   * CycList representation.
   *
   * @param object the object to be coerced into a CycNart
   * @return the given object if it is a <tt>CycNart</tt>, otherwise the object is expected to be
   * a <tt>CycList</tt> and a <tt>CycNart</tt> object is returned using the given
   * CycList representation
   */
  public static CycNart coerceToCycNart(Object object) {
    if (object instanceof CycNart) {
      return (CycNart) object;
    }
    if (!(object instanceof CycList)) {
      throw new RuntimeException("Cannot coerce to CycNart " + object);
    }
    return new CycNart((CycList) object);
  }

  /**
   * Returns the functor of the <tt>CycNart</tt>.
   *
   * @return the functor of the <tt>CycNart</tt>
   */
  @Override
  public CycFort getFunctor() {
    return (CycFort) formula.getOperator();
  }

  @Override
  public CycNaut getFormula() {
    return formula;
  }

  @Override
  public int getArity() {
    return getFormula().getArity();
  }

  /**
   * Sets the functor of the <tt>CycNart</tt>.
   *
   * @param functor the <tt>CycFort</tt> functor object of the <tt>CycNart</tt>
   */
  public void setFunctor(CycFort functor) {
    formula.getArgs().set(0, functor);
  }

  /**
   * Returns the arguments of the <tt>CycNart</tt>.
   * Modifications to this list will be reflected back to the original CycNart.
   *
   * @return the arguments of the <tt>CycNart</tt>
   */
  public List getArguments() {
    return (List) formula.getArgs().rest();
  }

  public Object getArgument(final int argnum) {
    return formula.getArg(argnum);
  }

  /**
   * Sets the arguments of the <tt>CycNart</tt>.
   *
   * @param arguments the arguments of the <tt>CycNart</tt>
   */
  public void setArguments(CycList arguments) {
    formula.setArgs(arguments);
  }

  /**
   * Sets the specified argument of the <tt>CycNart</tt> to argument.
   *
   * @param argument
   */
  public void setArgument(final int argNum, Object argument) {
    formula.getArgs().set(argNum, argument);
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
   * Prints the XML representation of the <ttt>CycNart</tt> to an <tt>XMLWriter</tt>
   * It is supposed to look like this:<p>
   * <pre>
   * <nat>
   *  <functor>
   *   <constant>
   *    <guid>bd58a976-9c29-11b1-9dad-c379636f7270</guid>
   *    <name>FruitFn</name>
   *   </constant>
   *  </functor>
   *  <arg>
   *   <constant>
   *    <guid>bd58c19d-9c29-11b1-9dad-c379636f7270</guid>
   *    <name>AppleTree</name>
   *   </constant>
   *  </arg>
   * </nat>
   * </pre>
   *
   * The parameter [int indent] specifies by how many spaces the XML
   * output should be indented.<p>
   *
   * The parameter [boolean relative] specifies whether the
   * indentation should be absolute -- indentation with respect to
   * the beginning of a new line, relative = false -- or relative
   * to the indentation currently specified in the indent_string field
   * of the xml_writer object, relative = true.
   *
   */
  @Override
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative)
          throws IOException {
    xmlWriter.printXMLStartTag(natXMLtag, indent, relative, true);
    xmlWriter.printXMLStartTag(functorXMLtag, indentLength, true, true);
    this.getFunctor().toXML(xmlWriter, indentLength, true);
    xmlWriter.printXMLEndTag(functorXMLtag, -indentLength, true);
    ListIterator iterator = this.getArguments().listIterator();
    Object arg;
    while (iterator.hasNext()) {
      xmlWriter.printXMLStartTag(argXMLtag, 0, true, true);
      arg = iterator.next();
      // Use a shared method with CycList for arbitrary elements.
      CycList.toXML(arg, xmlWriter, indentLength, true);
      xmlWriter.printXMLEndTag(argXMLtag, 0, true);
    }
    xmlWriter.printXMLEndTag(natXMLtag, -indentLength, true);
  }

  /**
   * Returns a list representation of the OpenCyc NART.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  @Override
  public CycList toCycList() {
    return getFormula().toCycList();
  }

  /**
   * Returns a list representation of the OpenCyc NART and expands any embedded NARTs as well.
   *
   * @return a <tt>CycList</tt> representation of the OpenCyc NART.
   */
  @Override
  public CycList toDeepCycList() {
    return getFormula().toDeepCycList();
  }

  /**
   * Returns a string representation of the OpenCyc NART.
   *
   * @return a <tt>String</tt> representation of the OpenCyc NART.
   */
  public String toString() {
    if (isInvalid) {
      return "INVALID-NART";
    }
    return getFormula().toString();
  }

  /**
   * Returns a cyclified string representation of the OpenCyc NART.
   * Embedded constants are prefixed with ""#$".
   *
   * @return a cyclified <tt>String</tt>.
   */
  public String cyclify() {
    if (isInvalid) {
      return "INVALID-NART";
    }
    return getFormula().cyclify();
  }

  /**
   * Returns a cyclified string representation of the OpenCyc NART.
   * Embedded constants are prefixed with "#$".
   * Embedded strings have escape characters inserted in front of any double-quote
   *   or backslash characters which they contain.
   *
   * @return a cyclified <tt>String</tt>.
   */
  public String cyclifyWithEscapeChars() {
    if (isInvalid) {
      return "INVALID-NART";
    }
    return getFormula().cyclifyWithEscapeChars();
  }

  /**
   * Returns this object in a form suitable for use as an <tt>String</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>String</tt> api expression value
   */
  public String stringApiValue() {
    return "(canonicalize-term '" + cyclifyWithEscapeChars() + ")";
  }

  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  public Object cycListApiValue() {
    CycList apiValue = new CycList();
    apiValue.add(CycObjectFactory.makeCycSymbol("canonicalize-term"));
    apiValue.addQuoted(this.toCycList());
    return apiValue;
  }

  /**
   * Returns a string representation of the <ttt>CycNart</tt> with the guid in place
   * of the constant name.
   *
   * @return a <tt>String</tt> representation of the <ttt>CycNart</tt> with <tt>Guid</tt>
   * external forms in place of the <tt>CycConstant</tt> names.
   */
  public String metaGuid() {
    final CycFort functor = getFunctor();
    String functorGuid =
            (functor instanceof CycConstant
            ? ((CycConstant) functor).getGuid().toString() : ((CycNart) functor).metaGuid());
    ListIterator iterator = getArguments().listIterator();
    StringBuffer result = new StringBuffer("(");
    result.append(functorGuid);
    Object arg;
    String argGuid;
    while (iterator.hasNext()) {
      arg = iterator.next();
      if (arg instanceof CycConstant) {
        argGuid = ((CycConstant) arg).getGuid().toString();
      } else if (arg instanceof CycNart) {
        argGuid = ((CycNart) arg).metaGuid();
      } else {
        argGuid = (String) arg;
      }
      result.append(" ");
      result.append(argGuid);
    }
    return result.append(")").toString();
  }

  /**
   * Returns a metaName representation of the <tt>CycNart</tt>.
   *
   * @return a <tt>String</tt> metaName representation
   */
  public String metaName() {
    String functorName =
            (this.getFunctor() instanceof CycConstant
            ? ((CycConstant) this.getFunctor()).getName()
            : ((CycNart) this.getFunctor()).metaName());
    ListIterator iterator = this.getArguments().listIterator();
    StringBuffer result = new StringBuffer("(");
    result.append(functorName);
    Object arg;
    String argName;
    while (iterator.hasNext()) {
      arg = iterator.next();
      if (arg instanceof CycConstant) {
        argName = ((CycConstant) arg).getName();
      } else if (arg instanceof CycNart) {
        argName = ((CycNart) arg).metaName();
      } else {
        argName = (String) arg;
      }
      result.append(" ");
      result.append(argName);
    }
    return result.append(")").toString();
  }

  /**
   * Return a hash value for this object.
   *
   * @return a hash value for this object
   */
  public int hashCode() {
    return formula.hashCode();
  }

  /**
   * Returns <tt>true</tt> some object equals this <tt>CycNart</tt>
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  public boolean equals(Object object) {
    if (!(object instanceof CycNart)) {
      return false;
    }
    CycNart thatNart = (CycNart) object;
    return formula.equals(thatNart.formula);
  }

  /**
   * Returns <tt>true</tt> some object equals this <tt>CycNart</tt>
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  public boolean equalsAtEL(Object object) {
    return getFormula().equalsAtEL(object);
  }

  /**
   * Returns true if the functor and arguments are instantiated.
   *
   * @return true if the functor and arguments are instantiated
   */
  public boolean hasFunctorAndArgs() {
    return formula.getArity() > 0;
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
    return getFormula().getReferencedConstants();
  }

  //// serialization implementation
  private void writeObject(ObjectOutputStream stream) throws java.io.IOException {
    stream.defaultWriteObject();
    stream.writeObject(getFunctor());
    stream.writeObject(getArguments());
  }

  private void readObject(ObjectInputStream stream) throws java.io.IOException,
          java.lang.ClassNotFoundException {
    stream.defaultReadObject();
    final CycFort functor = (CycFort) stream.readObject();
    final CycList arguments = (CycList) stream.readObject();
    formula = new CycNaut(functor, arguments);
  }
}

