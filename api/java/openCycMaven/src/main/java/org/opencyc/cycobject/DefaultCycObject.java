/*
 * DefaultCycObject.java
 *
 * Created on May 10, 2002, 11:35 AM
 */
package org.opencyc.cycobject;

import java.io.IOException;
import java.util.*;
import java.math.BigInteger;
import org.opencyc.api.CompactHLIDConverter;
import org.opencyc.xml.XMLWriter;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycObjectFactory;
import org.opencyc.util.StringUtils;
import org.opencyc.inference.InferenceParameters;

/**
 * This is the default implementation of a CycL object.
 *
 * @version $Id: DefaultCycObject.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author  tbrussea
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
 *
 */
public abstract class DefaultCycObject implements CycObject {

  /**
   * Field for storing the name of the XML tag for CycConstant objects
   */
  public static final String objectXMLTag = "cycl-object";

  /**
   * Returns a cyclified string representation of the CycL object.
   * By default, just returns the result of calling "toString()".
   * A cyclified string representation is one where constants have been
   * prefixed with #$.
   *
   * @return a cyclified <tt>String</tt>.
   */
  public String cyclify() {
    return toString();
  }

  /**
   * Returns a cyclified string representation of the CycL object.
   * By default, just returns the result of calling "cyclify()".
   * A cyclified string representation with escape chars is one where
   * constants have been prefixed with #$ and Strings have had an escape
   * character inserted before each character that needs to be escaped in SubL.
   *
   * @return a cyclified <tt>String</tt> with escape characters.
   */
  public String cyclifyWithEscapeChars() {
    return cyclify();
  }

  /**
   * Returns a cyclified string representation of the OpenCyc object.
   * Embedded constants are prefixed with "#$".  Embedded quote chars in strings
   * are escaped.
   *
   * @return a <tt>String</tt> representation in cyclified form.
   *
   */
  public static String cyclifyWithEscapeChars(Object obj) {
    return cyclifyWithEscapeChars(obj, false);
  }

  /**
   * Returns a cyclified string representation of the OpenCyc object.
   * Embedded constants are prefixed with "#$".  Embedded quote chars in strings
   * are escaped.
   *
   * @return a <tt>String</tt> representation in cyclified form.
   *
   */
  public static String cyclifyWithEscapeChars(Object obj, boolean isApi) {
    if ((obj == null) || (!isCycLObject(obj))) {
      throw new RuntimeException("Cannot cyclify (escaped): '" + obj + "'.");
    }
    if (obj instanceof CycObject) {
      return ((CycObject)obj).cyclifyWithEscapeChars();
    }
    if (obj instanceof String) {
      String str = (String) obj;
      if (StringUtils.is7BitASCII(str)) {
        return "\"" + StringUtils.escapeDoubleQuotes(str) + "\"";
      } else {
        return StringUtils.unicodeEscaped(str, isApi);
      }
    }
    if (obj instanceof Character) {
      return StringUtils.escapeDoubleQuotes(cyclify(obj));
    }
    if (obj instanceof Boolean) {
      return (obj == Boolean.FALSE) ? "nil" : "t";
    }
    return obj.toString();
  }

  /**
   * Returns a cyclified string representation of the given <tt>Object</tt>.
   * Embedded constants are prefixed with "#$".
   *
   * @return a <tt>String</tt> representation in cyclified form.
   *
   */
  public static String cyclify(Object obj) {
    if (obj == null) {
      throw new RuntimeException("Cannot cyclify null obj");
    } else if (!isCycLObject(obj)) {
      throw new RuntimeException("Cannot cyclify: '" + obj + "' (" + obj.getClass().getName() + ").");
    }
    if (obj instanceof CycObject) {
      return ((CycObject) obj).cyclify();
    }
    if (obj instanceof String) {
      return "\"" + (String) obj + "\"";
    }
    if (obj instanceof Character) {
      // @hack -- do better job of this. Need to support other non-printable characters!!!
      Character theChar = (Character) obj;
      if (theChar == ' ') {
        return "#\\Space";
      } else if (theChar == '\n') {
        return "#\\Newline";
      } else if (theChar == '\r') {
        return "#\\Return";
      } else if (theChar == '\t') {
        return "#\\Tab";
      }
      if (Character.isWhitespace(theChar)) {
        throw new IllegalArgumentException("Don't know how to trasmit the whitespace character: "
                + (int) theChar.charValue());
      }
      return "#\\" + obj;
    }
    return obj.toString();
  }

  public static List getReferencedConstants(Object obj) {
    if (obj == null) {
      return new ArrayList();
    }
    if ((obj == null) || (!isCycLObject(obj))) {
      throw new RuntimeException("Got an object that is not a valid CycL term: '" + obj + "' (" + obj.getClass().getName() + ").");
    }
    if (!(obj instanceof CycObject)) {
      return new ArrayList();
    }
    return ((CycObject) obj).getReferencedConstants();
  }
  private static final List emptyList = Arrays.asList(new Object[0]);

  public List getReferencedConstants() {
    return emptyList;
  }

  /**
   * Returns the given <tt>Object</tt> in a form suitable for use as a <tt>String</tt> api expression value.
   *
   * @return the given <tt>Object</tt> in a form suitable for use as a <tt>String</tt> api expression value
   */
  public static String stringApiValue(Object obj) {
    if ((obj == null) || (!isCycLObject(obj))) {
      throw new RuntimeException(obj + " cannot be converted to a form suitable for use as a String api expression value.");
    }
    if (obj instanceof CycObject) {
      return ((CycObject) obj).stringApiValue();
    }
    if (obj instanceof InferenceParameters) {
      return ((InferenceParameters) obj).stringApiValue();
    }
    return cyclifyWithEscapeChars(obj, true);
  }

  public static String stringApiValue(boolean val) {
    return (val == false) ? "nil" : "t";
  }

  public static String currentOpenCycURIForHLID(String id) {
    return "http://sw.opencyc.org/concept/" + id;
  }

  public static String currentOpenCycURIForCycLString(String cycl) {
    return "http://sw.opencyc.org/concept/en/" + cycl;
  }

  /**
   * Returns true iff the given object is an object than can be contained
   * in a CycL expression.
   *
   * @return true iff the given object is an object than can be contained
   * in a CycL expression
   */
  public static boolean isCycLObject(Object obj) {
    return (obj instanceof CycObject
            || obj instanceof InferenceParameters ||//@hack this is wrong GUIDs are not CycL objects --APB
            obj instanceof Boolean
            || obj instanceof String
            || obj instanceof Integer
            || obj instanceof Character
            || obj instanceof Long
            || obj instanceof BigInteger
            || obj instanceof Guid || //@hack this is wrong GUIDs are not CycL objects --APB
            obj instanceof Float
            || obj instanceof Double);
  }

  /**
   * Returns a pretty CycL representation of this object.
   *
   * @return a string representation without causing additional api calls to determine
   * constant names
   */
  public static String toPrettyString(Object obj) {
    if (obj instanceof String) {
      return "\"" + obj.toString() + "\"";
    } else if (obj instanceof CycList) {
      return ((CycList) obj).toPrettyString("");
    }
    return obj.toString();
  }

  /**
   * Returns this object in a form suitable for use as an <tt>String</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>String</tt> api expression value
   */
  public String stringApiValue() {
    return cyclifyWithEscapeChars();
  }

  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  public Object cycListApiValue() {
    return cyclify();
  }

  /**
   * Prints the XML representation of the CycConstant to an <code>XMLWriter</code>
   *
   * @param xmlWriter an <tt>XMLWriter</tt>
   * @param indent an int that specifies by how many spaces to indent
   * @param relative a boolean; if true indentation is relative, otherwise absolute
   */
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative)
          throws IOException {
    xmlWriter.printXMLStartTag(objectXMLTag, indent, relative, true);
    xmlWriter.print(stringApiValue());
    xmlWriter.printXMLEndTag(objectXMLTag, -indent, true);
  }

  public static final String toCompactExternalId(String cycObject) throws IOException {
    return CompactHLIDConverter.converter().toCompactHLId(cycObject);
  }

  public static final String toCompactExternalId(Number cycObject) throws IOException {
    return CompactHLIDConverter.converter().toCompactHLId(cycObject);
  }

  public static final String toCompactExternalId(Object cycObject, CycAccess access)
          throws IOException {
    if ((cycObject == null) || (!DefaultCycObject.isCycLObject(cycObject))) {
      throw new IllegalArgumentException(cycObject + "is not a valid CycL object.");
    }
    if (cycObject instanceof Number) {
      return CompactHLIDConverter.converter().toCompactHLId((Number) cycObject);
    }
    if (cycObject instanceof CycNumber) {
      return CompactHLIDConverter.converter().toCompactHLId(((CycNumber)cycObject).getNumber());
    }
    if (cycObject instanceof String) {
      return CompactHLIDConverter.converter().toCompactHLId((String) cycObject);
    }
    Object result = cycObjectToCompactExternalIdCache.get(cycObject);
    if (result != null) {
      return (String) result;
    }
    result = access.converseObject("(compact-hl-external-id-string " + DefaultCycObject.stringApiValue(cycObject) + ")");
    if (!(result instanceof String)) {
      throw new RuntimeException("Got invalid result: " + result);
    }
    cycObjectToCompactExternalIdCache.put(cycObject, (String) result);
    return (String) result;
  }

  public static final Object fromCompactExternalId(String compactExternalId, CycAccess access)
          throws IOException {
    if ((compactExternalId == null) || ("".equals(compactExternalId))) {
      throw new IllegalArgumentException(compactExternalId + "is not a valid compact external id.");
    }
    // @todo support CompactHLIDConverter.converter() once we can simply identify
    // if a compact external id is a String or Number
    Object result = compactExternalIdToCycObjectCache.get(compactExternalId);
    if (result != null) {
      return result;
    }
    if (CompactHLIDConverter.converter().isNumberCompactHLId(compactExternalId)) {
      Number num = (Number) CompactHLIDConverter.converter().fromCompactHLId(compactExternalId);
      result = CycObjectFactory.makeCycNumber(num);
    } else {
      result = access.converseObject("(find-cycl-object-by-compact-hl-external-id-string " + DefaultCycObject.stringApiValue(compactExternalId) + ")");
    }
    compactExternalIdToCycObjectCache.put(compactExternalId, result);
    return result;
  }
  //@todo provide separate cache per CycAccess
  private static final int MAX_ENTRIES = 20000;
  private static final LinkedHashMap<String, Object> compactExternalIdToCycObjectCache = new LinkedHashMap() {

    protected boolean removeEldestEntry(Map.Entry eldest) {
      return size() > MAX_ENTRIES;
    }
  };
  private static final LinkedHashMap<Object, String> cycObjectToCompactExternalIdCache = new LinkedHashMap() {

    protected boolean removeEldestEntry(Map.Entry eldest) {
      return size() > MAX_ENTRIES;
    }
  };

  /**
   * Returns whether the given Object represents a Cyc Collection.
   *
   * @return whether the given Object represents a Cyc Collection.
   */
  public static boolean isCollection(Object term, CycAccess cycAccess) throws IOException {
    return cycAccess.isCollection(term);
  }

  public static int getCycObjectType(Object object) {
    if (object instanceof ByteArray) {
      return CYCOBJECT_BYTEARRAY;
    } else if (object instanceof CycAssertion) {
      return CYCOBJECT_CYCASSERTION;
    } else if (object instanceof CycFort) {
      return CYCOBJECT_CYCFORT;
    } else if (object instanceof CycSymbol) {
      return CYCOBJECT_CYCSYMBOL;
    } else if (object instanceof CycVariable) {
      return CYCOBJECT_CYCVARIABLE;
    } else if (object instanceof CycList) {
      return CYCOBJECT_CYCLIST;
    } else if (object instanceof Double) {
      return CYCOBJECT_DOUBLE;
    } else if (object instanceof Float) {
      return CYCOBJECT_FLOAT;
    } else if (object instanceof Guid) {
      return CYCOBJECT_GUID;
    } else if (object instanceof Integer) {
      return CYCOBJECT_INTEGER;
    } else if (object instanceof Long) {
      return CYCOBJECT_LONG;
    } else if (object instanceof BigInteger) {
      return CYCOBJECT_BIGINTEGER;
    } else if (object instanceof String) {
      return CYCOBJECT_STRING;
    } else {
      return CYCOBJECT_UNKNOWN;
    }
  }
}














