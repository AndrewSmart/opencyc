package org.opencyc.cycobject;

//// External Imports:
import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;

//// Internal Imports
import org.opencyc.api.CycObjectFactory;
import org.opencyc.xml.TextUtil;
import org.opencyc.xml.XMLStringWriter;
import org.opencyc.xml.XMLWriter;

/**
 * Provides the behavior and attributes of an OpenCyc symbol, typically used
 * to represent api function names, and non <tt>CycConstant</tt> parameters.
 *
 * @version $0.1$
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
public class CycSymbol extends DefaultCycObject implements Serializable {

  private boolean shouldQuote = true;
  private boolean isSpecialPackageName = false;
  private boolean isSpecialSymbolName = false;
  private String packageName = "";
  /**
   * The symbol represented as a <tt>String</tt>.
   */
  private String symbolName;
  /**
   * The name of the XML tag for this object.
   */
  public static final String cycSymbolXMLTag = "symbol";
  public static final String KEYWORD_PACKAGE = "KEYWORD";
  public static final String PACKAGE_SEPARATOR = ":";
  public static final String SYMBOL_NAME_QUOTE = "|";

  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   * Note this is ta case-sensitive constructor.
   * Make sure to upperCase the package and symbol names if they
   * should not be case sensitive (which is typically the case)
   * before calling this constructor.
   *
   * @param packageName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   * @param symbolName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   */
  public CycSymbol(String packageName, String symbolName) {
    this(packageName, symbolName, true);
  }

  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   * Note this is a case-sensitive constructor.
   * Make sure to upperCase the package and symbol names if they
   * should not be case sensitive (which is typically the case)
   * before calling this constructor.
   *
   * @param packageName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   * @param symbolName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   */
  public CycSymbol(String packageName, String symbolName, boolean shouldQuote) {
    if ((symbolName == null) || (symbolName.equals(""))) {
      throw new IllegalArgumentException("Got null symbol name.");
    }
    this.shouldQuote = shouldQuote;
    int sepLength = PACKAGE_SEPARATOR.length();
    int quoteLength = SYMBOL_NAME_QUOTE.length();
    if (symbolName.startsWith(PACKAGE_SEPARATOR)) {
      symbolName = symbolName.substring(sepLength, symbolName.length());
      packageName = KEYWORD_PACKAGE;
    }
    if (packageName == null) {
      packageName = "";
    }
    if ((symbolName.length() > (2 * quoteLength))
            && (symbolName.startsWith(SYMBOL_NAME_QUOTE))
            && (symbolName.endsWith(SYMBOL_NAME_QUOTE))) {
      symbolName = symbolName.substring(quoteLength, symbolName.length() - quoteLength);
    }
    if ((packageName.length() > (2 * quoteLength))
            && (packageName.startsWith(SYMBOL_NAME_QUOTE))
            && (packageName.endsWith(SYMBOL_NAME_QUOTE))) {
      packageName = packageName.substring(quoteLength, packageName.length() - quoteLength);
    }
    if ((!(isValidSymbolName(packageName)))
            || (!canonicalizeName(packageName).equals(packageName))) {
      isSpecialPackageName = true;
    }
    if ((!(isValidSymbolName(symbolName)))
            || (!canonicalizeName(symbolName).equals(symbolName))) {
      isSpecialSymbolName = true;
    }
    this.symbolName = symbolName;
    this.packageName = packageName;
  }

  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   *
   * @param symbolName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   */
  public CycSymbol(String symbolName, boolean shouldQuote) {
    this(null, canonicalizeName(symbolName), shouldQuote);
  }

  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   *
   * @param symbolName the <tt>String</tt> name of the <tt>CycSymbol</tt>.
   */
  public CycSymbol(String symbolName) {
    this(null, canonicalizeName(symbolName));
  }

  /** @return the canonical symbol name for symbolNameAnyCase */
  public static String canonicalizeName(final String symbolNameAnyCase) {
    return symbolNameAnyCase.toUpperCase(Locale.ENGLISH);
  }

  /**
   * Returns the string representation of the <tt>CycSymbol</tt>
   *
   * @return the representation of the <tt>CycSymbol</tt> as a <tt>String</tt>
   */
  @Override
  public String toString() {
    if (isKeyword()) {
      return PACKAGE_SEPARATOR + getSymbolNamePrecise();
    } else {
      return getSymbolNamePrecise();
    }
  }

  public String toFullString() {
    return toFullString(null);
  }

  public String toCanonicalString() {
    return canonicalizeName(toString());
  }

  public String toFullString(String relativePackageName) {
    if (packageName.equals(relativePackageName) || isKeyword()) {
      return toString();
    }
    return toFullStringForced();
  }

  public String toFullStringForced() {
    return getPackageNamePrecise() + PACKAGE_SEPARATOR + getSymbolNamePrecise();
  }

  public boolean toBoolean() {
    if (toCanonicalString().equals("T")) {
      return true;
    } else {
      return false;
    }
  }

  public String getSymbolName() {
    return symbolName;
  }

  public String getPackageName() {
    return packageName;
  }

  public String getSymbolNamePrecise() {
    return getSymbolStringRep(symbolName, isSpecialSymbolName);
  }

  public String getPackageNamePrecise() {
    return getSymbolStringRep(packageName, isSpecialPackageName);
  }

  private static final String getSymbolStringRep(String str, boolean isSpecial) {
    if (isSpecial) {
      return SYMBOL_NAME_QUOTE + str + SYMBOL_NAME_QUOTE;
    }
    return str;
  }

  /**
   * Returns <tt>true</tt> iff some object equals this <tt>CycSymbol</tt>
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  public boolean equals(Object object) {
    if (object == this) {
      return true;
    }
    if ((!(object instanceof CycSymbol)) || (object == null)) {
      return false;
    }
    return ((CycSymbol) object).toFullStringForced().equals(toFullStringForced());
  }

  /**
   * Provides the hash code appropriate for this object.
   *
   * @return the hash code appropriate for this object
   */
  public int hashCode() {
    return toFullStringForced().hashCode();
  }

  /**
   * Returns <tt>true</tt> iff this symbol is a SubL keyword.
   *
   * @return <tt>true</tt> iff this symbol is a SubL keyword
   */
  public boolean isKeyword() {
    return KEYWORD_PACKAGE.equals(packageName);
  }

  /**
   * Returns <tt>true</tt> iff this symbol is a SubL keyword.
   *
   * @return <tt>true</tt> iff this symbol is a SubL keyword
   */
  public boolean shouldQuote() {
    return shouldQuote;
  }

  /** Returns a string suitable for use within an API call (i.e. quoted).
   *
   * @return a string suitable for use within an API call (i.e. quoted)
   */
  public String stringApiValue() {
    if (isKeyword() || (!shouldQuote())) {
      return toString();
    }
    return "(QUOTE " + toString() + ")";
  }

  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  public Object cycListApiValue() {
    if (isKeyword() || (!shouldQuote())) {
      return this;
    }
    return CycList.makeCycList(CycObjectFactory.quote, this);
  }

  /**
   * Compares this object with the specified object for order.
   * Returns a negative integer, zero, or a positive integer as this
   * object is less than, equal to, or greater than the specified object.
   *
   * @param object the reference object with which to compare.
   * @return a negative integer, zero, or a positive integer as this
   * object is less than, equal to, or greater than the specified object
   */
  public int compareTo(Object object) {
    if (!(object instanceof CycSymbol)) {
      throw new ClassCastException("Must be a CycSymbol object");
    }
    return toFullStringForced().compareTo(((CycSymbol) object).toFullStringForced());
  }

  /**
   * Returns <tt>true</tt> iff the given string is a valid symbol name.
   *
   * @param string the name to be tested
   * @return <tt>true</tt> iff the given string is a valid symbol name
   */
  public static boolean isValidSymbolName(String string) {
    for (int i = 0; i < string.length(); i++) {
      char c = string.charAt(i);
      if (!(Character.isLetterOrDigit(c)
              || c == '`'
              || c == '-'
              || c == '_'
              || c == '*'
              || c == '?'
              || c == '+'
              || c == '>'
              || c == '<'
              || c == '='
              || c == '/'
              || c == '&'
              || c == ':')) {
        return false;
      }
    }
    return true;
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
   * Prints the XML representation of the CycSymbol to an <code>XMLWriter</code>
   *
   * @param xmlWriter an <tt>XMLWriter</tt>
   * @param indent an int that specifies by how many spaces to indent
   * @param relative a boolean; if true indentation is relative, otherwise absolute
   */
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative)
          throws IOException {
    xmlWriter.printXMLStartTag(cycSymbolXMLTag, indent, relative, false);
    xmlWriter.print(TextUtil.doEntityReference(toString()));
    xmlWriter.printXMLEndTag(cycSymbolXMLTag);
  }
}
