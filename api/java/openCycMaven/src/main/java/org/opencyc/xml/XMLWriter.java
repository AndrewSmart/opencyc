package org.opencyc.xml;

import java.util.ListIterator;
import java.io.Writer;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.opencyc.util.Pair;

/**
 * Implements an XMLWriter with facilities for surrounding data
 * elements with tags appropriately named and indented.<p>
 *
 * @version $Id: XMLWriter.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stefano Bertolo
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
public abstract class XMLWriter {

  /**
   * The default indentation for strings printed to the <code>XMLPrinter</code>.
   */
  static final String baseIndentString = "";
  /**
   * A <code>Map</code> that records which XML entity references need to be
   * substituted in for XML's reserved characters.
   */
  private static final Map ENTITY_REFERENCES = new HashMap();

  static {
    ENTITY_REFERENCES.put(new Character('<'), "&lt;");
    ENTITY_REFERENCES.put(new Character('&'), "&amp;");
    ENTITY_REFERENCES.put(new Character('>'), "&gt;");
    ENTITY_REFERENCES.put(new Character('"'), "&quot;");
    ENTITY_REFERENCES.put(new Character('\''), "&apos;");
  }
  /**
   * The indentString records the current amount of indentation of the <code>XMLPrinter</code>.
   * It is used to maintain state for recursive indentation
   */
  String indentString = baseIndentString;

  /**
   * Replaces all occurences of reserved characters <, &, >, ", ' with
   * the predefined XML entities &lt;, &amp;, &gt;, &quot;, &apos;
   * @param inputString a string to be checked for the occurrence of
   * reserved characters.
   * @return a new string with all the reserved characters replaced by
   * the appropriate XML Entities.
   */
  public static String introduceXMLEntities(String inputString) {
    StringBuffer stringBuffer = new StringBuffer();
    int length = inputString.length();
    char c;
    for (int i = 0; i < length; i++) {
      c = inputString.charAt(i);
      if (ENTITY_REFERENCES.containsKey(new Character(c))) {
        stringBuffer.append(ENTITY_REFERENCES.get(new Character(c)));
      } else {
        stringBuffer.append(c);
      }
    }
    return stringBuffer.toString();
  }

  /**
   * Sets the length of the indentation of the XMLFileWriter
   * object. If relative = false the indentation will be a string consisting
   * of the number of spaces specified by the `indent' parameter. If relative = true
   * then the current indentation is augmented or decreased by the number of spaces
   * specified by the `indent' parameter.
   *
   * @param indent the number of spaces that constitute the indentation.
   * @param relative determines whether indentation should be relative to the
   * current level of indentation -- <code>relative = true</code> -- or else
   * absolute -- <code>relative = false</code>.
   */
  public void setIndent(int indent, boolean relative) throws RuntimeException {
    if (indent == 0 && !relative) {
      this.indentString = baseIndentString;
    } else if (indent > 0) {
      char[] ind = new char[indent];
      Arrays.fill(ind, ' ');
      String offset = this.indentString.copyValueOf(ind);
      if (relative) {
        this.indentString = this.indentString + offset;
      } else {
        this.indentString = offset;
      }
    } else {
      if (0 > indent) {
        if (relative) {
          int abs = -1 * indent;
          if (this.indentString.length() >= abs) {
            this.indentString = this.indentString.substring(abs);
          } else {
            //System.out.println("XMLWriter cannot have negative indentation");
            throw new RuntimeException("XMLWriter cannot have negative indentation");
          }
        } else {
          throw new RuntimeException("XMLWriter cannot have negative indentation");
        }
      }
    }
  }

  /**
   * Resets the indentationof the <code>XMLWriter</code> to its default value.
   */
  public void resetIndent() {
    this.indentString = this.baseIndentString;
  }

  /**
   * Returns the length of the current value of @see #indentString.
   */
  public int getIndentLength() {
    return this.indentString.length();
  }

  /**
   * Flushes the <code>XMLWriter</code>.
   */
  public abstract void flush() throws java.io.IOException;

  /**
   * Closes the <code>XMLWriter</code>.
   */
  public abstract void close() throws java.io.IOException;

  /**
   * Prints @see #string to this <code>XMLWriter</code>.
   *
   * @param string the string to be printed.
   * @exception java.io.IOException if the <code>XMLWriter</code> cannot
   * print to the file specified.
   */
  public abstract void print(String string) throws java.io.IOException;

  /**
   * Prints @see #string to this <code>XMLWriter</code> after performing all the appropriate
   * substitutions of XML reserved characters.
   *
   * @param string the string to be printed.
   * @exception java.io.IOException if the <code>XMLWriter</code> cannot
   * print to the file specified.
   */
  public void printSafe(String string) throws java.io.IOException {
    print(introduceXMLEntities(string));
  }

  /**
   * Prints a string to this <code>XMLWriter</code> indenting it
   * by the number of spaces indicated by @see #indent either relative to the
   * current indentation level (if @see #relative is <code>true</code>) or with
   * respect to the beginning of the line (if @see #relative is <code>false</code>).
   *
   * @param string the string to be printed.
   * @param indent the number of spaces by which the string needs to be indented.
   * @param relative id <code>true</code> the string is further indented with respect
   * to the current indentation level, if <code>false</code> is indented with respect to
   * the beginning of the line.
   */
  public abstract void indentPrint(String string, int indent, boolean relative) throws java.io.IOException;

  /**
   * Prints a string to this <code>XMLWriter</code> after replacing all the reserved XML characters
   * with the appropriate XML entity references indenting it
   * by the number of spaces indicated by @see #indent either relative to the
   * current indentation level (if @see #relative is <code>true</code>) or with
   * respect to the beginning of the line (if @see #relative is <code>false</code>).
   *
   * @param string the string to be printed.
   * @param indent the number of spaces by which the string needs to be indented.
   * @param relative id <code>true</code> the string is further indented with respect
   * to the current indentation level, if <code>false</code> is indented with respect to
   * the beginning of the line.
   */
  public void indentPrintSafe(String string, int indent, boolean relative) throws java.io.IOException {
    String safeString = introduceXMLEntities(string);
    indentPrint(safeString, indent, relative);
  }

  /**
   * Prints a string and terminates the line. The string is printed to this <code>XMLWriter</code>
   * and indented by the number of spaces indicated by @see #indent either relative to the
   * current indentation level (if @see #relative is <code>true</code>) or with
   * respect to the beginning of the line (if @see #relative is <code>false</code>).
   *
   * @param string the string to be printed.
   * @param indent the number of spaces by which the string needs to be indented.
   * @param relative id <code>true</code> the string is further indented with respect
   * to the current indentation level, if <code>false</code> is indented with respect to
   * the beginning of the line.
   */
  public void indentPrintln(String string, int indent, boolean relative) throws java.io.IOException {
    indentPrint(string + "\n", indent, relative);
  }

  /**
   * Prints a string after replacing all the reserved XML characters
   * with the appropriate XML entity references and terminates the line.
   * The string is printed to this <code>XMLWriter</code> and indented by the number
   * of spaces indicated by @see #indent either relative to the current indentation
   * level (if @see #relative is <code>true</code>) or with respect to the beginning
   * of the line (if @see #relative is <code>false</code>).
   * @param string the string to be printed.
   * @param indent the number of spaces by which the string needs to be indented.
   * @param relative id <code>true</code> the string is further indented with respect
   * to the current indentation level, if <code>false</code> is indented with respect to
   * the beginning of the line.
   */
  public void indentPrintSafeln(String string, int indent, boolean relative) throws java.io.IOException {
    String safeString = introduceXMLEntities(string) + "\n";
    indentPrint(safeString, indent, relative);
  }

  /**
   * Prints to this <code>XMLWriter</code> an atomic XML tag with no attributes.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   */
  public void printXMLAtomicTag(String tag, int indent, boolean relative) throws java.io.IOException {
    String xmltag = "<" + tag + "/>";
    this.indentPrintln(xmltag, indent, relative);
  }

  /**
   * Prints to this <code>XMLWriter</code> an atomic XML tag with attributes.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param attributesIterator an iterator over a list of attribute/value
   * <code>Pair</code>s.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   */
  public void printXMLAtomicTag(String tag, ListIterator attributesIterator, int indent, boolean relative) throws java.io.IOException {
    String attributeString = "";
    while (attributesIterator.hasNext()) {
      Pair attributePair = (Pair) attributesIterator.next();
      attributeString = " " + (String) attributePair.component1 + "=\"" + (String) attributePair.component2
              + "\"" + attributeString;
    }
    String xmltag = "<" + tag + attributeString + "/>";
    this.indentPrintln(xmltag, indent, relative);
  }

  public void printXMLStartTag(String tag, int indent, boolean relative) throws IOException {
    if (tag.length() > 0 && !(tag.charAt(0) == '<')) {
      tag = "<" + tag + ">";
    }
    indentPrint(tag, indent, relative);
  }

  /**
   * Prints to this <code>XMLWriter</code> an atomic XML tag with a single attribute.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param attributeName the name of the attribute.
   * @param attributeValue the value of the attribute.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   * @param newline specifies whether the line should be terminated after the tag
   * has been printed
   */
  public void printXMLAtomicTag(String tag, String attributeName, String attributeValue, int indent,
          boolean relative, boolean newline) throws java.io.IOException {
    String attributeString = attributeName + "=\"" + attributeValue + "\"";
    String xmltag = "<" + tag + " " + attributeString + "/>";
    if (newline) {
      this.indentPrintln(xmltag, indent, relative);
    } else {
      this.indentPrint(xmltag, indent, relative);
    }
  }

  /**
   * Prints to this <code>XMLWriter</code> an XML start tag with no attributes.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   * @param newline specifies whether the line should be terminated after the tag
   * has been printed
   */
  public void printXMLStartTag(String tag, int indent, boolean relative, boolean newline) throws java.io.IOException {
    String xmltag = "<" + tag + ">";
    if (newline) {
      this.indentPrintln(xmltag, indent, relative);
    } else {
      this.indentPrint(xmltag, indent, relative);
    }
  }

  /**
   * Prints to this <code>XMLWriter</code> an XML start tag with attributes.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param attributesIterator an iterator over a list of attribute/value
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   * @param newline specifies whether the line should be terminated after the tag
   * has been printed
   */
  public void printXMLStartTag(String tag, ListIterator attributesIterator, int indent, boolean relative,
          boolean newline) throws java.io.IOException {
    String attributeString = "";
    while (attributesIterator.hasNext()) {
      Pair attributePair = (Pair) attributesIterator.next();
      attributeString = " " + (String) attributePair.component1 + "=\"" + (String) attributePair.component2
              + "\"" + attributeString;
    }
    String xmltag = "<" + tag + attributeString + ">";
    if (newline) {
      this.indentPrintln(xmltag, indent, relative);
    } else {
      this.indentPrint(xmltag, indent, relative);
    }
  }

  /**
   * Prints to this <code>XMLWriter</code> an XML start tag with a single attribute.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param attributeName the name of the attribute.
   * @param attributeValue the value of the attribute.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   * @param newline specifies whether the line should be terminated after the tag
   * has been printed
   */
  public void printXMLStartTag(String tag, String attributeName, String attributeValue, int indent,
          boolean relative, boolean newline) throws java.io.IOException {
    String attributeString = attributeName + "=\"" + attributeValue + "\"";
    String xmltag = "<" + tag + " " + attributeString + ">";
    if (newline) {
      this.indentPrintln(xmltag, indent, relative);
    } else {
      this.indentPrint(xmltag, indent, relative);
    }
  }

  /**
   * Prints to this <code>XMLWriter</code> an XML end tag.
   * The tag is indented by @see #indent spaces either from the beginning of the
   * line (if @see #relative is <code>false</code>) or relative to the current
   * indentation level(if @see #relative is <code>true</code>).
   *
   * @param tag the name of the XML tag.
   * @param indent the number of spaces by which the tag needs to be indented.
   * @param relative specifies whether the indentation is from the beginning
   * of the line (if @see #relative is <code>false</code>) or from the current
   * level of indentation (if @see #relative is <code>true</code>).
   */
  public void printXMLEndTag(String tag, int indent, boolean relative) throws java.io.IOException {
    String xmltag = "</" + tag + ">";
    this.indentPrintln(xmltag, indent, relative);
  }

  /**
   * Prints to this <code>XMLWriter</code> an XML end tag.
   *
   * @param tag the name of the XML tag.
   */
  public void printXMLEndTag(String tag) throws java.io.IOException {
    String xmltag = "</" + tag + ">";
    this.print(xmltag + "\n");
  }
}







