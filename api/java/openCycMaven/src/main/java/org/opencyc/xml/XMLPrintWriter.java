package  org.opencyc.xml;

import  java.io.PrintStream;
import  java.io.PrintWriter;


/**
 * Implements an XMLPrintWriter with facilities for surrounding data
 * elements with tags appropriately named and indented.<p>
 *
 * @version $Id: XMLPrintWriter.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class XMLPrintWriter extends XMLWriter {
    /**
     * The <code>PrintWriter</code> to which the actual printing is delegated.
     */
    PrintWriter writer;

    /**
     * Constructs an XMLPrintWriter object given a PrintStream.
     *
     * @param printStream the PrintStream on which XML is output
     */
    public XMLPrintWriter (java.io.PrintStream printStream) {
        writer = new PrintWriter(printStream, true);
    }

    /**
     * Prints a string to the <code>PrintWriter</code> stored in the field @see #writer indenting it
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
    public void indentPrint (String string, int indent, boolean relative) {
        this.setIndent(indent, relative);
        writer.print(indentString + string);
    }


    /**
     * Prints a string to the <code>PrintWriter</code> stored in the field @see #writer.
     *
     * @param string the string to be printed.
     */
    public void print (String string) {
        writer.print(string);
    }

    /**
     * Flushes the <code>PrintWriter</code> in the field @see #writer.
     */
    public void flush () {
        writer.flush();
    }

  /**
   * Closes the <code>PrintWriter</code> in the field @see #writer.
   */
  public void close () {
    writer.close();
  }
}







