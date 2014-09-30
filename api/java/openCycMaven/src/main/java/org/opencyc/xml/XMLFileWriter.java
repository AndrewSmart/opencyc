package  org.opencyc.xml;

import  java.io.FileWriter;


/**
 * Implements an XMLFileWriter with facilities for surrounding data
 * elements with tags appropriately named and indented.<p>
 *
 * @version $Id: XMLFileWriter.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class XMLFileWriter extends XMLWriter {
    /**
     * The <code>FileWriter</code> to which the actual printing is delegated.
     */
    FileWriter writer;

    /**
     * Constructs a XMLFileWriter object given an output file name
     *
     * @param outputFile the output file name
     */
    public XMLFileWriter (String outputFile) throws java.io.IOException
    {
        writer = new FileWriter(outputFile);
    }

    /**
     * Prints a string to the <code>FileWriter</code> stored in the field @see #writer indenting it
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
    public void indentPrint (String string, int indent, boolean relative) throws java.io.IOException {
        this.setIndent(indent, relative);
        String newString = this.indentString + string;
        int newStringLength = newString.length();
        writer.write(newString, 0, newStringLength);
    }

    /**
     * Prints a string to the <code>FileWriter</code> stored in the field @see #writer.
     *
     * @param string the string to be printed.
     */
    public void print (String string) throws java.io.IOException {
        writer.write(string, 0, string.length());
    }

    /**
     * Flushes the <code>FileWriter</code> in the field @see #writer.
     */
    public void flush () throws java.io.IOException {
        writer.flush();
    }

  /**
   * Closes the <code>FileWriter</code> in the field @see #writer.
   */
  public void close() throws java.io.IOException {
    writer.close();
  }

}



