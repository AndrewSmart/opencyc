package  org.opencyc.cycobject;

import java.io.*;
import org.opencyc.xml.*;

/*****************************************************************************
 * Contains an array of bytes, as an object that directly represents a SubL
 * byte vector.  The primitive element in java is a byte having a signed numerical
 * value between -128 and +127.  The corresponding primitive element in SubL is a
 * byte having an unsigned numerical value between 0 and 255.<p>
 *
 * The ByteArray object can only be used in the OpenCyc binary api because it does not
 * have a string representation in either SubL or java.
 *
 * @version $Id: ByteArray.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Bjorn Aldag
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
public class ByteArray extends DefaultCycObject implements Serializable {

    static final long serialVersionUID = -6247169945186440269L;
    
    /**
     * the name of the XML tag for Cyc byte-vector objects
     */
    public static final String byteVectorXMLTag = "byte-vector";

    /**
     * the name of the XML tag for the length of Cyc byte-vector objects
     */
    public static final String lengthXMLTag = "length";

    /**
     * the name of the XML tag for the byte elements of Cyc byte-vector objects
     */
    public static final String byteXMLTag = "byte";

    /**
     * the default indentation for printing CycConstant objects to XML
     */
    public static int indentLength = 2;

    /**
     * The actual array of bytes of this byte array.
     */
    private byte[] bytes;

    /**
     * Creates a new byte array from the specified array of bytes.
     */
    public ByteArray (byte[] bytes) {
        this.bytes = bytes;
    }

    /**
     * Returns the array of bytes of this ByteArray.
     * @return the array of bytes of this ByteArray.
     */
    public byte[] byteArrayValue () {
        return  bytes;
    }

    /**
     * Returns <tt>true</tt> iff some object equals this <tt>ByteArray</tt>
     *
     * @param object the <tt>Object</tt> for equality comparison
     * @return equals <tt>boolean</tt> value indicating equality or non-equality.
     */
    public boolean equals(Object object) {
        if (! (object instanceof ByteArray))
            return false;
        if (bytes.length != ((ByteArray) object).bytes.length)
            return false;
        for (int i = 0; i < bytes.length; i++)
            if (bytes[i] != ((ByteArray) object).bytes[i])
                return false;
        return true;
    }

    /**
     * Returns the string representation of the <tt>ByteArray</tt>
     *
     * @return the representation of the <tt>ByteArray</tt> as a <tt>String</tt>
     */
    public String toString() {
        StringBuffer result = new StringBuffer("[ByteArray len:");
        result.append(bytes.length);
        result.append(" ");
        if (bytes.length > 0)
            result.append((new Byte(bytes[0])).toString());
        for (int i = 1; i < bytes.length; i++) {
            result.append(",");
            result.append((new Byte(bytes[i])).toString());
        }
        result.append("]");
        return result.toString();
    }

    /**
     * Marshalls this ByteArray object into its CYC-ML XML expression.
     *
     * @return the CYC-ML XML representation string
     */
    public String toXMLString() throws IOException {
        XMLStringWriter xmlStringWriter = new XMLStringWriter();
        toXML(xmlStringWriter, 0, false);
        return xmlStringWriter.toString();
    }

    /**
     * Prints the XML representation of this ByteArray to an <code>XMLWriter</code>
     *
     * @param xmlWriter an <tt>XMLWriter</tt>
     * @param indent an int that specifies by how many spaces to indent
     * @param relative a boolean; if true indentation is relative, otherwise absolute
     */
    public void toXML (XMLWriter xmlWriter, int indent, boolean relative)
        throws IOException {
        xmlWriter.printXMLStartTag(byteVectorXMLTag, indent, relative, true);
        xmlWriter.printXMLStartTag(lengthXMLTag, indentLength, true, false);
        xmlWriter.print(Integer.toString(bytes.length));
        xmlWriter.printXMLEndTag(lengthXMLTag);
        for (int i = 0; i < bytes.length; i++) {
            xmlWriter.printXMLStartTag(byteXMLTag, 0, true, false);
            xmlWriter.print(Byte.toString(bytes[i]));
            xmlWriter.printXMLEndTag(byteXMLTag);
        }
        xmlWriter.printXMLEndTag(byteVectorXMLTag, -indentLength, true);
    }
    
    public String stringApiValue() {
      StringBuffer buf = new StringBuffer(bytes.length*4);
      buf.append( "(read-from-string \"#(");
      for (int i = 0; i < bytes.length; i++) {
        buf.append( ' ');
        int value = bytes[i];
        if (value < 0) {
          value += 256;
        }
        buf.append( value);
      }
      buf.append( ")\")");
      return buf.toString();
    }

    //// Private Area
  private void writeObject(ObjectOutputStream stream) throws java.io.IOException {
    stream.defaultWriteObject();
    stream.writeInt( this.byteArrayValue().length);
    stream.write( byteArrayValue());
  }
   
  private void readObject(ObjectInputStream stream) throws java.io.IOException, 
  java.lang.ClassNotFoundException {
    stream.defaultReadObject();
    int size = stream.readInt();
    this.bytes = new byte[size];
    int index = 0, remainder = size;
    while (index < size) {
      int numOfBytes = stream.read(bytes, index, remainder);
      if (numOfBytes == -1) 
        throw new java.io.EOFException( "Unexpected EOF at index " + index + " of " + size);
      else {
        index += numOfBytes;
        remainder -= numOfBytes;
      }
    }
  }

  public int compareTo(Object o){
    if(!(o instanceof ByteArray)) return this.toString().compareTo(o.toString());
    int thisbound=bytes.length;
    ByteArray cmp=(ByteArray)o;
    int obound=cmp.bytes.length;
    int bound=thisbound>obound?obound:thisbound;
    for(int i=0;i<bound;i++){
      if(bytes[i]==cmp.bytes[i]) continue;
      return (int)bytes[i]-(int)cmp.bytes[i];
    }
    return bound-obound;
  }


}








