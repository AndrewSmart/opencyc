package org.opencyc.cycobject;

import java.io.*;
import java.util.UUID;
import org.opencyc.xml.*;

/**
 * Provides the behavior and attributes of an OpenCyc GUID (Globally Unique
 * IDentifier). Each OpenCyc constant has an associated guid.
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
public class Guid implements Serializable {

    /**
     * The name of the XML tag for this object.
     */
    public static final String guidXMLTag = "guid";

     /**
     * Constructs a new <tt>Guid</tt> object.
     */
    
    public Guid(UUID guid) {
      this(guid, guid.toString());
    }
    
    public Guid(String guidString) {
      this(UUID.fromString(guidString), guidString);
    }
    
    public Guid(UUID guid, String guidString) {
      this.guid = guid;
      this.guidString = guidString;
    }
    
    public Guid(byte[] data) {
      this(new UUID(guidMSB(data), guidLSB(data)));      
    }

    /**
     * Returns <tt>true</tt> if the object equals this object.
     *
     * @return <tt>boolean</tt> indicating equality of an object with this object.
     */
    public boolean equals(Object object) {
        if (object instanceof Guid &&
            this.guid.equals(((Guid) object).guid)) {
            return true;
        }
        else
            return false;
    }

    /**
     * Returns the hash code for this object.
     *
     * @return the hash code for this object
     */
    public int hashCode () {
        return guid.hashCode();
    }
    
    /**
     * Returns a string representation of the <tt>Guid</tt>.
     *
     * @return the <tt>Guid</tt> formated as a <tt>String</tt>.
     */
    public String toString() {
        return getGuidString();
    }

    public String getGuidString() {
      return guidString;
    }

    /**
     * Returns the XML representation of this object.
     *
     * @return the XML representation of this object
     */
    public String toXMLString () throws IOException {
        XMLStringWriter xmlStringWriter = new XMLStringWriter();
        toXML(xmlStringWriter, 0, false);
        return xmlStringWriter.toString();
    }

    /**
     * Prints the XML representation of the Guid to an <code>XMLWriter</code>
     *
     * @param xmlWriter an <tt>XMLWriter</tt>
     * @param indent an int that specifies by how many spaces to indent
     * @param relative a boolean; if true indentation is relative, otherwise absolute
     */
    public void toXML (XMLWriter xmlWriter, int indent, boolean relative)
        throws IOException {
        xmlWriter.printXMLStartTag(guidXMLTag, indent, relative, false);
        xmlWriter.print(guidString);
        xmlWriter.printXMLEndTag(guidXMLTag);
    }

    
    // private
    
    private static final long guidMSB(byte[] data) {
      long msb = 0;
      assert data.length == 16;
      for (int i=0; i<8; i++)
        msb = (msb << 8) | (data[i] & 0x0ff);
      return msb;
    }
 
    private static final long guidLSB(byte[] data) {
      long lsb = 0;
      assert data.length == 16;
      for (int i=8; i<16; i++)
        lsb = (lsb << 8) | (data[i] & 0x0ff);
      return lsb;
    }
    
    // Internal Rep
    
    /**
     * The GUID in string form.
     * @deprecated @see getGuidString()
     */
    public  final String guidString;
    private final UUID   guid;
    
    
    
    public static void main (String[] args) {
      final byte[] data = {(byte) 189,  (byte)  88, (byte) 128, (byte) 244, (byte) 156, (byte)  41, (byte)  17, (byte) 177,
                           (byte) 157,  (byte) 173, (byte) 195, (byte) 121, (byte)  99, (byte) 111, (byte) 114, (byte) 112 };
      final Guid guid1 = new Guid(data);
      final Guid guid2 = new Guid("bd5880f4-9c29-11b1-9dad-c379636f7270");
      System.out.println("guid1 = " + guid1);
      System.out.println("guid2 = " + guid2);
     if (!guid1.equals(guid2)) {
        throw new RuntimeException(guid1 + " does not equal " + guid2);     
      }
    }
}
