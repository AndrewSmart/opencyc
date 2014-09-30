/*
 * CycObject.java
 *
 * Created on May 10, 2002, 11:35 AM
 */

package org.opencyc.cycobject;

import java.io.IOException;
import org.opencyc.xml.XMLWriter;
import java.util.List;

/**
 * This interface marks an object as being a
 * CycL object.
 *
 * @version $Id: CycObject.java 138070 2012-01-10 19:46:08Z sbrown $
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
public interface CycObject extends Comparable<Object> {
  
  static final int CYCOBJECT_UNKNOWN      = 0;
  static final int CYCOBJECT_BYTEARRAY    = 1;
  static final int CYCOBJECT_CYCASSERTION = 2;
  static final int CYCOBJECT_CYCFORT      = 3;
  static final int CYCOBJECT_CYCLIST      = 4;
  static final int CYCOBJECT_CYCSYMBOL    = 5;
  static final int CYCOBJECT_CYCVARIABLE  = 6;
  static final int CYCOBJECT_DOUBLE       = 7;
  static final int CYCOBJECT_FLOAT        = 8;
  static final int CYCOBJECT_GUID         = 9;
  static final int CYCOBJECT_INTEGER      =10;
  static final int CYCOBJECT_LONG         =11;
  static final int CYCOBJECT_STRING       =12;
  static final int CYCOBJECT_BIGINTEGER   =13;
 
  /**
   * Returns a cyclified string representation of the Cyc object.
   * A cyclified string representation is one where constants have been
   * prefixed with #$.
   *
   * @return a cyclified <tt>String</tt>.
   */
  public String cyclify();
      
  /**
   * Returns a cyclified string representation of the Cyc object.
   * A cyclified string representation with escape chars is one where
   * constants have been prefixed with #$ and Strings have had an escape
   * character inserted before each character that needs to be escaped in SubL.
   *
   * @return a cyclified <tt>String</tt> with escape characters.
   */ 
  public String cyclifyWithEscapeChars();
  
  /**
   * Returns a list of all constants refered to by this CycObject.
   * For example, a CycConstant will return a List with itself as the
   * value, a nart will return a list of its functor and all the constants refered
   * to by its arguments, a CycList will do a deep search for all constants,
   * a symbol or variable will return the empty list.
   * @return a list of all constants refered to by this CycObject
   **/
  public List getReferencedConstants();
  
  /**
   * Prints the XML representation of the CycConstant to an <code>XMLWriter</code>
   *
   * @param xmlWriter an <tt>XMLWriter</tt>
   * @param indent an int that specifies by how many spaces to indent
   * @param relative a boolean; if true indentation is relative, otherwise absolute
   */
  public void toXML (XMLWriter xmlWriter, int indent, boolean relative)
    throws IOException;

  /**
   * Returns this object in a form suitable for use as an <tt>String</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>String</tt> api expression value
   */
  public String stringApiValue();
  
  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  public Object cycListApiValue();
}
