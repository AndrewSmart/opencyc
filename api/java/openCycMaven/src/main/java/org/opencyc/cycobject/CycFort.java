package org.opencyc.cycobject;

import java.io.*;
import org.opencyc.xml.XMLWriter;

/**
 * This class implements a Cyc Fort (First Order Reified Term).
 *
 * @version $Id: CycFort.java 138070 2012-01-10 19:46:08Z sbrown $
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
public abstract class CycFort extends DefaultCycObject implements CycDenotationalTerm, Serializable {
  
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
    if (this instanceof CycConstant) {
      if (object instanceof CycConstant)
        return this.toString().compareTo(object.toString());
      else if (object instanceof CycNart)
        return this.toString().compareTo(object.toString().substring(1));
      else
        throw new ClassCastException("Must be a CycFort object");
    }
    else {
      if (object instanceof CycNart)
        return this.toString().compareTo(object.toString());
      else if (object instanceof CycConstant)
        return this.toString().substring(1).compareTo(object.toString());
      else
        throw new ClassCastException("Must be a CycFort object");
    }
  }
  
  /**
   * Returns <tt>true</tt> some object equals this <tt>CycConstant</tt>. The equality check uses only the guid.
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  abstract public boolean equals(Object object);
  
  /**
   * Returns <tt>true</tt> some object equals this <tt>CycFort</tt> at the EL level.
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  abstract public boolean equalsAtEL(Object object);
  
  /**
   * When true, indicates that the fort is invalid.
   */
  protected boolean isInvalid = false;
  
}

