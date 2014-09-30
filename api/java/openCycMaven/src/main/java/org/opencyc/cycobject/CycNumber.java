/* $Id: CycNumber.java 132283 2010-09-21 18:20:46Z baxter $
 *
 * Copyright (c) 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.cycobject;

//// External Imports
import java.io.IOException;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

//// Internal Imports
import org.opencyc.xml.XMLWriter;

/**
 * Provides the behavior and attributes of an OpenCyc Number.
 *
 * @version $Id: CycNumber.java 132283 2010-09-21 18:20:46Z baxter $
 * @author Dave Schneider
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
public class CycNumber implements CycDenotationalTerm {

  /**
   * The Number that this term is wrapping.
   */
  private final Number number;
  /**
   * The Number as a Double. (initialized lazily)
   */
  private Double doubleValue = null;

  /**
   * Zero-arg constructor. Should generally not be used.
   */
  private CycNumber() {
    number = null;
  }

  /**
   * Constructs a new <tt>CycNumber</tt> object from a numeric string.
   *
   * @param numberString the constant name
   */
  public CycNumber(final String numberString) {
    if (numberString == null) {
      throw new IllegalArgumentException("number string must not be null.");
    } else {
      try {
        final NumberFormat nf = NumberFormat.getInstance(Locale.ENGLISH);
        number = (Number) nf.parseObject(numberString);
      } catch (ParseException ex) {
        Logger.getLogger(CycNumber.class.getName()).log(Level.SEVERE, null, ex);
        throw new IllegalArgumentException(numberString + " must be a number");
      }
    }
  }

  /**
   * Constructs a new <tt>CycNumber</tt> object from a java Number.
   *
   * @param name the constant name
   */
  public CycNumber(final Number num) {
    if (num == null) {
      throw new IllegalArgumentException("Number must not be null.");
    } else {
      number = num;
    }
  }

  /**
   * Gets the value of this CycNumber as a java Number.
   *
   * @return the number as a java Number
   */
  public Number getNumber() {
    return number;
  }

  /**
   * Convenience method to compare two CycNumbers numerically.
   * @param other
   * @return true iff this CycNumber is numerically larger than other.
   */
  public boolean isGreaterThan(CycNumber other) {
    return this.compareTo(other) > 0;
  }

  /**
   * Returns the XML representation of this object.
   *
   * @return the XML representation of this object
   */
  public String toXMLString() {
    return number.toString();
  }

  /**
   * Prints the XML representation of the CycConstant to an <code>XMLWriter</code>
   *
   * @param xmlWriter an <tt>XMLWriter</tt>
   * @param indent an int that specifies by how many spaces to indent
   * @param relative a boolean; if true indentation is relative, otherwise absolute
   * @throws IOException
   */
  @Override
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative)
          throws IOException {
    xmlWriter.print(this.toXMLString());
  }

  /**
   * Provides the hash code appropriate for the <tt>CycNumber</tt>.
   *
   * @return the hash code for the <tt>CycNumber</tt>
   */
  @Override
  public int hashCode() {
    return getNumber().hashCode();
  }

  /**
   * Returns <tt>true</tt> some object equals this <tt>CycNumber</tt>.
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  @Override
  public boolean equals(Object object) {
    if (!(object instanceof CycNumber)) {
      return false;
    } else {
      return this.getNumber().equals(((CycNumber) object).getNumber());
    }
  }

  /**
   * Returns <tt>true</tt> some object equals this <tt>CycNumber</tt>.
   * In constrast to equals(), this will also return true if a java Number
   * is equals() to the number encapsulated by this CycNumber.
   *
   * @param object the <tt>Object</tt> for equality comparison
   * @return equals <tt>boolean</tt> value indicating equality or non-equality.
   */
  @Override
  public boolean equalsAtEL(Object object) {
    return (this.getNumber().equals(object) || this.equals(object));
  }

  /**
   * Returns a String representation of the <tt>CycNumber</tt>.
   */
  @Override
  public String toString() {
    if (number != null) {
      return number.toString();
    }
    return "[CycNumber: " + number + "]";
  }

  /**
   * Returns this object in a form suitable for use as an <tt>CycList</tt> api expression value.
   *
   * @return this object in a form suitable for use as an <tt>CycList</tt> api expression value
   */
  @Override
  public Object cycListApiValue() {
    return this.getNumber();
  }

  public Object cycExpressionApiValue() {
    return this.getNumber();
  }

  @Override
  public String cyclifyWithEscapeChars() {
    return cyclify();
  }

  @Override
  public String stringApiValue() {
    return getNumber().toString();
  }

  @Override
  public int compareTo(Object o) {
    if (o instanceof CycNumber) {
      final CycNumber other = (CycNumber) o;
      final Class thisNumberClass = this.number.getClass();
      if (thisNumberClass.equals(other.number.getClass())
              && Comparable.class.isAssignableFrom(thisNumberClass)) {
        return ((Comparable)this.number).compareTo((Comparable)other.number);
      } else {
        return this.doubleValue().compareTo(other.doubleValue());
      }
    } else {
      throw new UnsupportedOperationException("Not supported yet.");
    }
  }

  /**
   * Gets the number as a Double
   *
   * @return the number as a Double
   */
  private synchronized Double doubleValue() {
    if (doubleValue == null) {
      doubleValue = number.doubleValue();
    }
    return doubleValue;
  }

  @Override
  public String cyclify() {
    return stringApiValue();
  }

  @Override
  public List getReferencedConstants() {
    return Collections.emptyList();
  }
}
