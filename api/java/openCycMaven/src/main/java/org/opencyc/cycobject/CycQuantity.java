package  org.opencyc.cycobject;

import java.math.BigDecimal;

/**
 * Represents a quantity or range with a CycFort unit of measure, a minimum numeric
 * value, and a maximum numeric value.
 *
 * @version $Id: CycQuantity.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author David Baxter
 *
 * <p>Copyright 2008 Cycorp, Inc., license is open source GNU LGPL.
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
public class CycQuantity implements Comparable<CycQuantity> {
  private CycQuantity(final CycFort unitOfMeasure, final Number minValue, final Number maxValue) {
    this.unitOfMeasure = unitOfMeasure;
    this.minValue = minValue;
    this.maxValue = maxValue;
  }
  
  /** Create a new CycQuantity from its CycList representation.
   * @deprecated Use CycNaut version.
   */
  public static CycQuantity valueOf(final CycList cycList) {
    if (cycList.size() < 2) {
      return null;
    } else if (cycList.size() > 3) {
      return null;
    } else {
      final Object unit = cycList.first();
      if (unit instanceof CycFort) { //@stub -- should check for UnitOfMeasure
        final Object min = cycList.second();
        if (min instanceof Number) {
          final Object max = (cycList.size() > 2) ? cycList.third() : min;
          if (max instanceof Number) {
            return new CycQuantity((CycFort)unit, (Number)min, (Number)max);
          }
        }
      }
    }
    return null;
  }

  /** Create a new CycQuantity from its CycNaut representation.
   */
  public static CycQuantity valueOf(final CycNaut naut) {
    if (naut.getArity() < 1) {
      return null;
    } else if (naut.getArity() > 2) {
      return null;
    } else {
      final Object unit = naut.getOperator();
      if (unit instanceof CycFort) { //@stub -- should check for UnitOfMeasure
        final Object min = naut.getArg1();
        if (min instanceof Number) {
          final Object max = (naut.getArity() > 1) ? naut.getArg2() : min;
          if (max instanceof Number) {
            return new CycQuantity((CycFort)unit, (Number)min, (Number)max);
          }
        }
      }
    }
    return null;
  }
  
  public String toString() {
    if (minValue.equals(maxValue)) {
      return "(" + unitOfMeasure + " " + minValue + ")";
    } else {
      return "(" + unitOfMeasure + " " + minValue + " " + maxValue + ")";
    }
  }
  
  
  public int compareTo(CycQuantity o) {
    if (o.unitOfMeasure.equals(unitOfMeasure)) {
      if (o.minValue.equals(minValue)) {
        return compare(maxValue, o.maxValue);
      } else {
        return compare(minValue, o.minValue);
      }
    } else {
      // @stub -- Use conversion factors to compare comparable quantities with different units.
      return unitOfMeasure.compareTo(o.unitOfMeasure);
    }
  }
  
  private static int compare(final Number num1, final Number num2) {
    if (num1 instanceof Integer && num2 instanceof Integer) {
      return ((Integer)num1).compareTo((Integer)num2);
    } else if (num1 instanceof Long && num2 instanceof Long) {
      return ((Long)num1).compareTo((Long)num2);
    } else {
      final int intResult = Integer.valueOf(num1.intValue()).compareTo(num2.intValue());
      if (intResult != 0) {
        return intResult;
      } else {
        return Double.compare(num1.doubleValue(), num2.doubleValue());
      }
    }
  }
  
  private final CycFort unitOfMeasure;
  private final Number minValue;
  private final Number maxValue;
}

