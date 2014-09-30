package  org.opencyc.util;

import  java.util.StringTokenizer;
import  java.io.Serializable;


/**
 * Implements an ordered pair, two associated <code>Object</code>s.<p>
 *
 * @version $Id: AbstractPair.java 138070 2012-01-10 19:46:08Z sbrown $
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
public abstract class AbstractPair
        implements Serializable {
    protected static final String beginChar = "(";
    protected static final String endChar = ")";
    protected static final String separatorChar = "#";
    protected static final String separators = beginChar + endChar + separatorChar;
    /**
     * The first component of the pair.
     */
    public Object component1;
    /**
     * The second component of the pair.
     */
    public Object component2;

    /**
     * Constructs an AbstractPair object.
     */
    public AbstractPair () {
    }

    /**
     * Constructs a new pair, with <code>component1</code> as its first and
     * <code>component2</code> as its second component.
     */
    public AbstractPair (Object component1, Object component2) {
        this.component1 = component1;
        this.component2 = component2;
    }

    /**
     * Compares the specified object with this <code>AbstractPair</code> for
     * equality.
     * <p>
     * Returns <code>true</code> if the given object is also a pair with the same
     * components, <code>false</code> otherwise.
     *
     * @return <code>true</code> if the given object is also a pair with the same
     * components, <code>false</code> otherwise.
     */
    public boolean equals (Object o) {
        return  (o.getClass().equals(this.getClass()) && (((((AbstractPair)o).component1 == null) &&
                (this.component1 == null)) || ((this.component1 != null) && ((AbstractPair)o).component1.equals(this.component1)))
                && (((((AbstractPair)o).component2 == null) && (this.component2 == null)) || ((this.component2
                != null) && ((AbstractPair)o).component2.equals(this.component2))));
    }

    public boolean elementsEqual () {
        return  component1.equals(component2);
    }

    /**
     * Returns a hash code value of this pair.
     *
     * @return a hash code value of this pair.
     */
    public int hashCode () {
        return  (component1 == null ? 0 : component1.hashCode()) + (component2 == null ? 1 : component2.hashCode());
    }

    /**
     * Returns a string representation of this pair.
     *
     * @return a string representation of this pair.
     */
    public String toString () {
        return  beginChar + component1 + separatorChar + component2 + endChar;
    }
}



