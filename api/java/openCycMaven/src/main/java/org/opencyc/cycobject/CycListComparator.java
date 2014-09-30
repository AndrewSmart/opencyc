package org.opencyc.cycobject;

import java.util.*;

/**
 * Implements a <tt>Comparator</tt> for the <tt>sort</tt> method of the
 * <tt>CycList</tt> class.
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
public class CycListComparator implements Comparator {

    /**
     * Constructs a new CycListComparator object.
     */
    public CycListComparator() {
    }

    /**
     * Compares two <tt>CycList</tt> elements, according to their string
     * representations.
     *
     * @param o1 an Object for comparison
     * @param o2 another Object for comparison.
     * @return a negative integer, zero, or a positive integer as the first
     * argument is less than, equal to, or greater than the second.
     * @exception ClassCastException - if the arguments' types prevent them from
     * being compared by this Comparator
     */
    public int compare (Object o1, Object o2) {
        String string1 = o1.toString();
        String string2 = o2.toString();
        return string1.compareTo(string2);
    }

    /**
     * Returns <tt>true</tt> if some other object is equal to this <tt>Comparator</tt>
     *
     * @param object the reference object with which to compare.
     * @return <tt>true</tt> only if the specified object is also a
     * comparator and it imposes the same ordering as this comparator.
     */
     public boolean equals (Object object) {
        return object instanceof CycListComparator;
     }
}
