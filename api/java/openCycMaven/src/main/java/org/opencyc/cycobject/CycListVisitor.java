package org.opencyc.cycobject;

import java.util.*;
import org.opencyc.api.*;

/**
 * Implements an <tt>Enumeration</tt> for <tt>CycList</tt> objects which traverses
 * recursively into embedded CycLists, in a depth-first fashion, returning the
 * objects which are both non-CycList and non-nil.
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
public class CycListVisitor implements Enumeration {

    /**
     * Contains the next <tt>Object</tt> in the sequence of non-CycList
     * elements of the <tt>CycList</tt> tree structure.
     */
    protected Object nextElement = null;

    /**
     * Stack of <tt>CycList</tt> <tt>Iterators</tt>
     */
    protected Stack iterators = new Stack();


    /**
     * Constructs a new <tt>CycListEnumeration</tt> object.
     *
     * @param the <tt>CycList</tt> for recursive enumeration.
     */
    public CycListVisitor(CycList cycList) {
        iterators.push(cycList.iterator());
        getNextElement();
    }

    /**
     * Tests if this enumeration contains more elements.
     *
     * @return  <tt>true</tt> if and only if this enumeration object
     *           contains at least one more element to provide;
     *          <tt>false</tt> otherwise.
     */
    public boolean hasMoreElements() {
        return nextElement != null;
    }

    /**
     * Returns the next element of this enumeration if this enumeration
     * object has at least one more element to provide.
     *
     * @return     the next element of this <tt>Enumeration</tt>.
     * @exception  NoSuchElementException  if no more elements exist.
     */
    public Object nextElement() {
        if (nextElement == null)
            throw new NoSuchElementException();
        Object answer = nextElement;
        // Stay one ahead to facilitate the determination of hasMoreElements.
        getNextElement();
        return answer;
    }

    /**
     * Gets the next element in the sequence.  This method uses recursive descent.
     */
    protected void getNextElement() {
        nextElement = null;
        while (true) {
            if (iterators.empty())
                // Reached the end of the whole CycList.
                return;
            Iterator iterator = (Iterator) iterators.peek();
            if (! iterator.hasNext()) {
                iterators.pop();
                // Reached the end of an embedded CycList.
                continue;
            }
            Object element = iterator.next();
            if (element.equals(CycObjectFactory.nil))
                // bypass nils.
                continue;
            if (! (element instanceof CycList)) {
                nextElement = element;
                // Found the next non-nil element.
                return;
            }
            // Iterate over the embedded CycList.
            iterators.push(((CycList) element).iterator());
        }
    }

}
