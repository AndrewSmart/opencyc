package org.opencyc.util;
import java.util.*;

/**
 * Provides <tt>Collection</tt> utilities not otherwise provided by Jakarta Commons.  All methods
 * are static.  There is no need to instantiate this class.<p>
 *
 * @version $Id: OcCollectionUtils.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class OcCollectionUtils {

    /**
     * Returns <tt>true</tt> iff the given {@link Collection}s.
     * have any elements in common.  Performs HashSet search for
     * larger collections.
     *
     * @param a the first collection considered for intersection
     * @param b the second collection considered for intersection
     * @return <tt>true</tt> if the given {@link Collection}s.
     * have any elements in common.
     */
    public static boolean hasIntersection(final Collection a, final Collection b) {
        if (a.size() < 50 && b.size() < 50) {
            Iterator it = a.iterator();
            while(it.hasNext())
                if (b.contains(it.next()))
                    return true;
        }
        else if (a.size() < b.size()) {
            HashSet bSet = new HashSet(b);
            Iterator it = a.iterator();
            while(it.hasNext())
                if (bSet.contains(it.next()))
                    return true;
        }
        else {
            HashSet aSet = new HashSet(a);
            Iterator it = b.iterator();
            while(it.hasNext())
                if (aSet.contains(it.next()))
                    return true;
        }
        return false;
    }

    /**
     * Returns <tt>true</tt> iff the given {@link Collection} has any
     * duplicated elements.
     *
     * @param collection the collection under consideration for having duplicate elements
     * @return <tt>true</tt> the given {@link Collection} has any duplicated elements
     */
    public static boolean hasDuplicates(final Collection collection) {
        HashSet aSet = new HashSet(collection);
        return collection.size() != aSet.size();
    }

}
