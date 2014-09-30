package org.opencyc.util;

import java.util.Stack;

/**
 * Extends the <tt>Stack</tt> class to provide a stack pointer.
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
public class StackWithPointer extends Stack {

    /**
     * Stack pointer.
     */
    public int sp = 0;

    /**
     * Constructs a new empty <tt>StackWithPointer<tt> object.
     */
    public StackWithPointer() {
    }

    /**
     * Pushes the argument onto the stack.
     *
     * @param item object to be pushed onto the <tt>Stack</tt>
     * @return Object that was pushed onto the <tt>Stack</tt>
     */
    public Object push ( Object item ) {
        sp++;
        return super.push(item);
    }

    /**
     * Returns the top of the stack, setting the new top of stack item.
     *
     * @return <tt>Object</tt> that was on the top of the <tt>Stack</tt>
     */
    public Object pop() {
        --sp;
        return super.pop();
    }
}
