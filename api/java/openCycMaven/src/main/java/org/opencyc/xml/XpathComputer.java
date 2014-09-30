package  org.opencyc.xml;

import  java.util.StringTokenizer;
import  org.w3c.dom.Node;
import  org.w3c.dom.NodeList;
import  org.w3c.dom.DOMException;


/**
 * Implements static methods for computing Xpath strings
 * (www.w3.org/TR/xpath) for DOM Nodes (www.w3.org/DOM/)<p>
 *
 * @version $Id: XpathComputer.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stefano Bertolo
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

// Reference tree for the examples below
//
//  <tree>
//   <branch>
//    <twig>
//     <leaf>the</leaf>
//     <leaf>programmer</leaf>
//    </twig>
//   </branch>
//   <branch>
//    <twig>
//     <leaf>visited</leaf>
//    </twig>
//    <twig>
//     <leaf>a</leaf>
//     <leaf>friend</leaf>
//    </twig>
//   </branch>
//   <branch>
//    <twig>
//     <leaf>last</leaf>
//     <leaf>Sunday</leaf>
//    </twig>
//   </branch>
//  </tree>
public class XpathComputer {

    /**
     * Returns the local Xpath string for the input node, i.e. the Xpath string
     * one would need to reach the node from its parent.
     *
     * For example, if node is the DOM node corresponding to the node
     *
     * <twig>
     *  <leaf>a</leaf>
     *  <leaf>friend</leaf>
     * </twig>
     *
     * in the tree above, XpathComputer.localXpath(node) would return
     *
     * "/twig[2]"
     *
     * @throws throws DOMException.
     * @return the local Xpath string for the input node.
     */
    public static String localXpath (Node node) throws DOMException {
        String localname = node.getLocalName();
        if (null == localname) {
            throw  new DOMException(DOMException.NOT_FOUND_ERR, "DOM Node doesn't have a local name!");
        }
        else {
            int position = 1;
            Node current = node;
            Node previous = current.getPreviousSibling();
            while (null != previous) {
                if (localname.equals(previous.getLocalName()))
                // need to check on this because sibling nodes are not guaranteed
                // to be all of the same kind.
                {
                    position++;
                }
                current = previous;
                previous = current.getPreviousSibling();
            }
            return  localname + "[" + position + "]";
        }
    }

    /**
     * Takes a starting DOM node and a local Xpath directive and returns the DOM
     * node, if any, which can be reached following the directive from the input node
     * It throws DOMExceptions if no node exists at the specified address or if the address
     * is not a valid local Xpath expression. We repeat here that this is a very limited
     * implementation of Xpath addressing which is guaranteed to handle all and *only* those
     * Xpath expressions that could have been the output of the XpathComputer.localXpath method
     *
     * For example, if start is the DOM node corresponding to the node
     *
     * <twig>
     *  <leaf>a</leaf>
     *  <leaf>friend</leaf>
     * </twig>
     *
     * in the tree above, XpathComputer.followLocalXpath(start, "leaf[2]") would return
     * the node
     *
     * <leaf>friend</leaf>
     *
     * @throws throws DOMException.
     * @return the DOM node that is reached following the input Xpath directiv
     * starting from the input node.
     */
    public static Node followLocalXpath (Node start, String localXpath) throws DOMException {
        String startXpath = localXpath(start);
        if (startXpath.equals(localXpath)) {
            return  start;
        }
        else {
            StringTokenizer tokenizer = new StringTokenizer(localXpath, "[]");
            if (tokenizer.countTokens() != 2) {
                String invalidXpath = localXpath + " is an invalid local Xpath!";
                throw  new DOMException(DOMException.SYNTAX_ERR, invalidXpath);
            }
            String tagname = tokenizer.nextToken();
            try {
                Integer position = new Integer(tokenizer.nextToken());
                int counter = 1;
                NodeList children = start.getChildNodes();
                for (int i = 0; i < children.getLength(); i++) {
                    Node child = children.item(i);
                    if (tagname.equals(child.getLocalName())) {
                        if (counter == position.intValue()) {
                            return  child;
                        }
                        counter++;
                    }
                }
            } catch (NumberFormatException excpt) {
                String invalidXpath = localXpath + " is an invalid local Xpath!";
                throw  new DOMException(DOMException.SYNTAX_ERR, invalidXpath);
            }
            String failed = "No node at location " + localXpath;
            throw  new DOMException(DOMException.NOT_FOUND_ERR, failed);
        }
    }

    /**
     * Returns the entire Xpath string for the input node, i.e. the Xpath string
     * one would need to reach the node from the root of the document.
     *
     * For example, if node is the DOM node corresponding to the node
     *
     *  <leaf>friend</leaf>
     *
     * in the tree above, XpathComputer.computeXpath(node) would return
     *
     * "/tree[1]/branch[2]/twig[2]/leaf[2]"
     *
     * @return the entire Xpath string for the input node.
     */
    public static String computeXpath (Node node) {
        String Xpath = localXpath(node);
        if ("" == Xpath) {
            return  Xpath;
        }
        else {
            Node parent = node.getParentNode();
            String moreXpath;
            while (null != parent.getLocalName()) {
                moreXpath = localXpath(parent);
                Xpath = moreXpath + "/" + Xpath;
                parent = parent.getParentNode();
            }
            return  "/" + Xpath;
        }
    }

    /**
     * Takes a starting DOM node and an Xpath directive and returns the DOM
     * node, if any, which can be reached following the directive from the input node.
     * It throws DOMExceptions if no node exists at the specified address or if the address
     * is not a valid Xpath expression. We repeat here that this is a very limited
     * implementation of Xpath addressing which is guaranteed to handle all and *only* those
     * Xpath expressions that could have been the output of the XpathComputer.computeXpath method
     *
     * For example, if start is the DOM node corresponding to the root node in the tree above
     * XpathComputer.follow.LocalXpath(start, "/tree[1]/branch[3]/twig[1]leaf[2]") would return
     * the node
     *
     * <leaf>Sunday</leaf>
     *
     * @throws throws DOMException.
     * @return the DOM node that is reached following the input Xpath directive
     * starting from the input node.
     */
    public static Node followXpath (Node start, String Xpath) throws DOMException {
        StringTokenizer tokenizer = new StringTokenizer(Xpath, "/");
        if (tokenizer.countTokens() == 0) {
            String invalidXpath = Xpath + " is an invalid Xpath!";
            throw  new DOMException(DOMException.SYNTAX_ERR, invalidXpath);
        }
        Node current = start;
        String localXpath;
        while (tokenizer.hasMoreTokens()) {
            localXpath = tokenizer.nextToken();
            current = followLocalXpath(current, localXpath);
        }
        return  current;
    }
}






