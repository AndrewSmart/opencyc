package org.opencyc.xml;

import org.opencyc.util.StringUtils;

/**
 * Provides utility methods for XML text processing.<p>
 *
 * @version $Id: TextUtil.java 138070 2012-01-10 19:46:08Z sbrown $
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

public class TextUtil {

    /**
     * Performs xml special character substitution.
     *
     * @param text the text be examined for xml special characters
     * @return the text with xml special character substitution
     */
    public static String doEntityReference (String text) {
        StringBuffer result = new StringBuffer();
        for (int i = 0; i < text.length(); i++) {
            char ch = text.charAt(i);
            if (ch == '<')
                result.append("&lt;");
            else if (ch == '&')
                result.append("&amp;");
            else if (ch == '>')
                result.append("&gt;");
            else
                result.append(ch);
        }
        return result.toString();
    }

    /**
     * Transforms xml special character substitution to plain text
     *
     * @param text the text be examined for xml special characters
     * @return the text with xml special character substitution
     */
    public static String undoEntityReference (String text) {
        String result = text.replaceAll("&lt;", "<");
        result = result.replaceAll("&amp;", "&");
        result = result.replaceAll("&gt;", ">");
        return result;
    }
}
