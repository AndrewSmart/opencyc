package org.opencyc.xml;

import java.io.*;
import org.opencyc.cycobject.*;

/**
 * Provides the behavior of a CYC-ML marshaller.<p>
 *
 * @version $Id: Marshaller.java 138070 2012-01-10 19:46:08Z sbrown $
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

public class Marshaller {

    /**
     * Marshalls the given object into its CYC-ML XML representation.
     *
     * @param object the object for marshalling
     * @return the CYC-ML XML representation string
     */
    public static String marshall(Object object) throws IOException {
        if (object instanceof Guid)
            return ((Guid) object).toXMLString();
        else if (object instanceof CycSymbol)
            return ((CycSymbol) object).toXMLString();
        else if (object instanceof CycVariable)
            return ((CycVariable) object).toXMLString();
        else if (object instanceof CycConstant)
            return ((CycConstant) object).toXMLString();
        else if (object instanceof CycNart)
            return ((CycNart) object).toXMLString();
        else if (object instanceof CycList)
            return ((CycList) object).toXMLString();
        else if (object instanceof String)
            return "<string>" + (String) object + "</string>\n";
        else if (object instanceof Integer)
            return "<integer>" + ((Integer) object).toString() + "</integer>\n";
        else if (object instanceof Double)
            return "<double>" + ((Double) object).toString() + "</double>\n";
        else if (object instanceof ByteArray)
            return ((ByteArray) object).toXMLString();
        else
            throw new IOException("Invalid object for marshalling " + object);
    }

}
