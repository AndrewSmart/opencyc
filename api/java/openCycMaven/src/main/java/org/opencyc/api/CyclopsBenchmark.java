package org.opencyc.api;

import java.net.*;
import java.io.*;
/**
 * Loads and executes the Cyclops (Cyc Logical Operations) benchmark.<p>
 *
 * @version $Id: CyclopsBenchmark.java 138070 2012-01-10 19:46:08Z sbrown $
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

public class CyclopsBenchmark {

    /**
     * Constructs a new CyclopsBenchmark object.
     */
    public CyclopsBenchmark() {
    }

    /**
     * Path to benchmark the subl file.
     */
    public String benchmarkFilePath = "benchmarks.lisp";

    /**
     * Main method to load and execute the Cyclops benchmark.
     */
    public static void main(String[] args) {
        CyclopsBenchmark cyclopsBenchmark = new CyclopsBenchmark();
        cyclopsBenchmark.execute();
    }

    public void execute () {
        Double cyclops = null;
        try {
            CycAccess cycAccess = new CycAccess();
            System.out.println("Loading benchmarks.lisp");
            String script = "(load \"" + benchmarkFilePath + "\")";
            cycAccess.converseVoid(script);
            script = "(benchmark-cyclops)";
            System.out.println("Running Cyclops benchmark");
            cyclops = (Double) cycAccess.converseObject(script);
        }
        catch (Exception e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
        System.out.println(cyclops + " Cyclops (Cyc Logical Operations Per Second)");
    }

}
