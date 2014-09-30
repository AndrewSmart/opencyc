package org.opencyc.util;

import java.util.StringTokenizer;

/**
 * Implements an ordered pair, two associated <code>Object</code>s.<p>
 *
 * @version $Id: Pair.java 138070 2012-01-10 19:46:08Z sbrown $
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
public class Pair extends AbstractPair{

  /**
   * Constructs a new Pair object.
   */
  public Pair(){
    }

  /**
   * Constructs a new Pair object given two components.
   *
   * @param component1 the first component of the Pair
   * @param component2 the second component of the Pair
   */
  public Pair(Object component1, Object component2) {super(component1, component2);}

  /**
   * Returns a <code>Pair</code> representation of a <code>String</code>.
   *
   * @param pairString the string to be parsed into a Pair object
   * @return a <coe>Pair</code> representation of a <code>String</code>.
   * @throws DataFormatException when the given string is not parsable to a
   *         <code>Pair</code>
   */
  public static Pair parsePair(String pairString) {
    StringTokenizer components = new StringTokenizer(pairString, separators);
    return new Pair(components.nextToken(), components.nextToken());
    }
}
