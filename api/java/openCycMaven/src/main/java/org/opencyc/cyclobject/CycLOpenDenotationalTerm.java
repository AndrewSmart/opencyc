package org.opencyc.cyclobject;

/*****************************************************************************
 * KB comment for #$CycLOpenDenotationalTerm as of 2002/05/07:<p>
 *
 * The collection of all open denotational terms in the CycL language.  An
 * expression is "open" if it contains one or more free variables (see
 * #$CycLOpenExpression).  A CycL term is said to be "denotational" if it is the
 * right sort of term to have a denotation (or value) in the universe of
 * discourse (see #$CycLDenotationalTerm).  Each instance of
 * #$CycLOpenDenotationalTerm is either a #$CycLOpenNonAtomicTerm (i.e. a "NAT"
 * with a free variable) or a #$CycLVariable iself.  Examples include `?X',
 * `(#$JuvenileFn ?X)', and `(#$JuvenileFn #$isa ?X #$genls #$JuvenileFn)' (even
 * though the latter is semantically ill-formed).<p>
 * 
 * @version $Id: CycLOpenDenotationalTerm.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau, Steve Reed
 *
 * <p>Copyright 2001 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://sf.net/projects/opencyc">OpenCyc at SourceForge</a>
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
 *****************************************************************************/
public interface CycLOpenDenotationalTerm 
  extends CycLOpenExpression, CycLDenotationalTerm {}
