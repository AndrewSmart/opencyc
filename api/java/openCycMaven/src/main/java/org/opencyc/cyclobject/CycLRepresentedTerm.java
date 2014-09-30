package org.opencyc.cyclobject;

/*****************************************************************************
 * KB comment for #$CycLRepresentedTerm as of 2002/05/07:<p>
 *
 * The collection of all denotational terms that are represented in the CycL
 * language, instead of being defined in SubL, the underlying implementation
 * language used by Cyc.  That is, each instance of #$CycLRepresentedTerm is
 * either (i) an atomic term, and thus also an instance of
 * #$CycLRepresentedAtomicTerm (q.v.), or (ii) a non-atomic term (or "NAT"), and
 * has a #$CycLRepresentedTerm as its arg0 functor (the other arguments in the
 * NAT need not be CycL represented terms).  Thus #$CycLRepresentedTerm has as
 * instances all #$CycLConstants, all #$CycLVariables, and all
 * #$CycLNonAtomicTerms.<p>
 *
 * @version $Id: CycLRepresentedTerm.java 138070 2012-01-10 19:46:08Z sbrown $
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
public interface CycLRepresentedTerm extends CycLDenotationalTerm {
}
