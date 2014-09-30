package org.opencyc.cyclobject;

import java.util.List;

/*****************************************************************************
 * KB comment for #$CycLFormula as of 2002/05/07:<p>
 *
 * The collection of well-formed non-atomic CycL expressions.  Each instance of
 * #$CycLFormula consists of a CycL expression that denotes a relation (e.g. a
 * #$Predicate, #$Function-Denotational, or #$TruthFunction) -- or at least an
 * expression that could be interpreted as having a relation as its semantic
 * value (see #$CycLGenericRelationFormula) -- followed by one or more CycL
 * terms (see #$CycLTerm), with the entire sequence enclosed in parentheses.
 * For example, (#$isa #$Muffet #$Poodle) and (#$BirthFn #$Muffet) are both CycL
 * formulas.  Two important specializations of #$CycLFormula are
 * #$CycLNonAtomicTerm (whose instances are also called "denotational formulas")
 * and #$CycLSentences (whose instances are also called "logical formulas").
 * (Note that this notion of "formula" differs somewhat from that used in formal
 * logic, where a formula is normally defined as an (atomic or non-atomic,
 * quantificationally closed or open) sentence.)<p>
 * 
 * @version $Id: CycLFormula.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau, Steven Reed
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
 *      Copyright &copy; 2000 - 2006 Cycorp, Inc.  All rights reserved.
 *****************************************************************************/
public interface CycLFormula extends CycLTerm, List {

  CycLTerm getSubTerm(int index);

}
