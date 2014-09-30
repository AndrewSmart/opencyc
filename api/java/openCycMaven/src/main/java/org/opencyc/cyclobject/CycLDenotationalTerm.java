package org.opencyc.cyclobject;

/*****************************************************************************
 * KB comment for #$CycLDenotationalTerm as of 2002/05/07:<p>
 *
 * The collection of all CycL terms that are not sentences (cf. #$CycLSentence),
 * and thus are either #$CycLAtomicTerms (such as constants or variables) or
 * #$CycLNonAtomicTerms (also known as "NAT"s).  #$CycLDenotationalTerms are
 * so-called, not because they all have denotations (not all of them do), but
 * because they are in a certain sense the right kind of term for having a
 * denotation.  That is, an instance of #$CycLDenotationalTerm -- if it is
 * semantically well-formed and closed (i.e. contains no free variables) --
 * might denote something in the universe of discourse.  (Even so, it will not
 * _necessarily_ denote something, considering (e.g.) the fact that a function
 * need not be defined for every (sequence of) thing(s) satisfying its argument
 * type constraints; see #$PartialDenotationalFunction.)  But note that neither
 * semantic well-formedness nor being closed is a requirement for being a CycL
 * denotational term: `(#$JuvenileFn #$isa ?X #$genls #$JuvenileFn)', for
 * example, is a NAT and thus a denotational term.  Other examples of
 * denotational terms are the expressions: `#$Muffet', `?X', `(#$JuvenileFn
 * ?X)', `(#$TheSetOf ?X (#$objectHasColor ?X #$GreenColor))', and `212'.  Note
 * also that, like most instances of #$CycLExpressionType,
 * #$CycLDenotationalTerm is a #$quotedCollection (q.v.).<p>
 * 
 * @version $Id: CycLDenotationalTerm.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau, Steven Reed
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
 *      Copyright &copy; 2000 - 2006 Cycorp, Inc.  All rights reserved.
 *****************************************************************************/
public interface CycLDenotationalTerm extends CycLTerm { }
