/* $Id: CycFormulaSentence.java 140312 2012-06-06 20:25:18Z mwitbrock $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.cycobject;

//// External Imports
import org.opencyc.api.SubLAPIHelper;
import org.opencyc.api.CycApiException;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycObjectFactory;
import static org.opencyc.api.SubLAPIHelper.makeSubLStmt;

/**
 * <P>CycSentence is designed to be an object that represents Sentences composed
 * of a truth function and a list of arguments
 *
 * <P>Copyright (c) 2008 Cycorp, Inc. All rights reserved. <BR>This software is
 * the proprietary information of Cycorp, Inc. <P>Use is subject to license
 * terms.
 *
 * Created on : Jul 6, 2009, 10:05:43 AM Author : baxter
 *
 * @version $Id: CycFormulaSentence.java 140312 2012-06-06 20:25:18Z mwitbrock $
 *
 * @todo make it implement CycLFormula, or get rid of CycLFormula, as
 * appropriate
 */
public class CycFormulaSentence extends CycFormula implements CycSentence {

  /**
   * Create and return a new CycSentence whose arguments are terms. CycList
   * arguments will be converted to CycNauts or CycSentences.
   *
   * @param terms
   */
  public CycFormulaSentence(Iterable<? extends Object> terms) {
    super(terms);
  }

  /**
   * Build a new CycSentence from terms.
   *
   */
  public static CycFormulaSentence makeCycFormulaSentence(Object... terms) {
    final CycFormulaSentence newSentence = new CycFormulaSentence();
    for (final Object arg : terms) {
      newSentence.addArg(arg);
    }
    return newSentence;
    //return new CycFormulaSentence(CycList.makeCycList(terms));
  }

  public static CycFormulaSentence makeConjunction(CycFormulaSentence... conjuncts) {
    return makeConjunction(Arrays.asList(conjuncts));
  }

  public static CycFormulaSentence makeConjunction(Iterable<CycFormulaSentence> conjuncts) {
    final CycFormulaSentence newSentence = makeCycFormulaSentence(AND);
    for (final Object conjunct : conjuncts) {
      newSentence.addArg(conjunct);
    }
    return newSentence;
  }

  public static CycFormulaSentence makeDisjunction(Iterable<CycFormulaSentence> conjuncts) {
    final CycFormulaSentence newSentence = makeCycFormulaSentence(OR);
    for (final Object conjunct : conjuncts) {
      newSentence.addArg(conjunct);
    }
    return newSentence;
  }

  public static CycFormulaSentence makeNegation(CycFormulaSentence sentence) {
    return makeCycFormulaSentence(NOT, sentence);
  }

  /**
   * Convert obj to a CycFormulaSentence if it looks like it ought to be one.
   */
  static public Object convertIfPromising(final Object obj) {
    if (obj instanceof List && !(obj instanceof CycFormulaSentence)) {
      final List<Object> termAsList = (List) obj;
      if (!termAsList.isEmpty() && termAsList.get(0) instanceof CycConstant) {
        final CycConstant arg0 = (CycConstant) termAsList.get(0);
        if (Character.isLowerCase(arg0.getName().charAt(0))) {
          return new CycFormulaSentence(termAsList);
        }
      }
    }
    return obj;
  }

  private CycFormulaSentence() {
  }

  @Override
  public boolean isConditionalSentence() {
    if (IMPLIES.equals(getOperator())) {
      return true;
    } else if (isConjunction() && getArity() == 1 && ((CycFormulaSentence) getArg(1)).isConditionalSentence()) {
      return true;
    } else {
      return false;
    }
  }

  @Override
  public boolean isConjunction() {
    return (AND.equals(getOperator()));
  }

  @Override
  public boolean isLogicalConnectorSentence() {
    return isLogicalOperatorFort(getOperator());
  }

  @Override
  public boolean isExistential() {
    return THERE_EXISTS.equals(getOperator());
  }

  @Override
  public boolean isUniversal() {
    return FOR_ALL.equals(getOperator());
  }

  private static boolean isLogicalOperatorFort(final Object obj) {
    return (LOGICAL_OPERATOR_FORTS.contains(obj));
  }

  public Map<CycVariable, String> getOptimizedVarNames(CycAccess access) throws UnknownHostException, IOException {
    Map<CycVariable, String> retMap = new HashMap<CycVariable, String>();
    String command = makeSubLStmt(PPH_OPTIMIZED_NAMES_FOR_VARIABLES, this);
    @SuppressWarnings("unchecked")
    CycList<CycObject> resultList = access.converseList(command);
    for (CycObject singleValue : resultList) {
      if (singleValue instanceof CycList
              && ((CycList) singleValue).first() instanceof CycVariable
              && ((CycList) singleValue).getDottedElement() instanceof String) {
        retMap.put((CycVariable) ((CycList) singleValue).first(), (String) (((CycList) singleValue).getDottedElement()));
      } else {
        throw new RuntimeException("Unable to interpret " + singleValue + " as an optimized variable name.");
      }
    }
    return retMap;
  }

  public CycSentence getEqualsFoldedSentence(CycAccess access) throws UnknownHostException, IOException {
    return getEqualsFoldedSentence(access, CycAccess.currentWorldDataMt);
  }

  public CycSentence getEqualsFoldedSentence(CycAccess access, ELMt mt) throws UnknownHostException, IOException {
    String command = null;
    try {
      command = "(with-inference-mt-relevance " + mt.stringApiValue() + " (fold-equals " + this.stringApiValue() + "))";
    } catch (Exception ex) {
      throw (new RuntimeException(ex));
    }
    Object rawResult = access.converseObject(command);
    CycSentence result;
    if (rawResult instanceof CycList) {
      result = new CycFormulaSentence((CycList) rawResult);
    } else if (rawResult instanceof CycConstant) {
      result = new CycConstantSentence((CycConstant) rawResult);
    } else {
      throw new CycApiException("getEqualsFoldedSentence returned " + rawResult
              + ", which is not a CycSentence.\nOriginal input: " + this.toString());
    }
    //System.out.println("FOLDED TO: "+result.toString());
    return result;
  }

  public CycSentence getSimplifiedSentence(CycAccess access) throws UnknownHostException, IOException {
    return getSimplifiedSentence(access, getDefaultSimplifierMt());
  }

  static synchronized ELMt getDefaultSimplifierMt() {
    if (simplifierMt == null && CycAccess.mtSpace != null) {
      simplifierMt = ELMtCycNaut.makeELMtCycNaut(Arrays.asList(CycAccess.mtSpace, CycAccess.currentWorldDataMt, CycAccess.anytimePSC));
    }
    return simplifierMt;
  }

  public CycSentence getSimplifiedSentence(CycAccess access, ELMt mt) throws UnknownHostException, IOException {
    String command = null;
    try {
      command = "(with-inference-mt-relevance " + mt.stringApiValue() + " (simplify-cycl-sentence (fold-equals "
              + this.stringApiValue() + ")))";
      //System.out.println("TRYING TO SIMPLIFY WITH:"+ command);
    } catch (Exception ex) {
      throw (new RuntimeException(ex));
    }
    Object rawResult = access.converseObject(command);
    CycSentence result;
    if (rawResult instanceof CycList) {
      result = new CycFormulaSentence((CycList) rawResult);
    } else if (rawResult instanceof CycConstant) {
      result = new CycConstantSentence((CycConstant) rawResult);
    } else {
      throw new CycApiException("getSimplifiedSentence returned " + rawResult
              + ", which is not a CycSentence.\nOriginal input: " + this.toString());
    }
    //System.out.println("SIMPLIFIED TO: "+result.toString());
    return result;
  }

  public CycFormulaSentence getExpandedSentence(CycAccess access) throws UnknownHostException, IOException {
      return getExpandedSentence(access, getDefaultSimplifierMt());
  }

  /**
   * Return a version of this with all expandable relations expanded into their more
   * verbose forms.  For example, this will expand Subcollection functions, as well as other
   * relations that have #$expansion's in the KB.
   * @param access
   * @param mt
   * @return
   * @throws UnknownHostException
   * @throws IOException 
   */
  public CycFormulaSentence getExpandedSentence(CycAccess access, ELMt mt) throws UnknownHostException, IOException {
    String command = null;
    try {
      command = "(el-expand-all " + this.stringApiValue() + " " + mt.stringApiValue() + ")";
    } catch (Exception ex) {
      throw (new RuntimeException(ex));
    }
    Object rawResult = access.converseObject(command);
    CycFormulaSentence result;
    if (rawResult instanceof CycList) {
      result = new CycFormulaSentence((CycList) rawResult);
    } else {
      throw new CycApiException("getExpandedSentence returned " + rawResult
              + ", which is not a CycFormulaSentence.\nOriginal input: " + this.toString());
    }
    //System.out.println("SIMPLIFIED TO: "+result.toString());
    return result;
  }

/**
 * Return a canonical version of this.  If two different sentences yield the same 
 * sentence after calling this method, then those two sentences are equal at the EL.
 * In other words, they are merely syntactic variants of the same semantic meaning.
 * @param access
 * @return
 * @throws UnknownHostException
 * @throws IOException 
 */
  public CycFormulaSentence getCanonicalElSentence(CycAccess access) throws UnknownHostException, IOException {
      return getCanonicalElSentence(access, getDefaultSimplifierMt(), true);
  }

  public CycFormulaSentence getCanonicalElSentence(CycAccess access, Boolean canonicalizeVars) throws UnknownHostException, IOException {
      return getCanonicalElSentence(access, getDefaultSimplifierMt(), canonicalizeVars);
  }
/**
 * Return a canonical version of this.  If two different sentences yield the same sentence after calling this method (with
 * canonicalizeVars set to True), then those two sentences are equal at the EL.
 * In other words, they are merely syntactic variants of the same semantic meaning.
 * @param access
 * @param mt
 * @param canonicalizeVars
 * @return
 * @throws UnknownHostException
 * @throws IOException 
 */
  public CycFormulaSentence getCanonicalElSentence(CycAccess access, ELMt mt, Boolean canonicalizeVars) throws UnknownHostException, IOException {
    String command = null;
    //need to add the following to the command..." " + DefaultCycObject.stringApiValue(canonicalizeVars) +
    try {
      command = "(canonicalize-el-sentence " + this.stringApiValue() + " " + mt.stringApiValue() + " " + DefaultCycObject.stringApiValue(canonicalizeVars) + ")";
    } catch (Exception ex) {
      throw (new RuntimeException(ex));
    }
    Object rawResult = access.converseObject(command);
    CycFormulaSentence result;
    if (rawResult instanceof CycList) {
      result = new CycFormulaSentence((CycList) rawResult);
    } else {
      throw new CycApiException("getCanonicalElSentence returned " + rawResult
              + ", which is not a CycFormulaSentence.\nOriginal input: " + this.toString());
    }
    return result;
  }

  
  /**
   * Is this sentence inconsistent with any of its constraints (e.g. predicate argument constraints)?  A false return value does not
   * mean that this meets all the constraints, but it means that it is not inconsistent with them.  For example, if an argument position
   * is constrained to be a spec of #$Mammal, and the argument is merely known to be a spec of #$Animal, then the argument does not meet all
   * of the constraints, but there are no constraint violations, and this method should return false.
   */
  public boolean hasWffConstraintViolations(CycAccess access, ELMt mt) {
    String command = null;
    try {
      command = "(el-lenient-wff-assertible? "
              + this.stringApiValue() + " " + mt.stringApiValue() + ")";
      Object rawResult = access.converseObject(command);
      boolean equalsT = CycObjectFactory.t.equals(rawResult);
      return (!equalsT);
    } catch (Exception ex) {
      throw (new CycApiException("Unable to decide whether " + this + " is well-formed in " + mt, ex));
    }
  }

  public String getNonWffAssertExplanation(CycAccess access) {
      try {  return getNonWffAssertExplanation(access, CycAccess.currentWorldDataMt);
    } catch (Exception ex) {
      throw (new CycApiException("Unable to retrieve explanation for why " + this + " is not well-formed in " +CycAccess.currentWorldDataMt, ex));
    }
  }

  /**
   * Returns a string that attempts to explain why this is not well-formed for
   * assertion. Return null if this is well-formed for assertion.
   */
  public String getNonWffAssertExplanation(CycAccess access, ELMt mt) {
    String command = null;
    try {
      command = "(with-inference-mt-relevance " + mt.stringApiValue() + " (opencyc-explanation-of-why-not-wff-assert "
              + this.stringApiValue() + " " + mt.stringApiValue() + "))";
      Object rawResult = access.converseObject(command);
      if (rawResult instanceof String) {
        return (String) rawResult;
      } else {
        return null;
      }
    } catch (Exception ex) {
      throw (new CycApiException("Unable to retrieve explanation for why " + this + " is not well-formed in " + mt, ex));
    }
  }

  public String getNonWffExplanation(CycAccess access) {
    return getNonWffExplanation(access, getDefaultSimplifierMt());
  }

  /**
   * Returns a string that attempts to explain why this is not well-formed for
   * any purpose. Return null if this is well-formed.  If you want to make an assertion with your sentence,
   * use the much more constraining {@link org.opencyc.cycobject.CycFormulaSentence#getNonWffAssertExplanation getNonWffAssertExplanation}.
   */
  public String getNonWffExplanation(CycAccess access, ELMt mt) {
    String command = null;
    try {
      command = "(with-inference-mt-relevance " + mt.stringApiValue() + " (opencyc-explanation-of-why-not-wff "
              + this.stringApiValue() + " " + mt.stringApiValue() + "))";
      Object rawResult = access.converseObject(command);
      if (rawResult instanceof String) {
        return (String) rawResult;
      } else {
        return null;
      }
    } catch (Exception ex) {
      throw (new CycApiException("Unable to retrieve explanation for why " + this + " is not well-formed in " + mt, ex));
    }
  }

  @Override
  public CycFormulaSentence deepCopy() {
    return new CycFormulaSentence(super.deepCopy().getArgsUnmodifiable());
  }

  public CycFormulaSentence substituteNonDestructive(CycObject original, CycObject replacement) {
    Map<CycObject, CycObject> map = new HashMap<CycObject, CycObject>();
    map.put(original, replacement);
    return (CycFormulaSentence) this.applySubstitutionsNonDestructive(map);
  }

  public void substituteDestructive(CycObject original, CycObject replacement) {
    Map<CycObject, CycObject> map = new HashMap<CycObject, CycObject>();
    map.put(original, replacement);
    this.applySubstitutionsDestructive(map);
    return;
  }

  /**
   * Returns the result of a tree substitution on the sentence. Note that this
   * leaves the original sentence unmodified.
   *
   * @param access
   * @param substitutions
   * @return The CycFormulaSentence resulting from the tree substitution.
   * @throws CycApiException
   * @throws IOException
   */
  public CycFormulaSentence treeSubstitute(CycAccess access, Map<CycObject, Object> substitutions) throws CycApiException, IOException {
    CycFormulaSentence result = this;
    if (substitutions != null) {
      for (CycObject o : substitutions.keySet()) {
        final String command = SubLAPIHelper.makeSubLStmt("tree-substitute", result, o, substitutions.get(o));
        result = access.converseSentence(command);
      }
    }
    return result;
  }

  @Override
  public Object clone() {
    return new CycFormulaSentence(args);
  }

  @Override
  public int compareTo(Object o) {
    if (o instanceof CycFormulaSentence) {
      return args.compareTo(((CycFormulaSentence) o).args);
    } else {
      return 0;
    }
  }
  private static final CycSymbol PPH_OPTIMIZED_NAMES_FOR_VARIABLES =
          CycObjectFactory.makeCycSymbol("pph-optimized-names-for-variables");
  public static final CycConstant AND = CycAccess.and;
  public static final CycConstant THERE_EXISTS = CycAccess.thereExistsConst;
  public static final CycConstant FOR_ALL = CycAccess.forAllConst;
  public static final CycConstant OR = CycAccess.or;
  public static final CycConstant NOT = CycAccess.not;
  public static final CycConstant UNKNOWN_SENTENCE = 
          new CycConstant("unknownSentence", new Guid("be1e5136-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant IMPLIES = CycAccess.impliesConst;
  private static final Collection<? extends Object> LOGICAL_OPERATOR_FORTS =
          Collections.unmodifiableCollection(
          Arrays.asList(AND, OR, NOT, UNKNOWN_SENTENCE, IMPLIES));
  private static ELMt simplifierMt = null;
}
