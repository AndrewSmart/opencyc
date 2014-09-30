/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.cycobject;

import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycConnection;

/**
 *
 * @author daves
 */
public class CycFormulaSentenceTest {

  private static String hostname = CycConnection.DEFAULT_HOSTNAME;
  private static int port = CycConnection.DEFAULT_BASE_PORT;
  private static final boolean SHOULD_SET_CONNECTION_PARAMS_INTERACTIVELY = false;
  private static boolean areConnectionParamsSet = !SHOULD_SET_CONNECTION_PARAMS_INTERACTIVELY;
  private static CycAccess cycAccess = null;
  
  public CycFormulaSentenceTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
    cycAccess = getCyc();
  }

  private static CycAccess getCyc() {
    if (cycAccess == null) {
      if (SHOULD_SET_CONNECTION_PARAMS_INTERACTIVELY && !areConnectionParamsSet) {
        cycAccess = CycAccess.getNewCycAccessInteractively();
        hostname = cycAccess.getHostName();
        port = cycAccess.getBasePort();
        areConnectionParamsSet = true;
      } else {
        try {
          cycAccess = new CycAccess(hostname, port);
        } catch (Exception ex) {
          fail(ex.getMessage());
        }
      }
    }
    return cycAccess;
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
    cycAccess.close();
  }
  
  @Before
  public void setUp() {
  }
  
  @After
  public void tearDown() {
  }

  /**
   * Test of makeCycFormulaSentence method, of class CycFormulaSentence.
   */
  //@Test
  public void testMakeCycFormulaSentence() {
    System.out.println("makeCycFormulaSentence");
    Object[] terms = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = CycFormulaSentence.makeCycFormulaSentence(terms);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of makeConjunction method, of class CycFormulaSentence.
   */
  //@Test
  public void testMakeConjunction_CycFormulaSentenceArr() {
    System.out.println("makeConjunction");
    CycFormulaSentence[] conjuncts = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = CycFormulaSentence.makeConjunction(conjuncts);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of makeConjunction method, of class CycFormulaSentence.
   */
  //@Test
  public void testMakeConjunction_Iterable() {
    System.out.println("makeConjunction");
    Iterable<CycFormulaSentence> conjuncts = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = CycFormulaSentence.makeConjunction(conjuncts);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of makeDisjunction method, of class CycFormulaSentence.
   */
  //@Test
  public void testMakeDisjunction() {
    System.out.println("makeDisjunction");
    Iterable<CycFormulaSentence> conjuncts = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = CycFormulaSentence.makeDisjunction(conjuncts);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of makeNegation method, of class CycFormulaSentence.
   */
  //@Test
  public void testMakeNegation() {
    System.out.println("makeNegation");
    CycFormulaSentence sentence = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = CycFormulaSentence.makeNegation(sentence);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of convertIfPromising method, of class CycFormulaSentence.
   */
  //@Test
  public void testConvertIfPromising() {
    System.out.println("convertIfPromising");
    Object obj = null;
    Object expResult = null;
    Object result = CycFormulaSentence.convertIfPromising(obj);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of isConditionalSentence method, of class CycFormulaSentence.
   */
  //@Test
  public void testIsConditionalSentence() {
    System.out.println("isConditionalSentence");
    CycFormulaSentence instance = null;
    boolean expResult = false;
    boolean result = instance.isConditionalSentence();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of isConjunction method, of class CycFormulaSentence.
   */
  //@Test
  public void testIsConjunction() {
    System.out.println("isConjunction");
    CycFormulaSentence instance = null;
    boolean expResult = false;
    boolean result = instance.isConjunction();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of isLogicalConnectorSentence method, of class CycFormulaSentence.
   */
  //@Test
  public void testIsLogicalConnectorSentence() {
    System.out.println("isLogicalConnectorSentence");
    CycFormulaSentence instance = null;
    boolean expResult = false;
    boolean result = instance.isLogicalConnectorSentence();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of isExistential method, of class CycFormulaSentence.
   */
  //@Test
  public void testIsExistential() {
    System.out.println("isExistential");
    CycFormulaSentence instance = null;
    boolean expResult = false;
    boolean result = instance.isExistential();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of isUniversal method, of class CycFormulaSentence.
   */
  //@Test
  public void testIsUniversal() {
    System.out.println("isUniversal");
    CycFormulaSentence instance = null;
    boolean expResult = false;
    boolean result = instance.isUniversal();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getOptimizedVarNames method, of class CycFormulaSentence.
   */
  //@Test
  public void testGetOptimizedVarNames() throws Exception {
    System.out.println("getOptimizedVarNames");
    CycAccess access = null;
    CycFormulaSentence instance = null;
    Map expResult = null;
    Map result = instance.getOptimizedVarNames(access);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getSimplifiedSentence method, of class CycFormulaSentence.
   */
  //@Test
  public void testGetSimplifiedSentence_CycAccess() throws Exception {
    System.out.println("getSimplifiedSentence");
    CycAccess access = null;
    CycFormulaSentence instance = null;
    CycSentence expResult = null;
    CycSentence result = instance.getSimplifiedSentence(access);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getSimplifiedSentence method, of class CycFormulaSentence.
   */
  //@Test
  public void testGetSimplifiedSentence_CycAccess_ELMt() throws Exception {
    System.out.println("getSimplifiedSentence");
    CycAccess access = null;
    ELMt mt = null;
    CycFormulaSentence instance = null;
    CycSentence expResult = null;
    CycSentence result = instance.getSimplifiedSentence(access, mt);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getNonWffAssertExplanation method, of class CycFormulaSentence.
   */
  @Test
  public void testGetNonWffAssertExplanation_CycAccess() {
    System.out.println("getNonWffAssertExplanation");
    CycFormulaSentence instance = cycAccess.makeCycSentence("(#$isa #$MarkDuggan-SoccerPlayer #$BiologicalLivingObject)");
    String expResult = null;
    String result = instance.getNonWffAssertExplanation(cycAccess);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
//    fail("The test case is a prototype.");
  }

  /**
   * Test of getNonWffAssertExplanation method, of class CycFormulaSentence.
   */
  @Test
  public void testGetNonWffAssertExplanation_CycAccess_ELMt() {
    System.out.println("getNonWffAssertExplanation");
    ELMt mt = CycFormulaSentence.getDefaultSimplifierMt();
    String result = null;
    CycFormulaSentence instance = cycAccess.makeCycSentence("(#$genls #$MarkDuggan-SoccerPlayer #$BiologicalLivingObject)");
    result = instance.getNonWffAssertExplanation(cycAccess, mt);
    assertTrue(result instanceof String);
    // TODO review the generated test code and remove the default call to fail.
//    fail("The test case is a prototype.");
  }

  /**
   * Test of getNonWffExplanation method, of class CycFormulaSentence.
   */
  //@Test
  public void testGetNonWffExplanation_CycAccess() {
    System.out.println("getNonWffExplanation");
    CycAccess access = null;
    CycFormulaSentence instance = null;
    String expResult = "";
    String result = instance.getNonWffExplanation(access);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getNonWffExplanation method, of class CycFormulaSentence.
   */
  //@Test
  public void testGetNonWffExplanation_CycAccess_ELMt() {
    System.out.println("getNonWffExplanation");
    CycAccess access = null;
    ELMt mt = null;
    CycFormulaSentence instance = null;
    String expResult = "";
    String result = instance.getNonWffExplanation(access, mt);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of deepCopy method, of class CycFormulaSentence.
   */
  //@Test
  public void testDeepCopy() {
    System.out.println("deepCopy");
    CycFormulaSentence instance = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = instance.deepCopy();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of substituteNonDestructive method, of class CycFormulaSentence.
   */
  //@Test
  public void testSubstituteNonDestructive() {
    System.out.println("substituteNonDestructive");
    CycObject original = null;
    CycObject replacement = null;
    CycFormulaSentence instance = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = instance.substituteNonDestructive(original, replacement);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of substituteDestructive method, of class CycFormulaSentence.
   */
  //@Test
  public void testSubstituteDestructive() {
    System.out.println("substituteDestructive");
    CycObject original = null;
    CycObject replacement = null;
    CycFormulaSentence instance = null;
    instance.substituteDestructive(original, replacement);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of treeSubstitute method, of class CycFormulaSentence.
   */
  //@Test
  public void testTreeSubstitute() throws Exception {
    System.out.println("treeSubstitute");
    CycAccess access = null;
    Map<CycObject, Object> substitutions = null;
    CycFormulaSentence instance = null;
    CycFormulaSentence expResult = null;
    CycFormulaSentence result = instance.treeSubstitute(access, substitutions);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of clone method, of class CycFormulaSentence.
   */
  //@Test
  public void testClone() {
    System.out.println("clone");
    CycFormulaSentence instance = null;
    Object expResult = null;
    Object result = instance.clone();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of compareTo method, of class CycFormulaSentence.
   */
  //@Test
  public void testCompareTo() {
    System.out.println("compareTo");
    Object o = null;
    CycFormulaSentence instance = null;
    int expResult = 0;
    int result = instance.compareTo(o);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }
}
