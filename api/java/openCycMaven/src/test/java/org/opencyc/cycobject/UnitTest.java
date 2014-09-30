package org.opencyc.cycobject;

//// External Imports
import java.io.IOException;
import java.math.BigInteger;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import static junit.framework.Assert.*;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.api.CycConnection;
import org.opencyc.api.CycObjectFactory;
import static org.opencyc.cycobject.CycList.makeCycList;
import static org.opencyc.cycobject.UnitTest.Constants.*;
import org.opencyc.util.CycUtils;
import org.opencyc.util.MyStreamTokenizer;
import org.opencyc.util.Span;
import org.opencyc.xml.Marshaller;
import org.opencyc.xml.XMLStringWriter;

/**
 * Provides a suite of JUnit test cases for the <tt>org.opencyc.cycobject</tt> package.<p>
 *
 * @version $Id: UnitTest.java 135652 2011-08-30 10:24:52Z baxter $
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
public class UnitTest extends TestCase {

  private static String hostname = CycConnection.DEFAULT_HOSTNAME;
  private static int port = CycConnection.DEFAULT_BASE_PORT;
  private static final boolean SHOULD_SET_CONNECTION_PARAMS_INTERACTIVELY = false;
  private static boolean areConnectionParamsSet = !SHOULD_SET_CONNECTION_PARAMS_INTERACTIVELY;
  private CycAccess cycAccess = null;

  /**
   * Main method in case tracing is preferred over running JUnit.
   */
  public static void main(String[] args) {
    TestRunner.run(suite());
    // close any threads left
    System.exit(0);
  }

  private CycAccess getCyc() {
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

  @Override
  protected void tearDown() throws Exception {
    super.tearDown();
    if (cycAccess != null) {
      cycAccess.close();
    }
  }

  /**
   * Construct a new UnitTest object.
   * @param name the test case name.
   */
  public UnitTest(String name) {
    super(name);
  }

  static public final class Constants {

    public static final CycConstant ARITY_RELATION_FN =
            new CycConstant("ArityRelationFn", new Guid("bf361058-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant TAME_ANIMAL =
            new CycConstant("TameAnimal", new Guid("c0fcd4a1-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant DOG =
            new CycConstant("#$Dog", CycObjectFactory.makeGuid("bd58daa0-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant CAT =
            new CycConstant("#$Cat", CycObjectFactory.makeGuid("bd590573-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant BRAZIL =
            new CycConstant("#$Brazil", CycObjectFactory.makeGuid("bd588f01-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant COLLECTION =
            new CycConstant("#$Collection", CycObjectFactory.makeGuid("bd5880cc-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant TRANSPORTATION_DEVICE_VEHICLE =
            new CycConstant("#$TransportationDevice-Vehicle", CycObjectFactory.makeGuid("c0bce169-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant YEAR_FN =
            new CycConstant("YearFn", CycObjectFactory.makeGuid("bd58f29a-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant CONVEY_FN =
            new CycConstant("ConveyFn", CycObjectFactory.makeGuid("c10afb3b-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant THE_LIST =
            new CycConstant("TheList", CycObjectFactory.makeGuid("bdcc9f7c-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant CITY_NAMED_FN =
            new CycConstant("CityNamedFn", CycObjectFactory.makeGuid("bd6870a6-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant ONTARIO_CANADIAN_PROVINCE =
            new CycConstant("Ontario-CanadianProvince", CycObjectFactory.makeGuid("bd58b6d5-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant REGISTRY_KEY_FN =
            new CycConstant("RegistryKeyFn", CycObjectFactory.makeGuid("e475c6b0-1695-11d6-8000-00a0c9efe6b4"));
    public static final CycConstant ISA =
            new CycConstant("isa", new Guid("bd588104-9c29-11b1-9dad-c379636f7270"));
    public static final CycConstant THING =
            new CycConstant("Thing", new Guid("bd5880f4-9c29-11b1-9dad-c379636f7270"));
    public static final CycVariable VAR_X = CycObjectFactory.makeCycVariable("X");
    public static final CycVariable VAR_Y = CycObjectFactory.makeCycVariable("Y");
    public static final CycVariable VAR_Z = CycObjectFactory.makeCycVariable("Z");
    public static final CycVariable VAR_VARIABLE = CycObjectFactory.makeCycVariable("VARIABLE");
    public static final CycVariable VAR_0 = new CycVariable("VAR0", 0);
  }

  /**
   * Runs the unit tests
   */
  public static Test suite() {
    TestSuite testSuite = new TestSuite();
    testSuite.addTest(new UnitTest("testCycNumber"));
    testSuite.addTest(new UnitTest("testCycFormula"));
    testSuite.addTest(new UnitTest("testELMTCycList"));
    testSuite.addTest(new UnitTest("testGuid"));
    testSuite.addTest(new UnitTest("testByteArray"));
    testSuite.addTest(new UnitTest("testCycAssertion"));
    testSuite.addTest(new UnitTest("testCycSymbol"));
    testSuite.addTest(new UnitTest("testCycVariable"));
    testSuite.addTest(new UnitTest("testCycConstant"));
    // testSuite.addTest(new UnitTest("testCycNart"));
    testSuite.addTest(new UnitTest("testStreamTokenizer"));
    testSuite.addTest(new UnitTest("testCycList"));
    testSuite.addTest(new UnitTest("testCycListVisitor"));
    testSuite.addTest(new UnitTest("testUnicodeString"));
    testSuite.addTest(new UnitTest("testCharacter"));
    testSuite.addTest(new UnitTest("testCycListPrettyStringDetails"));
    testSuite.addTest(new UnitTest("testCompactExternalIds"));
    testSuite.addTest(new UnitTest("testVariableNameOptimization"));
    testSuite.addTest(new UnitTest("testIsConditionalSentence"));
    testSuite.addTest(new UnitTest("testSubstituteDestructive"));
    testSuite.addTest(new UnitTest("testSubstituteNonDestructive"));
    testSuite.addTest(new UnitTest("testEqualsAtEL"));
    return testSuite;
  }

  public void testCycList50() throws NumberFormatException, CycApiException {

    // toHTMLPrettyString
    CycList cycList50 = getCyc().makeCycList("(QUOTE (#$and (#$isa ?UNIT #$ModernMilitaryOrganization) (#$objectFoundInLocation ?UNIT #$Illinois-State) (#$behaviorCapable ?UNIT (#$ReactionToSituationTypeFn #$ChemicalAttack) #$performedBy)))");
    assertEquals("<html><body>(QUOTE<br>&nbsp&nbsp(and<br>&nbsp&nbsp&nbsp&nbsp(isa ?UNIT ModernMilitaryOrganization)<br>&nbsp&nbsp&nbsp&nbsp(objectFoundInLocation ?UNIT Illinois-State)<br>&nbsp&nbsp&nbsp&nbsp(behaviorCapable ?UNIT<br>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp(ReactionToSituationTypeFn ChemicalAttack) performedBy)))</body></html>", cycList50.toHTMLPrettyString(""));
    // treeContains
    cycList50 = getCyc().makeCycList("(DEFMACRO-IN-API MY-MACRO (A B C) (RET ` (LIST , A , B , C)))");
    assertTrue(cycList50.treeContains(CycObjectFactory.backquote));

    // getValueForKeyword
    cycList50 = getCyc().makeCycList("(fipa-transport-message\n" + "  (envelope\n" + "    :to my-remote-agent\n" + "    :from my-cyc-agent\n" + "    :date 3215361678\n" + "    :X-agent-community :coabs\n" + "    :X-cyc-image-id \"balrog-200111112091457-939\"\n" + "    :X-base-tcp-port 3600)\n" + "  (payload\n" + "    (inform\n" + "      :sender my-cyc-agent\n" + "      :receiver my-remote-agent\n" + "      :reply-to message1\n" + "      :content \"Hello from my-cyc-agent\"\n" + "      :language :cycl\n" + "      :reply-with \"my cookie\"\n" + "      :ontology cyc-api\n" + "      :protocol :fipa-request)))");
    assertEquals(cycList50.size(), 3);
    assertEquals(cycList50.first(), CycObjectFactory.makeCycSymbol("fipa-transport-message"));
    assertTrue(cycList50.second() instanceof CycList);
    CycList envelope = (CycList) cycList50.second();
    assertEquals(CycObjectFactory.makeCycSymbol("my-remote-agent"), envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":to")));
    assertEquals(CycObjectFactory.makeCycSymbol("my-cyc-agent"), envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":from")));
    assertEquals(new Long("3215361678"), envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":date")));
    assertEquals(CycObjectFactory.makeCycSymbol(":coabs"), envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":X-agent-community")));
    assertEquals("balrog-200111112091457-939", envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":X-cyc-image-id")));
    assertEquals(Integer.valueOf(3600), envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":X-base-tcp-port")));
    assertNull(envelope.getValueForKeyword(CycObjectFactory.makeCycSymbol(":not-there")));
    assertTrue(cycList50.third() instanceof CycList);
    assertTrue(cycList50.third() instanceof CycList);
    CycList payload = (CycList) cycList50.third();
    assertTrue(payload.second() instanceof CycList);
    CycList aclList = (CycList) payload.second();
    assertEquals(CycObjectFactory.makeCycSymbol("my-cyc-agent"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":sender")));
    assertEquals(CycObjectFactory.makeCycSymbol("my-remote-agent"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":receiver")));
    assertEquals(CycObjectFactory.makeCycSymbol("message1"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":reply-to")));
    assertEquals("Hello from my-cyc-agent", aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":content")));
    assertEquals(CycObjectFactory.makeCycSymbol(":cycl"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":language")));
    assertEquals("my cookie", aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":reply-with")));
    assertEquals(CycObjectFactory.makeCycSymbol("cyc-api"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":ontology")));
    assertEquals(CycObjectFactory.makeCycSymbol(":fipa-request"), aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":protocol")));
    assertNull(aclList.getValueForKeyword(CycObjectFactory.makeCycSymbol(":not-there")));
  }

  /**
   * Tests the test harness itself.
   */
  public void testTestHarness() {
    System.out.println("\n*** testTestHarness ***");
    assertTrue(true);
    System.out.println("*** testTestHarness OK ***");
  }

  public static void testCycNumber() {
    System.out.println("\n*** testCycNumber ***");
    final CycNumber one = new CycNumber(1);
    assertTrue(one instanceof CycDenotationalTerm);
    assertTrue(one.equalsAtEL(1));
    final CycNumber oneTwoThree = CycObjectFactory.makeCycNumber(123);
    assertTrue(oneTwoThree instanceof Comparable);
    final CycNumber three = CycObjectFactory.makeCycNumber(3);
    final CycNumber minusZero = CycObjectFactory.makeCycNumber(-0.0);
    // Get a couple BigIntegers larger than the largest Double:
    BigInteger bigInteger = new BigInteger("212");
    BigInteger biggerInteger = new BigInteger("213");
    while (bigInteger.doubleValue() <= Double.MAX_VALUE) {
      bigInteger = biggerInteger;
      biggerInteger = biggerInteger.multiply(biggerInteger);
    }
    final CycNumber big = CycObjectFactory.makeCycNumber(bigInteger);
    final CycNumber bigger = CycObjectFactory.makeCycNumber(biggerInteger);
    assertNotSame(big, bigger);
    assertTrue(bigger.isGreaterThan(big));
    final List<CycNumber> numbers = Arrays.asList(three, big, minusZero, bigger);
    Collections.sort(numbers);
    assertEquals(minusZero, numbers.get(0));
    assertEquals(big, numbers.get(2));
    final CycNumber doubleOne = CycObjectFactory.makeCycNumber(1.0);
    assertNotSame(one, doubleOne);
    assertFalse(one.isGreaterThan(doubleOne));
    assertFalse(doubleOne.isGreaterThan(one));
    final CycNumber floatOne = CycObjectFactory.makeCycNumber(1.0F);
    assertNotSame(floatOne, doubleOne);
    assertNotSame(floatOne, one);
    assertEquals(doubleOne, doubleOne);
    final CycNumber plusZero = CycObjectFactory.makeCycNumber(0.0);
    assertNotSame(minusZero, plusZero);
    assertTrue(plusZero.isGreaterThan(minusZero));
    System.out.println("*** testCycNumber OK ***");
  }

  /**
   * Tests CycFormula functionality.
   */
  public void testCycFormula() {
    System.out.println("\n*** testCycFormula ***");
    final CycFormulaSentence isaXThing = CycFormulaSentence.makeCycFormulaSentence(ISA, VAR_X, THING);
    assertEquals(isaXThing.getFirstArgPositionForTerm(isaXThing), ArgPosition.TOP);
    final Set<CycConstant> gatheredConstants = isaXThing.treeGather(CycConstant.class);
    assertEquals(2, gatheredConstants.size());
    assertTrue(isaXThing.treeContains(isaXThing));
    assertTrue(isaXThing.treeContains(ISA));
    assertTrue(isaXThing.treeContains(VAR_X));
    assertTrue(isaXThing.treeContains(THING));
    assertTrue(isaXThing.contains(ISA));
    assertTrue(isaXThing.contains(VAR_X));
    assertTrue(isaXThing.contains(THING));
    final CycFormulaSentence someXIsaThing = CycFormulaSentence.makeCycFormulaSentence(
            CycFormulaSentence.THERE_EXISTS, VAR_X, isaXThing);
    assertTrue(someXIsaThing.contains(isaXThing));
    assertTrue(someXIsaThing.treeContains(isaXThing));
    assertTrue(someXIsaThing.treeContains(ISA));
    assertTrue(someXIsaThing.treeContains(VAR_X));
    assertTrue(someXIsaThing.treeContains(THING));
    assertEquals(new ArgPosition(1), isaXThing.getFirstArgPositionForTerm(VAR_X));
    assertEquals(new ArgPosition(2, 2), someXIsaThing.getFirstArgPositionForTerm(THING));
    {
      assertEquals(new ArrayList(Arrays.asList(VAR_X)), new ArrayList(isaXThing.findFreeVariables()));
      assertTrue(someXIsaThing.findFreeVariables().isEmpty());
      final CycFormulaSentence everyXIsaThing = CycFormulaSentence.makeCycFormulaSentence(
              CycFormulaSentence.FOR_ALL, VAR_X, isaXThing);
      assertTrue(everyXIsaThing.findFreeVariables().isEmpty());
      final CycFormulaSentence isaXY = CycFormulaSentence.makeCycFormulaSentence(ISA, VAR_X, VAR_Y);
      assertEquals(new ArrayList(Arrays.asList(VAR_X, VAR_Y)), new ArrayList(isaXY.findFreeVariables()));
      final CycFormulaSentence conj = CycFormulaSentence.makeConjunction(isaXThing, isaXY);
      assertEquals(new ArrayList(Arrays.asList(VAR_X, VAR_Y)), new ArrayList(conj.findFreeVariables()));
    }
    { //getSpecifiedObject()
      CycFormula formula2 = new CycFormula(getCyc().makeCycList("(1 (2 3 (4)) 5)"));
      ArgPosition pathSpecification = new ArgPosition(0);
      Object obj = formula2.getSpecifiedObject(pathSpecification);
      Object expectedObj = Integer.valueOf(1);
      assertEquals(expectedObj, obj);

      pathSpecification = new ArgPosition(1);
      obj = formula2.getSpecifiedObject(pathSpecification);
      expectedObj = formula2.getArg1();
      assertEquals(expectedObj, obj);

      pathSpecification = new ArgPosition(2);
      obj = formula2.getSpecifiedObject(pathSpecification);
      expectedObj = formula2.getArg2();
      assertEquals(expectedObj, obj);

      pathSpecification = new ArgPosition(1, 2, 0);
      obj = formula2.getSpecifiedObject(pathSpecification);
      expectedObj = 4;
      assertEquals(expectedObj, obj);

      // setSpecifedObject
      pathSpecification = new ArgPosition(0);
      formula2.setSpecifiedObject(pathSpecification, "a");
      expectedObj = new CycFormula(getCyc().makeCycList("(\"a\" (2 3 (4)) 5)"));
      assertEquals(expectedObj, formula2);

      pathSpecification = new ArgPosition(2);
      formula2.setSpecifiedObject(pathSpecification, "b");
      expectedObj = new CycFormula(getCyc().makeCycList("(\"a\" (2 3 (4)) \"b\")"));
      assertEquals(expectedObj, formula2);

      pathSpecification = new ArgPosition(1, 2, 0);
      formula2.setSpecifiedObject(pathSpecification, "c");
      expectedObj = new CycFormula(getCyc().makeCycList("(\"a\" (2 3 (\"c\")) \"b\")"));
      assertEquals(expectedObj, formula2);

      try {
        CycNart cycNart = new CycNart(ARITY_RELATION_FN, 1);
        formula2.addArg(cycNart);
        expectedObj = new CycFormula(getCyc().canonicalizeList(getCyc().makeCycList("(\"a\" (2 3 (\"c\")) \"b\" (#$ArityRelationFn 2))")));
        pathSpecification = new ArgPosition(3, 1);
        formula2.setSpecifiedObject(pathSpecification, 2);
        assertEquals(expectedObj, formula2);
      } catch (UnknownHostException ex) {
        Logger.getLogger(UnitTest.class.getName()).log(Level.SEVERE, null, ex);
      } catch (IOException ex) {
        Logger.getLogger(UnitTest.class.getName()).log(Level.SEVERE, null, ex);
      } catch (CycApiException ex) {
        Logger.getLogger(UnitTest.class.getName()).log(Level.SEVERE, null, ex);
      }

      // test getArgPositionsForTerm

      CycFormula list = new CycFormula(
              makeCycList(makeCycList("c", "1", "2"), "a", "b", "c",
              makeCycList("a", makeCycList("c", "10", "11"), "c", "2")));
      assertEquals("((\"c\" \"1\" \"2\") \"a\" \"b\" \"c\" (\"a\" (\"c\" \"10\" \"11\") \"c\" \"2\"))", list.toString());
      Set<ArgPosition> result = list.getArgPositionsForTerm("a");
      assertEquals(new HashSet(Arrays.asList(new ArgPosition(1), new ArgPosition(4, 0))), result);
      Set<ArgPosition> result1 = list.getArgPositionsForTerm("c");
      assertEquals(new HashSet(Arrays.asList(new ArgPosition(0, 0), new ArgPosition(3),
              new ArgPosition(4, 1, 0), new ArgPosition(4, 2))), result1);
      ArgPosition result2 = list.getFirstArgPositionForTerm("d");
      assertEquals(null, result2);
    }
    System.out.println("*** testCycFormula OK ***");
  }

  public void testCompactExternalIds() {
    System.out.println("\n*** testCompactExternalIds ***");
    try {
      final Object obj = CycAccess.baseKB;
      final String id = "Mx4rvViBEZwpEbGdrcN5Y29ycA";
      assertEquals(id, DefaultCycObject.toCompactExternalId(obj, getCyc()));
      assertEquals(obj, DefaultCycObject.fromCompactExternalId(id, getCyc()));
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testCompactExternalIds OK ***");
  }

  /**
   * Tests <tt>Guid</tt> object behavior.
   */
  public void testGuid() {
    System.out.println("\n*** testGuid ***");
    CycObjectFactory.resetGuidCache();
    assertEquals(0, CycObjectFactory.getGuidCacheSize());
    String guidString = "bd58c19d-9c29-11b1-9dad-c379636f7270";
    Guid guid = CycObjectFactory.makeGuid(guidString);
    assertEquals(1, CycObjectFactory.getGuidCacheSize());
    assertEquals(guidString, guid.toString());
    Guid guid2 = CycObjectFactory.getGuidCache(guidString);
    assertEquals(guid, guid2);
    Guid guid3 = CycObjectFactory.makeGuid(guidString);
    assertEquals(guid, guid3);
    assertEquals(1, CycObjectFactory.getGuidCacheSize());

    // toXML, toXMLString, unmarshall
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    try {
      guid.toXML(xmlStringWriter, 0, false);
      assertEquals("<guid>bd58c19d-9c29-11b1-9dad-c379636f7270</guid>\n", xmlStringWriter.toString());
      assertEquals("<guid>bd58c19d-9c29-11b1-9dad-c379636f7270</guid>\n", guid.toXMLString());
      String guidXMLString = guid.toXMLString();
      CycObjectFactory.resetGuidCache();
      Object object = CycObjectFactory.unmarshall(guidXMLString);
      assertTrue(object instanceof Guid);
      assertEquals(guid, (Guid) object);
      assertTrue(CycObjectFactory.unmarshall(guidXMLString)
              == CycObjectFactory.unmarshall(guidXMLString));
    } catch (Exception e) {
      failWithException(e);
    }

    System.out.println("*** testGuid OK ***");
  }

  /**
   * Tests <tt>CycSymbol</tt> object behavior.
   */
  public void testCycSymbol() {
    System.out.println("\n*** testCycSymbol ***");
    CycObjectFactory.resetCycSymbolCache();
    assertEquals(CycObjectFactory.RESET_SYMBOL_CACHE_SIZE, CycObjectFactory.getCycSymbolCacheSize());
    String symbolName = "WHY-ISA?";
    CycSymbol cycSymbol = CycObjectFactory.makeCycSymbol(symbolName);
    assertEquals(CycObjectFactory.RESET_SYMBOL_CACHE_SIZE + 1, CycObjectFactory.getCycSymbolCacheSize());
    assertEquals(symbolName, cycSymbol.toString());
    assertNotNull(CycObjectFactory.getCycSymbolCache(symbolName));
    CycSymbol cycSymbol2 = CycObjectFactory.getCycSymbolCache(symbolName);
    assertEquals(cycSymbol, cycSymbol2);
    CycSymbol cycSymbol3 = CycObjectFactory.makeCycSymbol(symbolName);
    assertEquals(cycSymbol, cycSymbol3);
    assertEquals(CycObjectFactory.RESET_SYMBOL_CACHE_SIZE + 1, CycObjectFactory.getCycSymbolCacheSize());
    String symbolName4 = "WHY-ISA?";
    CycSymbol cycSymbol4 = CycObjectFactory.makeCycSymbol(symbolName4);
    assertEquals(cycSymbol.toString(), cycSymbol4.toString());
    assertEquals(cycSymbol, cycSymbol4);
    try {
      testCycObjectRetrievable(cycSymbol);
    } catch (Exception ex) {
      failWithException(ex);
    }

    // compareTo
    ArrayList symbols = new ArrayList();
    symbols.add(CycObjectFactory.makeCycSymbol("isa?"));
    symbols.add(CycObjectFactory.makeCycSymbol("define-private"));
    symbols.add(CycObjectFactory.makeCycSymbol("nil"));
    Collections.sort(symbols);
    assertEquals("[DEFINE-PRIVATE, ISA?, NIL]", symbols.toString());

    // isKeyword
    CycSymbol cycSymbol5 = CycObjectFactory.makeCycSymbol("nil");
    assertFalse(cycSymbol5.isKeyword());
    CycSymbol cycSymbol6 = CycObjectFactory.makeCycSymbol(":pos");
    assertTrue(cycSymbol6.isKeyword());

    // isValidSymbolName
    assertTrue(CycSymbol.isValidSymbolName("t"));
    assertTrue(CycSymbol.isValidSymbolName("nil"));
    assertTrue(CycSymbol.isValidSymbolName("a_"));
    assertTrue(CycSymbol.isValidSymbolName("a-b"));
    assertTrue(CycSymbol.isValidSymbolName("a-b"));
    assertTrue(CycSymbol.isValidSymbolName("a-9b"));
    assertTrue(CycSymbol.isValidSymbolName("*MY-SYMBOL*"));
    assertFalse(CycSymbol.isValidSymbolName(" "));
    assertFalse(CycSymbol.isValidSymbolName("#$Brazil"));
    assertFalse(CycSymbol.isValidSymbolName("\"a-string\""));

    //packages
    CycSymbol symbol7 = new CycSymbol("CYC", "BLAH");
    CycSymbol symbol8 = new CycSymbol("|CYC|", "BLAH");
    CycSymbol symbol9 = new CycSymbol("CYC", "|BLAH|");
    CycSymbol symbol10 = new CycSymbol("|CYC|", "|BLAH|");
    assertEquals("CYC", symbol7.getPackageName());
    assertEquals("CYC", symbol8.getPackageName());
    assertEquals("CYC", symbol9.getPackageName());
    assertEquals("CYC", symbol10.getPackageName());
    assertEquals("CYC", symbol7.getPackageNamePrecise());
    assertEquals("CYC", symbol8.getPackageNamePrecise());
    assertEquals("CYC", symbol9.getPackageNamePrecise());
    assertEquals("CYC", symbol10.getPackageNamePrecise());
    assertEquals("BLAH", symbol7.getSymbolName());
    assertEquals("BLAH", symbol8.getSymbolName());
    assertEquals("BLAH", symbol9.getSymbolName());
    assertEquals("BLAH", symbol10.getSymbolName());
    assertEquals("BLAH", symbol7.getSymbolNamePrecise());
    assertEquals("BLAH", symbol8.getSymbolNamePrecise());
    assertEquals("BLAH", symbol9.getSymbolNamePrecise());
    assertEquals("BLAH", symbol10.getSymbolNamePrecise());
    assertEquals(symbol7, symbol8);
    assertEquals(symbol7, symbol9);
    assertEquals(symbol7, symbol10);
    assertEquals("BLAH", symbol7.toString());
    assertEquals("BLAH", symbol8.toString());
    assertEquals("BLAH", symbol9.toString());
    assertEquals("BLAH", symbol10.toString());
    assertEquals("CYC:BLAH", symbol7.toFullStringForced());
    assertEquals("CYC:BLAH", symbol8.toFullStringForced());
    assertEquals("CYC:BLAH", symbol9.toFullStringForced());
    assertEquals("CYC:BLAH", symbol10.toFullStringForced());
    assertEquals("CYC:BLAH", symbol7.toFullString("SL"));
    assertEquals("CYC:BLAH", symbol8.toFullString("SL"));
    assertEquals("CYC:BLAH", symbol9.toFullString("SL"));
    assertEquals("CYC:BLAH", symbol10.toFullString("SL"));
    assertEquals("BLAH", symbol10.toFullString("CYC"));
    assertFalse(symbol7.isKeyword());
    assertFalse(symbol8.isKeyword());
    assertFalse(symbol9.isKeyword());
    assertFalse(symbol10.isKeyword());

    CycSymbol symbol11 = new CycSymbol("|CYC RuLeS|", "|BLAH BiTeS|");
    CycSymbol symbol12 = new CycSymbol("CYC RuLeS", "BLAH BiTeS");
    assertEquals("CYC RuLeS", symbol11.getPackageName());
    assertEquals("CYC RuLeS", symbol12.getPackageName());
    assertEquals("|CYC RuLeS|", symbol11.getPackageNamePrecise());
    assertEquals("|CYC RuLeS|", symbol12.getPackageNamePrecise());
    assertEquals("BLAH BiTeS", symbol11.getSymbolName());
    assertEquals("BLAH BiTeS", symbol12.getSymbolName());
    assertEquals("|BLAH BiTeS|", symbol11.getSymbolNamePrecise());
    assertEquals("|BLAH BiTeS|", symbol12.getSymbolNamePrecise());
    assertEquals(symbol11, symbol12);
    assertEquals("|BLAH BiTeS|", symbol11.toString());
    assertEquals("|BLAH BiTeS|", symbol12.toString());
    assertEquals("|CYC RuLeS|:|BLAH BiTeS|", symbol11.toFullStringForced());
    assertEquals("|CYC RuLeS|:|BLAH BiTeS|", symbol12.toFullStringForced());
    assertEquals("|CYC RuLeS|:|BLAH BiTeS|", symbol11.toFullString("SL"));
    assertEquals("|CYC RuLeS|:|BLAH BiTeS|", symbol12.toFullString("SL"));
    assertEquals("|BLAH BiTeS|", symbol12.toFullString("CYC RuLeS"));
    assertFalse(symbol11.isKeyword());
    assertFalse(symbol12.isKeyword());

    CycSymbol symbol13 = new CycSymbol("KEYWORD", "BLAH");
    CycSymbol symbol14 = new CycSymbol("|KEYWORD|", "BLAH");
    CycSymbol symbol15 = new CycSymbol("", ":BLAH");
    CycSymbol symbol16 = new CycSymbol(null, ":BLAH");
    assertEquals("KEYWORD", symbol13.getPackageName());
    assertEquals("KEYWORD", symbol14.getPackageName());
    assertEquals("KEYWORD", symbol15.getPackageName());
    assertEquals("KEYWORD", symbol16.getPackageName());
    assertEquals("KEYWORD", symbol13.getPackageNamePrecise());
    assertEquals("KEYWORD", symbol14.getPackageNamePrecise());
    assertEquals("KEYWORD", symbol15.getPackageNamePrecise());
    assertEquals("KEYWORD", symbol16.getPackageNamePrecise());
    assertEquals("BLAH", symbol13.getSymbolName());
    assertEquals("BLAH", symbol14.getSymbolName());
    assertEquals("BLAH", symbol15.getSymbolName());
    assertEquals("BLAH", symbol16.getSymbolName());
    assertEquals("BLAH", symbol13.getSymbolNamePrecise());
    assertEquals("BLAH", symbol14.getSymbolNamePrecise());
    assertEquals("BLAH", symbol15.getSymbolNamePrecise());
    assertEquals("BLAH", symbol16.getSymbolNamePrecise());
    assertEquals(symbol13, symbol14);
    assertEquals(symbol13, symbol15);
    assertEquals(symbol13, symbol16);
    assertEquals(":BLAH", symbol13.toString());
    assertEquals(":BLAH", symbol14.toString());
    assertEquals(":BLAH", symbol15.toString());
    assertEquals(":BLAH", symbol16.toString());
    assertEquals("KEYWORD:BLAH", symbol13.toFullStringForced());
    assertEquals("KEYWORD:BLAH", symbol14.toFullStringForced());
    assertEquals("KEYWORD:BLAH", symbol15.toFullStringForced());
    assertEquals("KEYWORD:BLAH", symbol16.toFullStringForced());
    assertEquals(":BLAH", symbol13.toFullString("SL"));
    assertEquals(":BLAH", symbol14.toFullString("SL"));
    assertEquals(":BLAH", symbol15.toFullString("SL"));
    assertEquals(":BLAH", symbol16.toFullString("SL"));
    assertEquals(":BLAH", symbol16.toFullString("KEYWORD"));
    assertTrue(symbol13.isKeyword());
    assertTrue(symbol14.isKeyword());
    assertTrue(symbol15.isKeyword());
    assertTrue(symbol16.isKeyword());

    // toXML, toXMLString, unmarshall
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    try {
      cycSymbol6.toXML(xmlStringWriter, 0, false);
      assertEquals("<symbol>:POS</symbol>\n", xmlStringWriter.toString());
      assertEquals("<symbol>:POS</symbol>\n", cycSymbol6.toXMLString());
      String cycSymbolXMLString = cycSymbol6.toXMLString();
      Object object = CycObjectFactory.unmarshall(cycSymbolXMLString);
      assertTrue(object instanceof CycSymbol);
      assertEquals(cycSymbol6, (CycSymbol) object);
    } catch (Exception e) {
      failWithException(e);
    }

    System.out.println("*** testCycSymbol OK ***");
  }

  /**
   * Tests <tt>CycConstant</tt> object behavior.
   */
  public void testCycConstant() {
    System.out.println("\n*** testCycConstant ***");
    try {
      CycObjectFactory.resetCycConstantCaches();
      assertEquals(0, CycObjectFactory.getCycConstantCacheByNameSize());
      String guidString = TAME_ANIMAL.getGuid().getGuidString();
      String constantName = TAME_ANIMAL.getName();
      CycObjectFactory.addCycConstantCache(TAME_ANIMAL);
      assertNotNull(TAME_ANIMAL);
      assertEquals(1, CycObjectFactory.getCycConstantCacheByNameSize());

      // Attempt to create a duplicate returns the cached existing object.
      CycConstant cycConstant2 = new CycConstant(constantName, CycObjectFactory.makeGuid(guidString));
      CycObjectFactory.addCycConstantCache(cycConstant2);
      assertEquals(1, CycObjectFactory.getCycConstantCacheByNameSize());
      assertEquals(TAME_ANIMAL, cycConstant2);

      CycConstant cycConstant3 = new CycConstant(constantName, CycObjectFactory.makeGuid(guidString));
      CycObjectFactory.addCycConstantCache(cycConstant3);
      assertEquals(TAME_ANIMAL.toString(), cycConstant3.toString());
      assertEquals(TAME_ANIMAL.cyclify(), cycConstant3.cyclify());
      assertEquals(TAME_ANIMAL, cycConstant3);
      testCycObjectRetrievable(TAME_ANIMAL);


      // compareTo
      ArrayList constants = new ArrayList();

      constants.add(DOG);
      constants.add(CAT);
      constants.add(BRAZIL);
      constants.add(COLLECTION);
      Collections.sort(constants);
      assertEquals("[Brazil, Cat, Collection, Dog]", constants.toString());

      CycConstant cycConstant4 =
              TRANSPORTATION_DEVICE_VEHICLE;

      XMLStringWriter xmlStringWriter = new XMLStringWriter();
      cycConstant4.toXML(xmlStringWriter, 0, false);
      String expectedXML =
              "<constant>\n"
              + "  <guid>c0bce169-9c29-11b1-9dad-c379636f7270</guid>\n"
              + "  <name>TransportationDevice-Vehicle</name>\n"
              + "</constant>\n";
      assertEquals(expectedXML, xmlStringWriter.toString());
      assertEquals(expectedXML, cycConstant4.toXMLString());
      String cycConstantXMLString = cycConstant4.toXMLString();
      CycObjectFactory.resetCycConstantCaches();
      Object object = CycObjectFactory.unmarshall(cycConstantXMLString);
      assertTrue(object instanceof CycConstant);
      assertEquals(cycConstant4, (CycConstant) object);
    } catch (Exception e) {
      failWithException(e);
    }

    System.out.println("*** testCycConstant OK ***");
  }

  /**
   * Tests <tt>CycNart</tt> object behavior.
   */
  public void testCycNart() {
    System.out.println("\n*** testCycNart ***");
    try {
      CycNart cycNart = new CycNart(ARITY_RELATION_FN, 1);
      testCycObjectRetrievable(cycNart);
      CycNart arityRelationFn1 = cycNart;
      assertNotNull(cycNart);
      assertEquals("(ArityRelationFn 1)", cycNart.toString());
      assertEquals("(#$ArityRelationFn 1)", cycNart.cyclify());

      CycNart cycNart2 = new CycNart(ARITY_RELATION_FN, 1);
      assertEquals(cycNart.toString(), cycNart2.toString());
      assertEquals(cycNart, cycNart2);

      // compareTo
      ArrayList narts = new ArrayList();
      CycList<CycObject> nartCycList = new CycList<CycObject>();
      nartCycList.add(YEAR_FN);
      nartCycList.add(Integer.valueOf(2000));
      CycNart year2K = new CycNart(nartCycList);
      narts.add(year2K);
      assertEquals("[(YearFn 2000)]", narts.toString());
      CycConstant person =
              getCyc().getKnownConstantByGuid(
              CycObjectFactory.makeGuid("bd588092-9c29-11b1-9dad-c379636f7270"));
      CycList nartCycList2 = new CycList<CycObject>();
      nartCycList2.add(CONVEY_FN);
      nartCycList2.add(person);
      narts.add(new CycNart(nartCycList2));
      CycList nartCycList3 = new CycList<CycObject>();
      nartCycList3.add(ARITY_RELATION_FN);
      nartCycList3.add(Integer.valueOf(1));
      narts.add(new CycNart(nartCycList3));
      Collections.sort(narts);
      assertEquals("[(ArityRelationFn 1), (ConveyFn Person), (YearFn 2000)]",
              narts.toString());

      // hasFunctorAndArgs
      assertTrue(arityRelationFn1.hasFunctorAndArgs());
      assertFalse((new CycNart()).hasFunctorAndArgs());

      // toCycList()
      CycList cycList = new CycList<CycObject>();
      cycList.add(ARITY_RELATION_FN);
      cycList.add(Integer.valueOf(1));
      assertEquals(cycList, arityRelationFn1.toCycList());


      // check cfasl representation of narts in a list
      CycList myNarts = new CycList<CycObject>();
      myNarts.add(arityRelationFn1);
      CycNart arityRelationFn2 = new CycNart(ARITY_RELATION_FN, 2);
      myNarts.add(arityRelationFn2);

      for (int i = 0; i < myNarts.size(); i++) {
        assertTrue(myNarts.get(i) instanceof CycNart);
      }
      CycList command = new CycList();
      command.add(CycObjectFactory.makeCycSymbol("csetq"));
      command.add(CycObjectFactory.makeCycSymbol("my-narts"));
      command.addQuoted(myNarts);
      CycList myNartsBackFromCyc = getCyc().converseList(command);
      for (int i = 0; i < myNartsBackFromCyc.size(); i++) {
        assertTrue(myNartsBackFromCyc.get(i) instanceof CycNart);
        CycNart myNartBackFromCyc = (CycNart) myNartsBackFromCyc.get(i);
        assertTrue(myNartBackFromCyc.getFunctor() instanceof CycFort);
        assertTrue(myNartBackFromCyc.getArguments() instanceof ArrayList);
        ArrayList args = (ArrayList) myNartBackFromCyc.getArguments();
        for (int j = 0; j < args.size(); j++) {
          Object arg = args.get(j);
          assertTrue(arg instanceof Integer);
        }

      }

      // coerceToCycNart
      CycNart cycNart4 = new CycNart(ARITY_RELATION_FN, Integer.valueOf(1));
      assertEquals(cycNart4, CycNart.coerceToCycNart(cycNart4));
      CycList cycList4 = new CycList<CycObject>();
      cycList4.add(ARITY_RELATION_FN);
      cycList4.add(Integer.valueOf(1));
      assertEquals(cycNart2, CycNart.coerceToCycNart(cycList4));

      // toXML, toXMLString
      XMLStringWriter xmlStringWriter = new XMLStringWriter();
      cycNart4.toXML(xmlStringWriter, 0, false);
      System.out.println(xmlStringWriter.toString());

      String cycNartXMLString = cycNart4.toXMLString();
      System.out.println("cycNartXMLString\n" + cycNartXMLString);
      Object object = CycObjectFactory.unmarshall(cycNartXMLString);
      assertTrue(object instanceof CycNart);
      assertEquals(cycNart4, (CycNart) object);
      CycNart cycNart5 = new CycNart(THE_LIST, Integer.valueOf(1), "a string");
      cycNartXMLString = cycNart5.toXMLString();
      System.out.println("cycNartXMLString\n" + cycNartXMLString);
      object = CycObjectFactory.unmarshall(cycNartXMLString);
      assertTrue(object instanceof CycNart);
      assertEquals(cycNart5, (CycNart) object);

      // Check whether stringApiValue() behaves properly on a NART with a string argument
      CycNart attawapiskat = new CycNart(CITY_NAMED_FN, "Attawapiskat", ONTARIO_CANADIAN_PROVINCE);

      Object result = CycUtils.evalSubLWithWorker(getCyc(), attawapiskat.stringApiValue());
      assertTrue(result instanceof CycNart);
      assertEquals(attawapiskat, (CycNart) result);

      // Check whether stringApiValue() behaves properly on a NART
      // with a string that contains a character that needs to be escaped in SubL
      CycNart hklmSam = new CycNart(REGISTRY_KEY_FN, "HKLM\\SAM");

      Object result0 = CycUtils.evalSubLWithWorker(getCyc(), hklmSam.stringApiValue());
      assertTrue(result0 instanceof CycNart);
      assertEquals(hklmSam, (CycNart) result0);

      /*
      CycAssertion cycAssertion = getCyc().getAssertionById(Integer.valueOf(968857));
      CycNart complexNart = (CycNart) cycAssertion.getFormula().second();
      System.out.println(complexNart.toString());
      System.out.println(complexNart.cyclify());
       */
    } catch (Exception e) {
      failWithException(e);
    }

    System.out.println("*** testCycNart OK ***");
  }

  /**
   * Tests <tt>CycVariable</tt> object behavior.
   */
  public void testCycVariable() {
    System.out.println("\n*** testCycVariable ***");
    CycVariable cycVariable1 = VAR_X;
    assertNotNull(cycVariable1);
    assertEquals("?X", cycVariable1.toString());
    assertEquals("?X", cycVariable1.cyclify());
    assertEquals("'?X", cycVariable1.stringApiValue());
    CycVariable cycVariable2 = VAR_VARIABLE;
    assertNotNull(cycVariable2);
    assertEquals("?variable", cycVariable2.toString().toLowerCase());
    assertEquals("?variable", cycVariable2.cyclify().toLowerCase());
    assertEquals("'?variable", cycVariable2.stringApiValue().toLowerCase());
    CycVariable cycVariable3 = new CycVariable("?X");
    assertEquals(cycVariable1.toString(), cycVariable3.toString());
    assertEquals(cycVariable1.cyclify(), cycVariable3.cyclify());
    assertEquals(cycVariable1.stringApiValue(), cycVariable3.stringApiValue());
    assertEquals(cycVariable1, cycVariable3);

    // compareTo
    ArrayList variables = new ArrayList();
    variables.add(VAR_Y);
    variables.add(VAR_Z);
    variables.add(VAR_Y);
    variables.add(VAR_X);
    variables.add(VAR_Z);
    variables.add(VAR_X);
    Collections.sort(variables);
    assertEquals("[?X, ?X, ?Y, ?Y, ?Z, ?Z]", variables.toString().toUpperCase());
    CycVariable cycVariable1000 = new CycVariable(":X");
    assertNotSame(cycVariable1, cycVariable1000);

    try {
      testCycObjectRetrievable(VAR_0);
      testCycObjectRetrievable(VAR_X);
    } catch (Exception e) {
      failWithException(e);
    }

    // makeUniqueCycVariable
    CycVariable x = VAR_X;
    CycVariable x1 = CycObjectFactory.makeUniqueCycVariable(x);
    CycVariable x2 = CycObjectFactory.makeUniqueCycVariable(x);
    CycVariable x3 = CycObjectFactory.makeUniqueCycVariable(x);
    assertFalse((x.equals(x1)));
    assertFalse((x.equals(x2)));
    assertFalse((x.equals(x3)));
    assertFalse((x1.equals(x2)));
    assertTrue(x.cyclify().equals("?X"));
    assertTrue(x1.cyclify().startsWith("?X-"));
    assertTrue(x3.cyclify().startsWith("?X-"));

    // toXML, toXMLString, unmarshall
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    try {
      x.toXML(xmlStringWriter, 0, false);
      assertEquals("<variable>X</variable>\n", xmlStringWriter.toString());
      assertEquals("<variable>X</variable>\n", x.toXMLString());
      String cycVariableXMLString = x.toXMLString();
      CycObjectFactory.resetCycVariableCache();
      Object object = CycObjectFactory.unmarshall(cycVariableXMLString);
      assertTrue(object instanceof CycVariable);
      assertEquals(x, (CycVariable) object);
      assertTrue(CycObjectFactory.unmarshall(cycVariableXMLString)
              == CycObjectFactory.unmarshall(cycVariableXMLString));
    } catch (Exception e) {
      fail(e.getMessage());
    }

    System.out.println("*** testCycVariable OK ***");
  }

  private void testCycObjectRetrievable(final CycObject obj) throws UnknownHostException, IOException {
    final String command = "(IDENTITY " + obj.stringApiValue() + ")";
    final CycObject retrievedVersion = getCyc().converseCycObject(command);
    assertEquals("Retrieved version of " + obj + " is not 'equals' to the original.", obj, retrievedVersion);
  }

  /**
   * Tests StreamTokenizer CycList parsing behavior.
   */
  public void testStreamTokenizer() {
    System.out.println("\n*** testStreamTokenizer ***");
    try {
      String string = "()";
      MyStreamTokenizer st = CycListParser.makeStreamTokenizer(string);
      assertEquals(40, st.nextToken());
      assertEquals(41, st.nextToken());
      assertEquals(st.TT_EOF, st.nextToken());

      string = "(1)";
      st = CycListParser.makeStreamTokenizer(string);
      assertEquals(40, st.nextToken());

      int token = st.nextToken();
      assertEquals(st.TT_WORD, token);
      assertEquals("1", st.sval);

      assertEquals(41, st.nextToken());
      assertEquals(st.TT_EOF, st.nextToken());
      string = "(-10 -2 -1.0 -5.2E05)";
      st = CycListParser.makeStreamTokenizer(string);
      assertEquals(40, st.nextToken());

      token = st.nextToken();
      assertEquals(st.TT_WORD, token);
      assertEquals("-10", st.sval);

      token = st.nextToken();
      assertEquals(st.TT_WORD, token);
      assertEquals("-2", st.sval);

      token = st.nextToken();
      assertEquals(st.TT_WORD, token);
      assertEquals("-1.0", st.sval);

      token = st.nextToken();
      assertEquals(st.TT_WORD, token);
      assertEquals("-5.2E05", st.sval);

      assertEquals(41, st.nextToken());
      assertEquals(st.TT_EOF, st.nextToken());

    } catch (Exception e) {
      e.printStackTrace();
      fail();
    }

    System.out.println("*** testStreamTokenizer OK ***");
  }

  /**
   * Tests <tt>CycList</tt> object behavior.
   */
  public void testCycList() {
    System.out.println("\n*** testCycList ***");

    try {

      // Simple empty list constructor.
      ArrayList arrayList = new ArrayList();
      CycList cycList = new CycList<CycObject>(arrayList);
      assertNotNull(cycList);
      assertEquals("()", cycList.toString());

      // Construct list of one element.
      ArrayList arrayList2 = new ArrayList();
      CycObjectFactory.addCycConstantCache(BRAZIL);
      arrayList2.add(BRAZIL);
      CycList cycList2 = new CycList<CycObject>(arrayList2);
      assertEquals("(Brazil)", cycList2.toString());
      assertEquals("(#$Brazil)", cycList2.cyclify());

      // Construct list with embedded sublist.
      ArrayList arrayList3 = new ArrayList();
      arrayList3.add(BRAZIL);
      arrayList3.add(cycList);
      arrayList3.add(cycList2);
      CycList cycList3 = new CycList<CycObject>(arrayList3);
      assertEquals("(Brazil () (Brazil))", cycList3.toString());
      assertEquals("(#$Brazil () (#$Brazil))", cycList3.cyclify());

      // isValid()
      assertTrue(cycList.isValid());
      assertTrue(cycList2.isValid());
      assertTrue(cycList3.isValid());
      CycList cycList4 = new CycList(new Hashtable());
      assertFalse(cycList4.isValid());

      // first(), rest()
      ArrayList arrayList5 = new ArrayList();
      arrayList5.add(BRAZIL);
      CycList cycList5 = new CycList<CycObject>(arrayList5);
      assertEquals("(Brazil)", cycList5.toString());
      assertEquals("(#$Brazil)", cycList5.cyclify());
      assertEquals(cycList5.first(), BRAZIL);
      assertTrue(((CycList) (cycList5.rest())).size() == 0);
      CycList cycList5a = new CycList<CycObject>();
      cycList5a.add("a");
      cycList5a.setDottedElement("b");
      assertEquals("b", cycList5a.rest());

      // reverse()
      assertEquals(cycList5.toString(), cycList5.reverse().toString());
      assertEquals("((#$Brazil) () #$Brazil)", cycList3.reverse().cyclify());

      // reverse of strings.
      ArrayList arrayList6 = new ArrayList();
      arrayList6.add("z");
      arrayList6.add("y");
      arrayList6.add("x");
      CycList cycList6 = new CycList(arrayList6);
      assertEquals("(\"z\" \"y\" \"x\")", cycList6.toString());
      assertEquals("(\"x\" \"y\" \"z\")", cycList6.reverse().toString());

      // Improper lists.
      ArrayList arrayList7 = new ArrayList();
      arrayList7.add(Integer.valueOf(10));
      CycList cycList7 = new CycList(arrayList7);
      cycList7.setDottedElement(BRAZIL);
      assertTrue(cycList7.size() == 2);
      assertEquals("(10 . Brazil)", cycList7.toString());
      //CycListParser.verbosity = 10;

      CycListParser cycListParser = new CycListParser(null);
      CycList cycList7_1 = cycListParser.read("(a b c)");
      assertEquals("(A B C)", cycList7_1.toString());

      CycList cycList7a = getCyc().makeCycList("(a . (b . (c . (d))))");
      assertEquals("(A B C D)", cycList7a.toString());
      CycList cycList7b = getCyc().makeCycList("((a . b) . (c . d))");
      assertEquals("((A . B) C . D)", cycList7b.toString());
      CycList cycList7c = getCyc().makeCycList("((a . (b)) . (c . (d)))");
      assertEquals("((A B) C D)", cycList7c.toString());
      CycList cycList7d = getCyc().makeCycList("(a b . c)");
      assertEquals("(A B . C)", cycList7d.toString());
      CycList cycList7e = getCyc().makeCycList("(a b c . d)");
      assertEquals("(A B C . D)", cycList7e.toString());
      // construct
      Object object1 = CycList.construct(BRAZIL, CycObjectFactory.nil);
      assertNotNull(object1);
      assertTrue(object1 instanceof CycList);
      assertEquals("(Brazil)", object1.toString());
      // makeDottedPair
      CycList cycList8 = CycList.makeDottedPair(BRAZIL, "Atlantic");
      assertEquals("(Brazil . \"Atlantic\")", cycList8.toString());

      CycList cycList9 = CycList.makeDottedPair(BRAZIL, Integer.valueOf(1));
      assertEquals("(Brazil . 1)", cycList9.toString());

      CycList cycList10 = CycList.makeDottedPair(BRAZIL, CycObjectFactory.makeCycSymbol("foo"));
      assertEquals("(Brazil . FOO)", cycList10.toString());

      // Parse strings to make CycLists.
      String listAsString = "()";
      CycList cycList11 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList11.toString());
      listAsString = "(1)";
      CycList cycList12 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList12.toString());
      listAsString = "(1 2 3 4 5)";
      CycList cycList13 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList13.toString());
      listAsString = "(\"1\" \"bar\" A #$Brazil Z 4.25 :KEYWORD ?COLLECTION NIL)";
      CycList cycList14 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList14.cyclify());
      listAsString = "((A))";
      CycList cycList15 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList15.toString());
      listAsString = "((A) (B C) (((D))))";
      CycList cycList16 = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList16.toString());
      CycList cycList17 = getCyc().makeCycList(listAsString);
      assertEquals(cycList17.toString(), cycList16.toString());
      assertEquals(cycList17.toString(), cycList16.toString());
      assertEquals(getCyc().makeCycList("(A)"), cycList17.first());
      assertEquals(getCyc().makeCycList("(B C)"), cycList17.second());
      assertEquals(getCyc().makeCycList("(((D)))"), cycList17.third());
      listAsString = "(apply #'+ '(1 2 3))";
      CycList cycList18 = getCyc().makeCycList(listAsString);
      assertEquals("(APPLY (FUNCTION +) (QUOTE (1 2 3)))",
              cycList18.toString());
      listAsString = "(1 2 \n"
              + " ;; a comment \n"
              + " 3 4 5)";
      CycList cycList19 = getCyc().makeCycList(listAsString);
      assertEquals(cycList13, cycList19);
      listAsString = "(" + Double.toString(1.0E-05) + ")";
      CycList cycList19a = getCyc().makeCycList(listAsString);
      assertEquals(listAsString, cycList19a.cyclify());
      cycListParser = new CycListParser(getCyc());
      listAsString = "(1 2 3) 4 \"5 6\" 7 (8 9 10) 11 test";
      CycList cycList19b = cycListParser.read(listAsString);
      assertEquals("(1 2 3)", cycList19b.toString());
      assertEquals(" 4 \"5 6\" 7 (8 9 10) 11 test", cycListParser.remainingString());
      listAsString =
              "(#$ist-Asserted \n"
              + "  (#$totalInvestmentEarningsForStockTypeBoughtDuring  \n"
              + "    #$TechStock  \n"
              + "    (#$MinusFn (#$Pound-GreatBritain 330000000000))  \n"
              + "    (#$EarlyPartFn (#$YearFn 2000)))  \n"
              + "  #$TheMotleyFoolUKCorpusMt))";
      final CycList cycList19c = cycListParser.read(listAsString);
      assertTrue(cycList19c.cyclify().indexOf("330000000000") > -1);
      assertTrue(DefaultCycObject.cyclify(cycList19c).indexOf("330000000000") > -1);
      testCycListAdd();

      // subst
      cycList18 = getCyc().makeCycList("(b)");
      cycList19 = cycList18.subst(CycObjectFactory.makeCycSymbol("x"), CycObjectFactory.makeCycSymbol("a"));
      assertEquals(getCyc().makeCycList("(b)"), cycList19);
      CycList cycList20 = getCyc().makeCycList("(a)");
      CycList cycList21 = cycList20.subst(CycObjectFactory.makeCycSymbol("x"), CycObjectFactory.makeCycSymbol("a"));
      assertEquals(getCyc().makeCycList("(x)"), cycList21);
      CycList cycList22 = getCyc().makeCycList("((a))");
      CycList cycList23 = cycList22.subst(CycObjectFactory.makeCycSymbol("x"), CycObjectFactory.makeCycSymbol("a"));
      assertEquals(getCyc().makeCycList("((x))"), cycList23);
      CycList cycList24 = getCyc().makeCycList("((a) (b c) (((d))))");
      CycList cycList25 = cycList24.subst(CycObjectFactory.makeCycSymbol("x"), CycObjectFactory.makeCycSymbol("a"));
      assertEquals(getCyc().makeCycList("((x) (b c) (((d))))"), cycList25);

      // containsDuplicates
      CycList cycList26 = getCyc().makeCycList("(a b c d)");
      assertFalse(cycList26.containsDuplicates());
      CycList cycList27 = getCyc().makeCycList("(a a c d)");
      assertTrue(cycList27.containsDuplicates());
      CycList cycList28 = getCyc().makeCycList("(a b c c)");
      assertTrue(cycList28.containsDuplicates());
      CycList cycList29 = getCyc().makeCycList("(a (b) (b) c)");
      assertTrue(cycList29.containsDuplicates());

      // list
      CycList cycList30 = CycList.list(CycObjectFactory.makeCycSymbol("a"));
      assertEquals("(A)", cycList30.toString());
      CycList cycList31 = CycList.list(CycObjectFactory.makeCycSymbol("a"),
              CycObjectFactory.makeCycSymbol("b"));
      assertEquals("(A B)", cycList31.toString());
      CycList cycList32 = CycList.list(CycObjectFactory.makeCycSymbol("a"),
              CycObjectFactory.makeCycSymbol("b"),
              CycObjectFactory.makeCycSymbol("c"));
      assertEquals("(A B C)", cycList32.toString());

      // combinationsOf
      CycList cycList33 = getCyc().makeCycList("(1 2 3 4)");
      assertEquals("((1) (2) (3) (4))", cycList33.combinationsOf(1).toString());
      assertEquals("((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))",
              cycList33.combinationsOf(2).toString());
      assertEquals("((1 2 3 4))",
              cycList33.combinationsOf(4).toString());
      assertEquals("()",
              cycList33.combinationsOf(0).toString());
      assertEquals("()",
              (new CycList()).combinationsOf(4).toString());

      // randomPermutation
      CycList cycList34 = getCyc().makeCycList("(1 2 3 4 5 6 7 8 9 10)");
      CycList permutedCycList = cycList34.randomPermutation();
      assertEquals(10, permutedCycList.size());
      assertTrue(permutedCycList.contains(Integer.valueOf(2)));
      assertFalse(permutedCycList.containsDuplicates());

      // doesElementPrecedeOthers
      CycList cycList35 = getCyc().makeCycList("(1 2 3 4 5 6 7 8 9 10)");
      assertTrue(cycList35.doesElementPrecedeOthers(Integer.valueOf(1),
              getCyc().makeCycList("(8 7 6)")));
      assertTrue(cycList35.doesElementPrecedeOthers(Integer.valueOf(9),
              getCyc().makeCycList("(10)")));
      assertTrue(cycList35.doesElementPrecedeOthers(Integer.valueOf(10),
              getCyc().makeCycList("(18 17 16)")));
      assertFalse(cycList35.doesElementPrecedeOthers(Integer.valueOf(12),
              getCyc().makeCycList("(1 2 10)")));
      assertFalse(cycList35.doesElementPrecedeOthers(Integer.valueOf(9),
              getCyc().makeCycList("(8 7 6)")));

      // clone
      CycList cycList36 = getCyc().makeCycList("(1 2 3 4 5)");
      CycList cycList37 = (CycList) cycList36.clone();
      assertEquals(cycList36, cycList37);
      assertTrue(cycList36 != cycList37);
      CycList cycList38 = getCyc().makeCycList("(1 2 3 4 5 . 6)");
      CycList cycList39 = (CycList) cycList38.clone();

      assertEquals(cycList38, cycList39);
      assertTrue(cycList38 != cycList39);

      // deepCopy
      CycList cycList40 = getCyc().makeCycList("(1 2 3 4 5)");
      CycList cycList41 = (CycList) cycList40.deepCopy();
      assertEquals(cycList40, cycList41);
      assertTrue(cycList40 != cycList41);
      CycList cycList42 = getCyc().makeCycList("(1 2 3 4 5 . 6)");
      CycList cycList43 = (CycList) cycList42.deepCopy();
      assertEquals(cycList42, cycList43);
      assertTrue(cycList42 != cycList43);
      CycList cycList44 = getCyc().makeCycList("(1 (2 3) (4 5) ((6)))");
      CycList cycList45 = (CycList) cycList44.deepCopy();
      assertEquals(cycList44, cycList45);
      assertTrue(cycList44 != cycList45);
      assertEquals(cycList44.first(), cycList45.first());
      assertTrue(cycList44.first() == cycList45.first());
      assertEquals(cycList44.second(), cycList45.second());
      assertTrue(cycList44.second() != cycList45.second());
      assertEquals(cycList44.fourth(), cycList45.fourth());
      assertTrue(cycList44.fourth() != cycList45.fourth());
      assertEquals(((CycList) cycList44.fourth()).first(),
              ((CycList) cycList45.fourth()).first());
      assertTrue(((CycList) cycList44.fourth()).first()
              != ((CycList) cycList45.fourth()).first());

      // addNew
      CycList cycList46 = getCyc().makeCycList("(1 2 3 4 5)");
      assertEquals(5, cycList46.size());
      cycList46.addNew(Integer.valueOf(6));
      assertEquals(6, cycList46.size());
      cycList46.addNew(Integer.valueOf(2));
      assertEquals(6, cycList46.size());
      // addAllNew
      CycList cycList47 = getCyc().makeCycList("(1 2 3 4 5)");
      assertEquals(5, cycList47.size());
      CycList cycList48 = getCyc().makeCycList("(6 7 8 9 10)");
      assertEquals(5, cycList48.size());
      cycList47.addAllNew(cycList48);
      assertEquals(10, cycList47.size());
      CycList cycList49 = getCyc().makeCycList("(2 5 8 9 11)");
      assertEquals(5, cycList49.size());
      cycList47.addAllNew(cycList49);
      assertEquals(11, cycList47.size());

      // last
      cycList46 = getCyc().makeCycList("(8 7 6)");
      assertEquals(Integer.valueOf(6), cycList46.last());
      // toXML, toXMLString
      listAsString = "(\"1\" A (#$Brazil . Z) 4.25 :KEYWORD ?collection NIL . #$Dog)";
      cycList47 = getCyc().makeCycList(listAsString);
      XMLStringWriter xmlStringWriter = new XMLStringWriter();
      String cycListXMLString = cycList47.toXMLString();
      Object object = CycObjectFactory.unmarshall(cycListXMLString);
      assertTrue(object instanceof CycList);
      assertEquals(cycList47, (CycList) object);
      cycList48 =
              getCyc().makeCycList("(T (#$BiologicalTaxon "
              + "#$BiologicalSpecies "
              + "#$OrganismClassificationType "
              + "#$CycLTerm "
              + "#$CollectionType))");
      cycListXMLString = Marshaller.marshall(cycList48);
//      System.out.println(cycListXMLString);
      object = CycObjectFactory.unmarshall(cycListXMLString);
      assertTrue(object instanceof CycList);
      assertEquals(cycList48, (CycList) object);
      cycListXMLString =
              "\n<list>\n"
              + "  <symbol>QUOTE</symbol>\n"
              + "  <list>\n"
              + "    <symbol>A</symbol>\n"
              + "    <dotted-element>\n"
              + "      <symbol>B</symbol>\n"
              + "    </dotted-element>\n"
              + "  </list>\n"
              + "</list>\n";
      object = CycObjectFactory.unmarshall(cycListXMLString);
      assertTrue(object instanceof CycList);
      cycList49 = getCyc().makeCycList("(QUOTE (A . B))");
      assertEquals(cycList49, object);
      testCycList50();

      // addQuoted
      CycList cycList51 = new CycList();
      cycList51.add(Integer.valueOf(1));
      cycList51.addQuoted(CycObjectFactory.makeCycSymbol("quote-me"));
      assertEquals("(1 (QUOTE QUOTE-ME))", cycList51.toString());

      // toString (with null element)
      CycList cycList52 = new CycList();
      cycList52.add(null);
      assertNull(cycList52.first());
      assertEquals("(null)", cycList52.toString());

      // treeConstants
      CycList cycList54 =
              getCyc().makeCycList("(T (#$BiologicalTaxon "
              + "#$BiologicalSpecies "
              + "#$OrganismClassificationType "
              + "#$CycLTerm "
              + "#$CollectionType))");
      cycList54.add(new CycNart(getCyc().getKnownConstantByName("FruitFn"), getCyc().getKnownConstantByName("PumpkinPlant")));
      CycList cycList55 = cycList54.treeConstants();
      assertEquals(7, cycList55.size());

      // stringApiValue()
      CycConstant ontario = null;
      ontario =
              getCyc().getKnownConstantByGuid(
              CycObjectFactory.makeGuid("bd58b6d5-9c29-11b1-9dad-c379636f7270"));
      CycList cycList56 = new CycList(ontario);
      Object result56 = CycUtils.evalSubLWithWorker(getCyc(), cycList56.stringApiValue());
      assertTrue(result56 instanceof CycList);
      assertEquals(cycList56, (CycList) result56);
      // Check whether stringApiValue works properly on a CycList with a CycNart element
      CycConstant cityNamedFn =
              getCyc().getKnownConstantByGuid(
              CycObjectFactory.makeGuid("bd6870a6-9c29-11b1-9dad-c379636f7270"));
      CycNart attawapiskat = new CycNart(cityNamedFn, "Attawapiskat", ontario);
      CycList cycListWithNart = new CycList(ontario, attawapiskat);
      Object resultObj = CycUtils.evalSubLWithWorker(getCyc(), cycListWithNart.stringApiValue());
      assertTrue(resultObj instanceof CycList);
      assertEquals(cycListWithNart.cyclify(), ((CycList) resultObj).cyclify());
      // stringApiValue() on a CycList containing a String containing a double-quote
      CycList cycListWithString = new CycList(new String("How much \"wood\" would a \"woodchuck\" \"chuck\"?"));
      resultObj = CycUtils.evalSubLWithWorker(getCyc(), cycListWithString.stringApiValue());
      assertTrue(resultObj instanceof CycList);
      assertEquals(cycListWithString, (CycList) resultObj);

      // stringApiValue() on a dotted CycList
      CycList dottedCycList = new CycList("first element", "second element");
      dottedCycList.setDottedElement("dotted element");
      resultObj = CycUtils.evalSubLWithWorker(getCyc(), dottedCycList.stringApiValue());
      assertTrue(resultObj instanceof CycList);
      assertEquals(dottedCycList, (CycList) resultObj);
      // Parse a list containing a string with a backslash
      String script = "(identity \"abc\")";
      resultObj = CycUtils.evalSubLWithWorker(getCyc(), script);
      assertTrue(resultObj instanceof String);
      script = "(identity \"abc\\\\\")";
      resultObj = CycUtils.evalSubLWithWorker(getCyc(), script);
      assertTrue(resultObj instanceof String);
      CycList command = new CycList();
      command.add(CycObjectFactory.makeCycSymbol("identity"));
      command.add("abc\\");
      script = command.cyclifyWithEscapeChars();
      resultObj = CycUtils.evalSubLWithWorker(getCyc(), script);
      assertTrue(resultObj instanceof String);

    } catch (Exception e) {
      failWithException(e);
    }

    testUnmodifiableCycList();

    System.out.println("*** testCycList OK ***");
  }

  private static void testCycListAdd() {

    // add
    CycList<Long> longList = new CycList<Long>();
    long n = 16;
    longList.add(n);
    assertEquals(new CycList<Long>(n), longList);

    CycList<Boolean> booleanList = new CycList<Boolean>();
    booleanList.add(false);
    assertEquals(new CycList<Boolean>(false), booleanList);

    CycList<Float> floatList = new CycList<Float>();
    float f = 16.0f;
    floatList.add(f);
    assertEquals(new CycList<Float>(f), floatList);
  }

  /**
   * Tests <tt>CycListVisitor</tt> object behavior.
   */
  public void testCycListVisitor() {
    System.out.println("\n*** testCycListVisitor ***");

    CycListParser.verbosity = 0;

    try {
      CycList cycList2000 = getCyc().makeCycList("(1 . 24)");
      CycList cycList2001 = getCyc().makeCycList("(1 . 23)");
      assertFalse(cycList2001.equals(cycList2000));

      CycList cycList1 = getCyc().makeCycList("()");
      Enumeration e1 = cycList1.cycListVisitor();
      assertFalse(e1.hasMoreElements());

      CycList cycList2 = getCyc().makeCycList("(1 \"a\" :foo #$Brazil)");
      Enumeration e2 = cycList2.cycListVisitor();
      assertTrue(e2.hasMoreElements());
      Integer integer1 = Integer.valueOf(1);
      Object nextObject = e2.nextElement();
      assertTrue(nextObject instanceof Integer);
      assertTrue(((Integer) nextObject).intValue() == integer1.intValue());
      assertTrue(((Integer) nextObject).intValue() == 1);
      assertTrue(e2.hasMoreElements());
      assertEquals("a", e2.nextElement());
      assertTrue(e2.hasMoreElements());
      assertEquals(CycObjectFactory.makeCycSymbol(":foo"), e2.nextElement());
      assertTrue(e2.hasMoreElements());
      assertEquals(getCyc().makeCycConstant("#$Brazil"),
              e2.nextElement());
      assertFalse(e1.hasMoreElements());

      CycList cycList3 = getCyc().makeCycList("((()))");
      Enumeration e3 = cycList3.cycListVisitor();
      assertFalse(e3.hasMoreElements());

      CycList cycList4 = getCyc().makeCycList("(()())");
      Enumeration e4 = cycList4.cycListVisitor();
      assertFalse(e4.hasMoreElements());

      CycList cycList5 = getCyc().makeCycList("(\"a\" (\"b\") (\"c\") \"d\" \"e\")");
      Enumeration e5 = cycList5.cycListVisitor();
      assertTrue(e5.hasMoreElements());
      assertEquals("a", e5.nextElement());
      assertTrue(e5.hasMoreElements());
      assertEquals("b", e5.nextElement());
      assertTrue(e5.hasMoreElements());
      assertEquals("c", e5.nextElement());
      assertTrue(e5.hasMoreElements());
      assertEquals("d", e5.nextElement());
      assertTrue(e5.hasMoreElements());
      assertEquals("e", e5.nextElement());
      assertFalse(e5.hasMoreElements());

      CycList cycList6 = getCyc().makeCycList("(\"a\" (\"b\" \"c\") (\"d\" \"e\"))");
      Enumeration e6 = cycList6.cycListVisitor();
      assertTrue(e6.hasMoreElements());
      assertEquals("a", e6.nextElement());
      assertTrue(e6.hasMoreElements());
      assertEquals("b", e6.nextElement());
      assertTrue(e6.hasMoreElements());
      assertEquals("c", e6.nextElement());
      assertTrue(e6.hasMoreElements());
      assertEquals("d", e6.nextElement());
      assertTrue(e6.hasMoreElements());
      assertEquals("e", e6.nextElement());
      assertFalse(e6.hasMoreElements());
    } catch (Exception e) {
      failWithException(e);
    }

    System.out.println("*** testCycListVisitor OK ***");
  }

  /**
   * Tests the CycAssertion class.
   */
  public void testCycAssertion() {
    System.out.println("\n*** testCycAssertion ***");

    // stringApiValue() on a random assertion
    try {
      CycAssertion assertion = getCyc().getRandomAssertion();
      testCycObjectRetrievable(assertion);
      assertNotNull(assertion);
      String assertionAsString = assertion.stringApiValue();
      final Object assertionObject2 = getCyc().converseObject(assertionAsString);
      if (assertionObject2 instanceof CycAssertion) {
        final CycAssertion assertion2 = (CycAssertion) assertionObject2;
        assertEquals(assertion, assertion2);
      } else {
        System.err.println(assertionAsString + "\ndoes not returns the following which is not the expected assertion:\n" + assertionObject2);
      }
    } catch (Exception e) {
      failWithException(e);
    }

    // toXMLString()() on a random assertion
    try {
      final CycAssertion assertion = getCyc().getRandomAssertion();
      assertNotNull(assertion);
      final String assertionAsXML = assertion.toXMLString();
      assertNotNull(assertionAsXML);
      System.out.println();
    } catch (Exception e) {
      failWithException(e);
    }

    //TODO
        /*
    // toXML, toXMLString, unmarshall
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    try {
    String xmlString =
    "<assertion>\n" +
    "  <id>1000</id>\n" +
    "</assertion>\n";
    Object object = CycObjectFactory.unmarshall(xmlString);
    assertNotNull(object);
    assertTrue(object instanceof CycAssertion);
    CycAssertion cycAssertion = (CycAssertion) object;
    cycAssertion.toXML(xmlStringWriter, 0, false);
    assertEquals(xmlString, xmlStringWriter.toString());
    assertEquals(xmlString, cycAssertion.toXMLString());
    CycAssertion cycAssertion2 = new CycAssertion(new Integer (1000));
    assertEquals(cycAssertion2, cycAssertion);
    CycList cycList = new CycList();
    cycList.add(cycAssertion);
    //System.out.println(cycList.toXMLString());
    
    }
    catch (Exception e) {
    e.printStackTrace();
    fail(e.getMessage());
    }
     */
    System.out.println("*** testCycAssertion OK ***");
  }

  /**
   * Tests the ByteArray class.
   */
  public void testByteArray() {
    System.out.println("\n*** testByteArray ***");
    byte[] bytes = {0, 1, 2, 3, 4, -128};
    ByteArray byteArray1 = new ByteArray(bytes);
    assertNotNull(byteArray1);
    assertEquals(6, byteArray1.byteArrayValue().length);
    assertEquals(0, byteArray1.byteArrayValue()[0]);
    assertEquals(1, byteArray1.byteArrayValue()[1]);
    assertEquals(2, byteArray1.byteArrayValue()[2]);
    assertEquals(3, byteArray1.byteArrayValue()[3]);
    assertEquals(4, byteArray1.byteArrayValue()[4]);
    assertEquals(-128, byteArray1.byteArrayValue()[5]);
    byte[] bytes2 = {0, 1, 2, 3, 4, -128};
    ByteArray byteArray2 = new ByteArray(bytes2);
    assertEquals(byteArray1, byteArray1);
    assertEquals(byteArray1, byteArray2);
    byte[] bytes3 = {0, -1, 2, 3, 4, -128};
    ByteArray byteArray3 = new ByteArray(bytes3);
    assertFalse(byteArray1.equals(byteArray3));
    assertEquals("[ByteArray len:6 0,1,2,3,4,-128]", byteArray1.toString());

    // toXML, toXMLString, unmarshall
    XMLStringWriter xmlStringWriter = new XMLStringWriter();
    try {
      byteArray1.toXML(xmlStringWriter, 0, false);
      String expectedXmString =
              "<byte-vector>\n"
              + "  <length>6</length>\n"
              + "  <byte>0</byte>\n"
              + "  <byte>1</byte>\n"
              + "  <byte>2</byte>\n"
              + "  <byte>3</byte>\n"
              + "  <byte>4</byte>\n"
              + "  <byte>-128</byte>\n"
              + "</byte-vector>\n";

      assertEquals(expectedXmString, xmlStringWriter.toString());
      assertEquals(expectedXmString, byteArray1.toXMLString());
      assertEquals(byteArray1, CycObjectFactory.unmarshall(byteArray1.toXMLString()));
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testByteArray OK ***");
  }

  /**
   * Tests the ELMTCycList class.
   */
  public void testELMTCycList() {
    System.out.println("\n*** testELMTCycList ***");

    try {
      if (!getCyc().isOpenCyc()) {
        final CycObject mt = new CycNaut(getCyc().makeCycList("(#$MtSpace #$CyclistsMt (#$MtTimeWithGranularityDimFn (#$MonthFn #$January (#$YearFn 2004)) #$TimePoint))"));
        assertNotNull(getCyc().getComment(getCyc().isa, mt));
      }
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testELMTCycList OK ***");
  }

  /**
   * Tests the character support in the DefaultCycObject class.
   */
  public void testCharacter() {
    System.out.println("\n*** testCharacter ***");
    final Character[] testChars = {'a', 'A', '\t', ' '};
    for (final Character character : testChars) {
      final boolean testResult = DefaultCycObject.isCycLObject(character);
      assertTrue("char test " + character, testResult);
    }
    for (final Character character : testChars) {
      final String cyclified = DefaultCycObject.cyclify(character);
      final boolean testResult = cyclified.startsWith("#\\");
      assertTrue("char cyclify test " + character, testResult);
    }
    for (final Character character : testChars) {
      final String cyclified = DefaultCycObject.cyclifyWithEscapeChars(character, false);
      final boolean testResult = cyclified.startsWith("#\\\\");
      assertTrue("char escaped cyclify test " + character, testResult);
    }
    System.out.println("*** testCharacter OK ***");
  }

  /**
   * Tests the Unicode support in the DefaultCycObject class.
   */
  public void testUnicodeString() {
    System.out.println("\n*** testUnicodeString ***");
    String result = DefaultCycObject.cyclifyWithEscapeChars("abc", false);
    //System.out.println("abc test |"+result+"|");
    assertTrue("abc test", "\"abc\"".equals(result));



    result = DefaultCycObject.cyclifyWithEscapeChars("a\\b", false);
    //System.out.println("a\\b test |"+result+"|");
    assertTrue("a\\\\b test", "\"a\\\\b\"".equals(result));

    result = DefaultCycObject.cyclifyWithEscapeChars("a\"b", false);
    //System.out.println("a\"b test |"+result+"|");
    assertTrue("a\"c test", "\"a\\\"b\"".equals(result));

    StringBuffer sb = new StringBuffer();
    sb.append('a');
    sb.append((char) (0x140));
    result = DefaultCycObject.cyclifyWithEscapeChars(sb.toString(), false);
    //System.out.println("a&u140 test |"+result+"|");
    assertEquals("(#$UnicodeStringFn \"a&u140;\")", result);

    result = DefaultCycObject.cyclifyWithEscapeChars(sb.toString(), true);
    //System.out.println("a&u140 test |"+result+"|");
    assertEquals("(list #$UnicodeStringFn \"a&u140;\")", result);

    CycList list = new CycList();
    list.add(sb.toString());
    result = list.stringApiValue();
    //System.out.println("a&u140 test |"+result+"|");
    assertEquals("(list (list #$UnicodeStringFn \"a&u140;\"))", result);

    CycList list2 = new CycList();
    list2.add(list);
    result = list2.stringApiValue();
    //System.out.println("a&u140 test |"+result+"|");
    assertEquals("(list (list (list #$UnicodeStringFn \"a&u140;\")))", result);


    System.out.println("*** testUnicodeString OK ***");
  }

  private void checkPrettyStringDetail(Map<ArgPosition, Span> map, ArgPosition curPos,
          int expectedBegin, int expectedEnd) {
    Span span = map.get(curPos);
    assertNotNull(span);
    assertEquals(expectedBegin, span.getStart());
    assertEquals(expectedEnd, span.getEnd());
  }

  /**
   * Test the CycList pretty printer
   */
  public void testCycListPrettyStringDetails() {
    System.out.println("\n*** testCycListPrettyStringDetails ***");
    try {
      CycList example = null;
      Map<ArgPosition, Span> map = null;
      ArgPosition curPos = null;
      example = (CycList) org.opencyc.parser.CycLParserUtil.parseCycLTerm("(#$isa #$Muffet #$Dog)", true, getCyc());
      map = example.getPrettyStringDetails();
      checkPrettyStringDetail(map, ArgPosition.TOP, 0, 16);
      checkPrettyStringDetail(map, new ArgPosition(0), 1, 4);
      checkPrettyStringDetail(map, new ArgPosition(1), 5, 11);
      checkPrettyStringDetail(map, new ArgPosition(2), 12, 15);

      example = (CycList) org.opencyc.parser.CycLParserUtil.parseCycLTerm(
              "(#$isa (#$InstanceNamedFn \"Muffet\" (#$JuvenileFn #$Dog)) (#$JuvenileFn #$Dog))",
              true, getCyc());
      map = example.getPrettyStringDetails();
      checkPrettyStringDetail(map, ArgPosition.TOP, 0, 74);
      checkPrettyStringDetail(map, new ArgPosition(0), 1, 4);
      checkPrettyStringDetail(map, new ArgPosition(1, 0), 8, 23);
      checkPrettyStringDetail(map, new ArgPosition(1, 1), 24, 32);
      curPos = new ArgPosition(1, 2);
      curPos.extend(0);
      checkPrettyStringDetail(map, curPos, 38, 48);
      curPos = new ArgPosition(1, 2);
      curPos.extend(1);
      checkPrettyStringDetail(map, curPos, 49, 52);
      checkPrettyStringDetail(map, new ArgPosition(1, 2), 37, 53);
      checkPrettyStringDetail(map, new ArgPosition(1), 7, 54);
      checkPrettyStringDetail(map, new ArgPosition(2, 0), 58, 68);
      checkPrettyStringDetail(map, new ArgPosition(2, 1), 69, 72);
      checkPrettyStringDetail(map, new ArgPosition(2), 57, 73);

      final CycList<String> testList = new CycList<String>();
      final StringBuffer stringBuffer = new StringBuffer();
      stringBuffer.append('"');
      stringBuffer.append("abc");
      testList.add(stringBuffer.toString());
      final String testEscapedCyclifiedString = testList.toPrettyEscapedCyclifiedString("");
      assertEquals("(\"\\\"abc\")", testEscapedCyclifiedString);
    } catch (Exception e) {
      failWithException(e);
    }


    System.out.println("*** testCycListPrettyStringDetails OK ***");
  }

  private void testUnmodifiableCycList() {
    testEmptyCycListAdd();
    final CycList<Integer> frozenList = new CycList.UnmodifiableCycList<Integer>(CycList.makeCycList(1, 3, 2));
    testUnmodifiableCycListAdd(frozenList);
    testUnmodifiableCycListSort(frozenList);
  }

  private void testEmptyCycListAdd() {
    UnsupportedOperationException x = null;
    try {
      CycList.EMPTY_CYC_LIST.add(4);
    } catch (UnsupportedOperationException e) {
      x = e;
    }
    assertNotNull(x);
  }

  private void testUnmodifiableCycListAdd(final CycList frozenList) {
    UnsupportedOperationException x = null;
    try {
      frozenList.add(4);
    } catch (UnsupportedOperationException e) {
      x = e;
    }
    assertNotNull(x);
  }

  private void testUnmodifiableCycListSort(final CycList frozenList) {
    UnsupportedOperationException x = null;
    try {
      Collections.sort(frozenList);
    } catch (UnsupportedOperationException e) {
      x = e;
    }
    assertNotNull(x);
  }

  public void testVariableNameOptimization() {
    System.out.println("\n*** testVariableNameOptimization ***");
    boolean isOpenCyc =  false;
    try {
      isOpenCyc = getCyc().isOpenCyc();
      CycFormulaSentence sentence = getCyc().makeCycSentence("(#$likesAsFriend ?X ?Y)");
      Map<CycVariable, String> varMap = sentence.getOptimizedVarNames(getCyc());
      assertEquals(varMap.size(), 2);
    } catch (Exception e) {
      if (!isOpenCyc) {
        // this may or may not work under OpenCYC
        failWithException(e);
      }
    }
    System.out.println("*** testVariableNameOptimization OK ***");
  }

  public void testIsConditionalSentence() {
    System.out.println("\n*** testIsConditionalSentence ***");
    try {
      CycFormulaSentence sentence = getCyc().makeCycSentence("(#$likesAsFriend ?X ?Y)");
      boolean conditional = sentence.isConditionalSentence();
      assertEquals(conditional, false);
      sentence = getCyc().makeCycSentence("(#$implies (#$likesAsFriend ?X ?Y) (#$isa ?X #$Dog))");
      conditional = sentence.isConditionalSentence();
      assertEquals(conditional, true);

    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testVariableNameOptimization OK ***");
  }

  public void testSubstituteNonDestructive() {
    System.out.println("\n*** testSubstituteNonDestructive ***");
    try {
      CycFormulaSentence sentence = getCyc().makeCycSentence("(#$likesAsFriend ?X ?Y)");
      CycFormula newSentence =
              sentence.substituteNonDestructive(CycObjectFactory.makeCycVariable("X"), CycObjectFactory.makeCycVariable("Z"));
      assertFalse(sentence.equals(newSentence));
      assertFalse(sentence.equals(getCyc().makeCycSentence("(#$likesAsFriend ?Z ?Y)")));
      assertTrue(newSentence.equals(getCyc().makeCycSentence("(#$likesAsFriend ?Z ?Y)")));
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testSubstituteNonDestructive OK ***");
  }

  public void testSubstituteDestructive() {
    System.out.println("\n*** testSubstituteDestructive ***");
    try {
      CycFormulaSentence sentence = getCyc().makeCycSentence("(#$likesAsFriend ?X ?Y)");
      sentence =
              sentence.substituteNonDestructive(CycObjectFactory.makeCycVariable("X"), CycObjectFactory.makeCycVariable("Z"));
      assertTrue(sentence.equalsAtEL(getCyc().makeCycSentence("(#$likesAsFriend ?Z ?Y)")));
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testSubstituteDestructive OK ***");
  }

  public void testEqualsAtEL() {
    System.out.println("\n*** testEqualsAtEL ***");
    try {
      CycFormulaSentence sentence = getCyc().makeCycSentence("(#$likesAsFriend ?X ?Y)");
      assertTrue(sentence.equalsAtEL(getCyc().makeCycSentence("(#$likesAsFriend ?Z ?Y)")));
      CycFormulaSentence sentence2 = getCyc().makeCycSentence("(#$and (#$isa ?X #$Person) (#$likesAsFriend ?X ?Y))");
      assertTrue(sentence2.equalsAtEL(getCyc().makeCycSentence("(#$and (#$isa ?Z #$Person) (#$likesAsFriend ?Z ?Y))")));
      CycFormulaSentence sentence3 = getCyc().makeCycSentence("(#$and (#$isa ?Y #$Person) (#$likesAsFriend ?X ?Y))");
      assertFalse(sentence3.equalsAtEL(getCyc().makeCycSentence("(#$and (#$isa ?X #$Person) (#$likesAsFriend ?X ?Y))")));
    } catch (Exception e) {
      failWithException(e);
    }
    System.out.println("*** testEqualsAtEL OK ***");
  }

  private void failWithException(Exception e) {
    e.printStackTrace();
    fail(e.getMessage());
  }
}
