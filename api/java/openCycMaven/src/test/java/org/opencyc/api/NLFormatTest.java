/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.api;

import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.*;
import static org.junit.Assert.*;
import org.junit.*;
import org.opencyc.api.NLFormat.GenerationMode;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.Guid;

/**
 *
 * @author baxter
 */
public class NLFormatTest {

  public static final Object EMU = new CycConstant("Emu", new Guid("c01a4ba0-9c29-11b1-9dad-c379636f7270"));
  public static final CycConstant PERSON = new CycConstant("Person", new Guid("bd588092-9c29-11b1-9dad-c379636f7270"));

  public NLFormatTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
    try {
      cyc = new CycAccess();
    } catch (Exception e) {
      cyc = CycAccess.getNewCycAccessInteractively();
    }
    System.out.println("Connected to " + cyc);
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
    cyc.close();
  }

  @Before
  public void setUp() {
  }

  @After
  public void tearDown() {
  }

  /**
   * Test of getInstance method, of class NLFormat.
   */
  @Test
  public void testGetInstance_CycAccess() {
    System.out.println("getInstance");
    NLFormat result = NLFormat.getInstance(cyc);
    assertTrue("Failed to get an NLFormat instance from " + cyc, result instanceof NLFormat);
  }

  /**
   * Test of getInstance method, of class NLFormat.
   */
  @Test
  public void testGetInstance_Locale_CycAccess() {
    System.out.println("getInstance");
    Locale locale = Locale.CHINA;
    NLFormat result = NLFormat.getInstance(locale, cyc);
    assertTrue("Failed to get an NLFormat instance from " + locale, result instanceof NLFormat);
    assertEquals("Locale of NL Format not correctly set ", locale, result.getLocale());
  }

  /**
   * Test of format method, of class NLFormat.
   */
  @Test
  public void testFormat() {
    System.out.println("format");
    Object obj = 2;
    StringBuffer toAppendTo = new StringBuffer();
    FieldPosition pos = new FieldPosition(NumberFormat.INTEGER_FIELD);
    NLFormat instance = NLFormat.getInstance(cyc);
    String expResult = "2";
    StringBuffer result = instance.format(obj, toAppendTo, pos);
    assertEquals(expResult, result.toString());
  }

  /**
   * Test of formatMultiple method, of class NLFormat.
   */
  @Test
  public void testFormatMultiple() {
    System.out.println("formatMultiple");
    List<Integer> objList = Arrays.asList(1, 2, 3);
    NLFormat instance = NLFormat.getInstance(cyc);
    List<String> expResult = Arrays.asList("1", "2", "3");
    List result = instance.formatMultiple(objList);
    assertEquals(expResult, result);
  }

  /**
   * Test of parseObject method, of class NLFormat.
   */
  @Test
  public void testParseObject() {
    System.out.println("parseObject");
    String source = "emu";
    ParsePosition pos = new ParsePosition(0);
    NLFormat instance = NLFormat.getInstance(cyc);
    Object result = instance.parseObject(source, pos);
    assertEquals(EMU, result);
  }

  /**
   * Test of getFormatLanguageMt method, of class NLFormat.
   */
  @Test
  public void testGetFormatLanguageMt() {
    System.out.println("getFormatLanguageMt");
    NLFormat instance = NLFormat.getInstance(cyc);
    CycObject result = instance.getFormatLanguageMt();
    assertNotNull(result);
  }

  /**
   * Test of getParseLanguageMt method, of class NLFormat.
   */
  @Test
  public void testGetParseLanguageMt() {
    System.out.println("getParseLanguageMt");
    NLFormat instance = NLFormat.getInstance(cyc);
    Object result = instance.getParseLanguageMt();
    assertNotNull(result);
  }

  /**
   * Test of getDomainMt method, of class NLFormat.
   */
  @Test
  public void testGetDomainMt() {
    System.out.println("getDomainMt");
    NLFormat instance = NLFormat.getInstance(cyc);
    Object result = instance.getDomainMt();
    assertNotNull(result);
  }

  /**
   * Test of getMode method, of class NLFormat.
   */
  @Test
  public void testGetMode() {
    System.out.println("getMode");
    NLFormat instance = NLFormat.getInstance(cyc);
    GenerationMode result = instance.getMode();
    assertNotNull(result);
  }

  /**
   * Test of getLocale method, of class NLFormat.
   */
  @Test
  public void testGetLocale() {
    System.out.println("getLocale");
    NLFormat instance = NLFormat.getInstance(cyc);
    Locale expResult = Locale.getDefault();
    Locale result = instance.getLocale();
    assertEquals(expResult, result);

    instance = NLFormat.getInstance(Locale.CHINA, cyc);
    assertEquals(Locale.CHINA, instance.getLocale());
  }

  /**
   * Test of getRequiredIsas method, of class NLFormat.
   */
  @Test
  public void testGetRequiredIsas() {
    System.out.println("getRequiredIsas");
    NLFormat instance = NLFormat.getInstance(cyc);
    Collection expResult = Collections.emptyList();
    Collection result = new ArrayList(instance.getRequiredIsas());
    assertEquals(expResult, result);
  }

  /**
   * Test of getRequiredGenls method, of class NLFormat.
   */
  @Test
  public void testGetRequiredGenls() {
    System.out.println("getRequiredGenls");
    NLFormat instance = NLFormat.getInstance(cyc);
    Collection expResult = Collections.emptyList();
    Collection result = new ArrayList(instance.getRequiredGenls());
    assertEquals(expResult, result);
  }

  /**
   * Test of setLocale method, of class NLFormat.
   */
  @Test
  public void testSetLocale() {
    System.out.println("setLocale");
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setLocale(Locale.CHINA);
    assertEquals(Locale.CHINA, instance.getLocale());
  }

  /**
   * Test of setPrecise method, of class NLFormat.
   */
  @Test
  public void testSetPrecise() {
    System.out.println("setPrecise");
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setPrecise(false);
    assertFalse(instance.isPrecise());
    instance.setPrecise(true);
    assertTrue(instance.isPrecise());
  }

  /**
   * Test of setFormatLanguageMt method, of class NLFormat.
   */
  @Test
  public void testSetFormatLanguageMt() {
    System.out.println("setFormatLanguageMt");
    CycObject languageMt = new CycConstant("EnglishParaphraseMt", new Guid("bda16220-9c29-11b1-9dad-c379636f7270"));
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setFormatLanguageMt(languageMt);
    assertEquals(languageMt, instance.getFormatLanguageMt());
  }

  /**
   * Test of setParseLanguageMt method, of class NLFormat.
   */
  @Test
  public void testSetParseLanguageMt() {
    System.out.println("setParseLanguageMt");
    NLFormat instance = NLFormat.getInstance(cyc);
    CycObject languageMt = new CycConstant("EnglishMt", new Guid("bd588089-9c29-11b1-9dad-c379636f7270"));
    instance.setParseLanguageMt(languageMt);
    assertEquals(languageMt, instance.getParseLanguageMt());
  }

  /**
   * Test of setDomainMt method, of class NLFormat.
   */
  @Test
  public void testSetDomainMt() {
    System.out.println("setDomainMt");
    CycObject domainMt = new CycConstant("BaseKB", new Guid("bd588111-9c29-11b1-9dad-c379636f7270"));
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setDomainMt(domainMt);
    assertEquals(domainMt, instance.getDomainMt());
  }

  /**
   * Test of setMode method, of class NLFormat.
   */
  @Test
  public void testSetMode() {
    System.out.println("setMode");
    GenerationMode mode = GenerationMode.Text;
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setMode(mode);
    assertEquals(mode, instance.getMode());
  }

  /**
   * Test of setDisambiguate method, of class NLFormat.
   */
  @Test
  public void testSetDisambiguate() {
    System.out.println("setDisambiguate");
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.setDisambiguate(false);
    assertFalse(instance.shouldDisambiguate());
  }

  /**
   * Test of addRequiredIsa method, of class NLFormat.
   */
  @Test
  public void testAddRequiredIsa() {
    System.out.println("addRequiredIsa");
    CycObject isa = PERSON;
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.addRequiredIsa(isa);
    assertEquals(1, instance.getRequiredIsas().size());
    assertTrue(instance.getRequiredIsas().contains(isa));
  }

  /**
   * Test of addRequiredGenl method, of class NLFormat.
   */
  @Test
  public void testAddRequiredGenl() {
    System.out.println("addRequiredGenl");
    CycObject genl = PERSON;
    NLFormat instance = NLFormat.getInstance(cyc);
    instance.addRequiredGenl(genl);
    assertEquals(1, instance.getRequiredGenls().size());
    assertTrue(instance.getRequiredGenls().contains(genl));
  }

  /**
   * Test of parseObjects method, of class NLFormat.
   */
  @Test
  public void testParseObjects_String() {
    System.out.println("parseObjects");
    String denotationString = "person";
    NLFormat instance = NLFormat.getInstance(cyc);
    List result = instance.parseObjects(denotationString);
    assertTrue(result.contains(PERSON));
  }

  /**
   * Test of parseObjects method, of class NLFormat.
   */
  @Test
  public void testParseObjects_String_ParsePosition() {
    System.out.println("parseObjects");
    String denotationString = "one person";
    ParsePosition pos = new ParsePosition(4);
    NLFormat instance = NLFormat.getInstance(cyc);
    Collection result = instance.parseObjects(denotationString, pos);
    assertTrue(result.contains(PERSON));
  }
  static private CycAccess cyc;
}
