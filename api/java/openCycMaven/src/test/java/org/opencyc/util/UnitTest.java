package org.opencyc.util;

import org.opencyc.api.CycObjectFactory;
import org.opencyc.cycobject.CycList;

import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Currency;
import java.util.Date;
import junit.framework.Assert;
import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.textui.TestRunner;
import junit.framework.TestSuite;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycNaut;

/**
 * Provides a suite of JUnit test cases for the <tt>org.opencyc.constraintsolver</tt> package.<p>
 *
 * @version $Id: UnitTest.java 131054 2010-05-26 18:59:41Z baxter $
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

  /**
   * Main method in case tracing is prefered over running JUnit.
   */
  public static void main(String[] args) {
    TestRunner.run(suite());
  }

  /**
   * Constructs a new UnitTest object.
   * @param name the test case name.
   */
  public UnitTest(String name) {
    super(name);
  }

  /**
   * Runs the unit tests
   */
  public static Test suite() {
    TestSuite testSuite = new TestSuite();
    testSuite.addTest(new UnitTest("testChange"));
    testSuite.addTest(new UnitTest("testHasDuplicates"));
    testSuite.addTest(new UnitTest("testHasIntersection"));
    testSuite.addTest(new UnitTest("testRemoveDelimiters"));
    testSuite.addTest(new UnitTest("testIsDelimitedString"));
    testSuite.addTest(new UnitTest("testIsNumeric"));
    testSuite.addTest(new UnitTest("testWordsToString"));
    testSuite.addTest(new UnitTest("testEscapeDoubleQuotes"));
    testSuite.addTest(new UnitTest("testBase64"));
    testSuite.addTest(new UnitTest("testMyStreamTokenizer"));
    testSuite.addTest(new UnitTest("testIs7BitASCII"));
    testSuite.addTest(new UnitTest("testUnicodeEscaped"));
    testSuite.addTest(new UnitTest("testIsWhitespace"));
    testSuite.addTest(new UnitTest("testStripLeading"));
    testSuite.addTest(new UnitTest("testStripTrailing"));
    testSuite.addTest(new UnitTest("testStripTrailingBlanks"));
    testSuite.addTest(new UnitTest("testLogPrintln"));
    testSuite.addTest(new UnitTest("testMoneyConverter"));
    testSuite.addTest(new UnitTest("testDateConverter"));
    return testSuite;
  }

  /** Tests the StringUtils.change method. */
  public void testChange() {
    System.out.println("** testChange **");
    assertEquals("", StringUtils.change("", "", ""));
    assertEquals("a", StringUtils.change("a", "b", "c"));
    assertEquals("z", StringUtils.change("a", "a", "z"));
    assertEquals("xyz", StringUtils.change("abc", "abc", "xyz"));
    assertEquals("zbc", StringUtils.change("abc", "a", "z"));
    assertEquals("azc", StringUtils.change("abc", "b", "z"));
    assertEquals("abz", StringUtils.change("abc", "c", "z"));
    assertEquals("", StringUtils.change("abc", "abc", ""));
    assertEquals("a123c", StringUtils.change("abc", "b", "123"));
    assertEquals("123bc", StringUtils.change("abc", "a", "123"));
    assertEquals("ab123", StringUtils.change("abc", "c", "123"));
    final StringBuffer stringBuffer = new StringBuffer(100);
    stringBuffer.append("abc");
    stringBuffer.append('\n');
    stringBuffer.append("def");
    assertEquals("abc\\ndef", StringUtils.change(stringBuffer.toString(), "\n", "\\n"));
    System.out.println("** testChange OK **");
  }

  /**
   * Tests the OcCollectionUtils.hasDuplicates method.
   */
  public void testHasDuplicates() {
    System.out.println("** testHasDuplicates **");
    ArrayList collection1 = new ArrayList();
    collection1.add("a");
    collection1.add("b");
    collection1.add("b");
    assertTrue(OcCollectionUtils.hasDuplicates(collection1));
    ArrayList collection2 = new ArrayList();
    collection2.add("a");
    collection2.add("b");
    collection2.add("c");
    ArrayList arrayList = new ArrayList();
    arrayList.add("d");
    collection2.add(arrayList);
    collection2.add(arrayList);
    assertTrue(OcCollectionUtils.hasDuplicates(collection2));
    ArrayList collection3 = new ArrayList();
    collection3.add("a");
    collection3.add("b");
    collection3.add("c");
    assertTrue(!OcCollectionUtils.hasDuplicates(collection3));
    System.out.println("** testHasDuplicates OK **");
  }

  /**
   * Tests the OcCollectionUtils.hasIntersection method.
   */
  public void testHasIntersection() {
    System.out.println("** testHasIntersection **");
    ArrayList a = new ArrayList();
    ArrayList b = new ArrayList();
    assertTrue(!OcCollectionUtils.hasIntersection(a, b));
    a.add("a");
    assertTrue(!OcCollectionUtils.hasIntersection(a, b));
    b.add("a");
    assertTrue(OcCollectionUtils.hasIntersection(a, b));
    b.remove("a");
    assertTrue(!OcCollectionUtils.hasIntersection(a, b));
    a.add("b");
    a.add("d");
    a.add("e");
    a.add("f");
    b.add("f");
    assertTrue(OcCollectionUtils.hasIntersection(a, b));
    ArrayList bigA = new ArrayList();
    for (int i = 0; i < 150; i++) {
      bigA.add(new Integer(i));
    }
    ArrayList bigB = new ArrayList();
    for (int i = 0; i < 100; i++) {
      bigB.add(new Integer(i + 200));
    }
    assertTrue(!OcCollectionUtils.hasIntersection(bigA, bigB));
    assertTrue(!OcCollectionUtils.hasIntersection(bigB, bigA));
    assertTrue(OcCollectionUtils.hasIntersection(bigA, bigA));
    bigB.add(new Integer(10));
    assertTrue(OcCollectionUtils.hasIntersection(bigA, bigB));
    assertTrue(OcCollectionUtils.hasIntersection(bigB, bigA));
    System.out.println("** testHasIntersection OK **");
  }

  /**
   * Tests the StringUtils.removeDelimiters method.
   */
  public void testRemoveDelimiters() {
    System.out.println("** testRemoveDelimiters **");
    assertEquals("abc", StringUtils.removeDelimiters("\"abc\""));
    System.out.println("** testRemoveDelimiters OK**");
  }

  /**
   * Tests the StringUtils.isDelimitedString method.
   */
  public void testIsDelimitedString() {
    System.out.println("** testIsDelimitedString **");
    assertTrue(StringUtils.isDelimitedString("\"abc\""));
    assertTrue(StringUtils.isDelimitedString("\"\""));
    assertTrue(!StringUtils.isDelimitedString("\""));
    assertTrue(!StringUtils.isDelimitedString(new Integer(1)));
    assertTrue(!StringUtils.isDelimitedString("abc\""));
    assertTrue(!StringUtils.isDelimitedString("\"abc"));
    System.out.println("** testIsDelimitedString OK **");
  }

  /**
   * Tests the StringUtils.isNumeric method.
   */
  public void testIsNumeric() {
    System.out.println("** testIsNumeric **");
    assertTrue(StringUtils.isNumeric("0"));
    assertTrue(StringUtils.isNumeric("1"));
    assertTrue(StringUtils.isNumeric("2"));
    assertTrue(StringUtils.isNumeric("3"));
    assertTrue(StringUtils.isNumeric("4"));
    assertTrue(StringUtils.isNumeric("5"));
    assertTrue(StringUtils.isNumeric("6"));
    assertTrue(StringUtils.isNumeric("7"));
    assertTrue(StringUtils.isNumeric("8"));
    assertTrue(StringUtils.isNumeric("9"));
    assertTrue(!StringUtils.isNumeric("A"));
    assertTrue(!StringUtils.isNumeric("@"));
    assertTrue(!StringUtils.isNumeric("."));
    assertTrue(StringUtils.isNumeric("12345"));
    assertTrue(!StringUtils.isNumeric("123.45"));
    assertTrue(!StringUtils.isNumeric("123-45"));
    assertTrue(!StringUtils.isNumeric("12345+"));
    assertTrue(!StringUtils.isNumeric("+"));
    assertTrue(!StringUtils.isNumeric("-"));
    assertTrue(StringUtils.isNumeric("+1"));
    assertTrue(StringUtils.isNumeric("-1"));
    assertTrue(StringUtils.isNumeric("+12345"));
    assertTrue(StringUtils.isNumeric("-12345"));
    System.out.println("** testIsNumeric OK **");
  }

  /**
   * Tests the StringUtils.wordsToString method.
   */
  public void testWordsToString() {
    System.out.println("** testWordsToString **");
    ArrayList words = new ArrayList();
    assertEquals("", StringUtils.wordsToPhrase(words));
    words.add("word1");
    assertEquals("word1", StringUtils.wordsToPhrase(words));
    words.add("word2");
    assertEquals("word1 word2", StringUtils.wordsToPhrase(words));
    words.add("word3");
    assertEquals("word1 word2 word3", StringUtils.wordsToPhrase(words));

    System.out.println("** testWordsToString OK **");
  }

  /**
   * Tests the StringUtils.escapeDoubleQuotes method.
   */
  public void testEscapeDoubleQuotes() {
    System.out.println("** testEscapeDoubleQuotes **");
    String string = "";
    assertEquals(string, StringUtils.escapeDoubleQuotes(string));
    string = "1 2 3";
    assertEquals(string, StringUtils.escapeDoubleQuotes(string));
    string = "'1' '2' '3'";
    assertEquals(string, StringUtils.escapeDoubleQuotes(string));
    StringBuffer stringBuffer = new StringBuffer();
    stringBuffer.append("\"");
    stringBuffer.append("abc");
    stringBuffer.append("\"");
    string = stringBuffer.toString();
    String expectedString = "\\\"abc\\\"";
    String escapedString = StringUtils.escapeDoubleQuotes(string);
    assertEquals(expectedString, escapedString);

    System.out.println("** testEscapeDoubleQuotes OK **");
  }

  /**
   * Tests the Base64 methods.
   */
  public void testBase64() {
    System.out.println("** testBase64 **");
    CycList request = new CycList();
    request.add(CycObjectFactory.makeCycSymbol("list"));
    request.add(":none");
    request.add(CycObjectFactory.makeCycSymbol(":none"));
    String encodedRequest = null;
    Object response = null;
    Base64 base64 = new Base64();
    try {
      encodedRequest = base64.encodeCycObject(request, 0);
      response = base64.decodeCycObject(encodedRequest, 0);
    } catch (IOException e) {
      e.printStackTrace();
      Assert.fail(e.getMessage());
    }
    assertNotNull(response);
    assertTrue(response instanceof CycList);
    assertEquals(request, (CycList) response);

    request = new CycList();
    request.add(CycObjectFactory.makeCycSymbol("A"));
    request.setDottedElement(CycObjectFactory.makeCycSymbol("B"));
    encodedRequest = null;
    response = null;
    try {
      encodedRequest = base64.encodeCycObject(request, 0);
      response = base64.decodeCycObject(encodedRequest, 0);
    } catch (IOException e) {
      e.printStackTrace();
      Assert.fail(e.getMessage());
    }
    assertNotNull(response);
    assertTrue(response instanceof CycList);
    assertEquals(request, (CycList) response);
    System.out.println("** testBase64 OK **");
  }

  /**
   * Tests the MyStreamTokenizer methods.
   */
  public void testMyStreamTokenizer() {
    System.out.println("** testMyStreamTokenizer **");

    final String testString1 = "xyz\n;abc\ndef";

    final StringReader stringReader = new StringReader(testString1);
    final MyStreamTokenizer st = new MyStreamTokenizer(stringReader);
    st.resetSyntax();
    st.ordinaryChar('(');
    st.ordinaryChar(')');
    st.ordinaryChar('\'');
    st.ordinaryChar('`');
    st.ordinaryChar('.');
    st.whitespaceChars(0, ' ');
    st.quoteChar('"');
    st.wordChars('0', '9');
    st.wordChars('a', 'z');
    st.wordChars('A', 'Z');
    st.wordChars(128 + 32, 255);
    st.wordChars('=', '=');
    st.wordChars('+', '+');
    st.wordChars('-', '-');
    st.wordChars('_', '_');
    st.wordChars('<', '<');
    st.wordChars('>', '>');
    st.wordChars('*', '*');
    st.wordChars('/', '/');
    st.wordChars('#', '#');
    st.wordChars(':', ':');
    st.wordChars('!', '!');
    st.wordChars('$', '$');
    st.wordChars('?', '?');
    st.wordChars('%', '%');
    st.wordChars('&', '&');
    st.wordChars('.', '.');
    st.slashSlashComments(false);
    st.slashStarComments(false);
    st.commentChar(';');
    st.wordChars('?', '?');
    st.wordChars('%', '%');
    st.wordChars('&', '&');
    st.eolIsSignificant(false);

    try {
      st.nextToken();
      assertEquals("xyz", st.sval);
      st.nextToken();
      assertEquals("def", st.sval);
    } catch (IOException e) {
      Assert.fail(e.getMessage());
    }



    System.out.println("** testMyStreamTokenizer OK **");
  }

  /**
   * Tests the OcCollectionUtils.hasDuplicates method.
   */
  public void testIs7BitASCII() {
    System.out.println("** testIs7BitASCII **");
    assertTrue(StringUtils.is7BitASCII("abc"));
    StringBuffer sb = new StringBuffer();
    sb.append('a');
    sb.append((char) 140);
    assertTrue(!StringUtils.is7BitASCII(sb.toString()));
    sb.append('c');
    assertTrue(!StringUtils.is7BitASCII(sb.toString()));
    System.out.println("** testIs7BitASCII OK **");
  }

  public void testUnicodeEscaped() {
    System.out.println("** testUnicodeEscaped **");
    testUnicodeEscaped("abc", "abc");
    testUnicodeEscaped("ab\"c", "ab\\\"c");
    testUnicodeEscaped("ab\\c", "ab\\\\c");
    StringBuffer sb = new StringBuffer();
    sb.append((char) 0xff00);
    String testString = sb.toString();
    //System.out.println(testString);
    testUnicodeEscaped(testString, "&uff00;");
    sb = new StringBuffer();
    sb.append((char) 0xff00);
    sb.append(';');
    testString = sb.toString();
    //System.out.println(testString);
    testUnicodeEscaped(testString, "&uff00;;");
    sb = new StringBuffer();
    sb.append((char) 0xff00);
    sb.append(';');
    sb.append('a');
    testString = sb.toString();
    //System.out.println(testString);
    testUnicodeEscaped(testString, "&uff00;;a");
    System.out.println("** testUnicodeEscaped OK **");
  }

  private void testUnicodeEscaped(final String input, final String output) {
  	assertTrue(StringUtils.unicodeEscaped(input).equalsIgnoreCase("(list #$UnicodeStringFn \"" + output + "\")"));
  }

  /** Test isWhitespace. */
  public void testIsWhitespace() {
    System.out.println("** testIsWhitespace **");
    String string = "abc";
    assertTrue(!StringUtils.isWhitespace(string));
    string = " abc ";
    assertTrue(!StringUtils.isWhitespace(string));
    string = "";
    assertTrue(!StringUtils.isWhitespace(string));
    string = " ";
    assertTrue(StringUtils.isWhitespace(string));
    string = " \n\r\t  ";
    assertTrue(StringUtils.isWhitespace(string));
    System.out.println("** testIsWhitespace OK **");
  }

  /** Test stripLeading. */
  public void testStripLeading() {
    System.out.println("** testStripLeading **");
    String string = "abc";
    assertEquals("abc", StringUtils.stripLeading(string, ' '));
    string = "";
    assertEquals("", StringUtils.stripLeading(string, ' '));
    string = " abc ";
    assertEquals("abc ", StringUtils.stripLeading(string, ' '));
    string = "zzzzzzzzabc ";
    assertEquals("abc ", StringUtils.stripLeading(string, 'z'));
    string = "\n";
    assertEquals("", StringUtils.stripLeading(string, '\n'));
    System.out.println("** testStripLeading OK **");
  }

  /** Test stripTrailing. */
  public void testStripTrailing() {
    System.out.println("** testStripTrailing **");
    String string = "abc";
    assertEquals("abc", StringUtils.stripTrailing(string, ' '));
    string = "";
    assertEquals("", StringUtils.stripTrailing(string, ' '));
    string = " abc ";
    assertEquals(" abc", StringUtils.stripTrailing(string, ' '));
    string = " abczzzzzzzz";
    assertEquals(" abc", StringUtils.stripTrailing(string, 'z'));
    System.out.println("** testStripTrailing OK **");
  }

  /** Test stripTrailingBlanks. */
  public void testStripTrailingBlanks() {
    System.out.println("** testStripTrailingBlanks **");
    String string = "abc";
    assertEquals("abc", StringUtils.stripTrailingBlanks(string));
    string = "";
    assertEquals("", StringUtils.stripTrailingBlanks(string));
    string = " abc ";
    assertEquals(" abc", StringUtils.stripTrailingBlanks(string));
    string = " abc     ";
    assertEquals(" abc", StringUtils.stripTrailingBlanks(string));
    System.out.println("** testStripTrailingBlanks OK **");
  }

  /** Test Log println. */
  public void testLogPrintln() {
    System.out.println("** testLogPrintln **");
    Log.current.println("test log line");
    System.out.println("** testLogPrintln OK **");
  }

  /** Test MoneyConverter class */
  public void testMoneyConverter() {
    System.out.println("** testMoneyConverter **");
    final CycFort currUSD = MoneyConverter.lookupCycCurrencyTerm(Currency.getInstance("USD"));
    final BigDecimal amount = BigDecimal.valueOf(50.25);
    final CycNaut cycMoney = new CycNaut(currUSD, amount);
    final Money javaMoney = new Money(amount, Currency.getInstance("USD"));
    try {
      assertTrue(MoneyConverter.isCycMoney(cycMoney));
      assertEquals(javaMoney, MoneyConverter.parseCycMoney(cycMoney));
      assertEquals(cycMoney, MoneyConverter.toCycMoney(javaMoney));
      assertEquals(cycMoney, DataType.MONEY.convertJavaToCyc(javaMoney));
      assertEquals(javaMoney, DataType.MONEY.convertCycToJava(cycMoney));
    } catch (Exception e) {
      Assert.fail(e.getMessage());
    }
    System.out.println("** testMoneyConverter OK **");

  }

  /** Test DateConverter class */
  public void testDateConverter() {
    System.out.println("** testDateConverter **");
    final int year = 2008;
    final int dayOfMonth = 22;
    final int hour = 7;
    final int minute = 3;
    final CycNaut cycDate = new CycNaut(DateConverter.MINUTE_FN, minute,
            new CycNaut(DateConverter.HOUR_FN, hour,
            new CycNaut(DateConverter.DAY_FN, dayOfMonth,
            new CycNaut(DateConverter.MONTH_FN, DateConverter.APRIL,
            new CycNaut(DateConverter.YEAR_FN, year)))));
    final Calendar calendar = Calendar.getInstance();
    calendar.clear();
    calendar.set(year, Calendar.APRIL, dayOfMonth, hour, minute);
    final Date javaDate = calendar.getTime();
    try {
      assertTrue(DateConverter.isCycDate(cycDate));
      assertEquals(javaDate, DateConverter.parseCycDate(cycDate));
      assertEquals(cycDate, DateConverter.toCycDate(javaDate));
      assertEquals(cycDate, DataType.DATE.convertJavaToCyc(javaDate));
      assertEquals(javaDate, DataType.DATE.convertCycToJava(cycDate));
    } catch (Exception e) {
      Assert.fail(e.getMessage());
    }
    System.out.println("** testDateConverter OK **");

  }
}

