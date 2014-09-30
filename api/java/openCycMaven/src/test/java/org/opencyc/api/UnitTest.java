package org.opencyc.api;

import java.io.File;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import junit.framework.AssertionFailedError;
import static junit.framework.Assert.*;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import static org.opencyc.api.CycAccess.*;
import static org.opencyc.api.CycObjectFactory.*;
import org.opencyc.cycobject.CycAssertion;
import org.opencyc.cycobject.ByteArray;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycFormulaSentence;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycListParser;
import org.opencyc.cycobject.CycNart;
import org.opencyc.cycobject.CycNaut;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.CycVariable;
import org.opencyc.cycobject.DefaultCycObject;
import org.opencyc.cycobject.ELMt;
import org.opencyc.cycobject.Guid;
import org.opencyc.util.StringUtils;
import org.opencyc.inference.DefaultInferenceParameters;
import org.opencyc.inference.InferenceParameters;
import org.opencyc.inference.InferenceResultSet;

/**
 * Provides a unit test suite for the <tt>org.opencyc.api</tt> package
 *
 * @version $Id: UnitTest.java 135027 2011-07-15 21:35:08Z daves $
 * @author Stefano Bertolo
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
public class UnitTest extends TestCase implements CycLeaseManager.CycLeaseManagerListener {

  public static final String SUPERTAXONS_GUID_STRING = "bd58e36e-9c29-11b1-9dad-c379636f7270";
  /** the test host */
  public static String testHostName = CycConnection.DEFAULT_HOSTNAME;
  /** the test base port */
  public static int testBasePort = CycConnection.DEFAULT_BASE_PORT; 
  /** Indicates the use of a local CycConnection object to connect with a Cyc server. */
  public static final int LOCAL_CYC_CONNECTION = 1;
  /** Indicates the use of a java web service (SOAP XML) connection to the Cyc server. */
  public static final int SOAP_CYC_CONNECTION = 3;
  /** the connection mode */
  public static int connectionMode = LOCAL_CYC_CONNECTION;
//  public static int connectionMode = SOAP_CYC_CONNECTION;
  /** the SOAP service url */
  public static final String endpointURLString = "http://207.207.9.187/axis/services/CycSOAPService";
  /** the endpoint URL for the Cyc API web service */
  protected static URL endpointURL;
  /** Indicates whether unit tests should be performed only in binary api mode. */
  public static boolean performOnlyBinaryApiModeTests = false;
  //public static boolean performOnlyBinaryApiModeTests = true;
  /** indicates whether to enforce strict cyc-api function filtering */
  public static boolean enforceApiFunctions = false;
  private static final String AIR_BREATHING_VERTEBRATE_GUID_STRING = "bef7c9c1-9c29-11b1-9dad-c379636f7270";
  private static final String ALGERIA_GUID_STRING = "bd588c92-9c29-11b1-9dad-c379636f7270";
  private static final String ANIMAL_GUID_STRING = "bd58b031-9c29-11b1-9dad-c379636f7270";
  private static final String APPLE_TREE_GUID_STRING = "bd58c19d-9c29-11b1-9dad-c379636f7270";
  private static final String BIOLIGICAL_SPECIES_GUID_STRING = "bd58caeb-9c29-11b1-9dad-c379636f7270";
  private static final String BIOLOGY_VOCABULARY_MT_GUID_STRING = "bdd51776-9c29-11b1-9dad-c379636f7270";
  private static final String BRAZIL_GUID_STRING = "bd588f01-9c29-11b1-9dad-c379636f7270";
  private static final String BURNING_BUSH_GUID_STRING = "be846866-9c29-11b1-9dad-c379636f7270";
  private static final String CANINE_ANIMAL_GUID_STRING = "bd58d044-9c29-11b1-9dad-c379636f7270";
  private static final String CIA_WORLD_FACTBOOK_1995_MT_GUID_STRING = "c0a41a91-9c29-11b1-9dad-c379636f7270";
  private static final String CITY_NAMED_FN_GUID_STRING = "bd6870a6-9c29-11b1-9dad-c379636f7270";
  private static final String CONSTANT_NAME_GUID_STRING = "bd7183b0-9c29-11b1-9dad-c379636f7270";
  private static final String COUNTRY_GUID_STRING = "bd588879-9c29-11b1-9dad-c379636f7270";
  private static final String CYCL_TERM_GUID_STRING = "c107fffb-9c29-11b1-9dad-c379636f7270";
  private static final String DOMESTICATED_ANIMAL_GUID_STRING = "c10c22cd-9c29-11b1-9dad-c379636f7270";
  private static final String DONE_BY_GUID_STRING = "c0fd4798-9c29-11b1-9dad-c379636f7270";
  private static final String EXISTING_OBJECT_TYPE_GUID_STRING = "bd65d880-9c29-11b1-9dad-c379636f7270";
  private static final String FOX_GUID_STRING = "bd58be87-9c29-11b1-9dad-c379636f7270";
  private static final String FRUIT_FN_GUID_STRING = "bd58a976-9c29-11b1-9dad-c379636f7270";
  private static final String GENERAL_LEXICON_MT_GUID_STRING = "c109b867-9c29-11b1-9dad-c379636f7270";
  private static final String INTERNAL_PARTS_GUID_STRING = "bd58cf63-9c29-11b1-9dad-c379636f7270";
  private static final String JACKAL_GUID_STRING = "bd58c2de-9c29-11b1-9dad-c379636f7270";
  private static final String KE_REQUIREMENT_GUID_STRING = "c1141606-9c29-11b1-9dad-c379636f7270";
  private static final String LION_GUID_STRING = "bd58c467-9c29-11b1-9dad-c379636f7270";
  private static final String MALE_HUMAN_GUID_STRING = "bd58d6a1-9c29-11b1-9dad-c379636f7270";
  private static final String MICROTHEORY_GUID_STRING = "bd5880d5-9c29-11b1-9dad-c379636f7270";
  private static final String MODERN_MILITARY_MT_GUID_STRING = "c040a2f0-9c29-11b1-9dad-c379636f7270";
  private static final String MT_SPACE_GUID_STRING = "abb96eb5-e798-11d6-8ac9-0002b3a333c3";
  private static final String MT_TIME_WITH_GRANULARITY_DIM_FN_GUID_STRING = "47537943-331d-11d7-922f-0002b3a333c3";
  private static final String NEAREST_ISA_GUID_STRING = "bf411eed-9c29-11b1-9dad-c379636f7270";
  private static final String NON_PERSON_ANIMAL_GUID_STRING = "bd58e066-9c29-11b1-9dad-c379636f7270";
  private static final String NOW_GUID_STRING = "bd58a068-9c29-11b1-9dad-c379636f7270";
  private static final String OBJECT_TYPE_GUID_STRING = "bd58ab9d-9c29-11b1-9dad-c379636f7270";
  private static final String ORGANISM_CLASSIFICATION_TYPE_GUID_STRING = "bd58dfe4-9c29-11b1-9dad-c379636f7270";
  private static final String ORGANIZATION_GUID_STRING = "bd58d54f-9c29-11b1-9dad-c379636f7270";
  private static final String CAR_ACCIDENT_GUID_STRING = "bd58f4cd-9c29-11b1-9dad-c379636f7270";
  private static final String DOG_GUID_STRING = "bd58daa0-9c29-11b1-9dad-c379636f7270";
  private static final String PARAPHRASE_MT_GUID_STRING = "bf3ab672-9c29-11b1-9dad-c379636f7270";
  private static final String PENGUIN_GUID_STRING = "bd58a986-9c29-11b1-9dad-c379636f7270";
  private static final String PERCENT_OF_REGION_IS_GUID_STRING = "bfb0c6e5-9c29-11b1-9dad-c379636f7270";
  private static final String PERFORMED_BY_GUID_STRING = "bd58a962-9c29-11b1-9dad-c379636f7270";
  private static final String PHYSICAL_DEVICE_GUID_STRING = "bd58c72f-9c29-11b1-9dad-c379636f7270";
  private static final String PITTSBURGH_PENGUINS_GUID_STRING = "c08dec11-9c29-11b1-9dad-c379636f7270";
  private static final String PLANT_GUID_STRING = "bd58c6e1-9c29-11b1-9dad-c379636f7270";
  private static final String PLATO_GUID_STRING = "bd58895f-9c29-11b1-9dad-c379636f7270";
  private static final String RAINDROP_GUID_STRING = "bd58bec6-9c29-11b1-9dad-c379636f7270";
  private static final String RETRIEVER_DOG_GUID_STRING = "bd58e24b-9c29-11b1-9dad-c379636f7270";
  private static final String SIBLINGS_GUID_STRING = "bd58e3e9-9c29-11b1-9dad-c379636f7270";
  private static final String SINGLE_PURPOSE_DEVICE_GUID_STRING = "bd5897aa-9c29-11b1-9dad-c379636f7270";
  private static final String SWAZILAND_GUID_STRING = "bd588a92-9c29-11b1-9dad-c379636f7270";
  private static final String TAME_ANIMAL_GUID_STRING = "c0fcd4a1-9c29-11b1-9dad-c379636f7270";
  private static final String TARGET_GUID_STRING = "c10afaed-9c29-11b1-9dad-c379636f7270";
  private static final String TIMEPOINT_GUID_STRING = "bd58ca05-9c29-11b1-9dad-c379636f7270";
  private static final String TREATY_OAK_GUID_STRING = "bfc0aa80-9c29-11b1-9dad-c379636f7270";
  private static final String UNIVERSE_DATA_MT_GUID_STRING = "bd58d0f3-9c29-11b1-9dad-c379636f7270";
  private static final String VEGETABLE_MATTER_GUID_STRING = "bd58c455-9c29-11b1-9dad-c379636f7270";
  private static final String WOLF_GUID_STRING = "bd58c31f-9c29-11b1-9dad-c379636f7270";
  private static final String WORLD_GEOGRAPHY_MT_GUID_STRING = "bfaac020-9c29-11b1-9dad-c379636f7270";

  /**
   * Creates a <tt>UnitTest</tt> object with the given name.
   * 
   * @param name the given unit test name
   */
  public UnitTest(String name) {
    super(name);
  }

  /**
   * Returns the test suite.
   * 
   * @return the test suite
   */
  public static Test suite() {
    try {
      endpointURL = new URL(endpointURLString);
    } catch (MalformedURLException e) {
    }

    TestSuite testSuite = new TestSuite();
    testSuite.addTest(new UnitTest("testMakeValidConstantName"));
    testSuite.addTest(new UnitTest("testCycAccessInitialization"));
    testSuite.addTest(new UnitTest("testBinaryCycConnection1"));
    testSuite.addTest(new UnitTest("testBinaryCycConnection2"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess1"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess2"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess3"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess4"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess5"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess6"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess7"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess8"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess9"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess10"));
    /*    testSuite.addTest(new UnitTest("testBinaryCycAccess11")); */
    testSuite.addTest(new UnitTest("testBinaryCycAccess12"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess13"));
    testSuite.addTest(new UnitTest("testGetGafs"));
    testSuite.addTest(new UnitTest("testGetCycImage"));
    testSuite.addTest(new UnitTest("testGetELCycTerm"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess14"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess15"));
    testSuite.addTest(new UnitTest("testBinaryCycAccess16"));
    testSuite.addTest(new UnitTest("testAssertWithTranscriptAndBookkeeping"));
    testSuite.addTest(new UnitTest("testBigCycList"));
    testSuite.addTest(new UnitTest("testGetArg2"));
    testSuite.addTest(new UnitTest("testUnicodeCFASL"));
    testSuite.addTest(new UnitTest("testHLIDGeneration"));
    testSuite.addTest(new UnitTest("testHLIDRoundTripConversion"));
    testSuite.addTest(new UnitTest("testCycLeaseManager"));
    testSuite.addTest(new UnitTest("testInferenceProblemStoreReuse"));
    testSuite.addTest(new UnitTest("testInvalidTerms"));
    testSuite.addTest(new UnitTest("testCycSymbolLocaleIndependence"));
    return testSuite;
  }

  /**
   * Compares expected object to the test object without causing a unit test failure, reporting if
   * the parameters are not equal.
   * 
   * @param expectedObject the expected object
   * @param testObject the test object
   */
  public static void nofailAssertEquals(Object expectedObject,
          Object testObject) {
    if (!expectedObject.equals(testObject)) {
      System.out.println("Expected <" + expectedObject + "> \nfound <" + testObject);
    }
  }

  /**
   * Reports if the given boolen expression is false, without causing a unit test failure.
   * 
   * @param testExpression the test expression
   * @param message the message to display when the test fails
   */
  public static void nofailAssertTrue(boolean testExpression,
          String message) {
    if (!testExpression) {
      System.out.println("Test expression not true\n" + message);
    }
  }

  /**
   * Tests the makeValidConstantName method.
   */
  public void testMakeValidConstantName() {
    System.out.println("\n**** testMakeValidConstantName ****");

    String candidateName = "abc";
    assertEquals(candidateName, CycConstant.makeValidConstantName(candidateName));
    candidateName = "()[]//abc";

    String expectedValidName = "______abc";
    assertEquals(expectedValidName, CycConstant.makeValidConstantName(candidateName));
    System.out.println("**** testMakeValidConstantName OK ****");
  }

  /**
   * Tests CycAccess initialization.
   */
  public void testCycAccessInitialization() {
    System.out.println("\n**** testCycAccessInitialization ****");

    CycAccess cycAccess = null;

    System.out.println("creating CycAccess 1");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 1 closed, creating CycAccess 2");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 2 closed, creating CycAccess 3");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 3 closed, creating CycAccess 4");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 4 closed, creating CycAccess 5");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 5 closed, creating CycAccess 6");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("CycAccess 6 closed, creating CycAccess 7");
    try {
      if (connectionMode == LOCAL_CYC_CONNECTION) {
        cycAccess = new CycAccess(testHostName, testBasePort);
        System.out.println(cycAccess.getCycConnection().connectionInfo());
      } else if (connectionMode == SOAP_CYC_CONNECTION) {
        cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
      } else {
        fail("Invalid connection mode " + connectionMode);
      }
    } catch (ConnectException e) {
      System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
      fail(e.toString());
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("CycAccess 7 closed");

    System.out.println("**** testCycAccessInitialization OK ****");
  }

  /**
   * Tests the fundamental aspects of the binary (cfasl) api connection to the OpenCyc server.
   */
  public void testBinaryCycConnection1() {
    System.out.println("\n**** testBinaryCycConnection1 ****");

    CycAccess cycAccess = null;
    CycConnectionInterface cycConnection = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }

        cycConnection = cycAccess.cycConnection;

        //cycConnection.trace = true;
      } catch (ConnectException e) {
        System.out.println("Could not connect to host " + testHostName + " base port " + testBasePort);
        fail(e.toString());
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      //cycAccess.traceOn();
      // Test return of atom.
      CycList command = new CycList();
      command.add(makeCycSymbol("+"));
      command.add(2);
      command.add(3);

      Object[] response = {0, ""};

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals(5, response[1]);


      // Test return of string.
      command = new CycList();
      command.add(quote);
      command.add("abc");

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("abc", response[1]);


      // Test return of symbolic expression.
      command = new CycList();
      command.add(quote);

      CycList cycList2 = CycList.makeCycList(makeCycSymbol("a"), makeCycSymbol("b"));
      command.add(cycList2);

      CycList cycList3 = new CycList();
      cycList2.add(cycList3);
      cycList3.add(makeCycSymbol("c"));
      cycList3.add(makeCycSymbol("d"));

      CycList cycList4 = new CycList();
      cycList3.add(cycList4);
      cycList4.add(makeCycSymbol("e"));
      cycList3.add(makeCycSymbol("f"));
      cycList3.add(makeCycSymbol("?my-var"));

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("(A B (C D (E) F ?MY-VAR))", response[1].toString());


      // Test return of improper list.
      command = new CycList();
      command.add(quote);
      cycList2 = new CycList();
      command.add(cycList2);
      cycList2.add(makeCycSymbol("A"));
      cycList2.setDottedElement(makeCycSymbol("B"));

      try {
//      cycAccess.traceOn();
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("(A . B)", response[1].toString());


      // Test error return
      command = new CycList();
      command.add(nil);

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }
      assertTrue(response[1].toString().indexOf("NIL") > -1);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycConnection1 OK ****");
  }

  /**
   * Tests the fundamental aspects of the binary (cfasl) api connection to the OpenCyc server.
   * CycAccess is set to null;
   */
  public void testBinaryCycConnection2() {
    if ((connectionMode == SOAP_CYC_CONNECTION)) {
      return;
    }

    System.out.println("\n**** testBinaryCycConnection2 ****");

    CycConnectionInterface cycConnection = null;
    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode + "\n bailing on test.");

          return;
        }

        cycConnection = cycAccess.cycConnection;
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // Test return of atom.
      CycList command = new CycList();
      command.add(makeCycSymbol("+"));
      command.add(2);
      command.add(3);

      Object[] response = {0, ""};

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals(5, response[1]);


      // Test return of string.
      command = new CycList();
      command.add(quote);
      command.add("abc");

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("abc", response[1]);


      // Test return of symbolic expression.
      command = new CycList();
      command.add(quote);

      CycList cycList2 = new CycList();
      command.add(cycList2);
      cycList2.add(makeCycSymbol("a"));
      cycList2.add(makeCycSymbol("b"));

      CycList cycList3 = new CycList();
      cycList2.add(cycList3);
      cycList3.add(makeCycSymbol("c"));
      cycList3.add(makeCycSymbol("d"));

      CycList cycList4 = new CycList();
      cycList3.add(cycList4);
      cycList4.add(makeCycSymbol("e"));
      cycList3.add(makeCycSymbol("f"));

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("(A B (C D (E) F))", response[1].toString());


      // Test return of improper list.
      command = new CycList();
      command.add(quote);
      cycList2 = new CycList();
      command.add(cycList2);
      cycList2.add(makeCycSymbol("A"));
      cycList2.setDottedElement(makeCycSymbol("B"));

      try {
        //cycConnection.trace = true;
        response = cycConnection.converse(command);

        //cycConnection.trace = false;
      } catch (Throwable e) {
        fail(e.toString());
      }

      assertEquals(true, response[0]);
      assertEquals("(A . B)", response[1].toString());


      // Test error return
      command = new CycList();
      command.add(nil);

      try {
        response = cycConnection.converse(command);
      } catch (Throwable e) {
        fail(e.toString());
      }

      if (response[1].toString().indexOf("NIL") == -1) {
        System.out.println(response[1]);
      }


      // various error messages to effect that NIL is not defined in the API.
      assertTrue(response[1].toString().indexOf("NIL") > -1);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycConnection2 OK ****");
  }

  public void testCycSymbolLocaleIndependence() {
    System.out.println("\n**** testCycSymbolLocaleIndependence ****");
    final Locale defaultLocale = Locale.getDefault();
    final String lowercaseName = "abcdefghijklmnopqrstuvwxyz1234567890-_";
    Locale.setDefault(Locale.ENGLISH);
    final CycSymbol englishSymbol = makeCycSymbol(lowercaseName);
    Locale.setDefault(new Locale("tr"));
    final CycSymbol turkishSymbol = makeCycSymbol(lowercaseName);
    Locale.setDefault(defaultLocale);
    assertEquals(englishSymbol.toString(), turkishSymbol.toString());
    System.out.println("**** testCycSymbolLocaleIndependence OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess1() {
    System.out.println("\n**** testBinaryCycAccess 1 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        System.out.println("\nException: " + e.getMessage());
        fail(e.toString());
      }


      //cycAccess.traceOn();
      doTestCycAccess1(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 1 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess1(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    resetCycConstantCaches();

    // getConstantByName.
    CycConstant cycConstant = null;

    try {
      //cycAccess.traceOnDetailed();
      cycConstant = cycAccess.getConstantByName("#$Dog");
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(cycConstant);
    assertEquals(DOG_GUID_STRING, cycConstant.getGuid().toString());

    // getConstantByGuid.
    try {
      cycConstant = cycAccess.getConstantByGuid(makeGuid(DOG_GUID_STRING));
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(cycConstant);
    assertEquals("#$Dog", cycConstant.cyclify());
    assertEquals("Dog", cycConstant.getName());


    // getComment.
    String comment = null;

    try {
      CycConstant raindrop = cycAccess.getKnownConstantByGuid(RAINDROP_GUID_STRING);
      comment = cycAccess.getComment(raindrop);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(comment);
    assertEquals("The collection of drops of liquid water emitted by clouds in instances of #$RainProcess.",
            comment);

    // getIsas.
    List isas = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      isas = cycAccess.getIsas(dog);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(isas);
    assertTrue(isas instanceof CycList);
    isas = ((CycList) isas).sort();

    try {
      CycConstant biologicalSpecies = cycAccess.getKnownConstantByGuid(BIOLIGICAL_SPECIES_GUID_STRING);
      assertTrue(isas.contains(biologicalSpecies));
    } catch (Throwable e) {
      fail(e.toString());
    }


    isas = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      isas = cycAccess.getIsas(dog);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(isas);
    assertTrue(isas instanceof CycList);
    isas = ((CycList) isas).sort();

    try {
      CycConstant biologicalSpecies = cycAccess.getKnownConstantByGuid(BIOLIGICAL_SPECIES_GUID_STRING);
      assertTrue(isas.contains(biologicalSpecies));
    } catch (Throwable e) {
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess2() {
    System.out.println("\n**** testBinaryCycAccess 2 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }


//    cycAccess.traceOnDetailed();
//    cycAccess.traceNamesOn();
      doTestCycAccess2(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 2 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess2(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();
    System.out.println(cycAccess.getCycConnection().connectionInfo());
    resetCycConstantCaches();

    // getGenls.
    List genls = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      genls = cycAccess.getGenls(dog);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(genls);
    assertTrue(genls instanceof CycList);
    genls = ((CycList) genls).sort();
    assertTrue(genls.toString().indexOf("CanisGenus") > -1);
    assertTrue(genls.toString().indexOf("DomesticatedAnimal") > -1);

    // getGenlPreds.
    List genlPreds = null;

    try {
      CycConstant target = cycAccess.getKnownConstantByGuid(TARGET_GUID_STRING);
      genlPreds = cycAccess.getGenlPreds(target);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(genlPreds);
    assertTrue((genlPreds.toString().equals("(preActors)")) || (genlPreds.toString().equals("(actors)")));

    // getAllGenlPreds.
    List allGenlPreds = null;

    try {
      CycConstant target = cycAccess.getKnownConstantByGuid(TARGET_GUID_STRING);
      allGenlPreds = cycAccess.getAllGenlPreds(target);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(allGenlPreds);
    assertTrue(allGenlPreds.size() > 2);

    // getArg1Formats.
    List arg1Formats = null;

    try {
      CycConstant target = cycAccess.getKnownConstantByGuid(TARGET_GUID_STRING);
      arg1Formats = cycAccess.getArg1Formats(target);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(arg1Formats);
    assertEquals("(SetTheFormat)", arg1Formats.toString());


    // getArg1Formats.
    arg1Formats = null;

    try {
      CycConstant constantName = cycAccess.getKnownConstantByGuid(CONSTANT_NAME_GUID_STRING);
      arg1Formats = cycAccess.getArg1Formats(constantName);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(arg1Formats);
    assertEquals("(singleEntryFormatInArgs)", arg1Formats.toString());

    // getArg2Formats.
    List arg2Formats = null;

    try {
      CycConstant internalParts = cycAccess.getKnownConstantByGuid(INTERNAL_PARTS_GUID_STRING);
      arg2Formats = cycAccess.getArg2Formats(internalParts);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(arg2Formats);
    assertEquals("(SetTheFormat)", arg2Formats.toString());

    // getDisjointWiths.
    List disjointWiths = null;

    try {
      CycConstant vegetableMatter = cycAccess.getKnownConstantByGuid(VEGETABLE_MATTER_GUID_STRING);
      disjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(disjointWiths);
    assertTrue(disjointWiths.toString().indexOf(
            "AnimalBLO") > 0);

    // getCoExtensionals.
    List coExtensionals = null;

    try {
      CycConstant cycLTerm = cycAccess.getKnownConstantByGuid(CYCL_TERM_GUID_STRING);
      coExtensionals = cycAccess.getCoExtensionals(cycLTerm);
      if (!cycAccess.isOpenCyc()) {
        assertNotNull(coExtensionals);
        assertEquals("(CycLExpression)",
                coExtensionals.toString());
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }


    // getCoExtensionals.
    coExtensionals = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      coExtensionals = cycAccess.getCoExtensionals(dog);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(coExtensionals);
    assertEquals("()", coExtensionals.toString());

    // getArg1Isas.
    List arg1Isas = null;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      arg1Isas = cycAccess.getArg1Isas(doneBy);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(arg1Isas);
    assertEquals("(Event)", arg1Isas.toString());

    // getArg2Isas.
    List arg2Isas = null;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      arg2Isas = cycAccess.getArg2Isas(doneBy);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(arg2Isas);
    assertEquals("(SomethingExisting)", arg2Isas.toString());

    // getArgNIsas.
    List argNIsas = null;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      argNIsas = cycAccess.getArgNIsas(doneBy, 1);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(argNIsas);
    assertEquals("(Event)", argNIsas.toString());

    // getArgNGenls.
    List argGenls = null;

    try {
      CycConstant superTaxons = cycAccess.getKnownConstantByGuid(SUPERTAXONS_GUID_STRING);
      argGenls = cycAccess.getArgNGenls(superTaxons, 2);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(argGenls);
    assertEquals("(Organism-Whole)", argGenls.toString());

    // isCollection.
    boolean answer = false;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      answer = cycAccess.isCollection(dog);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    // isCollection.
    answer = true;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      answer = cycAccess.isCollection(doneBy);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);


    // isCollection on a NAUT
    answer = false;

    try {
      CycConstant fruitFn = cycAccess.getKnownConstantByGuid(FRUIT_FN_GUID_STRING);
      CycConstant appleTree = cycAccess.getKnownConstantByGuid(APPLE_TREE_GUID_STRING);
      CycNaut fruitFnAppleTreeNaut = new CycNaut(fruitFn, appleTree);
      answer = cycAccess.isCollection(fruitFnAppleTreeNaut);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    // isCollection on a NAUT
    answer = true;

    try {
      CycConstant cityNamedFn = cycAccess.getKnownConstantByGuid(CITY_NAMED_FN_GUID_STRING);
      CycConstant swaziland = cycAccess.getKnownConstantByGuid(SWAZILAND_GUID_STRING);
      CycNaut cityNamedFnNaut = new CycNaut(cityNamedFn, "swaziville", swaziland);
      answer = cycAccess.isCollection(cityNamedFnNaut);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);


    // isCollection on a non-CycObject
    answer = true;

    try {
      answer = cycAccess.isCollection(7);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);


    // isBinaryPredicate.
    answer = false;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      answer = cycAccess.isBinaryPredicate(doneBy);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    // isBinaryPredicate.
    answer = true;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      answer = cycAccess.isBinaryPredicate(dog);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);

    // getGeneratedPhrase.
    String phrase = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      phrase = cycAccess.getGeneratedPhrase(dog);
      assertNotNull(phrase);
      if (cycAccess.isOpenCyc()) {
        assertEquals("dog", phrase);
      } else {
        assertEquals("Canis familiaris", phrase);
      }
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getSingularGeneratedPhrase.
    phrase = null;

    try {
      CycConstant brazil = cycAccess.getKnownConstantByGuid(BRAZIL_GUID_STRING);
      phrase = cycAccess.getGeneratedPhrase(brazil);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(phrase);
    assertTrue(phrase.toLowerCase().indexOf("bra") > -1);


    // getGeneratedPhrase.
    phrase = null;

    try {
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      phrase = cycAccess.getGeneratedPhrase(doneBy);
      assertNotNull(phrase);
      assertTrue(phrase.indexOf("doer") > -1);
    } catch (Throwable e) {
      fail(e.toString());
    }

    // denots-of-string


    try {
      String denotationString = "Brazil";
      CycList denotations = cycAccess.getDenotsOfString(denotationString);
      System.out.println(denotations.cyclify());
      assertTrue(denotations.contains(cycAccess.getKnownConstantByGuid("bd588f01-9c29-11b1-9dad-c379636f7270")));
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }
    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess3() {
    System.out.println("\n**** testBinaryCycAccess 3 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
          assert cycAccess.getCycLeaseManager() != null : "CycLeaseManager must not be null";
          cycAccess.getCycLeaseManager().addListener(this);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      doTestCycAccess3(cycAccess);

      cycAccess.getCycLeaseManager().removeListener(this);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 3 OK ****");
  }

  /** Notifies the listener of the given Cyc API services lease event.
   *
   * @param evt the the given Cyc API services lease event
   */
  public void notifyCycLeaseEvent(org.opencyc.api.CycLeaseManager.CycLeaseEventObject evt) {
    System.out.println("Notified of: " + evt.toString());
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess3(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();
    resetCycConstantCaches();

    // getComment.
    String comment = null;

    try {
      CycConstant brazil = cycAccess.getKnownConstantByGuid(BRAZIL_GUID_STRING);
      comment = cycAccess.getComment(brazil);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(comment);
    assertEquals("An instance of #$IndependentCountry.  #$Brazil is the "
            + "largest country in South America, and is bounded on the "
            + "northwest by #$Colombia; on the north by #$Venezuela, "
            + "#$Guyana, #$Suriname, and #$FrenchGuiana; on the east by "
            + "the #$AtlanticOcean; on the south by #$Uruguay; on the "
            + "southwest by #$Argentina and #$Paraguay; and on the west "
            + "by #$Bolivia and #$Peru.",
            comment);

    // getIsas.
    List isas = null;

    try {
      CycConstant brazil = cycAccess.getKnownConstantByGuid(BRAZIL_GUID_STRING);
      isas = cycAccess.getIsas(brazil);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(isas);
    assertTrue(isas instanceof CycList);
    assertTrue(isas.toString().indexOf("IndependentCountry") > 0);
    isas = ((CycList) isas).sort();
    assertTrue(isas.toString().indexOf("IndependentCountry") > 0);

    // getGenls.
    List genls = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      genls = cycAccess.getGenls(dog);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(genls);
    assertTrue(genls instanceof CycList);
    genls = ((CycList) genls).sort();
    assertTrue(genls.toString().indexOf("CanisGenus") > -1);
    assertTrue(genls.toString().indexOf("DomesticatedAnimal") > -1);

    // getMinGenls.
    List minGenls = null;

    try {
      CycConstant lion = cycAccess.getKnownConstantByGuid(LION_GUID_STRING);
      minGenls = cycAccess.getMinGenls(lion);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(minGenls);
    assertTrue(minGenls instanceof CycList);
    minGenls = ((CycList) minGenls).sort();
    assertEquals("(FelidaeFamily)", minGenls.toString());


    // getMinGenls mt.
    minGenls = null;

    try {
      CycConstant lion = cycAccess.getKnownConstantByGuid(LION_GUID_STRING);


      // #$BiologyVocabularyMt
      minGenls = cycAccess.getMinGenls(lion,
              cycAccess.getKnownConstantByGuid(BIOLOGY_VOCABULARY_MT_GUID_STRING));
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(minGenls);
    assertTrue(minGenls instanceof CycList);
    minGenls = ((CycList) minGenls).sort();
    assertEquals("(FelidaeFamily)", minGenls.toString());

    // getSpecs.
    List specs = null;

    try {
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      specs = cycAccess.getSpecs(canineAnimal);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(specs);
    assertTrue(specs instanceof CycList);
    final String specsString = specs.toString();
    for (final String name : Arrays.asList("CanisGenus", "Coyote-Animal", "Dog", "Fox", "Jackal")) {
      assertTrue(specsString.indexOf(name) > 0);
    }

    // getMaxSpecs.
    List maxSpecs = null;

    try {
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      maxSpecs = cycAccess.getMaxSpecs(canineAnimal);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(maxSpecs);
    assertTrue(maxSpecs instanceof CycList);
    maxSpecs = ((CycList) maxSpecs).sort();
    assertTrue(maxSpecs.toString().indexOf("CanisGenus") > 0);
    assertTrue(maxSpecs.toString().indexOf("Fox") > 0);

    // getGenlSiblings.
    List genlSiblings = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      genlSiblings = cycAccess.getGenlSiblings(dog);
      assertNotNull(genlSiblings);
      assertTrue(genlSiblings instanceof CycList);
      genlSiblings = ((CycList) genlSiblings).sort();
      if (!cycAccess.isOpenCyc()) {
        assertTrue(genlSiblings.toString().indexOf("JuvenileAnimal") > -1);
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }


    /* long running.
    // getSiblings.
    List siblings = null;
    try {
    CycConstant dog = cycAccess.getKnownConstantByGuid("bd58daa0-9c29-11b1-9dad-c379636f7270");
    siblings = cycAccess.getSiblings(dog);
    assertNotNull(siblings);
    assertTrue(siblings instanceof CycList);
    CycConstant gooseDomestic = cycAccess.getKnownConstantByGuid("bd5ca864-9c29-11b1-9dad-c379636f7270");
    assertTrue(siblings.contains(gooseDomestic));
    CycConstant goatDomestic = cycAccess.getKnownConstantByGuid("bd58e278-9c29-11b1-9dad-c379636f7270");
    assertTrue(siblings.contains(goatDomestic));
    }
    catch (Throwable e) {
    e.printStackTrace();
    fail(e.toString());
    }
    // getSpecSiblings.
    List specSiblings = null;
    try {
    CycConstant dog = cycAccess.getKnownConstantByGuid("bd58daa0-9c29-11b1-9dad-c379636f7270");
    specSiblings = cycAccess.getSpecSiblings(dog);
    assertNotNull(specSiblings);
    assertTrue(specSiblings instanceof CycList);
    CycConstant gooseDomestic = cycAccess.getKnownConstantByGuid("bd5ca864-9c29-11b1-9dad-c379636f7270");
    assertTrue(specSiblings.contains(gooseDomestic));
    CycConstant goatDomestic = cycAccess.getKnownConstantByGuid("bd58e278-9c29-11b1-9dad-c379636f7270");
    assertTrue(specSiblings.contains(goatDomestic));
    }
    catch (Throwable e) {
    fail(e.toString());
    }
     */

    // getAllGenls.
    List allGenls = null;

    try {
      CycConstant existingObjectType = cycAccess.getKnownConstantByGuid(EXISTING_OBJECT_TYPE_GUID_STRING);
      allGenls = cycAccess.getAllGenls(existingObjectType);
      assertNotNull(allGenls);
      assertTrue(allGenls instanceof CycList);

      CycConstant objectType = cycAccess.getKnownConstantByGuid(OBJECT_TYPE_GUID_STRING);

      assertTrue(allGenls.contains(objectType));
      assertTrue(allGenls.contains(thing));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getAllSpecs.
    List allSpecs = null;

    try {
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      allSpecs = cycAccess.getAllSpecs(canineAnimal);
      assertNotNull(allSpecs);
      assertTrue(allSpecs instanceof CycList);

      CycConstant jackal = cycAccess.getKnownConstantByGuid(JACKAL_GUID_STRING);
      assertTrue(allSpecs.contains(jackal));

      CycConstant retrieverDog = cycAccess.getKnownConstantByGuid(RETRIEVER_DOG_GUID_STRING);
      assertTrue(allSpecs.contains(retrieverDog));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getAllGenlsWrt.
    List allGenlsWrt = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      allGenlsWrt = cycAccess.getAllGenlsWrt(dog, animal);
      assertNotNull(allGenlsWrt);
      assertTrue(allGenlsWrt instanceof CycList);

      CycConstant tameAnimal = cycAccess.getKnownConstantByGuid(TAME_ANIMAL_GUID_STRING);
      assertTrue(allGenlsWrt.contains(tameAnimal));

      CycConstant airBreathingVertebrate = cycAccess.getKnownConstantByGuid(
              AIR_BREATHING_VERTEBRATE_GUID_STRING);
      assertTrue(allGenlsWrt.contains(airBreathingVertebrate));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getAllDependentSpecs.
    List allDependentSpecs = null;

    try {
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      allDependentSpecs = cycAccess.getAllDependentSpecs(canineAnimal);
      assertNotNull(allDependentSpecs);

      CycConstant fox = cycAccess.getKnownConstantByGuid(FOX_GUID_STRING);
      assertTrue(allDependentSpecs instanceof CycList);
      assertTrue(allDependentSpecs.contains(fox));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getSampleLeafSpecs.
    List sampleLeafSpecs = null;

    try {
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      sampleLeafSpecs = cycAccess.getSampleLeafSpecs(canineAnimal, 3);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(sampleLeafSpecs);
    assertTrue(sampleLeafSpecs instanceof CycList);


    //System.out.println("sampleLeafSpecs: " + sampleLeafSpecsArrayList);
    assertTrue(sampleLeafSpecs.size() > 0);

    // isSpecOf.
    boolean answer = true;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      answer = cycAccess.isSpecOf(dog, animal);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    // isGenlOf.
    answer = true;

    try {
      CycConstant wolf = cycAccess.getKnownConstantByGuid(WOLF_GUID_STRING);
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(CANINE_ANIMAL_GUID_STRING);
      answer = cycAccess.isGenlOf(canineAnimal, wolf);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    // areTacitCoextensional.
    answer = true;

    try {
      CycConstant singlePurposeDevice = cycAccess.getKnownConstantByGuid(
              SINGLE_PURPOSE_DEVICE_GUID_STRING);
      CycConstant physicalDevice = cycAccess.getKnownConstantByGuid(
              PHYSICAL_DEVICE_GUID_STRING);


      //cycAccess.traceOn();
      answer = cycAccess.areTacitCoextensional(singlePurposeDevice, physicalDevice);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);


    // areAssertedCoextensional.
    answer = true;

    try {
      CycConstant singlePurposeDevice = cycAccess.getKnownConstantByGuid(
              SINGLE_PURPOSE_DEVICE_GUID_STRING);
      CycConstant physicalDevice = cycAccess.getKnownConstantByGuid(
              PHYSICAL_DEVICE_GUID_STRING);
      answer = cycAccess.areAssertedCoextensional(singlePurposeDevice, physicalDevice);
      if (!cycAccess.isOpenCyc()) {
        assertTrue(answer);
      }
    } catch (Throwable e) {
      fail(e.toString());
    }

    // areIntersecting.
    answer = true;

    //cycAccess.traceOn();
    try {
      CycConstant domesticatedAnimal = cycAccess.getKnownConstantByGuid(
              DOMESTICATED_ANIMAL_GUID_STRING);
      CycConstant tameAnimal = cycAccess.getKnownConstantByGuid(
              TAME_ANIMAL_GUID_STRING);
      answer = cycAccess.areIntersecting(domesticatedAnimal, tameAnimal);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);


    //cycAccess.traceOff();
    // areHierarchical.
    answer = true;

    try {
      CycConstant wolf = cycAccess.getKnownConstantByGuid(WOLF_GUID_STRING);
      CycConstant canineAnimal = cycAccess.getKnownConstantByGuid(
              CANINE_ANIMAL_GUID_STRING);
      answer = cycAccess.areHierarchical(canineAnimal, wolf);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);

  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess4() {
    System.out.println("\n**** testBinaryCycAccess 4 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }


      //cycAccess.traceOnDetailed();
      doTestCycAccess4(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 4 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess4(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();
    resetCycConstantCaches();

    // getCollectionLeaves.
    List collectionLeaves = null;

    try {
      //cycAccess.traceOnDetailed();
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      collectionLeaves = cycAccess.getCollectionLeaves(animal);
      assertNotNull(collectionLeaves);
      assertTrue(collectionLeaves instanceof CycList);
      //cycAccess.traceOff();
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getWhyGenl.
    CycList whyGenl = null;

    try {
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      whyGenl = cycAccess.getWhyGenl(dog, animal);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(whyGenl);
    System.out.println("whyGenl " + whyGenl);

    /*
    CycSymbol whyGenlFirst = (CycSymbol) ((CycList) ((CycList) whyGenl.first()).first()).second();
    CycSymbol whyGenlLast = (CycSymbol) ((CycList) ((CycList) whyGenl.last()).first()).third();
    try {
    CycConstant dog = cycAccess.getKnownConstantByGuid("bd58daa0-9c29-11b1-9dad-c379636f7270");
    assertEquals(dog, whyGenlFirst);
    CycConstant animal = cycAccess.getKnownConstantByGuid("bd58b031-9c29-11b1-9dad-c379636f7270");
    assertEquals(animal, whyGenlLast);
    }
    catch (Throwable e) {
    fail(e.toString());
    }
     */

    // getWhyCollectionsIntersect.
    List whyCollectionsIntersect = null;

    try {
      CycConstant domesticatedAnimal = cycAccess.getKnownConstantByGuid(
              DOMESTICATED_ANIMAL_GUID_STRING);
      CycConstant nonPersonAnimal = cycAccess.getKnownConstantByGuid(
              NON_PERSON_ANIMAL_GUID_STRING);
      whyCollectionsIntersect = cycAccess.getWhyCollectionsIntersect(domesticatedAnimal, nonPersonAnimal);
      assertNotNull(whyCollectionsIntersect);
      assertTrue(whyCollectionsIntersect instanceof CycList);
      System.out.println("whyCollectionsIntersect " + whyCollectionsIntersect);

      CycList expectedWhyCollectionsIntersect = cycAccess.makeCycList(
              "(((#$genls #$DomesticatedAnimal #$TameAnimal) :TRUE) "
              + "((#$genls #$TameAnimal #$NonPersonAnimal) :TRUE))");

      /**
       * assertEquals(expectedWhyCollectionsIntersect.toString(),
       * whyCollectionsIntersect.toString()); assertEquals(expectedWhyCollectionsIntersect,
       * whyCollectionsIntersect);
       */
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getLocalDisjointWith.
    List localDisjointWiths = null;

    try {
      CycConstant vegetableMatter = cycAccess.getKnownConstantByGuid(
              VEGETABLE_MATTER_GUID_STRING);
      localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
      assertNotNull(localDisjointWiths);
      assertTrue(localDisjointWiths.toString().indexOf("AnimalBLO") > 0);
    } catch (Throwable e) {
      fail(e.toString());
    }

    // areDisjoint.
    boolean answer = true;

    try {
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      CycConstant plant = cycAccess.getKnownConstantByGuid(PLANT_GUID_STRING);
      answer = cycAccess.areDisjoint(animal, plant);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);

    // getMinIsas.
    List minIsas = null;

    try {
      CycConstant wolf = cycAccess.getKnownConstantByGuid(WOLF_GUID_STRING);
      minIsas = cycAccess.getMinIsas(wolf);

      CycConstant organismClassificationType = cycAccess.getKnownConstantByGuid(
              ORGANISM_CLASSIFICATION_TYPE_GUID_STRING);
      assertTrue(minIsas.contains(organismClassificationType));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getInstances.
    List instances = null;

    try {
      CycConstant maleHuman = cycAccess.getKnownConstantByGuid(MALE_HUMAN_GUID_STRING);
      instances = cycAccess.getInstances(maleHuman);
      assertTrue(instances instanceof CycList);

      CycConstant plato = cycAccess.getKnownConstantByGuid(PLATO_GUID_STRING);
      assertTrue(((CycList) instances).contains(plato));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getAllIsa.
    List allIsas = null;

    try {
      //cycAccess.traceOn();
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      allIsas = cycAccess.getAllIsa(animal);

      //System.out.println(allIsas);
      CycConstant organismClassificationType = cycAccess.getKnownConstantByGuid(
              ORGANISM_CLASSIFICATION_TYPE_GUID_STRING);
      assertTrue(allIsas.contains(organismClassificationType));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // getAllInstances.
    List allPlants = null;

    try {
      if (!cycAccess.isOpenCyc()) {
        CycConstant plant = cycAccess.getKnownConstantByGuid(PLANT_GUID_STRING);
        allPlants = cycAccess.getAllInstances(plant);

        CycConstant treatyOak = cycAccess.getKnownConstantByGuid(TREATY_OAK_GUID_STRING);
        assertTrue(allPlants.contains(treatyOak));

        CycConstant burningBushOldTestament = cycAccess.getKnownConstantByGuid(
                BURNING_BUSH_GUID_STRING);
        assertTrue(allPlants.contains(burningBushOldTestament));
      }
    } catch (Throwable e) {
      fail(e.toString());
    }


    // isa.
    answer = true;

    try {
      if (!cycAccess.isOpenCyc()) {
        CycConstant plant = cycAccess.getKnownConstantByGuid(PLANT_GUID_STRING);
        CycConstant treatyOak = cycAccess.getKnownConstantByGuid(TREATY_OAK_GUID_STRING);
        answer = cycAccess.isa(treatyOak, plant);
        assertTrue(answer);

        final CycConstant term1 = cycAccess.getKnownConstantByName("NthSubSituationTypeOfTypeFn");
        final CycConstant term2 = cycAccess.getKnownConstantByName("PreparingFoodItemFn");
        final CycConstant term3 = cycAccess.getKnownConstantByName("SpaghettiMarinara");
        final CycConstant term4 = cycAccess.getKnownConstantByName("FluidFlow-Complete");
        final CycConstant collection = cycAccess.getKnownConstantByName("Collection");
        final CycConstant mt = cycAccess.getKnownConstantByName("HumanActivitiesMt");
        final CycNart nart1 = new CycNart(term2, term3);
        final CycList nartList = new CycList();
        nartList.add(term1);
        nartList.add(nart1);
        nartList.add(term4);
        nartList.add(2);
        final CycNart nart2 = new CycNart(nartList);

        //(ISA? (QUOTE (NthSubSituationTypeOfTypeFn (PreparingFoodItemFn SpaghettiMarinara) FluidFlow-Complete 2)) Collection HumanActivitiesMt)
        answer = cycAccess.isa(nart2, collection, mt);
        assertTrue(answer);
      }
    } catch (Throwable e) {
      fail(e.toString());
    }


    // getWhyCollectionsIntersectParaphrase.
    ArrayList whyCollectionsIntersectParaphrase = null;

    try {
      //cycAccess.traceOn();
      CycConstant domesticatedAnimal = cycAccess.getKnownConstantByGuid(
              DOMESTICATED_ANIMAL_GUID_STRING);
      CycConstant nonPersonAnimal = cycAccess.getKnownConstantByGuid(
              NON_PERSON_ANIMAL_GUID_STRING);
      System.out.println("bypassing getWhyCollectionsIntersectParaphrase");

      /*
      whyCollectionsIntersectParaphrase =
      cycAccess.getWhyCollectionsIntersectParaphrase(domesticatedAnimal, nonPersonAnimal);
       */
    } catch (Throwable e) {
      fail(e.toString());
    }

    /*
    assertNotNull(whyCollectionsIntersectParaphrase);
    String oneExpectedCollectionsIntersectParaphrase =
    "every domesticated animal (tame animal) is a tame animal";
    //System.out.println(whyCollectionsIntersectParaphrase);
    assertTrue(whyCollectionsIntersectParaphrase.contains(oneExpectedCollectionsIntersectParaphrase));
     */

    // getWhyGenlParaphrase.
    ArrayList whyGenlParaphrase = null;

    try {
      //cycAccess.traceOn();
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      System.out.println("bypassing getWhyGenlParaphrase");

      /*
      whyGenlParaphrase = cycAccess.getWhyGenlParaphrase(dog, animal);
       */
    } catch (Throwable e) {
      fail(e.toString());
    }

    /*
    assertNotNull(whyGenlParaphrase);
    String oneExpectedGenlParaphrase =
    "every tame animal is a non-human animal";
    
    //for (int i = 0; i < whyGenlParaphrase.size(); i++) {
    //    System.out.println(whyGenlParaphrase.get(i));
    //}
    
    assertTrue(whyGenlParaphrase.contains(oneExpectedGenlParaphrase));
     */
    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess5() {
    System.out.println("\n**** testBinaryCycAccess 5 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }


      //cycAccess.traceOnDetailed();
      doTestCycAccess5(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 5 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess5(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();
    resetCycConstantCaches();

    //cycAccess.traceOn();
    // createNewPermanent.
    CycConstant cycConstant = null;
    //cycAccess.traceNamesOn();

    try {
      cycAccess.setCyclist("#$CycAdministrator");
      cycAccess.setKePurpose("#$GeneralCycKE");
      cycConstant = cycAccess.createNewPermanent("CycAccessTestConstant");
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(cycConstant);
    assertEquals("CycAccessTestConstant", cycConstant.getName());

    // kill.
    try {
      cycAccess.kill(cycConstant);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }


    // assertComment.
    cycConstant = null;

    try {
      cycConstant = cycAccess.createNewPermanent("CycAccessTestConstant");
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    assertNotNull(cycConstant);
    assertEquals("CycAccessTestConstant", cycConstant.getName());

    CycConstant baseKb = null;

    try {
      baseKb = cycAccess.getConstantByName("BaseKB");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(baseKb);
    assertEquals("BaseKB", baseKb.getName());

    String assertedComment = "A test comment";

    try {
      cycAccess.assertComment(cycConstant, assertedComment, baseKb);
    } catch (Throwable e) {
      fail(e.toString());
    }

    String comment = null;

    try {
      comment = cycAccess.getComment(cycConstant);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertEquals(assertedComment, comment);

    try {
      cycAccess.kill(cycConstant);
    } catch (Throwable e) {
      fail(e.toString());
    }

    try {
      assertNull(cycAccess.getConstantByName("CycAccessTestConstant"));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // isValidConstantName.
    boolean answer = true;

    try {
      answer = cycAccess.isValidConstantName("abc");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);

    answer = true;

    try {
      answer = cycAccess.isValidConstantName(" abc");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);

    answer = true;

    try {
      answer = cycAccess.isValidConstantName("[abc]");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);


    // isConstantNameAvailable
    answer = true;

    try {
      answer = cycAccess.isConstantNameAvailable("Agent-PartiallyTangible");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(!answer);

    answer = false;

    try {
      answer = cycAccess.isConstantNameAvailable("myAgent");
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(answer);

    // createMicrotheory.
    CycConstant mt = null;
    ArrayList genlMts = new ArrayList();

    try {
      CycConstant modernMilitaryMt = cycAccess.getKnownConstantByGuid(
              MODERN_MILITARY_MT_GUID_STRING);
      CycConstant microtheory = cycAccess.getKnownConstantByGuid(
              MICROTHEORY_GUID_STRING);
      genlMts.add(modernMilitaryMt);
      mt = cycAccess.createMicrotheory("CycAccessTestMt",
              "a unit test comment for the CycAccessTestMt microtheory.",
              microtheory, genlMts);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertNotNull(mt);

    try {
      cycAccess.kill(mt);
    } catch (Throwable e) {
      fail(e.toString());
    }

    try {
      assertNull(cycAccess.getConstantByName("CycAccessTestMt"));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // createMicrotheorySystem.
    CycConstant[] mts = {null, null, null};
    genlMts = new ArrayList();

    try {
      genlMts.add(baseKB);
      mts = cycAccess.createMicrotheorySystem("CycAccessTest",
              "a unit test comment for the CycAccessTestMt microtheory.",
              genlMts);
    } catch (Throwable e) {
      fail(e.toString());
    }

    assertTrue(mts.length == 3);
    assertNotNull(mts[0]);
    assertEquals("#$CycAccessTestMt", mts[0].cyclify());
    assertNotNull(mts[1]);
    assertEquals("#$CycAccessTestVocabMt", mts[1].cyclify());
    assertNotNull(mts[2]);
    assertEquals("#$CycAccessTestDataMt", mts[2].cyclify());

    try {
      cycAccess.kill(mts);
    } catch (Throwable e) {
      fail(e.toString());
    }

    try {
      assertNull(cycAccess.getConstantByName("CycAccessTestMt"));
    } catch (Throwable e) {
      fail(e.toString());
    }

    // askCycQuery
    try {
      if (!cycAccess.isOpenCyc()) {
        CycFormulaSentence query = cycAccess.makeCycSentence("(#$objectFoundInLocation ?WHAT #$CityOfAustinTX)");
        mt = (CycConstant)everythingPSC;

        InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
        InferenceResultSet response = cycAccess.executeQuery(query, cycAccess.makeELMt(mt), queryProperties, 20000);
        assertNotNull(response);


        //System.out.println("query: " + query + "\n  response: " + response);
        queryProperties.setMaxNumber(4);
        queryProperties.put(makeCycSymbol(":max-time"), 30);
        queryProperties.put(makeCycSymbol(":max-transformation-depth"), 1);
        queryProperties.put(makeCycSymbol(":max-proof-depth"), 20);
        response = cycAccess.executeQuery(query, cycAccess.makeELMt(mt), queryProperties, 20000);
        //System.out.println("query: " + query + "\n  response: " + response);  
        assertTrue(response.getCurrentRowCount() > 3);
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // queryResultsToXMLString
    try {
      CycFormulaSentence query = cycAccess.makeCycSentence("(#$isa #$Person ?WHAT)");
      InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
      final CycList xmlSpec = null;
      final String xml = cycAccess.queryResultsToXMLString(query, universalVocabularyMt, queryProperties, xmlSpec);
      System.out.println("xml from queryResultsToXMLStream:\n" + xml);
      assertNotNull(xml);
      assertTrue(xml.indexOf("<what>") > -1);
      assertTrue(xml.indexOf("</what>") > -1);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }


    // askWithVariable
    try {
      if (!cycAccess.isOpenCyc()) {
        CycFormulaSentence query = cycAccess.makeCycSentence("(#$objectFoundInLocation ?WHAT #$CityOfAustinTX)");
        CycVariable variable = makeCycVariable("?WHAT");
        mt = (CycConstant)CycAccess.everythingPSC;
        InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
        CycList response = cycAccess.queryVariable(variable, query, mt, queryProperties);
        assertNotNull(response);
        assertTrue(response.contains(cycAccess.getConstantByName("#$UniversityOfTexasAtAustin")));
      }
    } catch (Throwable e) {
      fail(e.toString());
    }

    // askWithVariables
    try {
      if (!cycAccess.isOpenCyc()) {
        CycFormulaSentence query = cycAccess.makeCycSentence("(#$objectFoundInLocation ?WHAT ?WHERE)");
        CycList variables = new CycList();
        variables.add(makeCycVariable("?WHAT"));
        variables.add(makeCycVariable("?WHERE"));
        InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
        CycConstant universeDataMt = cycAccess.getKnownConstantByGuid(UNIVERSE_DATA_MT_GUID_STRING);
        CycList response = cycAccess.queryVariables(variables, query, universeDataMt, queryProperties);
        assertNotNull(response);
      }
    } catch (Throwable e) {
      fail(e.toString());
    }

    // isQueryTrue
    try {
      if (!cycAccess.isOpenCyc()) {
        //cycAccess.traceOn();
        CycFormulaSentence query = cycAccess.makeCycSentence("(#$objectFoundInLocation #$UniversityOfTexasAtAustin #$CityOfAustinTX)");
        mt = (CycConstant)everythingPSC;
        InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
        assertTrue(cycAccess.isQueryTrue(query, mt, queryProperties));
        query = cycAccess.makeCycSentence("(#$objectFoundInLocation #$UniversityOfTexasAtAustin #$CityOfHoustonTX)");
        assertTrue(!cycAccess.isQueryTrue(query, mt, queryProperties));
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // countAllInstances
    try {
      CycConstant country = cycAccess.getKnownConstantByGuid(COUNTRY_GUID_STRING);
      CycConstant worldGeographyMt = cycAccess.getKnownConstantByGuid(WORLD_GEOGRAPHY_MT_GUID_STRING);
      assertTrue(cycAccess.countAllInstances(country, worldGeographyMt) > 0);
    } catch (Throwable e) {
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess6() {
    System.out.println("\n**** testBinaryCycAccess 6 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }


      //cycAccess.traceOnDetailed();
      doTestCycAccess6(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 6 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess6(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    // Test sending a constant to Cyc.
    try {
      CycList command = new CycList();
      command.add(makeCycSymbol("identity"));
      command.add(collection);

      Object obj = cycAccess.converseObject(command);
      assertNotNull(obj);
      assertTrue(obj instanceof CycConstant);
      assertEquals(obj, collection);
    } catch (Throwable e) {
      fail(e.toString());
    }

    // Test isBackchainRequired, isBackchainEncouraged, isBackchainDiscouraged, isBackchainForbidden
    try {
      if (!cycAccess.isOpenCyc()) {
        CycConstant keRequirement = cycAccess.getKnownConstantByGuid(
                KE_REQUIREMENT_GUID_STRING);
        assertTrue(cycAccess.isBackchainRequired(keRequirement, baseKB));
        assertTrue(!cycAccess.isBackchainEncouraged(keRequirement, baseKB));
        assertTrue(!cycAccess.isBackchainDiscouraged(keRequirement, baseKB));
        assertTrue(!cycAccess.isBackchainForbidden(keRequirement, baseKB));

        CycConstant nearestIsa = cycAccess.getKnownConstantByGuid(NEAREST_ISA_GUID_STRING);
        assertTrue(!cycAccess.isBackchainRequired(nearestIsa, baseKB));
        assertTrue(!cycAccess.isBackchainEncouraged(nearestIsa, baseKB));
        assertTrue(!cycAccess.isBackchainDiscouraged(nearestIsa, baseKB));
        assertTrue(cycAccess.isBackchainForbidden(nearestIsa, baseKB));
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // isWellFormedFormula
    try {
      assertTrue(cycAccess.isWellFormedFormula(cycAccess.makeCycSentence(
              "(#$genls #$Dog #$Animal)")));


      // Not true, but still well formed.
      assertTrue(cycAccess.isWellFormedFormula(cycAccess.makeCycSentence(
              "(#$genls #$Dog #$Plant)")));
      assertTrue(cycAccess.isWellFormedFormula(cycAccess.makeCycSentence(
              "(#$genls ?X #$Animal)")));
      assertTrue(!cycAccess.isWellFormedFormula(
              cycAccess.makeCycSentence("(#$genls #$Dog #$Brazil)")));
      assertTrue(!cycAccess.isWellFormedFormula(
              cycAccess.makeCycSentence("(#$genls ?X #$Brazil)")));
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // isEvaluatablePredicate
    try {
      assertTrue(cycAccess.isEvaluatablePredicate(different));

      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      assertTrue(!cycAccess.isEvaluatablePredicate(
              doneBy));
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // hasSomePredicateUsingTerm
    try {
      if (!cycAccess.isOpenCyc()) {
        CycConstant algeria = cycAccess.getKnownConstantByGuid(
                ALGERIA_GUID_STRING);
        CycConstant percentOfRegionIs = cycAccess.getKnownConstantByGuid(
                PERCENT_OF_REGION_IS_GUID_STRING);
        CycConstant ciaWorldFactbook1995Mt = cycAccess.getKnownConstantByGuid(
                CIA_WORLD_FACTBOOK_1995_MT_GUID_STRING);

        assertTrue(cycAccess.hasSomePredicateUsingTerm(
                percentOfRegionIs,
                algeria,
                1,
                ciaWorldFactbook1995Mt));

        assertTrue(cycAccess.hasSomePredicateUsingTerm(
                percentOfRegionIs,
                algeria,
                1,
                CycAccess.inferencePSC));
        assertTrue(!cycAccess.hasSomePredicateUsingTerm(
                percentOfRegionIs,
                algeria,
                2,
                ciaWorldFactbook1995Mt));
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    // Test common constants.
    try {
      assertEquals(cycAccess.getConstantByName("and"), and);
      assertEquals(cycAccess.getConstantByName("BaseKB"), baseKB);
      assertEquals(cycAccess.getConstantByName("BinaryPredicate"), binaryPredicate);
      assertEquals(cycAccess.getConstantByName("comment"), comment);
      assertEquals(cycAccess.getConstantByName("different"), different);
      assertEquals(cycAccess.getConstantByName("elementOf"), elementOf);
      assertEquals(cycAccess.getConstantByName("genlMt"), genlMt);
      assertEquals(cycAccess.getConstantByName("genls"), genls);
      assertEquals(cycAccess.getConstantByName("isa"), isa);
      assertEquals(cycAccess.getConstantByName("numericallyEquals"), numericallyEqual);
      assertEquals(cycAccess.getConstantByName("or"), or);
      assertEquals(cycAccess.getConstantByName("PlusFn"), plusFn);

      resetCycConstantCaches();

      assertEquals(cycAccess.getConstantByName("and"), and);
      assertEquals(cycAccess.getConstantByName("BaseKB"), baseKB);
      assertEquals(cycAccess.getConstantByName("BinaryPredicate"), binaryPredicate);
      assertEquals(cycAccess.getConstantByName("comment"), comment);
      assertEquals(cycAccess.getConstantByName("different"), different);
      assertEquals(cycAccess.getConstantByName("elementOf"), elementOf);
      assertEquals(cycAccess.getConstantByName("genlMt"), genlMt);
      assertEquals(cycAccess.getConstantByName("genls"), genls);
      assertEquals(cycAccess.getConstantByName("isa"), isa);
      assertEquals(cycAccess.getConstantByName("numericallyEquals"), numericallyEqual);
      assertEquals(cycAccess.getConstantByName("or"), or);
      assertEquals(cycAccess.getConstantByName("PlusFn"), plusFn);
    } catch (Throwable e) {
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.  
   *
   * TODO associated the Cyc user state with the java client uuid, then put these tests back.
   */
  public void testBinaryCycAccess7() {
    if (connectionMode == SOAP_CYC_CONNECTION) {
      System.out.println("\n**** bypassing testBinaryCycAccess 7 in XML SOAP usage ****");

      return;
    }

    System.out.println("\n**** testBinaryCycAccess 7 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
      doTestCycAccess7(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 7 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   *
   * N O T E  be sure that the test system is clean of the special symbols 
   * introduced in the test.  E.G. MY-MACRO, A, B, C
   *
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess7(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();
    resetCycConstantCaches();

    //cycAccess.traceOn();
    // SubL scripts
    try {
      if (!cycAccess.isOpenCyc()) {
        //cycAccess.traceNamesOn();
        String script = null;
        // Java ByteArray  and SubL byte-vector are used only in the binary api.
        script = "(csetq my-byte-vector (vector 0 1 2 3 4 255))";

        Object responseObject = cycAccess.converseObject(script);
        assertNotNull(responseObject);
        assertTrue(responseObject instanceof ByteArray);

        byte[] myBytes = {0, 1, 2, 3, 4, -1};
        ByteArray myByteArray = new ByteArray(myBytes);
        assertEquals(myByteArray, responseObject);

        CycList command = new CycList();
        command.add(makeCycSymbol("equalp"));
        command.add(makeCycSymbol("my-byte-vector"));

        CycList command1 = new CycList();
        command.add(command1);
        command1.add(quote);
        command1.add(myByteArray);
        assertTrue(cycAccess.converseBoolean(command));
      }
      String script;
      Object responseObject;
      CycList responseList;
      String responseString;
      boolean responseBoolean;

      // definition
      script = "(define my-copy-tree (tree) \n" + "  (ret \n" + "    (fif (atom tree) \n"
              + "         tree \n" + "         ;; else \n"
              + "         (cons (my-copy-tree (first tree)) \n"
              + "               (my-copy-tree (rest tree))))))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-copy-tree"), responseObject);
      script = "(define my-floor (x y) \n" + "  (clet (results) \n"
              + "    (csetq results (multiple-value-list (floor x y))) \n"
              + "    (ret results)))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-floor"), responseObject);
      script = "(my-floor 5 3)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 2)"), responseList);

      script = "(defmacro my-macro (a b c) \n" + "  (ret `(list ,a ,b ,c)))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-macro"), responseObject);
      script = "(my-macro #$Dog #$Plant #$Brazil)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(#$Dog #$Plant #$Brazil)"),
              responseList);

      script = "(defmacro my-floor-macro (x y) \n" + "  (ret `(floor ,x ,y)))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-floor-macro"), responseObject);
      script = "(define my-floor-macro-test (x y) \n" + "    (ret (multiple-value-list (my-floor-macro x y))))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-floor-macro-test"), responseObject);
      script = "(my-floor-macro-test 5 3)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 2)"), responseList);

      script = "(defmacro my-floor-macro (x y) \n" + "  (ret `(floor ,x ,y)))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-floor-macro"), responseObject);
      script = "(my-floor-macro-test 5 3)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 2)"), responseList);

      // assignment

      /**
       * TODO: Use of the task processor means that CSETQ statements appear inside of a
       * CLET wrapper.  Need a way to set global variables.  Current method removes
       * the effect of CSETQ if setting a new variable.
       *
       *
       *
       *
       *
       *
       *
       *
       *
       *
       *
       *
       *
       */
      if (!cycAccess.isOpenCyc()) {
        script = "(csetq a '(1 #$Dog #$Plant))";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'a)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 #$Dog #$Plant)"), responseList);

        script = "(csetq a -1)";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'a)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(-1, responseObject);

        script = "(csetq a '(1 #$Dog #$Plant) \n" + "       b '(2 #$Dog #$Plant) \n" + "       c 3)";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'a)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 #$Dog #$Plant)"), responseList);
        script = "(symbol-value 'b)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(2 #$Dog #$Plant)"), responseList);
        script = "(symbol-value 'c)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(3, responseObject);

        script = "(clet ((a 0)) (cinc a) a)";
        assertEquals(1, cycAccess.converseObject(script));

        script = "(clet ((a 0)) (cinc a 10) a)";
        assertEquals(10, cycAccess.converseObject(script));

        script = "(clet ((a 0)) (cdec a) a)";
        assertEquals(-1, cycAccess.converseObject(script));

        script = "(clet ((a 0)) (cdec a 10) a)";
        assertEquals(-10, cycAccess.converseObject(script));

        script = "(cpush 4 a)";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'a)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(4 1 #$Dog #$Plant)"),
                responseList);

        script = "(cpop a)";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'a)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 #$Dog #$Plant)"),
                responseList);

        script = "(fi-set-parameter 'my-parm '(1 #$Dog #$Plant))";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'my-parm)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 #$Dog #$Plant)"), responseList);

        script = "(clet (a b) \n" + "  (csetq a '(1 2 3)) \n" + "  (csetq b (cpop a)) \n" + "  (list a b))";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("((2 3) (2 3))"), responseList);
      }

      // boundp
      if (!cycAccess.isOpenCyc()) {
        final Random random = new Random();
        CycSymbol symbol = makeCycSymbol("test-symbol-for-value-binding" + random.nextInt());
        assertTrue(!cycAccess.converseBoolean("(boundp '" + symbol + ")"));
        cycAccess.converseVoid("(csetq " + symbol + " nil)");
        assertTrue(cycAccess.converseBoolean("(boundp '" + symbol + ")"));
      }

      // fi-get-parameter
      if (!cycAccess.isOpenCyc()) {
        script = "(csetq my-parm '(2 #$Dog #$Plant))";
        cycAccess.converseVoid(script);
        script = "(fi-get-parameter 'my-parm)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(2 #$Dog #$Plant)"), responseList);
      }

      // eval
      script = "(eval '(csetq a 4))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(4,
              responseObject);
      script = "(eval 'a)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(4,
              responseObject);

      script = "(eval (list 'csetq 'a 5))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(5,
              responseObject);
      script = "(eval 'a)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(5,
              responseObject);

      // apply
      script = "(apply #'+ '(1 2 3))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(6,
              responseObject);

      script = "(apply #'+ 1 2 '(3 4 5))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(15,
              responseObject);

      script = "(apply (function +) '(1 2 3))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(6,
              responseObject);

      script = "(apply (function +) 1 2 '(3 4 5))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(15,
              responseObject);

      script = "(apply #'my-copy-tree '((1 (2 (3)))))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 (2 (3)))"),
              responseList);


      // funcall
      script = "(funcall #'+ 1 2 3)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(6,
              responseObject);

      script = "(funcall (function +) 1 2 3)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(6,
              responseObject);

      script = "(funcall #'first '(1 (2 (3))))";
      responseObject = cycAccess.converseObject(script);
      assertEquals("1", responseObject.toString());

      script = "(funcall #'my-copy-tree '(1 (2 (3))))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 (2 (3)))"),
              responseList);

      // multiple values
      script = "(multiple-value-list (floor 5 3))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1 2)"),
              responseList);

      if (!cycAccess.isOpenCyc()) {
        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);

        script = "(cmultiple-value-bind (a b) \n" + "    (floor 5 3) \n" + "  (csetq answer (list a b)))";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 2)"),
                responseList);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(1 2)"),
                responseList);

        script = "(define my-multiple-value-fn (arg1 arg2) \n"
                + "  (ret (values arg1 arg2 (list arg1 arg2) 0)))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(makeCycSymbol(
                "my-multiple-value-fn"),
                responseObject);

        script = "(my-multiple-value-fn #$Brazil #$Dog)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(cycAccess.getKnownConstantByGuid(
                BRAZIL_GUID_STRING),
                responseObject);

        script = "(cmultiple-value-bind (a b c d) \n" + "    (my-multiple-value-fn #$Brazil #$Dog) \n"
                + "  (csetq answer (list a b c d)))";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(#$Brazil #$Dog (#$Brazil #$Dog) 0)"),
                responseList);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(#$Brazil #$Dog (#$Brazil #$Dog) 0)"),
                responseList);
      }

      // arithmetic
      script = "(add1 2)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(3,
              responseObject);

      script = "(eq (add1 2) 3)";
      assertTrue(cycAccess.converseBoolean(script));

      script = "(sub1 10)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(9,
              responseObject);

      script = "(eq (sub1 10) 9)";
      assertTrue(cycAccess.converseBoolean(script));


      // sequence
      script = "(csetq a nil)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(nil,
              responseObject);

      script = "(progn (csetq a nil) (csetq a (list a)) (csetq a (list a)))";
      cycAccess.converseVoid(script);
      script = "(symbol-value 'a)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("((nil))"),
              responseList);

      // sequence with variable bindings
      script = "(clet (a b) " + "  (csetq a 1) " + "  (csetq b (+ a 3)) " + "  b)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(4,
              responseObject);

      script = "(clet ((a nil)) " + "  (cpush 1 a) " + "  a)";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(1)"),
              responseList);

      script = "(clet (a b) " + "  (csetq a '(1 2 3)) " + "  (csetq b (cpop a)) " + "  (list a b))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("((2 3) (2 3))"),
              responseList);

      script = "(clet ((a 1) " + "       (b (add1 a)) " + "       (c (sub1 b))) " + "  c)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(1, responseObject);

      // boolean expressions
      script = "(cand t nil t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(!responseBoolean);

      script = "(cand t t t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cand t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cand nil)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(!responseBoolean);

      script = "(cand t #$Dog)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(t,
              responseObject);

      script = "(cor t nil t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cor nil nil nil)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(!responseBoolean);

      script = "(cor t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cor nil)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(!responseBoolean);

      script = "(cor nil #$Plant)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(t,
              responseObject);

      script = "(cnot nil)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cnot t)";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(!responseBoolean);

      script = "(cnot (cand t nil))";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);

      script = "(cand (cnot nil) (cor t nil))";
      responseBoolean = cycAccess.converseBoolean(script);
      assertTrue(responseBoolean);


      // conditional sequencing
      if (!cycAccess.isOpenCyc()) {
        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(pcond ((eq 0 0) \n" + "        (csetq answer \"clause 1 true\")) \n"
                + "       ((> 1 4) \n" + "        (csetq answer \"clause 2 true\")) \n"
                + "       (t \n" + "        (csetq answer \"clause 3 true\")))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);

        script = "(pcond ((eq 1 0) \n" + "        (csetq answer \"clause 1 true\")) \n"
                + "       ((> 5 4) \n" + "        (csetq answer \"clause 2 true\")) \n"
                + "       (t \n" + "        (csetq answer \"clause 3 true\")))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);

        script = "(pcond ((eq 1 0) \n" + "        (csetq answer \"clause 1 true\")) \n"
                + "       ((> 1 4) \n" + "        (csetq answer \"clause 2 true\")) \n"
                + "       (t \n" + "        (csetq answer \"clause 3 true\")))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 3 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 3 true",
                responseString);

        script = "(pif (string= \"abc\" \"abc\") \n" + "     (csetq answer \"clause 1 true\") \n"
                + "     ;; else \n" + "     (csetq answer \"clause 2 true\"))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);

        script = "(pif (string> \"abc\" \"abc\") \n" + "     (csetq answer \"clause 1 true\") \n"
                + "     ;; else \n" + "     (csetq answer \"clause 2 true\"))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);

        script = "(csetq answer \n" + "       (fif (string= \"abc\" \"abc\") \n"
                + "            \"clause 1 true\" \n" + "            ;; else \n"
                + "            \"clause 2 true\"))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);

        script = "(csetq answer \n" + "       (fif (string> \"abc\" \"abc\") \n"
                + "            \"clause 1 true\" \n" + "            ;; else \n" + "            \"clause 2 true\"))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);

        script = "(progn \n" + "  (csetq answer \"clause 1 true\") \n"
                + "  (pwhen (string= \"abc\" \"abc\") \n"
                + "         (csetq answer \"clause 2 true\")))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);

        script = "(progn \n" + "  (csetq answer \"clause 1 true\") \n"
                + "  (pwhen (string> \"abc\" \"abc\") \n"
                + "         (csetq answer \"clause 2 true\")))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);

        script = "(progn \n" + "  (csetq answer \"clause 1 true\") \n"
                + "  (punless (string> \"abc\" \"abc\") \n"
                + "           (csetq answer \"clause 2 true\")))";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 2 true",
                responseString);

        script = "(progn \n" + "  (csetq answer \"clause 1 true\") \n"
                + "  (punless (string= \"abc\" \"abc\") \n"
                + "           (csetq answer \"clause 2 true\")))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(symbol-value 'answer)";
        responseString = cycAccess.converseString(script);
        assertEquals("clause 1 true",
                responseString);
      }

      // iteration
      if (!cycAccess.isOpenCyc()) {
        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);

        script = "(clet ((i 11)) \n" + "  (csetq answer -10) \n"
                + "  ;;(break \"environment\") \n" + "  (while (> i 0) \n"
                + "    (cdec i) \n" + "    (cinc answer)))";
        cycAccess.converseVoid(script);
        script = "(symbol-value 'answer)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(1,
                responseObject);

        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(progn \n" + "  (cdo ((x 0 (add1 x)) \n" + "        (y (+ 0 1) (+ y 2)) \n"
                + "        (z -10 (- z 1))) \n" + "       ((> x 3)) \n"
                + "    (cpush (list 'x x 'y y 'z z) answer)) \n" + "  (csetq answer (nreverse answer)))";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("((x 0 y 1 z -10) " + " (x 1 y 3 z -11) "
                + " (x 2 y 5 z -12) " + " (x 3 y 7 z -13))"),
                responseList);

        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(progn \n" + "  (clet ((x '(1 2 3))) \n" + "    (cdo nil ((null x) (csetq x 'y)) \n"
                + "      (cpush x answer) \n" + "      (cpop x)) \n" + "    x) \n"
                + "  (csetq answer (reverse answer)))";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("((1 2 3) " + " (2 3) " + " (3))"),
                responseList);

        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(cdolist (x '(1 2 3 4)) \n" + "  (cpush x answer))";
        assertEquals(nil, cycAccess.converseObject(script));
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertEquals(cycAccess.makeCycList("(4 3 2 1)"), responseList);
      }

      // mapping
      script = "(mapcar #'list '(a b c))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("((a) (b) (c))"), responseList);

      script = "(mapcar #'list '(a b c) '(d e f))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("((a d) (b e) (c f))"), responseList);

      script = "(mapcar #'eq '(a b c) '(d b f))";
      responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(nil t nil)"), responseList);

      if (!cycAccess.isOpenCyc()) {
        script = "(csetq answer nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);

        script = "(csetq my-small-dictionary nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);


        // Wrap the dictionary assignment in a progn that returns nil, to avoid sending the
        // dictionary itself back to the client, where it is not supported.
        script = "(progn (csetq my-small-dictionary (new-dictionary #'eq 3)) nil)";
        responseObject = cycAccess.converseObject(script);
        script = "(progn \n" + "  (dictionary-enter my-small-dictionary 'a 1) \n"
                + "  (dictionary-enter my-small-dictionary 'b 2) \n"
                + "  (dictionary-enter my-small-dictionary 'c 3))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(makeCycSymbol("c"), responseObject);
        script = "(define my-mapdictionary-fn (key value) \n"
                + "  (cpush (list key value) answer) \n"
                + "  (ret nil))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(makeCycSymbol("my-mapdictionary-fn"),
                responseObject);

        script = "(mapdictionary my-small-dictionary #'my-mapdictionary-fn)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertTrue(responseList.contains(cycAccess.makeCycList("(a 1)")));
        assertTrue(responseList.contains(cycAccess.makeCycList("(b 2)")));
        assertTrue(responseList.contains(cycAccess.makeCycList("(c 3)")));

        script = "(csetq my-large-dictionary nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(progn (csetq my-large-dictionary (new-dictionary #'eq 200)) nil)";
        responseObject = cycAccess.converseObject(script);
        script = "(clet ((cities (remove-duplicates \n" + "                 (with-all-mts \n"
                + "                   (instances #$IndependentCountry)))) \n"
                + "        capital-city) \n" + "  (cdolist (city cities) \n"
                + "    (csetq capital-city (pred-values-in-any-mt city #$capitalCity)) \n"
                + "    (dictionary-enter my-large-dictionary \n" + "                      city \n"
                + "                      (fif (consp capital-city) \n"
                + "                           (first capital-city) \n"
                + "                           ;; else \n" + "                           nil))))";
        responseObject = cycAccess.converseObject(script);

        script = "(mapdictionary my-large-dictionary #'my-mapdictionary-fn)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertTrue(responseList.contains(cycAccess.makeCycList(
                "(#$Brazil #$CityOfBrasiliaBrazil)")));

        script = "(define my-parameterized-mapdictionary-fn (key value args) \n"
                + "  (cpush (list key value args) answer) \n" + "  (ret nil))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(makeCycSymbol("my-parameterized-mapdictionary-fn"),
                responseObject);

        script = "(mapdictionary-parameterized my-small-dictionary #'my-parameterized-mapdictionary-fn '(\"x\"))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertTrue(responseList.contains(cycAccess.makeCycList("(a 1 (\"x\"))")));
        assertTrue(responseList.contains(cycAccess.makeCycList("(b 2 (\"x\"))")));
        assertTrue(responseList.contains(cycAccess.makeCycList("(c 3 (\"x\"))")));

        script = "(mapdictionary-parameterized my-large-dictionary #'my-parameterized-mapdictionary-fn '(1 2))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(symbol-value 'answer)";
        responseList = cycAccess.converseList(script);
        assertTrue(responseList.contains(cycAccess.makeCycList(
                "(#$Brazil #$CityOfBrasiliaBrazil (1 2))")));
      }

      // ccatch and throw
      script = "(define my-super () \n" + "  (clet (result) \n" + "    (ccatch :abort \n"
              + "      result \n" + "      (my-sub) \n" + "      (csetq result 0)) \n"
              + "  (ret result)))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-super"), responseObject);

      script = "(define my-sub () \n" + "  (clet ((a 1) (b 2)) \n" + "  (ignore a b) \n"
              + "  (ret (throw :abort 99))))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("my-sub"), responseObject);
      script = "(my-super)";
      responseObject = cycAccess.converseObject(script);
      assertEquals(99, responseObject);


      // ignore-errors, cunwind-protect
      //cycAccess.traceOn();
      script = "(clet (result) \n" + "  (ignore-errors \n" + "    (cunwind-protect \n"
              + "	(/ 1 0) \n" + "      (csetq result \"protected\"))) \n"
              + "  result)";
      responseObject = cycAccess.converseObject(script);
      assertEquals("protected", responseObject);


      // get-environment
      if (!cycAccess.isOpenCyc()) {
        script = "(csetq a nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil, responseObject);
        script = "(csetq b -1)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(-1, responseObject);
      }


      // cdestructuring-bind
      script = "(cdestructuring-bind () '() (print 'foo))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("foo"), responseObject);

      script = "(cdestructuring-bind (&whole a) () (print 'foo))";
      responseObject = cycAccess.converseObject(script);
      assertEquals(makeCycSymbol("foo"), responseObject);

      script = "(cdestructuring-bind (&whole a b c) '(1 2) (print (list a b c)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("((1 2) 1 2)"));

      script = "(cdestructuring-bind (a b . c) '(1 2 3 4) (print (list a b c)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 2 (3 4))"));

      script = "(cdestructuring-bind (&optional a) '(1) (print (list a)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1)"));

      script = "(cdestructuring-bind (a &optional b) '(1 2) (print (list a b)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 2)"));

      script = "(cdestructuring-bind (&whole a &optional b) '(1) (print (list a b)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("((1) 1)"));

      script = "(cdestructuring-bind (&rest a) '(1 2) (print (list a)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("((1 2))"));

      script = "(cdestructuring-bind (&whole a b &rest c) '(1 2 3) (print (list a b c)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("((1 2 3) 1 (2 3))"));

      script = "(cdestructuring-bind (&key a b) '(:b 2 :a 1) (print (list a b)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 2)"));

      script = "(cdestructuring-bind (&key a b) '(:b 2 :allow-other-keys t :a 1 :c 3) (print (list a b)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 2)"));

      script = "(cdestructuring-bind (&key ((key a) 23 b)) '(key 1) (print (list a b)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 T)"));

      script = "(cdestructuring-bind (a &optional b &key c) '(1 2 :c 3) (print (list a b c)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("(1 2 3)"));

      script = "(cdestructuring-bind (&whole a b &optional c &rest d &key e &allow-other-keys &aux f) '(1 2 :d 4 :e 3) (print (list a b c d e f)))";
      responseList = cycAccess.converseList(script);
      assertEquals(responseList, cycAccess.makeCycList("((1 2 :D 4 :E 3) 1 2 (:D 4 :E 3) 3 NIL)"));


      if (!cycAccess.isOpenCyc()) {
        // type testing
        script = "(csetq a 1)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(1, responseObject);
        script = "(numberp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a \"abc\")";
        responseObject = cycAccess.converseObject(script);
        assertEquals("abc",
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a 2.14)";
        responseObject = cycAccess.converseObject(script);
        assertTrue(responseObject instanceof Double);
        assertTrue(((Double) responseObject).doubleValue() > 2.13999);
        assertTrue(((Double) responseObject).doubleValue() < 2.14001);
        script = "(numberp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a 'my-symbol)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(makeCycSymbol(
                "my-symbol"),
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a '(1 . 2))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(cycAccess.makeCycList("(1 . 2)"),
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a '(1 2))";
        responseObject = cycAccess.converseObject(script);
        assertEquals(cycAccess.makeCycList("(1 2)"),
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(!cycAccess.converseBoolean(script));

        script = "(csetq a nil)";
        responseObject = cycAccess.converseObject(script);
        assertEquals(nil,
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(cycAccess.converseBoolean(script));

        // empty list is treated the same as nil.
        CycList command = new CycList();
        command.add(makeCycSymbol("csetq"));
        command.add(makeCycSymbol("a"));
        command.add(new CycList());
        responseObject = cycAccess.converseObject(command);
        assertEquals(nil,
                responseObject);
        script = "(numberp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(integerp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(stringp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(atom a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(floatp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(symbolp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(consp a)";
        assertTrue(!cycAccess.converseBoolean(script));
        script = "(listp a)";
        assertTrue(cycAccess.converseBoolean(script));
        script = "(null a)";
        assertTrue(cycAccess.converseBoolean(script));
      }


      /*
      // constant name with embedded slash
      //cycAccess.traceOn();
      script =
      "(rtp-parse-exp-w/vpp \"Symptoms of EEE begin 4-10 days after infection\" \n" +
      "(fort-for-string \"STemplate\") \n" +
      "(fort-for-string \"AllEnglishTemplateMt\") \n" +
      "(fort-for-string \"RKFParsingMt\"))";
      responseList = cycAccess.converseList(script);
       */

      // check-type
      script =
              "(clet (result) \n" + "  (ignore-errors \n" + "    (/ 1 1) \n"
              + "    (csetq result t)) \n" + "  result)";
      assertEquals((Object) t, cycAccess.converseObject(script));
      script =
              "(clet (result) \n" + "  (ignore-errors \n" + "    (/ 1 0) \n"
              + "    (csetq result t)) \n" + "  result)";
      assertEquals((Object) nil, cycAccess.converseObject(script));

      //cycAccess.traceOn();
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess8() {
    System.out.println("\n**** testBinaryCycAccess 8 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }


      //cycAccess.traceOnDetailed();
      doTestCycAccess8(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 8 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess8(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    try {

      // List containing null is coerced to list containing NIL.
      String script = "(put-api-user-variable 'a '(nil 1))";
      Object responseObject = cycAccess.converseObject(script);
      assertEquals(nil, responseObject);

      script = "(get-api-user-variable 'a)";

      CycList responseList = cycAccess.converseList(script);
      assertEquals(cycAccess.makeCycList("(nil 1)"), responseList);

      if (!cycAccess.isOpenCyc()) {
        // rkfPhraseReader
        CycFort rkfEnglishLexicalMicrotheoryPsc = cycAccess.getKnownConstantByGuid(
                "bf6df6e3-9c29-11b1-9dad-c379636f7270");
        String text = "penguins";
        CycList parseExpressions = cycAccess.rkfPhraseReader(
                text,
                rkfEnglishLexicalMicrotheoryPsc,
                (CycFort)CycAccess.inferencePSC);
        CycList parseExpression = (CycList) parseExpressions.first();
        CycList spanExpression = (CycList) parseExpression.first();
        CycList terms = (CycList) parseExpression.second();

        // #$Penguin
        CycFort penguin = cycAccess.getKnownConstantByGuid(PENGUIN_GUID_STRING);
        assertTrue(terms.contains(penguin));

        // #$PittsburghPenguins
        CycFort pittsburghPenguins = cycAccess.getKnownConstantByGuid(
                PITTSBURGH_PENGUINS_GUID_STRING);
        assertTrue(terms.contains(pittsburghPenguins));

        // generateDisambiguationPhraseAndTypes
        CycList objects = new CycList();
        objects.add(penguin);
        objects.add(pittsburghPenguins);

        CycList disambiguationExpression = cycAccess.generateDisambiguationPhraseAndTypes(
                objects);
        System.out.println("disambiguationExpression\n" + disambiguationExpression);
        assertEquals(2, disambiguationExpression.size());

        CycList penguinDisambiguationExpression = (CycList) disambiguationExpression.first();
        System.out.println("penguinDisambiguationExpression\n" + penguinDisambiguationExpression);
        assertTrue(penguinDisambiguationExpression.contains("penguin"));

        CycList pittsburghPenguinDisambiguationExpression =
                (CycList) disambiguationExpression.second();
        System.out.println("pittsburghPenguinDisambiguationExpression\n" + pittsburghPenguinDisambiguationExpression);
        assertTrue(pittsburghPenguinDisambiguationExpression.contains("the Pittsburgh Penguins"));
        assertTrue(pittsburghPenguinDisambiguationExpression.contains("ice hockey team"));
      }
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess9() {
    System.out.println("\n**** testBinaryCycAccess 9 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      System.out.println(cycAccess.getCycConnection().connectionInfo());


      //cycAccess.traceOn();
      doTestCycAccess9(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 9 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess9(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    try {
      CycConstant brazil = cycAccess.getKnownConstantByGuid(BRAZIL_GUID_STRING);
      CycConstant country = cycAccess.getKnownConstantByGuid(COUNTRY_GUID_STRING);
      CycConstant worldGeographyMt = cycAccess.getKnownConstantByGuid(
              WORLD_GEOGRAPHY_MT_GUID_STRING);
      CycConstant dog = cycAccess.getKnownConstantByGuid(DOG_GUID_STRING);
      HashSet hashSet = new HashSet();
      hashSet.add(dog);
      assertTrue(hashSet.contains(dog));

      CycConstant animal = cycAccess.getKnownConstantByGuid(ANIMAL_GUID_STRING);
      CycConstant biologyVocabularyMt = cycAccess.getKnownConstantByGuid(
              BIOLOGY_VOCABULARY_MT_GUID_STRING);
      CycConstant performedBy = cycAccess.getKnownConstantByGuid(PERFORMED_BY_GUID_STRING);
      CycConstant doneBy = cycAccess.getKnownConstantByGuid(DONE_BY_GUID_STRING);
      CycConstant siblings = cycAccess.getKnownConstantByGuid(SIBLINGS_GUID_STRING);
      CycConstant generalLexiconMt = cycAccess.getKnownConstantByGuid(
              GENERAL_LEXICON_MT_GUID_STRING);
      CycConstant paraphraseMt = cycAccess.getKnownConstantByGuid(PARAPHRASE_MT_GUID_STRING);
      CycConstant mtTimeWithGranularityDimFn = cycAccess.getKnownConstantByGuid(
              MT_TIME_WITH_GRANULARITY_DIM_FN_GUID_STRING);
      CycConstant mtSpace = cycAccess.getKnownConstantByGuid(MT_SPACE_GUID_STRING);
      CycConstant now = cycAccess.getKnownConstantByGuid(NOW_GUID_STRING);
      CycConstant timePoint = cycAccess.getKnownConstantByGuid(TIMEPOINT_GUID_STRING);
      //(#$MtSpace (#$MtTimeWithGranularityDimFn #$Now #$TimePoint) #$WorldGeographyMt)
      ELMt worldGeographyMtNow =
              cycAccess.makeELMt(
              new CycNaut(
              mtSpace, new CycNaut(mtTimeWithGranularityDimFn, now, timePoint), worldGeographyMt));


      // isa
      assertTrue(cycAccess.isa(brazil, country, worldGeographyMt));
      assertTrue(cycAccess.isa(brazil, country, worldGeographyMtNow));
      assertTrue(cycAccess.isa(brazil, country));

      // isGenlOf
      assertTrue(cycAccess.isGenlOf(animal, dog, biologyVocabularyMt));
      assertTrue(cycAccess.isGenlOf(animal, dog));


      // isGenlPredOf
      assertTrue(cycAccess.isGenlPredOf(doneBy, performedBy, baseKB));
      assertTrue(cycAccess.isGenlPredOf(doneBy, performedBy));


      // isGenlInverseOf
      assertTrue(cycAccess.isGenlInverseOf(siblings, siblings,
              biologyVocabularyMt));
      assertTrue(cycAccess.isGenlInverseOf(siblings, siblings));


      // isGenlMtOf
      if (!cycAccess.isOpenCyc()) {
        assertTrue(cycAccess.isGenlMtOf(baseKB, biologyVocabularyMt));
      }

      /*
      // tests proper receipt of narts from the server.
      String script = "(csetq all-narts nil)";
      cycAccess.converseVoid(script);
      script = "(progn \n" +
      "  (do-narts (nart) \n" +
      "    (cpush nart all-narts)) \n" +
      "  nil)";
      cycAccess.converseVoid(script);
      script = "(clet (nart) \n" +
      "  (csetq nart (first all-narts)) \n" +
      "  (csetq all-narts (rest all-narts)) \n" +
      "  nart)";
      long numberGood = 0;
      long numberNil = 0;
      while (true) {
      Object obj = cycAccess.converseObject(script);
      if (obj.equals(nil))
      break;
      assertTrue(obj instanceof CycNart);
      CycNart cycNart = (CycNart) obj;
      assertTrue(cycNart.cyclify() instanceof String);
      String script2 = "(find-nart " + cycNart.stringApiValue() + ")";
      Object obj2 = cycAccess.converseObject(script2);
      if (cycNart.equals(obj))
      numberGood++;
      else
      numberNil++;
      }
      assertTrue(numberGood > 20 * numberNil);
      script = "(csetq all-narts nil)";
      cycAccess.converseVoid(script);
       */
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess10() {
    System.out.println("\n**** testBinaryCycAccess 10 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      System.out.println(cycAccess.getCycConnection().connectionInfo());


      //cycAccess.traceOn();
      doTestCycAccess10(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 10 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess10(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    try {
      // demonstrate quoted strings
      //cycAccess.traceOn();
      StringBuffer stringBuffer = new StringBuffer();
      stringBuffer.append("a");
      stringBuffer.append('"');
      stringBuffer.append("b");
      stringBuffer.append('"');
      stringBuffer.append("c");

      String expectedString = stringBuffer.toString();
      CycList command = new CycList();
      command.add(makeCycSymbol("identity"));
      command.add(expectedString);

      String resultString = cycAccess.converseString(command);
      assertEquals(expectedString, resultString);

      CycList cycList53 = cycAccess.makeCycList("(\"abc\")");
      assertEquals(1,
              cycAccess.converseInt("(length '" + cycList53.cycListApiValue() + ")"));
      assertEquals(3,
              cycAccess.converseInt("(length (first '" + cycList53.cycListApiValue() + "))"));

      String string = "abc";
      CycList cycList54 = new CycList();
      cycList54.add(makeCycSymbol("length"));
      cycList54.add(string);
      assertEquals(3, cycAccess.converseInt(cycList54));

      String quotedString = "\"abc\" def";
      CycList cycList55 = new CycList();
      cycList55.add(makeCycSymbol("length"));
      cycList55.add(quotedString);


      // Note that in binary mode, that Cyc's cfasl input will insert the required escape
      // chars for embedded quotes.
      // And in ascii mode note that CycConnection will insert the required escape
      // chars for embedded quotes.  While in binary mode, CfaslOutputStream will insert
      // the required escapes.
      //
      // Cyc should see (length "\"abc\" def") and return 9
      assertEquals(9, cycAccess.converseInt(cycList55));

      // demonstrate quoted strings with the CycListParser
      CycList cycList56 = cycAccess.makeCycList("(\"" + string + "\")");
      assertEquals(1, cycAccess.converseInt("(length " + cycList56.stringApiValue() + ")"));
      assertEquals(3, cycAccess.converseInt("(length (first " + cycList56.stringApiValue() + "))"));

      String embeddedQuotesString = "(" + "\"\\\"abc\\\" def\"" + ")";
      CycList cycList57 = cycAccess.makeCycList(embeddedQuotesString);
      String script = "(length " + cycList57.stringApiValue() + ")";
      int actualLen = cycAccess.converseInt(script);
      assertEquals(1, actualLen);
      assertEquals(9, cycAccess.converseInt("(length (first " + cycList57.stringApiValue() + "))"));

      script = "(identity (quote (#$givenNames #$Guest \"\\\"The\\\" Guest\")))";

      String script1 = "(IDENTITY (QUOTE (#$givenNames #$Guest \"\"The\" Guest\")))";

      //CycListParser.verbosity = 3;
      CycList scriptCycList = cycAccess.makeCycList(script);


      // Java strings do not escape embedded quote chars
      assertEquals(script1, scriptCycList.cyclify());

      CycList answer = cycAccess.converseList(script);
      Object third = answer.third();
      assertTrue(third instanceof String);
      assertEquals(11, ((String) third).length());

      answer = cycAccess.converseList(scriptCycList);
      third = answer.third();
      assertTrue(third instanceof String);
      assertEquals(11, ((String) third).length());

      // isFormulaWellFormed
      if (!cycAccess.isOpenCyc()) {
        CycFormulaSentence formula1 = cycAccess.makeCycSentence("(#$isa #$Brazil #$IndependentCountry)");
        CycConstant mt = cycAccess.getKnownConstantByName("WorldPoliticalGeographyDataVocabularyMt");
        assertTrue(cycAccess.isFormulaWellFormed(formula1, mt));
        CycFormulaSentence formula2 = cycAccess.makeCycSentence("(#$genls #$Brazil #$Collection)");
        assertTrue(!cycAccess.isFormulaWellFormed(formula2, mt));
      }

      // isCycLNonAtomicReifableTerm
      CycNaut formula3 = cycAccess.makeCycNaut("(#$TheCovering #$Watercraft-Surface #$Watercraft-Subsurface)");
      assertTrue(cycAccess.isCycLNonAtomicReifableTerm(formula3));

      CycFormulaSentence formula4 = cycAccess.makeCycSentence("(#$isa #$Plant #$Animal)");
      assertTrue(!cycAccess.isCycLNonAtomicReifableTerm(formula4));

      CycNaut formula5 = cycAccess.makeCycNaut("(#$PlusFn 1)");
      assertTrue(!cycAccess.isCycLNonAtomicReifableTerm(formula5));

      // isCycLNonAtomicUnreifableTerm
      CycNaut formula6 = cycAccess.makeCycNaut("(#$TheCovering #$Watercraft-Surface #$Watercraft-Subsurface)");
      assertTrue(!cycAccess.isCycLNonAtomicUnreifableTerm(formula6));

      CycFormulaSentence formula7 = cycAccess.makeCycSentence("(#$isa #$Plant #$Animal)");
      assertTrue(!cycAccess.isCycLNonAtomicUnreifableTerm(formula7));

      CycNaut formula8 = cycAccess.makeCycNaut("(#$PlusFn 1)");
      assertTrue(cycAccess.isCycLNonAtomicUnreifableTerm(formula8));
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess11() {
    System.out.println("\n**** testBinaryCycAccess 11 ****");
    resetCaches();

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
          cycAccess.getCycLeaseManager().addListener(this);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }


      //cycAccess.traceOnDetailed();
      try {
        if (!cycAccess.isOpenCyc()) {
          doTestCycAccess11(cycAccess);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      cycAccess.getCycLeaseManager().removeAllListeners();
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 11 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess11(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    try {
      String script = "(+ 1 2)";
      int answer = cycAccess.converseInt(script);
      assertEquals(3, answer);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }
//    cycAccess.getCycLeaseManager().setLeaseDurationMilliseconds(100000);
//    cycAccess.getCycLeaseManager().immediatelyRenewLease();

    System.out.println("Concurrent API requests.");

    ArrayList apiRequestors = new ArrayList();

    ApiRequestor apiRequestor = new ApiRequestor("Long", 1, "1", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short1", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short2", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short3", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short4", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short5", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short6", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short7", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    apiRequestor = new ApiRequestor("Short8", 4, "150000", cycAccess);
    apiRequestor.start();
    apiRequestors.add(apiRequestor);

    int iterationsUntilCancel = 10;
    boolean isCancelled = false;
    while (true) {
      boolean apiRequestorTheadRunning = false;

      try {
        Thread.sleep(3000);
      } catch (InterruptedException e) {
        break;
      }

      System.out.println("-----------------------");
      for (int i = 0; i < apiRequestors.size(); i++) {
        apiRequestor = (ApiRequestor) apiRequestors.get(i);

        if (!apiRequestor.done) {
          apiRequestorTheadRunning = true;

          if ((iterationsUntilCancel-- < 0) && apiRequestor.name.equals("Long") && !isCancelled) {
            System.out.println("Cancelling " + apiRequestor.name);
            isCancelled = true;
            try {
              apiRequestor.cancel();
            } catch (Throwable e) {
              e.printStackTrace();
              fail(e.getMessage());
            }
          } else {
            System.out.println("  " + apiRequestor.name + " is still running");
          }
        }
      }

      if (!apiRequestorTheadRunning) {
        break;
      }
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Class ApiRequestor.
   */
  protected class ApiRequestor extends Thread {

    /** the connection to Cyc */
    final private CycAccess cycAccess;
    /** the name of the api requestor process */
    final public String name;
    /** the api request repeat count */
    final private int repeatCount;
    /** the api request duration factor */
    final private String durationFactor;
    /** the process completion indicator */
    public boolean done = false;
    public SubLWorkerSynch worker;

    /**
     * Constructs a ApiRequestor object.
     * 
     * @param name the name of the api requestor process
     * @param repeatCount the api request repeat count
     * @param durationFactor the api request duration factor
     * @param cycAccess the connection to Cyc
     */
    public ApiRequestor(final String name,
            final int repeatCount,
            final String durationFactor,
            final CycAccess cycAccess) {
      this.name = name;
      this.repeatCount = repeatCount;
      this.durationFactor = durationFactor;
      this.cycAccess = cycAccess;
    }

    /**
     * Makes some API requests.
     *
     * @throws RuntimeException when wrong answer detected
     */
    public void run() {
      System.out.println("ApiRequestor " + name + " starting.");

      try {
        for (int i = 0; i < repeatCount; i++) {
          final String testPhrase = name + "-" + Integer.toString(i + 1);
          final String script = (name.equals("Long"))
                  ? "(catch-task-processor-termination-quietly (progn (do-assertions (assertion))\n"
                  + " \"" + testPhrase + "\"))"
                  : "(catch-task-processor-termination-quietly (progn (cdotimes (x "
                  + durationFactor + "))\n" + " \"" + testPhrase + "\"))";
          worker = new DefaultSubLWorkerSynch(script, cycAccess);
          final Object answer = worker.getWork();
          if (answer.toString().equals(":CANCEL")) {
            System.out.println(name + " returned :CANCEL");
            done = true;
            return;
          } else {
            System.out.println(name + " iteration " + answer + " done.");
            if (!answer.equals(testPhrase)) {
              throw new RuntimeException(testPhrase + " not equal to " + answer);
            }
          }
        }
      } catch (Throwable e) {
        System.out.println("ApiRequestor " + name + " exception: " + e.toString());
        e.printStackTrace();
        done = true;
        return;
      }

      System.out.println("ApiRequestor " + name + " done.");
      done = true;
    }

    /**
     * Cancels this thread at the Cyc Server.
     * 
     * @throws CycApiException when an api error occurs
     * @throws IOException when a communication error occurs
     */
    public void cancel() throws CycApiException, IOException {
      cycAccess.getCycConnection().cancelCommunication(worker);
    }
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess12() {
    System.out.println("\n**** testBinaryCycAccess 12 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName,
                  testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      doTestCycAccess12(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 12 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess12(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    try {
      //cycAccess.traceOn();

      String utf8String = "ABCdef";
      assertEquals(utf8String,
              cycAccess.converseString("(identity \"" + utf8String + "\")"));

      InputStreamReader inputStreamReader = null;

      try {
        inputStreamReader = new InputStreamReader(new FileInputStream(
                new File("utf8-sample.html")),
                "UTF-8");
      } catch (IOException e) {
        return;
      }

      StringBuffer utf8StringBuffer = new StringBuffer();

      while (true) {
        int ch = inputStreamReader.read();

        if (ch == -1) {
          break;
        }

        if ((ch == '\n') || (ch == '\r')) {
          utf8StringBuffer.append(' ');
        } else {
          utf8StringBuffer.append((char) ch);
        }
      }

      utf8String = utf8StringBuffer.toString();

      PrintWriter utf8Output = new PrintWriter(new OutputStreamWriter(
              new FileOutputStream("utf8-sample-without-newlines.html"), "UTF8"));
      utf8Output.print(utf8String);
      utf8Output.close();

      CycList command = new CycList();
      command.add(makeCycSymbol("identity"));
      command.add(utf8String);

      String echoUtf8Sting = cycAccess.converseString(command);

      utf8Output = new PrintWriter(new OutputStreamWriter(
              new FileOutputStream("utf8-sample-from-cyc.html"), "UTF8"));
      utf8Output.print(utf8String);
      utf8Output.close();

      System.out.println("utf8String\n" + utf8String);
      System.out.println("echoUtf8Sting\n" + echoUtf8Sting);
      assertEquals(utf8String,
              echoUtf8Sting);

      CycFort myTerm = cycAccess.getConstantByName("my-term");

      if (myTerm != null) {
        cycAccess.kill(myTerm);
      }

      myTerm = cycAccess.findOrCreate("my-term");
      cycAccess.assertComment(myTerm, utf8String, baseKB);
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.toString());
    }
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess13() {
    System.out.println("\n**** testBinaryCycAccess 13 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

//    cycAccess.traceOn();

      // canonicalizeHLMT
      try {
        if (!(cycAccess.isOpenCyc())) {
          // NART case
          {
            //cycAccess.traceNamesOn();
            final String elmtString = "(#$LexicalMtForLanguageFn #$AzeriLanguage)";
            final CycNaut mt = cycAccess.makeCycNaut(elmtString);
            final ELMt hlmtObject = cycAccess.canonicalizeHLMT(mt);
            assertNotNull(hlmtObject);
            assertTrue(hlmtObject instanceof CycNart);
            assertEquals(elmtString, hlmtObject.cyclify());
          }

          // NAUT case
          {
            final String elmtString = "(#$MtSpace #$BaseKB (#$MtTimeDimFn #$Now))";
            final CycNaut mt = cycAccess.makeCycNaut(elmtString);
            final ELMt hlmtObject = cycAccess.canonicalizeHLMT(mt);
            assertNotNull(hlmtObject);
            assertTrue(hlmtObject instanceof CycNaut);
            assertEquals(elmtString, hlmtObject.cyclify());
          }

          // Constant case
          {
            final String elmtString = "(#$MtSpace #$BaseKB (#$MtTimeWithGranularityDimFn #$Always-TimeInterval #$Null-TimeParameter))";
            final CycNaut mt = cycAccess.makeCycNaut(elmtString);
            final ELMt hlmtObject = cycAccess.canonicalizeHLMT(mt);
            assertNotNull(hlmtObject);
            assertTrue(hlmtObject instanceof CycConstant);
            assertEquals("#$BaseKB", hlmtObject.cyclify());
          }

          // Nonsense case
          {
            final String elmtString = "(#$PlusFn 1 1)";
            final CycNaut mt = cycAccess.makeCycNaut(elmtString);
            final CycObject hlmtObject = cycAccess.canonicalizeHLMT(mt);
            assertNotNull(hlmtObject);
            assertTrue(hlmtObject instanceof CycNaut);
            assertEquals(elmtString,
                    hlmtObject.cyclify());
          }
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // makeELMt
      try {
        if (!(cycAccess.isOpenCyc())) {
          // NART case
          String elmtString = "(#$LexicalMtForLanguageFn #$AzeriLanguage)";
          CycNaut naut = cycAccess.makeCycNaut(elmtString);
          ELMt elmt = cycAccess.makeELMt(naut);
          assertNotNull(elmt);
          assertTrue(elmt instanceof CycNart);
          assertEquals(elmtString, elmt.cyclify());


          // Nonsense case
          elmtString = "(#$PlusFn 1 1)";
          naut = cycAccess.makeCycNaut(elmtString);
          elmt = cycAccess.makeELMt(naut);
          assertNotNull(elmt);
          assertTrue(elmt instanceof CycNaut);
          assertEquals(elmtString, elmt.cyclify());


          // Constant case
          elmtString = "BaseKB";
          CycFort baseKB = cycAccess.getKnownConstantByName(elmtString);
          elmt = cycAccess.makeELMt(baseKB);
          assertNotNull(elmt);
          assertTrue(elmt instanceof CycConstant);
          assertEquals(elmtString, elmt.toString());
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // getHLCycTerm
      try {
        Object obj = cycAccess.getHLCycTerm("1");
        assertTrue(obj instanceof Integer);
        obj = cycAccess.getHLCycTerm("\"abc\"");
        assertTrue(obj instanceof String);
        {
          CycConstant randomConstant = cycAccess.getRandomConstant();
          obj = cycAccess.getHLCycTerm(randomConstant.cyclify());
          assertEquals(randomConstant, obj);
        }
        {
          boolean ok = true;
          CycNart randomNart = null;
          for (int count = 0; (count < 1000) && (ok == true); count++) {
            while (randomNart == null || !(cycAccess.isGround(randomNart))) { //Non-ground NARTs can have canonicalization issues.
              randomNart = cycAccess.getRandomNart();
            }
            obj = cycAccess.getHLCycTerm(randomNart.cyclify());
            if (!randomNart.equalsAtEL(obj)) {
              cycAccess.getHLCycTerm(randomNart.cyclify());
              randomNart.equalsAtEL(obj);
              ok = false;
            }
          }
          assertTrue(randomNart.cyclify() + " is not equal (at EL) to " + obj, ok);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }


      // getELCycTerm
      try {
        Object obj = cycAccess.getELCycTerm("1");
        assertTrue(obj instanceof Integer);
        obj = cycAccess.getELCycTerm("\"abc\"");
        assertTrue(obj instanceof String);

        CycConstant randomConstant = cycAccess.getRandomConstant();
        obj = cycAccess.getHLCycTerm(randomConstant.cyclify());
        assertEquals(randomConstant, obj);

        CycNart randomNart = cycAccess.getRandomNart();
        obj = cycAccess.getHLCycTerm(randomNart.cyclify());
        assertEquals(randomNart, obj);
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // canonicalizeList
      try {
        String query = "(#$isa (#$DayFn 1 (#$MonthFn #$March (#$YearFn 2004))) #$Event)";
        CycList queryList = cycAccess.makeCycList(query);
        //System.out.println(queryList.cyclify());
        CycList canonicalizedList = cycAccess.canonicalizeList(queryList);
        //System.out.println(canonicalizedList.cyclify());
        assertTrue(canonicalizedList.second() instanceof CycList);
        InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
        assertTrue(!cycAccess.isQueryTrue(canonicalizedList, universalVocabularyMt, queryProperties));

        query = "(#$isa (#$DayFn 1 (#$MonthFn #$March (#$YearFn 2004))) #$CalendarDay)";
        queryList = cycAccess.makeCycList(query);
        //System.out.println(queryList.cyclify());
        canonicalizedList = cycAccess.canonicalizeList(queryList);
        //System.out.println(canonicalizedList.cyclify());
        assertTrue(canonicalizedList.second() instanceof CycList);
        assertTrue(cycAccess.isQueryTrue(canonicalizedList, universalVocabularyMt, queryProperties));

        query = "(#$isa (#$DayFn 1 (#$MonthFn #$March (#$YearFn 2004))) #$CalendarDay)";
        final CycFormulaSentence sentence = cycAccess.makeCycSentence(query);
        //System.out.println(queryList.cyclify());
        assertTrue(cycAccess.isQueryTrue(sentence, universalVocabularyMt, queryProperties));
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // assertions containing hl variables
      try {
        if (!cycAccess.isOpenCyc()) {
          CycFormulaSentence query = cycAccess.makeCycSentence("(#$salientAssertions #$performedBy ?ASSERTION)");
          InferenceParameters queryProperties = cycAccess.getHLQueryProperties();
          queryProperties.put(makeCycSymbol(":answer-language"), makeCycSymbol(":hl"));
          InferenceResultSet resultSet = cycAccess.executeQuery(query, baseKB, queryProperties);
          resultSet.next();
          CycAssertion cycAssertion = (CycAssertion) resultSet.getCycObject(1);
          System.out.println("cycAssertion= " + cycAssertion.cyclify());
          assertTrue(cycAssertion.cyclify().indexOf("?VAR0") > -1);
          CycList command = new CycList();
          command.add(makeCycSymbol("identity"));
          command.add(cycAssertion);
          //cycAccess.traceOnDetailed();
          Object result = cycAccess.converseObject(command);
          assertTrue(result instanceof CycAssertion);
          assertTrue(((CycAssertion) result).cyclify().indexOf("?VAR0") > -1);
          //System.out.println("cycAssertion= " + ((CycAssertion) result).cyclify());
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 13 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.
   */
  public void testBinaryCycAccess14() {
    System.out.println("\n**** testBinaryCycAccess 14 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        System.out.println("\nException: " + e.getMessage());
        fail(e.toString());
      }

      assertNotNull(cycAccess.getCycConnection());
      doTestCycAccess14(cycAccess);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 14 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the given api connection.
   * 
   * @param cycAccess the server connection handler
   */
  protected void doTestCycAccess14(CycAccess cycAccess) {
    long startMilliseconds = System.currentTimeMillis();

    if (cycAccess.getCycConnection() != null) {
      System.out.println(cycAccess.getCycConnection().connectionInfo());
    } else {
      System.out.println("CycConnection info is null.");
    }

    resetCycConstantCaches();

    try {
      // second call should access the cache by GUID
      cycAccess.traceOn();
      System.out.println("------------");

      Guid organizationGuid = new Guid(ORGANIZATION_GUID_STRING);
      CycConstant organization = cycAccess.getConstantByGuid(organizationGuid);
      System.out.println("------------");
      organization = cycAccess.getConstantByGuid(organizationGuid);
      System.out.println("------------");
      cycAccess.traceOff();
    } catch (Throwable e) {
      e.printStackTrace();
      fail(e.getMessage());
    }

    List localDisjointWiths = null;


    /*
    // complete received objects immediately
    cycAccess.deferObjectCompletion = false;
    System.out.println("deferObjectCompletion = false");
    
    // trace should show the use of the CycConstantCache to avoid redundant server
    // accesses for the term name.
    // getLocalDisjointWith.
    try {
    CycConstant vegetableMatter =
    cycAccess.getKnownConstantByGuid("bd58c455-9c29-11b1-9dad-c379636f7270");
    localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    assertNotNull(localDisjointWiths);
    assertTrue(localDisjointWiths.toString().indexOf("AnimalBLO") > 0);
    localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    }
    catch (Throwable e) {
    fail(e.toString());
    }
     */

    // getLocalDisjointWith.
    localDisjointWiths = null;

    try {
      CycConstant vegetableMatter = cycAccess.getKnownConstantByGuid(VEGETABLE_MATTER_GUID_STRING);
      localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
      assertNotNull(localDisjointWiths);


      //System.out.println("localDisjointWiths.toString()");
      //assertTrue(localDisjointWiths.toString().indexOf("AnimalBLO") > 0);
      //System.out.println("localDisjointWiths.toString()");
      //assertTrue(localDisjointWiths.toString().indexOf("AnimalBLO") > 0);
      localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
      localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
      localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
    } catch (Throwable e) {
      fail(e.toString());
    }

    // makeUniqueCycConstant
    try {
      final String constantName = "MyConstant";
      CycConstant cycConstant1 = cycAccess.makeUniqueCycConstant(constantName);
      System.out.println(cycConstant1.cyclify());
      assertTrue(cycConstant1.getName().startsWith(constantName));
      CycConstant cycConstant2 = cycAccess.makeUniqueCycConstant(constantName);
      System.out.println(cycConstant2.cyclify());
      assertTrue(cycConstant2.getName().startsWith(constantName));
      assertTrue(!cycConstant1.getName().equals(cycConstant2.getName()));
      CycConstant cycConstant3 = cycAccess.makeUniqueCycConstant(constantName);
      System.out.println(cycConstant3.cyclify());
      assertTrue(cycConstant3.getName().startsWith(constantName));
      assertTrue(!cycConstant3.getName().equals(cycConstant1.getName()));
      assertTrue(!cycConstant3.getName().equals(cycConstant2.getName()));
    } catch (Throwable e) {
      fail(e.toString());
    }

    long endMilliseconds = System.currentTimeMillis();
    System.out.println("  " + (endMilliseconds - startMilliseconds) + " milliseconds");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.  This test case
   * specifically is used to test cfasl id versus guid constant encoding, and the eager obtaining
   * of constant names.
   */
  public void testBinaryCycAccess15() {
    System.out.println("\n**** testBinaryCycAccess 15 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      // backquote
      String command =
              "(identity " + "`(,(canonicalize-term \'(#$CollectionUnionFn (#$TheSet #$Tourist (#$GroupFn #$Tourist)))) ,#$ComputationalSystem))";
      Object result;
      try {
        //cycAccess.traceOn();
        result = cycAccess.converseObject(command);
        assertNotNull(result);
        assertTrue(result instanceof CycList);
        System.out.println("backquoted nart: " + ((CycList) result).cyclify());
        System.out.println("embedded obj class: " + ((CycList) result).first().getClass().toString());
        assertTrue(((CycList) result).first() instanceof CycNart);
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // getComment with CycNart
      CycNart nart = null;
      String comment = null;
      try {
        if (!cycAccess.isOpenCyc()) {
          nart = (CycNart) cycAccess.converseObject("(find-nart '(#$JuvenileFn #$Dog))");
          comment = cycAccess.getComment(nart);
          assertNotNull(comment);
          //assertEquals("", comment);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // newlines in strings

      try {
        command = "(nart-substitute \"first text line\nsecond text line\")";
        System.out.println("string with newlines: " + command);
        System.out.println(cycAccess.converseObject(command));
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      //cycAccess.traceOnDetailed();
      // getLocalDisjointWith.
      List localDisjointWiths = null;

      try {
        CycConstant vegetableMatter = cycAccess.getKnownConstantByGuid(VEGETABLE_MATTER_GUID_STRING);
        localDisjointWiths = cycAccess.getDisjointWiths(vegetableMatter);
        assertNotNull(localDisjointWiths);

        //assertTrue(localDisjointWiths.toString().indexOf("AnimalBLO") > 0);
      } catch (Throwable e) {
        fail(e.toString());
      }

      // ensure that constants have names
      try {
        String physicalDeviceGuidString = PHYSICAL_DEVICE_GUID_STRING;
        CycConstant physicalDevice = cycAccess.getKnownConstantByGuid(physicalDeviceGuidString);
        final CycList constants = cycAccess.getAllInstances(physicalDevice);
        if (constants.size() > 0 && constants.first() instanceof CycConstant) {
          assertNotNull(((CycConstant) constants.first()).name);
        }
        if (constants.size() > 1 && constants.second() instanceof CycConstant) {
          assertNotNull(((CycConstant) constants.second()).name);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      try {
        CycNart nart1 = cycAccess.getRandomNart();
        assertNotNull(nart1);
        assertNotNull(nart1.getFunctor());
        assertTrue(nart1.getFunctor() instanceof CycFort);
        assertNotNull(nart1.getArguments());
        assertTrue(nart1.getArguments() instanceof CycList);

        //System.out.println(nart1.cyclify());
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

      // Narts in a list.
      try {
        //cycAccess.traceOn();
        CycNart nart1 = cycAccess.getRandomNart();
        CycNart nart2 = new CycNart(nart1.toCycList());
        assertEquals(nart1, nart2);

        CycList nartList = new CycList();
        nartList.add(nart1);
        nartList.add(nart2);

        Object object = null;
        CycSymbol a = makeCycSymbol("a");
        cycAccess.setSymbolValue(a, nartList);

        object = cycAccess.getSymbolValue(a);
        assertNotNull(object);
        assertTrue(object instanceof CycList);

        CycList nartList1 = (CycList) object;
        Object element1 = nartList1.first();
        assertTrue((element1 instanceof CycNart) || (element1 instanceof CycList));

        if (element1 instanceof CycList) {
          element1 = CycNart.coerceToCycNart(element1);
        }

        CycNart nart3 = (CycNart) element1;
        assertNotNull(nart3.getFunctor());
        assertTrue(nart3.getFunctor() instanceof CycFort);
        assertNotNull(nart3.getArguments());
        assertTrue(nart3.getArguments() instanceof CycList);

        Object element2 = nartList1.second();
        assertTrue((element2 instanceof CycNart) || (element2 instanceof CycList));

        if (element2 instanceof CycList) {
          element2 = CycNart.coerceToCycNart(element2);
        }

        CycNart nart4 = (CycNart) element2;
        assertNotNull(nart4.getFunctor());
        assertTrue(nart4.getFunctor() instanceof CycFort);
        assertNotNull(nart4.getArguments());
        assertTrue(nart4.getArguments() instanceof CycList);

        assertEquals(nart1.cyclify(), nart3.cyclify());
        assertEquals(nart1.cyclify(), nart4.cyclify());
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 15 OK ****");
  }

  /**
   * Tests a portion of the CycAccess methods using the binary api connection.  This test case
   * specifically is used to test soap service handling of an xml response from Cyc.
   */
  public void testBinaryCycAccess16() {
    System.out.println("\n**** testBinaryCycAccess 16 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      //cycAccess.traceOnDetailed();
      try {
        if (!(cycAccess.isOpenCyc())) {
          List genls = null;
          CycConstant carAccident = null;
          carAccident = cycAccess.getKnownConstantByGuid(CAR_ACCIDENT_GUID_STRING);
          genls = cycAccess.getGenls(carAccident);
          assertNotNull(genls);
          assertTrue(genls instanceof CycList);

          Iterator iter = genls.iterator();

          while (iter.hasNext()) {
            Object obj = iter.next();
            assertTrue(obj instanceof CycFort);
          }

          List coExtensionals = null;
          coExtensionals = cycAccess.getCoExtensionals(carAccident);
          assertNotNull(coExtensionals);
          assertTrue(coExtensionals instanceof CycList);
          iter = coExtensionals.iterator();

          while (iter.hasNext()) {
            Object obj = iter.next();
            assertTrue(obj instanceof CycFort);
          }
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }

    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBinaryCycAccess 16 OK ****");
  }

  /**
   * Tests the api getting of gafs (Ground Atomic Formula).
   */
  public void testGetGafs() {
    System.out.println("\n**** testGetGafs ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      try {
        CycListParser parser = new CycListParser(cycAccess);
        CycList nart = parser.read("(#$RemotelyExploitableFn #$VulnerableToDTMLMethodExecution)");
        System.out.println("Nart: " + nart);

        CycList gafs = cycAccess.getGafs(CycNart.coerceToCycNart(nart), isa);
        assertTrue(gafs.size() > 0);
      } catch (Throwable e) {
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testGetGafs OK ****");
  }

  /**
   * Tests the getCycImageID() api method.
   */
  public void testGetCycImage() {
    System.out.println("\n**** testGetCycImage ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      try {
        if (!cycAccess.isOpenCyc()) {
          String id = cycAccess.getCycImageID();
          assertTrue(id != null);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testGetCycImage OK ****");
  }

  /**
   * Tests the ggetELCycTerm method.
   */
  public void testGetELCycTerm() {
    System.out.println("\n**** testGetELCycTerm ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }

      try {
        Object obj = cycAccess.getELCycTerm("(#$JuvenileFn #$Dog)");
        assertTrue(obj != null);
      } catch (Throwable e) {
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testGetELCycTerm OK ****");
  }

  /**
   * Tests the assertWithTranscriptAndBookkeeping method.
   */
  public void testAssertWithTranscriptAndBookkeeping() {
    System.out.println("\n**** testAssertWithTranscriptAndBookkeeping ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(StringUtils.getStringForException(e));
      }

      try {
        CycConstant cycAdministrator = cycAccess.getKnownConstantByName("CycAdministrator");
        cycAccess.setCyclist(cycAdministrator);
        String assertionString = "(#$isa #$CycAdministrator #$Person)";
        ELMt mt = universalVocabularyMt;
        cycAccess.assertWithTranscriptAndBookkeeping(assertionString, mt);
      } catch (Throwable e) {
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testAssertWithTranscriptAndBookkeeping OK ****");
  }

  /**
   * Tests the getArg2 method.
   */
  public void testGetArg2() {
    System.out.println("\n**** testGetArg2 ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(StringUtils.getStringForException(e));
      }

      try {
        CycFort cycAdministrator = cycAccess.getKnownConstantByName("CycAdministrator");
        Object obj = cycAccess.getArg2(isa, cycAdministrator);
        assertNotNull(obj);
        assertTrue(obj instanceof CycFort || obj instanceof CycList);
        if (!cycAccess.isOpenCyc()) {
          final String predName = "scientificName";
          final String termName = "Emu";
          final String mtName = "AllEnglishValidatedLexicalMicrotheoryPSC";
          obj = cycAccess.getArg2(predName, termName, mtName);
          System.out.println(obj);
          assertNotNull(obj);
          CycFort predicate = cycAccess.getKnownConstantByName(predName);
          CycFort arg1 = cycAccess.getKnownConstantByName(termName);
          CycFort mt = cycAccess.getKnownConstantByName(mtName);
          obj = cycAccess.getArg2(predicate, arg1, mt);
          System.out.println(obj);
          assertNotNull(obj);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testGetArg2 OK ****");
  }

  public void testBigCycList() {
    System.out.println("\n**** testBigCycList ****");

    CycAccess cycAccess = null;
    CycList cycList = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        System.out.println("\nException: " + e.getMessage());
        fail(e.toString());
      }
      cycList = new CycListParser(cycAccess).read(
              "(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
              + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)");
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testBigCycList OK (length = " + cycList.size() + ") ****");
  }

  public void testUnicodeCFASL() {
    System.out.println("\n**** testUnicodeCFASL ****");
    CFASLStringTest("abc", 15);
    CFASLStringTest("", 15);
    StringBuffer sb = new StringBuffer();
    sb.append("a");
    sb.append((char) 0x401);
    CFASLStringTest(sb.toString(), 53);
    System.out.println("**** testUnicodeCFASL OK ****");
  }

  private boolean CFASLStringTest(String str, int opcode) {
    try {
      ByteArrayOutputStream baos = new ByteArrayOutputStream(4096);
      CfaslOutputStream cos = new CfaslOutputStream(baos);
      cos.writeObject(str);
      cos.flush();
      byte[] ba = baos.toByteArray();
      if (ba == null || ba.length == 0) {
        fail("Null Byte Array Return");
      }
      //System.out.println("BA test: "+ba.length);
      //for(int i=0;i<ba.length;i++)
      //  System.out.println("ba check "+i+" "+Integer.toHexString(0xff & (int)ba[i]));
      assertEquals((int) ba[0], opcode);  // make sure opcode is correct
      ByteArrayInputStream bais = new ByteArrayInputStream(ba);

      CfaslInputStream cis = new CfaslInputStream(bais);
      Object obj = cis.readObject();
      assertTrue(obj instanceof String);
      String result = (String) obj;
      assertTrue(result.equals(str));
    } catch (IOException e) {
      fail("IOException CFASLStringTest for: " + str);
    }
    return true;

  }

  public void testHLIDGeneration() {
    System.out.println("\n**** testHLIDGeneration ****");
    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(e.toString());
      }
      HLIDGenerationTest(1, cycAccess);
      HLIDGenerationTest(0, cycAccess);
      HLIDGenerationTest(-1, cycAccess);
      //the following two tests are broken because of an apparent problem
      //in sending 1.0 over to Cyc via the API (it seems to come out on the
      //Cyc side as 1 instead of 1.0 or 1.0d0
//    HLIDGenerationTest(-1.0, cycAccess);
//    HLIDGenerationTest(1.0, cycAccess);
      HLIDGenerationTest(-1.1231231, cycAccess);
      HLIDGenerationTest(-12385341.1231231, cycAccess);

      HLIDGenerationTest("", cycAccess);
      HLIDGenerationTest("The quick brown fox jumps over the lazy dog", cycAccess);

      System.out.println("**** testHLIDGeneration OK ****");
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
  }

  private boolean HLIDGenerationTest(Object obj, CycAccess cyc) {
    try {
      String cmd = "(compact-hl-external-id-string "
              + DefaultCycObject.stringApiValue(obj) + ")";
      String cycId = cyc.converseString(cmd);
      String apiId = null;
      if (obj instanceof String) {
        apiId = CompactHLIDConverter.converter().toCompactHLId((String) obj);
        assertTrue(CompactHLIDConverter.converter().isStringCompactHLId(apiId));
      } else if (obj instanceof Number) {
        apiId = CompactHLIDConverter.converter().toCompactHLId((Number) obj);
        assertTrue(CompactHLIDConverter.converter().isNumberCompactHLId(apiId));
      }
      assertTrue(apiId.equals(cycId));
    } catch (Throwable e) {
      fail(e.toString());
    }
    return true;
  }

  public void testHLIDRoundTripConversion() {
    System.out.println("\n**** testHLIDRoundTripConversion ****");
    HLIDRoundTripConversionTest(1);
    HLIDRoundTripConversionTest(0);
    HLIDRoundTripConversionTest(-1);
    HLIDRoundTripConversionTest(-1.0);
    HLIDRoundTripConversionTest(1.0);
    HLIDRoundTripConversionTest(-1.1231231);
    HLIDRoundTripConversionTest(-12385341.1231231);

    HLIDRoundTripConversionTest("");
    HLIDRoundTripConversionTest("This is a string");

    System.out.println("**** testHLIDRoundTripConversion OK ****");
  }

  private boolean HLIDRoundTripConversionTest(Object obj) {
    try {
      String apiId = null;
      if (obj instanceof String) {
        apiId = CompactHLIDConverter.converter().toCompactHLId((String) obj);
      } else if (obj instanceof Number) {
        apiId = CompactHLIDConverter.converter().toCompactHLId((Number) obj);
      }
      Object roundTripObj = CompactHLIDConverter.converter().fromCompactHLId(apiId);
      assertTrue(obj.equals(roundTripObj));
    } catch (Throwable e) {
      fail(e.toString());
    }
    return true;
  }

  /**
   * Tests the CycLeaseManager.
   */
  public void testCycLeaseManager() {
    System.out.println("\n**** testCycLeaseManager ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
          cycAccess.getCycLeaseManager().addListener(this);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(StringUtils.getStringForException(e));
      }

      try {
        Thread.sleep(6000);
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
      if (cycAccess.getCycLeaseManager() != null) {
        cycAccess.getCycLeaseManager().removeAllListeners();
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testCycLeaseManager OK ****");
  }

  /**
   * Tests inference problem store reuse.
   */
  public void testInferenceProblemStoreReuse() {
    System.out.println("\n**** testInferenceProblemStoreReuse ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(StringUtils.getStringForException(e));
      }

      try {
        if (!cycAccess.isOpenCyc()) {
          final String inferenceProblemStoreName = "my-problem-store";
          cycAccess.initializeNamedInferenceProblemStore(inferenceProblemStoreName, null);
          CycFormulaSentence query = cycAccess.makeCycSentence("(#$objectFoundInLocation ?WHAT ?WHERE)");
          CycList variables = new CycList();
          variables.add(makeCycVariable("?WHAT"));
          variables.add(makeCycVariable("?WHERE"));
          InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
          CycConstant universeDataMt = cycAccess.getKnownConstantByGuid(UNIVERSE_DATA_MT_GUID_STRING);
          CycList response = cycAccess.queryVariables(variables, query, universeDataMt, queryProperties, inferenceProblemStoreName);
          assertNotNull(response);
          response = cycAccess.queryVariables(variables, query, universeDataMt, queryProperties, inferenceProblemStoreName);
          assertNotNull(response);
          response = cycAccess.queryVariables(variables, query, universeDataMt, queryProperties, inferenceProblemStoreName);
          assertNotNull(response);
          cycAccess.destroyInferenceProblemStoreByName(inferenceProblemStoreName);
        }
      } catch (Throwable e) {
        e.printStackTrace();
        fail(e.toString());
      }
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }
    System.out.println("**** testInferenceProblemStoreReuse OK ****");
  }

  /**
   * Tests inference problem store reuse.
   */
  public void testInvalidTerms() {
    System.out.println("\n**** testInvalidTerms ****");

    CycAccess cycAccess = null;
    try {
      try {
        if (connectionMode == LOCAL_CYC_CONNECTION) {
          cycAccess = new CycAccess(testHostName, testBasePort);
        } else if (connectionMode == SOAP_CYC_CONNECTION) {
          cycAccess = new CycAccess(endpointURL, testHostName, testBasePort);
        } else {
          fail("Invalid connection mode " + connectionMode);
        }
      } catch (Throwable e) {
        fail(StringUtils.getStringForException(e));
      }
      if (cycAccess.isOpenCyc()) {
        System.out.println("Can't test invalid terms on OpenCyc image.");
      } else {
        // invalid constant
        {
          final boolean invalidConstantCfaslWorks = false; // Until system 1.131015.
          if (invalidConstantCfaslWorks) {
            try {
              final String command = "(list \"a\" 1 #$Brazil (cfasl-invalid-constant) \"z\")";
              cycAccess.converseList(command);
              fail("Expected CycApiException not thrown.");
            } catch (CycApiException e) {
            } catch (IOException e) {
              fail(StringUtils.getStringForException(e));
            } catch (Throwable t) {
              fail(t.getLocalizedMessage());
            }
            try {
              final String command = "(list \"a\" 1 #$Brazil \"z\")";
              final CycList result = cycAccess.converseList(command);
              assertEquals(result.toString(), "(\"a\" 1 Brazil \"z\")");
            } catch (Throwable e) {
              fail(StringUtils.getStringForException(e));
            }
          }
        }

        // invalid nart
        try {
          final String command = "(list \"a\" 1 #$Brazil (cfasl-invalid-nart) \"z\")";
          final CycList result = cycAccess.converseList(command);
          fail("Expected CycApiException not thrown.");
        } catch (CycApiException e) {
        } catch (IOException e) {
          fail(StringUtils.getStringForException(e));
        }
        try {
          final String command = "(list \"a\" 1 #$Brazil \"z\")";
          final CycList result = cycAccess.converseList(command);
          assertEquals(result.toString(), "(\"a\" 1 Brazil \"z\")");
        } catch (Throwable e) {
          fail(StringUtils.getStringForException(e));
        }

        // invalid assertion
        try {
          final String command = "(list \"a\" 1 #$Brazil (create-sample-invalid-assertion) \"z\")";
          final CycList result = cycAccess.converseList(command);
          fail("Expected CycApiException not thrown.");
        } catch (CycApiException e) {
          //System.out.println(e.getMessage());
        } catch (IOException e) {
          fail(StringUtils.getStringForException(e));
        }
        try {
          final String command = "(list \"a\" 1 #$Brazil \"z\")";
          final CycList result = cycAccess.converseList(command);
          assertEquals(result.toString(), "(\"a\" 1 Brazil \"z\")");
        } catch (Throwable e) {
          fail(StringUtils.getStringForException(e));
        }
      }
    } catch (AssertionFailedError afe) {
      throw afe;
    } catch (Throwable t) {
      System.out.println("Caught " + t);
    } finally {
      if (cycAccess != null) {
        cycAccess.close();
        cycAccess = null;
      }
    }

    System.out.println("**** testInvalidTerms OK ****");
  }

  /**
   * Main method in case tracing is prefered over running the JUnit GUI.
   *
   * @param args the list of command line args (unused)
   */
  public static void main(String[] args) {
    System.out.println("Starting.");
    if (args.length > 0 && args[0] != null) {
      testHostName = args[0];
    }
    if (args.length > 1 && args[1] != null) {
      testBasePort = Integer.parseInt(args[1]);
    }
    try {
      TestRunner.run(suite());
    } catch (Throwable e) {
      e.printStackTrace();
    } finally {
      System.out.println("Finished.");
    }
    // kill all threads when exiting.
    System.exit(0);
  }
}
