package org.opencyc.api;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import java.lang.reflect.Method;

import java.math.BigInteger;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import java.util.HashMap;
import java.util.Map;

import org.opencyc.cycobject.ByteArray;
import org.opencyc.cycobject.CycAssertion;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycNart;
import org.opencyc.cycobject.CycObject;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.CycVariable;
import org.opencyc.cycobject.Guid;
import org.opencyc.util.Log;

/**
 * A CFASL translating input stream.  All Java-native types which have logical sublisp equivalents
 * are translated automatically by this stream.  Classes implementing the CfaslTranslatingObject
 * interface are created using thier readObject() method.  Other CYC objects, such as
 * binding-lists and formulas, must be explicitly coerced using their static constructors.
 * 
 * @version $Id: CfaslInputStream.java 131020 2010-05-24 21:31:00Z baxter $
 * @author Stephen L. Reed <p><p><p><p><p>
 */
public class CfaslInputStream extends BufferedInputStream {

  /** No api trace. */
  public static final int API_TRACE_NONE = 0;
  /** Message-level api trace. */
  public static final int API_TRACE_MESSAGES = 1;
  /** Detailed api trace. */
  public static final int API_TRACE_DETAILED = 2;
  /** Parameter that, when true, causes a trace of the messages to and from the server. */
  public int trace = API_TRACE_NONE;
  /**
   * Parameter that when set true, causes CFASL object errors to be reported back as strings the
   * caller.
   */
  public boolean reportCfaslErrors = false;
  /** CFASL code */
  protected static final int CFASL_IMMEDIATE_FIXNUM_CUTOFF = 128;
  /** CFASL code */
  protected static final int CFASL_IMMEDIATE_FIXNUM_OFFSET = 256 - CFASL_IMMEDIATE_FIXNUM_CUTOFF;
  /** CFASL code */
  protected static final int CFASL_P_8BIT_INT = 0;
  /** CFASL code */
  protected static final int CFASL_N_8BIT_INT = 1;
  /** CFASL code */
  protected static final int CFASL_P_16BIT_INT = 2;
  /** CFASL code */
  protected static final int CFASL_N_16BIT_INT = 3;
  /** CFASL code */
  protected static final int CFASL_P_24BIT_INT = 4;
  /** CFASL code */
  protected static final int CFASL_N_24BIT_INT = 5;
  /** CFASL code */
  protected static final int CFASL_P_32BIT_INT = 6;
  /** DCFASL code */
  protected static final int CFASL_N_32BIT_INT = 7;
  /** DCFASL code */
  protected static final int CFASL_P_FLOAT = 8;
  /** CFASL code */
  protected static final int CFASL_N_FLOAT = 9;
  /** CFASL code */
  protected static final int CFASL_KEYWORD = 10;
  /** CFASL code */
  protected static final int CFASL_SYMBOL = 11;
  /** CFASL code */
  protected static final int CFASL_NIL = 12;
  /** CFASL code */
  protected static final int CFASL_LIST = 13;
  /** CFASL code */
  protected static final int CFASL_VECTOR = 14;
  /** CFASL code */
  protected static final int CFASL_STRING = 15;
  /** CFASL code */
  protected static final int CFASL_CHARACTER = 16;
  /** CFASL code */
  protected static final int CFASL_DOTTED = 17;
  /** CFASL code */
  protected static final int CFASL_HASHTABLE = 18;
  /** CFASL code */
  protected static final int CFASL_BTREE_LOW_HIGH = 19;
  /** CFASL code */
  protected static final int CFASL_BTREE_LOW = 20;
  /** CFASL code */
  protected static final int CFASL_BTREE_HIGH = 21;
  /** CFASL code */
  protected static final int CFASL_BTREE_LEAF = 22;
  /** CFASL code */
  protected static final int CFASL_P_BIGNUM = 23;
  /** CFASL code */
  protected static final int CFASL_N_BIGNUM = 24;
  /** CFASL code */
  protected static final int CFASL_LEGACY_GUID = 25;
  /** CFASL code */
  protected static final int CFASL_GUID = 43;
  /** CFASL code */
  protected static final int CFASL_BYTE_VECTOR = 26;
  /** CFASL code */
  protected static final int CFASL_CONSTANT = 30;
  /** CFASL code */
  protected static final int CFASL_NART = 31;
  /** CFASL code */
  protected static final int CFASL_COMPLETE_CONSTANT = 32;
  /** CFASL code */
  protected static final int CFASL_ASSERTION = 33;
  /** CFASL code */
  protected static final int CFASL_ASSERTION_SHELL = 34;
  /** CFASL code */
  protected static final int CFASL_ASSERTION_DEF = 35;
  /** CFASL code */
  protected static final int CFASL_SOURCE = 36;
  /** CFASL code */
  protected static final int CFASL_SOURCE_DEF = 37;
  /** CFASL code */
  protected static final int CFASL_AXIOM = 38;
  /** CFASL code */
  protected static final int CFASL_AXIOM_DEF = 39;
  /** CFASL code */
  protected static final int CFASL_VARIABLE = 40;
  /** CFASL code */
  protected static final int CFASL_INDEX = 41;
  /** CFASL code */
  protected static final int CFASL_COMPLETE_VARIABLE = 42;
  /** CFASL code */
  protected static final int CFASL_SPECIAL_OBJECT = 50;
  /** CFASL code */
  protected static final int CFASL_EXTERNALIZATION = 51;
  /** CFASL code */
  protected static final int CFASL_UNICODE_CHAR = 52;
  /** CFASL code */
  protected static final int CFASL_UNICODE_STRING = 53;
  /** CFASL code */
  protected static final int CFASL_DICTIONARY = 64;
  /** CFASL code */
  protected static final int CFASL_SERVER_DEATH = -1;
  /** CFASL code */
  protected static final int DEFAULT_READ_LIMIT = 8192;
  static private final Map<Integer, String> CFASL_OPCODE_DESCRIPTIONS = new HashMap<Integer, String>();
  /** indicator that the input contains something invalid, for example an invalid constant */
  protected boolean isInvalidObject = false;

  /**
   * Initializes the opcode descriptions used in trace output.
   */
  static {
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_IMMEDIATE_FIXNUM_CUTOFF, "CFASL_IMMEDIATE_FIXNUM_CUTOFF");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_IMMEDIATE_FIXNUM_OFFSET, "CFASL_IMMEDIATE_FIXNUM_OFFSET");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_8BIT_INT, "CFASL_P_8BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_8BIT_INT, "CFASL_N_8BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_16BIT_INT, "CFASL_P_16BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_16BIT_INT, "CFASL_N_16BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_24BIT_INT, "CFASL_P_24BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_24BIT_INT, "CFASL_N_24BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_32BIT_INT, "CFASL_P_32BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_32BIT_INT, "CFASL_N_32BIT_INT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_FLOAT, "CFASL_P_FLOAT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_FLOAT, "CFASL_N_FLOAT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_KEYWORD, "CFASL_KEYWORD");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_SYMBOL, "CFASL_SYMBOL");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_NIL, "CFASL_NIL");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_LIST, "CFASL_LIST");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_VECTOR, "CFASL_VECTOR");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_STRING, "CFASL_STRING");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_CHARACTER, "CFASL_CHARACTER");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_DOTTED, "CFASL_DOTTED");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_HASHTABLE, "CFASL_HASHTABLE");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_BTREE_LOW_HIGH, "CFASL_BTREE_LOW_HIGH");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_BTREE_LOW, "CFASL_BTREE_LOW");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_BTREE_HIGH, "CFASL_BTREE_HIGH");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_BTREE_LEAF, "CFASL_BTREE_LEAF");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_P_BIGNUM, "CFASL_P_BIGNUM");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_N_BIGNUM, "CFASL_N_BIGNUM");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_LEGACY_GUID, "CFASL_LEGACY_GUID");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_GUID, "CFASL_GUID");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_BYTE_VECTOR, "CFASL_BYTE_VECTOR");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_CONSTANT, "CFASL_CONSTANT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_NART, "CFASL_NART");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_COMPLETE_CONSTANT, "CFASL_COMPLETE_CONSTANT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_ASSERTION, "CFASL_ASSERTION");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_ASSERTION_SHELL, "CFASL_ASSERTION_SHELL");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_ASSERTION_DEF, "CFASL_ASSERTION_DEF");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_SOURCE, "CFASL_SOURCE");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_SOURCE_DEF, "CFASL_SOURCE_DEF");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_AXIOM, "CFASL_AXIOM");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_AXIOM_DEF, "CFASL_AXIOM_DEF");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_VARIABLE, "CFASL_VARIABLE");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_INDEX, "CFASL_INDEX");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_COMPLETE_VARIABLE, "CFASL_COMPLETE_VARIABLE");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_SPECIAL_OBJECT, "CFASL_SPECIAL_OBJECT");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_EXTERNALIZATION, "CFASL_EXTERNALIZATION");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_UNICODE_CHAR, "CFASL_UNICODE_CHAR");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_UNICODE_STRING, "CFASL_UNICODE_STRING");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_DICTIONARY, "CFASL_DICTIONARY");
    CFASL_OPCODE_DESCRIPTIONS.put(CFASL_SERVER_DEATH, "CFASL_SERVER_DEATH");
  }

  /**
   * Creates a new CfaslInputStream to read data from the specified underlying input stream.
   * 
   * @param in the underlying input stream.
   */
  public CfaslInputStream(InputStream in) {
    super(in, DEFAULT_READ_LIMIT);

    if (Log.current == null) {
      Log.makeLog("cfasl.log");
    }
  }

  /** Gets the indicator that the input contains something invalid, for example an invalid constant.
   *
   * @return the indicator that the input contains something invalid, for example an invalid constant
   */
  public boolean isInvalidObject() {
    return isInvalidObject;
  }

  /** Resets the indicator that the input contains something invalid, for example an invalid constant.  */
  public void resetIsInvalidObject() {
    isInvalidObject = false;
  }

  /**
   * Reads an Object from this CfaslInputStream.  Basic Java types are wrapped as appropriate (e.g.
   * ints become Integer objects).  New constants are missing name and GUID values and will be
   * completed by the caller to avoid recursion within the api call.
   * 
   * @return the object read from the binary OpenCyc input stream
   * 
   * @throws IOException if a communications error occurs
   */
  public Object readObject()
          throws IOException {
    final int cfaslOpcode = readCfaslOpcode();
    Object o = null;
    if (trace == API_TRACE_DETAILED) {
      debugNote("reading opcode = " + cfaslOpcode + " ("
              + CFASL_OPCODE_DESCRIPTIONS.get(cfaslOpcode) + ")");
    }
    if (cfaslOpcode >= CFASL_IMMEDIATE_FIXNUM_OFFSET) {
      o = cfaslOpcode - CFASL_IMMEDIATE_FIXNUM_OFFSET;
      if (trace == API_TRACE_DETAILED) {
        debugNote("Reading Immediate Fixnum: " + o);
      }
    }
    if (o == null) {
      o = maybeReadNumber(cfaslOpcode);
    }
    if (o == null) {
      o = maybeReadSymbol(cfaslOpcode);
    }
    if (o == null) {
      o = maybeReadSequence(cfaslOpcode);
    }
    if (o == null) {
      o = maybeReadOther(cfaslOpcode);
    }

    if (trace == API_TRACE_DETAILED) {
      try {
        // If object o understands the safeToString method, then use it.
        Method safeToString = o.getClass().getMethod("safeToString");
        debugNote("readObject = " + safeToString.invoke(o) + " (" + o.getClass() + ")");
      } catch (Exception e) {
        debugNote("readObject = " + o + " (" + o.getClass() + ")");
      }
    }

    return o;
  }

  /**
   * Reports the unhandled cfasl opcode or throws an exception.
   * 
   * @param cfaslOpcode the unhandled cfasl opcode
   * 
   * @return the unhandled cfasl opcode
   * 
   * @throws CfaslInputStreamClosedException if the socket connection is closed by the peer
   * @throws RuntimeException if the error is not logged and ignored
   */
  protected Object reportUnhandledCfaslOpcode(int cfaslOpcode) {
    String errorMessage;

    if (cfaslOpcode == -1) {
      throw new CfaslInputStreamClosedException("Cfasl connection closed by peer because of bad opcode: " + cfaslOpcode);
    } else {
      errorMessage = "Unknown cfasl opcode: " + cfaslOpcode;
    }

    if (reportCfaslErrors) {
      debugWarn(errorMessage);

      return Integer.toString(cfaslOpcode);
    } else {
      //TODO create a new exception class for this case.
      throw new RuntimeException(errorMessage);
    }
  }

  /**
   * Reads an char from this CfaslInputStream.  If the next item on the stream is not a char, throw
   * an exception, and leave that object on the input stream.
   * 
   * @return the character read
   * 
   * @throws IOException if a communications error occurs
   * @throws RuntimeException if an unexpected cfasl opcode occurs
   */
  public char readChar()
          throws IOException {
    mark(DEFAULT_READ_LIMIT);
    int cfaslOpcode = read();
    if (cfaslOpcode == CFASL_CHARACTER) {
      return (char) read();
    }
    reset();
    throw new RuntimeException("Expected a char but received opCode=" + cfaslOpcode);
  }

  /**
   * Reads a double from this CfaslInputStream.  If the next item on the stream is not a double,
   * throw an exception, and leave that object on the input stream.
   * 
   * @return the double read
   * 
   * @throws IOException if a communications error occurs
   * @throws RuntimeException if an unexpected cfasl opcode occurs
   */
  public double readDouble()
          throws IOException {
    mark(DEFAULT_READ_LIMIT);
    int cfaslOpcode = read();
    switch (cfaslOpcode) {
      case CFASL_P_FLOAT:
        return readFloatBody(false);
      case CFASL_N_FLOAT:
        return readFloatBody(true);
      default:
        reset();
        throw new RuntimeException("Expected a double but received OpCode=" + cfaslOpcode);
    }
  }

  /**
   * Reads an int from this CfaslInputStream.  If the next item on the stream is not an int, throw
   * an exception, and leave that object on the input stream.  Bignum ints are not allowed.
   * 
   * @return the int read
   * 
   * @throws IOException if a communications error occurs
   * @throws RuntimeException if an unexpected cfasl opcode occurs
   */
  public int readInt()
          throws IOException {
    mark(DEFAULT_READ_LIMIT);

    int cfaslOpcode = read();

    if (cfaslOpcode >= CFASL_IMMEDIATE_FIXNUM_OFFSET) {
      return cfaslOpcode - CFASL_IMMEDIATE_FIXNUM_OFFSET;
    } else {
      switch (cfaslOpcode) {
        case CFASL_P_8BIT_INT:
          return readFixnumBody(1, false);
        case CFASL_N_8BIT_INT:
          return readFixnumBody(1, true);
        case CFASL_P_16BIT_INT:
          return readFixnumBody(2, false);
        case CFASL_N_16BIT_INT:
          return readFixnumBody(2, true);
        case CFASL_P_24BIT_INT:
          return readFixnumBody(3, false);
        case CFASL_N_24BIT_INT:
          return readFixnumBody(3, true);
        case CFASL_P_32BIT_INT:
          return readFixnumBody(4, false);
        case CFASL_N_32BIT_INT:
          return readFixnumBody(4, true);
        default:
          reset();
          throw new RuntimeException("Expected an int but received OpCode=" + cfaslOpcode);
      }
    }
  }

  private int readCfaslOpcode() throws IOException {
    if (trace == API_TRACE_DETAILED) {
      debugNote("Reading CFASL opcode.");
    }
    int cfaslOpcode = read();
    if (trace == API_TRACE_DETAILED) {
      debugNote("Read CFASL opcode: " + cfaslOpcode);
    }
    if (cfaslOpcode == CFASL_EXTERNALIZATION) {
      if (trace == API_TRACE_DETAILED) {
        debugNote("reading opcode = " + cfaslOpcode + " (" + CFASL_OPCODE_DESCRIPTIONS.get(cfaslOpcode) + ")");
      }
      cfaslOpcode = read();
    }
    return cfaslOpcode;
  }

  /**
   * Reads the body of a CFASL Fixnum (everything but the opcode) from this CFASL input stream.
   * 
   * @param nBytes  The number of bytes to read
   * @param shouldBeNegative    true iff the Fixnum is negative
   * 
   * @return an int holding the CFASL Fixnum read in
   * 
   * @throws IOException if a communications error occurs
   * @throws ArithmeticException if nBytes > 4 or if the integer read in does not fit into a signed
   *         32 bit integer (i.e. the sign bit is being used for magnitude).
   */
  private int readFixnumBody(int nBytes, final boolean shouldBeNegative)
          throws IOException {
    if (trace == API_TRACE_DETAILED) {
      debugNote("readFixnumBody isNegative=" + shouldBeNegative + " length=" + nBytes);
    }

    if (nBytes > 4) {
      throw new ArithmeticException("Cannot fit " + nBytes + " bytes into an int");
    }

    int num = 0;

    for (int i = 0; i < nBytes; i++) {
      int j = read();

      if (trace == API_TRACE_DETAILED) {
        debugNote("\t" + j);
      }

      num |= (j << (8 * i));
    }

    // num should always be positive here.  Negatives indicate overflows.
    if (num < 0) {
      throw new ArithmeticException("Overflow: " + ((long) num & 0xFFFFFFFFL)
              + " does not fit into an int");
    }

    return (shouldBeNegative) ? (-num) : num;

  }

  /**
   * Reads the body of a CFASL Bignum (everything but the opcode) off of this CFASL input stream.
   * 
   * @param shouldBeNegative    true iff the Bignum should be negative
   * 
   * @return a BigInteger holding the CFASL Bignum read in
   * 
   * @throws IOException if a communications error occurs
   */
  private BigInteger readBignumBody(final boolean shouldBeNegative)
          throws IOException {
    int length = readInt();
    if (trace == API_TRACE_DETAILED) {
      debugNote("readBignumBody shouldBeNegative=" + shouldBeNegative + " length=" + length);
    }
    byte[] b = new byte[length];
    for (int i = length - 1; i >= 0; i--) {
      int j = readInt();
      if (trace == API_TRACE_DETAILED) {
        debugNote("\t" + j);
      }
      b[i] = (byte) j;
    }
    return new BigInteger((shouldBeNegative) ? -1 : 1, b);
  }

  /**
   * Reads the body of a CFASL Float (everything but the opcode) off of this CFASL input stream.
   * 
   * @param shouldBeNegative    true iff the Float should be negative
   * 
   * @return a double holding the CFASL Float read in
   * 
   * @throws IOException if a communications error occurs
   * @throws ArithmeticException if significand cannot fit into a 64 bit signed long int
   */
  private double readFloatBody(final boolean shouldBeNegative)
          throws IOException {
    long signif;
    long exp;
    if (trace == API_TRACE_DETAILED) {
      debugNote("readFloatBody shouldBeNegative=" + shouldBeNegative);
    }
    Object obj = readObject();
    if (obj instanceof BigInteger) {
      BigInteger bi = (BigInteger) obj;
      if (bi.bitCount() < 64) {
        signif = bi.longValue();
      } else {
        throw new ArithmeticException("Overflow reading significand of float");
      }
    } else {
      signif = ((Number) obj).longValue();
    }
    exp = readInt();
    if (trace == API_TRACE_DETAILED) {
      debugNote("readFloatBody shouldBeNegative=" + shouldBeNegative
              + " signif=" + signif + " exp= " + exp);
    }
    final Double absoluteValue = (double) signif * Math.pow(2.0, exp);
    return (shouldBeNegative) ? (-absoluteValue) : absoluteValue;
  }

  /**
   * Reads the body of a keyword Symbol from the CfaslInputStream.  The CFASL opcode has already
   * been read in at this point, so we only read in what follows.
   * 
   * @return the keyword <tt>CycSymbol</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public CycSymbol readKeyword()
          throws IOException {
    String keywordString = (String) readObject();

    if (!(keywordString.startsWith(":"))) {
      keywordString = ":" + keywordString;
    }

    return CycObjectFactory.makeCycSymbol(keywordString);
  }

  /**
   * Reads the body of a Symbol or EL variable from the CfaslInputStream.  The CFASL opcode has
   * already been read in at this point, so we only read in what follows.
   * 
   * @return the <tt>CycSymbol</tt> or EL <tt>CycVariable</tt>
   * 
   * @throws IOException if a communications error occurs
   */
  public Object readSymbol()
          throws IOException {
    if (trace >= API_TRACE_DETAILED) {
      debugNote("About to read symbol name.");
    }
    Object response = readObject();
    if (!(response instanceof String)) {
      throw new CycApiException("Expecting  a String, got: " + response.getClass() + " for object: " + response);
    }
    String name = (String) response;

    if (name.startsWith("?")) {
      return CycObjectFactory.makeCycVariable(name);
    } else {
      return CycObjectFactory.makeCycSymbol(name);
    }
  }

  /**
   * Reads the body of a Guid from the CfaslInputStream.  The CFASL opcode has already been read in
   * at this point, so we only read in what follows.
   * 
   * @return the <tt>Guid</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public Guid readLegacyGuid()
          throws IOException {
    Guid guid = CycObjectFactory.makeGuid((String) readObject());

    if (trace == API_TRACE_DETAILED) {
      debugNote("readLegacyGuid: " + guid);
    }

    return guid;
  }

  /**
   * Reads the body of a Guid from the CfaslInputStream.  The CFASL opcode has already been read in
   * at this point, so we only read in what follows.
   * 
   * @return the <tt>Guid</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public Guid readGuid()
          throws IOException {
    final byte[] data = new byte[16]; // @todo resource this
    for (int i = 0; i < 16; i++) {
      final int currByte = read();
      if (currByte == -1) {
        throw new RuntimeException("Illegal end of stream");
      }
      data[i] = (byte) currByte;
    }

    final Guid guid = CycObjectFactory.makeGuid(data);
    if (trace == API_TRACE_DETAILED) {
      debugNote("readGuid: " + guid);
    }
    return guid;
  }

  /**
   * Reads the body of a Unicode Character from the CfaslInputStream.  
   * The CFASL opcode has already been read in
   * at this point, so we only read in what follows.
   * 
   * @return the <tt>Integer</tt> of the unicode value read
   * 
   * @throws IOException if a communications error occurs
   */
  public Integer readUnicodeChar()
          throws IOException {

    int off = 0;
    int len = readInt();
    byte[] s = new byte[len];


    while (off < len) {
      off += read(s, off, len - off);
    }

    String charString = new String(s, "UTF-8");
    int retval = (int) charString.charAt(0);
    // NOTE: When we upgrade to java 1.5 change the above line to 
    //     int retval = charString.codePointAt(0);
    if (trace == API_TRACE_DETAILED) {
      debugNote("readUnicodeChar: 0x" + Integer.toHexString(retval));
    }

    return new Integer(retval);
  }

  /**
   * Reads the body of a Unicode String from the CfaslInputStream.  
   * The CFASL opcode has already been read in
   * at this point, so we only read in what follows.
   * 
   * @return the <tt>String</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public String readUnicodeString()
          throws IOException {
    int off = 0;
    int len = readInt();
    byte[] s = new byte[len];


    while (off < len) {
      off += read(s, off, len - off);
    }

    String retval = new String(s,
            "UTF-8");
    if (trace == API_TRACE_DETAILED) {
      debugNote("readUnicodeString: " + retval);
    }

    return retval;
  }

  /**
   * Reads a byte vector from the CfaslInputStream. The CFASL opcode has already been read in at
   * this point, so we only read in what follows.
   * 
   * @return the <tt>ByteArray</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public ByteArray readByteArray()
          throws IOException {
    int off = 0;
    int len = readInt();
    byte[] bytes = new byte[len];

    while (off < len) {
      off += read(bytes, off, len - off);
    }

    return new ByteArray(bytes);
  }

  /**
   * Reads a list from the CfaslInputStream.  The CFASL opcode has already been read in at this
   * point, so we only read in what follows.
   * 
   * @return the <tt>CycList</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public CycList readCycList()
          throws IOException {
    int size = readInt();

    if (trace == API_TRACE_DETAILED) {
      debugNote("readCycList.size: " + size);
    }

    CycList cycList = new CycList(size);

    for (int i = 0; i < size; i++) {
      cycList.add(readObject());
    }

    if (trace == API_TRACE_DETAILED) {
      debugNote("readCycList.readObject: " + cycList.toString());
    }

    return cycList;
  }

  /**
   * Reads a dotted list from the CfaslInputStream.  The CFASL opcode has already been read in at
   * this point, so we only read in what follows.
   * 
   * @return the <tt>CycList</tt> read
   * 
   * @throws IOException if a communications error occurs
   */
  public CycList readCons()
          throws IOException {
    int size = readInt();

    if (trace == API_TRACE_DETAILED) {
      debugNote("readCons.size: " + size);
    }

    CycList cycList = new CycList();

    //for (int i = 0; i < (size - 1); i++) {
    for (int i = 0; i < size; i++) {
      Object consObject = readObject();

      if (trace == API_TRACE_DETAILED) {
        if (consObject instanceof CycFort) {
          debugNote("readCons.consObject: " + ((CycFort) consObject).toString());
        } else {
          debugNote("readCons.consObject: " + consObject);
        }
      }

      cycList.add(consObject);
    }

    Object cdrObject = readObject();

    if (trace == API_TRACE_DETAILED) {
      try {
        // If element understands the safeToString method, then use it.
        final Method safeToString = cdrObject.getClass().getMethod("safeToString");
        debugNote("readCons.cdrObject: " + safeToString.invoke(cdrObject));
      } catch (Exception e) {
        debugNote("readCons.cdrObject: " + cdrObject.toString());
      }
    }
    cycList.setDottedElement(cdrObject);

    if (trace == API_TRACE_DETAILED) {
      debugNote("readCons.readCons: " + cycList.toString());
    }

    return cycList;
  }

  /**
   * Reads a complete constant from a CfaslInputStream.
   * 
   * @return an complete <tt>CycConstant</tt> having the input guid and name
   * 
   * @throws IOException if a communications error occurs
   * @throws RuntimeException if an unexpected constant id type occurs
   */
  public CycConstant readCompleteConstant()
          throws IOException {
    CycConstant cycConstant = null;
    Object idObject = readObject();

    if (idObject instanceof Guid) {
      final Guid guid = (Guid) idObject;
      final String name = (String) readObject();
      cycConstant = CycObjectFactory.getCycConstantCacheByGuid(guid);
      if (cycConstant == null) {
        cycConstant = new CycConstant(name, guid);
        CycObjectFactory.addCycConstantCache(cycConstant);
      }
    } else if ((idObject instanceof CycSymbol)
            && (idObject.equals(CycObjectFactory.makeCycSymbol(":FREE")))) {
      cycConstant = CycObjectFactory.FREE_CONSTANT;
    } else {
      // ignore the name, which is expected to be blank
      readObject();
      cycConstant = CycObjectFactory.INVALID_CONSTANT;
      isInvalidObject = true;
    }

    if (trace == API_TRACE_DETAILED) {
      debugNote("readConstant: " + cycConstant.toString());
    }

    return cycConstant;
  }

  /**
   * Reads a variable from the CfaslInputStream.
   * 
   * @return an complete <tt>CycVariable</tt> having the name
   * 
   * @throws IOException if a communications error occurs
   */
  public CycVariable readCompleteVariable()
          throws IOException {
    final Integer hlVariableId = (Integer) readObject();
    final String name = (String) readObject();
    CycVariable cycVariable = new CycVariable(name, hlVariableId);
    if (trace == API_TRACE_DETAILED) {
      debugNote("readVariable: " + cycVariable.toString());
    }

    return cycVariable;
  }

  /**
   * Reads a NART from a CfaslInputStream.
   * 
   * @return a the CycNart having the input HL Formula or having the input id, or NIL if the nart is invalid
   * 
   * @throws IOException if a communications error occurs
   */
  public CycObject readNart()
          throws IOException {
    CycNart cycNart = null;
    int cfaslOpcode = read();

    if (cfaslOpcode == CFASL_NIL) {
      cycNart = CycObjectFactory.INVALID_NART;
      isInvalidObject = true;
    } else if (cfaslOpcode != CFASL_LIST) {
      if (cfaslOpcode == CFASL_SYMBOL) {
        String name = (String) readObject();
        System.err.println("readNart, symbol=" + name);
      }
      throw new RuntimeException("reading nart, expected a list, found " + cfaslOpcode);
    } else {
      cycNart = new CycNart(readCycList());
    }

    if (trace == API_TRACE_DETAILED) {
      debugNote("readNart: " + cycNart.toString());
    }

    return cycNart;
  }

  /**
   * Reads an assertion from a CfaslInputStream.
   * 
   * @return an incomplete <tt>CycAssertion</tt> having the input id
   * 
   * @throws IOException if a communications error occurs
   */
  public CycAssertion readAssertion()
          throws IOException {
    CycList formula = null;
    Object formulaObject = null;
    CycAssertion cycAssertion = null;
    formulaObject = readObject();
    if (formulaObject.toString().equals("NIL")) {
      // bypass invalid assertion mt
      readObject();
      cycAssertion = CycObjectFactory.INVALID_ASSERTION;
      isInvalidObject = true;
    } else {
      try {
        formula = (CycList) formulaObject;
        CycObject mt = (CycObject) readObject();
        cycAssertion = new CycAssertion(formula, mt);
      } catch (ClassCastException e) {
        System.err.println("formulaObject " + formulaObject.toString() + "(" + formulaObject.getClass().getName() + ")");
      }
    }
    if (trace == API_TRACE_DETAILED) {
      debugNote("readAssertion: " + cycAssertion.toString());
    }

    return cycAssertion;
  }

  private Object maybeReadNumber(int cfaslOpcode) throws IOException {
    switch (cfaslOpcode) {
      case CFASL_P_8BIT_INT:
        return readFixnumBody(1, false);
      case CFASL_N_8BIT_INT:
        return readFixnumBody(1, true);
      case CFASL_P_16BIT_INT:
        return readFixnumBody(2, false);
      case CFASL_N_16BIT_INT:
        return readFixnumBody(2, true);
      case CFASL_P_24BIT_INT:
        return readFixnumBody(3, false);
      case CFASL_N_24BIT_INT:
        return readFixnumBody(3, true);
      case CFASL_P_32BIT_INT:
        return readFixnumBody(4, false);
      case CFASL_N_32BIT_INT:
        return readFixnumBody(4, true);
      case CFASL_P_FLOAT:
        return readFloatBody(false);
      case CFASL_N_FLOAT:
        return readFloatBody(true);
      case CFASL_P_BIGNUM:
        return readBignumBody(false);
      case CFASL_N_BIGNUM:
        return readBignumBody(true);
      default:
        return null;
    }
  }

  private Object maybeReadSymbol(int cfaslOpcode) throws IOException {
    switch (cfaslOpcode) {
      case CFASL_KEYWORD:
        // Keywords can be distinguished from Symbols by internal evidence
        return readKeyword();
      case CFASL_SYMBOL:
        return readSymbol();
      case CFASL_NIL:
        return CycObjectFactory.nil;
      default:
        return null;
    }
  }

  private Object maybeReadSequence(int cfaslOpcode) throws IOException {
    switch (cfaslOpcode) {
      case CFASL_LIST:
        return readCycList();
      case CFASL_DOTTED:
        return readCons();
      case CFASL_VECTOR:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_STRING:
        int off = 0;
        int len = readInt();
        byte[] s = new byte[len];
        while (off < len) {
          off += read(s, off, len - off);
        }
        return new String(s, "UTF-8");
      default:
        return null;
    }
  }

  private Object maybeReadOther(int cfaslOpcode) throws IOException {
    switch (cfaslOpcode) {
      case CFASL_CHARACTER:
        return new Character((char) read());
      case CFASL_HASHTABLE:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_BTREE_LOW_HIGH:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_BTREE_LOW:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_BTREE_HIGH:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_BTREE_LEAF:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_LEGACY_GUID:
        return readLegacyGuid();
      case CFASL_GUID:
        return readGuid();
      case CFASL_UNICODE_STRING:
        return readUnicodeString();
      case CFASL_UNICODE_CHAR:
        return readUnicodeChar();
      case CFASL_BYTE_VECTOR:
        return readByteArray();
      case CFASL_CONSTANT:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_NART:
        return readNart();
      case CFASL_COMPLETE_CONSTANT:
        return readCompleteConstant();
      case CFASL_ASSERTION:
        return readAssertion();
      case CFASL_ASSERTION_SHELL:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_ASSERTION_DEF:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_SOURCE:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_SOURCE_DEF:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_AXIOM:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_AXIOM_DEF:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_VARIABLE:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_INDEX:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_COMPLETE_VARIABLE:
        return readCompleteVariable();
      case CFASL_SPECIAL_OBJECT:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_DICTIONARY:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      case CFASL_SERVER_DEATH:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
      default:
        return reportUnhandledCfaslOpcode(cfaslOpcode);
    }
  }

  static private void debugNote(String string) {
    //Log.current.println(string);
    Debug.maybeDebugNote(string, true);
  }

  static private void debugWarn(String string) {
    //Log.current.println(string);
    Debug.maybeDebugWarn(string, true);
  }

  static private class Debug {

    private static boolean DEBUG_ALL = false;
    private static DateFormat df = null;
    private static Date date = null;

    private static synchronized Date getDate() {
      if (date == null) {
        date = new Date();
      } else {
        date.setTime(System.currentTimeMillis());
      }
      return date;
    }

    private static synchronized DateFormat getDf() {
      if (df == null) {
        df = new SimpleDateFormat("HH:mm:ss.SSS");
      }
      return df;
    }

    private static String getTimestamp() {
      final String timestamp = getDf().format(getDate());
      //final String timestamp = ((Long) System.currentTimeMillis()).toString().substring(8);
      return "[" + Thread.currentThread().getName() + " " + Thread.currentThread().getId() + "] " + timestamp;
    }
    //// Constructors

    /** Creates a new instance of Debug. */
    private Debug() {
    }

    static void maybeDebugNote(final String message, final boolean force) {
      if (DEBUG_ALL || force) {
        System.out.println(getTimestamp() + " " + message);
      }
    }

    static void maybeDebugWarn(final String message, final boolean force) {
      if (DEBUG_ALL || force) {
        System.err.println(getTimestamp() + " " + message);
      }
    }
  }
}
