package org.opencyc.api;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.List;

import org.opencyc.cycobject.ByteArray;
import org.opencyc.cycobject.CycAssertion;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycNart;
import org.opencyc.cycobject.CycSymbol;
import org.opencyc.cycobject.CycVariable;
import org.opencyc.cycobject.Guid;
import org.opencyc.util.ResultSetSlice;
import org.opencyc.util.Log;
import org.opencyc.util.StringUtils;
import org.opencyc.util.UUID;
import org.opencyc.cycobject.CycFormula;


/**
 * A CFASL translating buffered output stream.  All Java-native types which have logical sublisp
 * equivalents are translated automatically by this stream.  Classes implementing the
 * CfaslTranslatingObject interface are translated using their writeObject() method.  Other CYC
 * objects, such as binding-lists and formulas, should be explicitly coerced before being sent,
 * unless they inherit from a class which can be translated automatically.
 * 
 * @version $Id: CfaslOutputStream.java 140083 2012-05-18 17:40:09Z rck $
 * @author Christopher
 * @author Dan Lipofsky <p><p><p><p><p>
 *
 * @todo add support for new, terser GUID serialization
 */
public class CfaslOutputStream
  extends BufferedOutputStream {
  /** No api trace. */
  public static final int API_TRACE_NONE = 0;

  /** Message-level api trace. */
  public static final int API_TRACE_MESSAGES = 1;

  /** Detailed api trace. */
  public static final int API_TRACE_DETAILED = 2;

  /** Parameter that, when true, causes a trace of the messages to and from the server. */
  public int trace = API_TRACE_NONE;

  /** Binary values for assembling CFASL messages. */
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

  /** CFASL code */
  protected static final int CFASL_N_32BIT_INT = 7;

  /** CFASL code */
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
  protected static final int CFASL_BYTE_VECTOR = 26;

  /** CFASL code */
  protected static final int CFASL_RESULT_SET_SLICE = 27;

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

  /**
   * Creates a new CfaslOutputStream to write data to the specified underlying output stream with
   * the default buffer size.
   * 
   * @param out the underlying output stream.
   */
  public CfaslOutputStream(OutputStream out) {
    super(out);

    if (Log.current == null) {
      Log.makeLog("cfasl.log");
    }
  }

  /**
   * Creates a new CfaslOutputStream to write data to the specified underlying output stream with
   * the specified buffer size.
   * 
   * @param out    the underlying output stream.
   * @param size   the buffer size.
   */
  public CfaslOutputStream(OutputStream out, 
                           int size) {
    super(out, size);

    if (Log.current == null) {
      Log.makeLog("cfasl.log");
    }
  }

  /**
   * Writes a boolean onto this CFASL output stream. What is actually written is either the symbol
   * T or NIL.
   * 
   * @param v the boolean value to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeBoolean(boolean v)
                    throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeBoolean = " + v);
    }

    if (v) {
      writeSymbol(CycObjectFactory.t);
    }
    else {
      writeSymbol(CycObjectFactory.nil);
    }
  }

  /**
   * Writes a one byte character onto this CFASL output stream. Crudely converts from Unicode to
   * 8-bit ASCII.
   * 
   * @param v the character to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeChar(char v)
                 throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeChar = " + v);
    }

    write(CFASL_CHARACTER);
    write(v);
  }

  /**
   * Writes a long integer to this CFASL output stream.  It may be written as either a CFASL Fixnum
   * or a CFASL Bignum, depending on its size. For legacy reasons it is called writeInt instead of
   * writeLong.
   * 
   * @param v long integer to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeInt(long v)
                throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeInt = " + v);
    }

    if ((-2147483648L < v) && (v < 2147483648L)) {
      writeFixnum((int) v);
    }
    else {
      writeBignum(v);
    }
  }

  /**
   * Writes an integer to this CFASL output stream as a Fixnum. This method is protected because it
   * does no size checking, so the calling method must be wise as to what fits in a Fixnum.
   * 
   * @param v the integer to be written
   * 
   * @throws IOException if a communications error occurs
   */
  protected void writeFixnum(int v)
                      throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("* writeFixnum(long " + v + ")");
    }

    int numBytes;

    if (v >= 0) {
      if (v < CFASL_IMMEDIATE_FIXNUM_CUTOFF) {
        // We have a special way of transmitting very small positive integers
        if (trace == API_TRACE_DETAILED) {
          Log.current.println("Writing Immediate Fixnum: " + v);
        }

        write((int) v + CFASL_IMMEDIATE_FIXNUM_OFFSET);
        numBytes = 0;
      }
      else if (v < 128) { // v < 2^7
        write(CFASL_P_8BIT_INT);
        numBytes = 1;
      }
      else if (v < 32768) { // v < 2^15
        write(CFASL_P_16BIT_INT);
        numBytes = 2;
      }
      else if (v < 8388608) { // v < 2^23
        write(CFASL_P_24BIT_INT);
        numBytes = 3;
      }
      else { // v < 2^31 (implicit: nothing bigger should ever be passed in)
        write(CFASL_P_32BIT_INT);
        numBytes = 4;
      }
    }
    else {
      v = -v;

      if (v < 128) { // v < 2^7
        write(CFASL_N_8BIT_INT);
        numBytes = 1;
      }
      else if (v < 32768) { // v < 2^15
        write(CFASL_N_16BIT_INT);
        numBytes = 2;
      }
      else if (v < 8388608) { // v < 2^23
        write(CFASL_N_24BIT_INT);
        numBytes = 3;
      }
      else { // v < 2^31 (implicit: nothing bigger should ever be passed in)
        write(CFASL_N_32BIT_INT);
        numBytes = 4;
      }
    }

    // Transmit the bytes of the Fixnum in little-endian order (LSB first)
    for (int i = 0; i < numBytes; i++) {
      if (trace == API_TRACE_DETAILED) {
        Log.current.println("f\t" + ((v >>> (8 * i)) & 0xFF));
      }

      write(v >>> (8 * i));
    }
  }

  /**
   * Writes a long integer to this CFASL output stream as a Bignum. This method is protected
   * because it does no size checking, so the calling method must be wise as to whether Fixnum or
   * Bignum is better.
   * 
   * @param v the long integer to be written
   * 
   * @throws IOException if a communications error occurs
   */
  protected void writeBignum(long v)
                      throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("* writeBignum(long " + v + ")");
    }

    // Determine the sign, transmit the opcode, and take the absolute value
    if (v < 0) {
      write(CFASL_N_BIGNUM);
      v = -v;
    }
    else {
      write(CFASL_P_BIGNUM);
    }

    // Convert to an array of bytes in little-endian order (LSB at 0)
    int[] parts = new int[8];
    int numBytes = 0;

    while (v > 0) {
      parts[numBytes++] = (int) (v & 0x000000FF);
      v = v >>> 8;
    }


    // Transmit the size of the Bignum
    writeFixnum(numBytes);

    // Transmit the bytes of the Bignum in little-endian order (LSB first)
    for (int i = 0; i < numBytes; i++) {
      if (trace == API_TRACE_DETAILED) {
        Log.current.println("b\t" + parts[i]);
      }


      // It sure seems dumb to send each byte as a fixnum instead of as
      // a raw byte.  But that is the way the CFASL protocol was written.
      writeFixnum(parts[i]);
    }
  }

  /**
   * Writes a BigInteger to this CFASL output stream as a CFASL Bignum (unless it is small enough
   * to be transmitted as a CFASL Fixnum, in which case it is passed on to writeFixnum(long)).
   * 
   * @param v the <tt>BigInteger</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeBigInteger(BigInteger v)
                       throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeBigInteger = " + v);
    }

    // If the absolute value of the BigInteger is less than 2^31, it can to be
    // transmitted as a CFASL Fixnum.  Why do we use v.abs().bitLength()
    // instead of just v.bitLength()?  There is exactly 1 case that is
    // different: -2^31 has a bitLength of 31 while 2^31 has a bitLength of 32.
    if (v.abs().bitLength() < 32) {
      writeFixnum(v.intValue());

      return;
    }

    // Determine the sign, transmit the opcode, and take the absolute value
    if (v.signum() < 0) {
      write(CFASL_N_BIGNUM);
      v = v.abs();
    }
    else {
      write(CFASL_P_BIGNUM);
    }

    // Convert the number to an array of bytes in big-endian order (MSB at 0)
    byte[] parts = v.toByteArray();


    // Transmit the size of the Bignum
    writeFixnum(parts.length);

    // Transmit the bytes of the Bignum in little-endian order (LSB first)
    for (int i = parts.length - 1; i >= 0; i--) {
      // Log.current.println("b\t" + (parts[i] & 0x00FF));
      // It sure seems dumb to send each byte as a fixnum instead of as
      // a raw byte.  But that is the way the CFASL protocol was written.
      writeFixnum(parts[i] & 0x00FF);
    }
  }

  /**
   * Writes a double onto this CfaslOutputStream.  The double is encoded as the sign (part of the
   * opcode), significand, and exponent, such that the original double can be reconstructed as
   * sign  significand  2^exp. All parts are integers with the significand as small as possible.
   * 
   * @param v the double value to be written
   * 
   * @throws IOException if a communications error occurs
   * @throws RuntimeException if the argument is Not A Number (NaN)
   */
  public void writeDouble(double v)
                   throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeDouble = " + v);
    }

    if (Double.isNaN(v)) {
      throw new RuntimeException("Tried to send a NaN floating-point");
    }
    else if (Double.isInfinite(v)) {
      throw new RuntimeException("Tried to send an infinite floating-point");
    }
    else {
      if (v < 0.0) {
        write(CFASL_N_FLOAT);
        v = -v;

        // Log.current.print("writeDouble sign=-1");
      }
      else {
        write(CFASL_P_FLOAT);

        // Log.current.print("writeDouble sign=+1");
      }
      DecodedDouble decoded = new DecodedDouble(v);
      int exp = decoded.exponent;
      double sig = decoded.mantissa;

      // Log.current.println(" signif=" + (long)Math.floor(sig) + " exp=" + exp);
      writeInt((long) Math.floor(sig));
      writeInt(exp);
    }
  }

  final static private class DecodedDouble {
    //this code mirrors exactly the funcionality provided by Cyc servers.

    public DecodedDouble(double value) {
      final long longBits = Double.doubleToLongBits(value);
      sign = decodeDoubleSign(longBits);
      final int rawExponent = decodeDoubleExponent(longBits);
      final long rawMantissa = decodeDoubleMantissa(longBits);
      if (rawExponent > 0 && rawExponent < 2047) {
        exponent = rawExponent - 1023 - 52;
        mantissa = rawMantissa | 0x10000000000000L;
      } else if (rawExponent == 0) {
        if (rawMantissa != 0) {
          // denormalized number
          exponent = rawExponent - 1022 - 52;
          mantissa = rawMantissa;
        } else {
          exponent = 0;
          mantissa = 0;
        }
      } else {
        throw new NumberFormatException();
      }
    }

    static private int decodeDoubleSign(long longBits) {
      final int signBit = (int) ((longBits >> 63) & 1L);
      if (signBit == 0) {
        return 1;
      } else {
        return -1;
      }
    }

    static private int decodeDoubleExponent(long longBits) {
      return (int) ((longBits & 0x7ff0000000000000L) >> 52);
    }

    static private long decodeDoubleMantissa(long longBits) {
      return (longBits & 0x000fffffffffffffL);
    }
    final public int sign;
    final public int exponent;
    final public long mantissa;
  }


  /**
   * Writes a String to this CfaslOutputStream.
   * 
   * @param s the string to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeString(String s)
    throws IOException {
    String escapedString = escapeString(s);
    
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeString = \"" + escapedString + "\"");
    }
    
    if (StringUtils.is7BitASCII(escapedString))   write(CFASL_STRING);
    else write(CFASL_UNICODE_STRING);
    
    byte[] bytes = escapedString.getBytes("UTF-8");
    
    writeInt(bytes.length);
    write(bytes);
  }
  
  /**
   * Adds required escape chars for cfasl strings.
   * 
   * @param s the string
   * 
   * @return the string with required escape chars for cfasl
   */
  private String escapeString(String s) {
    StringBuffer stringBuffer = new StringBuffer();
    char backslash = '\\';
    char previousChar = 0;
    char ch = 0;
    
    for (int i = 0; i < s.length(); i++) {
      previousChar = ch;
      ch = s.charAt(i);

      if (ch == '\\') {
        //stringBuffer.append("\\\\");
        stringBuffer.append(ch);
      }
      else if (ch == '"') {
        if (previousChar == '\\') {
          stringBuffer.append("\\\"");
        }
        else {
          stringBuffer.append('"');
        }
      }
      else {
        stringBuffer.append(ch);
      }
    }

    return stringBuffer.toString();
  }

  /**
   * Writes a byte array to this CfaslOutputStream.
   * 
   * @param bytes the byte array to be written.
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeByteArray(byte[] bytes)
                      throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeByteArray = \"" + bytes + "\"");
    }

    write(CFASL_BYTE_VECTOR);
    writeInt(bytes.length);
    write(bytes);
  }

  /**
   * Writes a List of Objects to this CfaslOutputStream as a CFASL List.
   * 
   * @param list the list of objects to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeList(List list)
                 throws IOException {
    if (list instanceof CycList && !((CycList) list).isProperList()) {
      writeDottedList((CycList) list);

      return;
    }

    if (trace == API_TRACE_DETAILED) {
      if (list instanceof CycList) {
        Log.current.println("writeList = " + ((CycList) list).toString() + "\n  of size " + 
                            list.size());
      }
      else {
        Log.current.println("writeList = " + list + "\n  of size " + list.size());
      }
    }

    write(CFASL_LIST);
    writeInt(list.size());

    for (int i = 0; i < list.size(); i++) {
      writeObject(list.get(i));
    }
  }

  /**
   * Writes an improper (dotted) CycList of Objects to this CfaslOutputStream as a CFASL dotted
   * list.
   * 
   * @param dottedList the list of objects to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeDottedList(CycList dottedList)
                       throws IOException {
    int size = dottedList.getProperListSize();

    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeDottedList = " + dottedList.toString() + 
                          "\n  proper elements size " + size);
    }
    
    write(CFASL_DOTTED);
    writeInt(size);

    for (int i = 0; i < size; i++) {
      writeObject(dottedList.get(i));
    }

    Object dottedElement = dottedList.getDottedElement();

    if (trace == API_TRACE_DETAILED) {
      try {
        // If object dottedElement understands the safeToString method, then use it.
        Method safeToString = dottedElement.getClass().getMethod("safeToString");
        Log.current.println("writeDottedList.cdr = " + 
                            safeToString.invoke(dottedElement));
      }
       catch (Exception e) {
        Log.current.println("writeDottedList.cdr = " + dottedElement);
      }
    }

    writeObject(dottedElement);
  }

  /**
   * Writes an array of Objects to this CfaslOutputStream as a CFASL List.
   * 
   * @param list the array of objects to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeList(Object[] list)
                 throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeList(Array) = " + list + "\n  of size " + list.length);
    }

    write(CFASL_LIST);
    writeInt(list.length);

    for (int i = 0; i < list.length; i++) {
      writeObject(list[i]);
    }
  }

  /**
   * Writes a <tt>Guid</tt> object to this CfaslOutputStream.
   * 
   * @param guid the <tt>Guid</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeGuid(Guid guid)
                 throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeGuid = " + guid);
    }

    write(CFASL_LEGACY_GUID);
    writeString(guid.toString());
  }

  /**
   * Writes a <tt>CycSymbol</tt> object to this CfaslOutputStream.
   * 
   * @param cycSymbol the <tt>CycSymbol</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeSymbol(CycSymbol cycSymbol)
                   throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeSymbol = " + cycSymbol);
    }

    if (cycSymbol.isKeyword()) {
      writeKeyword(cycSymbol);

      return;
    }

    if (cycSymbol.equals(CycObjectFactory.nil)) {
      if (trace == API_TRACE_DETAILED) {
        Log.current.println("writing CFASL_NIL");
      }

      write(CFASL_NIL);
    }
    else {
      write(CFASL_SYMBOL);
      writeString(cycSymbol.toCanonicalString());
    }
  }

  /**
   * Writes a keyword symbol object to this CfaslOutputStream.
   * 
   * @param cycSymbol the keyword to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeKeyword(CycSymbol cycSymbol)
                    throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeKeyword = " + cycSymbol);
    }

    write(CFASL_KEYWORD);
    writeString(cycSymbol.toCanonicalString());
  }

  /**
   * Writes a <tt>CycVariable</tt> object to this CfaslOutputStream.
   * 
   * @param cycVariable the <tt>CycVariable</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeVariable(CycVariable cycVariable)
                     throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeVariable = " + cycVariable.toString());
    }
    final String name = cycVariable.toString();
    if (cycVariable.isHLVariable()) {
      write(CFASL_COMPLETE_VARIABLE);
      writeInt(cycVariable.hlVariableId.intValue());
      writeString(name);
    }
    else {
      write(CFASL_SYMBOL);
      writeString(cycVariable.toCanonicalString());
    }
  }

  /**
   * Writes a <tt>CycConstant</tt> object to this CfaslOutputStream.
   * 
   * @param cycConstant the <tt>CycConstant</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeCompleteConstant(CycConstant cycConstant)
                     throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeCompleteConstant = " + cycConstant.toString());
    }

    write(CFASL_EXTERNALIZATION);
    write(CFASL_COMPLETE_CONSTANT);
    writeGuid(cycConstant.getGuid());
    writeString(cycConstant.getName());
  }

  /**
   * Writes a <tt>CycNart</tt> object to this CfaslOutputStream.
   * 
   * @param cycNart the <tt>CycNart</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeNart(CycNart cycNart)
                 throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeNart = " + cycNart.toString());
    }

    write(CFASL_EXTERNALIZATION);
    write(CFASL_NART);
    writeList(cycNart.toCycList());
  }

  /**
   * Writes a <tt>CycAssertion</tt> object to this CfaslOutputStream.
   * 
   * @param cycAssertion the <tt>CycAssertion</tt> to be written
   * 
   * @throws IOException if a communications error occurs
   */
  public void writeAssertion(CycAssertion cycAssertion)
                      throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeAssertion = " + cycAssertion.toString());
    }
    write(CFASL_EXTERNALIZATION);
    write(CFASL_ASSERTION);
    writeList(cycAssertion.getFormula());
    writeObject(cycAssertion.getMt());
  }

  /**
   * Writes a ResultSetSlice onto this CFASL output stream. ResultSetSlices are a slice from
   * actual java.sql.ResultSets.
   */ 

  public void writeResultSetSlice(ResultSetSlice rss) throws IOException {
    if (trace == API_TRACE_DETAILED) {
      Log.current.println("writeResultSetSlice = " + rss);
    }

    ResultSet rs = rss.resultSet();
    
    int first = rss.first();
    int last = rss.last();
    int columnCount = rss.columnCount();
    
    write(CFASL_RESULT_SET_SLICE);
    writeInt(rss.rowCount());
    writeInt(rss.sliceRowCount());
    writeInt(columnCount);
    writeInt(first);
    
    try {
      rss.beforeFirst();
      for (int row = first; row <= last; row++) {
 	rs.next();
 	for (int column = 1; column <= columnCount; column++) {
 	  writeObject(rs.getObject(column));
 	}
      }
    }
    catch (SQLException e) {
      throw new RuntimeException(e.getMessage());
    }
  }
  


  /**
   * Writes a generic object to this CfaslOutputStream.
   * 
   * @param o the object to be written
   * 
   * @throws IOException if the Object cannot be translated.
   */
  public void writeObject(Object o)
                   throws IOException {
    if (trace == API_TRACE_DETAILED) {
      try {
        if (o == null) {
          Log.current.println("writeObject = null");
        }
        else {
          // If object o understands the safeToString method, then use it.
          Method safeToString = o.getClass().getMethod("safeToString");
          Log.current.println("writeObject = " + 
                              safeToString.invoke(o) + " (" + o.getClass() + ")");
        }
      }
       catch (Exception e) {
        Log.current.println("writeObject = " + o + " (" + o.getClass() + ")");
      }
    }

    if (o == null) {
      // Substitute :NULL symbol for java null
      writeKeyword(CycObjectFactory.nul);
    }
    else if (o instanceof Guid) {
      writeGuid((Guid) o);
    }
    else if (o instanceof CycSymbol) {
      writeSymbol((CycSymbol) o);
    }
    else if (o instanceof CycVariable) {
      writeVariable((CycVariable) o);
    }
    else if (o instanceof CycConstant) {
      writeCompleteConstant((CycConstant) o);
    }
    else if (o instanceof CycNart) {
      writeNart((CycNart) o);
    }
    else if (o instanceof CycAssertion) {
      writeAssertion((CycAssertion) o);
    }
    else if (o instanceof CycFormula) {
      writeList(((CycFormula) o).getArgsUnmodifiable());
    }
    else if (o instanceof List) {
      writeList((List) o);
    }
    else if (o instanceof Boolean) {
      writeBoolean(((Boolean) o).booleanValue());
    }
    else if (o instanceof ResultSetSlice) {
      writeResultSetSlice((ResultSetSlice) o);
    }
    else if (o instanceof Character) {
      writeChar(((Character) o).charValue());
    }
    else if (o instanceof String) {
      writeString((String) o);
    }
    else if (o instanceof Double) {
      writeDouble(((Double) o).doubleValue());
    }
    else if (o instanceof Float) {
      writeDouble(((Float) o).doubleValue());
    }
    else if (o instanceof Long) {
      writeInt(((Long) o).longValue());
    }
    else if (o instanceof Integer) {
      writeInt(((Integer) o).longValue());
    }
    else if (o instanceof Short) {
      writeInt(((Short) o).longValue());
    }
    else if (o instanceof Byte) {
      writeInt(((Byte) o).longValue());
    }
    else if (o instanceof BigInteger) {
      writeBigInteger((BigInteger) o);
    }
    else if (o instanceof BigDecimal) {
      // if the BigDecimal is actually an integer, it would be preferable to send 
      // it as a BigInteger rather than a double as done here
      if (((BigDecimal) o).scale() <= 0) {
	writeBigInteger(((BigDecimal) o).toBigInteger());
      } else {
	writeDouble(((BigDecimal) o).doubleValue());
      }
    }
    else if (o instanceof Object[]) {
      writeList((Object[]) o);
    }
    else if (o instanceof ByteArray) {
      writeByteArray(((ByteArray) o).byteArrayValue());
    }
    else if (o instanceof byte[]) {
      writeByteArray((byte[]) o);
    }
    else if (o instanceof UUID) {
      writeString(o.toString());
    }
    else if (o instanceof Timestamp) {
      writeString(o.toString());
    }
    else {
      System.err.println("CfaslOutputStream: unknown class " + o.getClass().getName() + " sent as a string");
      writeString(o.toString());
    }
  }
}
