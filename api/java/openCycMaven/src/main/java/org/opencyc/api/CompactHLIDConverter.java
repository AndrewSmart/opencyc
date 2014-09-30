package org.opencyc.api;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opencyc.util.Base64;

import org.opencyc.util.Base64Url;

/**
 * A CFASL translating convert to/from compact external representations for CycL literal
 * types (Strings and Numbers) without needing to access a Cyc image.
 *
 * @see DefaultCycObject.toCompactExternalId()
 * @see DefaultCycObject.fromComactExternalId()
 * 
 * @version $Id: CompactHLIDConverter.java 131073 2010-05-27 21:47:32Z daves $
 * @author daves
 * @todo add support for new, terser GUID serialization
 */
public class CompactHLIDConverter {

  //// Constructors
  /**
   * Creates a new CfaslOutputStream to write data to the specified underlying output stream with
   * the default buffer size.
   * 
   * @param out the underlying output stream.
   */
  private CompactHLIDConverter() {
    byteStream = new ByteArrayOutputStream();
    cfaslStream = new CfaslOutputStream(byteStream);
    base64 = new Base64Url();
  }

  //// Public Area
  /**
   *
   * @return
   */
  public static CompactHLIDConverter converter() {
    return converter;
  }

  /**
   *
   * @param obj Must be either a String or a Number.  Otherwise will throw IllegalArgumentException
   * @return
   * @throws IOException
   */
  public synchronized String toCompactHLId(Object obj) throws IOException {
    return toCompactHLIdStringInternal(obj);
  }

  /**
   *
   * @param obj
   * @return
   * @throws IOException
   */
  public synchronized String toCompactHLId(String obj) throws IOException {
    return toCompactHLIdStringInternal(obj);
  }

  /**
   *
   * @param obj
   * @return
   * @throws IOException
   */
  public synchronized String toCompactHLId(Number obj) throws IOException {
    return toCompactHLIdStringInternal(obj);
  }

  /**
   *
   * @param obj ; must be either a Number or a String.
   * Otherwise, throws IllegalArgumentException
   * @return
   * @throws IOException
   */
  public String toOpenCycURI(Object obj) throws IOException {
    return "http://sw.opencyc.org/concept/" + toCompactHLId(obj);
  }

  /**
   *
   * @param num
   * @return
   * @throws IOException
   */
  public String toOpenCycURI(String num) throws IOException {
    return "http://sw.opencyc.org/concept/" + toCompactHLId(num);
  }

  /**
   *
   * @param num
   * @return
   * @throws IOException
   */
  public String toOpenCycURI(Number num) throws IOException {
    return "http://sw.opencyc.org/concept/" + toCompactHLId(num);
  }

  /**
   *
   * @param obj ; must be either a Number or a String.
   * Otherwise, throws IllegalArgumentException
   * @return
   * @throws IOException
   */
  public String toOpenCyReabablecURI(Object obj) throws IOException {
    if (obj instanceof Number) {
      return toOpenCycReadableURI((Number) obj);
    } else if (obj instanceof String) {
      return toOpenCycReadableURI((String) obj);
    } else {
      throw new IllegalArgumentException(obj + " is neither a String nor a Number.");
    }
  }

  /**
   *
   * @param num
   * @return
   * @throws IOException
   */
  public String toOpenCycReadableURI(String num) throws IOException {
    return "http://sw.opencyc.org/concept/en/" + toOwlNlId(num);
  }

  /**
   *
   * @param num
   * @return
   * @throws IOException
   */
  public String toOpenCycReadableURI(Number num) throws IOException {
    return "http://sw.opencyc.org/concept/en/" + toOwlNlId(num);
  }

  public String toOwlNlId(Object obj) {
    if (obj instanceof Number) {
      return toOwlNlId((Number) obj);
    } else if (obj instanceof String) {
      return toOwlNlId((String) obj);
    } else {
      throw new IllegalArgumentException(obj + " is neither a String nor a Number.");
    }
  }

  /**
   * 
   * @param num
   * @return
   */
  public String toOwlNlId(Number num) {
    return num.toString();
  }

  /**
   *
   * @param str
   * @return
   */
  public String toOwlNlId(String str) {
    return "STRING_" + str;
  }

  /**
   *
   * @param id
   * @return
   * @throws IOException
   */
  public synchronized Object fromCompactHLId(String id) throws IOException {
    id = padWithEqualSigns(id);
    byte[] buf = base64.decode(id);
    CfaslInputStream cfaslInStream = new CfaslInputStream(new ByteArrayInputStream(buf));
    Object result = cfaslInStream.readObject();
    return result;
  }

  /**
   *
   * @param id
   * @return
   * @note This does not return true for UnicodeStrings at this time.
   */
  public boolean isStringCompactHLId(String id) throws IOException {
    byte[] bytes = base64.decode(padWithEqualSigns(id));
    CfaslInputStream cfaslInStream = new CfaslInputStream(new ByteArrayInputStream(bytes));
    Integer obj1 = cfaslInStream.read();
    Integer obj2 = cfaslInStream.read();
    return isStringCompactHLId(obj1, obj2);
  }

  /**
   *
   * @param id
   * @return
   */
  public boolean isNumberCompactHLId(String id) throws IOException {
    byte[] bytes = base64.decode(padWithEqualSigns(id));
    CfaslInputStream cfaslInStream = new CfaslInputStream(new ByteArrayInputStream(bytes));
    Integer obj1 = cfaslInStream.read();
    Integer obj2 = cfaslInStream.read();
    return isNumberCompactHLId(obj1, obj2);
  }

  /**
   *
   * @param id
   * @return
   */
  public boolean isLiteralCompactHLId(String id) throws IOException {
    byte[] bytes = base64.decode(padWithEqualSigns(id));
    CfaslInputStream cfaslInStream = new CfaslInputStream(new ByteArrayInputStream(bytes));
    Integer obj1 = cfaslInStream.read();
    Integer obj2 = cfaslInStream.read();
    return (isStringCompactHLId(obj1, obj2) || isNumberCompactHLId(obj1, obj2));
  }

  //// Protected Area
  //// Private Area
  private synchronized String toCompactHLIdStringInternal(Object obj) throws IOException {
    if (!((obj instanceof String) || (obj instanceof Number))) {
      throw new IllegalArgumentException(obj + " must be either a number or a string.");
    }
    byteStream.reset();
    cfaslStream.write(CfaslOutputStream.CFASL_EXTERNALIZATION);
    cfaslStream.writeObject(obj);
    cfaslStream.flush();
    byte[] bytes = byteStream.toByteArray();
    for (byte b : bytes) {
      int wideB = b;
      if (wideB < 0) {
        wideB += 256;
      }
    }
    String rawBase64UrlEncoded = base64.encodeBytes(bytes);
    final int equalSignPos = rawBase64UrlEncoded.indexOf('=');
    if (equalSignPos >= 0) {
      return rawBase64UrlEncoded.substring(0, equalSignPos);
    } else {
      return rawBase64UrlEncoded;
    }
  }

  private boolean isNumberCompactHLId(Integer code1, Integer code2) throws IOException {
    boolean isExternal = code1.equals(CfaslInputStream.CFASL_EXTERNALIZATION);
    boolean isNumber = numberOpCodes.contains(code2);
    boolean isSmallNumber =  code2 >= CfaslInputStream.CFASL_IMMEDIATE_FIXNUM_OFFSET;
    return (isExternal && (isNumber || isSmallNumber));
  }

  private boolean isStringCompactHLId(Integer code1, Integer code2) throws IOException {
    return (code1.equals(CfaslInputStream.CFASL_EXTERNALIZATION) &&
          code2.equals(CfaslInputStream.CFASL_STRING));
  }

  private static String padWithEqualSigns(String str) {
    int toAdd = str.length() % 4;
    for (int i = 0; i < toAdd; i++) {
      str = str + "=";
    }
    return str;


  }
  //// Internal Rep
  private final ByteArrayOutputStream byteStream;
  private final CfaslOutputStream cfaslStream;
  private final Base64 base64;
  private static CompactHLIDConverter converter = new CompactHLIDConverter();
  private static final List<Integer> numberOpCodes = Arrays.asList(
          CfaslInputStream.CFASL_N_BIGNUM,
          CfaslInputStream.CFASL_P_BIGNUM,
          CfaslInputStream.CFASL_N_16BIT_INT,
          CfaslInputStream.CFASL_N_24BIT_INT,
          CfaslInputStream.CFASL_N_32BIT_INT,
          CfaslInputStream.CFASL_N_8BIT_INT,
          CfaslInputStream.CFASL_N_FLOAT,
          CfaslInputStream.CFASL_P_16BIT_INT,
          CfaslInputStream.CFASL_P_24BIT_INT,
          CfaslInputStream.CFASL_P_32BIT_INT,
          CfaslInputStream.CFASL_P_8BIT_INT,
          CfaslInputStream.CFASL_P_FLOAT);

  //// Main
  /**
   *
   * @param args
   */
  public static void main(String[] args) {
    final Logger logger = Logger.getLogger(CompactHLIDConverter.class.toString());
    logger.info("Starting");
    try {
      logger.info("HLId for " + 1 + " is '" + CompactHLIDConverter.converter().toCompactHLId(1) + "'.");
      logger.info("HLId for " + 122 + " is '" + CompactHLIDConverter.converter().toCompactHLId(122) + "'.");
      logger.info("HLId for " + 0 + " is '" + CompactHLIDConverter.converter().toCompactHLId(0) + "'.");
      logger.info("HLId for " + -122 + " is '" + CompactHLIDConverter.converter().toCompactHLId(-122) + "'.");
      logger.info("HLId for " + 128 + " is '" + CompactHLIDConverter.converter().toCompactHLId(128) + "'.");
      logger.info("HLId for " + -128 + " is '" + CompactHLIDConverter.converter().toCompactHLId(-128) + "'.");
      logger.info("HLId for " + 128.2 + " is '" + CompactHLIDConverter.converter().toCompactHLId(128.2) + "'.");
      logger.info("HLId for " + -128.2 + " is '" + CompactHLIDConverter.converter().toCompactHLId(-128.2) + "'.");
      logger.info("Object for M4w= is " + CompactHLIDConverter.converter().fromCompactHLId("M4w="));
      logger.info("Object for M4E= is " + CompactHLIDConverter.converter().fromCompactHLId("M4E="));
      logger.info("Object for M4w is " + CompactHLIDConverter.converter().fromCompactHLId("M4w"));
      logger.info("Object for Mw-EYXNiYw== is " + CompactHLIDConverter.converter().fromCompactHLId("Mw-EYXNiYw=="));
      logger.info("Object for Mw-EYXNiYw is " + CompactHLIDConverter.converter().fromCompactHLId("Mw-EYXNiYw"));
      logger.info("Object for Mw-IMTIzNS4xMjM is " + CompactHLIDConverter.converter().fromCompactHLId("Mw-IMTIzNS4xMjM"));
      System.out.flush();
    } catch (Exception e) {
      logger.log(Level.SEVERE, e.getMessage(), e);
    } finally {
      logger.info("Finished.");
      System.exit(0);
    }
  }
}

