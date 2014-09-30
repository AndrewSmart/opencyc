/* $Id: PasswordManager.java 130961 2010-05-14 21:19:18Z daves $
 *
 * Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.util;

//// External Imports
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

//// Internal Imports
import org.opencyc.api.CycAccess;
import org.opencyc.api.CycApiException;
import org.opencyc.cycobject.CycConstant;
import org.opencyc.cycobject.CycDenotationalTerm;
import org.opencyc.cycobject.DefaultCycObject;
import static org.opencyc.cycobject.DefaultCycObject.stringApiValue;
import org.opencyc.cycobject.Guid;

/** 
 * <P>PasswordManager is designed to handle password encryption, authentication ,etc.
 *
 * <P>Copyright (c) 2008 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * Created on : Jul 27, 2009, 3:10:59 PM
 * Author     : baxter
 * @version $Id: PasswordManager.java 130961 2010-05-14 21:19:18Z daves $
 */
public class PasswordManager {

  //// Constructors
  /** Creates a new instance of PasswordManager. */
  public PasswordManager(final CycAccess cycAccess) {
    this.cycAccess = cycAccess;
  }

  //// Public Area
  /** Verify that a user/password combination for an application is valid.
   *
   * @param user
   * @param applicationTerm
   * @param unencryptedPassword
   * @return true iff Cyc verifies the authenticity of the login credentials.
   */
  public boolean areLoginCredentialsValid(final CycConstant user, final CycDenotationalTerm applicationTerm,
          final char[] unencryptedPassword) {
    boolean isValid = false;
    final String encryptedPassword = encryptPassword(unencryptedPassword, user);
    // @note Authentication sets the cyclist to Guest on failure:
    final String command = "(PROGN" +
            "(AUTHENTICATE-THE-CYCLIST " + stringApiValue(user.getName()) +
            " " + stringApiValue(encryptedPassword) + " " + applicationTerm.stringApiValue() + ")" +
            "(CNOT (THE-CYCLIST-IS-GUEST?)))";
    try {
      isValid = CommUtils.convertResponseToBoolean(CommUtils.performApiCommand(command, cycAccess));
      if (isValid) {
        noteValidPassword(user, applicationTerm, encryptedPassword);
      }
    } catch (IOException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (TimeOutException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (CycApiException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (OpenCycTaskInterruptedException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    }
    return isValid;
  }

  /** Does the Cyc Image require authentication before allowing a user access? */
  public Boolean isPasswordRequired() throws IOException {
    return CommUtils.convertResponseToBoolean(CommUtils.performApiCommand("(image-requires-authentication?)", cycAccess));
  }

  /** Update the password for the specified user and application.
   *
   * @param user the user whose password is to be updated
   * @param applicationTerm the application for which the user's password is to be updated.
   * @param unencryptedPassword the new password
   * @return true iff Cyc signals the password has been successfully updated.
   */
  public boolean updatePassword(final CycConstant user, final CycDenotationalTerm applicationTerm,
          final char[] unencryptedPassword) {
    final String encryptedPassword = encryptPassword(unencryptedPassword, user);
    final String command = "(SPECIFY-AUTHENTICATION-INFO-FOR-USER " + user.stringApiValue() +
            " " + DefaultCycObject.stringApiValue(encryptedPassword) +
            " " + applicationTerm.stringApiValue() + ")";
    try {
      final boolean success = CommUtils.convertResponseToBoolean(CommUtils.performApiCommand(command, cycAccess));
      if (success) {
        noteValidPassword(user, applicationTerm, encryptedPassword);
      }
      return success;
    } catch (IOException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (TimeOutException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (CycApiException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    } catch (OpenCycTaskInterruptedException ex) {
      Logger.getLogger(PasswordManager.class.getName()).log(Level.SEVERE, null, ex);
    }
    return false;
  }

  /** Encrypt a user/password combination using Cyc's standard password encryption.
   * 
   * @param unencryptedPassword
   * @param user
   * @return the encrypted password
   */
  public static String encryptPassword(final char[] unencryptedPassword, final CycConstant user) {
    try {
      final MessageDigest encryptor = MessageDigest.getInstance("SHA-1");
      final String charSet = "iso-8859-1";
      encryptor.update(user.getName().getBytes(charSet));
      encryptor.update(passwordCharsToBytes(unencryptedPassword, charSet));
      return base64.encodeBytes(encryptor.digest());
    } catch (NoSuchAlgorithmException ex) {
      throw new RuntimeException("Failed to encrypt password", ex);
    } catch (UnsupportedEncodingException ex) {
      throw new RuntimeException("Failed to encrypt password", ex);
    }
  }

  /** Look up the (encrypted) password for user using application.
   *
   * This will only find passwords that have been entered into this application.
   *
   * @param user
   * @param applicationTerm
   * @return the *encrypted* password, or null if it is not known.
   */
  public String lookupPassword(final CycConstant user, final CycDenotationalTerm applicationTerm) {
    return CACHE.get(new CacheKey(user, applicationTerm, cycAccess));
  }
  //// Protected Area

  //// Private Area
  /** Convert password in char[] form to byte[] form.
   *
   * @note Ideally, we'd like to do this conversion without constructing an intermediate String representation.
   * @param unencryptedPassword
   * @param charSet
   * @return
   * @throws java.io.UnsupportedEncodingException
   */
  private static byte[] passwordCharsToBytes(final char[] unencryptedPassword,
          final String charSet) throws UnsupportedEncodingException {
    final String unencryptedPasswordString = new String(unencryptedPassword);
    final byte[] passwordBytes = unencryptedPasswordString.getBytes(charSet);
    return passwordBytes;
  }

  private void noteValidPassword(final CycConstant user, final CycDenotationalTerm applicationTerm,
          final String encryptedPassword) {
    CACHE.put(new CacheKey(user, applicationTerm, cycAccess), encryptedPassword);
  }
  //// Internal Rep
  private final CycAccess cycAccess;
  static private final CycConstant testUser = new CycConstant("Baxter", new Guid("beeac37c-9c29-11b1-9dad-c379636f7270"));
  static private int successCount = 0;
  static private int testCount = 0;
  private final static Base64 base64 = new Base64();
  private final static Map<CacheKey, String> CACHE = new HashMap<CacheKey, String>();

  private class CacheKey {

    final private CycDenotationalTerm user;
    final private CycDenotationalTerm applicationTerm;
    final private CycAccess cyc;

    private CacheKey(final CycConstant user, final CycDenotationalTerm applicationTerm,
            final CycAccess cyc) {
      this.user = user;
      this.applicationTerm = applicationTerm;
      this.cyc = cyc;
    }

    @Override
    public int hashCode() {
      return user.hashCode() + applicationTerm.hashCode() + cyc.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      final CacheKey other = (CacheKey) obj;
      if (this.user != other.user && (this.user == null || !this.user.equals(other.user))) {
        return false;
      }
      if (this.applicationTerm != other.applicationTerm && (this.applicationTerm == null ||
              !this.applicationTerm.equals(other.applicationTerm))) {
        return false;
      }
      if (this.cyc != other.cyc && (this.cyc == null || !this.cyc.equals(other.cyc))) {
        return false;
      }
      return true;
    }
  }

  //// Main
  public static void main(final String[] args) {
    testCount = successCount = 0;
    checkOne("D", "c4W9SqCQVJfGGLjEehulnXTOCpM=");
    checkOne("dog", "ERaE0f2BsQ8hg2udao8mJakTiPY=");
    System.out.println(successCount + " of " + testCount + " tests passed.");
  }

  private static void checkOne(final String unencryptedPassword, final String encryptedPassword) {
    final String actual = encryptPassword(unencryptedPassword.toCharArray(), testUser);
    final String expected = encryptedPassword;
    if (actual.equals(expected)) {
      successCount++;
    } else {
      System.out.println("Wanted " + expected + " Got " + actual);
    }
    testCount++;
  }
}
