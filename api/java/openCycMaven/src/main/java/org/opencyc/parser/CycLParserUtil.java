
package org.opencyc.parser;

//// Internal Imports
import org.opencyc.api.*;
import org.opencyc.cycobject.*;

//// External Imports
import java.io.*;
import java.util.*;

/**
 * <P>CycLParserUtil is designed to be the main entry point into parsing
 * CycL expressions.
 *
 * @version $Id: CycLParserUtil.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Tony Brusseau
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
public class CycLParserUtil {
  
  //// Constructors
  
  /** Creates a new instance of CycLParserUtil. */
  private CycLParserUtil() {}
  
  //// Public Area
  
  public static Object parseCycLTerm(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLTerm(new StringReader(toParse), testForEOF, access);
  }
  
  public static Object parseCycLTerm(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return completeConstants(parser.term(testForEOF), access);
  }
  
  public static CycList parseCycLTermList(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLTermList(new StringReader(toParse), testForEOF, access);
  }
  
  public static CycList parseCycLTermList(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return (CycList)completeConstants(parser.termList(testForEOF), access);
  }
  
  public static CycFormulaSentence parseCycLSentence(String toParse, boolean testForEOF, CycAccess access)
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLSentence(new StringReader(toParse), testForEOF, access);
  }
  
  public static CycFormulaSentence parseCycLSentence(Reader reader, boolean testForEOF, CycAccess access)
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return new CycFormulaSentence((CycList)completeConstants(parser.sentence(testForEOF), access));
  }
  
  public static String parseCycLString(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  TokenMgrError {
    return parseCycLString(new StringReader(toParse), testForEOF, access);
  }
  
  public static String parseCycLString(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException,
  TokenMgrError{
    CycLParser parser = new CycLParser(reader, access);
    return parser.string(testForEOF);
  }
  
  public static Number parseCycLNumber(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  TokenMgrError {
    return parseCycLNumber(new StringReader(toParse), testForEOF, access);
  }
  
  public static Number parseCycLNumber(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return parser.number(testForEOF);
  }
  
  public static CycConstant parseCycLConstant(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLConstant(new StringReader(toParse), testForEOF, access);
  }
  
  public static CycConstant parseCycLConstant(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return (CycConstant)completeConstants(parser.constant(testForEOF), access);
  }
  
  public static CycVariable parseCycLVariable(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  TokenMgrError {
    return parseCycLVariable(new StringReader(toParse), testForEOF, access);
  }
  
  public static CycVariable parseCycLVariable(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return parser.variable(testForEOF);
  }
  
  public static Object parseCycLDenotationalTerm(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLDenotationalTerm(new StringReader(toParse), testForEOF, access);
  }
  
  public static Object parseCycLDenotationalTerm(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return (Object)completeConstants(parser.denotationalTerm(testForEOF), access);
  }

  public static CycFort parseCycLFORT(String toParse, boolean testForEOF, CycAccess access)
  throws ParseException, IOException, CycApiException, CycApiServerSideException,
  InvalidConstantNameException, InvalidConstantGuidException,
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLFORT(new StringReader(toParse), testForEOF, access);
  }

  public static CycFort parseCycLFORT(Reader reader, boolean testForEOF, CycAccess access)
  throws ParseException, IOException, CycApiException, CycApiServerSideException,
  InvalidConstantNameException, InvalidConstantGuidException,
  UnsupportedVocabularyException, TokenMgrError {
    Object result = parseCycLDenotationalTerm(reader, testForEOF, access);
    if (result instanceof CycList) {
      Object result2 = access.getHLCycTerm(DefaultCycObject.cyclify(result));
      if (!(result2 instanceof CycNart)) {
        throw new RuntimeException("Unknown fort: " + result);
      }
      return (CycNart)result2;
    } else if (result instanceof CycConstant) {
      return (CycConstant)result;
    } else if (result instanceof CycNart) {
      return (CycNart)result;
    }
    throw new RuntimeException("Unable to find appropriate FORT.");
  }

  /** Takes a CycL formula represented in CycList form and replaces all the subcomponents
   * that are NARTs in the KB with CycNart objects. All other CycL objects are returned unchanged.
   * Note, this function calls a non-api level SubL call, therefore it may error when called
   * against OpenCyc but will be accessible from ResearchCyc. It has the advantage over "toHL"
   * becuase nartSubstitute does not do other sorts of cannoicalizations like reordering arguments
   * and such. Note: this isn't a parsing function and passing a string of a formula will just
   * return the string unchanged. Note: NARTs should be an implementation detail of the
   * inference engine, however, there are quite a few expections where api methods
   * behave differently whether they are used or not.
   **/
  public static Object nartSubstitute(Object cyclObject, CycAccess access)
  throws IOException {
    if (!DefaultCycObject.isCycLObject(cyclObject)) {
      throw new RuntimeException(DefaultCycObject.cyclify(cyclObject) + " is not a valid Cyc object.");
    }
    if (!(cyclObject instanceof CycObject)) { // @todo need a test that sees if the CycObject
                                              // contains any CycLists any fast fail if not
      return cyclObject;
    }
    return access.converseObject("(nart-substitute  '" + DefaultCycObject.cyclifyWithEscapeChars(cyclObject, true) + ")");
  }

   /** Takes a CycL formula represented in CycList form and replaces all the subcomponents
   * that are NARTs in the KB with CycNart objects as well as doing arguments reordering and
   * variable renaming and conversion. All other CycL objects are returned unchanged.
   * Note: this isn't a parsing function and passing a string of a formula will just
   * return the string unchanged. Note: NARTs and HL constructs should be an implementation
   * detail of the inference engine, however, there are quite a few expections where api methods
   * behave differently whether they are used or not.
   **/
  public static Object toHL(Object cyclObject, CycAccess access)
  throws IOException {
    if (!DefaultCycObject.isCycLObject(cyclObject)) {
      throw new RuntimeException(DefaultCycObject.cyclify(cyclObject) + " is not a valid Cyc object.");
    }
    if (!(cyclObject instanceof CycObject)) {
      return cyclObject;
    }
    return access.converseObject("(canonicalize-term  '" + DefaultCycObject.cyclifyWithEscapeChars(cyclObject, true) + ")");
  }
  
  public static Object parseCycLNonAtomicDenotationalTerm(String toParse, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    return parseCycLNonAtomicDenotationalTerm(new StringReader(toParse), testForEOF, access);
  }
  
  public static Object parseCycLNonAtomicDenotationalTerm(Reader reader, boolean testForEOF, CycAccess access) 
  throws ParseException, IOException, CycApiException, CycApiServerSideException, 
  InvalidConstantNameException, InvalidConstantGuidException, 
  UnsupportedVocabularyException, TokenMgrError {
    CycLParser parser = new CycLParser(reader, access);
    return (Object)completeConstants(parser.nonAtomicDenotationalTerm(testForEOF), access);
  }

  public static Object completeConstants(Object obj, CycAccess access) 
    throws IOException, 
           CycApiException, 
           CycApiServerSideException, 
           InvalidConstantNameException, 
           InvalidConstantGuidException, 
           UnsupportedVocabularyException {
    List allConstants = DefaultCycObject.getReferencedConstants(obj);
    if ((allConstants == null) || (allConstants.size() == 0)) { return obj; }
    CycList incompleteConstantsWithNames = null;
    CycList incompleteConstantsWithGuids = null;
    //Find incomplete constants
    for ( Iterator iter = allConstants.iterator(); iter.hasNext(); ) {
      CycConstant curConst = (CycConstant)iter.next();
      if ((curConst.name == null) && (curConst.guid == null)) {
        throw new IllegalArgumentException("Can't deal with completely bare constants.");
      }
      if (curConst.name == null) {
        if (incompleteConstantsWithGuids == null) { incompleteConstantsWithGuids = new CycList(); }
        incompleteConstantsWithGuids.add(curConst);
      }
      if (curConst.guid == null) {
        if (incompleteConstantsWithNames == null) { incompleteConstantsWithNames = new CycList(); }
        incompleteConstantsWithNames.add(curConst);
      }
    }
    //Find invalid constant names from the list of incomplete constants
    List cycConstants = access.findConstantsForNames(incompleteConstantsWithNames);
    if ((cycConstants != null) && (incompleteConstantsWithNames != null)) {
      InvalidConstantNameException icne = null;
      for (Iterator iter = cycConstants.iterator(), 
          oldIter = incompleteConstantsWithNames.iterator(); iter.hasNext(); ) {
        Object curConstant = iter.next();
        CycConstant oldConstant = (CycConstant)oldIter.next();
        if (!(curConstant instanceof CycConstant)) {
          if (icne == null) { icne = new InvalidConstantNameException(); }
          icne.addInvalidConstantName(oldConstant.getName());
        } else {
          oldConstant.setGuid(((CycConstant)curConstant).getGuid());
          CycObjectFactory.addCycConstantCache(oldConstant);
        }
      }
      if (icne != null) { throw icne; }
    }
    //Find invlaid GUIDs from the list of incomplete constants
    cycConstants = access.findConstantsForGuids(incompleteConstantsWithGuids);
    if ((cycConstants != null) && (incompleteConstantsWithGuids != null)) {
      InvalidConstantGuidException icge = null;
      for (Iterator iter = cycConstants.iterator(), 
          oldIter = incompleteConstantsWithGuids.iterator(); iter.hasNext(); ) {
        Object curConstant = iter.next();
        CycConstant oldConstant = (CycConstant)oldIter.next();
        if (!(curConstant instanceof CycConstant)) {
          if (icge == null) { icge = new InvalidConstantGuidException(); }
          icge.addInvalidConstantGuid(oldConstant.getGuid());
        } else {
          oldConstant.setName(((CycConstant)curConstant).getName());
          CycObjectFactory.addCycConstantCache(oldConstant);
        }
      }
      if (icge != null) { throw icge; }
    }
    //Find unsupported constants
    for ( Iterator iter = allConstants.iterator(); iter.hasNext(); ){
      CycConstant curConst = (CycConstant)iter.next();
      if (access.sublQuoteFnConst.guid.equals(curConst.getGuid())) {
        throw new UnsupportedVocabularyException(access.sublQuoteFnConst);
      }
      if (access.expandSubLFnConst.guid.equals(curConst.getGuid())) {
        throw new UnsupportedVocabularyException(access.expandSubLFnConst);
      }
    }
    return obj;
  }
  
  //// Protected Area
  
  protected static void sanityCheck() {
    try {
      CycAccess access = new CycAccess(TEST_CYC_MACHINE, TEST_CYC_PORT);
      Object obj = parseCycLConstant("Dog", true, access);
      System.out.println("Got result: " + obj);
      obj = parseCycLConstant("#G\"bd590573-9c29-11b1-9dad-c379636f7270\"", true, access);
      System.out.println("Got result: " + obj);
      try {
        obj = parseCycLConstant("Dogqweqr", true, access);
        System.out.println("Got result: " + obj);
      } catch (Exception e) { e.printStackTrace(System.out); }
      try {
        obj = parseCycLConstant("#G\"bd590573-9c29-11b1-9dad-c379636f7279\"", true, access);
        System.out.println("Got result: " + obj);
      } catch (Exception e) { e.printStackTrace(System.out); }
      try {
        obj = parseCycLFORT("(FruitFn AppleTree)", true, access);
        System.out.println("Got FORT: " + obj);
      } catch (Exception e) { e.printStackTrace(System.out); }
      try {
        obj = parseCycLDenotationalTerm("(FruitFn AppleTree)", true, access);
        System.out.println("Got FORT: " + obj + " of type: " + obj.getClass());
        obj = nartSubstitute(obj, access);
        System.out.println("Got FORT: " + obj + " of type: " + obj.getClass());
        obj = toHL(obj, access);
        System.out.println("Got FORT: " + obj + " of type: " + obj.getClass());
        obj = nartSubstitute("\"", access);
        System.out.println("Got single double quote string as: " + obj);
        obj = toHL("\"", access);
        System.out.println("Got single double quote string as: " + obj);
      } catch (Exception e) { e.printStackTrace(System.out); }
    } catch (Exception e) {
      e.printStackTrace();
      System.out.println("Finished abnormally");
      System.exit(-1);
    }
  }
  
  //// Private Area
  
  //// Internal Reader
  
  private static String TEST_CYC_MACHINE = "localhost";
  
  private static int TEST_CYC_PORT = 3660;
  
  //// Main
  
  public static void main(String[] args) {
    System.out.println("Starting");
    try {
      sanityCheck();
    } catch (Exception e) {
      e.printStackTrace();
      System.out.println("Finished abnormally");
      System.exit(-1);
    }
    System.out.println("Finished");
    System.exit(0);
  }
  
}
