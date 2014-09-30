package org.opencyc.api;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.UnknownHostException;
import org.opencyc.cycobject.CycFormulaSentence;
import org.opencyc.cycobject.CycFort;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.CycNart;
import org.opencyc.cycobject.Guid;
import org.opencyc.inference.DefaultInferenceParameters;
import org.opencyc.inference.InferenceParameters;
import org.opencyc.util.Log;

/**
 * Provides a simple demo of the OpenCyc API.<p>
 *
 * @version $Id: ApiDemo.java 130974 2010-05-18 16:07:47Z baxter $
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

public class ApiDemo {
  
  /**
   * the CycAccess object
   */
  protected CycAccess cycAccess;
  
  public ApiDemo() {
    Log.makeLog();
    Log.current.println("Initializing Cyc server connection, and caching frequently used terms.");
    try {
      cycAccess = new CycAccess();
    }
    catch (Exception e) {
      Log.current.errorPrintln(e.getMessage());
      Log.current.printStackTrace(e);
    }
    cycAccess.traceOn();
    Log.current.println("Now tracing Cyc server messages");
  }
  
  /**
   * Interacts with the user to perform specified demos.
   */
  protected void demoInteraction() {
    Log.current.println("Ready.  Enter demo number 1 ... 17, or exit");
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
    try {
      while (true) {
        System.out.print("> ");
        String userDemoCommand = stdin.readLine();
        if (userDemoCommand.equals("exit"))
          return;
        int demoNbr = 0;
        try {
          demoNbr = Integer.parseInt(userDemoCommand);
        }
        catch (NumberFormatException e) {
          Log.current.println("Not a valid demo number");
          continue;
        }
        switch (demoNbr) {
          case 1:
            demo1();
            break;
          case 2:
            demo2();
            break;
          case 3:
            demo3();
            break;
          case 4:
            demo4();
            break;
          case 5:
            demo5();
            break;
          case 6:
            demo6();
            break;
          case 7:
            demo7();
            break;
          case 8:
            demo8();
            break;
          case 9:
            demo9();
            break;
          case 10:
            demo10();
            break;
          case 11:
            demo11();
            break;
          case 12:
            demo12();
            break;
          case 13:
            demo13();
            break;
          case 14:
            demo14();
            break;
          case 15:
            demo15();
            break;
          case 16:
            demo16();
            break;
          case 17:
            demo17();
            break;
          default:
            Log.current.println("Not a valid demo number");
        }
      }
    }
    catch (Exception e) {
      Log.current.errorPrintln(e.getMessage());
      Log.current.printStackTrace(e);
    }
  }
  
  /**
   * Provides the main method for the api demo application
   */
  public static void main(String[] args) {
    ApiDemo apiDemo = new ApiDemo();
    apiDemo.demoInteraction();
    // kill all threads
    System.exit(0);
  }
  
  /**
   * Demonstrates getKnownConstantByName api function.
   */
  protected void demo1() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getKnownConstantByName api function.\n");
    CycFort snowSkiing = cycAccess.getKnownConstantByName("SnowSkiing");
    Log.current.println("\nThe obtained constant is " + snowSkiing.cyclify());
  }
  
  /**
   * Demonstrates getConstantGuid api function.
   */
  protected void demo2() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getConstantGuid api function.\n");
    Guid unitedStatesOfAmericaGuid = cycAccess.getConstantGuid("UnitedStatesOfAmerica");
    Log.current.println("\nThe obtained guid is " + unitedStatesOfAmericaGuid);
  }
  
  /**
   * Demonstrates getComment api function.
   */
  protected void demo3() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getComment api function.\n");
    String comment = cycAccess.getComment(cycAccess.getKnownConstantByName("bordersOn"));
    Log.current.println("\nThe obtained comment is:\n" + comment);
  }
  
  /**
   * Demonstrates getIsas api function.
   */
  protected void demo4() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getIsas api function.\n");
    CycList isas = cycAccess.getIsas(cycAccess.getKnownConstantByName("BillClinton"));
    Log.current.println("\nThe obtained isas are:\n" + isas.cyclify());
  }
  
  /**
   * Demonstrates getGenls api function.
   */
  protected void demo5() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getGenls api function.\n");
    CycList genls = cycAccess.getGenls(cycAccess.getKnownConstantByName("Dog"));
    Log.current.println("\nThe obtained direct genls are:\n" + genls.cyclify());
  }
  
  /**
   * Demonstrates getArity api function.
   */
  protected void demo6() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getArity api function.\n");
    int arity = cycAccess.getArity(cycAccess.getKnownConstantByName("likesAsFriend"));
    Log.current.println("\nThe obtained arity is " + arity);
  }
  
  /**
   * Demonstrates arg1Isas api function.
   */
  protected void demo7() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating arg1Isas api function.\n");
    CycList arg1Isas = cycAccess.getArg1Isas(cycAccess.getKnownConstantByName("performedBy"));
    Log.current.println("\nThe obtained arg1Isas are:\n" + arg1Isas.cyclify());
  }
  
  /**
   * Demonstrates getArgNGenls api function.
   */
  protected void demo8() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getArgNGenls api function.\n");
    CycList argNGenls = cycAccess.getArgNGenls(cycAccess.getKnownConstantByName("skillCapableOf"), 2);
    Log.current.println("\nThe obtained getArgNGenls are:\n" + argNGenls.cyclify());
  }
  
  /**
   * Demonstrates getParaphrase (with quantified formula) api function.
   */
  protected void demo9() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating getParaphrase api function.\n");
    CycFormulaSentence formula = cycAccess.makeCycSentence("(#$forAll ?THING (#$isa ?Thing #$Thing))");
    String paraphrase = cycAccess.getParaphrase(formula);
    Log.current.println("\nThe obtained paraphrase for\n" + formula + "\nis:\n" + paraphrase);
  }
  
  /**
   * Demonstrates getParaphrase (with quantified formula) api function.
   */
  protected void demo10() throws IOException, UnknownHostException, CycApiException {
    if (cycAccess.isOpenCyc()) {
      Log.current.println("\nThis demo is not available in OpenCyc");
    }
    else {
      Log.current.println("Demonstrating getParaphrase api function.\n");
      CycFormulaSentence formula = cycAccess.makeCycSentence(
      "(#$thereExists ?PLANET\n" +
      "  (#$and\n" +
      "    (#$isa ?PLANET #$Planet)\n" +
      "    (#$orbits ?PLANET #$Sun)))");
      String paraphrase = cycAccess.getParaphrase(formula);
      Log.current.println("\nThe obtained paraphrase for\n" + formula + "\nis:\n" + paraphrase);
    }
  }
  
  /**
   * Demonstrates getImpreciseParaphrase (with quantified formula) api function.
   */
  protected void demo11() throws IOException, UnknownHostException, CycApiException {
    if (cycAccess.isOpenCyc()) {
      Log.current.println("\nThis demo is not available in OpenCyc");
    }
    else {
      Log.current.println("Demonstrating getImpreciseParaphrase api function.\n");
      CycFormulaSentence formula = cycAccess.makeCycSentence(
      "(#$forAll ?PERSON1\n" +
      "  (#$implies\n" +
      "    (#$isa ?PERSON1 #$Person)\n" +
      "    (#$thereExists ?PERSON\n" +
      "      (#$and\n" +
      "        (#$isa ?PERSON2 #$Person)\n" +
      "        (#$loves ?PERSON1 ?PERSON2)))))");
      String paraphrase = cycAccess.getImpreciseParaphrase(formula);
      Log.current.println("\nThe obtained imprecise paraphrase for\n" + formula + "\nis:\n" + paraphrase);
    }
  }
  
  /**
   * Demonstrates usage of CycNart and getInstanceSiblings api function.
   */
  protected void demo12() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating CycNart and getInstanceSiblings api function.\n");
    CycNart usGovernment = new CycNart(cycAccess.getKnownConstantByName("GovernmentFn"),
    cycAccess.getKnownConstantByName("UnitedStatesOfAmerica"));
    CycList siblings = cycAccess.getInstanceSiblings(usGovernment);
    Log.current.println("\nThe obtained instance sibling terms of " + usGovernment + "\nare:\n" + siblings.cyclify());
  }
  
  /**
   * Demonstrates usage of isQueryTrue api function.
   */
  protected void demo13() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating isQueryTrue api function.\n");
    CycFormulaSentence gaf = cycAccess.makeCycSentence("(#$likesAsFriend #$BillClinton #$JimmyCarter)");
    CycFort mt = cycAccess.getKnownConstantByName("PeopleDataMt");
    InferenceParameters queryProperties = new DefaultInferenceParameters(cycAccess);
    boolean isQueryTrue = cycAccess.isQueryTrue(gaf, mt, queryProperties);
    if (isQueryTrue)
      Log.current.println("\nThe assertion\n" + gaf + "\nis true in the " + mt.cyclify());
    else
      Log.current.println("\nThe assertion\n" + gaf + "\nis not known to be true in the " + mt.cyclify());
  }
  
  /**
   * Demonstrates usage of the assertGaf api function.
   */
  protected void demo14() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating usage of the assertGaf api function.\n");
    CycFort mt = cycAccess.getKnownConstantByName("PeopleDataMt");
    CycFormulaSentence gaf = cycAccess.makeCycSentence("(#$likesAsFriend #$BillClinton #$JimmyCarter)");
    cycAccess.assertGaf(gaf, mt);
  }
  
  /**
   * Demonstrates usage of the unassertGaf api function.
   */
  protected void demo15() throws IOException, UnknownHostException, CycApiException {
    Log.current.println("Demonstrating usage of the unassertGaf api function.\n");
    CycFort mt = cycAccess.getKnownConstantByName("PeopleDataMt");
    CycFormulaSentence gaf = cycAccess.makeCycSentence("(#$likesAsFriend #$BillClinton #$JimmyCarter)");
    cycAccess.unassertGaf(gaf, mt);
  }
  
  /**
   * Demonstrates usage of the rkfPhraseReader api function.
   */
  protected void demo16() throws IOException, UnknownHostException, CycApiException {
    if (cycAccess.isOpenCyc()) {
      Log.current.println("\nThis demo is not available in OpenCyc");
    }
    else {
      Log.current.println("Demonstrating usage of the rkfPhraseReader api function.\n");
      String phrase = "penguins";
      CycFort inferencePsc =
      cycAccess.getKnownConstantByGuid("bd58915a-9c29-11b1-9dad-c379636f7270");
      CycFort rkfEnglishLexicalMicrotheoryPsc =
      cycAccess.getKnownConstantByGuid("bf6df6e3-9c29-11b1-9dad-c379636f7270");
      CycList parsingExpression = cycAccess.rkfPhraseReader(phrase,
      rkfEnglishLexicalMicrotheoryPsc,
      inferencePsc);
      Log.current.println("the result of parsing the phrase \"" + phrase + "\" is\n" + parsingExpression);
    }
  }
  
  /**
   * Demonstrates usage of the generateDisambiguationPhraseAndTypes api function.
   */
  protected void demo17() throws IOException, UnknownHostException, CycApiException {
    if (cycAccess.isOpenCyc()) {
      Log.current.println("\nThis demo is not available in OpenCyc");
    }
    else {
      Log.current.println("Demonstrating usage of the generateDisambiguationPhraseAndTypes api function.\n");
      CycFort mt = cycAccess.getKnownConstantByName("PeopleDataMt");
      CycList objects = cycAccess.makeCycList("(#$Penguin #$PittsburghPenguins)");
      CycList disambiguationExpression = cycAccess.generateDisambiguationPhraseAndTypes(objects);
      Log.current.println("the result of disambiguating the objects \"" + objects.cyclify() + "\" is\n" +
      disambiguationExpression);
    }
  }
  
}
