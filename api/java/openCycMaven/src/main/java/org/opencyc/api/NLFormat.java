package org.opencyc.api;

//// External Imports
import java.io.IOException;
import java.net.UnknownHostException;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;

//// Internal Imports
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opencyc.cycobject.*;

/** A class to handle translation between NL strings and Cyc objects.
 *
 * @author baxter
 */
public class NLFormat extends Format {

  /** An enum of values that will control the format of the output to generation calls (HTML or simple (Unicode) text) */
  public enum GenerationMode {

    /** Return simple (Unicode) text from generation calls.*/
    Text,
    /**Return HTML strings from generation calls. */
    HTML
  }
  /** Allows Cyc to pick a standard, default value for a parameter. */
  public static final CycSymbol DEFAULT = CycObjectFactory.makeCycSymbol(":DEFAULT");
  protected static final CycSymbol TEXT = CycObjectFactory.makeCycSymbol(":TEXT");
  protected static final CycSymbol HTML = CycObjectFactory.makeCycSymbol(":HTML");
  protected Locale locale = Locale.getDefault();
  protected static final String GENERATE_TEXT_W_SENTENTIAL_FORCE = "GENERATE-TEXT-W/SENTENTIAL-FORCE";
  protected static final String GENERATE_DISAMBIGUATION_PHRASES = "GENERATE-DISAMBIGUATION-PHRASES";
  private static final CycSymbol LEXICON_LOOKUP_MT = CycObjectFactory.makeCycSymbol("*LEXICON-LOOKUP-MT*");
  private static final CycSymbol PPH_DOMAIN_MT = CycObjectFactory.makeCycSymbol("*PPH-DOMAIN-MT*");
  private static final CycSymbol DENOTS_OF_STRING = CycObjectFactory.makeCycSymbol("denots-of-string");
  private static final String WITH_PRECISE_PARAPHRASE_ON = "WITH-PRECISE-PARAPHRASE-ON";
  private static final String OMITTING_HYPERLINKS_IN_PPH_HTML_MODE = "OMITTING-HYPERLINKS-IN-PPH-HTML-MODE";
  private static CycObject defaultDomainMt = null;
  private CycAccess cyc;
  protected CycObject formatLanguageMt = DEFAULT;
  protected CycObject parseLanguageMt = DEFAULT;
  protected CycObject domainMt = DEFAULT;
  protected boolean shouldDisambiguate = true;
  protected boolean precise = false;
  protected boolean useBulletsInHTMLMode = false;
  private GenerationMode mode = GenerationMode.Text;
  /** A "force" parameter. Tells Cyc to generate a question with the specified semantics. */
  protected static final CycSymbol QUESTION = CycObjectFactory.makeCycSymbol(":QUESTION");
  /** A "force" parameter. Tells Cyc to generate a question with the specified semantics. */
  protected static final CycSymbol INTERROGATIVE = QUESTION;
  /** A "force" parameter. Tells Cyc to generate a non-illocutionary with the specified semantics. */
  protected static final CycSymbol NONE = CycObjectFactory.makeCycSymbol(":NONE");
  /** A "force" parameter. Tells Cyc to generate a declarative sentence with the specified semantics. */
  protected static final CycSymbol DECLARATIVE = CycObjectFactory.makeCycSymbol(":DECLARATIVE");
  /** A "force" parameter. Tells Cyc to generate a command with the specified semantics. */
  protected static final CycSymbol IMPERATIVE = CycObjectFactory.makeCycSymbol(":IMPERATIVE");
  private Collection<CycObject> requiredIsas = new HashSet<CycObject>();
  private Collection<CycObject> requiredGenls = new HashSet<CycObject>();

  protected NLFormat(CycAccess cyc) {
    this.cyc = cyc;
  }

  /** Get a default NLFormat instance that will use the specified CycAccess. */
  public static NLFormat getInstance(CycAccess cyc) {
    return getInstance(Locale.getDefault(), cyc);
  }

  /** Get an NLFormat instance for the specified Locale that will use the specified CycAccess. */
  public static NLFormat getInstance(Locale locale, CycAccess cyc) {
    final NLFormat nlf = new NLFormat(cyc);
    nlf.setLocale(locale);
    return nlf;
  }

  @Override
  public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
    final String command = buildFormatCommand(obj);
    try {
      toAppendTo.append(getCycAccess().converseString(command));
    } catch (Exception ex) {
      throw new RuntimeException("Exception formatting " + obj, ex);
    }
    return toAppendTo;
  }

  private String buildFormatCommand(Object obj) {
    String command = getBasicFormatCommand(obj);
    if (precise) {
      command = SubLAPIHelper.makeSubLStmt(WITH_PRECISE_PARAPHRASE_ON,
              new SubLAPIHelper.AsIsTerm(command));
    }
    //by default, don't emit links in generated NL.
    if (getMode().equals(GenerationMode.HTML)) {
      command = SubLAPIHelper.makeSubLStmt(OMITTING_HYPERLINKS_IN_PPH_HTML_MODE,
              new SubLAPIHelper.AsIsTerm(command));
    }
    return command;
  }

  private String getBasicFormatCommand(Object obj) {
    return SubLAPIHelper.makeSubLStmt(GENERATE_TEXT_W_SENTENTIAL_FORCE,
            obj, DEFAULT, DEFAULT, getFormatLanguageMt(), getDomainMt(), getCycSymbol(getMode()));
  }
  
  /** Format multiple items with a single call.  This is particularly useful if disambiguating 
   * (accomplished by calling <code>setDisambiguate(True)</code>),
   * as that will force the format code to try to generate each element of <code>objList</code> differently, even if they all have a natural
   * format that would be the same.  For example, an attempt to generate #$Horse, #$Horse-Domesticated, and (#$MeatFn&nbsp;#$Horse) individually would
   * likely yield "horse", "horse", and "horse".  Using this method with <code>setDisambiguate</code> set to <code>True</code> would yield something
   * more like "horse", "domesticated horse", and "horse meat".  The resulting output List<String> will be in the
   * same order as the input.
   */
  public List<String> formatMultiple(List<? extends Object> objList) {
    final String command = buildFormatMultipleCommand(objList);
    List<String> resultList = new ArrayList<String>();
    CycList rawResult;
    try {
      rawResult = getCycAccess().converseList(command);
    } catch (Exception ex) {
      throw new RuntimeException("Exception formatting " + objList, ex);
    }
    Map<Object, String> resultMap = rawResult.toMap();
    for (Object oneInput : objList) {
      resultList.add(resultMap.get(oneInput));
    }
    return resultList;
  }

  protected String buildFormatMultipleCommand(List<? extends Object> objList) {
    String command = null;
    if (shouldDisambiguate) {
      command = SubLAPIHelper.makeSubLStmt(GENERATE_DISAMBIGUATION_PHRASES,
              new CycList(objList), DEFAULT, DEFAULT, getFormatLanguageMt(), getDomainMt(), getCycSymbol(getMode()));
    } else {
      command = "(list ";
      for (Object obj : objList) {
        command += "(cons " + DefaultCycObject.stringApiValue(obj) + " ";
        command += getBasicFormatCommand(obj);
        command += ")";
      }
      command += ")";
    }
    if (precise) {
      command = SubLAPIHelper.makeSubLStmt(WITH_PRECISE_PARAPHRASE_ON,
              new SubLAPIHelper.AsIsTerm(command));
    }
    //by default, don't emit links in generated NL.
    if (getMode().equals(GenerationMode.HTML)) {
      command = SubLAPIHelper.makeSubLStmt(OMITTING_HYPERLINKS_IN_PPH_HTML_MODE,
              new SubLAPIHelper.AsIsTerm(command));
    }
    return command;
  }

  @Override
  public Object parseObject(String source, ParsePosition pos) {
    final Collection<? extends Object> results = parseObjects(source, pos);
    return (results == null || results.isEmpty()) ? null : results.iterator().next();
  }

  /** @return the language microtheory for NL generation. */
  public CycObject getFormatLanguageMt() {
    if (DEFAULT.equals(formatLanguageMt)) {
      try {
        String command = "(denots-mt-for-language (get-language-for-code " + DefaultCycObject.stringApiValue(locale.getLanguage()) + "))";
        formatLanguageMt = cyc.converseCycObject(command);
      } catch (UnknownHostException ex) {
        Logger.getLogger(NLFormat.class.getName()).log(Level.SEVERE, null, ex);
        throw new RuntimeException("Unable to retrieve language Mt for " + locale.getLanguage(), ex);
      } catch (IOException ex) {
        Logger.getLogger(NLFormat.class.getName()).log(Level.SEVERE, null, ex);
        throw new RuntimeException("Unable to retrieve language Mt for " + locale.getLanguage(), ex);
      } catch (CycApiException ex) {
        Logger.getLogger(NLFormat.class.getName()).log(Level.SEVERE, null, ex);
        throw new RuntimeException("Unable to retrieve language Mt for " + locale.getLanguage(), ex);
      }
    }
    return formatLanguageMt;
  }

  /** @return the language microtheory for NL parsing. */
  public Object getParseLanguageMt() {
    if (DEFAULT.equals(parseLanguageMt)) {
      return locale.getLanguage();
    } else {
      return parseLanguageMt;
    }
  }

  /** @return the domain microtheory. */
  public Object getDomainMt() {
    if (domainMt == DEFAULT) {
      if (defaultDomainMt == null) {
        try {
          defaultDomainMt = (cyc.converseCycObject("(eval " + PPH_DOMAIN_MT.stringApiValue() + ")"));
        } catch (Exception e) {
          throw new RuntimeException("Unable to find PPH domain Mt", e);
        }
      }
      return defaultDomainMt;
    }
    return domainMt;
  }

  /** @return the mode for generation (text or HTML). */
  public GenerationMode getMode() {
    return mode;
  }

  public Locale getLocale() {
    return locale;
  }

  public Collection<CycObject> getRequiredIsas() {
    return requiredIsas;
  }

  public Collection<CycObject> getRequiredGenls() {
    return requiredGenls;
  }

  private String buildParseCommand(String denotationString) {
    String command = SubLAPIHelper.makeSubLStmt(DENOTS_OF_STRING, denotationString);
    final Object languageMt = getParseLanguageMt();
    command = SubLAPIHelper.wrapVariableBinding(command, LEXICON_LOOKUP_MT, languageMt);
    return command;
  }

  protected CycAccess getCycAccess() {
    return cyc;
  }

  /** Set the Locale, which is used to find an appropriate language microtheory. */
  public void setLocale(Locale locale) {
    this.locale = locale;
  }

  /** Set whether we should prefer precision over conciseness. */
  public void setPrecise(boolean b) {
    this.precise = b;
  }

  public boolean isPrecise() {
    return precise;
  }

  public boolean shouldDisambiguate() {
    return shouldDisambiguate;
  }

  /** Set the language microtheory for NL generation. */
  public void setFormatLanguageMt(CycObject languageMt) {
    this.formatLanguageMt = languageMt;
  }

  /** Set the language microtheory for NL parsing. */
  public void setParseLanguageMt(CycObject languageMt) {
    this.parseLanguageMt = languageMt;
  }

  /** The Mt from which any queries against Cyc knowledge will be performed.
   */
  public void setDomainMt(CycObject domainMt) {
    this.domainMt = domainMt;
  }

  /** Set the mode (html or text). */
  public void setMode(GenerationMode mode) {
    this.mode = mode;
  }

  /** Should the different elements in a call to <code>formatMultiple</code> be generated distinctly from each other?  Defaults to <code>True</code>. <p/>
   * If <code>True</code>, for format code will try to generate the objects so that the output for each is distinct. <p/>
   * If <code>False</code>, each element of the input will be formatted independently of the others, with no attempt to keep them
   * distinct. See {@link #formatMultiple}.
   */
  public void setDisambiguate(boolean shouldDisambiguate) {
    this.shouldDisambiguate = shouldDisambiguate;
  }

  /** Add an isa restriction on parse results.  
   * Any results from <code>parseObject</code> will be required to be a known 
   * instance of <code>isa</code> in the domain Mt (see {@link #setDomainMt}). 
   * <p/>
   * Not yet fully respected in the codebase.
   */
  public void addRequiredIsa(CycObject isa) {
    requiredIsas.add(isa);
  }

  /** Add a genls restriction on parse results.  
   * Any results from <code>parseObject</code> will be required to be a known 
   * specialization of <code>genl</code> in the domain Mt (see {@link #setDomainMt}). 
   * <p/>
   * Not yet fully respected in the codebase.
   */
  public void addRequiredGenl(CycObject genl) {
    requiredGenls.add(genl);
  }

  /** Parse denotationString into Cyc terms. */
  public List<? extends Object> parseObjects(String denotationString) {
    final String command = buildParseCommand(denotationString);
    try {
      return getCycAccess().converseList(command);
    } catch (Exception ex) {
      throw new RuntimeException("Exception parsing " + denotationString, ex);
    }
  }

  /** Parse denotationString into Cyc terms, starting at the specified position.
   @return collection of Objects denoted by the substring starting at the specified position.
   * 
   * If parsing succeeds, the index pos will be set to the index after the last consumed character.
   * If parsing fails, the error index of pos will be set.
   */
  public Collection<? extends Object> parseObjects(String denotationString, ParsePosition pos) {
    if (pos == null) {
      throw new NullPointerException();
    }
    final List<? extends Object> result = parseObjects(denotationString.substring(pos.getIndex()));
    if (result == null || result.isEmpty()) {
      pos.setErrorIndex(pos.getIndex());
    } else {
      pos.setIndex(denotationString.length());
    }
    return result;
  }

  protected static CycSymbol getCycSymbol(GenerationMode mode) {
    switch (mode) {
      case Text:
        return TEXT;
      case HTML:
        return NLFormat.HTML;
    }
    throw new UnsupportedOperationException("Can't turn " + mode + " into a CycSymbol.");
  }
}
