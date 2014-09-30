package org.opencyc.api;

//// External Imports
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.opencyc.cycobject.*;
import org.opencyc.util.LRUCache;
import org.opencyc.xml.TextUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Provides the way to create cyc objects and reuse previously cached instances.<br>
 *
 * All methods are static.<p>
 *
 * Collaborates with the <tt>CycConnection</tt> class which manages the api connections.
 *
 * @version $Id: CycObjectFactory.java 140074 2012-05-18 14:24:36Z daves $
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
public class CycObjectFactory {

  /**
   * Least Recently Used Cache of CycSymbols, so that a reference to an existing <tt>CycSymbol</tt>
   * is returned instead of constructing a duplicate.
   */
  protected final static LRUCache cycSymbolCache = new LRUCache(500, 500, true);
  /**
   * Built in CycSymbols.
   */
  public static final CycSymbol t = makeCycSymbol("T", false);
  public static final CycSymbol nil = makeCycSymbol("NIL", false);
  public static final CycSymbol quote = makeCycSymbol("QUOTE", false);
  public static final CycSymbol backquote = makeCycSymbol("`", false);
  public static final CycSymbol cons = makeCycSymbol("CONS", false);
  public static final CycSymbol dot = makeCycSymbol(".", false);
  public static final CycSymbol nul = makeCycSymbol(":NULL", false);
  /** the free constant */
  public static CycConstant FREE_CONSTANT = CycConstant.makeFreeConstant();
  /** the invalid constant */
  public static CycConstant INVALID_CONSTANT = CycConstant.makeInvalidConstant();
  /** the invalid nart */
  public static CycNart INVALID_NART = CycNart.makeInvalidNart();
  /** the invalid assertion */
  public static CycAssertion INVALID_ASSERTION = CycAssertion.makeInvalidAssertion();
  /**
   * The api command which is intercepted by the CycProxy agent to close the CycAccess object
   * associated with the connection between this agent and the particular cyc image.
   */
  public static final CycList END_CYC_CONNECTION = (new CycList(makeCycSymbol("end-cyc-connection")));
  /**
   * the default size of the constant cache by name
   */
  public static final int CONSTANT_CACHE_BY_NAME_SIZE = 10000;
  /**
   * Least Recently Used Cache of CycConstants, so that a reference to an existing <tt>CycConstant</tt>
   * is returned instead of constructing a duplicate.  Indexed via the name, so is optimised for the ascii api.
   */
  protected static LRUCache cycConstantCacheByName = new LRUCache(1000, CONSTANT_CACHE_BY_NAME_SIZE, true);
  /**
   * the default size of the constant cache by GUID
   */
  public static final int CONSTANT_CACHE_BY_GUID_SIZE = 10000;
  /**
   * Least Recently Used Cache of CycConstants, so that a reference to an existing <tt>CycConstant</tt>
   * is returned instead of constructing a duplicate.  Indexed via the guid.
   */
  protected static LRUCache cycConstantCacheByGuid = new LRUCache(CONSTANT_CACHE_BY_GUID_SIZE, CONSTANT_CACHE_BY_GUID_SIZE, true);

  /**
   * the default size of the CycNumber cache
   */
  public static final int NUMBER_CACHE_SIZE = 500;
  /**
   * Least Recently Used Cache of CycNumbers, so that a reference to an existing <tt>CycNumber</tt>
   * is returned instead of constructing a duplicate.
   */
  protected static LRUCache<Number, CycNumber> cycNumberCache =
          new LRUCache<Number, CycNumber>(NUMBER_CACHE_SIZE, NUMBER_CACHE_SIZE, true);
 
  /**
   * Least Recently Used Cache of guids, so that a reference to an existing <tt>Guid</tt>
   * is returned instead of constructing a duplicate.
   */
  protected static LRUCache guidCache = new LRUCache(500, 500, true);
    /**
     * the default size of the variable cache
     */
    public static final int VARIABLE_CACHE_SIZE = 500;
    
            /**
         * A variable name suffix used to make unique names.
         */
        protected static int suffix = 1;
        
        
  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   *
   * @param symbolName a <tt>String</tt> name.
   */
  public static CycSymbol makeCycSymbol(String symbolNameAnyCase) {
    String symbolName = CycSymbol.canonicalizeName(symbolNameAnyCase);
    CycSymbol cycSymbol = (CycSymbol) cycSymbolCache.get(symbolName);
    if (cycSymbol == null) {
      cycSymbol = new CycSymbol(symbolName);
      cycSymbolCache.put(symbolName, cycSymbol);
    }
    return cycSymbol;
  }

  public static CycSymbol makeCycSymbol(String packageNameCaseSensitive, String symbolNameCaseSensitive) {
    CycSymbol cycSymbol = null;
    String symbolName = symbolNameCaseSensitive;
    if ((packageNameCaseSensitive != null) && (!"".equals(packageNameCaseSensitive))) {
      symbolName = packageNameCaseSensitive + ":" + symbolNameCaseSensitive;
    }
    cycSymbol = (CycSymbol) cycSymbolCache.get(symbolNameCaseSensitive);
    if (cycSymbol == null) {
      cycSymbol = new CycSymbol(packageNameCaseSensitive, symbolNameCaseSensitive);
      cycSymbolCache.put(symbolName, cycSymbol);
    }
    return cycSymbol;
  }

  /**
   * Constructs a new <tt>CycSymbol</tt> object.
   *
   * @param symbolName a <tt>String</tt> name.
   */
  public static CycSymbol makeCycSymbol(String symbolNameAnyCase, boolean shouldQuote) {
    String symbolName = CycSymbol.canonicalizeName(symbolNameAnyCase);
    CycSymbol cycSymbol = (CycSymbol) cycSymbolCache.get(symbolName);
    if (cycSymbol == null) {
      cycSymbol = new CycSymbol(symbolName, shouldQuote);
      cycSymbolCache.put(symbolName, cycSymbol);
    }
    return cycSymbol;
  }

  public static CycSymbol makeCycBoolean(final boolean b) {
    return (b) ? t : nil;
  }

  /**
   * Resets the <tt>CycSymbol</tt> cache.
   */
  public static void resetCycSymbolCache() {
    cycSymbolCache.clear();
    cycSymbolCache.put(CycSymbol.canonicalizeName("NIL"), nil);
    cycSymbolCache.put(CycSymbol.canonicalizeName("QUOTE"), quote);
    cycSymbolCache.put(CycSymbol.canonicalizeName("CONS"), cons);
    cycSymbolCache.put(CycSymbol.canonicalizeName("."), dot);
    cycSymbolCache.put(CycSymbol.canonicalizeName("T"), t);
    cycSymbolCache.put(CycSymbol.canonicalizeName("`"), backquote);
    cycSymbolCache.put(CycSymbol.canonicalizeName(":NULL"), nul);
  }

  /** The number of symbols that should be in a freshly reset cache. */
  public static final int RESET_SYMBOL_CACHE_SIZE = 7;

  /** Return the :FREE constant (a singleton).
   *
   * @return the :FREE constant (a singleton)
   */
  public static CycConstant getFreeConstant() {
    return FREE_CONSTANT;
  }

  /**
   * Retrieves the <tt>CycSymbol</tt> with <tt>symbolName</tt>,
   * returning null if not found in the cache.
   *
   * @return a <tt>CycSymbol</tt> if found in the cache, otherwise <tt>null</tt>
   */
  public static CycSymbol getCycSymbolCache(String symbolName) {
    return (CycSymbol) cycSymbolCache.get(symbolName);
  }

  /**
   * Removes the <tt>CycSymbol</tt> from the cache if it is contained within.
   */
  public static void removeCycSymbolCache(CycSymbol cycSymbol) {
    Object element = cycSymbolCache.get(cycSymbol.toString());
    if (element != null) {
      cycSymbolCache.put(cycSymbol.toString(), null);
    }
  }

  /**
   * Returns the size of the <tt>Guid</tt> object cache.
   *
   * @return an <tt>int</tt> indicating the number of <tt>CycSymbol</tt> objects in the cache
   */
  public static int getCycSymbolCacheSize() {
    return cycSymbolCache.size();
  }

  /**
   * Resets all the caches.
   */
  public static void resetCaches() {
    resetCycConstantCaches();
    resetCycSymbolCache();
    resetCycVariableCache();
    resetGuidCache();
  }

  /**
   * Resets the Cyc constant caches.
   */
  public static void resetCycConstantCaches() {
    cycConstantCacheByName = new LRUCache(CONSTANT_CACHE_BY_NAME_SIZE, CONSTANT_CACHE_BY_NAME_SIZE, true);
    cycConstantCacheByGuid = new LRUCache(CONSTANT_CACHE_BY_GUID_SIZE, CONSTANT_CACHE_BY_NAME_SIZE, true);
  }

  /**
   * Adds the <tt>CycConstant<tt> to the cache by name and by guid
   * @param cycConstant the Cyc constant to be added to the cache
   */
  public static void addCycConstantCache(final CycConstant cycConstant) {
    if (cycConstant.name != null && cycConstant.guid != null) {
      cycConstantCacheByName.put(cycConstant.getName(), cycConstant);
      cycConstantCacheByGuid.put(cycConstant.getGuid().toString(), cycConstant);
    }
  }

  /**
   * Retrieves the <tt>CycConstant<tt> with name, returning null if not found in the cache.
   */
  public static CycConstant getCycConstantCacheByName(String name) {
    return (CycConstant) cycConstantCacheByName.get(name);
  }

  /**
   * Retrieves the <tt>CycConstant<tt> with guid, returning null if not found in the cache.
   */
  public static CycConstant getCycConstantCacheByGuid(Guid guid) {
    return (CycConstant) cycConstantCacheByGuid.get(guid.toString());
  }

  /**
   * Removes the <tt>CycConstant</tt> from the caches if it is contained within.
   *
   * @param cycConstant the Cyc constant
   */
  public static void removeCaches(final CycConstant cycConstant) {
    if (cycConstant.name != null) {
      Object element = cycConstantCacheByName.get(cycConstant.name);
      if (element != null) {
        cycConstantCacheByName.put(cycConstant.name, null);
      }
    }
    if (cycConstant.guid != null) {
      Object element = cycConstantCacheByGuid.get(cycConstant.guid);
      if (element != null) {
        cycConstantCacheByGuid.put(cycConstant.guid, null);
      }
    }
  }

  /**
   * Returns the size of the <tt>CycConstant</tt> object cache by id.
   *
   * @return an <tt>int</tt> indicating the number of <tt>CycConstant</tt> objects in the cache by id
   */
  public static int getCycConstantCacheByNameSize() {
    return cycConstantCacheByName.size();
  }

    /**
     * Constructs a new <tt>CycVariable</tt> object using the variable name.
     *
     * @param name a <tt>String</tt> name.
     */
    public static CycVariable makeCycVariable(String name) {
        /*
         * if (name.startsWith("?")) name = name.substring(1);
         */
        return CycVariableFactory.get(name);
    }

    /**
     * Constructs a new <tt>CycVariable</tt> object by suffixing the given
     * variable.
     *
     * @param modelCycVariable a <tt>CycVariable</tt> to suffix
     */
    public static CycVariable makeUniqueCycVariable(CycVariable modelCycVariable) {
        return CycVariableFactory.get(modelCycVariable.name + "-" + suffix++);
    }
    
    private static Pattern variableNumericSuffixPattern = Pattern.compile("-([0-9]*)$");

    /**
     * Return a CycVariable that is guaranteed to be different from all the
     * variables in existingVariables.  If <code>modelCycVariable</code> is already
     * different from those in <code>existingVariables</code>, it may be returned.
     * 
     * @param modelCycVariable
     * @param existingVariables
     * @return 
     */
    public static CycVariable makeUniqueCycVariable(CycVariable modelCycVariable,
            Collection<CycObject> existingVariables) {
        if (!existingVariables.contains(modelCycVariable)) {
            return modelCycVariable;
        } else {
            CycVariable newVar;
            Matcher matcher = variableNumericSuffixPattern.matcher(modelCycVariable.name);
            if (matcher.find()) {
                Integer num = Integer.parseInt(matcher.group(1)) + 1;
                do {
                    newVar = CycVariableFactory.get(modelCycVariable.name.substring(0, matcher.start() + 1) + num++);
                } while (existingVariables.contains(newVar));
            } else {
                Integer num = 1;
                do {
                    newVar = CycVariableFactory.get(modelCycVariable.name + "-" + num++);
                } while (existingVariables.contains(newVar));
            }
            return newVar;
        }
    }

    /**
     * Resets the <tt>CycVariable</tt> cache.
     */
    @Deprecated
    public static void resetCycVariableCache() {
        CycVariableFactory.reset();
    }

    /**
     * Adds the <tt>CycVariable<tt> to the cache.
     */
    @Deprecated
    public static void addCycVariableCache(CycVariable cycVariable) {
        if (cycVariable.name == null) {
            throw new RuntimeException("Invalid variable for caching " + cycVariable);
        }
        CycVariableFactory.add(cycVariable);
    }

    /**
     * Retrieves the <tt>CycVariable</tt> with <tt>name</tt>, returning null if
     * not found in the cache.
     *
     * @return a <tt>CycVariable</tt> if found in the cache, otherwise
     * <tt>null</tt>
     */
    @Deprecated
    public static CycVariable getCycVariableCache(String name) {
        return CycVariableFactory.get(name);
    }

    /**
     * Removes the <tt>CycVariable</tt> from the cache if it is contained
     * within.
     */
    @Deprecated
    public static void removeCycVariableCache(CycVariable cycVariable) {
        CycVariableFactory.remove(cycVariable);
    }

    /**
     * Returns the size of the <tt>CycVariable</tt> object cache.
     *
     * @return an <tt>int</tt> indicating the number of <tt>CycVariable</tt>
     * objects in the cache
     */
    public static int getCycVariableCacheSize() {
        return CycVariableFactory.size();
    }

  /**
   * Constructs a new <tt>CycNumber</tt> object using the variable name.
   *
   * @param name a <tt>String</tt> name.
   */
  public static CycNumber makeCycNumber(Number num) {
    CycNumber cycNumber = (CycNumber) cycNumberCache.get(num);
    if (cycNumber == null) {
      cycNumber = new CycNumber(num);
      cycNumberCache.put(num, cycNumber);
    }
    return cycNumber;
  }

  /**
   * Resets the <tt>CycNumber</tt> cache.
   */
  public static void resetCycNumberCache() {
    cycNumberCache = new LRUCache(NUMBER_CACHE_SIZE, NUMBER_CACHE_SIZE, true);
  }

  /**
   * Adds the <tt>CycNumber<tt> to the cache.
   */
  public static void addCycNumberCache(CycNumber cycNumber) {
    if (cycNumber.getNumber() == null) {
      throw new RuntimeException("Invalid number for caching " + cycNumber);
    }
    cycNumberCache.put(cycNumber.getNumber().doubleValue(), cycNumber);
  }

  /**
   * Retrieves the <tt>CycNumber</tt> with <tt>num</tt>,
   * returning null if not found in the cache.
   *
   * @return a <tt>CycVariable</tt> if found in the cache, otherwise
   * <tt>null</tt>
   */
  public static CycNumber getCycNumberCache(Double num) {
    return (CycNumber) cycNumberCache.get(num);
  }

  /**
   * Removes the <tt>CycVariable</tt> from the cache if it is contained within.
   */
  public static void removeCycNumberCache(CycNumber cycNumber) {
    Object element = cycNumberCache.get(cycNumber.getNumber().doubleValue());
    if (element != null) {
      cycNumberCache.put(cycNumber.getNumber().doubleValue(), null);
    }
  }

  /**
   * Returns the size of the <tt>CycNumber</tt> object cache.
   *
   * @return an <tt>int</tt> indicating the number of <tt>CycVariable</tt> objects in the cache
   */
  public static int getCycNumberCacheSize() {
    return cycNumberCache.size();
  }

  /**
   * Returns a cached <tt>Guid</tt> object or construct a new
   * Guid object from a guid string if the guid is not found in the cache.
   *
   * @param guid a <tt>String</tt> form of a GUID.
   */
  public static Guid makeGuid(String guidString) {
    Guid guid = (Guid) guidCache.get(guidString);
    if (guid == null) {
      guid = new Guid(guidString);
      addGuidCache(guid);
    }
    return guid;
  }

  public static Guid makeGuid(byte[] data) {
    final Guid guid = new Guid(data);
    final String guidString = guid.getGuidString();
    return makeGuid(guidString);
  }

  /**
   * Adds the <tt>Guid</tt> to the cache.
   */
  public static void addGuidCache(Guid guid) {
    guidCache.put(guid.getGuidString(), guid);
  }

  /**
   * Resets the <tt>Guid</tt> cache.
   */
  public static void resetGuidCache() {
    guidCache = new LRUCache(500, 500, true);
  }

  /**
   * Retrieves the <tt>Guid</tt> with <tt>guidName</tt>,
   * returning null if not found in the cache.
   *
   * @return the <tt>Guid</tt> if it is found in the cache, otherwise
   * <tt>null</tt>
   */
  public static Guid getGuidCache(String guidName) {
    return (Guid) guidCache.get(guidName);
  }

  /**
   * Removes the <tt>Guid</tt> from the cache if it is contained within.
   */
  public static void removeGuidCache(Guid guid) {
    Object element = guidCache.get(guid.getGuidString());
    if (element != null) {
      guidCache.put(guid.getGuidString(), null);
    }
  }

  /**
   * Returns the size of the <tt>Guid</tt> object cache.
   *
   * @return an <tt>int</tt> indicating the number of <tt>Guid</tt> objects in the cache
   */
  public static int getGuidCacheSize() {
    return guidCache.size();
  }

  /**
   * Unmarshalls a cyc object from an XML representation.
   *
   * @param xmlString the XML representation of the cyc object
   * @return the cyc object
   */
  public static Object unmarshall(final String xmlString) 
      throws IOException, ParserConfigurationException, SAXException {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    dbf.setNamespaceAware(true);
    DocumentBuilder db = dbf.newDocumentBuilder();
    Document doc = db.parse( new InputSource(new ByteArrayInputStream(xmlString.getBytes())));
    return unmarshallElement(doc.getDocumentElement(), doc);
  }

  /**
   * Unmarshalls a cyc object from the given element in an XML Document object.
   *
   * @param element the element representing the cyc object
   * @param document the XML document containing the element
   * @return the cyc object
   */
  protected static Object unmarshallElement(final Element element,
          final Document document) throws IOException {
    String elementName = element.getTagName();
    if (elementName.equals("guid")) {
      return unmarshallGuid(element);
    } else if (elementName.equals("symbol")) {
      return unmarshallCycSymbol(element);
    } else if (elementName.equals("variable")) {
      return unmarshallCycVariable(element);
    } else if (elementName.equals("constant")) {
      return unmarshallCycConstant(element, document);
    } else if (elementName.equals("nat")) {
      return unmarshallCycNart(element, document);
    } else if (elementName.equals("list")) {
      return unmarshallCycList(element, document);
    } else if (elementName.equals("string")) {
      return TextUtil.undoEntityReference(element.getTextContent());
    } else if (elementName.equals("integer")) {
      return new Integer(element.getTextContent().trim());
    } else if (elementName.equals("double")) {
      return new Double(element.getTextContent().trim());
    } else if (elementName.equals("byte-vector")) {
      return unmarshallByteArray(element, document);
    } else if (elementName.equals("assertion")) {
      return unmarshallCycAssertion(element);
    } else {
      throw new IOException("Invalid element name " + elementName);
    }
  }

  /**
   * Unmarshalls a Guid from the given element in an XML Document object.
   *
   * @param guidElement the guid xml element
   * @return the guid or cached reference to an existing guid object
   */
  protected static Guid unmarshallGuid(Element guidElement) {
    String guidString = guidElement.getTextContent().trim();
    Guid guid = getGuidCache(guidString);
    if (guid != null) {
      return guid;
    }
    return makeGuid(guidString);
  }

  /**
   * Unmarshalls a CycSymbol from the given element in an XML Document object.
   *
   * @param cycSymbolElement the CycSymbol xml element
   * @return the CycSymbol or cached reference to an existing CycSymbol object
   */
  protected static CycSymbol unmarshallCycSymbol(Element cycSymbolElement) {
    String symbolName = TextUtil.undoEntityReference(cycSymbolElement.getTextContent().trim());
    CycSymbol cycSymbol = getCycSymbolCache(symbolName);
    if (cycSymbol != null) {
      return cycSymbol;
    }
    return makeCycSymbol(symbolName);
  }

  /**
   * Unmarshalls a CycAssertion from the given element in an XML Document object.
   *
   * @param cycAssertionElement the CycAssertion xml element
   * @return the CycAssertion object
   */
  protected static CycAssertion unmarshallCycAssertion(Element cycAssertionElement) {
    //TODO
    CycList hlFormula = new CycList();
    CycFort mt = null;
    return new CycAssertion(hlFormula, mt);
  }

  /**
   * Unmarshalls a CycVariable from the given element in an XML Document object.
   *
   * @param cycVariableElement the CycVariable xml element
   * @return the CycVariable or cached reference to an existing CycVariable object
   */
  protected static CycVariable unmarshallCycVariable(Element cycVariableElement) {
    String name = TextUtil.undoEntityReference(cycVariableElement.getTextContent().trim());
    CycVariable cycVariable = getCycVariableCache(name);
    if (cycVariable != null) {
      return cycVariable;
    }
    return makeCycVariable(name);
  }
  
  private static Element getChild(Element parent, String tagName) {
    NodeList nodes = parent.getChildNodes();
    for (int i = 0, size = nodes.getLength(); i < size; i++) {
      Node node = nodes.item(i);
      if (node.getNodeType() == Node.ELEMENT_NODE) {
        String possibleTagName = node.getLocalName();
        if (tagName.equals(possibleTagName)) {
          return (Element) node;
        }
      }
    }
    return null;
  }
  
  private static Element getFirstChildElement(Element parent) {
    NodeList nodes = parent.getChildNodes();
    for (int i = 0, size = nodes.getLength(); i < size; i++) {
      Node node = nodes.item(i);
      if (node.getNodeType() == Node.ELEMENT_NODE) {
        return (Element) node;
      }
    }
    return null;
  }

  /**
   * Unmarshalls a CycConstant from the given element in an XML Document object.
   *
   * @param cycConstantElement the element representing the CycConstant
   * @param document the XML document containing the element
   * @param cycAccess the Cyc communications object
   * @return the CycConstant
   */
  protected static CycConstant unmarshallCycConstant(final Element cycConstantElement,
          final Document document) {
    CycConstant cycConstant = null;
    Guid guid = null;
    Node guidElement = getChild(cycConstantElement, "guid");
    if (guidElement != null) {
      guid = makeGuid(guidElement.getTextContent().trim());
      cycConstant = getCycConstantCacheByGuid(guid);
      if (cycConstant != null) {
        return cycConstant;
      }
    }
    String name = null;
    Node nameElement = getChild(cycConstantElement, "name");
    if (nameElement != null) {
      name = TextUtil.undoEntityReference(nameElement.getTextContent().trim());
      cycConstant = getCycConstantCacheByName(name);
      if (cycConstant != null) {
        return cycConstant;
      }
    }
    cycConstant = new CycConstant(name, guid);
    if (guid != null || name != null) {
      addCycConstantCache(cycConstant);
    }
    return cycConstant;
  }

  /**
   * Unmarshalls a CycNart from the given element in an XML Document object.
   *
   * @param cycNartElement the element representing the CycNart
   * @param document the XML document containing the element
   * @return the CycNart
   */
  protected static CycNart unmarshallCycNart(final Element cycNartElement,
          final Document document) throws IOException {
    CycFort functor = null;
    Node functorElement = getChild(cycNartElement, "functor");
    if (functorElement != null) {
      Element cycConstantFunctorElement = getChild(cycNartElement, "constant");
      Element cycNartFunctorElement = getChild(cycNartElement, "nat");
      if (cycConstantFunctorElement != null) {
        if (cycNartFunctorElement != null) {
          throw new IOException("Invalid CycNart functor" + functorElement);
        }
        functor = unmarshallCycConstant(cycConstantFunctorElement, document);
      } else if (cycNartFunctorElement != null) {
        functor = unmarshallCycNart(cycNartFunctorElement, document);
      } else {
        throw new IOException("Missing functor constant/nart from CycNart " + cycNartElement);
      }
    }
    NodeList argElements = cycNartElement.getElementsByTagName("arg");
    CycList arguments = new CycList();
    for (int i = 0; i < argElements.getLength(); i++) {
      if (argElements.item(i) instanceof Element) {
        Element argElement = (Element) argElements.item(i);
        arguments.add(unmarshallElement(getFirstChildElement(argElement), document));
      }
    }
    CycList nartCycList = new CycList();
    nartCycList.add(functor);
    nartCycList.addAll(arguments);
    CycNart cycNart = new CycNart(nartCycList);
    return cycNart;
  }

  /**
   * Unmarshalls a CycList from the given element in an XML Document object.
   *
   * @param cycListElement the element representing the CycList
   * @param document the XML document containing the element
   * @return the CycList
   */
  protected static CycList unmarshallCycList(final Element cycListElement,
          final Document document)
          throws IOException {
    NodeList elements = cycListElement.getChildNodes();
    CycList cycList = new CycList();
    for (int i = 0, size = elements.getLength(); i < size; i++) {
      Node node = elements.item(i);
      if (node.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) node;
        if (element.getTagName().equals("dotted-element")) {
          cycList.setDottedElement(unmarshallElement(getFirstChildElement(element), document));
        } else {
          cycList.add(unmarshallElement(element, document));
        }
      }
    }
    return cycList;
  }

  /**
   * Unmarshalls a ByteArray from the given element in an XML Document object.
   *
   * @param byteArrayElement the element representing the CycList
   * @param document the XML document containing the element
   * @return the ByteArray
   */
  protected static ByteArray unmarshallByteArray(Element byteArrayElement, Document document)
          throws IOException {
    NodeList elements = byteArrayElement.getChildNodes();
    ArrayList arrayList = new ArrayList();
    for (int i = 0, size = elements.getLength(); i < size; i++) {
      Node node = elements.item(i);
      if (node.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) node;
        if (element.getTagName().equals("byte")) {
          arrayList.add(new Byte(element.getTextContent().trim()));
        }
      }
    }
    byte[] bytes = new byte[arrayList.size()];
    for (int i = 0; i < arrayList.size(); i++) {
      bytes[i] = ((Byte) arrayList.get(i)).byteValue();
    }
    return new ByteArray(bytes);
  }
  
    private static class CycVariableFactory {


        /**
         * Least Recently Used Cache of CycVariables, so that a reference to an
         * existing <tt>CycVariable</tt> is returned instead of constructing a
         * duplicate.
         */
        protected static LRUCache cycVariableCache = new LRUCache(VARIABLE_CACHE_SIZE, VARIABLE_CACHE_SIZE, true);

        public static CycVariable get(String name) {
            if (name.startsWith("?")) {
                name = name.substring(1);
            }
            CycVariable cycVariable = (CycVariable) cycVariableCache.get(name);
            if (cycVariable == null) {
                cycVariable = new CycVariable(name);
                cycVariableCache.put(name, cycVariable);
            }
            return cycVariable;
        }
        
        public static void add(CycVariable var) {
            cycVariableCache.put(var.name, var);
        }

        public static void remove(CycVariable var) {
            cycVariableCache.remove(var.name);
        }

        public static int size() {
            return cycVariableCache.size();
        }        
        static void reset() {
                cycVariableCache = new LRUCache(VARIABLE_CACHE_SIZE, VARIABLE_CACHE_SIZE, true);
        }
    }
}
