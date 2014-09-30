/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.cycobject;

//// External Imports
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

//// Internal Imports
import org.opencyc.util.StringUtils;
import org.opencyc.xml.XMLWriter;

/**
 *
 * @author baxter
 */
public class CycFormula implements CycObject {

  /**
   * Create and return a new CycFormula whose arguments are terms.
   * CycList arguments will be converted to CycNauts or CycSentences.
   * @param terms
   */
  public CycFormula(Iterable<? extends Object> terms) {
    for (final Object arg : terms) {
      addArg(arg);
    }
  }

  /**
   * Create and return a new CycFormula with the specific functor and args.
   * CycList arguments will be converted to CycNauts or CycSentences.
   * @param functor 
   * @param args
   */
  public CycFormula(final CycDenotationalTerm functor, final Object... args) {
    this();
    addArg(functor);
    for (final Object arg : args) {
      addArg(arg);
    }
  }

  /*Build a new CycFormula from terms.
   *
   */
  public static CycFormula makeCycFormula(Object... terms) {
    final CycFormula newFormula = new CycFormula();
    for (final Object arg : terms) {
      newFormula.addArg(arg);
    }
    return newFormula;
    //return new CycFormula(CycList.makeCycList(terms));
  }

  protected CycFormula() {
  }

  /** Add arg to the end of this formula. */
  protected void addArg(final Object arg) {
    if (arg instanceof Iterable) {
      Object betterArg = CycNaut.convertIfPromising(arg);
      if (!(betterArg instanceof CycNaut)) {
        betterArg = new CycFormulaSentence((Iterable) arg);
      }
      addArgLow(betterArg);
    } else if (arg instanceof CycSymbol) {
      Object betterArg = CycVariable.convertIfPromising(arg);
      addArgLow(betterArg);
    } else {
      addArgLow(arg);
    }
  }

  private void addArgLow(final Object arg) {
    args.add(arg);
  }

  @Override
  public String toString() {
    return args.toString();
  }

  @Override
  public Object cycListApiValue() {
    return args.cycListApiValue(true);
  }

  @Override
  public String cyclify() {
    return args.cyclify();
  }

  @Override
  public String cyclifyWithEscapeChars() {
    return args.cyclifyWithEscapeChars();
  }

  public Object getArg(final int argNum) {
    return args.get(argNum);
  }

  public CycFormula deepCopy() {
    final List<Object> newArgs = new ArrayList<Object>(args.size());
    for (final Object arg : args) {
      if (arg instanceof CycFormula) {
        newArgs.add(((CycFormula) arg).deepCopy());
      } else {
        newArgs.add(arg);
      }
    }
    return new CycFormula(newArgs);
  }

  /**
   *
   * @return The arguments of this formula (including the arg0, or operator) as a CycList.
   * Modifications to this list will be reflected back to the original CycFormula.
   */
  public CycList<Object> getArgs() {
    return args;
  }

  /**
   *
   * @return The arguments of this formula (including the arg0, or operator) as a CycList.
   * Modifications to this list will be reflected back to the original CycFormula.
   */
  public CycList<Object> toCycList() {
    return getArgs();
  }

  /**
   *
   * Set the arguments of this formula (including the arg0, or operator) to the
   * elements of the specified List.
   */
  public void setArgs(List<Object> newArgs) {
    args.clear();
    args.addAll(newArgs);
  }

  /**
   *
   * @return The arguments of this formula (including the arg0, or operator) as
   * an unmodifiable List.
   */
  public List<Object> getArgsUnmodifiable() {
    return Collections.unmodifiableList(args);
  }

  public int getArity() {
    return args.size() - 1;
  }

  /**
   *
   * @return The operator (arg0) of this formula.
   */
  public Object getOperator() {
    return args.get(0);
  }

  /**
   *
   * @return The arg0 (predicate or function) of this formula.
   */
  public Object getArg0() {
    return args.get(0);
  }

  /**
   *
   * @return The arg1 of this formula.
   */
  public Object getArg1() {
    return args.get(1);
  }

  /**
   *
   * @return The arg2 of this formula.
   */
  public Object getArg2() {
    return args.get(2);
  }

  /**
   *
   * @return The arg3 of this formula.
   */
  public Object getArg3() {
    return args.get(3);
  }

  @Override
  public List getReferencedConstants() {
    return args.getReferencedConstants();
  }

  public Collection<CycVariable> getReferencedVariables() {
    return treeGather(CycVariable.class);
  }

  public boolean contains(final Object obj) {
    return args.contains(obj);
  }

  /**
   * Does this formula contain term anywhere inside it?
   * @param term
   * @return
   */
  public boolean treeContains(Object term) {
    for (final TreeWalker tw = new TreeWalker(); tw.hasNext();) {
      if (tw.next().equals(term)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Gather all instances of cls anywhere inside this formula.
   * @param cls
   * @return
   */
  @SuppressWarnings("unchecked")
  public <T> Set<T> treeGather(Class<T> cls) {
    final Set<T> found = new HashSet<T>();
    for (final TreeWalker tw = new TreeWalker(); tw.hasNext();) {
      final Object arg = tw.next();
      if (cls.isInstance(arg)) {
        found.add((T) arg);
      }
    }
    return found;
  }

  /**
   * Returns a set of arg positions that describe all the locations where
   * the given term can be found in this formula.
   * @param term The term to search for
   * @return The set of all arg positions where term can be found
   */
  public Set<ArgPosition> getArgPositionsForTerm(final Object term) {
    final Set<ArgPosition> argPositions = new HashSet<ArgPosition>();
    for (final ArgPositionTrackingTreeWalker tw = new ArgPositionTrackingTreeWalker(); tw.hasNext();) {
      final Object arg = tw.next();
      if (term.equals(arg)) {
        argPositions.add(tw.getCurrentArgPosition());
      }
    }
    return argPositions;
  }

  /**
   * Returns an argument position that describes a location where
   * the given term can be found in this formula.
   * @param term The term to search for
   * @return Arg position where term can be found, null if it does not occur in this formula.
   */
  public ArgPosition getFirstArgPositionForTerm(final Object term) {
    for (final ArgPositionTrackingTreeWalker tw = new ArgPositionTrackingTreeWalker(); tw.hasNext();) {
      final Object arg = tw.next();
      if (term.equals(arg)) {
        return tw.getCurrentArgPosition();
      }
    }
    return null;
  }

  /**
   * Gather any constants anywhere inside this formula.
   * @return
   */
  @SuppressWarnings("unchecked")
  public Set<CycConstant> treeConstants() {
    final Set<CycConstant> constants = new HashSet<CycConstant>();
    constants.addAll(treeGather(CycConstant.class));
    return constants;
  }

  /** Non-destructively replace one set of terms with another.
   *
   * @param substitutions Map from terms to be replaced to their replacements
   * @return A new formula, with the specified substitutions performed.
   */
  public <K extends Object, V extends Object> CycFormula applySubstitutionsNonDestructive(Map<K, V> substitutions) {
    final CycFormula newFormula = deepCopy();
    newFormula.applySubstitutionsDestructive(substitutions);
    return newFormula;
  }

  /** Destructively replace one set of terms with another.
   *
   * @param substitutions Map from terms to be replaced to their replacements
   */
  public <K extends Object, V extends Object> void applySubstitutionsDestructive(Map<K, V> substitutions) {
    for (final Map.Entry<K, V> entry : substitutions.entrySet()) {
      for (final ArgPosition argPos : getArgPositionsForTerm(entry.getKey())) {
        setSpecifiedObject(argPos, entry.getValue());
      }
    }
  }

  /** Iterator over the non-formula args in this formula, descending into formula args. **/
  private class TreeWalker implements Iterator<Object> {

    private final Stack<Object> stack = new Stack<Object>();

    private TreeWalker() {
      initStack();
    }

    protected void initStack() {
      stack.push(CycFormula.this);
    }

    @Override
    public boolean hasNext() {
      return !stack.isEmpty();
    }

    @Override
    public Object next() {
      final Object item = popStack();
      if (item instanceof CycFormula) {
        for (final Object arg : ((CycFormula) item).args) {
          pushStack(arg);
        }
      }
      return item;
    }

    protected Object popStack() {
      return stack.pop();
    }

    protected Object pushStack(final Object arg) {
      return stack.push(arg);
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Not supported.");
    }
  }

  private class ArgPositionTrackingTreeWalker extends TreeWalker {

    private ArgPosition currentArgPosition = null; // ...for the last item served up.

    public ArgPosition getCurrentArgPosition() {
      return currentArgPosition;
    }

    @Override
    protected void initStack() {
      pushStack(new ObjectWithArgPosition(CycFormula.this, ArgPosition.TOP));
    }

    @Override
    protected ObjectWithArgPosition popStack() {
      return (ObjectWithArgPosition) super.popStack();
    }

    @Override
    protected Object pushStack(Object arg) {
      return super.pushStack((ObjectWithArgPosition) arg);
    }

    @Override
    public Object next() {
      final ObjectWithArgPosition itemPlus = popStack();
      final Object item = itemPlus.object;
      currentArgPosition = itemPlus.argPosition;
      if (item instanceof CycFormula) {
        final CycFormula formula = (CycFormula) item;
        for (int argNum = 0; argNum <= formula.getArity(); argNum++) {
          final Object arg = formula.getArg(argNum);
          final ArgPosition newArgPos = currentArgPosition.deepCopy();
          newArgPos.extend(argNum);
          pushStack(new ObjectWithArgPosition(arg, newArgPos));
        }
      }
      return item;
    }

    private class ObjectWithArgPosition {

      private final Object object;
      private final ArgPosition argPosition;

      private ObjectWithArgPosition(final Object object, final ArgPosition argPosition) {
        this.object = object;
        this.argPosition = argPosition;
      }
    }
  }

  public List<CycVariable> findFreeVariables() {
    return findFreeVariables(new ArrayList<CycVariable>(), new HashSet<CycVariable>(), false);
  }

  public List<CycVariable> findQueryableVariables() {
    return findFreeVariables(new ArrayList<CycVariable>(), new HashSet<CycVariable>(), true);
  }

  private List<CycVariable> findFreeVariables(List<CycVariable> freeVarsSoFar, Set<CycVariable> boundVars, boolean includeQueryable) {
    if (this instanceof CycFormulaSentence
            && (((CycFormulaSentence) this).isExistential() || ((CycFormulaSentence) this).isUniversal())) {
      final CycVariable boundVar = (CycVariable) getArg1();
      boundVars.add(boundVar);
      ((CycFormula) getArg2()).findFreeVariables(freeVarsSoFar, boundVars, includeQueryable);
      boundVars.remove(boundVar);
    } else if (includeQueryable
	    && this instanceof CycFormulaSentence
	    && CycFormulaSentence.IMPLIES.equals(getOperator())) {
      List<CycVariable> antecedentVars = 
	      ((CycFormula) getArg1()).findFreeVariables(new ArrayList<CycVariable>(), boundVars, includeQueryable);
      for (final CycVariable antecedentVar : antecedentVars) {
	boundVars.add(antecedentVar);
	freeVarsSoFar.remove(antecedentVar);
      }
      ((CycFormula) getArg2()).findFreeVariables(freeVarsSoFar, boundVars, includeQueryable);
      for (final CycVariable antecedentVar : antecedentVars) {
	boundVars.remove(antecedentVar);
      }
    } else {
      for (final Object arg : getArgsUnmodifiable()) {
        if (arg instanceof CycVariable) {
          final CycVariable var = (CycVariable) arg;
          if (!boundVars.contains(var) && !freeVarsSoFar.contains(var)) {
            freeVarsSoFar.add(var);
          }
        } else if (arg instanceof CycFormula) {
	  ((CycFormula) arg).findFreeVariables(freeVarsSoFar, boundVars, includeQueryable);
        }
      }
    }
    return freeVarsSoFar;
  }

  /**
   * Returns the object from the this sentence specified by the given arg position.
   *
   * @param argPosition the given arg position
   * @return the object from this sentence according to the
   * path specified by the given ArgPosition.
   */
  public Object getSpecifiedObject(final ArgPosition argPosition) {
    if (argPosition.depth() == 0) {
      return this;
    }
    Object answer = this;
    final List<Integer> tempPath = new ArrayList(argPosition.getPath());
    int index = 0;
    try {
      while (!tempPath.isEmpty()) {
        index = tempPath.get(0);
        if (answer instanceof CycNonAtomicTerm) {
          if (index == 0) {
            answer = ((CycNonAtomicTerm) answer).getFunctor();
          } else {
            answer = ((CycNonAtomicTerm) answer).getArgument(index);
          }
        } else if (answer instanceof CycFormula) {
          answer = ((CycFormula) answer).getArg(index);
        } else {
          answer = ((CycList) answer).get(index);
        }
        tempPath.remove(0);
      }
      return answer;
    } catch (Exception e) {
      throw new RuntimeException("Can\'t get object specified by \'" + argPosition + "\' in forumla: \'" + this + "\'.  answer: " + answer + " index: " + index + "\n" + StringUtils.getStringForException(e));
    }
  }

  /**
   * Destructively modify this formula, replacing the current value at argPosition with newArg.
   * @param argPosition
   * @param newArg
   */
  public void setSpecifiedObject(ArgPosition argPosition, Object newArg) {
    Object context = this;
    for (final Iterator<Integer> it = argPosition.getPath().iterator(); it.hasNext();) {
      final int index = it.next();
      if (!it.hasNext()) {
        setSpecifiedObject(context, index, newArg);
      } else if (context instanceof CycFormula) {
        context = ((CycFormula) context).getArg(index);
      } else if (context instanceof List) {
        context = ((List) context).get(index);
      } else {
        throw new RuntimeException("Can't find position " + argPosition + " in " + this);
      }
    }
  }

  private void setSpecifiedObject(Object context, int index, Object newArg) {
    if (context instanceof CycFormula) {
      ((CycFormula) context).args.set(index, newArg);
    } else if (context instanceof CycNart) {
      ((CycNart) context).setArgument(index, newArg);
    } else if (context instanceof List) {
      ((List) context).set(index, newArg);
    } else {
      throw new RuntimeException("Can't set " + index + " of " + context);
    }
  }

  @Override
  public String stringApiValue() {
    return args.stringApiValue();
  }

  public String toPrettyCyclifiedString(String indent) {
    return args.toPrettyCyclifiedString(indent);
  }

  public String toPrettyString(String indent) {
    return args.toPrettyString(indent);
  }

  public String toHTMLPrettyString(String indent) {
    return args.toHTMLPrettyString(indent);
  }

  @Override
  public void toXML(XMLWriter xmlWriter, int indent, boolean relative) throws IOException {
    args.toXML(xmlWriter, indent, relative);
  }

  @Override
  public int compareTo(Object o) {
    if (o instanceof CycFormula) {
      return args.compareTo(((CycFormula) o).args);
    } else {
      return 0;
    }
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final CycFormula other = (CycFormula) obj;
    if (this.args != other.args && (this.args == null || !this.args.equals(other.args))) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 67 * hash + (this.args != null ? this.args.hashCode() : 0);
    return hash;
  }

  public boolean equalsAtEL(final Object other) {
    return other instanceof CycFormula && args.equalsAtEL(((CycFormula) other).args);
  }
  //// Internal Representation
  protected final CycList<Object> args = new CycList<Object>();
}
