/*
 * A default implementation of QueryResult
 */
package org.opencyc.util.query;

//// External Imports
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

//// Internal Imports
import org.opencyc.cycobject.CycVariable;
import org.opencyc.inference.InferenceAnswerIdentifier;

/**
 *
 * @author baxter
 */
public class DefaultQueryResult implements QueryResult {

  public DefaultQueryResult(final List<? extends Binding> bindings, final InferenceAnswerIdentifier inferenceAnswerIdentifier) {
    this.answerID = (inferenceAnswerIdentifier == null) ? null : inferenceAnswerIdentifier.getAnswerID();
    if (bindings != null) {
      this.bindings.addAll(bindings);
    }
  }

  protected DefaultQueryResult() {
    this(null, null);
  }

  public List<Binding> getBindings() {
    return Collections.unmodifiableList(bindings);
  }

  public Integer getAnswerID() {
    return answerID;
  }

  public Object getBindingForVar(final CycVariable var) {
    for (final Binding binding : getBindings()) {
      if (binding.getVariable().equals(var)) {
        return binding.getValue();
      }
    }
    return null;
  }

  public int compareTo(final QueryResult o) {
    if (o == null) {
      return -1;
    } else {
      return answerID.compareTo(o.getAnswerID());
    }
  }

  public static class DefaultBinding implements Binding {

    private final Object term;
    private CycVariable variable;

    public DefaultBinding(final CycVariable variable, final Object term) {
      this.variable = variable;
      this.term = term;
    }

    @Override
    public String toString() {
      return variable + " -> " + term;
    }

    @Override
    public String getVariableName() {
      return variable.name;
    }

    @Override
    public CycVariable getVariable() {
      return variable;
    }

    @Override
    public Object getValue() {
      return term;
    }

    @Override
    public void setVariable(CycVariable variable) {
      this.variable = variable;
    }
  }
  private final List<Binding> bindings = new ArrayList<Binding>();
  private final Integer answerID;
}
