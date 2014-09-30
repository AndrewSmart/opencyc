/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.util.query;

//// External Imports
import java.util.List;

//// Internal Imports
import org.opencyc.cycobject.CycVariable;

/**
 *
 * @author baxter
 */
public interface QueryResult extends Comparable<QueryResult> {

  List<Binding> getBindings();

  Integer getAnswerID();

  Object getBindingForVar(CycVariable var);

  public static interface Binding {

    String getVariableName();

    CycVariable getVariable();

    void setVariable(CycVariable variable);

    Object getValue();
  }
}
