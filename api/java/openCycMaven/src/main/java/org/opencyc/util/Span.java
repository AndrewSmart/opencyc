/*
 * A representation of a start/end pair of ints.
 */
package org.opencyc.util;

/**
 *
 * @author baxter
 */
public class Span extends AbstractPair {

  public Span(Object component1, Object component2) {
    throw new IllegalArgumentException("Span components must be ints.");
  }

  public Span(int start, int end) {
    super(start, end);
  }

  public int getStart() {
    return (Integer) component1;
  }

  public int getEnd() {
    return (Integer) component2;
  }
}
