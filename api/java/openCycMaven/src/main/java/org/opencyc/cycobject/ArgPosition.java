package org.opencyc.cycobject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author baxter
 */
public class ArgPosition extends DefaultCycObject {

  private final List<Integer> path = new ArrayList<Integer>();

  /**
   * Create a new arg position for the specified path.
   * @param path
   */
  public ArgPosition(final List<Integer> path) {
    this.path.addAll(path);
  }
  /** arg position for the top-level term/formula itself. */
  public static final ArgPosition TOP = new ArgPosition() {

    @Override
    public String toString() {
      return "TOP";
    }

    @Override
    public ArgPosition extend(ArgPosition otherArgPos) {
      throw new UnsupportedOperationException();
    }

    @Override
    public ArgPosition extend(Integer argnum) {
      throw new UnsupportedOperationException();
    }

    @Override
    public ArgPosition toParent() {
      throw new UnsupportedOperationException();
    }
  };

  /**
   * Create a new arg position for the specified argnums.
   * If argnums is empty, then returns an arg position for the top-level term/formula itself.
   * @param argNums
   */
  public ArgPosition(final Integer... argNums) {
    this(Arrays.asList(argNums));
  }

  /**
   * Get the list of argnums for this arg position, from top level to deepest level.
   * @return
   */
  public List<Integer> getPath() {
    return Collections.unmodifiableList(path);
  }

  /**
   * Destructively extend this arg position by another arg position.
   * @param otherArgPos
   * @return this ArgPosition.
   */
  public ArgPosition extend(ArgPosition otherArgPos) {
    path.addAll(otherArgPos.getPath());
    return this;
  }

  /**
   * Destructively extend this arg position by one argnum.
   * @param argnum
   * @return this ArgPosition.
   */
  public ArgPosition extend(Integer argnum) {
    path.add(argnum);
    return this;
  }

  public Integer first() {
    return getPath().get(0);
  }

  /**
   * Get the argnum of the designated argument in its immediate context.
   * @return
   */
  public Integer last() {
    final List<Integer> myPath = getPath();
    return myPath.get(myPath.size() - 1);
  }

  /**
   * Get the nesting depth of this arg position. Top-level argument positions have depth 1.
   * @return
   */
  public int depth() {
    return getPath().size();
  }

  @Override
  public String toString() {
    return path.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof ArgPosition) {
      return path.equals(((ArgPosition) obj).getPath());
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return path.hashCode();
  }

  /**
   * Destructively modify this ArgPosition to be its parent arg position.
   * @return this ArgPosition.
   */
  public ArgPosition toParent() {
    path.remove(path.size() - 1);
    return this;
  }

  ArgPosition deepCopy() {
    return new ArgPosition(path);
  }

  /**
   * Check if this arg position is for an ancestor of another arg position.
   * @param otherArgPosition
   * @return
   */
  public boolean isPrefixOf(ArgPosition otherArgPosition) {
    if (otherArgPosition == null || otherArgPosition.depth() < depth()) {
      return false;
    } else {
      return path.equals(otherArgPosition.getPath().subList(0, path.size()));
    }
  }

  @Override
  public String stringApiValue() {
    return new CycList(getPath()).stringApiValue();
  }

  /**
   * Does this arg position match candidate?
   * @param candidate
   * @param matchEmpty Should we match the null arg position?
   * @return
   */
  public boolean matchingArgPosition(ArgPosition candidate, boolean matchEmpty) {
    if (candidate == null) {
      return matchEmpty;
    } else {
      return isPrefixOf(candidate);
    }
  }

  public int compareTo(Object o) {
    if (o instanceof ArgPosition) {
      ArgPosition other = (ArgPosition) o;
      int i = 0;
      while (this.path.size() > i && other.path.size() > i) {
        final int result = this.path.get(i).compareTo(other.path.get(i));
        if (result != 0) {
          return result;
        }
        i++;
      }
      if (this.path.size() <= i) {
        return -1;
      } else if (other.path.size() <= i) {
        return 1;
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  }
}
