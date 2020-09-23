
import java.util.*;

/**
 * A Gaussian vector variable.
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.7 $ ($Date: 2002/12/28 22:02:10 $)
 */
public class Variable {

  /**
   * The label for this variable.
   */
  public final Object label;

  /**
   * The dimension of this variable.
   */
  public final int dim;

  /**
   * Default constructor.
   *
   * @param label an object that labels this variable.  Labels are not
   *              used to determine the identity of variables; see {@link
   *              Variable#equals(Object) equals}.
   * @param dim   the dimension of this vector variable
   */
  public Variable(Object label, int dim) {
    this.label = label;
    this.dim = dim;
  }

  /**
   * Constructor.  The variable has no label.
   *
   * @param dim   the dimension of this vector variable
   */
  public Variable(int dim) {
    this.label = null;
    this.dim = dim;
  }

  /**
   * Finalizes the definition of {@link Object#equals(Object) equals}
   * so that two variable references are equal iff they refer to the
   * same object.  This notion of equality is consistent with two
   * variables having identical labels, but being distinct.  This
   * notion of equality prevents two variable objects from referring
   * to the same variable but having differing dimensions.
   */
  public final boolean equals(Object o) {
    return super.equals(o);
  }

  /**
   * Finalizes the definition of {@link Object#hashCode() hashCode}.
   */
  public final int hashCode() {
    return super.hashCode();
  }

  /**
   * Returns the string representation of this variable's label.
   */
  public String toString() {
    return (label != null) ? label.toString() : super.toString();
  }

  /**
   * Computes the sum dimension of a set of {@link Variable Variable}s.
   */
  public static int dimension(Set vars) {
    int dim = 0;
    for (Iterator i = vars.iterator(); i.hasNext();)
      dim += ((Variable)i.next()).dim;
    return dim;
  }
}
