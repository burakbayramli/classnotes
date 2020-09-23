
import java.util.*;
import Jama.*;

/**
 * A linear-Gaussian filter.
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.13 $ ($Date: 2002/12/28 22:02:57 $)
 */
public interface Filter {

  /**
   * Marginalizes a set of variables out of the belief state.
   *
   * @param mvars a set of {@link Variable Variable}s to marginalize out
   */
  public void marginalizeOut(Set mvars);

  /**
   * Performs a linear-Gaussian measurement update.  The parameters
   * <code>vars</code>, <code>C</code> and <code>R</code> define the
   * measurement equation as follows:
   *
   * <blockquote>
   * <nobr><b>y</b>=<code>y0</code> + 
   * <code>C</code><b>x</b>(<code>vars</code>)+<b>w</b></nobr>
   * </blockquote>
   *
   * where <b>w</b> is a white-noise variable with covariance
   * <code>R</code>.  Given the actual measurement <code>y</code>,
   * this method updates the belief state.
   *
   * @param vars an ordered set of the variables with sum dimension 
   *             <i>n</i> in the belief state that causally influenced
   *             this measurement; any variables in this list that are
   *             not currently in the belief state are added with
   *             uninformative priors.
   * @param y0   a <i>k</i>-vector giving the constant term
   * @param C    a <i>k</i> by <i>n</i> observation matrix that defines the
   *             linear measurement model (and whose columns are ordered 
   *             consistently with the order of <code>vars</code>)
   * @param R    a <i>k</i> by <i>k</i> symmetric positive definite matrix
   *             giving the covariance of the measurement white noise
   * @param y    the measurement <i>k</i>-vector
   * @throws IllegalArgumentException if there are any dimension mismatches
   */
  public abstract void measurement(ListSet vars, double[] y0, double[][] C, 
				   double[][] R, double[] y);

  /**
   * Performs a linear-Gaussian time update.  The parameters
   * <code>vars</code>, <code>A</code> and <code>Q</code> define the
   * state evolution equation as follows:
   *
   * <blockquote>
   * <nobr><b>x</b><sub>t + 1</sub>(<code>vars</code>)=<code>x0</code>
   * <code>A</code><b>x</b><sub>t</sub>(<code>vars</code>)+<b>v</b></nobr>
   * </blockquote>
   *
   * where <b>v</b> is a white-noise variable with covariance
   * <code>Q</code>.  All variables not in <code>vars</code> are
   * assumed stationary.
   *
   * @param vars an ordered set of the variables with sum dimension <i>n</i>
   *             in the belief state that evolve over time
   * @param x0   an <i>n</i>-vector giving the constant term
   * @param A    an <i>n</i> by <i>n</i> evolution matrix that defines the
   *             linear evolution model (and whose blocks are ordered 
   *             consistently with the order of <code>vars</code>)
   * @param Q    an <i>n</i> by <i>n</i> symmetric positive definite matrix
   *             giving the covariance of the evolution white noise (and 
   *             whose blocks are ordered consistently with the order
   *             of <code>vars</code>)
   * @throws IllegalArgumentException if there are any dimension mismatches 
   *                                  or <code>vars</code> contains variables
   *                                  that are not in the current belief state
   */
  public abstract void time(ListSet vars, double[] x0, 
			    double[][] A, double[][] Q);

  /**
   * Extracts the filtered marginal distribution.
   *
   * @param vars  the set of {@link Variable Variable}s whose filtered
   *              marginal is to be computed
   * @return a filtered marginal potential over <code>vars</code>
   */
  public abstract Gaussian getMarginal(Set vars);

  /**
   * Extracts a set of unary marginals.
   *
   * @param  vars a collection of {@link Variable Variable}s, or 
   *         <code>null</code> to indicate all variables in the belief state
   * @return a map whose keys are the (distinct) elements of
   *         <code>vars</code> and whose values are the corresponding 
   *         marginals (in the moment parameterization)
   */
  public Map getMarginals(Collection vars);

  /**
   * Gets an unmodifiable set of the {@link Variable Variable}s in the
   * filtered belief state.
   */
  public Set getVariables();
}
