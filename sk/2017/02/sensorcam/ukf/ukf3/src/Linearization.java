
import java.util.*;
import Jama.*;

/**
 * A linear-Gaussian approximation to a nonlinear vector-valued
 * function of Gaussian-distributed inputs.
 *
 * <p>The inputs to a linearization are:
 *
 * <ul>
 *   <li>a vector function <b>y</b> = <i>f</i>(<b>x</b>, <b>v</b>)
 *       where <b>x</b> is an <i>n</i>-by-1 vector, <b>v</b> is an
 *       <i>m</i>-by-1 vector, and <b>y</b> is a <i>k</i>-by-1 
 *       vector; <b>x</b> may actually be the concatenation of 
 *       several vector variables; <b>v</b> is treated separately 
 *       because it is assumed to be independent of <b>x</b> and all 
 *       other random variables in the system (and thus no attempt is 
 *       made to estimate it from observations of <b>y</b>)</li>
 *   <li>a Gaussian distribution <i>p</i>(<b>x</b>)</li>
 *   <li>a Gaussian distribution <i>p</i>(<b>v</b>)</li>
 * </ul>
 *
 * The linearization is an affine-Gaussian approximation <i>g</i> to
 * <i>f</i> where
 *
 * <blockquote>
 * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
 * </blockquote>
 *
 * where <i>a</i> is a <i>k</i>-by-1 vector (the <i>constant
 * term</i>), <i>B</i> is a <i>k</i>-by-<i>n</i> matrix (the <i>linear
 * coefficient</i>) and <b>w</b> is an independent white noise vector
 * (of dimension <i>k</i>) with covariance <i>G</i>.  The outputs of
 * the linearization are the parameters of the approximation <i>g</i>:
 * <i>a</i>, <i>B</i>, and <i>G</i>.</p>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $) 
 */
public abstract class Linearization {

  /**
   * A random variable representing the output of the function.
   */
  protected Variable y;

  /**
   * The dimension of the output vector.
   */
  protected int k;
    
  /**
   * A list set containing only {@link #y} (for convenience).
   */
  protected ListSet ySet;

  /**
   * A list of random variables representing the inputs of the
   * function.  The actual input, <b>x</b>, is the vertical
   * concatenation of these column vectors in the order specified by
   * this list.
   */
  protected ListSet xSet;

  /**
   * The sum dimension of the input vector.
   */
  protected int n;

  /**
   * A list of random variables representing the noise inputs of the
   * function.  The actual input, <b>v</b>, is the vertical
   * concatenation of these column vectors in the order specified by
   * this list.
   */
  protected ListSet vSet;

  /**
   * The sum dimension of the noise vector.
   */
  protected int m;

  /**
   * A Gaussian distribution (in the moment parameterization) that
   * approximates <i>p</i>(<b>x</b>, <b>v</b>, <b>y</b>).
   */
  protected Gaussian q;

  /**
   * The constant term in the affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   */
  protected Matrix a;

  /**
   * The linear coefficient in the affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   */
  protected Matrix B;

  /**
   * The covariance of the white noise variable <b>w</b> in the
   * affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   */
  protected Matrix G;

  /**
   * Constructor.
   *
   * @param f  a vector function that takes a 
   *           <nobr>(<i>n</i> + <i>m</i>)</nobr>-by-1 vector as input 
   *           and returns a <i>k</i>-by-1 vector; the function's input
   *           is <b>x</b> stacked on top of <b>v</b>
   * @param px a Gaussian distribution (in the moment parameterization)
   *           over the <i>n</i>-by-1 vector <b>x</b> (which can be 
   *           partitioned into several sub-variables)
   * @throws IllegalArgumentException if the sum dimension of 
   *                                  <code>px</code> and
   *                                  <code>pv</code> does not match the
   *                                  input dimension of <code>f</code> 
   */
  public Linearization(NoisyVectorFunction f, Gaussian px) {
    n = px.getDimension();
    xSet = new ListSet(px.getVariables());
    Gaussian pv = f.getNoiseModel();
    m = (pv != null) ? pv.getDimension() : 0;
    vSet = (pv != null) ? new ListSet(pv.getVariables()) : new ListSet();
    k = f.getOutputDim();
    y = new Variable("output of " + f, k);
    ySet = new ListSet(y);
    if (n + m != f.getInputDim())
      throw new IllegalArgumentException("Gaussians' dimensions do not sum " +
					 "to that of the function's input");
  }

  /**
   * Returns a Gaussian approximation of the distribution over
   * the function's inputs and output.
   *
   * @return a Gaussian approximation of the distribution over
   * the function's inputs and output
   */
  public Gaussian getDistribution() {
    return new Gaussian(q);
  }

  /**
   * Returns the variable representing the output of the random
   * function.
   *
   * @param the variable representing the output of the random
   * function
   */
  public Variable getOutputVariable() {
    return y;
  }

  /**
   * Returns the constant term in the affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   *
   * @return the constant term <i>a</i>, a column vector with <i>k</i>
   *         elements 
   */
  public Matrix getConstantTerm() {
    return a;
  }
    
  /**
   * Returns the linear coefficient <i>B</i> in the affine-Gaussian
   * approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   *
   * @param v a subvariable of <b>x</b>
   * @return the linear coefficient <i>B</i>
   */
  public Matrix getCoefficient() {
    return B;
  }

  /**
   * Returns the submatrix of <i>B</i> in the affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   *
   * that corresponds to the subvariable <code>u</code> of <b>x</b>.
   *
   * @param u a subvariable of <b>x</b>
   * @return the subblock of <i>B</i> corresponding to <code>v</code>
   */
  public Matrix getCoefficient(Variable u) {
    return B.getMatrix(0, B.getRowDimension() - 1, 
		       q.getIndices(new ListSet(u)));
  }

  /**
   * Returns the covariance matrix of the noise term <b>w</b> in the
   * affine-Gaussian approximation
   *
   * <blockquote>
   * <i>g</i>(<b>x</b>) = <i>a</i> + <i>B</i> <b>x</b> + <b>w</b>
   * </blockquote>
   *
   * @return the <i>k</i>-by-<i>k</i> covariance matrix of <b>w</b>
   */
  public Matrix getNoiseCovariance() {
    return G;
  }
}
