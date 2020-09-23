
import java.util.*;
import Jama.*;

/**
 * A filter for nonlinear systems that uses local linearizations of
 * the dynamics and measurement models so that a linear-Gaussian
 * filter can be used to maintain an approximate Gaussian belief
 * state.  This class permits an arbitrary combination of
 * linearization techniques and filter techniques; for example: 
 * <ul>
 *   <li>by combining the {@link UnscentedTransformation} with the 
 *       {@link KalmanFilter} we get the Unscented Kalman filter;</li>
 *   <li>by combining the {@link ExtendedTransformation} with the 
 *       {@link InformationFilter} we get the Extended Information
 *       filter;</li>
 * </ul>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $) */
public class LinearizedFilter implements NonlinearFilter {

  /**
   * The underlying linear-Gaussian filter.
   */
  protected Filter filter;

  /**
   * The factory for creating linearizations.
   */
  protected LinearizationFactory linearization;

  /**
   * Constructor.
   *
   * @param filter the underlying linear-Gaussian filter used for
   *               filtering
   * @param linearization a linearization factory used to 
   *                      create linearizations of nonlinear models
   */
  public LinearizedFilter(Filter filter,
			  LinearizationFactory linearization) {
    this.filter = filter;
    this.linearization = linearization;
  }

  /**
   * Performs an approximate nonlinear measurement update.  The
   * parameters <code>vars</code>, <code>g</code>, and
   * <code>q</code> define the measurement equation as follows:
   *
   * <blockquote>
   * <nobr><b>y</b>=<code>g</code>(<b>x</b>[<code>vars</code>], <b>v</b>)
   * </blockquote>
   *
   * where <b>v</b> is an independent white noise vector.  Given the
   * actual measurement <code>y</code>, this method updates the belief
   * state.
   *
   * @param vars an ordered set of the variables with sum dimension 
   *             <i>n</i> in the belief state that causally influenced
   *             this measurement.
   * @param g    the noisy measurement function with input dimension 
   *             <i>n</i> + <i>m</i> and output dimension <i>k</i>; 
   *             the input is formed from <b>x</b>[<code>vars</code>]
   *             stacked on top of the noise vector <b>v</b>
   * @param y    the measurement <i>k</i>-vector
   * @throws IllegalArgumentException if there are any dimension mismatches */
  public void measurement(ListSet vars, 
			  NoisyVectorFunction g, 
			  double[] y) {
    Gaussian p = filter.getMarginal(vars);
    Linearization l = linearization.linearize(g, p);
    filter.measurement(vars,
		       l.getConstantTerm().getColumnPackedCopy(),
		       l.getCoefficient().getArray(),
		       l.getNoiseCovariance().getArray(),
		       y);
  }

  /**
   * Performs an approximate nonlinear time update.  The parameters
   * <code>vars</code>, <code>f</code>, and <code>q</code> define the
   * state evolution equation as follows:
   *
   * <blockquote> <nobr><b>x</b><sub>t + 1</sub>[<code>vars</code>]=
   * <code>f</code>(<b>x</b><sub>t</sub>[<code>vars</code>], <b>w</b>)</nobr>
   * </blockquote>
   *
   * where <b>w</b> is an independent white noise vector.  All
   * variables not in <code>vars</code> are assumed stationary.
   *
   * @param vars an ordered set of the variables with sum dimension 
   *             <i>n</i> in the belief state that evolve.
   * @param f    the state evolution function with input dimension 
   *             <i>n</i> + <i>m</i> and output dimension <i>n</i>
   * @throws IllegalArgumentException if there are any dimension mismatches */
  public void time(ListSet vars, 
		   NoisyVectorFunction f) {
    Gaussian p = filter.getMarginal(vars);
    Linearization l = linearization.linearize(f, p);
    filter.time(vars,
		l.getConstantTerm().getColumnPackedCopy(),
		l.getCoefficient().getArray(),
		l.getNoiseCovariance().getArray());
  }
}
