
import Jama.*;
import java.util.*;

/**
 * A Kalman filter.
 *
 * <p>This class records counts of all floating point operations using
 * {@link Flops#count(long)} (except those used in the service of
 * debugging and avoiding numerical errors).</p>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.8 $ ($Date: 2003/02/07 20:06:18 $)
 */
public class KalmanFilter implements Filter {

  /**
   * The moment potential representing the belief state.
   */
  protected Gaussian p;

  /**
   * Constructor.
   *
   * @param p the initial belief state
   */
  public KalmanFilter(Gaussian p) {
    this.p = p;
  }

  /**
   * Returns the current filtered belief state.
   *
   * @return the current filtered belief state
   */
  public Gaussian getDistribution() {
    return p;
  }

  /**
   * Marginalizes a set of variables out of the belief state.
   *
   * @param mvars a set of {@link Variable Variable}s to marginalize out
   */
  public void marginalizeOut(Set mvars) {
    p.marginalizeOut(mvars);
  }

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
   * @throws IllegalArgumentException if there are any dimension mismatches */
  public void measurement(ListSet vars, double[] y0, double[][] C, 
			  double[][] R, double[] y) {
    Matrix y0M = new Matrix(y0, y0.length);
    Matrix CM = new Matrix(C);
    Matrix RM = new Matrix(R);
    Matrix yM = new Matrix(y, y.length);
    /* Let V = vars be the set of parents, U be the set of all
     * variables, W = U - V so that U = [V; W], and Y be the observed
     * variable. */
    ListSet all = new ListSet(p.getVariables());   // U
    ListSet comp = vars.complement(all);           // W
    // Compute the marginal distribution over the parent variables V
    Matrix muV = p.getMu(vars);
    Matrix sigmaVV = p.getSigma(vars, null);
    // Compute the marginal distribution over the observed variables Y
    Matrix muY = y0M.plus(CM.times(muV));
    Matrix sigmaVY = sigmaVV.times(CM.transpose());
    Matrix sigmaYY = CM.times(sigmaVY).plus(RM);

    // Compute Cov(U, Y) = SigmaUV * C'
    Matrix sigmaUV = p.getSigma(all, vars);
    Matrix sigmaUY = sigmaUV.times(CM.transpose());

    // Compute the Kalman gain matrix K = sigmaUY / sigmaYY
    Matrix K = sigmaYY.solve(sigmaUY.transpose()).transpose();

    // Get the current distribution over U (not copy).
    Matrix muU = p.getMu(null);
    Matrix sigmaUU = p.getSigma(null, null);
    /* Compute the updated distribution over vars:
     *   mu    <-- mu + K (y - muY)
     *   sigma <-- (I - K C) sigma = sigma - K sigmaUY'
     * (Note the order of the matrix multiplications above really
     * matters: ((KC) sigma) requires O(N^3) time, but (K(C sigma))
     * requires O(N^2) time.)  In order to avoid constructing large 
     * temporary matrices, the update to Sigma is done manually.
     */
    int n = p.getDimension();
    int m = y0.length;
    double[][] emuU = muU.getArray();
    Matrix innov = K.times(yM.minus(muY));

    double[][] eInnov = innov.getArray();
    for (int i = 0; i < n; i++)
      emuU[i][0] += eInnov[i][0];
    /* Perform the covariance update using 
     *
     *    sigmaUU = sigmaUU - ((K * sigmaYU) + (K * sigmaYU)') / 2
     *
     * which is equivalent to 
     *
     *    sigmaUU = sigmaUU - K * sigmaYU
     *
     * (the standard Kalman covariance update) because K * sigmaUY is
     * positive semidefinite.  This variation has the benefit of
     * maintaining the symmetry of sigmaUU.  We first compute the
     * upper-triangular elements of sigmaUU and then copy them down to
     * the lower-triangle.
     */
    double[][] esUU = sigmaUU.getArray();
    double[][] esUY = sigmaUY.getArray();
    double[][] eK = K.getArray();
    for (int i = 0; i < n; i++)
      for (int j = i; j < n; j++) {
	double d = 0.0d;
	for (int k = 0; k < m; k++)
	  d += (eK[i][k] * esUY[j][k]) + (eK[j][k] * esUY[i][k]);
	esUU[i][j] -= d / 2.0d;
      }
    // Copy the result down to the lower-triangular elements.
    for (int i = 0; i < n; i++)
      for (int j = i + 1; j < n; j++)
	esUU[j][i] = esUU[i][j];

  }

  /**
   * Performs a linear-Gaussian measurement update using the <i>Joseph
   * form</i> of the covariance update, which is numerically more
   * stable.  The parameters <code>vars</code>, <code>C</code> and
   * <code>R</code> define the measurement equation as follows:
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
   * @throws IllegalArgumentException if there are any dimension mismatches */
  public void joseph(ListSet vars, double[] y0, double[][] C, 
		     double[][] R, double[] y) {
    Matrix y0M = new Matrix(y0, y0.length);
    Matrix CM = new Matrix(C);
    Matrix RM = new Matrix(R);
    Matrix yM = new Matrix(y, y.length);
    /* Let V = vars be the set of parents, U be the set of all
     * variables, W = U - V so that U = [V; W], and Y be the observed
     * variable. */
    ListSet all = new ListSet(p.getVariables());   // U
    ListSet comp = vars.complement(all);           // W
    // Compute the marginal distribution over the parent variables V
    Matrix muV = p.getMu(vars);
    Matrix sigmaVV = p.getSigma(vars, null);
    // Compute the marginal distribution over the observed variables Y
    Matrix muY = y0M.plus(CM.times(muV));
    Matrix sigmaVY = sigmaVV.times(CM.transpose());
    Matrix sigmaYY = CM.times(sigmaVY).plus(RM);

    // Compute Cov(U, Y) = SigmaUV * C'
    Matrix sigmaUV = p.getSigma(all, vars);
    Matrix sigmaUY = sigmaUV.times(CM.transpose());

    // Compute the Kalman gain matrix K = sigmaUY / sigmaYY
    Matrix K = sigmaYY.solve(sigmaUY.transpose()).transpose();

    // Get the current distribution over U (not copy).
    Matrix muU = p.getMu(null);
    Matrix sigmaUU = p.getSigma(null, null);
    /* Compute the updated distribution over vars:
     *   mu    <-- mu + K (y - muY)
     *   sigma <-- (I - K C) sigma = sigma - K sigmaUY'
     * (Note the order of the matrix multiplications above really
     * matters: ((KC) sigma) requires O(N^3) time, but (K(C sigma))
     * requires O(N^2) time.)  In order to avoid constructing large 
     * temporary matrices, the update to Sigma is done manually.
     */
    int n = p.getDimension();
    int m = y0.length;
    double[][] emuU = muU.getArray();
    Matrix innov = K.times(yM.minus(muY));

    double[][] eInnov = innov.getArray();
    for (int i = 0; i < n; i++)
      emuU[i][0] += eInnov[i][0];
    /* Perform the covariance update using the Joseph update:
     *
     */
    Matrix fullC = new Matrix(m, n);
    fullC.setMatrix(0, m - 1, p.getIndices(vars), CM);
    Matrix J = Matrix.identity(n, n).minus(K.times(fullC));
    Matrix H = 
      J.times(sigmaUU.getMatrix(0, n - 1, 0, n - 1).times(J.transpose()));
    sigmaUU.setMatrix(0, n - 1, 0, n - 1,
		      H.plus(K.times(RM.times(K.transpose()))));

  }

  /**
   * Performs a linear-Gaussian time update.  The parameters
   * <code>vars</code>, <code>A</code> and <code>Q</code> define the
   * state evolution equation as follows:
   *
   * <blockquote>
   * <nobr><b>x</b><sub>t + 1</sub>(<code>vars</code>)=<code>x0</code> +
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
  public void time(ListSet vars, double[] x0, 
		   double[][] A, double[][] Q) {
    Matrix x0M = new Matrix(x0, x0.length);
    Matrix AM = new Matrix(A);
    Matrix QM = new Matrix(Q);
    Matrix mu = p.getMu(vars);
    p.setMu(vars, x0M.plus(AM.times(mu)));
    Matrix sigma = p.getSigma(vars, null);
    Matrix tmp = sigma.times(AM.transpose());
    tmp = AM.times(tmp).plus(QM);
    p.setSigma(vars, null, tmp);
    ListSet all = new ListSet(p.getVariables());
    ListSet comp = vars.complement(all);
    Matrix CC = p.getSigma(vars, comp);
    p.setSigma(vars, comp, AM.times(CC));

  }

  /**
   * Extracts the filtered marginal distribution.
   *
   * @param vars  the set of {@link Variable Variable}s whose filtered
   *              marginal is to be computed
   * @return a filtered marginal potential over <code>vars</code>
   */
  public Gaussian getMarginal(Set vars) {
    return p.marginalize(vars, false);   
  }

  /**
   * Extracts a set of unary marginals.
   *
   * @param  vars a collection of {@link Variable Variable}s, or 
   *         <code>null</code> to indicate all variables in the belief state
   * @return a map whose keys are the (distinct) elements of
   *         <code>vars</code> and whose values are the corresponding 
   *         marginals (in the moment parameterization)
   */
  public Map getMarginals(Collection vars) {
    if (vars == null) vars = getVariables();
    HashMap marginals = new HashMap(vars.size());
    for (Iterator i = vars.iterator(); i.hasNext();) {
      Variable v = (Variable)i.next();
      marginals.put(v, getMarginal(Collections.singleton(v)));
    }
    return marginals;
  }

  /**
   * Gets an unmodifiable set of the {@link Variable Variable}s in the
   * filtered belief state.
   */
  public Set getVariables() {
    return p.getVariables();
  }
}
