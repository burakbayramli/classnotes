
import java.util.*;
import Jama.*;

/**
 * The Unscented Transformation for linearizing functions with
 * Gaussian-distributed inputs.  This technique selects a set of
 * <i>sigma points</i> that characterize the Gaussian distribution of
 * the input vector, passes these points through the nonlinear
 * function, and then computes the best Gaussian approximation of
 * their images.
 *
 * <p>This class records counts of all floating point operations using
 * {@link Flops#count(long)}.</p>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $) 
 * @see <a href="http://cslu.cse.ogi.edu/nsel/ukf/">http://cslu.cse.ogi.edu/nsel/ukf/</a> 
 * @see "S. J. Julier and J. K. Uhlmann, &quot;A new extension of the
 * kalman filter to nonlinear systems,&quot; <i>The Proceedings of
 * AeroSense: The 11th International Symposium on Aerospace/Defense
 * Sensing, Simulation and Controls</i>, Orlando, Florida, 1997."
 */
public class UnscentedTransformation extends Linearization {

  /**
   * A factory for creating linearizations using the unscented
   * transformation.
   */
  protected static LinearizationFactory factory = 
    new LinearizationFactory() {
      public Linearization linearize(NoisyVectorFunction f, Gaussian px) {
	return new UnscentedTransformation(f, px);
      }
    };

  /**
   * A scaling parameter that determines the spread of the sigma
   * points about the mean of <b>x</b>.
   */
  private double alpha;

  /**
   * A parameter that is used to incorporate prior knowledge of the
   * distribution <i>p</i>(<b>x</b>).
   */
  private double beta;

  /**
   * A secondary scaling parameter.
   */
  private double kappa;

  /**
   * A scaling parameter; <font face="symbol">l</font> = <font
   * face="symbol">a</font><sup>2</sup> (<i>n</i> + <font
   * face="symbol">k</font>) - <i>n</i>.
   */
  private double lambda;

  /**
   * Constructor.  The parameters used to choose sigma points are set
   * at default values.
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
   *                                  input dimension of <code>f</code> */
  public UnscentedTransformation(NoisyVectorFunction f, Gaussian px) {
    this(f, px, 1e-3, 2.0d, 0.0d);
  }

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
   * @param alpha a scaling parameter that determines the spread of the
   *              sigma points about the mean 
   * @param beta  a parameter that is used to incorporate prior knowledge 
   *              of the input distribution
   * @param kappa a secondary scaling parameter
   *
   * @throws IllegalArgumentException if the sum dimension of 
   *                                  <code>px</code> and
   *                                  <code>pv</code> does not match the
   *                                  input dimension of <code>f</code>
   */
  public UnscentedTransformation(NoisyVectorFunction f, 
				 Gaussian px, 
				 double alpha,
				 double beta,
				 double kappa) {
    super(f, px);
    this.alpha = alpha;
    this.beta = beta;
    this.kappa = kappa;
    lambda = alpha * alpha * (n + kappa) - n;
    // Compute the sigma points; first form the product distribution.
    Gaussian pv = f.getNoiseModel();
    q = new Gaussian(px);
    if (pv != null) q.times(pv, true);
    ListSet iSet = new ListSet(q.getVariables());
    double[][] sp = getSigmaPoints(q);
    int nsp = sp.length;
    // Compute the images of the sigma points.
    double[][] spi = new double[nsp][];
    for (int i = 0; i < nsp; i++)
      spi[i] = f.evaluate(sp[i]);
    // Wrap the sigma points and their images by Matrix objects
    Matrix spm = new Matrix(sp).transpose();
    Matrix spim = new Matrix(spi).transpose();
    // Create a distribution that extends p to include the function's output
    q.extend(ySet);
    // Compute the mean and covariance of the sigma point images.
    Matrix muY = center(spim);
    Matrix sigmaYY = cov(spim, spim);
    q.setMu(ySet, muY);
    q.setSigma(ySet, null, sigmaYY);
    // Compute the (weighted) cross-covariance of the sigma points and
    // their images.
    center(spm);
    Matrix crossCov = cov(spm, spim);
    q.setSigma(iSet, ySet, crossCov);
    Matrix sigmaXY = q.getSigma(xSet, ySet);
    // Compute the linear coefficient B.  I would prefer to use the
    // Cholesky solver, but accumulated error can result in sigmaXX
    // having small negative eigenvalues.
    Matrix muX = q.getMu(xSet);
    Matrix sigmaXX = q.getSigma(xSet, null);
    B = sigmaXX.solve(sigmaXY).transpose();
    // Compute the constant term a
    a = muY.minus(B.times(muX));
    // Compute the covariance of the noise term w
    G = sigmaYY.minus(B.times(sigmaXY));

  }

  /**
   * Computes the weight of the <code>i</code><sup>th</sup> sigma
   * point to be used when computing means.
   *
   * @param i the id of the sigma point (between 0 and 2 <code>n</code>)
   * @return the weight of the <code>i</code><sup>th</sup> sigma point
   */
  private double getMeanWeight(int i) {
    if (i == 0) return lambda / ((double)(n + m) + lambda);
    else return 1.0d / (2.0d * ((double)(n + m) + lambda));
  }

  /**
   * Computes the weight of the <code>i</code><sup>th</sup> sigma
   * point to be used when computing covariances.
   *
   * @param i the id of the sigma point (between 0 and 2 <code>n</code>)
   * @return the weight of the <code>i</code><sup>th</sup> sigma point
   */
  private double getCovWeight(int i) {
    if (i == 0) 
      return lambda / ((double)(n + m) + lambda) + 
	1.0d - alpha * alpha + beta;
    else return 1.0d / (2.0d * ((double)(n + m) + lambda));
  }

  /**
   * Computes a weighted mean of the columns of a matrix and then
   * subtracts this weighted mean from each column.  The weighted mean
   * is returned.
   * 
   * @param A an <i>m</i>-by-<i>n</i> matrix
   * @return an <i>m</i>-by-<i>1</i> vector giving the weighed mean of
   *         the columns of <code>A</code>
   */
  private Matrix center(Matrix sp) {
    int d = sp.getRowDimension();
    int nsp = sp.getColumnDimension();
    double[][] elts = sp.getArray();
    // Compute the mean.
    Matrix mean = new Matrix(d, 1);
    double[][] meanElts = mean.getArray();
    for (int i = 0; i < d; i++) {
      for (int j = 0; j < nsp; j++)
	meanElts[i][0] += getMeanWeight(j) * elts[i][j];
    }
    // Center the matrix.
    for (int i = 0; i < d; i++) 
      for (int j = 0; j < nsp; j++)
	elts[i][j] -= meanElts[i][0];
    return mean;
  }

  /**
   * Computes the weighted covariance between two (centered) matrices
   * of centered column vectors.
   * 
   */
  private Matrix cov(Matrix A, Matrix B) {
    Matrix C = A.copy();
    double[][] Celts = C.getArray();
    for (int i = 0; i < C.getRowDimension(); i++)
      for (int j = 0; j < C.getColumnDimension(); j++)
	Celts[i][j] *= getCovWeight(j);

    return C.times(B.transpose());
  }

  /**
   * Returns a matrix whose columns are the sigma points of the
   * supplied distribution.
   *
   * @param p a Gaussian distribution (in the moment parameterization)
   * @return a two-dimensional array <code>sp</code> of
   *         <code>double</code> values; <code>sp[i]</code> is the 
   *         <i>i</i><sup>th</sup> sigma point and <code>sp[i][j]</code> 
   *         is its <i>j</i><sup>th</sup> coordinate.
   */
  private double[][] getSigmaPoints(Gaussian p) {
    int d = p.getDimension();
    int nsp = 2 * d + 1;
    ListSet vars = new ListSet(p.getVariables());
    Matrix mean = p.getMu(vars);
    Matrix sigma = p.getSigma(vars, null);
    CholeskyDecomposition cd = sigma.timesEquals(d + lambda).chol();
    Matrix L = cd.getL();
    Matrix sp = new Matrix(d, nsp);
    sp.setMatrix(0, d - 1, 0, 0, mean);
    for (int i = 1; i <= d; i++) {
      sp.setMatrix(0, d - 1, i, i, 
		   mean.plus(L.getMatrix(0, d - 1, i - 1, i - 1)));
      sp.setMatrix(0, d - 1, d + i, d + i, 
		   mean.minus(L.getMatrix(0, d - 1, i - 1, i - 1)));
    }
    return sp.transpose().getArray();
  }

  /**
   * Returns a handle on a factory for creating linearizations using
   * the unscented transformation.
   *
   * @return a factory for creating linearizations
   */
  public static LinearizationFactory getFactory() {
    return factory;
  }
}
