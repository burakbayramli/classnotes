import Jama.*;

import java.util.*;

/**
 * <p>A Gaussian probability density over a set of vector-valued
 * variables.</p>
 *
 * <p>In the <i>moment parameterization</i>, the density is expressed as
 *
 * <blockquote>
 * <i>p</i>(<b>x</b>) = (1 / <i>Z</i>) exp {-0.5 (<b>x</b> -
 * <font face="Symbol">m</font>)<sup>T</sup>
 * <font face="Symbol">S</font><sup>-1</sup>(<b>x</b> -
 * <font face="Symbol">m</font>)}
 * </blockquote>
 *
 * where <i>Z</i> is a normalizing constant, and the vector <font
 * face="Symbol">m</font> (mu) and the (positive definite) matrix
 * <font face="Symbol">S</font> (sigma) are the parameters.  <font
 * face="Symbol">m</font> is the expected value (mean) of <b>x</b> and
 * <font face="Symbol">S</font> is its covariance matrix.</p>
 *
 * <p>In the <i>canonical parameterization</i>, the density is expressed as
 *
 * <blockquote>
 * <i>p</i>(<b>x</b>) = exp (<i>a</i> + 
 * <font face="Symbol">h</font><sup>T</sup><b>x</b> 
 * - (1/2) <b>x</b><sup>T</sup> <font face="Symbol">L</font> <b>x</b>)
 * </blockquote>
 *
 * where <i>a</i> is a normalizing constant, and the vector <font
 * face="Symbol">h</font> (eta) and the (positive definite) matrix
 * <font face="Symbol">L</font> (lambda) are the parameters.</p>
 *
 * The parameters of the moment and canonical representations are
 * related by
 *
 * <blockquote>
 * <nobr><font face="Symbol">S</font> = 
 * <font face="Symbol">L</font><sup>-1</sup></nobr>
 * <br>
 * <nobr><font face="Symbol">m</font> = 
 * <font face="Symbol">L</font><sup>-1</sup><font face="Symbol">h</font></nobr>
 * </blockquote>
 *
 * Because some operations are more efficient or more numerically
 * stable in one or the other parameterization, this class uses both
 * representations.  When an operation is unsupported by the current
 * parameterization, the user must first {@link
 * #reparameterize(boolean)} the density, or else an {@link
 * IllegalStateException} will be thrown.</p>
 *
 * <p>This class records counts of all floating point operations using
 * {@link Flops#count(long)} (except those used in the service of
 * debugging and avoiding numerical errors).</p>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.18 $ ($Date: 2003/02/07 20:06:25 $)
 */
public class Gaussian {

  /**
   * A map whose keys are the {@link Variable Variable}s bound by this
   * Gaussian and whose values are {@link Integer Integer}s giving
   * the starting index of the corresponding subvectors of {@link #vP}
   * and subblocks of {@link #mP}.
   */
  protected final HashMap varsToStarts;

  /**
   * An ordered set whose elements are the {@link Variable Variable}s
   * bound by this Gaussian.  The iteration order of the set is the
   * storage order of the parameters.  */
  protected ListSet vars;

  /**
   * An unmodifiable ordered set whose elements are the {@link
   * Variable Variable}s bound by this Gaussian.  The iteration order
   * of the set is the storage order of the parameters.  This is
   * merely a wrapper over {@link Gaussian#vars}.  */
  protected final Set variables;

  /**
   * A field indicating whether this Gaussian is currently
   * represented in the moment parameterization.
   */
  protected boolean isMoment;

  /**
   * The vector parameter; this is <font face="Symbol">m</font> if
   * {@link #isMoment} is <code>true</code> and <font
   * face="Symbol">h</font> otherwise.  The length of this vector is
   * greater than or equal to the sum of the dimensions of the
   * variables bound by this Gaussian.  
   */
  protected Matrix vP;

  /**
   * The positive definite matrix parameter; this is <font
   * face="Symbol">S</font> if {@link #isMoment} is <code>true</code>
   * and <font face="Symbol">L</font> otherwise.  This matrix is
   * always square, and the lengths of its sides are greater than or
   * equal to the sum of the dimensions of the variables bound by this
   * Gaussian.  
   */
  protected Matrix mP;

  /**
   * If <code>true</code>, then the capacity (i.e., the <i>actual</i>
   * dimension of {@link #vP} and {@link #mP}) of this Gaussian is
   * doubled when it must be increased to accomodate new variables;
   * otherwise it is increased only enough to admit the new variables.
   */
  protected boolean doubling;

  /**
   * Returns <code>true</code> if this Gaussian is represented using
   * the moment parameterization, i.e., using the mean vector <font
   * face="Symbol">m</font> and the (positive definite)
   * covariance matrix <font face="Symbol">S</font>.
   *
   * @return <code>true</code> if this Gaussian is represented using
   *         the moment parameterization
   */
  public boolean isMoment() {
    return isMoment;
  }

  /**
   * Returns <code>true</code> if this Gaussian is represented using
   * the canonical parameterization, i.e., using the vector <font
   * face="Symbol">h</font> and the (positive definite) matrix
   * <font face="Symbol">L</font>.
   *
   * @return <code>true</code> if this Gaussian is represented using
   *         the canonical parameterization
   */
  public boolean isCanonical() {
    return !isMoment;
  }

  /**
   * Reparameterizes this Gaussian; i.e., the parameterization will
   * be switched from the canonical to the moment parameterization, or
   * vice-versa.
   *
   * @param inPlace if <code>true</code>, then this Gaussian is 
   *                internally reparameterized; otherwise a fresh 
   *                Gaussian is constructed
   * @return the reparameterized Gaussian
   */
  public Gaussian reparameterize(boolean inPlace) {
    Gaussian q = null;
    if (inPlace) q = this;
    else q = new Gaussian(this);
    q.isMoment = !this.isMoment;
    int dim = getDimension();
    Matrix tmp = q.mP.getMatrix(0, dim - 1, 0, dim - 1).inverse();
    q.mP.setMatrix(0, dim - 1, 0, dim - 1, tmp);
    q.vP.setMatrix(0, dim - 1, 0, 0, 
		   tmp.times(q.vP.getMatrix(0, dim - 1, 0, 0)));
    // Count flops.

    return q;
  }

  /**
   * Controls the memory allocation behavior of this Gaussian.  If
   * <code>on == true</code>, then the capacity of the Gaussian
   * (i.e., the lengths of {@link #vP} and the sides of {@link #mP})
   * is doubled whenever adding a variable to the Gaussian (via
   * {@link #extend(java.util.Set)}) would cause the capacity to be
   * exceeded; otherwise, the capacity is increased only enough to
   * admit the new variable.  Doubling is off by default; Gaussians
   * that are frequently extended should turn it on.
   */
  public void setDoubling(boolean on) {
    this.doubling = on;
  }

  /**
   * Default constructor.
   * 
   * @param isMoment <code>true</code> if this Gaussian should be
   *                 represented in the moment parameterization
   */
  public Gaussian(boolean isMoment) {
    this(isMoment, 0);
  }

  /**
   * Default constructor.
   * 
   * @param isMoment <code>true</code> if this Gaussian should be
   *                 represented in the moment parameterization
   * @param capacity the initial capacity of the parameter matrices
   */
  public Gaussian(boolean isMoment, int capacity) {
    doubling = false;
    varsToStarts = new HashMap();
    vars = new ListSet();
    variables = Collections.unmodifiableSet(vars);
    vP = new Matrix(capacity, 1);
    mP = new Matrix(capacity, capacity);
    this.isMoment = isMoment;
  }

  /**
   * Copy constructor.  This object becomes a deep copy of the
   * supplied Gaussian density.
   */
  public Gaussian(Gaussian p) {
    varsToStarts = (HashMap)p.varsToStarts.clone();
    vars = new ListSet(p.vars);
    variables = Collections.unmodifiableSet(vars);
    vP = p.vP.copy();
    mP = p.mP.copy();
    isMoment = p.isMoment;
  }

  /**
   * Extends this Gaussian to include a new set of variables.  The
   * parameters are initialized to be uninformed over the new
   * variables.  The subvectors of {@link #vP} and blocks of {@link
   * #mP} are ordered consistently with the iteration order of
   * <code>vars</code>.
   *
   * @param vars a set of {@link Variable Variable} objects
   */
  public void extend(Set vars) {
    // Record the current dimension of the Gaussian
    int dim = getDimension(), oldDim = dim;
    // Add all variables in vars that are not currently in the Gaussian
    for (Iterator i = vars.iterator(); i.hasNext();) {
      Variable v = (Variable)i.next();
      if (!this.varsToStarts.containsKey(v)) {
	this.varsToStarts.put(v, new Integer(dim));
	this.vars.add(v);
	dim += v.dim;
      }
    }
    // If the new dimension exceeds the capacity, increase the capacity.
    if (dim > vP.getRowDimension()) {
      // Increase the capacity
      int capacity = (doubling && (2 * oldDim > dim)) ? 2 * oldDim : dim;
      // Extend the parameters
      Matrix newVP = new Matrix(capacity, 1);
      Matrix newMP = new Matrix(capacity, capacity);
      newVP.setMatrix(0, oldDim - 1, 0, 0, this.vP);
      newMP.setMatrix(0, oldDim - 1, 0, oldDim - 1, this.mP);
      this.vP = newVP;
      this.mP = newMP;
      if (isMoment)
	// Make the new diagonal covariance infinity.
	for (int i = oldDim; i < capacity; i++)
	  this.mP.set(i, i, Double.POSITIVE_INFINITY);
    }
  }

  /**
   * Creates an uninformative Gaussian density over the supplied set
   * of variables.  Note that this is an ill-conditioned density: in
   * the moment parameterization, this implies zero mean and infinite
   * spherical covariance; in the canonical parameterization, this
   * implies zero information vector and matrix.  {@link #vP} and
   * {@link #mP} are structured so that their blocks are ordered
   * consistently with the iteration order of <code>vars</code>.
   *
   * @param vars a set of {@link Variable Variable} objects
   * @param isMoment <code>true</code> if this Gaussian should be
   *                 represented in the moment parameterization
   */
  public Gaussian(Set vars, boolean isMoment) {
    this(isMoment);
    this.extend(vars);
  }

  /**
   * Separator Gaussian constructor.  This Gaussian's variables are
   * the intersection of those variables in the supplied Gaussians,
   * and its density is uninformative.  Note that this is an
   * ill-conditioned density: in the moment parameterization, this
   * implies zero mean and infinite spherical covariance; in the
   * canonical parameterization, this implies zero information vector
   * and matrix.
   *
   * @param p a Gaussian density
   * @param q a Gaussian density
   */
  public Gaussian(Gaussian p, Gaussian q) {
    this(false);
    // Compute the intersection of the two Gaussian's variables.
    HashSet s = new HashSet(p.getVariables());
    s.retainAll(q.getVariables());
    extend(s);
  }

  /**
   * Initializes this Gaussian using the supplied arguments.  The
   * parameters are not copied.
   *
   * @param vars     an ordered set of {@link Variable Variable} objects.
   * @param vP       a column vector whose length matches the sum dimensions 
   *                 of the variables and whose subvectors are ordered 
   *                 consistently with the order of <code>vars</code>; 
   * @param mP       a positive definite matrix whose lengths
   *                 match the sum dimensions of the variables and whose
   *                 blocks are ordered consistently with the order 
   * @throws IllegalArgumentException if <code>mP</code> is not square, or
   *                                  the dimensions of <code>vP</code> or 
   *                                  <code>mP</code> do not match the
   *                                  sum dimension of <code>vars</code>
   */
  protected void initialize(ListSet vars, 
			    Matrix vP, 
			    Matrix mP) {
    clear();
    extend(vars);
    // Check that vP and mP are properly sized
    int dim = Variable.dimension(vars);
    if (vP.getColumnDimension() != 1)
      throw new IllegalArgumentException("vP must be a column vector");
    if (vP.getRowDimension() != dim)
      throw new IllegalArgumentException("length of vP (" + 
					 vP.getRowDimension() + 
					 ") does not match variable " + 
					 "dimensions (" + dim + ")");
    if (mP.getRowDimension() != mP.getColumnDimension())
      throw new IllegalArgumentException("mP must be a square matrix");
    if (mP.getRowDimension() != dim)
      throw new IllegalArgumentException("size of mP (" + 
					 mP.getRowDimension() + 
					 ") does not match variable " +
					 "dimensions (" + dim + ")");
    // Install the parameters
    this.vP = vP;
    this.mP = mP;
  }

  /**
   * Constructor.  The parameters are not copied.
   *
   * @param vars     an ordered set of {@link Variable Variable} objects
   * @param vP       a column vector whose length matches the sum dimensions 
   *                 of the variables and whose subvectors are ordered 
   *                 consistently with the order of <code>vars</code>
   * @param mP       a positive definite matrix whose lengths
   *                 match the sum dimensions of the variables and whose
   *                 blocks are ordered consistently with the order of
   *                 <code>vars</code>
   * @param isMoment if <code>true</code>, then <code>vP</code> is
   *                 interpreted as <font face="Symbol">m</font> and
   *                 <code>mP</code> is interpreted as <font
   *                 face="Symbol">S</font>; otherwise, 
   *                 <code>vP</code> is interpreted as <font
   *                 face="Symbol">h</font> and <code>mP</code> is
   *                 interpreted as <font face="Symbol">L</font>
   * @throws IllegalArgumentException if <code>mP</code> is not square, or 
   *                                  the dimensions of <code>vP</code> or 
   *                                  <code>mP</code> do not match the
   *                                  sum dimension of <code>vars</code>
   */
  public Gaussian(ListSet vars, 
		  Matrix vP, 
		  Matrix mP, 
		  boolean isMoment) {
    this(isMoment);
    initialize(vars, vP, mP);
  }
  
  /**
   * Constructor.  The parameter objects are not copied.
   *
   * @param vars     an ordered set of {@link Variable Variable} objects
   * @param vP       a column vector whose length matches the sum dimensions 
   *                 of the variables and whose subvectors are ordered 
   *                 consistently with the order of <code>vars</code>
   * @param mP       a positive definite matrix whose lengths
   *                 match the sum dimensions of the variables and whose
   *                 blocks are ordered consistently with the order of 
   *                 <code>vars</code>
   * @param isMoment if <code>true</code>, then <code>vP</code> is
   *                 interpreted as <font face="Symbol">m</font> and
   *                 <code>mP</code> is interpreted as <font
   *                 face="Symbol">S</font>; otherwise, 
   *                 <code>vP</code> is interpreted as <font
   *                 face="Symbol">h</font> and <code>mP</code> is
   *                 interpreted as <font face="Symbol">L</font>
   * @throws IllegalArgumentException if lambda is not square, or the
   *                                  dimensions of <code>vP</code> or 
   *                                  <code>mP</code> do not match the
   *                                  sum dimension of <code>vars</code>
   */
  public Gaussian(ListSet vars, 
		  double[] vP, 
		  double[][] mP, 
		  boolean isMoment) {
    this(vars, new Matrix(vP, mP.length), new Matrix(mP), isMoment);
  }

  /**
   * Clears all variables from this Gaussian.
   */
  protected void clear() {
    varsToStarts.clear();
    vars.clear();
    vP = new Matrix(0, 0);
    mP = new Matrix(0, 0);
  }

  /**
   * Renames a variable in this Gaussian.
   *
   * @param var   the original variable
   * @param subst the variable substituted for the original variable
   * @throws IllegalArgumentException if <code>var</code> is not in this 
   *                                  Gaussian or <code>var</code> and 
   *                                  <code>subst</code> have differing
   *                                  dimension
   */
  public void rename(Variable var, Variable subst) {
    Integer starts = (Integer)varsToStarts.get(var);
    if (starts == null) 
      throw new IllegalArgumentException("" + var + " not in Gaussian");
    if (var.dim != subst.dim)
      throw new IllegalArgumentException("" + var + " and " + subst + 
					 " have different dimensions");
    varsToStarts.remove(var);
    varsToStarts.put(subst, starts);
    vars.set(vars.indexOf(var), subst);
  }

  /**
   * Returns an unmodifiable ordered set of the {@link Variable
   * Variable}s bound by this Gaussian.  The iteration order of this
   * set is the storage order of the parameters. */
  public Set getVariables() {
    return variables;
  }

  /**
   * Gets the sum of the dimensions of all variables bound by this
   * Gaussian.  
   *
   * @return the sum of the dimensions of all variables bound by this
   *         Gaussian
   */
  public int getDimension() {
    return Variable.dimension(variables);
  }

  /**
   * Returns the number of variables bound by this Gaussian.
   *
   * @return the number of variables bound by this Gaussian
   */
  public int getSize() {
    return variables.size();
  }

  /**
   * Gets an array of indices into the parameters that corresponds to
   * the supplied set of variables.  The order of the indices is
   * determined by the order of <code>vars</code>.
   *
   * @param vars An ordered set of {@link Variable Variable}s; the indices
   *             corresponding to these variables is returned.  If this
   *             Gaussian binds only one variable, then <code>null</code>
   *             is interpreted as the singleton containing the only 
   *             variable bound by this Gaussian.
   * @return an array of indices into the parameters that corresponds
   *         to <code>vars</code>
   */
  public int[] getIndices(ListSet vars) {
    if (vars == null) {
      if (variables.size() == 1) {
	int[] idx = new int[getDimension()];
	for (int i = 0; i < idx.length; i++)
	  idx[i] = i;
	return idx;
      } else
	throw new IllegalArgumentException("null disallowed when " + 
					   "Gaussian binds more " + 
					   "than one variable");
    }
    int[] idx = new int[Variable.dimension(vars)];
    int k = 0;
    for (Iterator i = vars.iterator(); i.hasNext();) {
      Variable v = ((Variable)i.next());
      if (!getVariables().contains(v)) {
	throw new IllegalArgumentException("Variable " + v + 
					   " not in " + this);
      }
      int start = ((Integer)varsToStarts.get(v)).intValue();
      for (int j = start; j < start + v.dim; j++) idx[k++] = j;
    } 
   return idx;
  }

  /**
   * Gets a subvector of <font face="Symbol">h</font>.  The subvectors
   * of <font face="Symbol">h</font> are ordered to be consistent with
   * the iteration order of <code>vars</code>.
   *
   * @param vars an ordered set of {@link Variable Variable}s; if this
   *             is <code>null</code>, then it is like supplying the 
   *             result of {@link Gaussian#getVariables()}, except the
   *             underlying parameter vector is returned, not copied
   * @return A subvector of <font face="Symbol">h</font>; this is a copy 
   *         unless <code>vars == null</code>
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the canonical parameterization
   */
  public Matrix getEta(ListSet vars) {
    if (!isCanonical())
      throw new IllegalStateException("Not in canonical parameterization; " +
				      "must first reparameterize.");
    if (vars == null)
      return vP; 
    else
      return vP.getMatrix(getIndices(vars), 0, 0);
  }

  /**
   * Sets a subvector of <font face="Symbol">h</font>.  The subvectors
   * of <code>eta</code> are assumed to be ordered consistently with
   * the order of <code>vars</code>.
   *
   * @param vars an ordered set of {@link Variable Variable}s; this can
   *             be <code>null</code> if this Gaussian binds only one 
   *             variable
   * @param eta  the new subvector value
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the canonical parameterization
   */
  public void setEta(ListSet vars, Matrix eta) {
    if (!isCanonical())
      throw new IllegalStateException("Not in canonical parameterization; " +
				      "must first reparameterize.");
    this.vP.setMatrix(getIndices(vars), 0, 0, eta);
  }

  /**
   * Gets a submatrix of <font face="Symbol">L</font>.  The blocks of
   * <font face="Symbol">L</font> are ordered consistently with the
   * orders of the index sets.
   *
   * @param rowVars an ordered set of {@link Variable Variable}s; if this
   *                is <code>null</code>, then it is like supplying the 
   *                result of {@link Gaussian#getVariables()}, except the
   *                underlying parameter matrix is returned, not copied
   * @param colVars an ordered set of {@link Variable Variable}s; use
   *                <code>null</code> to indicate that this is the same as
   *                <code>rowVars</code>
   * @return A matrix consisting of blocks of <font face="Symbol">L</font>, 
   *         as a {@link Jama.Matrix Matrix} object; this is a copy 
   *         unless <code>vars == null</code>
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the canonical parameterization
   */
  public Matrix getLambda(ListSet rowVars, ListSet colVars) {
    if (!isCanonical())
      throw new IllegalStateException("Not in canonical parameterization; " +
				      "must first reparameterize.");
    if (rowVars == null)
      return mP;
    int[] rowIndices = getIndices(rowVars);
    int[] colIndices = null;
    if (colVars == null)
      colIndices = rowIndices;
    else
      colIndices = getIndices(colVars);
    return mP.getMatrix(rowIndices, colIndices);
  }

  /**
   * Sets a submatrix of <font face="Symbol">L</font>.  The blocks of
   * <code>lambda</code> must be ordered consistently with the orders
   * of the index sets.  This method permits the setting of diagonal
   * blocks or off diagonal blocks, but not both simultaneously; if
   * off diagonal blocks are specified, then their transpose blocks
   * are also set to maintain the symmetry of 
   * <font face="Symbol">L</font>.
   *
   * @param rowVars an ordered set of {@link Variable Variable}s; this 
   *                can be <code>null</code> if this Gaussian binds 
   *                only one variable
   * @param colVars an ordered set of {@link Variable Variable}s; use
   *                <code>null</code> to indicate that this is the same as
   *                <code>rowVars</code>
   * @param lambda  the new submatrix value; this must be symmetric 
   *                if <code>colVars</code> is <code>null</code>
   * @return A matrix consisting of blocks of <font face="Symbol">L</font>, 
   *         as a two-dimensional <code>double</code> array
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the canonical parameterization
   * @throws IllegalArgumentException if <code>colVars</code> is 
   *         <code>null</code> and <code>lambda</code> is not symmetric
   */
  public void setLambda(ListSet rowVars, ListSet colVars, Matrix lambda) {
    if (!isCanonical())
      throw new IllegalStateException("Not in canonical parameterization; " +
				      "must first reparameterize.");
    int[] rowIndices = getIndices(rowVars);
    int[] colIndices = null;
    if (colVars == null) {
      this.mP.setMatrix(rowIndices, rowIndices, lambda);
    } else {
      for (Iterator i = colVars.iterator(); i.hasNext();)
	if (rowVars.contains(i.next()))
	  throw new IllegalArgumentException("cannot set both diagonal " + 
					     "and off-diagonal blocks");
      colIndices = getIndices(colVars);
      this.mP.setMatrix(rowIndices, colIndices, lambda);
      this.mP.setMatrix(colIndices, rowIndices, lambda.transpose());
    }
  }

  /**
   * Gets a subvector of <font face="Symbol">m</font>.  The subvectors
   * of <font face="Symbol">m</font> are ordered to be consistent with
   * the iteration order of <code>vars</code>.
   *
   * @param vars an ordered set of {@link Variable Variable}s; if this
   *             is <code>null</code>, then it is like supplying the 
   *             result of {@link Gaussian#getVariables()}, except the
   *             underlying parameter vector is returned, not copied
   * @return A subvector of <font face="Symbol">m</font>; this is a copy 
   *         unless <code>vars == null</code>
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the moment parameterization
   */
  public Matrix getMu(ListSet vars) {
    if (!isMoment())
      throw new IllegalStateException("Not in moment parameterization; " +
				      "must first reparameterize.");
    if (vars == null)
      return vP;
    else
      return vP.getMatrix(getIndices(vars), 0, 0);
  }

  /**
   * Sets a subvector of <font face="Symbol">m</font>.  The subvectors
   * of <code>mu</code> are assumed to be ordered consistently with
   * the order of <code>vars</code>.
   *
   * @param vars an ordered set of {@link Variable Variable}s; this can
   *             be <code>null</code> if this Gaussian binds only one 
   *             variable
   * @param mu  the new subvector value
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the moment parameterization
   */
  public void setMu(ListSet vars, Matrix mu) {
    if (!isMoment())
      throw new IllegalStateException("Not in moment parameterization; " +
				      "must first reparameterize.");
    this.vP.setMatrix(getIndices(vars), 0, 0, mu);
  }

  /**
   * Gets a submatrix of <font face="Symbol">S</font>.  The blocks of
   * <font face="Symbol">S</font> are ordered consistently with the
   * orders of the index sets.
   *
   * @param rowVars an ordered set of {@link Variable Variable}s; if this
   *                is <code>null</code>, then it is like supplying the 
   *                result of {@link Gaussian#getVariables()}, except the
   *                underlying parameter matrix is returned, not copied
   * @param colVars an ordered set of {@link Variable Variable}s; use
   *                <code>null</code> to indicate that this is the same as
   *                <code>rowVars</code>
   * @return A matrix consisting of blocks of <font face="Symbol">S</font>, 
   *         as a {@link Jama.Matrix Matrix} object; this is a copy 
   *         unless <code>vars == null</code>
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the moment parameterization
   */
  public Matrix getSigma(ListSet rowVars, ListSet colVars) {
    if (!isMoment())
      throw new IllegalStateException("Not in moment parameterization; " +
				      "must first reparameterize.");
    if (rowVars == null)
      return mP;
    int[] rowIndices = getIndices(rowVars);
    int[] colIndices = null;
    if (colVars == null)
      colIndices = rowIndices;
    else
      colIndices = getIndices(colVars);
    return mP.getMatrix(rowIndices, colIndices);
  }

  /**
   * Sets a submatrix of <font face="Symbol">S</font>.  The blocks of
   * <code>sigma</code> must be ordered consistently with the orders
   * of the index sets.  This method permits the setting of diagonal
   * blocks or off diagonal blocks, but not both simultaneously; if
   * off diagonal blocks are specified, then their transpose blocks
   * are also set to maintain the symmetry of 
   * <font face="Symbol">S</font>.
   *
   * @param rowVars an ordered set of {@link Variable Variable}s; this 
   *                can be <code>null</code> if this Gaussian binds 
   *                only one variable
   * @param colVars an ordered set of {@link Variable Variable}s; use
   *                <code>null</code> to indicate that this is the same as
   *                <code>rowVars</code>
   * @param sigma  the new submatrix value; this must be symmetric 
   *                if <code>colVars</code> is <code>null</code>
   * @return A matrix consisting of blocks of <font face="Symbol">S</font>, 
   *         as a two-dimensional <code>double</code> array
   * @throws IllegalStateException if this Gaussian is not currently in 
   *                               the moment parameterization
   * @throws IllegalArgumentException if <code>colVars</code> is 
   *         <code>null</code> and <code>sigma</code> is not symmetric
   */
  public void setSigma(ListSet rowVars, ListSet colVars, Matrix sigma) {
    if (!isMoment())
      throw new IllegalStateException("Not in moment parameterization; " +
				      "must first reparameterize.");
    int[] rowIndices = getIndices(rowVars);
    int[] colIndices = null;
    if (colVars == null) {
      this.mP.setMatrix(rowIndices, rowIndices, sigma);
    } else {
      for (Iterator i = colVars.iterator(); i.hasNext();)
	if (rowVars.contains(i.next()))
	  throw new IllegalArgumentException("cannot set both diagonal " + 
					     "and off-diagonal blocks");
      colIndices = getIndices(colVars);
      this.mP.setMatrix(rowIndices, colIndices, sigma);
      this.mP.setMatrix(colIndices, rowIndices, sigma.transpose());
    }
  }

  /**
   * Marginalizes a set of variables out of this Gaussian (in place).
   *
   * @param mvars a set of {@link Variable Variable}s to marginalize out;
   *              any of these that are not bound by the Gaussian are ignored
   */
  public void marginalizeOut(Set mvars) {
    HashSet rvars = new HashSet(getVariables());
    rvars.removeAll(mvars);
    this.marginalize(rvars, true);
  }

  /**
   * Marginalizes out a subset of the variables in this Gaussian.
   * The result uses the same parameterization as this Gaussian.
   *
   * @param rvars   a set of {@link Variable Variable}s for
   *                the marginal; the complement of this collection is 
   *                marginalized out of this Gaussian
   * @param inPlace if <code>true</code>, then the Gaussian is
   *                marginalized in place.
   * @return this Gaussian with the variables in <code>mvars</code>
   *         marginalized out
   * @throws IllegalArgumentException if this Gaussian does not contain
   *                                  the variables in <code>rvars</code>
   */
  public Gaussian marginalize(Set vars, boolean inPlace) {
    if (!getVariables().containsAll(vars))
      throw new IllegalArgumentException("" + this + 
					 " does not contain variables" + 
					 vars);
    // Special-case speedups.
    if (vars.isEmpty()) {
      if (inPlace) {
	this.clear();
	return this;
      } else return new Gaussian(isMoment());
    } else if (vars.containsAll(getVariables())) {
      if (inPlace) return this;
      else return new Gaussian(this);
    }
    ListSet rvars = new ListSet(vars);
    if (isMoment()) {
      // MOMENT PARAMETERIZATION
      Matrix m_mu = getMu(rvars);
      Matrix m_sigma = getSigma(rvars, null);
      if (inPlace) {
        this.initialize(rvars, m_mu, m_sigma);
	return this;
      } else 
	return new Gaussian(rvars, m_mu, m_sigma, true);
    } else {
      // CANONICAL PARAMETERIZATION
      ListSet mvars = rvars.complement(variables);
      // Compute the indices of the marginalized (m) and remaining
      // (r) variables.
      int[] midx = getIndices(mvars);
      int[] ridx = getIndices(rvars);
      // Extract the blocks from eta and lambda
      Matrix eta_m = vP.getMatrix(midx, 0, 0);
      Matrix eta_r = vP.getMatrix(ridx, 0, 0);
      Matrix lambda_mm = mP.getMatrix(midx, midx);
      /*
	for (int i = 0; i < lambda_mm.getRowDimension(); i++)
	  for (int j = i + 1; j < lambda_mm.getColumnDimension(); j++)
	    if (lambda_mm.get(i, j) != lambda_mm.get(j, i))
	      throw new Error("Asymmetric precision matrix!");
      */
      Matrix lambda_mr = mP.getMatrix(midx, ridx);
      Matrix lambda_rr = mP.getMatrix(ridx, ridx);
      // lambda_div = (lambda_rm / lambda_mm)
      Matrix lambda_div = null;
      // m_eta = eta_r - (lambda_rm / lambda_mm) * eta_m;
      Matrix m_eta = eta_r;
      // m_lambda = lambda_rr - (lambda_rm / lambda_mm) * lambda_rm';
      Matrix m_lambda = lambda_rr;
      // When the matrix is not well conditioned (which is frequent
      // in SLAM), the Cholesky solver introduces a huge amount of
      // error.  Thus, we use the standard solver.
      if (false) {
	CholeskyDecomposition cd = lambda_mm.chol();
	if (!cd.isSPD())
	  throw new Error("Conditional precision is not p.s.d.");
	lambda_div = cd.solve(lambda_mr).transpose();
      } else  {
	lambda_div = lambda_mm.solve(lambda_mr).transpose();
      }
      m_eta.minusEquals(lambda_div.times(eta_m));
      m_lambda.minusEquals(lambda_div.times(lambda_mr));
      // Count the flops.
      if (inPlace) {
        this.initialize(rvars, m_eta, m_lambda);
	return this;
      } else 
	return new Gaussian(rvars, m_eta, m_lambda, false);
    }
  }

  /**
   * Conditions on a subset of the variables in this Gaussian.
   *
   * @param cvars   an ordered set of the {@link Variable Variable}s
   *                of this Gaussian being conditioned on
   * @param obs     a vector (whose length and order is consistent with 
   *                <code>cvars</code>) representing the observation
   * @param inPlace if <code>true</code>, the conditioning is done in-place.
   * @return this Gaussian with the variables in <code>cvars</code>
   *         conditioned on
   * @throws IllegalArgumentException if this Gaussian does not contain
   *                                  the variables in <code>rvars</code>
   */
  public Gaussian condition(ListSet cvars, Matrix obs, boolean inPlace) {
    // TODO: This can be written more generally so that the Gaussian
    // need not cover the conditioning variables.
    if (!getVariables().containsAll(cvars))
      throw new IllegalArgumentException("" + this + 
					 " does not contain variables" + 
					 cvars);
    // Special-case speedups.
    if (cvars.isEmpty()) {
      if (inPlace) return this;
      else return new Gaussian(this);
    } else if (cvars.containsAll(getVariables())) {
      if (inPlace) {
	this.clear();
	return this;
      } else return new Gaussian(isMoment());
    }
    // Compute the indices of the conditioned (c) and remaining
    // (r) variables.
    ListSet rvars = cvars.complement(variables);
    int[] cidx = getIndices(cvars);
    int[] ridx = getIndices(rvars);
    if (isCanonical()) {
      // Extract the blocks from eta and lambda
      Matrix eta_r = vP.getMatrix(ridx, 0, 0);
      Matrix lambda_rr = mP.getMatrix(ridx, ridx);
      Matrix lambda_rc = mP.getMatrix(ridx, cidx);
      // c_eta = eta_r - lambda_rc * obs
      Matrix c_eta = eta_r.minus(lambda_rc.times(obs));
      // c_lambda = lambda_rr
      Matrix c_lambda = lambda_rr;
      // Return the conditional Gaussian.
      if (inPlace) {
	this.initialize(cvars, c_eta, c_lambda);
	return this;
      } else 
        return new Gaussian(rvars, c_eta, c_lambda, false);
    } else {
      // MOMENT PARAMETERIZATION
      Matrix mu_r = vP.getMatrix(ridx, 0, 0);
      Matrix mu_c = vP.getMatrix(cidx, 0, 0);
      Matrix sigma_rr = mP.getMatrix(ridx, ridx);
      Matrix sigma_cc = mP.getMatrix(cidx, cidx);
      Matrix sigma_cr = mP.getMatrix(cidx, ridx);
      // sigma_div = (sigma_rc / sigma_cc)
      Matrix sigma_div = null;
      // c_mu = mu_r + (sigma_rc / sigma_cc) * (obs - mu_c);
      Matrix c_mu = mu_r;
      // m_lambda = sigma_rr - (sigma_rc / sigma_cc) * sigma_rc';
      Matrix c_sigma = sigma_rr;
      // When the matrix is not well conditioned (which is frequent
      // in SLAM), the Cholesky solver introduces a huge amount of
      // error.  Thus, we use the standard solver.
      if (false) {
	CholeskyDecomposition cd = sigma_cc.chol();
	if (!cd.isSPD())
	  throw new Error("Marginal covariance is not p.s.d.");
	sigma_div = cd.solve(sigma_cr).transpose();
      } else  {
	sigma_div = sigma_cc.solve(sigma_cr).transpose();
      }
      c_mu.minusEquals(sigma_div.times(obs.minus(mu_c)));
      c_sigma.minusEquals(sigma_div.times(sigma_cr));
      // Count the flops.
      // Return the conditional Gaussian.
      if (inPlace) {
        this.initialize(rvars, c_mu, c_sigma);
	return this;
      } else 
	return new Gaussian(rvars, c_mu, c_sigma, true);
    }
  }

  /**
   * Sets this Gaussian equal to the supplied Gaussian.
   */
  public void set(Gaussian p) {
    int dim = getDimension();
    if (dim == p.getDimension()) {
      vP.setMatrix(0, dim - 1, 0, 0, p.vP);
      mP.setMatrix(0, dim - 1, 0, dim - 1, p.mP);
    } else {
      vP = p.vP.copy();
      mP = p.mP.copy();
    }
    isMoment = p.isMoment;
    varsToStarts.clear();
    varsToStarts.putAll(p.varsToStarts);
    vars.clear();
    vars.addAll(p.vars);
  }

  /**
   * Multiplies two Gaussians.  This method cannot be applied to
   * Gaussians that are in different parameterizations, or to a pair
   * of moment Gaussians that share variables.
   *
   * @param inPlace if <code>true</code>, then <code>p</code> is
   *                multiplied into <code>this</code> and <code>this</code>
   *                is returned.
   * @return the product of <code>this</code> and <code>p</code>
   * @throws IllegalStateException if this Gaussian or the supplied 
   *                               Gaussian are in the moment 
   *                               parameterization
   */
  public Gaussian times(Gaussian p, boolean inPlace) {
    Gaussian q;
    if (inPlace)
      q = this;
    else
      q = new Gaussian(this);
    ListSet pvars = new ListSet(p.getVariables());
    if (isMoment()) {
      if (pvars.containsAny(q.variables))
        throw new IllegalStateException("Multiplication not supported for " +
					"overlapping moment Gaussians.");
      // First, extend the Gaussian to contain all variables in p
      q.extend(pvars);
      q.setMu(pvars, p.getMu(pvars));
      q.setSigma(pvars, null, p.getSigma(pvars, null));
    } else {
      // First, extend the Gaussian to contain all variables in p
      q.extend(pvars);
      // Now, add the parameters of p into those of q.
      q.setEta(pvars, 
	       q.getEta(pvars).plusEquals(p.getEta(pvars)));
      q.setLambda(pvars, null,
		  q.getLambda(pvars, null).
		  plusEquals(p.getLambda(pvars, null)));
      // Count the flops
      int k = Variable.dimension(pvars);
    }
    return q;
  }

  /**
   * Divides two Gaussians (in the canonical parameterization).
   *
   * @param inPlace if <code>true</code>, then <code>p</code> is
   *                divided into <code>this</code> and <code>this</code>
   *                is returned.
   * @return <code>this</code> divided by <code>p</code>
   * @throws IllegalArgumentException if <code>p</code> binds variables
   *                                  not bound by this Gaussian
   * @throws IllegalStateException if this Gaussian or the supplied 
   *                               Gaussian are in the moment 
   *                               parameterization
   */
  public Gaussian div(Gaussian p, boolean inPlace) {
    Gaussian q;
    if (inPlace)
      q = this;
    else
      q = new Gaussian(this);
    ListSet pvars = new ListSet(p.getVariables());
    if (!getVariables().containsAll(pvars))
      throw new IllegalArgumentException("Gaussian does not " + 
					 "contain divisor variables");
    // Subtract the parameters of p from those of q.
    q.setEta(pvars, 
	     q.getEta(pvars).minusEquals(p.getEta(pvars)));
    q.setLambda(pvars, null,
		q.getLambda(pvars, null).
		minusEquals(p.getLambda(pvars, null)));
    // Count the flops
    int k = Variable.dimension(pvars);
    return q;
  }

  /**
   * Computes the differential entropy <i>H</i> of this Gaussian.
   *
   * @return the differential entropy of this Gaussian (in nats,
   *         i.e., natural logarithmic units).
   */
  public double entropy() {
    // Count the flops
    if (isMoment())
      return 0.5 * (((double)getDimension() * Math.log(2.0 * Math.PI * Math.E))
		    + Math.log(mP.det()));
    else
      return 0.5 * (((double)getDimension() * Math.log(2.0 * Math.PI * Math.E))
		    - Math.log(mP.det()));
  }

  /**
   * Computes the relative entropy (Kullback-Liebler divergence) from
   * this Gaussian to the supplied Gaussian.
   *
   * This method is most efficient when this Gaussian is represented
   * in moment form.
   *
   * @throws IllegalArgumentException if this and the supplied
   *         Gaussian do not cover the same set of variables
   * @return D(<code>this</code> || <code>q</code>) (in nats, i.e.,
   *         natural logarithmic units).  
   */
  public double kl(Gaussian q) {
    if (!q.getVariables().equals(vars))
      throw new IllegalArgumentException("Both distributions must " + 
					 "cover the same variables ");
    int n = getDimension();
    Gaussian p = isMoment() ? this : reparameterize(false);
    Matrix muP = p.getMu(null);
    Matrix sigmaP = p.getSigma(null, null);
    Matrix muQ = null;
    Matrix sigmaQ = null;
    Matrix lambdaQ = null;
    if (q.isMoment()) {
      muQ = q.getMu(vars);
      sigmaQ = q.getSigma(vars, null);
      lambdaQ = sigmaQ.inverse();
    } else {
      lambdaQ = q.getLambda(vars, null);
      Gaussian qm = q.reparameterize(false);
      muQ = qm.getMu(vars);
      sigmaQ = qm.getSigma(vars, null);
    }
    // Compute the KL incrementally.
    Matrix d = muP.minus(muQ);
    double kl = lambdaQ.times(sigmaP.plus(d.times(d.transpose()))).trace();
    kl = kl - (double)n;
    kl = kl + (isMoment() ? -Math.log(mP.det()) : Math.log(mP.det()));
    kl = kl + (q.isMoment() ? Math.log(q.mP.det()) : -Math.log(q.mP.det()));
    kl = kl / 2.0d;
    // TODO: This overcounts; we can compute at least one of the
    // determinants using the Cholesky decompositions used to compute
    // the inverses above.

    return kl;
  }

  /**
   * Computes the (conditional) mutual information
   * <nobr>I(<code>x</code>;<code>y</code> -
   * <code>x</code>|<code>z</code>)</nobr> in nats (natural
   * logarithmic units).
   *
   * @param x a set of {@link Variable Variable}s in this Gaussian
   * @param y a set of {@link Variable Variable}s in this Gaussian
   * @param z a set of {@link Variable Variable}s in this Gaussian
   * @return <nobr>I(<code>x</code>;<code>y</code> - 
   *         <code>x</code>|<code>z</code>)</nobr> in nats
   * @throws IllegalArgumentException if <code>x</code>, <code>y</code>, 
   *                                  or <code>z</code> contain variables 
   *                                  not bound by this Gaussian
   */
  public double mutualInformation(Set x, Set y, Set z) {
    // Recompute x, y, and z so that they are disjoint.
    x = new HashSet(x);
    x.removeAll(z);
    y = new HashSet(y);
    y.removeAll(z);
    y.removeAll(x);
    /* Condition on (an arbitrary value of) z.  The value doesn't
     * matter because the entropy (and thus the mutual information) of
     * a gaussian variable depends only upon its covariance, which is
     * independent of the conditioning value. */
    int dim = Variable.dimension(z);
    Gaussian p = condition(new ListSet(z), new Matrix(dim, 1), false);
    // Compute the entropy of x and y given z.
    double entropyOfXYgivenZ = p.entropy();
    // Marginalize out x and y to get the other entropies.
    double entropyOfXgivenZ = p.marginalize(x, false).entropy();
    double entropyOfYgivenZ = p.marginalize(y, false).entropy();
    // Compute the mutual information as a difference of entropies.
    double mutualInformation = 
      entropyOfXgivenZ + entropyOfYgivenZ - entropyOfXYgivenZ;
    return mutualInformation;
  }

  /**
   * Samples from this Gaussian distribution.  The distribution must
   * be in the moment parameterization.
   *
   * @param n the number of samples to draw
   * @param r the source of random bits
   * @return a matrix with <code>n</code> columns, each of which 
   *         is a sample from this distribution
   */
  public Matrix sample(int n, Random r) {
    Matrix s = new Matrix(getDimension(), n);
    double[][] elts = s.getArray();
    for (int i = 0; i < getDimension(); i++)
      for (int j = 0; j < n; j++)
	elts[i][j] = r.nextGaussian();
    Matrix mu = getMu(vars);
    Matrix sigma = getSigma(vars, null);
    Matrix chol = sigma.chol().getL();
    Matrix samples = chol.times(s);
    elts = samples.getArray();
    for (int i = 0; i < getDimension(); i++)
      for (int j = 0; j < n; j++)
	elts[i][j] += mu.get(i, 0);
    return samples;
  }

  /**
   * Computes the likelihood of a particular value under this
   * distribution.
   *
   * @param val the value
   * @return the likelihood of <code>val</code>
   * @throws IllegalArgumentException if the length of <code>val</code> 
   *                                  does not equal the dimension of 
   *                                  this distribution
   */
  public double likelihood(double[] val) {
    Matrix x = new Matrix(val, val.length);
    if (isMoment()) {
      Matrix mu = getMu(vars);
      Matrix sigma = getSigma(vars, null);
      Matrix diff = x.minus(mu);
      int dim = val.length;
      Matrix mahal = diff.transpose().times(sigma.chol().solve(diff));

      return Math.exp(-0.5d * mahal.get(0, 0)) / 
	Math.sqrt(Math.pow(Math.sqrt(2.0 * Math.PI), getDimension()) 
		  * sigma.det());
    } else {
      throw new InternalError("not yet implemented");
    }
  }

  /**
   * Creates a simple <code>String</code> representation of this
   * Gaussian that prints out its parameters using Matlab notation.
   */
  public String toString() {
    if (varsToStarts.size() == 0)
      return "empty Gaussian";
    StringBuffer b = 
	new StringBuffer((isMoment() ? "Moment" : "Canonical") + 
			 " Gaussian over: ");
    for (Iterator i = varsToStarts.entrySet().iterator(); i.hasNext();) {
      Map.Entry e = (Map.Entry)i.next();
      Variable v = (Variable)e.getKey();
      int start = ((Integer)e.getValue()).intValue();
      b.append("" + v + "(" + start + ":" + (start + v.dim - 1) + ") ");
    }
    b.append("\nvector parameter:\n");
    return b.toString();
  }
  
  /**
   * Paints a 2D confidence ellipse for the first two dimensions of
   * this Gaussian distribution.  
   */
  public void paint(java.awt.Graphics2D g, double conf) {
    if (!isMoment())
      throw new UnsupportedOperationException();
    double mx = vP.get(0, 0);
    double my = vP.get(1, 0);
    double vx = mP.get(0, 0);
    double vy = mP.get(1, 1);
    double vxy = mP.get(0, 1);
    double theta = 0.5d * Math.atan((2.0d * vxy) / (vx - vy));
    // Apply a transform to center and axis-align the ellipse.
    g.translate(mx, my);
    g.rotate(theta);
    // Paint the axis-aligned ellipse.
    java.awt.geom.Ellipse2D ellipse = 
      new java.awt.geom.Ellipse2D.Double(-vx, -vy, vx, vy);
    g.draw(ellipse);
    // Undo the transform.
    g.rotate(-theta);
    g.translate(-mx, -my);
  }
}
