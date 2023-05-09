

/**
 * A factory for linearization objects.
 *
 * @see ExtendedTransformation#getFactory
 * @see UnscentedTransformation#getFactory
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $) 
 */
public interface LinearizationFactory {

  /**
   * Creates a new linearization.
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
  public Linearization linearize(NoisyVectorFunction f, Gaussian px); 
}
