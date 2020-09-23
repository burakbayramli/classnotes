

/**
 * A vector-valued function that takes a vector input, the bottom part
 * of which is a white noise vector.  In particular, if
 * <code>dim</code> is the result of calling {@link
 * Gaussian#getDimension()} on the Gaussian {@link #getNoiseModel()},
 * then elements <code>{@link VectorFunction#getInputDim()} -
 * dim</code> through <code>{@link VectorFunction#getInputDim()} -
 * 1</code> of the input vector are the noise variables.
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $) */
public interface NoisyVectorFunction extends VectorFunction {

  /**
   * Returns the Gaussian distribution over the noise input.
   *
   * @return the Gaussian distribution over the noise input
   */
  public Gaussian getNoiseModel();
}
