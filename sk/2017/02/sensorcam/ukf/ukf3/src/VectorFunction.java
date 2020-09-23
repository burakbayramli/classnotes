
/**
 * A vector-valued function of a vector input.
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.1 $ ($Date: 2003/02/07 02:25:45 $)
 */
public interface VectorFunction {

  /**
   * Returns the input dimension of this function.
   */
  public int getInputDim();

  /**
   * Returns the output dimension of this function.
   */
  public int getOutputDim();

  /**
   * Evaluates this function at the supplied input.
   *
   * @param input an array with {@link #getInputDim()} elements
   * @return an array with {@link #getOutputDim()} elements
   */
  public double[] evaluate(double[] input);
}
