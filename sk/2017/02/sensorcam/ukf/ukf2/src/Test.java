import java.util.Arrays;
import org.junit.Assert;
    
public class Test {

    /***********************************************************
     *  PosVelUKF
     */
    private static class PosVelUKF extends UKF
    {
	private final double	deltaT;
				
	public PosVelUKF(final double dT)
	{
	    super(2, 1, 0);			
	    deltaT = dT;
	}
		
	protected void stateFunction(final double[] stateInOut,
				     final double[] controlIn)
	{
	    // linear velocity model
	    stateInOut[0] += stateInOut[1] * deltaT;
	}

	protected void measurementFunction(final double[] stateIn,
					   final double[] controlIn,
					   final double[] measOut)
	{
	    // measurement is position directly
	    measOut[0] = stateIn[0];
	}
    }
	
    /***********************************************************
     *  BotUKF
     */
    private static class BotUKF extends UKF
    {
	private final double	deltaT;
				
	public BotUKF(final double dT)
	{
	    super(6, 3, 0);			
	    deltaT = dT;
	}
				
	protected void stateFunction(final double[] stateInOut,
				     final double[] controlIn)
	{
	    // non-linear bot model
	    double[] pos = Arrays.copyOfRange(stateInOut, 0, 3);
	    double[] vel = Arrays.copyOfRange(stateInOut, 3, 6);
			
	    // rotate velocity according to angular velocity
	    double rot = vel[2] * deltaT;
			
	    vel[0] = (vel[0] * Math.cos(rot)) - (vel[1] * Math.sin(rot));
	    vel[1] = (vel[1] * Math.cos(rot)) + (vel[0] * Math.sin(rot));
			
	    pos[0] += vel[0] * deltaT;
	    pos[1] += vel[1] * deltaT;
	    pos[2] += vel[2] * deltaT;
			
	    stateInOut[0] = pos[0];
	    stateInOut[1] = pos[1];
	    stateInOut[2] = pos[2];
	    stateInOut[3] = vel[0];
	    stateInOut[4] = vel[1];
	    stateInOut[5] = vel[2];
	}

	protected void measurementFunction(final double[] stateIn,
					   final double[] controlIn,
					   final double[] measOut)
	{
	    measOut[0] = stateIn[0];
	    measOut[1] = stateIn[1];
	    measOut[2] = stateIn[2];
	}
    } /* end of class */
	
    /***********************************************************
     *  Testing
     ***********************************************************/
    
    public static void testPosVelUKF()
    {
	PosVelUKF ukf = new PosVelUKF(0.01);
	ukf.setProcessNoise(0.01, 10.0);
	ukf.setMeasurementNoise(0.01);
		
	double[] meas = new double[1];
	meas[0] = 0.1;
		
	double[] state = new double[2];
		
	for (int i = 0; i < 20; i++)
	    {
		ukf.predict();
			
		state = ukf.update(meas);
		// System.out.println(Arrays.toString(state));
	    }
		
	Assert.assertEquals(meas[0], state[0], 0.001);
	Assert.assertEquals(0, state[1], 0.01);
    }
		
    public static void testBotUKF()
    {
	BotUKF ukf = new BotUKF(0.01);
	ukf.setProcessNoise(0.001, 0.001, 0.001, 0.5, 0.5, 0.5);
	ukf.setMeasurementNoise(0.01, 0.01, 0.01);
	ukf.setState(0, 0, -2, 0, 0, 0);
	ukf.addOrientationComponent(2, 2);
		
	double[] meas = { 1, 1, 2 };
		
	double[] state = new double[6];
		
	for (int i = 0; i < 40; i++) {
	    ukf.predict();	    
	    state = ukf.update(meas);
	    System.out.println(Arrays.toString(state));
	}

	System.out.println(""+meas[0] );
	Assert.assertEquals(meas[0], state[0], 0.001);
	Assert.assertEquals(meas[1], state[1], 0.001);
	Assert.assertEquals(meas[2], state[2], 0.001);
	Assert.assertEquals(0, state[3], 0.01);
	Assert.assertEquals(0, state[4], 0.01);
	Assert.assertEquals(0, state[5], 0.01);
    }
    
    public static void main(String[] args) throws Exception{
	testPosVelUKF();
	testBotUKF();
    }    
}
