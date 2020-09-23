/*
 * *********************************************************
 * Copyright (c) 2009 - 2015, DHBW Mannheim - Tigers Mannheim
 * Project: TIGERS - Sumatra
 * Date: 25.07.2015
 * Author(s): AndreR
 * *********************************************************
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import Jama.CholeskyDecomposition;
import Jama.Matrix;

/**
 * Unscented Kalman Filter - additive noise case Taken from
 * "Sigma-Point Kalman Filters for Probabilistic Inference in Dynamic
 * State-Space Models" by Rudolph van der Merwe, p.108, Algorithm 8
 * 
 * @author AndreR
 */
public abstract class UKF
{
    /** Sigma scaling factor */
    private double						c;
	
    private final int					numStates;
    private final int					numMeas;
    private final int					numSigma;
	
    /** Weights for means (1 x numSigma) */
    private final Matrix				Wm;
	
    /** Weights for covariance (1 x numSigma) */
    private final Matrix				Wc;
	
    /** Process noise covariance (numStates x numStates) */
    private final Matrix				Rv;
	
    /** Measurement noise covariance (numMeas x numMeas) */
    private final Matrix				Rn;
	
    /** State covariance (numStates x numStates) */
    private final Matrix				Px;
	
    /** Measurement covariance */
    private final Matrix				Py;
	
    /** State-Measurement cross-covariance */
    private final Matrix				Pxy;
	
    /** Internal state (1 x numStates) */
    private final Matrix				x;
	
    /** Control input (1 x numControl) */
    private final Matrix				u;
	
    /** Sensor input (1 x numMeas) */
    private final Matrix				yk;
	
    /** Internal matrix with sigma points from state (numSigma x numStates) */
    private final Matrix				Xk;
	
    /** Internal matrix with sigma points from measurement (numSigma x numMeas) */
    private final Matrix				Yk;
	
    protected final List<Integer> orientationComponentsState = new ArrayList<Integer>();
    protected final List<Integer> orientationComponentsMeas = new ArrayList<Integer>();
	
	
    /**
     * Initialize the filter.
     * 
     * @param numStates Number of states
     * @param numMeas Number of measurements
     * @param numControl Number of control inputs
     */
    public UKF(final int numStates,
	       final int numMeas,
	       final int numControl)
    {
	this.numStates = numStates;
	this.numMeas = numMeas;
		
	numSigma = (2 * numStates) + 1;
		
	Wm = new Matrix(1, numSigma);
	Wc = new Matrix(1, numSigma);
		
	Rv = Matrix.identity(numStates, numStates);
	Rn = Matrix.identity(numMeas, numMeas);
	Px = Matrix.identity(numStates, numStates);
		
	Py = new Matrix(numMeas, numMeas);
	Pxy = new Matrix(numStates, numMeas);
		
	x = new Matrix(1, numStates);
	u = new Matrix(1, numControl);
	yk = new Matrix(1, numMeas);
		
	Xk = new Matrix(numSigma, numStates);
	Yk = new Matrix(numSigma, numMeas);
		
	configure(0.01, 2.0, 0.0);
    }
	
	
    /**
     * Configure UKF parameters, please read the reference paper
     * for further information.  Default values are suitable for
     * most cases.
     * 
     * @param alpha Alpha value
     * @param beta Beta value, 2 is optimal for gaussian problems
     * @param ki Ki value
     */
    public void configure(final double alpha, final double beta, final double ki)
    {
	double lambda = (alpha * alpha * (numStates + ki)) - numStates;
	double c = numStates + lambda;
	this.c = Math.sqrt(c);
		
	Wm.set(0, 0, lambda / c);
	Wc.set(0, 0, (lambda / c) + ((1 - (alpha * alpha)) + beta));
		
	for (int i = 1; i < numSigma; i++) {
	    Wm.set(0, i, 0.5 / c);
	    Wc.set(0, i, 0.5 / c);
	}
    }
	
	
    /**
     * Draw Sigma Points by using Px and x and put them in Xk.
     */
    private void drawSigmaPoints()
    {
	CholeskyDecomposition decomp = Px.chol();
	// Note: The following assert is commented out on purpose. It
	// may occasionally fail because the matrix Px is not
	// symmetric due to numerical instabilities, but it is always
	// positive semi-definite assert (decomp.isSPD());
		
	// get cholesky
	double[][] chol = decomp.getL().getArray();
		
	double[][] XkA = Xk.getArray();
	double[] xA = x.getArray()[0];
		
	// set Xk - first row
	for (int c = 0; c < numStates; c++) {
	    XkA[0][c] = xA[c];
	}
		
	// set Xk - other rows
	for (int r = 0; r < numStates; r++) {
	    for (int c = 0; c < numStates; c++) {
		XkA[r + 1][c] = xA[c] + (this.c * chol[c][r]);
		XkA[r + numStates + 1][c] = xA[c] - (this.c * chol[c][r]);
	    }
	}
    }
	
	
    /**
     * Call state function to predict future state.
     * 
     * @return Updated state.
     */
    public double[] predict()
    {
	drawSigmaPoints();
	double[][] XkA = Xk.getArray();
		
	// now call state function with sigma points
	double[] control = u.getArray()[0];
		
	for (int i = 0; i < numSigma; i++) {
	    stateFunction(XkA[i], control);
	}
		
	// calculate mean
	double[] xA = x.getArray()[0];
	double[] WmA = Wm.getArray()[0];
	Arrays.fill(xA, 0);
	for (int i = 0; i < numSigma; i++) {
	    for (int c = 0; c < numStates; c++) {
		xA[c] += WmA[i] * XkA[i][c];
	    }
	}
		
	// calculate covariance
	Px.timesEquals(0);
	double[][] PxA = Px.getArray();
	double[] WcA = Wc.getArray()[0];
	for (int i = 0; i < numSigma; i++) {
	    double[] XiA = XkA[i];
			
	    for (int r = 0; r < numStates; r++) {
		for (int c = 0; c < numStates; c++) {
		    PxA[r][c] += WcA[i] * (XiA[r] - xA[r]) * (XiA[c] - xA[c]);
		}
	    }
	}
		
	Px.plusEquals(Rv);
		
	for (Integer i : orientationComponentsState) {
	    xA[i] = AngleMath.normalizeAngle(xA[i]);
	}
		
	return Arrays.copyOf(xA, xA.length);
    }
	
	
    /**
     * Update state with new measurement.
     * 
     * @param measurement Array of new measurements.
     * @return Updated state.
     */
    public double[] update(final double[] measurement)
    {
	drawSigmaPoints();
	double[][] XkA = Xk.getArray();
	double[][] YkA = Yk.getArray();
	double[] control = u.getArray()[0];
		
	// call measurement function with sigma points
	for (int i = 0; i < numSigma; i++) {
	    measurementFunction(XkA[i], control, YkA[i]);
	}
		
	// calculate mean
	double[] ykA = yk.getArray()[0];
	double[] WmA = Wm.getArray()[0];
	Arrays.fill(ykA, 0);
	for (int i = 0; i < numSigma; i++) {
	    for (int c = 0; c < numMeas; c++) {
		ykA[c] += WmA[i] * YkA[i][c];
	    }
	}
		
	// calculate covariance
	Py.timesEquals(0);
	double[][] PyA = Py.getArray();
	double[] WcA = Wc.getArray()[0];
	for (int i = 0; i < numSigma; i++) {
	    double[] YiA = YkA[i];
			
	    for (int r = 0; r < numMeas; r++) {
		for (int c = 0; c < numMeas; c++) {
		    PyA[r][c] += WcA[i] * (YiA[r] - ykA[r]) * (YiA[c] - ykA[c]);
		}
	    }
	}
		
	Py.plusEquals(Rn);
		
	// calculate cross-covariance
	Pxy.timesEquals(0);
	double[][] PxyA = Pxy.getArray();
	double[] xA = x.getArray()[0];
	for (int i = 0; i < numSigma; i++) {
	    double[] XiA = XkA[i];
	    double[] YiA = YkA[i];
			
	    for (int r = 0; r < numStates; r++) {
		for (int c = 0; c < numMeas; c++) {
		    PxyA[r][c] += WcA[i] * (XiA[r] - xA[r]) * (YiA[c] - ykA[c]);
		}
	    }
	}
		
	// calculate Kalman gain
	Matrix K = Pxy.times(Py.inverse());
		
	// calculate new estimate
	Matrix y = new Matrix(measurement, numMeas);
		
	Matrix change = y.minus(yk.transpose());
	double[][] changeA = change.getArray();
	for (Integer i : orientationComponentsMeas) {
	    changeA[i][0] = AngleMath.normalizeAngle(changeA[i][0]);
	}
		
	x.plusEquals(K.times(change).transpose());
		
	for (Integer i : orientationComponentsState) {
	    xA[i] = AngleMath.normalizeAngle(xA[i]);
	}
		
	// calculate new state covariance
	Px.minusEquals(K.times(Pxy.transpose()));
		
	return Arrays.copyOf(xA, xA.length);
    }
	
	
    /**
     * Set process noise covariance.
     * 
     * @note Also resets state covariance to this value.
     * @param index Index in the state vector
     * @param value New covariance value
     */
    public void setProcessNoise(final int index, final double value)
    {
	Rv.set(index, index, value);
	Px.set(index, index, value);
    }
	
	
    /**
     * Set all process noise values at once.
     * 
     * @param values New covariance values.
     */
    public void setProcessNoise(final double... values)
    {
	for (int i = 0; i < numStates; i++)
	    {
		Rv.set(i, i, values[i]);
		Px.set(i, i, values[i]);
	    }
    }
	
	
    /**
     * Set measurement noise covariance.
     * 
     * @param index Index in the measurement vector
     * @param value New covariance value
     */
    public void setMeasurementNoise(final int index, final double value)
    {
	Rn.set(index, index, value);
    }
	
	
    /**
     * Set all measurement noise values at once.
     * 
     * @param values New covariance values.
     */
    public void setMeasurementNoise(final double... values)
    {
	for (int i = 0; i < numMeas; i++) {
	    Rn.set(i, i, values[i]);
	}
    }
	
	
    /**
     * Override internal state value.
     * 
     * @param values New state.
     */
    public void setState(final double... values)
    {
	for (int i = 0; i < numStates; i++) {
	    x.set(0, i, values[i]);
	}
    }
	
	
    /**
     * Set control input.
     * Not used in input-less configurations.
     * 
     * @param values Input values, passed on to state an measurement function.
     */
    public void setControl(final double... values)
    {
	double[] control = u.getArray()[0];
	System.arraycopy(values, 0, control, 0, values.length);
    }
	
	
    /**
     * Add orientation component. They are automatically normalized.
     * 
     * @param stateIndex State index
     * @param measIndex Meas index
     */
    public void addOrientationComponent(final int stateIndex, final int measIndex)
    {
	orientationComponentsState.add(stateIndex);
	orientationComponentsMeas.add(measIndex);
    }
	
	
    /**
     * Get current state estimate.
     * 
     * @return Current state.
     */
    public double[] getState()
    {
	double[] xA = x.getArray()[0];
		
	return Arrays.copyOf(xA, xA.length);
    }
	
	
    /**
     * State prediction function.
     * 
     * @param stateInOut State input and output. Modify this in your function.
     * @param controlIn Control vector input.
     */
    protected abstract void stateFunction(double[] stateInOut, double[] controlIn);
	
	
    /**
     * Measurement mapping function.
     * This function must map the states to the expected measurement values.
     * 
     * @param stateIn State input.
     * @param controlIn Control input.
     * @param measOut Measurement output.
     */
    protected abstract void measurementFunction(double[] stateIn, double[] controlIn, double[] measOut);
}
