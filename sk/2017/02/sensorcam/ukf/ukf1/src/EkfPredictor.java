/*
 * Copyright (c) 2013-2014, Peter Abeles. All Rights Reserved.
 *
 * This file is part of Project BUBO.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


import org.ejml.data.DMatrixRMaj;

/**
 * A state propagator for an extended Kalman filter.
 */
public interface EkfPredictor<Control> {

	/**
	 * Before any of the other functions are called this needs to be called first.
	 * It tells the propagator to compute matrices for a time step
	 *
	 * @param state The initial system state. Not modified.
	 * @param control Known control input.  null if there is none
	 * @param elapsedTime Elapsed time since previous prediction.  Ignored by discrete filters
	 */
	public void predict(DMatrixRMaj state, Control control , double elapsedTime);

	/**
	 * Returns the jacobian of f(x) with respect to x.
	 *
	 * @return State transition Jacobian matrix. WARNING DO NOT MODIFY!  DATA IS MODIFIED EACH TIME COMPUTE IS CALLED.
	 */
	public DMatrixRMaj getJacobianF();

	/**
	 * Returns the propagation noise.
	 *
	 * @return Plant noise covariance matrix. WARNING DO NOT MODIFY!  DATA IS MODIFIED EACH TIME COMPUTE IS CALLED.
	 */
	public DMatrixRMaj getPlantNoise();

	/**
	 * Returns the predicted state at the next time step.
	 *
	 * @return Predicted state column vector. WARNING DO NOT MODIFY!  DATA IS MODIFIED EACH TIME COMPUTE IS CALLED.
	 */
	public DMatrixRMaj getPredictedState();

	/**
	 * The number of states in the state vector which is being manipulated
	 */
	public int getSystemSize();
}
