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
 * Projects an observed object's state into sensor space to predict what the sensor should see given
 * its current model of the world's state.
 */
public interface EkfProjector {

	/**
	 * Returns the number of states in the system.
	 */
	public int getSystemSize();

	/**
	 * Returns the number of states in the measurement
	 */
	public int getMeasurementSize();

	/**
	 * Computes a measurement given the system's current state
	 *
	 * @param systemState The state of the system making the observation.
	 */
	public void compute(DMatrixRMaj systemState);

	/**
	 * <p>
	 * Returns the jacobian of h(x) with respect to x<br>
	 * <br>
	 * H_k = \frac{\partial h}{\partial x}|_{\hat{x}_{k|k-1}}<br>
	 * where H_k has dimensions M by N, M = measurement size and N is system size.
	 * </p>
	 *
	 * @return Observation Jacobian matrix. WARNING DO NOT MODIFY!
	 * DATA IS MODIFIED EACH TIME {@link #compute} IS CALLED.
	 */
	public DMatrixRMaj getJacobianH();

	/**
	 * Returns the estimated measurement, given the current state estimate.
	 *
	 * @return Estimated measurement column vector.
	 * WARNING DO NOT MODIFY!  DATA IS MODIFIED EACH TIME {@link #compute} IS CALLED.
	 */
	public DMatrixRMaj getProjected();
}
