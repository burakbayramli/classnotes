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



/**
 * There are many different forums of the KalmanFilter
 */
public interface KalmanFilterInterface<Control> {

	/**
	 * Predictor which supports discrete and continuous-discrete models.
	 *
	 * @param state The initial system state. Not modified.
	 * @param control Known control input.  null if there is none
	 * @param elapsedTime Elapsed time since previous prediction. Ignored by discrete systems.
	 */
	public void predict(MultivariateGaussianDM state, Control control , double elapsedTime );

	public void update(MultivariateGaussianDM state, MultivariateGaussianDM meas);
}
