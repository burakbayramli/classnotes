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
import org.ejml.dense.row.CommonOps_DDRM;

/**
 * A simple wrapper that lets a KF propagator be used as a propagator for an EKF.
 * If the model is not linear this is highly likely
 */
public class KfToEkfPredictor<Control> implements EkfPredictor<Control> {

	private KalmanPredictor<Control> predictor;
	private DMatrixRMaj controlInput;
	private DMatrixRMaj a;

	public KfToEkfPredictor(KalmanPredictor<Control> predictor, DMatrixRMaj controlInput) {
		this.predictor = predictor;
		this.a = new DMatrixRMaj(predictor.getNumStates(), 1);
		this.controlInput = controlInput;
	}

	@Override
	public void predict(DMatrixRMaj state, Control control, double elapsedTime) {
		predictor.compute(control,elapsedTime);
		DMatrixRMaj F = predictor.getStateTransition();
		DMatrixRMaj G = predictor.getControlTransition();

		// predict the state
		CommonOps_DDRM.mult(F, state, a);

		// handle the control, if there is one
		if (G != null) {
			CommonOps_DDRM.multAdd(G, controlInput, a);
		}
	}

	public DMatrixRMaj getControlInput() {
		return controlInput;
	}

	@Override
	public DMatrixRMaj getJacobianF() {
		return predictor.getStateTransition();
	}

	@Override
	public DMatrixRMaj getPlantNoise() {
		return predictor.getPlantNoise();
	}

	@Override
	public DMatrixRMaj getPredictedState() {
		return a;
	}

	@Override
	public int getSystemSize() {
		return predictor.getNumStates();
	}
}
