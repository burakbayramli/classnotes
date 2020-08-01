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

import static org.ejml.dense.row.CommonOps_DDRM.multTransB;


/**
 * Here a piecewise constant wiener process acceleration model is formulated.
 * In one dimension.
 */
public class ConstAccel1D implements KalmanPredictor {

	DMatrixRMaj F;
	DMatrixRMaj Q;
	// magnitude of the process noise
	double q;
	// an optional fixed control input.  primarily for debuggin toy problems
	DMatrixRMaj G;
	double T;

	public ConstAccel1D(double q, double T) {
		this.T = T;
		F = new DMatrixRMaj(3, 3);
		Q = new DMatrixRMaj(3, 3);
		this.q = q;

		setT(T);
	}

	public ConstAccel1D(DMatrixRMaj G, double q) {
		F = new DMatrixRMaj(3, 3);
		Q = new DMatrixRMaj(3, 3);
		this.q = q;
		this.G = new DMatrixRMaj(G);

		setT(T);
	}

	private void setT(double T) {
		F.set(0, 0, 1);
		F.set(0, 1, T);
		F.set(0, 2, 0.5 * T * T);
		F.set(1, 1, 1);
		F.set(1, 2, T);
		F.set(2, 2, 1);

		DMatrixRMaj L = new DMatrixRMaj(3, 1);
		L.set(0, 0, 0.5 * T * T);
		L.set(1, 0, T);
		L.set(2, 0, 1);

		multTransB(q, L, L, Q);
	}

	@Override
	public void compute(Object o, double elapsedTime) {}

	@Override
	public DMatrixRMaj getStateTransition() {
		return F;
	}

	@Override
	public DMatrixRMaj getControlTransition() {
		return G;
	}

	@Override
	public DMatrixRMaj getPlantNoise() {
		return Q;
	}

	@Override
	public int getNumStates() {
		return 3;
	}

	@Override
	public int getNumControl() {
		if (G == null)
			return 0;
		else
			return G.numCols;
	}
}
