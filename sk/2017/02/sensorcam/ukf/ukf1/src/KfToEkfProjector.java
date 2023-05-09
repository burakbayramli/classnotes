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
 * A wrapper around a KalmanProjector that allows it to be used inside of an EKF.
 */
public class KfToEkfProjector implements EkfProjector {
	private KalmanProjector proj;
	private DMatrixRMaj H;
	private DMatrixRMaj z_hat;

	public KfToEkfProjector(KalmanProjector proj) {
		this.proj = proj;
		H = proj.getProjectionMatrix();
		z_hat = new DMatrixRMaj(proj.getNumStates(), 1);
	}

	@Override
	public int getSystemSize() {
		return proj.getNumStates();
	}

	@Override
	public int getMeasurementSize() {
		return z_hat.numRows;
	}

	@Override
	public void compute(DMatrixRMaj state) {
		CommonOps_DDRM.mult(H, state, z_hat);
	}

	@Override
	public DMatrixRMaj getJacobianH() {
		return H;
	}

	@Override
	public DMatrixRMaj getProjected() {
		return z_hat;
	}
}
