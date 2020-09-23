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
 * A Gaussian (or normal) distribution in N dimensions is stored here.  The distributions is described by a mean
 * and a covariance.
 */
public final class MultivariateGaussianDM {
	// the mean
	public DMatrixRMaj x;
	// the covariance
	public DMatrixRMaj P;

	/**
	 * Creates a new structure with null values for its mean and covariance.
	 * This should be used in conjunction with assignRef()
	 */
	public MultivariateGaussianDM() {
	}

	/**
	 * Creates a distribution with the specified dimentions.  All values will
	 * initially be set to zero.
	 */
	public MultivariateGaussianDM(int dimen) {
		x = new DMatrixRMaj(dimen, 1);
		P = new DMatrixRMaj(dimen, dimen);
	}

	/**
	 * Creates a new distribution that uses the specified matrices.  A copy of the
	 * matrices is made.
	 */
	public MultivariateGaussianDM(DMatrixRMaj x, DMatrixRMaj P) {
		this.x = new DMatrixRMaj(x);
		this.P = new DMatrixRMaj(P);
	}

	/**
	 * Creates a copy of the provided distribution.
	 */
	public MultivariateGaussianDM(MultivariateGaussianDM d) {
		this.x = new DMatrixRMaj(d.getMean());
		this.P = new DMatrixRMaj(d.getCovariance());
	}

	/**
	 * If the parameter is not null, then the reference provided is used internally.
	 */
	public void assignRef(DMatrixRMaj x, DMatrixRMaj P) {
		if (x != null) {
			this.x = x;
		}

		if (P != null) {
			this.P = P;
		}
	}

	public int getDimension() {
		return x.numRows;
	}

	/**
	 * Returns the mean of the Gaussian distribution
	 */
	public DMatrixRMaj getMean() {
		return x;
	}

	/**
	 * Returns the covariance of the Gaussian distribution
	 */
	public DMatrixRMaj getCovariance() {
		return P;
	}

	public MultivariateGaussianDM copy() {
		return new MultivariateGaussianDM(x, P);
	}

	public void set(MultivariateGaussianDM distribution) {
		x.set(distribution.getMean());
		P.set(distribution.getCovariance());
	}
}
