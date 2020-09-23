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
 * When a measurement is used to update the filter's state several statistics need to be
 * calculated.  These statistics can be useful else where too.  This interface provides
 * a generic mechanism for accessing that information.
 */
// TODO if nothing is using this, delete it
public interface InnovationInterface {

	public DMatrixRMaj getInnovation();

	public DMatrixRMaj getInnovationCov();

	public DMatrixRMaj getInnovationCovInverse();

}
