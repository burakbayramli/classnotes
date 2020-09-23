import org.ejml.data.DMatrixRMaj;
import org.ejml.dense.row.factory.DecompositionFactory_DDRM;
import org.ejml.interfaces.decomposition.CholeskyDecomposition_F64;
import org.ejml.dense.row.CommonOps_DDRM;

/**
 * An implementation of the Unscented Kalman Filter based on the description in this paper:
 * <p/>
 * Simon Julier, Jeffrey Uhlmann, and Hugh F. Durrant-Whyte "A New Method for the Nonlinear Transformation
 * of Means and Covariances in Filters and Estimators"  IEEE AES Vol. 45, No 3, March 2000
 * <p/>
 * and their 1997 paper.
 * <p/>
 * ---------------------------
 * <p/>
 * The n-dimensional random variable x(k) with mean \hat{x}(k|k) and covariance P(k|k) is approximated
 * by 2n+1 weighted samples or sigma points selected by the algorithm.
 * <p/>
 * In this implementation the unscented transform can be applied to the prediction and update steps
 * independently.  If the unscented tranform is not used the extended Kalman filter equations are used
 * instead.
 * <p/>
 * The following system equations are assumed:
 * <p/>
 * x(k+1) = f( x(k) , u(k) ) + w(k)
 * z(k)   = h( x(k) ) + v(k)
 * <p/>
 * where w(k) and v(k) are independent white Gaussian noise processes.
 * <p/>
 * *note* This is a straight forward implementation.  There are ways to slightly improve performance (some times
 * at the cost of accuracy) that have not been used.
 */
@SuppressWarnings({"ForLoopReplaceableByForEach"})
public class UnscentedKalmanFilter<Control> implements KalmanFilterInterface<Control> {

    // should it perform an unscented update on the prediction and/or update?
    boolean unscentedPred;
    boolean unscentedUpdate;
    // tuning parameters
    private double kappa;
    // the DOF of the state
    private int n;
    // target and sensor models
    private EkfPredictor<Control> predictor;
    private EkfProjector projector;

    // sample points
    private DMatrixRMaj points[];
    private DMatrixRMaj measPoints[];
    private double weights[];
    // used to handle kappa's less than 0
    // this is the first index in the points list thats used
    private int firstCovIndex;

    // used to update the state when the unscented transform has not been requested
    private ExtendedKalmanFilter<Control> ekf;

    // used during the unscented prediction and update steps
    private DMatrixRMaj tempMxM;
    private DMatrixRMaj temp0_Nx1;
    private DMatrixRMaj temp1_Nx1;
    private DMatrixRMaj temp0_NxN;
    private DMatrixRMaj temp1_NxN;
    private DMatrixRMaj temp0_MxN;
    private DMatrixRMaj temp1_MxN;

    // used to compute the matrix square root
    private CholeskyDecomposition_F64<DMatrixRMaj> choleky;
    private DMatrixRMaj tempL;

    /**
     * @param kappa           a tuning parameter the effects the distribution of sample points. kappa \in Re
     * @param unscentedPred   If true an unscented transform will be applied to the prediction
     * @param unscentedUpdate If true an unscented transform will be applied to the update
     * @param predictor       Used to predict the state of the system at a future time
     * @param projector       Creates a measurement from the given state.
     */
    public UnscentedKalmanFilter(double kappa,
				 boolean unscentedPred,
				 boolean unscentedUpdate,
				 EkfPredictor<Control> predictor,
				 EkfProjector projector) {
	if (!unscentedPred && !unscentedUpdate)
	    throw new IllegalArgumentException("No point in using the unscented filter if it " +
					       "is turned off for both prediction and update");

	this.kappa = kappa;
	this.predictor = predictor;
	this.projector = projector;
	this.unscentedPred = unscentedPred;
	this.unscentedUpdate = unscentedUpdate;
	this.n = predictor.getSystemSize();

	if (kappa <= 0) {
	    firstCovIndex = 1;
	} else {
	    firstCovIndex = 0;
	}

	weights = new double[2 * n + 1];

	// get the DOF of the state and measurement
	int M = predictor.getSystemSize();
	int N = projector.getSystemSize();


	points = new DMatrixRMaj[2 * n + 1];
	for (int i = 0; i < points.length; i++) {
	    points[i] = new DMatrixRMaj(M, 1);
	}

	if (unscentedUpdate) {
	    measPoints = new DMatrixRMaj[2 * n + 1];
	    for (int i = 0; i < measPoints.length; i++) {
		measPoints[i] = new DMatrixRMaj(N, 1);
	    }
	}

	if (!unscentedPred || !unscentedUpdate) {
	    ekf = new ExtendedKalmanFilter<Control>(predictor, projector);
	}

	weights[0] = kappa / (n + kappa);
	double w = 1.0 / (2.0 * (n + kappa));
	for (int i = 1; i <= n; i++) {
	    weights[i] = w;
	    weights[i + n] = w;
	}

	choleky = DecompositionFactory_DDRM.chol(M, true);
	tempL = new DMatrixRMaj(M, M);

	declareTemporary(M, N);
    }

    private void declareTemporary(int M, int N) {
	tempMxM = new DMatrixRMaj(M, M);
	temp0_Nx1 = new DMatrixRMaj(N, 1);
	temp1_Nx1 = new DMatrixRMaj(N, 1);
	temp0_NxN = new DMatrixRMaj(N, N);
	temp1_NxN = new DMatrixRMaj(N, N);
	temp0_MxN = new DMatrixRMaj(M, N);
	temp1_MxN = new DMatrixRMaj(M, N);
    }

    public EkfProjector getProjector() {
	return projector;
    }

    public EkfPredictor getPredictor() {
	return predictor;
    }


    /**
     * Intelligently sample the points of the distribution
     */
    private void setPoints(MultivariateGaussianDM output) {
	DMatrixRMaj x = output.getMean();
	DMatrixRMaj P = output.getCovariance();

	tempMxM.set(P);
	CommonOps_DDRM.scale(n + kappa, tempMxM);

	tempL.set(tempMxM);
	if (!choleky.decompose(tempL))
	    throw new RuntimeException("Cholesky failed");

	DMatrixRMaj A = choleky.getT(null);
	//        UtilMtjMatrix.print(A,null);

	points[0].set(x);

	for (int i = 1; i <= n; i++) {

	    DMatrixRMaj pA = points[i];
	    DMatrixRMaj pB = points[i + n];

	    for (int j = 0; j < x.numRows; j++) {
		pA.set(j, 0, x.get(j, 0) + A.get(j, i - 1));
		pB.set(j, 0, x.get(j, 0) - A.get(j, i - 1));
	    }
	}
    }

    /**
     * Predicts the state of the system at the end of the next time step.
     *
     * @param state Initially its the current state of the system.
     *              After the function exits, it is the predicted
     *              state of the system
     */
    @Override
    public void predict(MultivariateGaussianDM state, Control control, double elapsedTime) {
	if (unscentedPred) {
	    predictUnscented(state,control,elapsedTime);
	} else {
	    ekf.predict(state,control,elapsedTime);
	}
    }

    /**
     * Updates the state using the specified measurement.
     *
     * @param state The current state of the system.  When the
     *              function exits, it is the updated state of the
     *              system.
     * @param meas  The measurement which will be used to update the system.
     */
    public void update(MultivariateGaussianDM state, MultivariateGaussianDM meas) {
	if (unscentedUpdate) {
	    updateUnscented(state, meas);
	} else {
	    ekf.update(state, meas);
	}
    }

    /**
     * Predicts the state unsing the unscented transforme
     */
    private void predictUnscented(MultivariateGaussianDM state,
				  Control control,
				  double elapsedTime) {
	setPoints(state);

	DMatrixRMaj x = state.getMean();
	DMatrixRMaj P = state.getCovariance();

	x.zero();

	// compute the state
	for (int i = 0; i < points.length; i++) {
	    DMatrixRMaj p = points[i];

	    predictor.predict(p,control,elapsedTime);
	    p.set(predictor.getPredictedState());

	    CommonOps_DDRM.add(x, weights[i], p, x);
	}

	// compute the covariance with plant noise
	predictor.predict(x, control, elapsedTime);
	P.set(predictor.getPlantNoise());

	for (int i = firstCovIndex; i < points.length; i++) {
	    DMatrixRMaj p = points[i];

	    CommonOps_DDRM.add(p, -1, x, p);
	    CommonOps_DDRM.multAddTransB(weights[i], p, p, P);
	}

    }

    /**
     * Updates the state with the measurement and using the unscented transform
     */
    private void updateUnscented(MultivariateGaussianDM state,
				 MultivariateGaussianDM meas) {
	setPoints(state);

	DMatrixRMaj x = state.getMean();
	DMatrixRMaj P = state.getCovariance();

	DMatrixRMaj z = meas.getMean();
	DMatrixRMaj R = meas.getCovariance();

	// compute predicted measurement for each point as well as the weighted
	// average of all the predited points
	DMatrixRMaj z_hat = computePredictedMeas();

	// estimate predicted measurement covariance
	DMatrixRMaj Pz = computePredMeasCov(R, z_hat);

	// estimate the cross covariance
	DMatrixRMaj Pxz = computeCrossCov(x);

	// perform the unscented update

	// K = Pxz Pz^{-1}
	DMatrixRMaj K = temp1_MxN;
	CommonOps_DDRM.invert(Pz, temp1_NxN);
	CommonOps_DDRM.mult(Pxz, temp1_NxN, K);

	// x = x + K*(z-\hat{z})
	temp1_Nx1.set(z);
	CommonOps_DDRM.add(temp1_Nx1, -1, z_hat, temp1_Nx1);
	CommonOps_DDRM.multAdd(K, temp1_Nx1, x);

	// P = P - K*PzK'
	CommonOps_DDRM.mult(K, Pz, temp0_MxN);
	CommonOps_DDRM.multAddTransB(-1, temp0_MxN, K, P);
    }

    private DMatrixRMaj computeCrossCov(DMatrixRMaj x) {
	// Pxz = sum(i=0:2n+1) ( w_i (x_i - x)(z_ihat - z_hat)' )
	DMatrixRMaj Pxz = temp0_MxN;
	Pxz.zero();
	for (int i = firstCovIndex; i < measPoints.length; i++) {
	    //
	    DMatrixRMaj pX = points[i];
	    CommonOps_DDRM.add(pX, -1, x, pX);
	    DMatrixRMaj pZ = measPoints[i];

	    CommonOps_DDRM.multAddTransB(weights[i], pX, pZ, Pxz);
	}
	return Pxz;
    }

    private DMatrixRMaj computePredMeasCov(DMatrixRMaj r, DMatrixRMaj z_hat) {
	DMatrixRMaj Pz = temp0_NxN;
	Pz.set(r);

	// Pz = sum(i=0:2n+1) ( w_i (z_ihat - z_hat)(...)' )
	for (int i = firstCovIndex; i < measPoints.length; i++) {
	    DMatrixRMaj z_ihat = measPoints[i];
	    CommonOps_DDRM.add(z_ihat, -1, z_hat, z_ihat);

	    CommonOps_DDRM.multAddTransB(weights[i], z_ihat, z_ihat, Pz);
	}
	return Pz;
    }

    private DMatrixRMaj computePredictedMeas() {
	DMatrixRMaj z_hat = temp0_Nx1;
	z_hat.zero();
	for (int i = 0; i < points.length; i++) {
	    DMatrixRMaj p = points[i];

	    projector.compute(p);
	    DMatrixRMaj z_ihat = projector.getProjected();
	    measPoints[i].set(z_ihat);
	    CommonOps_DDRM.add(z_hat, weights[i], z_ihat, z_hat);
	}
	return z_hat;
    }
}
