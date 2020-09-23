import org.ejml.data.DMatrixRMaj;
    
public class Test {

    public static MultivariateGaussianDM createState(double cov, double... x) {
	DMatrixRMaj X = new DMatrixRMaj(x.length, 1);
	DMatrixRMaj P = new DMatrixRMaj(x.length, x.length);

	for (int i = 0; i < x.length; i++) {
	    X.set(i, 0, x[i]);
	    P.set(i, i, cov);
	}
	MultivariateGaussianDM ret = new MultivariateGaussianDM();
	ret.assignRef(X, P);

	return ret;
    }
    
    public static void main(String[] args) throws Exception{
	ConstAccel1D constAccelProp = new ConstAccel1D(1.0, 1);	
	EkfPredictor predictor = new KfToEkfPredictor(constAccelProp, null);
	DMatrixRMaj H = new DMatrixRMaj(new double[][]{{1, 1, 1}, {0, 1, 2}});
	EkfProjector projector = new KfToEkfProjector(new FixedKalmanProjector(H));
	UnscentedKalmanFilter f = new UnscentedKalmanFilter(0.3, true, true, predictor, projector);

	DMatrixRMaj s = createState(9.0, 1, 1, 1).getMean();
	f.getProjector().compute(s);
	DMatrixRMaj z = f.getProjector().getProjected();
	MultivariateGaussianDM r = createState(2.0, z.get(0, 0), z.get(1, 0));
	System.out.println(""+r );
    }    
}
