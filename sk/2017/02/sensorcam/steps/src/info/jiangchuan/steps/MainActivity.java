package info.jiangchuan.steps;

import java.util.*;
import java.io.*;
import android.os.*;
import android.view.*;
import android.widget.*;
import android.location.*;
import android.hardware.*;
import android.util.Log;
import android.content.Context;
import android.graphics.drawable.*;
import android.graphics.Color;
import android.view.ViewParent;
import android.app.*;

public class MainActivity extends Activity implements SensorEventListener
{
    private final String TAG = "MainActivity";

    // view elements
    private TextView mDir; 
    private TextView mPace;
    private TextView mStopWatch;

    long lastTime = 0;
    
    private SensorManager mSensorManager;
    
    private LocationManager locationManager = null;
    
    private PrisLocationListener mLocationListener;

    ArrayList<float[]> accs = new ArrayList<float[]>();
    ArrayList<Long> accTs = new ArrayList<Long>();
    
    ArrayList<float[]> laccs = new ArrayList<float[]>();
    ArrayList<Long> laccTs = new ArrayList<Long>();
    
    ArrayList<float[]> grav = new ArrayList<float[]>();
    ArrayList<Long> gravTs = new ArrayList<Long>();
    
    ArrayList<Long> magTs = new ArrayList<Long>();
    ArrayList<float[]> mags = new ArrayList<float[]>();

    ArrayList<Long> gpsTs = new ArrayList<Long>();
    ArrayList<double[]> gps = new ArrayList<double[]>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
	getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        setContentView(R.layout.activity_main);

        mDir = (TextView)findViewById(R.id.textView_steps);
        mPace = (TextView)findViewById(R.id.textView_StepsPerMinute);
        mStopWatch = (TextView)findViewById(R.id.textView_stopWatch);
        mSensorManager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),SensorManager.SENSOR_DELAY_GAME);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD),SensorManager.SENSOR_DELAY_FASTEST);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_LINEAR_ACCELERATION),SensorManager.SENSOR_DELAY_GAME);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_GRAVITY),SensorManager.SENSOR_DELAY_GAME);

	PrisLocationListener locationListener = new PrisLocationListener();
	locationListener.parent = this;
	HandlerThread t = new HandlerThread("LocationHandlerThread");
	t.start();
	locationManager = (LocationManager)getSystemService(LOCATION_SERVICE);
	locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, locationListener, t.getLooper());
    }

    public static String base_uri = "/storage/emulated/0/Steps/";

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }
    
    public void onAccuracyChanged(Sensor sensor, int accuracy) { }

    public void onSensorChanged(SensorEvent event) {
	if (event.sensor.getType() == Sensor.TYPE_ACCELEROMETER) {
	    lastTime = event.timestamp;
	    accTs.add(lastTime);
	    accs.add(event.values.clone());	   			    
	}
	else if (event.sensor.getType() == Sensor.TYPE_LINEAR_ACCELERATION) {
	    laccTs.add(lastTime);
	    laccs.add(event.values.clone());

	    if (gps.size() > 0) {
		double [] g = gps.get(gps.size()-1);
		String s = Double.toString(g[0]).substring(0,8) + " " + Double.toString(g[1]).substring(0,8);
		mStopWatch.setText(s);
	    }
	    
	}
	else if (event.sensor.getType() == Sensor.TYPE_GRAVITY) {
	    gravTs.add(lastTime);
	    grav.add(event.values.clone());
	}	
	else if (event.sensor.getType() == Sensor.TYPE_MAGNETIC_FIELD) {
	    magTs.add(lastTime);
	    mags.add(event.values.clone());	    
	}

	//mDir.setText("-");
	//mPace.setText("-");
	
    }

    public static class PrisLocationListener implements LocationListener{

	public MainActivity parent = null;
	
	@Override
	public void onLocationChanged(Location arg0) {
	    double [] tmp = new double[]{arg0.getLatitude(),
				       arg0.getLongitude(),
				       arg0.getBearing(),
				       arg0.getSpeed(),
				       arg0.getAccuracy(),
				       arg0.getAltitude()};
	    parent.gpsTs.add(parent.lastTime);
	    parent.gps.add(tmp);
	}

	@Override
	public void onProviderDisabled(String provider) {
	}

	@Override
	public void onProviderEnabled(String provider) {
	}

	@Override
	public void onStatusChanged(String provider, int status, Bundle extras) {
	}
    }
    
    public void turnoff(View view) {
    }
    
    public void record(View view) {

	String outDir = base_uri + "/" + Calendar.getInstance().getTimeInMillis();
	File tmp = new File(outDir);
	tmp.mkdir();
			    
	final File path = new File(outDir);
	Log.d("cam","path "+ path);
		
	File filea = new File(path, "acc.txt");
	try {
	    filea.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filea);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<accs.size(); i++) {
		float[] x = accs.get(i);
		String xx = "" +
		    accTs.get(i) + " " +
		    Float.toString(x[0]) + " " + Float.toString(x[1]) + " " + Float.toString(x[2]);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filema = new File(path, "mags.txt");
	try {
	    filema.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filema);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<mags.size(); i++) {
		float[] x = mags.get(i);
		String xx = "" +
		    magTs.get(i) + " " +
		    Float.toString(x[0]) + " " + Float.toString(x[1]) + " " + Float.toString(x[2]);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File fileal = new File(path, "lacc.txt");
	try {
	    fileal.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(fileal);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<laccs.size(); i++) {
		float[] x = laccs.get(i);
		String xx = "" +
		    laccTs.get(i) + " " +
		    Float.toString(x[0]) + " " + Float.toString(x[1]) + " " + Float.toString(x[2]);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filegr = new File(path, "grav.txt");
	try {
	    filegr.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filegr);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<grav.size(); i++) {
		float[] x = grav.get(i);
		String xx = "" +
		    gravTs.get(i) + " " +
		    Float.toString(x[0]) + " " + Float.toString(x[1]) + " " + Float.toString(x[2]);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File fileg = new File(path, "gps.txt");
	try {
	    fileg.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(fileg);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<gps.size(); i++) {
		double[] x = gps.get(i);
		String xx = gpsTs.get(i) + " ";
		for (int j=0;j<6;j++) xx += x[j] + " ";
		myOutWriter.append(xx);
		myOutWriter.append("\n");

	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	AlertDialog.Builder builder=new AlertDialog.Builder(this);
	builder
	    .setTitle("Msg")
	    .setMessage("Done")
	    .setPositiveButton("OK", null)
	    .show();
    }


    @Override
    public void onBackPressed() {
	android.os.Process.killProcess(android.os.Process.myPid());
	System.exit(0);
    }
    
    
}
