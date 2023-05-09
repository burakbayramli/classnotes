package my.project.MyCamera;

import java.io.File;

import android.os.*;
import android.view.*;
import android.app.*;
import android.view.ViewGroup.LayoutParams;
import android.widget.FrameLayout;
import android.content.Context;
import android.hardware.*;
import android.location.*;
import android.content.Intent;
import android.util.Log;
import android.os.HandlerThread;
import android.speech.tts.TextToSpeech;
import android.widget.*;
import android.telephony.*;
import android.telephony.gsm.*;
import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.sql.*;
 
public class MyCamera extends Activity implements SensorEventListener,
						  RadioGroup.OnCheckedChangeListener,
						  GpsStatus.Listener
{
    private CameraPreview camPreview; 
    private FrameLayout mainLayout;
    private int PreviewSizeWidth = 320;
    private int PreviewSizeHeight= 240;    
    private Handler mHandler = new Handler(Looper.getMainLooper());

    private SensorManager mSensorManager;
    public float[] mAccelerometerReading = new float[3];
    public float[] mLAccelerometerReading = new float[3];
    public float[] mMagnetometerReading = new float[3];
    public float[] mRotationMatrix = new float[9];
    public float[] mOrientationAngles = new float[3];

    public String gpsInfo = null;
    public String gpsInfo2 = null;
    public String satInfo = null;
    public String cellInfo = null;
    public double latitude;
    public double longitude;
    LocationManager locationManager = null;

    private PrisLocationListener mLocationListener;

    TextView textView = null;
    EditText textEdit = null;

    TextToSpeech t1;

    private static final String[] items={"","1", "2"};
    private static final String[] locs={"berlin","world1", "world2", "istanbul"};

    SelectListener slCities = new SelectListener(locs);
    SelectListener slTrips = new SelectListener(items);

    RadioGroup tripRadio = null;

    public MyApp appState = null;

    public TelephonyManager telephonyManager = null; 
    
    @Override
    public void onCreate(Bundle savedInstanceState) 
    {
        super.onCreate(savedInstanceState);

	appState = ((MyApp)getApplicationContext());
	
	mSensorManager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);     
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),SensorManager.SENSOR_DELAY_GAME);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_LINEAR_ACCELERATION),SensorManager.SENSOR_DELAY_GAME);
	mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD),SensorManager.SENSOR_DELAY_GAME);
	//mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_ORIENTATION),SensorManager.SENSOR_DELAY_GAME);	

	PrisLocationListener locationListener = new PrisLocationListener();
	locationListener.parent = this;
	HandlerThread t = new HandlerThread("LocationHandlerThread");
	t.start();
	locationManager = (LocationManager)getSystemService(LOCATION_SERVICE);
	locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, locationListener, t.getLooper());

	telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
	CellLocationListener phoneListener = new CellLocationListener();
	phoneListener.parent = this;
	phoneListener.telephonyManager = telephonyManager;
	telephonyManager.listen(phoneListener, CellLocationListener.LISTEN_CELL_LOCATION);
	
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,  
			     WindowManager.LayoutParams.FLAG_FULLSCREEN);
	
        requestWindowFeature(Window.FEATURE_NO_TITLE);  
        setContentView(R.layout.main);

	textView = (TextView)findViewById(R.id.txt02);
        
    	SurfaceView camView = new SurfaceView(this);
        SurfaceHolder camHolder = camView.getHolder();
        camPreview = new CameraPreview(PreviewSizeWidth, PreviewSizeHeight);
	camPreview.mCameraActivity = this;
        
        camHolder.addCallback(camPreview);
        camHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
        
        mainLayout = (FrameLayout) findViewById(R.id.frameLayout1);
        mainLayout.addView(camView, new LayoutParams(PreviewSizeWidth, PreviewSizeHeight));

	t1=new TextToSpeech(getApplicationContext(), new TextToSpeech.OnInitListener() {
		@Override
		public void onInit(int status) {
		    if(status != TextToSpeech.ERROR) {
			t1.setLanguage(Locale.UK);
		    }
		}
	    });
	
	appState.t1 = t1;
	appState.startTrip();
	
	Spinner spin=(Spinner)findViewById(R.id.spinner);
	spin.setOnItemSelectedListener(slTrips);
	ArrayAdapter<String> aa=new ArrayAdapter<String>(this,
							 android.R.layout.simple_spinner_item,
							 items);
	aa.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
	spin.setAdapter(aa);

	Spinner spin2=(Spinner)findViewById(R.id.spinner2);
	spin2.setOnItemSelectedListener(slCities);
	ArrayAdapter<String> bb=new ArrayAdapter<String>(this,
							 android.R.layout.simple_spinner_item,
							 locs);
	bb.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
	spin2.setAdapter(bb);

	tripRadio=(RadioGroup)findViewById(R.id.trip);
	tripRadio.setOnCheckedChangeListener(this);
	
	locationManager.addGpsStatusListener(this);
	
    }

    public void onCheckedChanged(RadioGroup group, int checkedId) {
	switch (checkedId) {
	case R.id.yestrip:
	    Log.d("cam", "trip on");
	    break;
	case R.id.notrip:
	    Log.d("cam", "trip off");	    
	    break;
	}
    }    

    @Override 
    public boolean onTouchEvent(MotionEvent event) 
    { 
    	return true;
    };

    @Override
    public void onSensorChanged(SensorEvent event) {
	if (event.sensor.getType() == Sensor.TYPE_ACCELEROMETER) {
	    System.arraycopy(event.values, 0, mAccelerometerReading,
			     0, mAccelerometerReading.length);
	}
	else if (event.sensor.getType() == Sensor.TYPE_LINEAR_ACCELERATION) {
	    System.arraycopy(event.values, 0, mLAccelerometerReading,
			     0, mLAccelerometerReading.length);
	}
	else if (event.sensor.getType() == Sensor.TYPE_MAGNETIC_FIELD) {
	    System.arraycopy(event.values, 0, mMagnetometerReading,
			     0, mMagnetometerReading.length);
	}

	mSensorManager.getRotationMatrix(mRotationMatrix,
					 null,
					 mAccelerometerReading,
					 mMagnetometerReading);	

	mSensorManager.getOrientation(mRotationMatrix, mOrientationAngles);

	double or0 = mOrientationAngles[0];
	double or1 = mOrientationAngles[1];
	double or2 = mOrientationAngles[2];
	double acc0 = mAccelerometerReading[0];
	double acc1 = mAccelerometerReading[1];
	double acc2 = mAccelerometerReading[2];
	double lacc0 = mLAccelerometerReading[0];
	double lacc1 = mLAccelerometerReading[1];
	double lacc2 = mLAccelerometerReading[2];
	
	String disp =
	    "Orientation:" + Util.round(or0) + " " + Util.round(or1) + " " + Util.round(or2) + "\n" +
	    "GPS:" + gpsInfo;
	textView.setText(disp);
    }
    
    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {	
    }

    @Override
    public void onGpsStatusChanged(int event){
	GpsStatus gpsStatus = locationManager.getGpsStatus(null);
	if(gpsStatus != null) {
	    Iterable<GpsSatellite>satellites = gpsStatus.getSatellites();
	    Iterator<GpsSatellite>sat = satellites.iterator();
	    String strGpsStats = "";
	    while (sat.hasNext()) {
		GpsSatellite satellite = sat.next();
		strGpsStats += satellite.getPrn() + ":" +
		    satellite.getSnr() + ":" +
		    satellite.getAzimuth() + ":" +
		    satellite.getElevation()+ "|";
	    }	    
	    satInfo = strGpsStats;
	}
    }

    public class CellLocationListener extends PhoneStateListener {

	public TelephonyManager telephonyManager = null;
	public MyCamera parent = null;
	@Override
	public void onCellLocationChanged(CellLocation location) {
	    super.onCellLocationChanged(location);
	    GsmCellLocation cellLocation = (GsmCellLocation)telephonyManager.getCellLocation(); 
	    String networkOperator = telephonyManager.getNetworkOperator();
	    if (networkOperator.length() > 0) {
		String mcc = networkOperator.substring(0, 3);
		String mnc = networkOperator.substring(3);
		int cid = cellLocation.getCid();
		int lac = cellLocation.getLac();
		System.out.println("gsm location area code:"+String.valueOf(lac) );
		System.out.println("cid "+String.valueOf(cid) );
		System.out.println("mcc:"+mcc );
		System.out.println("mnc:"+mnc );
		parent.cellInfo = "" +
		    mcc + ":" +
		    mnc + ":" +
		    String.valueOf(lac) + ":" +
		    String.valueOf(cid) + ":";
	    }
	}
    }    
   
    public static class PrisLocationListener implements LocationListener{

	private static final String TAG = PrisLocationListener.class.getSimpleName();
	public MyCamera parent = null;

	@Override
	public void onLocationChanged(Location arg0) {
	    double lat = arg0.getLatitude();
	    double lon = arg0.getLongitude();
	    double bearing = arg0.getBearing();
	    double speed = arg0.getSpeed();
	    double acc = arg0.getAccuracy();
	    double alt = arg0.getAltitude();	    
	    String s =
		Util.round(lat) + "," + Util.round(lon) + "," + Util.round(bearing) + "," +
		Util.round(speed) + "," + Util.round(acc) + "," + Util.round(alt) ;
	    parent.gpsInfo = s;
	    String s2 =
		lat + "," + lon + "," + bearing + "," + speed + "," + acc  ;
	    parent.gpsInfo2 = s2;
	    
	    parent.latitude = arg0.getLatitude();
	    parent.longitude = arg0.getLongitude();
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
    
    public void record(View view) {
	camPreview.writeToDisk();
	AlertDialog.Builder builder=new AlertDialog.Builder(this);
	builder
	    .setTitle("Msg")
	    .setMessage("Done")
	    .setPositiveButton("OK", null)
	    .show();
    }
    
    public void viewImage(View view) {
	try {

	    Intent intent = new Intent(this, ViewImage.class);
	    Log.d("cam",""+latitude);
	    Log.d("cam",""+longitude);
	    intent.putExtra("latitude", ""+latitude);
	    intent.putExtra("longitude", ""+longitude);
	    intent.putExtra("city", slCities.text);
	    intent.putExtra("trip", slTrips.text);
	    Log.d("cam","intent "+slCities.text);
	    startActivity(intent);
	} catch (Exception e) {
	    Log.e("cam", "viewimage error");
	    e.printStackTrace();
	}
    }

    @Override
    public void onBackPressed() {
	android.os.Process.killProcess(android.os.Process.myPid());
	System.exit(0);
    }

    public class SelectListener implements AdapterView.OnItemSelectedListener {

	public String text = "";

	public String [] items = null;
	
	public SelectListener(String [] s) {
	    items = s;
	    text = items[0];	    
	}
	
	public void onItemSelected(AdapterView<?> parent, View v, int position, long id) {
	    Log.d("cam", ""+items[position]);
	    text = items[position];
	}

	public void onNothingSelected(AdapterView<?> parent) {
	    Log.d("cam", "no selection");
	}
    }
}
