# Android, Kamera, Yön, GPS Verisini Kaydetmek

Test amaçlı olarak cep telefonunda kamera görüntüsü (video olarak),
hangi yöne dönük olduğumüz, ve varsa GPS verisini telefon üzerinde
bazı dosyalara kaydedebiliriz (DCİM dizini). Bir kaydedici program
işleyince görüntü, GPS, vs. kaydedilir, program bitişinde dizine
yazılır.. Veri

```
ANDROID/platform-tools/adb -d pull /sdcard/DCIM/cam.txt /tmp
ANDROID/platform-tools/adb -d pull /sdcard/DCIM/orientations.txt /tmp
ANDROID/platform-tools/adb -d pull /sdcard/DCIM/gps.txt /tmp
ANDROID/platform-tools/adb -d pull /sdcard/DCIM/sizes.txt /tmp
```

ile dizustu ortamina indirilebilir. Kamera goruntusunu gormek icin Python ile

```
from PIL import Image
import pandas as pd
import io

data = np.fromfile("/home/burak/Downloads/cam.txt", dtype=np.uint8)
df = pd.read_csv("/home/burak/Downloads/sizes.txt",header=None)
df['cum'] = df.cumsum()
df['cum2'] = df.cum.shift(-1)
df.columns = ['x','fr','to']
frame = 3
arr = data[int(df.ix[frame]['fr']) : int(df.ix[frame]['to'])]
im = Image.open(io.BytesIO(arr))

im.save('out.png')
```

Ardi ardina tum resimler tek bir byte dizisi icinde koyuldu, dizinden
ayri ayri resimleri cekip cikartmak lazim. Bir problem su, JPG
formatinda her resim farkli boyutlarda olabilir, o yuzden hangi
byte'tan hangi byte'a kadar cekip cikarma yapacagimizi bilmemiz lazim,
yani resim buyukluklerini bilmemiz lazim, bu buyuklukleri ayri bir
dosyadan aliyoruz.

Kayit yapilirken kayit hangi aralikta yapilir? Bizim kullandigimiz
zaman birimi kamera tek goruntusudur. Android'de onizleme (preview)
mod'unda iken onPreviewFrame adli cagriya her kamera goruntusu teker
teker gecilir. Bu gecilme sirasinda (ve bizim tanimladigimiz bir
sabite gore bazi goruntuler atlanarak) o anda elde bulunan diger tum
algilayici verilerini de kamera goruntusu ile birlikte yaziyoruz. Yani
kayit sonunda elde bir ufak video'nun 40 tek imaji (frame) var ise, o
zaman yon ve GPS icin de 40 tane veri noktasi olacak. 

GPS verisi her zaman mevcut olmayabilir, olmadigi zaman onun yerine
null yazilir. GPS bilindigi gibi her zaman uydu baglantisi ve
hesaplarini kitleyemiyor. Odada, kapali bir yerde olundugu zaman GPS
islemez. 

Alttaki kod daha once paylastigimiz altyapi icine atilabilir, derleme orada halledilir.

```
package my.project.MyCamera;

import java.io.FileOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.io.File;
import android.view.SurfaceHolder;
import android.os.Environment;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.YuvImage;
import android.graphics.Rect;
import android.graphics.ImageFormat;
import android.graphics.BitmapFactory;
import android.hardware.Camera;
import android.hardware.Camera.Parameters;
import android.hardware.Camera.ShutterCallback;
import android.util.Log;
import java.util.ArrayList;

public class CameraPreview implements SurfaceHolder.Callback,
                                      Camera.PreviewCallback
{
    private Camera mCamera = null;
    private int PreviewSizeWidth;
    private int PreviewSizeHeight;
    private String NowPictureFileName;
    private Boolean TakePicture = false;
    private ArrayList<byte[]> images = null;
    private ArrayList<String> orientations = null;
    private ArrayList<String> gps = null;
    public MyCamera mCameraActivity = null;

    public CameraPreview(int PreviewlayoutWidth, int PreviewlayoutHeight)
    {
        PreviewSizeWidth = PreviewlayoutWidth;
        PreviewSizeHeight = PreviewlayoutHeight;
    }

    private int frameCounter = 0;

    @Override
    public void onPreviewFrame(byte[] arg0, Camera arg1)
    {
        frameCounter++;
        if (frameCounter < 10) return;
        frameCounter = 0;
        images.add(arg0);
        orientations.add(Float.toString(mCameraActivity.mOrientationAngles[0]) + " " +
                         Float.toString(mCameraActivity.mOrientationAngles[1]) + " " +
                         Float.toString(mCameraActivity.mOrientationAngles[2]));
        if (gps != null) gps.add(mCameraActivity.gpsInfo);

    }

    @Override
    public void surfaceChanged(SurfaceHolder arg0, int arg1, int arg2, int arg3)
    {
        Parameters parameters;

        parameters = mCamera.getParameters();
        // Set the camera preview size
        parameters.setPreviewSize(PreviewSizeWidth, PreviewSizeHeight);
        // Set the take picture size, you can set the large size of the camera supported.
        parameters.setPictureSize(PreviewSizeWidth, PreviewSizeHeight);
        parameters.setPreviewFormat(ImageFormat.NV21);

        mCamera.setParameters(parameters);

        images = new ArrayList();
        orientations = new ArrayList<String>();
        gps = new ArrayList<String>();

        mCamera.startPreview();
    }

    @Override
    public void surfaceCreated(SurfaceHolder arg0)
    {
        mCamera = Camera.open();
        try {
            // If did not set the SurfaceHolder, the preview area will be black.
            mCamera.setPreviewDisplay(arg0);
            mCamera.setPreviewCallback(this);
        }
        catch (IOException e){
            mCamera.release();
            mCamera = null;
        }
    }


    @Override
    public void surfaceDestroyed(SurfaceHolder arg0)
    {
        Log.d("cam","images size "+ Integer.toString(images.size()));

        mCamera.setPreviewCallback(null);
        mCamera.stopPreview();
        mCamera.release();
        mCamera = null;

        final File path = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DCIM );

        File fileo = new File(path, "orientations.txt");
        try
            {
                fileo.createNewFile();
                FileOutputStream fOut = new FileOutputStream(fileo);
                OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
                for (String xx : orientations) {
                    myOutWriter.append(xx);
                    myOutWriter.append("\n");
                }
                myOutWriter.close();
                fOut.flush();
                fOut.close();
            }
        catch (IOException e)
            {
                Log.e("Exception", "File write failed: " + e.toString());
            }

        File fileg = new File(path, "gps.txt");
        try
            {
                fileg.createNewFile();
                FileOutputStream fOut = new FileOutputStream(fileg);
                OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
                for (String xx : gps) {
                    myOutWriter.append(xx);
                    myOutWriter.append("\n");
                }
                myOutWriter.close();
                fOut.flush();
                fOut.close();
            }
        catch (IOException e)
            {
                Log.e("Exception", "File write failed: " + e.toString());
            }

        File filec = new File(path, "cam.txt");
        File files = new File(path, "sizes.txt");
        try
            {
                filec.createNewFile();
                files.createNewFile();
                FileOutputStream fOut = new FileOutputStream(filec);
                FileOutputStream fOut2 = new FileOutputStream(files);
                OutputStreamWriter myOut2Writer = new OutputStreamWriter(fOut2);
                for (byte[] xx : images) {
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    YuvImage yuv = new YuvImage(xx, ImageFormat.NV21, PreviewSizeWidth, PreviewSizeHeight, null);
                    yuv.compressToJpeg(new Rect(0, 0, PreviewSizeWidth, PreviewSizeHeight), 50, out);
                    byte[] bytes = out.toByteArray();
                    fOut.write(bytes);
                    myOut2Writer.append("" + bytes.length);
                    myOut2Writer.append("\n");
                    Log.d("cam","single jpg size "+ bytes.length);
                }
                myOut2Writer.close();
                fOut.flush();
                fOut.close();
                fOut2.flush();
                fOut2.close();
            }
        catch (IOException e)
            {
                Log.e("Exception", "File write failed: " + e.toString());
            }
    }
}

package my.project.MyCamera;

import java.io.File;

import android.app.Activity;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorManager;
import android.hardware.SensorEventListener;
import android.hardware.SensorEvent;
import android.location.GpsStatus;
import android.location.LocationManager;
import android.location.LocationListener;
import android.location.GpsSatellite;
import android.location.Location;
import android.util.Log;
import android.os.HandlerThread;

public class MyCamera extends Activity implements SensorEventListener
{
    private CameraPreview camPreview;
    private FrameLayout mainLayout;
    private int PreviewSizeWidth = 320;
    private int PreviewSizeHeight= 240;
    private Handler mHandler = new Handler(Looper.getMainLooper());

    private SensorManager mSensorManager;
    public float[] mAccelerometerReading = new float[3];
    public float[] mMagnetometerReading = new float[3];
    public float[] mRotationMatrix = new float[9];
    public float[] mOrientationAngles = new float[3];

    public String gpsInfo = null;
    LocationManager locationManager = null;

    private PrisLocationListener mLocationListener;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        mSensorManager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);
        mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),SensorManager.SENSOR_DELAY_GAME);
        mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD),SensorManager.SENSOR_DELAY_GAME);
        mSensorManager.registerListener(this,mSensorManager.getDefaultSensor(Sensor.TYPE_ORIENTATION),SensorManager.SENSOR_DELAY_GAME);

        PrisLocationListener locationListener = new PrisLocationListener();
        locationListener.parent = this;
        HandlerThread t = new HandlerThread("LocationHandlerThread");
        t.start();
        locationManager = (LocationManager)getSystemService(LOCATION_SERVICE);
        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, locationListener, t.getLooper());

        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                             WindowManager.LayoutParams.FLAG_FULLSCREEN);

        requestWindowFeature(Window.FEATURE_NO_TITLE);
        setContentView(R.layout.main);

        SurfaceView camView = new SurfaceView(this);
        SurfaceHolder camHolder = camView.getHolder();
        camPreview = new CameraPreview(PreviewSizeWidth, PreviewSizeHeight);
        camPreview.mCameraActivity = this;

        camHolder.addCallback(camPreview);
        camHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);

        mainLayout = (FrameLayout) findViewById(R.id.frameLayout1);
        mainLayout.addView(camView, new LayoutParams(PreviewSizeWidth, PreviewSizeHeight));
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
        else if (event.sensor.getType() == Sensor.TYPE_MAGNETIC_FIELD) {
            System.arraycopy(event.values, 0, mMagnetometerReading,
                             0, mMagnetometerReading.length);
        }
        else if (event.sensor.getType() == Sensor.TYPE_ORIENTATION) {
            System.arraycopy(event.values, 0, mOrientationAngles,
                             0, mOrientationAngles.length);
        }
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
    }

    public static class PrisLocationListener implements LocationListener{

        private static final String TAG = PrisLocationListener.class.getSimpleName();
        public MyCamera parent = null;

        @Override
        public void onLocationChanged(Location arg0) {
            String s = "" +
                arg0.getLatitude() + "," +
                arg0.getLongitude() + "," +
                arg0.getSpeed() + "," +
                arg0.getAccuracy() + "," +
                arg0.getAltitude() ;
            parent.gpsInfo = s;
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
}
```

