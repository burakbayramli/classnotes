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
import java.util.Calendar;

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
    private ArrayList<String> rotationMatrices = null;
    private ArrayList<String> accelerations = null;
    private ArrayList<String> laccelerations = null;
    private ArrayList<String> gps = null;
    private ArrayList<String> cell = null;
    private ArrayList<String> sat = null;
    public MyCamera mCameraActivity = null;
    public short[] buffer = new short[10000];
	
    public CameraPreview(int PreviewlayoutWidth, int PreviewlayoutHeight)
    {
	PreviewSizeWidth = PreviewlayoutWidth;
    	PreviewSizeHeight = PreviewlayoutHeight;
    }
    
    private int frameCounter = 0;
    
    @Override
    public void onPreviewFrame(byte[] arg0, Camera arg1) 
    {
	// onizleme sirasinda her kamera goruntusu (tek imaj olarak) bu
	// metota gecilir.

	// bu fonksiyone kac gere gelindigini say, ki bazi goruntuleri
	// almadan atabilelim, saniye basina daha fazla goruntu toplamak
	// icin alttaki sayi kucultulebilir.
	frameCounter++;
	//if (frameCounter < 10) return;
	if (frameCounter < 3) return;
	frameCounter = 0;	
	// hafizaya ekle
	images.add(arg0);
	String time = "" + Calendar.getInstance().getTimeInMillis();
	orientations.add(time + " " +
			 Float.toString(mCameraActivity.mOrientationAngles[0]) + " " +
			 Float.toString(mCameraActivity.mOrientationAngles[1]) + " " +
			 Float.toString(mCameraActivity.mOrientationAngles[2]));	
	rotationMatrices.add(time + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[0]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[1]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[2]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[3]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[4]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[5]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[6]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[7]) + " " +
			     Float.toString(mCameraActivity.mRotationMatrix[8])
			     );
	accelerations.add(time + " " +
			  Float.toString(mCameraActivity.mAccelerometerReading[0]) + " " +
			  Float.toString(mCameraActivity.mAccelerometerReading[1]) + " " +
			  Float.toString(mCameraActivity.mAccelerometerReading[2]));	
	laccelerations.add(time + " " +
			   Float.toString(mCameraActivity.mLAccelerometerReading[0]) + " " +
			   Float.toString(mCameraActivity.mLAccelerometerReading[1]) + " " +
			   Float.toString(mCameraActivity.mLAccelerometerReading[2]));	
	if (gps != null) gps.add(mCameraActivity.gpsInfo2);
	if (sat != null) sat.add(mCameraActivity.satInfo);
	if (cell != null) cell.add(mCameraActivity.cellInfo);
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
	rotationMatrices = new ArrayList<String>();
	accelerations = new ArrayList<String>();
	laccelerations = new ArrayList<String>();
	gps = new ArrayList<String>();
	sat = new ArrayList<String>();
	cell = new ArrayList<String>();
	
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
	    //recorder.stop();
	    //recorder.release();
	}
    }

    
    @Override
    public void surfaceDestroyed(SurfaceHolder arg0) 
    {
	// program kapatilinca hafizada ne varsa diske yazilir.
    	mCamera.setPreviewCallback(null);
	mCamera.stopPreview();
	mCamera.release();
	mCamera = null;	
	//recorder.stop();
	//recorder.release();
    }

    public void writeToDisk() {
	Log.d("cam","images size "+ Integer.toString(images.size()));

	String outDir = Util.base_uri + "/" + Calendar.getInstance().getTimeInMillis();
	File tmp = new File(outDir);
	tmp.mkdir();
			    
	final File path = new File(outDir);
	Log.d("cam","path "+ path);

	int L = orientations.size();
		
	File fileo = new File(path, "orientations.txt");
	try {
	    fileo.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(fileo);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = orientations.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filer = new File(path, "rotations.txt");
	try {
	    filer.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filer);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = rotationMatrices.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filea = new File(path, "acc.txt");
	try {
	    filea.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filea);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = accelerations.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	} catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filela = new File(path, "lacc.txt");
	try {
	    filea.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filela);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = laccelerations.get(i);
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
	    for (int i=0; i<L; i++) {
		String xx = gps.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	}
	catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filesc = new File(path, "cell.txt");
	try {
	    filesc.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filesc);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = cell.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	}
	catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	} 

	File filess = new File(path, "satellites.txt");
	try {
	    filess.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filess);
	    OutputStreamWriter myOutWriter = new OutputStreamWriter(fOut);
	    for (int i=0; i<L; i++) {
		String xx = sat.get(i);
		myOutWriter.append(xx);
		myOutWriter.append("\n");
	    }
	    myOutWriter.close();
	    fOut.flush();
	    fOut.close();
	}
	catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	}

	File filec = new File(path, "cam.bin");
	File files = new File(path, "sizes.txt");
	try {
	    filec.createNewFile();
	    files.createNewFile();
	    FileOutputStream fOut = new FileOutputStream(filec);
	    FileOutputStream fOut2 = new FileOutputStream(files);
	    OutputStreamWriter myOut2Writer = new OutputStreamWriter(fOut2);
	    for (int i=0; i<L; i++) {
		byte[] xx = images.get(i);
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
	catch (IOException e) {
	    Log.e("Exception", "File write failed: " + e.toString());
	}	

	Log.d("cam","dump done");       
    }

    
}
