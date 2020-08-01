package my.project.MyCamera;

import android.app.*;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.graphics.drawable.*;
import android.graphics.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.zip.*;
import android.view.Display;


public class ViewImage extends Activity {

    public ArrayList<String[]> lls = null;

    MyApp appState = null;

    public static String city;

    public double lastLatitude = 0;
    public double lastLongitude = 0;
    public double lastMapCenterLat;
    public double lastMapCenterLon;

    public static Drawable plotTrip(Drawable res2, double imageCenterLat, double imageCenterLon, ArrayList<String[]> tripArg) {
	Drawable res3 = res2;
	try {
	    for (String [] t : tripArg) {
		String[] tokens = t[2].split(",");
		double tlat = Double.parseDouble(tokens[1].replaceAll("\\s",""));
		double tlon = Double.parseDouble(tokens[0].replaceAll("\\s",""));		
		System.out.println(""+tlat );
		System.out.println(""+tlon );
		res2 = mark(res3, tlat, tlon, imageCenterLat, imageCenterLon, Color.RED);
		res3 = res2;
	    }
	    
	} catch (Exception e) {
	    Log.d("cam", "Error viewimage");
	    res3 = res2;
	    e.printStackTrace();
	}
	return res3;
    }
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bundle extras = getIntent().getExtras();
	appState = ((MyApp)getApplicationContext());
	// default
	double lat = 0;
	double lon = 0;
        String uri = "";
        if (extras != null) {
            lat = Double.parseDouble(extras.getString("latitude"));
            lon = Double.parseDouble(extras.getString("longitude"));
            city = extras.getString("city");
	    if (lat==0 && lon==0) {
		//lat = 52.511736; // test vals pdamer
		//lon = 13.375345; // test vals
		lat = 52.543374670025102; // test vals
		lon = 13.417444774102901; // test vals
	    }
	    String tripId = extras.getString("trip");
            Log.d("cam","trip>>"+tripId+"<<");
    	    if (tripId.length() > 0) {
		Log.d("cam","trip id found");
		ArrayList<String []> trip = new ArrayList<String []>();
		ArrayList<String []> allTrips = Util.readTrips(Util.base_uri + "trip.csv");
		for (String [] t : allTrips ) {
		    if (t[0].equals(tripId)) trip.add(t);
		}
		appState.trip = trip;
	    }
	    
        }
	lls = Util.initMapFiles(city);
	uri = Util.mapFile(lls, city, lat, lon);

	Util.Geo imageCenterGeo = Util.latLonFromFileName(uri);
	lastMapCenterLat = imageCenterGeo.lat;
	lastMapCenterLon = imageCenterGeo.lon;

        TouchImageView image = new TouchImageView(this);
	image.parent = this;	
	Util.writeTempImage(uri, city);
	Drawable res2 = new BitmapDrawable(getResources(), Util.base_uri + "/out.png");
	res2 = mark(res2, lat, lon, imageCenterGeo.lat, imageCenterGeo.lon, Color.RED);
	Drawable res3 = plotTrip(res2, imageCenterGeo.lat, imageCenterGeo.lon, appState.trip);
	lastLatitude = lat;
	lastLongitude = lon;
	
        image.setImageDrawable(res3);

        LinearLayout ll = new LinearLayout(this);
        ll.addView(image);
        setContentView(ll);
    }

    public static Drawable markPixel(Drawable d, int x, int y, int to)
    {
	Bitmap src = ((BitmapDrawable) d).getBitmap();	
	Bitmap bitmap = src.copy(Bitmap.Config.ARGB_8888, true);
	bitmap.setPixel(x, y, to);
	bitmap.setPixel(x, y+1, to);bitmap.setPixel(x+1, y, to);
	bitmap.setPixel(x-1, y, to);bitmap.setPixel(x, y-1, to);
	bitmap.setPixel(x-1, y+1, to);bitmap.setPixel(x+1, y-1, to);
	
	return new BitmapDrawable(bitmap);
    }    

    public static double SCALEX = 23000;
    public static double SCALEY = -35000;
    
    public static Drawable mark(Drawable d, double lat, double lon, double latcen, double loncen, int to)
    {
	Bitmap src = ((BitmapDrawable) d).getBitmap();
	
	int x=100;int y=100;
	if (lat > 0) {
	    double dy = (lat-latcen)*ViewImage.SCALEY;
	    double dx = (lon-loncen)*ViewImage.SCALEX;
	    x = (int)(src.getWidth()/2 + dx);
	    y = (int)(src.getHeight()/2 + dy);
	}
	if (x <= 0 || y <= 0) return d;
	Bitmap bitmap = src.copy(Bitmap.Config.ARGB_8888, true);
	bitmap.setPixel(x, y, to);
	bitmap.setPixel(x, y+1, to);bitmap.setPixel(x+1, y, to);
	bitmap.setPixel(x-1, y, to);bitmap.setPixel(x, y-1, to);
	bitmap.setPixel(x-1, y+1, to);bitmap.setPixel(x+1, y-1, to);
	
	return new BitmapDrawable(bitmap);
    }    
    
}
