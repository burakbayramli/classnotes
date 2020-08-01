package my.project.MyCamera;

import android.util.Log;
import java.util.*;
import java.util.zip.*;
import java.util.regex.*;
import java.io.*;
import java.sql.*;
import java.net.*;

public class Util {

    public static String base_uri = "/storage/emulated/0/Bass/";

    public static String base_file(String city) { return city + "_map_"; }

    private static StringBuffer encodeSignedNumber(int num) {
        int sgn_num = num << 1;
        if (num < 0) {
            sgn_num = ~(sgn_num);
        }
        return(encodeNumber(sgn_num));
    }

    private static StringBuffer encodeNumber(int num) {
        StringBuffer encodeString = new StringBuffer();
        while (num >= 0x20) {
                int nextValue = (0x20 | (num & 0x1f)) + 63;
                encodeString.append((char)(nextValue));
            num >>= 5;
        }
        num += 63;
        encodeString.append((char)(num));
        return encodeString;
    }
    

    public static String encode(ArrayList<GeoPoint> polyline, int precision) {
        StringBuilder encodedPoints = new StringBuilder();
        int prev_lat = 0, prev_lng = 0;
	for (GeoPoint trackpoint:polyline) {
	    int lat = trackpoint.getLatitudeE6() / precision;
	    int lng = trackpoint.getLongitudeE6() / precision;
	    encodedPoints.append(encodeSignedNumber(lat - prev_lat));
	    encodedPoints.append(encodeSignedNumber(lng - prev_lng));			
	    prev_lat = lat;
	    prev_lng = lng;
	}
	return encodedPoints.toString();
    }
        
    public static class Geo {
	public double lat;
	public double lon;
    }

    public static double round(double d, double precision){
	double p = Math.pow(10,precision);
	return Math.round(d*p) / p;
    }
    
    public static String round(double d){
	String s = ""+d;
	if (s.length() > 4) return s.substring(0,5);
	else return s;
    }
    
    public static Connection connection() {
	try {
            DriverManager.registerDriver((Driver) Class.forName("org.sqldroid.SQLDroidDriver").newInstance());
        } catch (Exception e) {
            throw new RuntimeException("Failed to register SQLDroidDriver");
        }
        String jdbcUrl = "jdbc:sqldroid:" + Util.base_uri + "network.db";
        try {
            Connection conn = DriverManager.getConnection(jdbcUrl);
	    return conn;
        } catch (SQLException e) {
            return null;
        }
    }
        
    public static Geo latLonFromFileName(String s) {
	Pattern r = Pattern.compile("(-*\\d+)_(\\d+)_(-*\\d+)_(\\d+)");
	Matcher m = r.matcher(s);
	if (m.find( )) {
	    double lat = Double.parseDouble(m.group(1) + "." + m.group(2));
	    double lon = Double.parseDouble(m.group(3) + "." + m.group(4));
	    Geo g = new Geo();
	    g.lat = lat;
	    g.lon = lon;
	    return g;
	}else {
	    System.out.println("NO MATCH");
	    return null;
	}	
    }
    
    public static ArrayList<String[]> initMapFiles(String city) {
	ArrayList<String[]> lls = new ArrayList<String[]>();    
	try {	    
	    ZipFile  zipFile = new ZipFile (base_uri + city + ".zip");
	    Log.d("cam", "before initMap " + base_uri + city + ".zip");
	    Enumeration <?  extends   ZipEntry > entries = zipFile.entries();
	    while (entries.hasMoreElements()){
		ZipEntry  entry = entries.nextElement();
		Geo g = latLonFromFileName(entry.toString());
		String [] tmp = new String[2];
		tmp[0] = ""+g.lat; tmp[1] = ""+g.lon;
		lls.add(tmp);
	    }
	    zipFile.close();
	} catch (IOException e) {
	    Log.e("cam", "error initmapfiles");
	    e.printStackTrace();
	}

	Log.d("cam", "map file size "+lls.size());
	return lls;	
    }

    public static Integer[] getMapsClosestToGeoSorted(ArrayList<String[]> lls, double lat, double lon)
    { 
	// GEO adresine en yakin haritayi bul
	final double[] dists = new double[lls.size()];
	final Integer[] idxs = new Integer[lls.size()];
        for (int i=0;i<lls.size();i++){
	    idxs[i] = i; String[] ll = lls.get(i);
	    // Oklit mesafesini hesapla
	    dists[i]=Math.sqrt(Math.pow((lat-Float.parseFloat(ll[0])),2) +
			       Math.pow((lon-Float.parseFloat(ll[1])),2));
	}
	// Tum uzakliklari sirala, en yakin tabii ki en ustte olacak
	// Siralama teknigi degere gore ama id'ler uzerinden yapiliyor
	// (o sebeple altta id uzerinden degeri aliyoruz) ki boylece
	// dizindeki en yakin ID hangisi hemen anlayabiliyoruz.
	// Ayrica siralama kullanmanin bir faydasi ileride sag,sol,vs.
	// haritasini ararken en yakin haritalardan aramaya
	// baslayabilmek. "En yakin" dizinde sifirinci tabii ki.
	Arrays.sort(idxs, new Comparator<Integer>() {
		@Override public int compare(final Integer o1, final Integer o2) {
		    return Double.compare(dists[o1], dists[o2]);
		}
	    });
	return idxs;
    }
    
    public static String mapFile(ArrayList<String[]> lls, String city, double x, double y) {
	String uri = "";
	final double[] dists = new double[lls.size()];
	final Integer[] idxs = Util.getMapsClosestToGeoSorted(lls, x, y);
	Log.d("cam","base_file"+base_file(city));
	uri = base_file(city) +
	    lls.get(idxs[0])[0].replace(".","_") + "_" +
	    lls.get(idxs[0])[1].replace(".","_");
	uri = uri + ".png";
	Log.d("cam",""+uri);
	return uri;
    }
        
    public static byte[] getAllBytesFromStream(InputStream is) throws IOException {
        ByteArrayOutputStream bs = new ByteArrayOutputStream();
        byte[] buf = new byte[512];
        int iRead;
        int off;
        while (is.available() > 0) {
            iRead = is.read(buf, 0, buf.length);
            if (iRead > 0) bs.write(buf, 0, iRead);
        }
        return bs.toByteArray();
    }
    
    public static void writeTempImage(String uri, String city) {
	try {	    
	    ZipFile  zipFile =  new  ZipFile (Util.base_uri + city + ".zip");
	    ZipEntry e = zipFile.getEntry(uri)	;
            File f = new File(Util.base_uri + "out.png");
            InputStream is = zipFile.getInputStream(e);
            FileOutputStream fos = new FileOutputStream(f);
            byte[] rbuf = getAllBytesFromStream(is);
            fos.write(rbuf, 0, rbuf.length);
            is.close();
            fos.close();
	    zipFile.close();	    
	} catch (IOException e) {
	    Log.e("cam", "error writetemp");
	    e.printStackTrace();
	}
    }

    public static ArrayList<String []> readTrips(String filename) {
	ArrayList<String []> f = new ArrayList<String []>();
	try {	    
	    FileInputStream fstream = new FileInputStream(filename);
	    DataInputStream in = new DataInputStream(fstream);
	    BufferedReader br = new BufferedReader(new InputStreamReader(in));
	    String line = "";
	    while ((line = br.readLine()) != null) {
		f.add(line.split("\\|"));		
	    }
	    in.close();
	} catch (IOException e) {
	    Log.e("cam", "error readtrips");
	    e.printStackTrace();
	}	
	return f;
    }

    public static ArrayList<String []> readOsrmUrl(String urlString) {
	ArrayList<String []> res = new ArrayList<String []>();
	try {	    
	    URL url = new URL(urlString);
	    BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
	    String line = "";
	    while ((line = in.readLine()) != null) {
		String regex =
		    "bearing_after.*?:(\\d+),.*?" +
		    "\\[(-*\\d+\\.\\d+,-*\\d+\\.\\d+)\\].*?" + 
		    "distance.\\:(.*?),.*?" +
		    "name.\\:(.*?)," ;
		Pattern r = Pattern.compile(regex);
		Matcher m = r.matcher(line);
		while (m.find( )) {
		    String [] tmp = new String[5];
		    tmp[0] = "dummy";
		    tmp[1] = m.group(4); // name
		    tmp[2] = m.group(2); // coord
		    tmp[3] = m.group(1); // bearing
		    tmp[4] = m.group(3); // dist
		    for (String s : tmp) System.out.println(""+s );
		    res.add(tmp);
		}
	    }

	} catch (IOException e) {
	    e.printStackTrace();
	}
	return res;
    }
    
}
