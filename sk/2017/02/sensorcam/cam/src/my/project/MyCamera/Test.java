package my.project.MyCamera;

import java.net.*;
import java.util.*;
import java.util.regex.*;
import java.io.*;
import java.text.*;

public class Test {

    
    public static void main(String[] args) {

	System.out.println(""+Calendar.getInstance().getTimeInMillis() );

	Calendar cal = Calendar.getInstance();
	SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmssSS");
	String strDate = sdf.format(cal.getTime());
	System.out.println(""+strDate );

    }
}
