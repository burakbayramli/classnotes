package my.project.MyCamera;

import android.app.Application;
import java.util.*;
import android.speech.tts.TextToSpeech;

public class MyApp extends Application {
    
    public TextToSpeech t1;

    public ArrayList<String[]> trip = new ArrayList<String[]>();
    
    public class ConcurrentTask implements Runnable {
	
	public MyApp parent = null;
	
	public void run() {
	    while (true) {
		try {
		    Thread.sleep(10000);
		    // String toSpeak = "Woken up motherfucka ";
		    // toSpeak += "orientation " + (int)parent.or0;
		    // parent.t1.speak(toSpeak, TextToSpeech.QUEUE_FLUSH, null);

		} catch (InterruptedException e) {
		    System.out.println("error concur" );
		}
	    }
	}
    }
    
    public Thread runner = null;
        
    public void startTrip() {
	if (runner != null) return;
	ConcurrentTask task = new ConcurrentTask();
	task.parent = this;
	runner = new Thread(task);
	runner.start();
    }
       
    
}
