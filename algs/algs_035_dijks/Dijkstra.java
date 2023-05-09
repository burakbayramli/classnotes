import java.util.*;

public class Dijkstra {
    
    public static class Edge {
	public String key;
	public Double dist;
	public String toString() { return key + " " + dist; }
    }
       
    public static ArrayList<String> shortestPath(String start, String end, HashMap<String, ArrayList<Edge>> graph) throws Exception {

	HashMap<String, Double> D = new HashMap<String, Double>();
	HashMap<String, String> P = new HashMap<String, String>();
	PQ<String, Double> queue =  new PQ<String, Double>();
	queue.addItem(start, 0.0);

	while (queue.getSize() > 0) {
	    Double dist = queue.peekPriority();
	    String popped = queue.removeItem();
	    D.put(popped, dist);
	    if (popped.equals(end) == true) {
		break;
	    }		
	    for (Edge ee : graph.get(popped)) {
		Double vwLength = D.get(popped) + ee.dist;
		if (D.get(ee.key) != null) {
		}
		else if (queue.getPriority(ee.key) == null) {
		    queue.addItem(ee.key, vwLength);
		    P.put(ee.key, popped);
		}
		else if (vwLength < queue.getPriority(ee.key)) {		    
		    queue.changePriority(ee.key, vwLength);
		    P.put(ee.key, popped);
		}
	    }

	}

	ArrayList<String> path = new ArrayList<String>();
	String ee = end;
	while (true){
	    path.add(ee);
	    ee = P.get(ee);
	    if (ee.equals(start)) break;
	}

	Collections.reverse(path);
	return path;
    }
    
    public static void main(String[] args) {
	HashMap<String, ArrayList<Edge>> graph = new HashMap<String, ArrayList<Edge>>();
	ArrayList<Edge> elist = null;
	Edge e = null;
	
	elist = new ArrayList<Edge>();
	e = new Edge(); e.key = "t"; e.dist = 10.0;
	elist.add(e);
	e = new Edge(); e.key = "y"; e.dist = 5.0;
	elist.add(e);	
	graph.put("s", elist);

	elist = new ArrayList<Edge>();
	e = new Edge(); e.key = "x"; e.dist = 1.0;
	elist.add(e);
	e = new Edge(); e.key = "z"; e.dist = 2.0;
	elist.add(e);	
	graph.put("t", elist);
	
	elist = new ArrayList<Edge>();
	e = new Edge(); e.key = "z"; e.dist = 4.0;
	elist.add(e);
	graph.put("x", elist);
	
	elist = new ArrayList<Edge>();
	e = new Edge(); e.key = "t"; e.dist = 3.0;
	elist.add(e);
	e = new Edge(); e.key = "x"; e.dist = 9.0;
	elist.add(e);	
	e = new Edge(); e.key = "z"; e.dist = 2.0;
	elist.add(e);	
	graph.put("y", elist);
	
	elist = new ArrayList<Edge>();
	e = new Edge(); e.key = "s"; e.dist = 7.0;
	elist.add(e);	
	e = new Edge(); e.key = "x"; e.dist = 6.0;
	elist.add(e);	
	graph.put("z", elist);
	
	try {	    	    
	    ArrayList<String> res3 = Dijkstra.shortestPath("s","x",graph);
	    for (String s : res3) {
	    	System.out.println(""+s);
	    } 
	} catch (Exception eee) {
	    eee.printStackTrace();
	}
	
    }

}
