import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * A priority queue.  When items are added, they are given priorities.
 * Only the item with lowest priority can be removed.  "Lowest" is determined
 * by the natural order of the values.  Priorities of
 * items on the queue can be changed.
 * 
 * http://www3.amherst.edu/~jrglenn92/301/F2014/Projects/P3-ShortestPaths/Files/PriorityQueue.java
 *
 * @param K the types of the keys in this priority queue
 * @param V the types of the values in this priority queue 
 *
 * @author Jim Glenn
 * @version 0.2 2014-11-15 updated for generics
 * @version 0.1 2/12/2003
 */

public class PQ<K, V extends Comparable<? super V>>
{
    /**
     * Maps items to entries in the itemTable array.
     */
    private Map<K, Integer> itemMap;

    /**
     * Keeps track of an item's location in the heap.
     * <CODE>itemTable[itemTable[itemMap.get(o)]</CODE>
     * (an abuse of [] notation) should be the heap element
     * corresponding to <CODE>o</CODE>.
     */
    private List<TableEntry<K>> itemTable;

    /**
     * The heap used to implement this priority queue.
     */
    private List<HeapEntry<V>> itemHeap;

    /**
     * The number of items in this priority queue.
     */
    private int size;

    /**
     * Information necessary to store on the heap: 
     * its priority and a pointer back into the <CODE>heapIndex</CODE>
     * array.
     */
    private class HeapEntry<V>
    {
	public HeapEntry(V pri, int index)
	{
	    priority = pri;
	    tableIndex = index;
	}

	public String toString()
	{
	    return "(" + priority + " " + tableIndex + ")";
	}

	private V priority;
	private int tableIndex;
    }

    /**
     * Information necessary to store in the table:
     * an item and its location in the heap.
     */
    private class TableEntry<K>
    {
	public TableEntry(K it, int index)
	{
	    item = it;
	    heapIndex = index;
	}

	public String toString()
	{
	    return "(" + item + ", " + heapIndex + ")";
	}

	private K item;
	private int heapIndex;
    }

    /**
     * Constructs an empty queue.
     */

    public PQ()
    {
	itemMap = new HashMap<K, Integer>();
	itemHeap = new ArrayList<HeapEntry<V>>();
	itemTable = new ArrayList<TableEntry<K>>();
	size = 0;
    }

    /**
     * Returns the size of this priority queue.
     *
     * @return the size of this queue
     */
    public int getSize()
    {
	return size;
    }

    /**
     * Adds an item to this queue.
     *
     * @param item the item to add
     * @param pri the new item's priority
     */
    public void addItem(K item, V pri)
    {
	int index = itemHeap.size();

	itemMap.put(item, index);
	itemHeap.add(new HeapEntry<V>(pri, index));
	itemTable.add(new TableEntry<K>(item, index));
	size++;

	reheapUp(index);
    }

    /**
     * Returns the lowest priority on this queue.
     *
     * @return the lowest priority
     */
    public V peekPriority()
    {
	HeapEntry<V> heapTop = itemHeap.get(0);
	return heapTop.priority;
    }

    /**
     * Returns the item with the lowest priority on this queue.
     *
     * @return the item with the lowest priority
     */
    public K peekTop()
    {
	HeapEntry<V> heapTop = itemHeap.get(0);
	return itemTable.get(heapTop.tableIndex).item;
    }

    /**
     * Removes the item with the lowest priority from this queue.
     *
     * @return the item with the lowest priority
     */
    public K removeItem()
    {
	HeapEntry<V> heapTop = itemHeap.get(0);
	K result = itemTable.get(heapTop.tableIndex).item;

	// move last elt to top of heap and remove old top
	swap(0, size - 1);
	itemHeap.remove(size - 1);

	// fix table
	TableEntry<K> temp = itemTable.get(size - 1);
	itemTable.remove(size - 1);
	if (heapTop.tableIndex != size - 1)
	    {
		itemTable.set(heapTop.tableIndex, temp);
		itemHeap.get(temp.heapIndex).tableIndex = heapTop.tableIndex;
	    }
	
	// fix hash
	itemMap.remove(result);
	itemMap.put(temp.item, heapTop.tableIndex);

	// restore heap order
	size--;
	reheapDown(0);

	return result;
    }

    /**
     * Restores the heap property upwards starting from the given index.
     *
     * @param index an index of a node in the heap
     */
    private void reheapUp(int index)
    {
	int parent = (index - 1) / 2;
	while (index > 0 && getPriority(index).compareTo(getPriority(parent)) < 0)
	    {
		swap(index, parent);
		index = parent;
		parent = (index - 1) / 2;
	    }
    }

    /**
     * Restores the heap order property downwards starting from the
     * given index.
     *
     * @param index an index of a node in the heap
     */
    private void reheapDown(int index)
    {
	int leftChild = index * 2 + 1;
	int rightChild = leftChild + 1;

	while ((leftChild < size
		&& getPriority(leftChild).compareTo(getPriority(index)) < 0)
	       || (rightChild < size
		   && getPriority(rightChild).compareTo(getPriority(index)) < 0))
	    {
		if (rightChild >= size || getPriority(leftChild).compareTo(getPriority(rightChild)) < 0)
		    {
			swap(index, leftChild);
			index = leftChild;
		    }
		else
		    {
			swap(index, rightChild);
			index = rightChild;
		    }

		leftChild = index * 2 + 1;
		rightChild = leftChild + 1;
	    }
    }

    /**
     * Swaps items at the given locations in the heap.
     *
     * @param i the index of one item to swap
     * @param j the index of the other item to swap
     */
    
    private void swap(int i, int j)
    {
	// swap entries in the heap
	HeapEntry<V> one = itemHeap.get(i);
	HeapEntry<V> two = itemHeap.get(j);
	itemHeap.set(i, two);
	itemHeap.set(j, one);

	// fix entries in the table
	itemTable.get(one.tableIndex).heapIndex = j;
	itemTable.get(two.tableIndex).heapIndex = i;
    }

    /**
     * Returns the priority of the given heap element.
     *
     * @param index the index in the heap to examine
     * @return the priority of the item there
     */
    public V getPriority(int index)
    {
	return itemHeap.get(index).priority;
    }

    /**
     * Returns the priority of the given item.
     *
     * @param item an item on this queue
     * @return the priority of that item
     */
    public V getPriority(K item)
    {
	if (itemMap.containsKey(item) == false) return null;
	int tableLoc = itemMap.get(item);
	int heapLoc = itemTable.get(tableLoc).heapIndex;
	return itemHeap.get(heapLoc).priority;
    }

    /**
     * Changes the priority of the given item on this priority queue.
     *
     * @param item the item to change the priority of
     * @param pri the new priority of that item
     */
    public void changePriority(K item, V pri)
    {
	int tableLoc = itemMap.get(item);
	int heapLoc = itemTable.get(tableLoc).heapIndex;
	V oldPriority = itemHeap.get(heapLoc).priority;
	itemHeap.get(heapLoc).priority = pri;
	if (pri.compareTo(oldPriority) < 0)
	    reheapUp(heapLoc);
	else
	    reheapDown(heapLoc);
    }

    /**
     * Changes the priority of the given item in this queue
     * if the new priority is less than the current priority.
     * If the item is not in this queue it is added.
     *
     * @param item an item in this queue
     * @param pri the new priority of that item
     * @return true iff the new priority was lower than the old of the item is new
     */
    public boolean decreasePriority(K item, V pri)
    {
	if (itemMap.containsKey(item))
	    {
		V oldPriority = getPriority(item);
		
		if (pri.compareTo(oldPriority) < 0)
		    {
			changePriority(item, pri);
			return true;
		    }
		else
		    {
			return false;
		    }
	    }
	else
	    {
		addItem(item, pri);
		return true;
	    }
    }

    public String toString()
    {
	return "table: " + itemTable + "\nheap: " + itemHeap;
    }

    public static void main (String [] args)  {
	PQ<String, Double> pq = new PQ<String, Double>();
	pq.addItem("adskasf", 1.0);
	pq.addItem("adskasf3", 10.0);
	pq.addItem("adskasf4", 4.0);
	pq.addItem("adskasf1", 0.5);

	String res = pq.removeItem();
	System.out.println(""+res );

	pq.changePriority("adskasf3", 0.1);
	System.out.println(""+pq.removeItem());

	System.out.println(""+pq.getPriority("adskasf4") );
	
	System.out.println(""+pq.getPriority("adskasfasdkfasdf") );
	
    }
    
}
    
