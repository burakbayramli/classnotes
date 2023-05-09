
import java.util.*;

/**
 * A set implemented by a list, or equivalently, a list with no
 * duplicate elements.  This class provides a simple implementation of
 * a set whose elements can be placed in an arbitrary order.  (In
 * contrast, the elements of a {@link java.util.SortedSet SortedSet}
 * are ordered based upon intrinsic properties of the objects.)  This
 * class is implemented in terms of a backing list, and it will
 * support all of the methods supported by that list.
 *
 * <p>The notion of equality is different for sets and lists, and this
 * class adheres to the contract of {@link java.util.Set Set} rather
 * than {@link java.util.Set List}.  In particular, this implies that
 * a <code>List</code> and a <code>ListSet</code> formed from it are
 * <i>not</i> equal.  Such comparisons can be made correctly by
 * accessing the backing list via {@link ListSet#asList() asList}.</p>
 *
 * @author Mark A. Paskin
 * @version $Revision: 1.3 $ ($Date: 2002/12/28 21:58:35 $)
 */
public class ListSet extends AbstractSet implements List {

    /**
     * The underlying list.
     */
    protected List list;

    /**
     * Constructor.  The backing list is a {@link java.util.LinkedList
     * LinkedList}.
     */
    public ListSet() {
	list = new LinkedList();
    }

    /**
     * Constructor.  The backing list is a {@link java.util.LinkedList
     * LinkedList}, and the set is initialized to contain the single
     * supplied object.
     */
    public ListSet(Object obj) {
	this(Collections.singleton(obj));
    }

    /**
     * Constructor.  The backing list is a {@link java.util.LinkedList
     * LinkedList}, and the set is initialized to contain the single
     * supplied object, and the set is initialized to contain the
     * elements of the supplied array (in order).  Only the first
     * occurrence of each element is retained.  
     */
    public ListSet(Object[] objs) {
	this(java.util.Arrays.asList(objs));
    }

    /**
     * Constructor.  The backing list is a {@link java.util.LinkedList
     * LinkedList}, and the set is initialized to contain the elements
     * of the supplied collection (in their iteration order).  Only the
     * first occurrence of each element is retained.
     */
    public ListSet(Collection c) {
	list = new LinkedList();
	addAll(c);
    }

    public Spliterator spliterator() {
        return List.super.spliterator();
    }    
    
    /**
     * Returns an unmodifiable view of this set as a list.
     */
    public List asList() {
	return Collections.unmodifiableList(list);
    }

    /**
     * Returns the number of elements in this set.  If this set contains
     * more than <tt>Integer.MAX_VALUE</tt> elements, returns
     * <tt>Integer.MAX_VALUE</tt>.
     *
     * @return the number of elements in this set.
     */
    public int size() {
	return list.size();
    }

    /**
     * Returns <tt>true</tt> if this set contains no elements.
     *
     * @return <tt>true</tt> if this set contains no elements.
     */
    public boolean isEmpty() {
	return list.isEmpty();
    }

    /**
     * Returns <tt>true</tt> if this set contains the specified element.
     * More formally, returns <tt>true</tt> if and only if this set contains
     * at least one element <tt>e</tt> such that
     * <tt>(o==null&nbsp;?&nbsp;e==null&nbsp;:&nbsp;o.equals(e))</tt>.
     *
     * @param o element whose presence in this set is to be tested.
     * @return <tt>true</tt> if this set contains the specified element.
     */
    public boolean contains(Object o) {
	return list.contains(o);
    }

    /**
     * Returns an iterator over the elements in this set in proper sequence.
     *
     * @return an iterator over the elements in this set in proper sequence.
     */
    public Iterator iterator() {
	return list.iterator();
    }

    /**
     * Returns an array containing all of the elements in this set in proper
     * sequence.  Obeys the general contract of the
     * <tt>Collection.toArray</tt> method.
     *
     * @return an array containing all of the elements in this set in proper
     *	       sequence.
     */
    public Object[] toArray() {
	return list.toArray();
    }

    /**
     * Returns an array containing all of the elements in this set in proper
     * sequence; the runtime type of the returned array is that of the
     * specified array.  Obeys the general contract of the
     * <tt>Collection.toArray(Object[])</tt> method.
     *
     * @param a the array into which the elements of this set are to
     *		be stored, if it is big enough; otherwise, a new array of the
     * 		same runtime type is allocated for this purpose.
     * @return  an array containing the elements of this set.
     * 
     * @throws ArrayStoreException if the runtime type of the specified array
     * 		  is not a supertype of the runtime type of every element in
     * 		  this set.
     */
    public Object[] toArray(Object a[]) {
	return list.toArray(a);
    }

    /**
     * Appends the specified element to the end of this set if it is not
     * already in the set (optional operation).
     *
     * @param o element to be appended to this set.
     * @return <code>true</code> if the element was added
     * 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is
     *         not supported by the backing list.
     * 
     * @throws ClassCastException if the class of an element in the specified
     * 	       collection prevents it from being added to the backing list.
     * 
     * @throws IllegalArgumentException if some aspect of an element in the
     *         specified collection prevents it from being added to this
     *         backing list.
     */
    public boolean add(Object o) {
	if (!contains(o)) {
	    list.add(o);
	    return true;
	} else
	    return false;
    }

    /**
     * Removes the the specified element from this set (optional
     * operation).  If this set does not contain the element, it is
     * unchanged.  More formally, removes the element with the lowest
     * index i such that <tt>(o==null ? get(i)==null :
     * o.equals(get(i)))</tt> (if such an element exists).
     *
     * @param o element to be removed from this set, if present.
     * @return <tt>true</tt> if this set contained the specified element.
     *
     * @throws UnsupportedOperationException if the <tt>remove</tt> method is
     *         not supported by the backing list.
     * 
     */
    public boolean remove(Object o) {
	return list.remove(o);
    }

    /**
     * Returns <tt>true</tt> if this set contains all of the elements of the
     * specified collection.
     *
     * @param c collection to be checked for containment in this set.
     * @return <tt>true</tt> if this set contains all of the elements of the
     * 	       specified collection.
     * 
     * @see #contains(Object)
     */
    public boolean containsAll(Collection c) {
	return list.containsAll(c);
    }

    /**
     * Returns <tt>true</tt> if this set contains any of the elements of the
     * specified collection.
     *
     * @param c collection to be checked for intersection in this set.
     * @return <tt>true</tt> if this set contains any of the elements of the
     * 	       specified collection.
     * 
     * @see #contains(Object)
     */
    public boolean containsAny(Collection c) {
	for (Iterator i = c.iterator(); i.hasNext();)
	    if (contains(i.next())) return true;
	return false;
    }

    /**
     * Creates an ordered set that is the complement of this set in the
     * supplied set of objects.  
     */
    public ListSet complement(Set other) {
	ListSet c = new ListSet();
	c.addAll(other);
	c.removeAll(this);
	return c;
    }

    /**
     * Appends all of the elements in the specified collection to the
     * end of this set, in the order that they are returned by the
     * specified collection's iterator.  The behavior of this operation
     * is unspecified if the specified collection is modified while the
     * operation is in progress.  (Note that this will occur if the
     * specified collection is this set, and it's nonempty.)
     *
     * @param c collection whose elements are to be added to this set.
     * @return <tt>true</tt> if this set changed as a result of the call.
     * 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is
     *         not supported by the backing list.
     * 
     * @throws ClassCastException if the class of an element in the specified
     * 	       collection prevents it from being added to the backing list.
     * 
     * @throws IllegalArgumentException if some aspect of an element in the
     *         specified collection prevents it from being added to the
     *         backing list.
     * 
     * @see #add(Object)
     */
    public boolean addAll(Collection c) {
	boolean changed = false;
	for (Iterator i = c.iterator(); i.hasNext();)
	    if (add(i.next()))
		changed = true;
	return changed;
    }

    /**
     * Inserts all of the elements in the specified collection into this
     * set at the specified position (optional operation).  Shifts the
     * element currently at that position (if any) and any subsequent
     * elements to the right (increases their indices).  The new elements
     * will appear in this set in the order that they are returned by the
     * specified collection's iterator.  The behavior of this operation is
     * unspecified if the specified collection is modified while the
     * operation is in progress.  (Note that this will occur if the specified
     * collection is this set, and it's nonempty.)
     *
     * @param index index at which to insert first element from the specified
     *	            collection.
     * @param c elements to be inserted into this set.
     * @return <tt>true</tt> if this set changed as a result of the call.
     * 
     * @throws UnsupportedOperationException if the <tt>addAll</tt> method is
     *		  not supported by the backing list.
     * @throws ClassCastException if the class of one of elements of the
     * 		  specified collection prevents it from being added to the
     * 		  backing list.
     * @throws IllegalArgumentException if some aspect of one of elements of
     *		  the specified collection prevents it from being added to
     *		  the backing list.
     * @throws IndexOutOfBoundsException if the index is out of range (index
     *		  &lt; 0 || index &gt; size()).
     */
    public boolean addAll(int index, Collection c) {
	boolean changed = false;
	for (Iterator i = c.iterator(); i.hasNext();) {
	    Object item = i.next();
	    if (!contains(item)) {
		add(index++, item);
		changed = true;
	    }
	}
	return changed;
    }

    /**
     * Removes from this set all the elements that are contained in the
     * specified collection (optional operation).
     *
     * @param c collection that defines which elements will be removed from
     *          this set.
     * @return <tt>true</tt> if this set changed as a result of the call.
     * 
     * @throws UnsupportedOperationException if the <tt>removeAll</tt> method
     * 		  is not supported by the backing list.
     * 
     * @see #remove(Object)
     * @see #contains(Object)
     */
    public boolean removeAll(Collection c) {
	return list.removeAll(c);
    }

    /**
     * Retains only the elements in this set that are contained in the
     * specified collection (optional operation).  In other words, removes
     * from this set all the elements that are not contained in the specified
     * collection.
     *
     * @param c collection that defines which elements this set will retain.
     * 
     * @return <tt>true</tt> if this set changed as a result of the call.
     * 
     * @throws UnsupportedOperationException if the <tt>retainAll</tt> method
     * 		  is not supported by the backing list.
     * 
     * @see #remove(Object)
     * @see #contains(Object)
     */
    public boolean retainAll(Collection c) {
	return list.retainAll(c);
    }

    /**
     * Removes all of the elements from this set (optional operation).  This
     * set will be empty after this call returns (unless it throws an
     * exception).
     *
     * @throws UnsupportedOperationException if the <tt>clear</tt> method is
     * 		  not supported by the backing list.
     */
    public void clear() {
	list.clear();
    }

    /**
     * Returns the element at the specified position in this set.
     *
     * @param index index of element to return.
     * @return the element at the specified position in this set.
     * 
     * @throws IndexOutOfBoundsException if the index is out of range (index
     * 		  &lt; 0 || index &gt;= size()).
     */
    public Object get(int index) {
	return list.get(index);
    }

    /**
     * Replaces the element at the specified position in this set with the
     * specified element (optional operation).
     *
     * @param index index of element to replace.
     * @param element element to be stored at the specified position; this
     *                element must not be currently in the set.
     * @return the element previously at the specified position.
     * 
     * @throws UnsupportedOperationException if the <tt>set</tt> method is not
     *		  supported by the backing list.
     * @throws    ClassCastException if the class of the specified element
     * 		  prevents it from being added to the backing list.
     * @throws    IllegalArgumentException if the supplied element is already 
     *               in the set
     * @throws    IndexOutOfBoundsException if the index is out of range
     *		  (index &lt; 0 || index &gt;= size()).  */
    public Object set(int index, Object element) {
	if (contains(element))
	    throw new IllegalArgumentException("Already contains " + element);
	return list.set(index, element);
    }

    /**
     * Inserts the specified element at the specified position in this set
     * (optional operation).  Shifts the element currently at that position
     * (if any) and any subsequent elements to the right (adds one to their
     * indices).
     *
     * @param index index at which the specified element is to be inserted.
     * @param element element to be inserted.
     * 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is not
     *		  supported by the backing list.
     * @throws    ClassCastException if the class of the specified element
     * 		  prevents it from being added to the backing list.
     * @throws    IllegalArgumentException if the supplied element is already
     *               in this set
     * @throws    IndexOutOfBoundsException if the index is out of range
     *		  (index &lt; 0 || index &gt; size()).
     */
    public void add(int index, Object element) {
	if (contains(element))
	    throw new IllegalArgumentException("Already contains " + element);
	else
	    list.add(index, element);
    }

    /**
     * Removes the element at the specified position in this set (optional
     * operation).  Shifts any subsequent elements to the left (subtracts one
     * from their indices).  Returns the element that was removed from the
     * set.
     *
     * @param index the index of the element to removed.
     * @return the element previously at the specified position.
     * 
     * @throws UnsupportedOperationException if the <tt>remove</tt> method is
     *		  not supported by the backing list.
     * 
     * @throws IndexOutOfBoundsException if the index is out of range (index
     *            &lt; 0 || index &gt;= size()).
     */
    public Object remove(int index) {
	return list.remove(index);
    }

    /**
     * Returns the index in this set of the specified element, or -1 if
     * this set does not contain this element.
     *
     * @param o element to search for.
     * @return the index in this set of the specified
     * 	       element, or -1 if this set does not contain this element.
     */
    public int indexOf(Object o) {
	return list.indexOf(o);
    }

    /**
     * Returns the index in this set of the specified element, or -1 if
     * this set does not contain this element.
     *
     * @param o element to search for.
     * @return the index in this set of the specified
     * 	       element, or -1 if this set does not contain this element.
     */
    public int lastIndexOf(Object o) {
	return list.lastIndexOf(o);
    }

    /**
     * Returns a list iterator of the elements in this set (in proper
     * sequence).
     *
     * @return a list iterator of the elements in this set (in proper
     * 	       sequence).
     */
    public ListIterator listIterator() {
	return new ListSetIterator(this);
    }

    /**
     * Returns a list iterator of the elements in this list (in proper
     * sequence), starting at the specified position in this set.  The
     * specified index indicates the first element that would be returned by
     * an initial call to the <tt>next</tt> method.  An initial call to
     * the <tt>previous</tt> method would return the element with the
     * specified index minus one.
     *
     * @param index index of first element to be returned from the
     *		    list iterator (by a call to the <tt>next</tt> method).
     * @return a list iterator of the elements in this list (in proper
     * 	       sequence), starting at the specified position in this list.
     * @throws IndexOutOfBoundsException if the index is out of range (index
     *         &lt; 0 || index &gt; size()).
     */
    public ListIterator listIterator(int index) {
	return new ListSetIterator(this, index);
    }

    /**
     * Returns a view of the portion of this set between the specified
     * <tt>fromIndex</tt>, inclusive, and <tt>toIndex</tt>, exclusive.
     * (If <tt>fromIndex</tt> and <tt>toIndex</tt> are equal, the
     * returned set is empty.)  The returned set is backed by this set,
     * so changes in the returned set are reflected in this set, and
     * vice-versa.  The returned set supports all of the optional
     * operations supported by this list.<p>
     *
     * @param fromIndex low endpoint (inclusive) of the subList.
     * @param toIndex high endpoint (exclusive) of the subList.
     * @return a view of the specified range within this list.
     * 
     * @throws IndexOutOfBoundsException for an illegal endpoint index value
     *     (fromIndex &lt; 0 || toIndex &gt; size || fromIndex &gt; toIndex).
     */
    public List subList(int fromIndex, int toIndex) {
	ListSet l = new ListSet();
	l.list = list.subList(fromIndex, toIndex);
	return l;
    }
}

/**
 * This class wraps a list iterator to prevent additions that would
 * introduce duplicates into the set.
 */
class ListSetIterator implements ListIterator {

    protected ListIterator li;

    protected ListSet s;

    public ListSetIterator(ListSet s) {
	this.s = s;
	li = s.list.listIterator();
    }

    public ListSetIterator(ListSet s, int index) {
	this.s = s;
	li = s.list.listIterator(index);
    }

    public boolean hasNext() { return li.hasNext(); }

    public Object next() { return li.next(); }

    public boolean hasPrevious() { return li.hasPrevious(); }

    public Object previous() { return li.previous(); }

    public int nextIndex() { return li.nextIndex(); }

    public int previousIndex() { return li.previousIndex(); }

    public void remove() { li.remove(); }

    /**
     * Replaces the last element returned by <tt>next</tt> or
     * <tt>previous</tt> with the specified element (optional operation).
     * This call can be made only if neither <tt>ListIterator.remove</tt> nor
     * <tt>ListIterator.add</tt> have been called after the last call to
     * <tt>next</tt> or <tt>previous</tt>.
     *
     * @param o the element with which to replace the last element returned by
     *          <tt>next</tt> or <tt>previous</tt>.
     * @exception UnsupportedOperationException if the <tt>set</tt> operation
     * 		  is not supported by this list iterator.
     * @exception ClassCastException if the class of the specified element
     * 		  prevents it from being added to this set.
     * @exception IllegalArgumentException if some aspect of the specified
     *		  element prevents it from being added to this set or the
     *               element is already in the set
     * @exception IllegalStateException if neither <tt>next</tt> nor
     *	          <tt>previous</tt> have been called, or <tt>remove</tt> or
     *		  <tt>add</tt> have been called after the last call to
     * 		  <tt>next</tt> or <tt>previous</tt>.
     */
    public void set(Object o) {
	if (s.contains(o))
	    throw new IllegalArgumentException("Already contains " + o);
	else
	    li.set(o);
    }

    /**
     * Inserts the specified element into the set (optional operation).  The
     * element is inserted immediately before the next element that would be
     * returned by <tt>next</tt>, if any, and after the next element that
     * would be returned by <tt>previous</tt>, if any.  (If the set contains
     * no elements, the new element becomes the sole element on the set.)
     * The new element is inserted before the implicit cursor: a subsequent
     * call to <tt>next</tt> would be unaffected, and a subsequent call to
     * <tt>previous</tt> would return the new element.  (This call increases
     * by one the value that would be returned by a call to <tt>nextIndex</tt>
     * or <tt>previousIndex</tt>.)
     *
     * @param o the element to insert.
     * @exception UnsupportedOperationException if the <tt>add</tt> method is
     * 		  not supported by this list iterator.
     * 
     * @exception ClassCastException if the class of the specified element
     * 		  prevents it from being added to this Set.
     * 
     * @exception IllegalArgumentException if some aspect of this element
     *            prevents it from being added to this Collection or the
     *               element is already in the set
     */
    public void add(Object o) {
	if (s.contains(o))
	    throw new IllegalArgumentException("Already contains " + o);
	else
	    li.add(o);
    }

}
