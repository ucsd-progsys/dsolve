public class ArrayList
{
    private /*: encap */ Object[] elementData;
    private int size;
    
    /*: public specvar init :: bool;
        public encap specvar content :: "(int * obj) set";
	public encap specvar csize :: int;
	public specvar msize :: int;
	
    */

    public ArrayList(int initialCapacity)
    /*: requires "~init & 0 < initialCapacity"
        modifies "this..init", "this..msize"
        ensures "init & content = {} & csize = 0 & msize = initialCapacity"
    */

    public void trimToSize()
    /*: requires "init"
        modifies msize
	ensures "msize = csize"
    */

    public void ensureCapacity(int minCapacity)
    /*: requires "init"
        modifies msize
        ensures "minCapacity <= msize & old msize <= msize"
    */

    public int size()
    /*: requires "init"
        ensures "result = csize"
    */

    public boolean isEmpty()
    /*: requires "init"
        ensures "result = (csize = 0)"
    */
    
    public boolean contains(Object elem)
    /*: requires "init"
        ensures "(result = (EX i. (i, elem) : content))";
    */

    public int indexOf(Object elem)
    /*: requires "init"
        ensures "-1 <= result & result < csize &
	         (result ~= -1 --> (result, elem) : content) &
		 (result ~= -1 --> ~(EX i. i < result & (i, elem) : content)) &
                 (result = -1 --> ~(EX i. (i, elem) : content))"
    */

    public int lastIndexOf(Object elem)
    /*: requires "init"
        ensures "-1 <= result & result < csize &
	         (result ~= -1 --> 
		  ((result, elem) : content) &
		  ~(EX i. result < i & (i, elem) : content)) &
                 (result = -1 --> ~(EX i. (i, elem) : content))"
    */

    public Object[] toArray()
    /*: requires "init"
        modifies arrayState
        ensures "(ALL i e. (i, e) : content --> result.[i] = e) &
	         (ALL i. 0 <= i & i < result..Array.length --> 
		  (i, result.[i]) : content) &
		 (ALL a i. a ~= result --> a.[i] = old (a.[i]))"
    */

    public Object get(int index)
    /*: requires "init & 0 <= index & index < csize"
        ensures "(index, result) : content"
     */

    public /*: encap */ Object set(int index, Object element)
    /*: requires "init & 0 <= index & index < csize"
        modifies content
	ensures "(index, result) : old content & 
	  content = (old content - {(index, result)}) Un {(index, element)}"
    */

    public /*: encap */ boolean add(Object o)
    /*: requires "init"
        modifies content, csize, msize
	ensures "(old csize, o) : content &
	         (ALL j. ((0 <= j & j < old csize) -->
		  ((ALL e. (j, e) : content --> (j, e) : old content) &
		   (ALL e. (j, e) : old content --> (j, e) : content)))) &
	         csize = old csize + 1 & result &
                 old msize <= msize & csize <= msize"
    */

    public /*: encap */ void add_at(int index, Object element)
    /*: requires "comment ''addAtPre'' (init & 0 <= index & index <= csize)"
        modifies content, csize, msize
	ensures "((index, element) : content) &
	         (ALL j e.
                  (0 <= j & j < index --> ((j, e) : content) = ((j, e) : old content)) &
		  (index < j & j < csize --> ((j, e) : content) = ((j - 1, e) : old content))) &
	         (csize = (old csize) + 1) &
		 (msize >= (old msize)) &
		 (csize <= msize)"
    */

    public Object remove_at(int index)
    /*: requires "init & 0 <= index & index < csize"
        modifies content, csize
	ensures "(ALL j e.
                  (0 <= j & j < index --> ((j, e) : content) = ((j, e) : old content)) &
		  (index <= j & j < csize --> ((j, e) : content) = ((j + 1, e) : old content))) &
		 ((index, result) : old content) &
                 (csize = old csize - 1)";
    */

    public /*: encap */ Object remove_at_dep(int index)
    /*: requires "init & 0 <= index & index < csize"
        modifies content, csize
	ensures "((index, result) : old content) &
	         (csize = old csize - 1) &
                 (ALL j e. 
		  ((0 <= j & j < index) --> ((j, e) : content) = ((j, e) : old content)) &
		  ((index <= j & j < csize) --> ((j, e) : content) = ((j + 1, e) : old content)))" */

    public /*: encap */ boolean remove(Object o1)
    /*: requires "init"
        modifies content, csize
	ensures "((EX i. (i, o1) : old content) --> (result &
	          (EX i. ((i, o1) : old content) &
		         ~(EX j. j < i & (j, o1) : old content) &
			  (ALL j e. ((0 <= j & j < i) -->
			             (((j, e) : content) = ((j, e) : old content))) &
				    ((i <= j & j < csize) -->
			             (((j, e) : content) = ((j + 1, e) : old content))))))) &
	         (~(EX i. (i, o1) : old content) --> (~result & (content = old content)))"
    */

    public /*: encap */ void clear()
    /*: requires "init"
        modifies content, csize
	ensures "content = {} & csize = 0"
     */

    // Two stack operations

    public /*: encap */ void pushLast(Object element)
    /*: requires "init"
        modifies content, csize, msize
	ensures "comment ''sizeInc'' (csize = old csize + 1) &
                 comment ''contentAdd'' (content = old content Un {(csize - 1,element)})"
     */

    public Object popLast()
    /*: requires "init & csize > 0"
        modifies content, csize
	ensures "csize = old csize - 1 &
                 content = old content - {(csize,result)}"
     */
}
