public /*: claimedby Hashtable */ class Node {
    public /*: claimedby Hashtable */ Object key;
    public /*: claimedby Hashtable */ Object value;
    public /*: claimedby Hashtable */ Node next;
}

class Hashtable {
    private static Node[] table = null;

    /*: 
     public static ghost specvar content :: "(obj * obj) set" = "{}";
     public static ghost specvar init :: "bool" = "False";

     private static ghost specvar h :: "(obj \<Rightarrow> int \<Rightarrow> int)";

     private static ghost specvar con :: "obj => (obj * obj) set" = "\<lambda> this. {}";

     invariant ContentDefInv:
     "init \<longrightarrow> content = {(k,v). (k,v) \<in> table.[(h k (table..length))]..con}";

     public ensured invariant ContentAlloc:
     "\<forall> x y. (x,y) \<in> content \<longrightarrow> (x \<in> alloc \<and> y : alloc)";

     invariant TableNotNull: "init \<longrightarrow> table \<noteq> null";
     invariant TableHidden: "init \<longrightarrow> table \<in> hidden";
     invariant AllocInv: "table \<in> alloc";

     invariant NodeHidden: 
     "init \<longrightarrow> (\<forall> x. x \<in> alloc & x \<in> Node --> x \<in> hidden)";

     invariant HashInv:
     "init \<longrightarrow> (\<forall> k. 0 \<le> (h k (table..length)) \<and> (h k (table..length)) < table..length)";

     invariant ElementAllocInv:
       "\<forall> i. 0 \<le> i \<and> i < table..length \<longrightarrow> table.[i] \<in> alloc";

     invariant FirstInjInv:
       "init \<longrightarrow> (\<forall> i x y. 
          y = x..next \<and> y \<noteq> null \<and> 0 \<le> i \<and> i < table..length \<longrightarrow> y \<noteq> table.[i])";

     invariant NextInjInv:
       "\<forall> x1 x2 y. y \<noteq> null \<and> y = x1..next \<and> y = x2..next \<longrightarrow> x1 = x2";

     invariant ElementInjInv:
       "init \<longrightarrow> (\<forall> i j.
	  ((0 \<le> i \<and> i < table..length \<and>
	    0 \<le> j \<and> j < table..length \<and>
	    table.[i] \<noteq> null \<and> table.[i] = table.[j]) \<longrightarrow> i = j))";

     invariant ElementTypeInv:
       "\<forall> i. 0 \<le> i \<and> i < table..length \<longrightarrow> table.[i] \<in> Node";

     public ensured invariant SetInv:
       "\<forall> k v0 v1. (k,v0) \<in> content \<and> (k,v1) \<in> content \<longrightarrow> v0 = v1";

     public ensured invariant NonNullInv:
       "\<forall> k v. (k,v) \<in> content \<longrightarrow> k \<noteq> null \<and> v \<noteq> null";

     invariant Coherence: "init \<longrightarrow>
       (\<forall> i k v. 0 \<le> i \<and> i < table..length \<longrightarrow>
           (k,v) \<in> table.[i]..con \<longrightarrow> h k (table..length) = i)";

     invariant ConAlloc:
       "\<forall> z x y. z \<in> Node \<and> z \<in> alloc \<and> (x,y) \<in> z..con \<longrightarrow> x \<in> alloc \<and> y \<in> alloc";

     invariant ConNull:
       "\<forall> x. x \<in> Node \<and> x \<in> alloc \<and> x = null \<longrightarrow> x..con = {}";

     invariant ConDef:
       "ALL x. x : Node & x : Object.alloc & x ~= null -->
          x..con = 
	  {(x..Node.key, x..Node.value)} Un x..Node.next..con &
	  (ALL v. (x..Node.key, v) ~: x..Node.next..con)";

     invariant ConNonNull:
       "ALL z x y. z : Node & z : Object.alloc & z ~= null & 
       (x, y) : z..con --> x ~= null & y ~= null";

    */

    public static void init(int n)
    /*: 
      modifies content, init
      ensures "init & content = {}"
    */
    {
	int i = 0;
	//: "init" := "False";
	table = new /*: hidden */ Node[n];
	
	while /*: inv "(ALL j. 0 <= j & j < i --> table.[j] = null) &
		       (ALL a i. (a : Object.alloc & a : Array & a ~: hidden) -->
		        (a.[i] = (arrayRead (old Array.arrayState) a i))) &
		       null..con = {}";
	       */
	    (i < n) {
	    table[i] = null;
	}
	//: "content" := "{}";
	//: "init" := "True";

	//: assume "theinv HashInv";
    }

    private static int compute_hash(Object o1)
    /*: requires "o1 ~= null & init & theinvs"
        ensures "result = h o1 (table..Array.length) & 0 <= result & 
	         result < table..Array.length &
		 Object.alloc = (old Object.alloc) & theinvs"
    */
    {
	int result = o1.hashCode() % table.length;
	//: assume "result = h o1 (table..Array.length)";
	//: assume "Object.alloc = (old Object.alloc)";
	return result;
    }

    public static boolean containsKey(Object k0)
    /*: requires "init"
        ensures "result = (EX v. ((k0, v) : content))"
     */
    {
        return containsKey0(k0);
    }

    private static boolean containsKey0(Object k0)
    /*: requires "init & theinvs"
        ensures "result = (EX v. ((k0, v) : content)) & theinvs"
    */
    {
	int hc = compute_hash(k0);
	boolean res = bucketContainsKey(hc, k0);
	return res;
    }
    
    private static boolean bucketContainsKey(int bucket_id, Object k0)
    /*: requires "init & theinvs & 0 <= bucket_id & 
                  bucket_id < table..Array.length"
	ensures "result = (EX v. ((k0, v) : table.[bucket_id]..con)) & 
	         theinvs"
     */
    {
        Node current = table[bucket_id];
        while /*: inv "current : Node & current : Object.alloc & theinvs &
		       (EX v. (k0, v) : table.[bucket_id]..con) = 
		       (EX v. (k0, v) : current..con)"
	       */
            (current != null) {
            if (current.key == k0) {
                return true;
            }
            current = current.next;
        }
        return false;
    }

    public static Object remove1(Object k0)
    /*: requires "init & k0 ~= null & (EX v. (k0, v) : content)"
        modifies content
        ensures "content = old content \<setminus> {(k0, result)} &
	         (k0, result) : old content"
    */
    {
	return remove(k0);
    }

    private static Object remove(Object k0)
    /*: requires "theinvs & init & k0 ~= null & (EX v. (k0, v) : content)"
        modifies content, con, next, arrayState
        ensures "theinvs &
	         comment ''IndeedRemoved'' (content = old content \<setminus> {(k0, result)}) &
	         comment ''IndeedWasThere'' ((k0, result) : old content) &
		 comment ''frameArray'' (ALL a i. a ~= table --> a.[i] = old (a.[i]))"
    */
    {
	//: private ghost specvar v0 :: obj
	//: havoc v0 suchThat "(k0, v0) : content"
	int hc = compute_hash(k0);
	Node f = table[hc];
	//: noteThat KeyInBucket: "(k0, v0) : table.[hc]..con";
	//: noteThat HProps: "hc = h k0 (table..Array.length)";
	//: noteThat FProps: "f : Node & f : old Object.alloc";
	/*: noteThat FNonNull: "f ~= null" 
	  from FProps, KeyInBucket, ConNull, FNonNull;
	*/
	//: noteThat Init: "init";
	if (f.key == k0) {
	    Node second = f.next;
	    f.next = null;
	    //: "f..con" := "{(f..Node.key, f..Node.value)}";
	    table[hc] = second;
	    //: "content" := "old content \<setminus> {(k0, v0)}";

	    //: noteThat HCProps: "0 <= hc & hc < table..Array.length";
	    /*: noteThat Acyclic:
	      "fieldRead (old Node.next) 
	      (arrayRead (old Array.arrayState) table hc) ~=
	      (arrayRead (old Array.arrayState) table hc)"
	      from Init, FNonNull, HCProps, FirstInjInv, Acyclic;
	    */
	    //: noteThat KFound: "f..Node.key = k0";
	    //: noteThat VFound: "f..Node.value = v0";
	    /*: noteThat NewContentDef: "theinv ContentDefInv"
	      from ContentDefInv, Acyclic, ConDef, ElementInjInv, FProps, 
	      FNonNull, KFound, VFound, HashInv, HCProps, HProps, 
	      NewContentDef;
	    */

	    //: noteThat AllocUnchanged: "Object.alloc = old Object.alloc";
	    /*: noteThat FNotRefByNext: "ALL x. x..Node.next ~= f"
	      from FNotRefByNext, FirstInjInv, Init, FNonNull, HCProps;
	    */
	    //: noteThat NullNode: "null : Node";
	    //: noteThat NullAlloc: "null : Object.alloc";
	    /*: noteThat NewConDef: "theinv ConDef"
	      from ConDef, AllocUnchanged, ConNull, ElementAllocInv,
	      ElementTypeInv, FNotRefByNext, HCProps, NullNode, NullAlloc,
	      NewConDef;
	     */

	    //: noteThat NextNull: "(old Node.next) null = null";
	    /*: noteThat NewCoherence: "theinv Coherence"
	      from Coherence, Acyclic, ConDef, ConNull, NextNull, 
	      ElementAllocInv, ElementTypeInv, NewCoherence;
	     */

	    /*: noteThat FirstInjLemma: "theinv FirstInjInv"
	      from FirstInjInv, NextInjInv;
	    */
	    /*: noteThat ElementInjLemma: "theinv ElementInjInv"
	      from ElementInjInv, FirstInjInvProcedurePrecondition;
	    */
	    return f.value;
	} else {
	    Node prev = f;
	    /*: "prev..con" := 
	        "prev..con \<setminus> {(k0, v0)}";
	    */
	    //: "content" := "old content \<setminus> {(k0, v0)}";
	    Node current = prev.next;
	    while /*: inv "
		    prev : Node & prev : Object.alloc & prev ~= null &
		    comment ''PrevCon'' (prev..con = 
		     fieldRead (old con) prev \<setminus> {(k0, v0)}) &
	             current : Node & current : Object.alloc & current ~= null &
		    comment ''PrevCurr'' (prev..Node.next = current & prev ~= current) &
		    content = old content \<setminus> {(k0, v0)} &
		    (k0, v0) : current..con &
		    comment ''ConDefInv''
		    (ALL n. n : Node & n : Object.alloc & n ~= null & n ~= prev -->
		    n..con = {(n..Node.key, n..Node.value)} Un n..Node.next..con &
		    (ALL v. (n..Node.key, v) ~: n..Node.next..con)) &
		    (ALL n. n..con = old (n..con) |
		    n..con = old (n..con) \<setminus> {(k0, v0)}) &
		    null..con = {} &
		    comment ''FConInv'' (f..con = 
		    (fieldRead (old con) f) \<setminus> {(k0, v0)})"
		  */
		(current.key != k0)
                {
                    /*: "current..con" := 
		      "current..con \<setminus> {(k0, v0)}"; */
                    prev = current;
                    current = current.next;
                }
            //: noteThat ConNonNullLemma: "theinv ConNonNull";
	    Node nxt = current.next;
	    prev.next = nxt;
	    current.next = null;
	    //: "current..con" := "{(current..Node.key, current..Node.value)}";

	    //: noteThat "theinv FirstInjInv";

	    //: noteThat NullNotCurrent: "null ~= current";
	    //: noteThat ConOfNull: "null..con = {}";
	    /*: noteThat NextNotCurr:
	      "ALL x. x ~= prev --> fieldRead (old Node.next) x ~= current";
	    */
	    /*: noteThat UpdatedConDef:
	      "(ALL n. n : Node & n : Object.alloc & n ~= null & n ~= prev -->
	       n..con = 
	       {(n..Node.key, n..Node.value)} Un n..Node.next..con &
	       (ALL v. (n..Node.key, v) ~: n..Node.next..con))"
	       from ConDefInv, NullNotCurrent, ConOfNull, NextNotCurr,
	       UpdatedConDef;
	    */

	    //: noteThat NextOfNull: "fieldRead (old Node.next) null = null";
	    //: noteThat PrevNotCurr: "prev ~= current";
	    /*: noteThat CurrNextNotCurr: 
	      "fieldRead (old Node.next) current ~= current";
	     */
	    /*: noteThat KeyNotInRest:
	      "(k0, v0) ~: fieldRead (old con) 
	                    (fieldRead (old Node.next) current)";
	     */
	    //: noteThat PrevNotKey: "prev..Node.key ~= k0";
	    /*: noteThat OldCurrConDef: 
	      "fieldRead (old con) current = 
	      {(k0, v0)} Un 
	      (fieldRead (old con) (fieldRead (old Node.next) current))";
	     */
	    /*: noteThat OldPrevConIs:
	      "fieldRead (old con) prev =
	      {(prev..Node.key, prev..Node.value)} Un
	      (fieldRead (old con) current)";
	     */
	    /*: noteThat PrevConUpdate:
	      "prev..con = 
	      (fieldRead (old con) prev) \<setminus> {(k0, v0)}";
	    */
	    /*: noteThat ConEitherOr:
	      "(fieldRead con (fieldRead (old Node.next) current) =
	       fieldRead (old con) (fieldRead (old Node.next) current)) |
	       (fieldRead con (fieldRead (old Node.next) current) =
	       ((fieldRead (old con) (fieldRead (old Node.next) current))
	       \<setminus> {(k0, v0)}))";
	    */
	    /*: noteThat PrevConADef:
	      "prev..con = 
	       {(prev..Node.key, prev..Node.value)} Un 
	       prev..Node.next..con"
	       from NullNotCurrent, NextOfNull, PrevNotCurr, CurrNextNotCurr,
	       KeyNotInRest, PrevNotKey, OldCurrConDef, OldPrevConIs, 
	       PrevConUpdate, ConEitherOr, PrevConADef;
	    */
	    /*: noteThat PrevKeyNotInRest:
	      "ALL v. (prev..Node.key, v) ~: 
	      fieldRead (old con) (fieldRead (old Node.next) current)";
	     */
	    /*: noteThat PrevConBDef:
	      "(ALL v. (prev..Node.key, v) ~: prev..Node.next..con)"
	      from PrevNotCurr, CurrNextNotCurr, ConEitherOr, KeyNotInRest,
	      PrevKeyNotInRest, PrevConBDef;
	    */
	    /*: noteThat PrevConDef:
	      "prev..con = 
	       {(prev..Node.key, prev..Node.value)} Un 
	        prev..Node.next..con &
	       (ALL v. (prev..Node.key, v) ~: prev..Node.next..con)"
	       from PrevConADef, PrevConBDef, PrevConDef;
	     */
	    /*: noteThat NewConDef: "theinv ConDef"
	      from UpdatedConDef, PrevConDef, NewConDef;
	     */

	    /*: noteThat CurrentNext:
	      "current = fieldRead (old Node.next) prev";
	    */
	    //: noteThat CurrentNotNull: "current ~= null";
	    /*: noteThat NotFirstAlt: 
	      "ALL k. current ~= table.[(h k (table..Array.length))]"
	      from FirstInjInv, CurrentNext, CurrentNotNull, Init, HashInv,
	      NotFirstAlt;
	     */
	    /*: noteThat ConLInv: "ALL n. n ~= current --> 
	           (n..con = old (n..con) |
	            n..con = old (n..con) \<setminus> {(k0, v0)})"
	     */
	    //: noteThat KCurrent: "current..Node.key = k0";
	    //: noteThat VCurrent: "current..Node.value = v0";
	    //: noteThat NotFirstThisBucket: "table.[hc] ~= current";
	    /*: noteThat FConLemma: 
	      "f..con = 
	       (fieldRead (old con) f) \<setminus> {(k0, v0)}"
	       from FConInv, NotFirstThisBucket, FConLemma;
	     */
	    /*: noteThat OtherBucketsK: 
	          "ALL k. h k (table..Array.length) ~= hc --> 
		    k ~= current..Node.key";
	    */
	    /*: noteThat FBContentDef: "theinv ContentDefInv"
	      from ContentDefInv, KCurrent, VCurrent, ConLInv, NotFirstAlt, 
	      HashInv, FConLemma, HProps, OtherBucketsK, FBContentDef;
	    */
	    return current.value;
	}
    }

    private static void add(Object k0, Object v0)
    /*: requires "init & k0 ~= null & v0 ~= null & 
                  ~(EX v. (k0, v) : content) & theinvs"
        modifies content, arrayState, "new..con", "new..next", "new..value", "new..key"
        ensures "content = old content Un {(k0, v0)} &
	        (ALL a i. a ~= table \<longrightarrow> a.[i] = old (a.[i])) & theinvs" */
    {
	int hc = compute_hash(k0);
	Node n = new /*: hidden */ Node();
	n.key = k0;
	n.value = v0;
	Node first = table[hc];
	n.next = first;
	//: "n..con" := "{(k0, v0)} Un first..con";
	table[hc] = n;
	//: "content" := "(old content) Un {(k0, v0)}";

	//: noteThat Init: "init";
	//: noteThat AllocChange: "Object.alloc = old Object.alloc Un {n}";
	//: noteThat HCBounds: "0 <= hc & hc < table..Array.length";

	//: noteThat AllocChange: "Object.alloc = old Object.alloc Un {n}";
	//: noteThat HProp: "hc = h k0 (table..Array.length)";

	//: noteThat NewNotAlloc: "n ~: old Object.alloc";
	/*: noteThat NewNotRefArray:
	  "ALL i. 0 <= i & i < table..Array.length -->
	  (arrayRead (old Array.arrayState) table i) ~= n"
	  from ElementAllocInv, NewNotAlloc, NewNotRefArray;
	 */
	/*: noteThat NewContentDef: "theinv ContentDefInv"
	  from ContentDefInv, HProp, NewNotRefArray, HashInv, NewContentDef;
	*/

	//: noteThat NewOldNEq: "n ~= first";
	/*: noteThat NewNotRefByNext: "ALL x. x..Node.next ~= n"
	  from NewOldNEq, NewNotAlloc, unalloc_lonely, NewNotRefByNext;
	*/
	/*: noteThat OldNotRefByNext: 
	  "ALL x. first ~= null --> old (x..Node.next) ~= first"
	  from FirstInjInv, Init, HCBounds, OldNotRefByNext;
	 */
	/*: noteThat OldNotRefInTable: 
	  "ALL i. 0 <= i & i < table..Array.length & first ~= null --> 
	  table.[i] ~= first"
	  from Init, HCBounds, ElementInjInv, NewOldNEq, OldNotRefInTable;
	 */
	/*: noteThat NewFirstInj: "theinv FirstInjInv"
	  from FirstInjInv, NewNotRefByNext, OldNotRefByNext, 
	  OldNotRefInTable, HCBounds, NewFirstInj;
	 */

	/*: noteThat NewNextInj: "theinv NextInjInv"
	  from NextInjInv, OldNotRefByNext, NewNextInj;
	 */

	//: noteThat NotInContent: "ALL v. (k0, v) ~: old content";
	/*: noteThat NewConDef: "theinv ConDef" 
	  from Init, NotInContent, ContentDefInv, ConDef, AllocChange, 
	  NewNotRefByNext, HProp, NewConDef;
	 */
    }

    public static void add1(Object k0, Object v0)
    /*: requires "init & k0 ~= null & v0 ~= null & 
                  ~(EX v. (k0, v) : content)"
        modifies content
        ensures "content = old content Un {(k0, v0)}"
    */
    {
        add(k0, v0);
    }

    public static Object replace(Object k0, Object v0)
    /*: requires "init & k0 ~= null & v0 ~= null & (EX v. (k0, v) : content)"
        modifies content
	ensures "content = old content - {(k0, result)} Un {(k0, v0)} &
                 (k0, result) : old content"
    */
    {
	Object v1 = remove(k0);
	add(k0, v0);
	return v1;
    }

    public static Object put(Object k0, Object v0)
    /*: requires "init & k0 ~= null & v0 ~= null"
        modifies content
	ensures "(result = null --> content = old content Un {(k0, v0)}) &
	         (result ~= null -->
		   content = old content - {(k0, result)} Un {(k0, v0)})"
     */
    {
	if (containsKey0(k0)) {
            Object v1 = remove(k0);
            add(k0, v0);
	    return v1;
	} else {
	    add(k0, v0);
	    return null;
	}
    }

    public static Object get(Object k0)
    /*: requires "init & k0 ~= null"
        ensures "(result ~= null --> (k0, result) : content) &
	         (result = null --> ~(EX v. (k0, v) : content)) &
		 Object.alloc = old Object.alloc"
	         
    */
    {
	int hc = compute_hash(k0);
	Node current = table[hc];
        while /*: inv "current : Node & current : Object.alloc &
                       (ALL v. ((k0, v) : content) = ((k0, v) : current..con))" */
            (current != null) {
            if (current.key == k0) {
                return current.value;
            }
            current = current.next;
        }
        return null;
    }

    public static boolean isEmpty()
    /*: requires "init"
        ensures "result = (content = {})";
    */
    {
	int i = 0;
	while /*: inv "0 <= i &
		       (ALL j. 0 <= j & j < i --> table.[j]..con = {})" */
	    (i < table.length) {
	    if (table[i] != null) {
		return false;
	    }
	}
	return true;
    }

}
