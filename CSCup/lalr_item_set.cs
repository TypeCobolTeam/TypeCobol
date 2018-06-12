namespace TUVienna.CS_CUP
{
	using System;
	using System.Collections;

	/** This class represents a Set of LALR items.  For purposes of building
	 *  these sets, items are considered unique only if they have unique cores
	 *  (i.e., ignoring differences in their lookahead sets).<p>
	 *
	 *  This class provides fairly conventional Set oriented operations (union,
	 *  sub/super-Set tests, etc.), as well as an LALR "closure" operation (see 
	 *  compute_closure()).
	 *
	 * @see     java_cup.lalr_item
	 * @see     java_cup.lalr_state
	 * @version last updated: 3/6/96
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class lalr_item_set 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Constructor for an empty Set. */
		public lalr_item_set() { }

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Constructor for cloning from another Set. 
		 * @param other indicates Set we should copy from.
		 */
		public lalr_item_set(lalr_item_set other) 
			{
				not_null(other);
		_all = (Hashtable)other._all.Clone();
	}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** A hash table to implement the Set.  We store the items using themselves
	 *  as keys. 
	 */
	protected Hashtable _all = new Hashtable(11);

	/** Access to all elements of the Set. */
	public IEnumerator all() {return _all.Values.GetEnumerator();}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Cached hashcode for this Set. */
	protected int hashcode_cache = 0;
	protected bool is_cached =false;

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Size of the Set */
	public int size() {return _all.Count;}

	/*-----------------------------------------------------------*/
	/*--- Set Operation Methods ---------------------------------*/
	/*-----------------------------------------------------------*/

	/** Does the Set contain a particular item? 
	 * @param itm the item in question.
	 */
	public bool contains(lalr_item itm) {return _all.ContainsKey(itm);}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Return the item in the Set matching a particular item (or null if not 
	 *  found) 
	 *  @param itm the item we are looking for.
	 */
	public lalr_item find(lalr_item itm) {return (lalr_item)_all[itm];}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Is this Set an (improper) subset of another? 
	 * @param other the other Set in question.
	 */
	public bool is_subset_of(lalr_item_set other) 
{
	not_null(other);

	/* walk down our Set and make sure every element is in the other */
	IEnumerator e = all();
	while ( e.MoveNext() )
	if (!other.contains((lalr_item)e.Current))
	return false;

	/* they were all there */
	return true;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Is this Set an (improper) superset of another? 
	 * @param other the other Set in question.
	 */
	public bool is_superset_of(lalr_item_set other)
{
	not_null(other);
	return other.is_subset_of(this);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add a singleton item, merging lookahead sets if the item is already 
	 *  part of the Set.  returns the element of the Set that was added or 
	 *  merged into.
	 * @param itm the item being added.
	 */
	public lalr_item add(lalr_item itm) 
{
	lalr_item other;

	not_null(itm); 

	/* see if an item with a matching core is already there */
	other = (lalr_item)_all[itm];

	/* if so, merge this lookahead into the original and leave it */
	if (other != null)
{
	other.lookahead().add(itm.lookahead());
	return other;
}
	/* otherwise we just go in the Set */
	else
{
	/* invalidate cached hashcode */
	is_cached=false;

	_all.Add(itm,itm);
	return itm;
}
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove a single item if it is in the Set. 
	 * @param itm the item to remove.
	 */
	public void remove(lalr_item itm) 
{
	not_null(itm); 

	/* invalidate cached hashcode */
	is_cached=false;

	/* remove it from hash table implementing Set */
	_all.Remove(itm);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add a complete Set, merging lookaheads where items are already in 
	 *  the Set 
	 * @param other the Set to be added.
	 */
	public void add(lalr_item_set other) 
{
	not_null(other);

	/* walk down the other Set and do the adds individually */
	IEnumerator e = other.all();
	while ( e.MoveNext() )
	add((lalr_item)e.Current);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove (Set subtract) a complete Set. 
	 * @param other the Set to remove.
	 */
	public void remove(lalr_item_set other) 
{
	not_null(other);

	/* walk down the other Set and do the removes individually */
	IEnumerator e = other.all();
	while ( e.MoveNext() )
	remove((lalr_item)e.Current);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove and return one item from the Set (done in hash order). */
	public lalr_item get_one() 
{
	IEnumerator the_set;
	lalr_item result;

	the_set = all();
	if (the_set.MoveNext())
{
	result = (lalr_item)the_set.Current;
	remove(result);
	return result;
}
	else
	return null;
}

	/*-----------------------------------------------------------*/
	/*--- General Methods ---------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Helper function for null test.  Throws an interal_error exception if its
	 *  parameter is null.
	 *  @param obj the object we are testing.
	 */
	protected void not_null(object obj) 
{
	if (obj == null) 
	throw new internal_error("Null object used in Set operation");
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Compute the closure of the Set using the LALR closure rules.  Basically
	 *  for every item of the form: <pre>
	 *    [L ::= a *N alpha, l] 
	 *  </pre>
	 *  (where N is a a non terminal and alpha is a string of symbols) make 
	 *  sure there are also items of the form:  <pre>
	 *    [N ::= *beta, first(alpha l)] 
	 *  </pre>
	 *  corresponding to each production of N.  Items with identical cores but 
	 *  differing lookahead sets are merged by creating a new item with the same 
	 *  core and the union of the lookahead sets (the LA in LALR stands for 
	 *  "lookahead merged" and this is where the merger is).  This routine 
	 *  assumes that nullability and first sets have been computed for all 
	 *  productions before it is called.
	 */
	public void compute_closure()
	
{
	lalr_item_set consider;
	lalr_item     itm, new_itm, add_itm;
	non_terminal  nt;
	terminal_set  new_lookaheads;
	IEnumerator   p;
	production    prod;
	bool       need_prop;



	/* invalidate cached hashcode */
	is_cached=false;

	/* each current element needs to be considered */
	consider = new lalr_item_set(this);

	/* repeat this until there is nothing else to consider */
	while (consider.size() > 0)
{
	/* get one item to consider */
	itm = consider.get_one(); 

	/* do we have a dot before a non terminal */
	nt = itm.dot_before_nt();
	if (nt != null)
{
	/* create the lookahead Set based on first after dot */
	new_lookaheads = itm.calc_lookahead(itm.lookahead());

	/* are we going to need to propagate our lookahead to new item */
	need_prop = itm.lookahead_visible();

	/* create items for each production of that non term */
	p = nt.productions();
	while ( p.MoveNext() )
{
	prod = (production)p.Current;

	/* create new item with dot at start and that lookahead */
	new_itm = new lalr_item(prod, 
	new terminal_set(new_lookaheads));

	/* add/merge item into the Set */
	add_itm = add(new_itm);
	/* if propagation is needed link to that item */
	if (need_prop)
	itm.add_propagate(add_itm);

	/* was this was a new item*/
	if (add_itm == new_itm)
{
	/* that may need further closure, consider it also */ 
	consider.add(new_itm);
} 
} 
} 
} 
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Equality comparison. */
	public bool Equals(lalr_item_set other)
{
	if (other == null || other.size() != size()) return false;

	/* once we know they are the same size, then improper subset does test */
	try 
{
	return is_subset_of(other);
} 
	catch (internal_error e) 
{
	/* can't throw error from here (because superclass doesn't) so crash */
	e.crash();
	return false;
}

}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Generic equality comparison. */
	public override bool Equals(object other)
{
	if (other.GetType()!=typeof(lalr_item_set))
	return false;
	else
	return Equals((lalr_item_set)other);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Return hash code. */
	public override int GetHashCode()
{
	int result = 0;
	IEnumerator e;
//	int cnt;

	/* only compute a new one if we don't have it cached */
	if (!is_cached)
{
	/* hash together codes from at most first 5 elements */
	//   CSA fix! we'd *like* to hash just a few elements, but
	//   that means equal sets will have inequal hashcodes, which
	//   we're not allowed (by contract) to do.  So hash them all.
	e = all();
	while(e.MoveNext())
		result ^=((lalr_item)e.Current).GetHashCode();

	hashcode_cache = result;
	is_cached=true;
}

	return hashcode_cache;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Convert to string. */
	public override string ToString()
{
	System.Text.StringBuilder result=new System.Text.StringBuilder() ;

	result.Append("{\n");
	IEnumerator e=all();
	while ( e.MoveNext() ) 
{
	result.Append("  " + (lalr_item)e.Current + "\n");
}
	result.Append("}");

	return result.ToString();
}
	/*-----------------------------------------------------------*/
}
}
