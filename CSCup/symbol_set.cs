
	
namespace TUVienna.CS_CUP
{
	using System.Collections;


	/** This class represents a Set of symbols and provides a series of 
	 *  Set operations to manipulate them.
	 *
	 * @see     java_cup.symbol
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 */
	public class symbol_set 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Constructor for an empty Set. */
		public symbol_set() { }

		/** Constructor for cloning from another Set. 
		 * @param other the Set we are cloning from.
		 */
		public symbol_set(symbol_set other) 
											{
												not_null(other);
		_all = (Hashtable)other._all.Clone();
	}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** A hash table to hold the Set. Symbols are keyed using their name string. 
	 */
	protected Hashtable _all = new Hashtable(11);

	/** Access to all elements of the Set. */
	public IEnumerator all() {return _all.Values.GetEnumerator();}

	/** size of the Set */
	public int size() {return _all.Count;}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** Helper function to test for a null object and throw an exception
	 *  if one is found.
	 * @param obj the object we are testing.
	 */
	protected void not_null(object obj) 
{
	if (obj == null) 
	throw new internal_error("Null object used in Set operation");
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if the Set contains a particular symbol. 
	 * @param sym the symbol we are looking for.
	 */
	public bool contains(symbol sym) {return _all.ContainsKey(sym.name());}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if this Set is an (improper) subset of another. 
	 * @param other the Set we are testing against.
	 */
	public bool is_subset_of(symbol_set other) 
{
	not_null(other);

	/* walk down our Set and make sure every element is in the other */
	IEnumerator e = all();
	while ( e.MoveNext() )
	if (!other.contains((symbol)e.Current))
	return false;

	/* they were all there */
	return true;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if this Set is an (improper) superset of another. 
	 * @param other the Set we are are testing against.
	 */
	public bool is_superset_of(symbol_set other) 
{
	not_null(other);
	return other.is_subset_of(this);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add a single symbol to the Set.  
	 * @param sym the symbol we are adding.
	 * @return true if this changes the Set.
	 */
	public bool add(symbol sym) 
{
//	object previous;

	not_null(sym); 

	/* put the object in */
		try
		{
			_all.Add(sym.name(),sym);
		}
		catch
		{
			return false;
		}

	/* if we had a previous, this is no change */
	//return previous == null;
		return true;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove a single symbol if it is in the Set. 
	 * @param sym the symbol we are removing.
	 */
	public void remove(symbol sym) 
{
	not_null(sym); 
	_all.Remove(sym.name());
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add (union) in a complete Set.  
	 * @param other the Set we are adding in.
	 * @return true if this changes the Set. 
	 */
	public bool add(symbol_set other)
{
	bool result = false;

	not_null(other);

	/* walk down the other Set and do the adds individually */
	IEnumerator e = other.all();
	while ( e.MoveNext())
	result = add((symbol)e.Current) || result;

	return result;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove (Set subtract) a complete Set. 
	 * @param other the Set we are removing.
	 */
	public void remove(symbol_set other) 
{
	not_null(other);

	/* walk down the other Set and do the removes individually */
	IEnumerator e = other.all();
	while ( e.MoveNext() )
	remove((symbol)e.Current);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Equality comparison. */
	public bool Equals(symbol_set other) 
{
	if (other == null || other.size() != size()) return false;

	/* once we know they are the same size, then improper subset does test */
	try 
{
	return is_subset_of(other);
} 
	catch (internal_error e) 
{
	/* can't throw the error (because super class doesn't), so we crash */
	e.crash();
	return false;
}
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Generic equality comparison. */
	public override  bool Equals(object other)
{
	if (other.GetType()!=typeof(symbol_set))
	return false;
	else
	return Equals((symbol_set)other);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Compute a hash code. */
	public override int GetHashCode()
{
	int result = 0;
	int cnt;
	IEnumerator e;

	/* hash together codes from at most first 5 elements */
	e = all();
	cnt=0;
	while (e.MoveNext() && cnt<5 )
	{
		cnt++;
	   result ^= ((symbol)e.Current).GetHashCode();
	}

	return result;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Convert to a string. */
	public override string ToString()
{
	string result;
	bool comma_flag;

	result = "{";
	comma_flag = false;
	IEnumerator e = all();
	while ( e.MoveNext() )
{
	if (comma_flag)
	result += ", ";
	else
	comma_flag = true;

	result += ((symbol)e.Current).name();
}
	result += "}";

	return result;
}

	/*-----------------------------------------------------------*/

}
}

