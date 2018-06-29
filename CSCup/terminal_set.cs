
namespace TUVienna.CS_CUP
{
	using System.Collections;



	/** A Set of terminals implemented as a bitset. 
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
     * 
     * most changes are resulting from using BitArray instead of BitSet
     *  and the different functionality
	 */
	public class terminal_set 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Constructor for an empty Set. */
		public terminal_set() 
		{ 
			/* allocate the bitset at what is probably the right size */
			_elements = new BitArray(terminal.number());
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Constructor for cloning from another Set. 
		 * @param other the Set we are cloning from.
		 */
		public terminal_set(terminal_set other) 
			{
				not_null(other);
		_elements = (BitArray)other._elements.Clone();
	}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Static (Class) Variables ------------------*/
	/*-----------------------------------------------------------*/

	/** Constant for the empty Set. */
	public static terminal_set EMPTY = new terminal_set();

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** Bitset to implement the actual Set. */
	protected BitArray _elements;

	/*-----------------------------------------------------------*/
	/*--- General Methods ----------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Helper function to test for a null object and throw an exception if
	 *  one is found. 
	 * @param obj the object we are testing.
	 */
	protected void not_null(object obj)
{
	if (obj == null) 
	throw new internal_error("Null object used in Set operation");
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if the Set is empty. */
	public bool empty()
{
	return equals(_elements,EMPTY);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if the Set contains a particular terminal. 
	 * @param sym the terminal symbol we are looking for.
	 */
	public bool contains(terminal sym) 
{
	not_null(sym); 
	return _elements.Get(sym.index());
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Given its index determine if the Set contains a particular terminal. 
	 * @param indx the index of the terminal in question.
	 */
	public bool contains(int indx) 
{
	return _elements.Get(indx);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if this Set is an (improper) subset of another.
	 * @param other the Set we are testing against.
	 */

        /* If BitArray are of different size the shorter one is stretched to accomodate
         * this is needed for logical Set operations like or,and,xor
         */

        public void equalize(ref BitArray set1,ref BitArray set2)
        {
            if (set1.Length<set2.Length)
            {
                bool[] arr=new bool[set2.Length];
                set1.CopyTo(arr,0);
                set1= new BitArray(arr);
            }
            else if(set1.Length>set2.Length)
            {
                bool[] arr=new bool[set1.Length];
                set2.CopyTo(arr,0);
                set2= new BitArray(arr);
            }
        }

	public bool is_subset_of(terminal_set other)
{
	not_null(other);

	/* make a copy of the other Set */ 
    
	BitArray copy_other = (BitArray)other._elements.Clone();
    equalize(ref copy_other,ref  _elements);
	/* and or in */
       
    copy_other.Or(_elements);

          
    
	/* if it hasn't changed, we were a subset */
	return equals(copy_other,other);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if this Set is an (improper) superset of another.
	 * @param other the Set we are testing against.
	 */
	public bool is_superset_of(terminal_set other)
{
	not_null(other);
	return other.is_subset_of(this);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add a single terminal to the Set.  
	 * @param sym the terminal being added.
	 * @return true if this changes the Set.
	 */
	public bool add(terminal sym) 
{
	bool result;

	not_null(sym); 

	/* see if we already have this */ 
	result = _elements.Get(sym.index());

	/* if not we add it */
	if (!result)
	_elements.Set(sym.index(),true);

	return result;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Remove a terminal if it is in the Set.
	 * @param sym the terminal being removed.
	 */
	public void remove(terminal sym) 
{
	not_null(sym); 
	_elements.Set(sym.index(),false);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add (union) in a complete Set.  
	 * @param other the Set being added.
	 * @return true if this changes the Set.
	 */
	public bool add(terminal_set other)
{
	not_null(other);
  
	/* make a copy */
	BitArray copy = (BitArray)_elements.Clone();

	/* or in the other Set */
    equalize(ref _elements,ref other._elements);
    _elements.Or(other._elements);

	/* changed if we are not the same as the copy */
	return !equals(_elements,copy);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Determine if this Set intersects another.
	 * @param other the other Set in question.
	 */
	public bool intersects(terminal_set other)
{
	not_null(other);

	/* make a copy of the other Set */
	BitArray copy = (BitArray)other._elements.Clone();

	/* xor out our values */
	copy.Xor(this._elements);

	/* see if its different */
	return !equals(copy,other);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Equality comparison. */
        /* equality comparison, but with different name as Equal because 
         * it's only logical equation. That means that Sets of different length but 
         * padded only with 0's are equal
         */
        public  bool equals(BitArray set1,terminal_set set2)
        {
            if (set2 == null||set1==null) 
                return false;
            else
            {
                int max=System.Math.Max(set1.Length,set2._elements.Length);
                bool[] ar1=new bool[max];
                bool[] ar2=new bool[max];
                set1.CopyTo(ar1,0);
                set2._elements.CopyTo(ar2,0);
                bool eq=true;
                for (int i=0;i<max;i++)
                    if (ar1[i]!=ar2[i]) eq=false;

                return eq;
            }
        }

        public  bool equals(BitArray set1,BitArray set2)
        {
            if (set2 == null||set1==null) 
                return false;
            else
            {
                int max=System.Math.Max(set1.Length,set2.Length);
                bool[] ar1=new bool[max];
                bool[] ar2=new bool[max];
                set1.CopyTo(ar1,0);
                set2.CopyTo(ar2,0);
                bool eq=true;
                for (int i=0;i<max;i++)
                    if (ar1[i]!=ar2[i]) eq=false;

                return eq;
            }
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Generic equality comparison. */
	public override  bool Equals(object other)
{
	if (other.GetType()!=typeof(terminal_set))
	return false;
	else
	return equals(_elements,(terminal_set)other);
}

		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Convert to string. */
	public override string ToString()
{
	string result;
	bool comma_flag;
      
	result = "{";
	comma_flag = false;

	for (int t = 0; t < terminal.number(); t++)
{
	if (_elements.Get(t))
{
	if (comma_flag)
	result += ", ";
	else
	comma_flag = true;

	result += terminal.find(t).name();
}
}
	result += "}";

	return result;
}
        //own debugging function
        public string ToMString()
        {
            string result="{";
            for (int t =0; t<terminal.number();t++)
                if (t>=_elements.Length)
                    result+="0";
                else if (!_elements.Get(t))
                    result+="0";
                else 
                    result +="1";
            return result +"}";
        }


	/*-----------------------------------------------------------*/

}
}
