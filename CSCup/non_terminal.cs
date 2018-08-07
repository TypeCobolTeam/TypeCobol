namespace TUVienna.CS_CUP
{
	
	using System.Collections;

	/** This class represents a non-terminal symbol in the grammar.  Each
	 *  non terminal has a textual name, an index, and a string which indicates
	 *  the type of object it will be implemented with at runtime (i.e. the class
	 *  of object that will be pushed on the parse stack to represent it). 
	 *
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class non_terminal : symbol 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Full constructor.
		 * @param nm  the name of the non terminal.
		 * @param tp  the type string for the non terminal.
		 */
		public non_terminal(string nm, string tp):base(nm,tp)
		{
			/* super class does most of the work */
		

			/* add to Set of all non terminals and check for duplicates */
			try
			{
				_all.Add(nm,this);
			}
			catch 
			{
				// can't throw an exception here because these are used in static
				// initializers, so we crash instead
				// was: 
				// throw new internal_error("Duplicate non-terminal ("+nm+") created");
				(new internal_error("Duplicate non-terminal ("+nm+") created")).crash();
			}
			/* assign a unique index */
			_index = next_index++;

			/* add to by_index Set */
			_all_by_index.Add(_index, this);
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Constructor with default type. 
		 * @param nm  the name of the non terminal.
		 */
		public non_terminal(string nm) :this(nm,null){	}

		/*-----------------------------------------------------------*/
		/*--- (Access to) Static (Class) Variables ------------------*/
		/*-----------------------------------------------------------*/

		/** Table of all non-terminals -- elements are stored using name strings 
		 *  as the key 
		 */
		protected static Hashtable _all = new Hashtable();

		/** Access to all non-terminals. */
		public static IEnumerator all() {return _all.Values.GetEnumerator();}

		/** lookup a non terminal by name string */ 
		public static non_terminal find(string with_name)
		{
			if (with_name == null)
				return null;
			else 
				return (non_terminal)_all[with_name];
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Table of all non terminals indexed by their index number. */
		protected static Hashtable _all_by_index = new Hashtable();

		/** Lookup a non terminal by index. */
		public static non_terminal find(int indx)
		{
			int the_indx = indx;

			return (non_terminal)_all_by_index[the_indx];
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Total number of non-terminals. */
		public static int number() {return _all.Count;}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Static counter to assign unique indexes. */
		protected static int next_index = 0;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Static counter for creating unique non-terminal names */
		static protected int next_nt = 0;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** special non-terminal for start symbol */
		public static  non_terminal START_nt = new non_terminal("$START");

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** flag non-terminals created to embed action productions */
		public bool is_embedded_action = false; /* added 24-Mar-1998, CSA */

		/*-----------------------------------------------------------*/
		/*--- Static Methods ----------------------------------------*/
		/*-----------------------------------------------------------*/
	 
		/** Method for creating a new uniquely named hidden non-terminal using 
		 *  the given string as a base for the name (or "NT$" if null is passed).
		 * @param prefix base name to construct unique name from. 
		 */
		static non_terminal create_new(string prefix)
													  {
														  if (prefix == null) prefix = "NT$";
		return new non_terminal(prefix + next_nt++);
	}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** static routine for creating a new uniquely named hidden non-terminal */
	public static non_terminal create_new() 
{ 
	return create_new(null); 
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Compute nullability of all non-terminals. */
	public static void compute_nullability() 
{
	bool      change = true;
	non_terminal nt;
	IEnumerator  e;
	production   prod;

	/* repeat this process until there is no change */
	while (change)
{
	/* look for a new change */
	change = false;

	/* consider each non-terminal */
	e=all();
	while ( e.MoveNext())
{
	nt = (non_terminal)e.Current;

	/* only look at things that aren't already marked nullable */
	if (!nt.nullable())
{
	if (nt.looks_nullable())
{
	nt._nullable = true;
	change = true;
}
}
}
}
      
	/* do one last pass over the productions to finalize all of them */
	e=production.all();
	while ( e.MoveNext())
{
	prod = (production)e.Current;
	prod.set_nullable(prod.check_nullable());
}
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Compute first sets for all non-terminals.  This assumes nullability has
	 *  already computed.
	 */
	public static void compute_first_sets() 
{
	bool      change = true;
	IEnumerator  n;
	IEnumerator  p;
	non_terminal nt;
	production   prod;
	terminal_set prod_first;
      

	/* repeat this process until we have no change */
	while (change)
    {    
	    /* look for a new change */
	    change = false;

	    /* consider each non-terminal */
	    n = all();
	    while ( n.MoveNext() )
        {
	        nt = (non_terminal)n.Current;
      
	        /* consider every production of that non terminal */
	        p = nt.productions();
      
       
	        while ( p.MoveNext() )
            {
         
	            prod = (production)p.Current;

	            /* get the updated first of that production */
	            prod_first = prod.check_first_set();
                /* if this going to add anything, add it */
             
                if (!prod_first.is_subset_of(nt._first_set))
                {
                    change = true;
                    nt._first_set.add(prod_first);
                }           
            }
        }
    }     
}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** Table of all productions with this non terminal on the LHS. */
	protected Hashtable _productions = new Hashtable(11);

	/** Access to productions with this non terminal on the LHS. */
	public IEnumerator productions() {return _productions.Values.GetEnumerator();}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Total number of productions with this non terminal on the LHS. */
	public int num_productions() {return _productions.Count;}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Add a production to our Set of productions. */
	public void add_production(production prod)
{
	/* catch improper productions */
	if (prod == null || prod.lhs() == null || prod.lhs().the_symbol() != this)
	throw new internal_error(
	"Attempt to add invalid production to non terminal production table");

	/* add it to the table, keyed with itself */
	_productions.Add(prod,prod);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Nullability of this non terminal. */
	protected bool _nullable;

	/** Nullability of this non terminal. */
	public bool nullable() {return _nullable;}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** First Set for this non-terminal. */
	protected terminal_set _first_set = new terminal_set();

	/** First Set for this non-terminal. */
	public terminal_set first_set() {return _first_set;}

	/*-----------------------------------------------------------*/
	/*--- General Methods ---------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Indicate that this symbol is a non-terminal. */
	 public override bool is_non_term() 
{
	return true;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Test to see if this non terminal currently looks nullable. */
	protected bool looks_nullable() 
{
	/* look and see if any of the productions now look nullable */
	IEnumerator e = productions();
	while ( e.MoveNext() )
	/* if the production can go to empty, we are nullable */
	if (((production)e.Current).check_nullable())
	return true;

	/* none of the productions can go to empty, so we are not nullable */
	return false;
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** convert to string */
	  public override string ToString()
{
	return base.ToString() + "[" + index() + "]" + (nullable() ? "*" : "");
}

	/*-----------------------------------------------------------*/
}
}