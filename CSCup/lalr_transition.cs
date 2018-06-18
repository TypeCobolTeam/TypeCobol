namespace TUVienna.CS_CUP
{

	/** This class represents a transition in an LALR viable prefix recognition 
	 *  machine.  Transitions can be under terminals for non-terminals.  They are
	 *  internally linked together into singly linked lists containing all the 
	 *  transitions out of a single state via the _next field.
	 *
	 * @see     java_cup.lalr_state
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 *
	 */
	public class lalr_transition 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Full constructor.
		 * @param on_sym  symbol we are transitioning on.
		 * @param to_st   state we transition to.
		 * @param nxt     next transition in linked list.
		 */
		public lalr_transition(symbol on_sym, lalr_state to_st, lalr_transition nxt)
		
			{
				/* sanity checks */
				if (on_sym == null)
		throw new internal_error("Attempt to create transition on null symbol");
		if (to_st == null)
		throw new internal_error("Attempt to create transition to null state");

		/* initialize */
		_on_symbol = on_sym;
		_to_state  = to_st;
		_next      = nxt;
	}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Constructor with null next. 
	 * @param on_sym  symbol we are transitioning on.
	 * @param to_st   state we transition to.
	 */
	public lalr_transition(symbol on_sym, lalr_state to_st):this(on_sym,to_st,null)
{
//	this(on_sym, to_st, null);
}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** The symbol we make the transition on. */
	protected symbol _on_symbol;

	/** The symbol we make the transition on. */
	public symbol on_symbol() {return _on_symbol;}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** The state we transition to. */
	protected lalr_state _to_state;

	/** The state we transition to. */
	public lalr_state to_state() {return _to_state;}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Next transition in linked list of transitions out of a state */
	protected lalr_transition _next;

	/** Next transition in linked list of transitions out of a state */
	public lalr_transition next() {return _next;}

	/*-----------------------------------------------------------*/
	/*--- General Methods ---------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Convert to a string. */
	public override string ToString()
{
	string result;

	result = "transition on " + on_symbol().name() + " to state [";
	result += _to_state.index();
	result += "]";

	return result;
}

	/*-----------------------------------------------------------*/
}
}