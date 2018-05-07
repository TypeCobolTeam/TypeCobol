namespace TUVienna.CS_CUP
{

	/** This class represents a part of a production which is a symbol (terminal
	 *  or non terminal).  This simply maintains a reference to the symbol in 
	 *  question.
	 *
	 * @see     java_cup.production
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 */
	public class symbol_part : production_part 
	{

		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Full constructor. 
		 * @param sym the symbol that this part is made up of.
		 * @param lab an optional label string for the part.
		 */
		public symbol_part(symbol sym, string lab) : base(lab)
												   {

		if (sym == null)
		throw new internal_error(
				  "Attempt to construct a symbol_part with a null symbol");
		_the_symbol = sym;
	}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Constructor with no label. 
	 * @param sym the symbol that this part is made up of.
	 */
        public symbol_part(symbol sym): this( sym,null){}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/

	/** The symbol that this part is made up of. */
	protected symbol _the_symbol;

	/** The symbol that this part is made up of. */
	public symbol the_symbol() {return _the_symbol;}

	/*-----------------------------------------------------------*/
	/*--- General Methods ---------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Respond that we are not an action part. */
	 public override bool is_action() { return false; }

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Equality comparison. */
	public bool Equals(symbol_part other)
{
	return other != null && base.Equals(other) && 
	the_symbol().Equals(other.the_symbol());
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Generic equality comparison. */
	 public override bool Equals(object other)
{
	if (other.GetType()!=typeof(symbol_part))
	return false;
	else
	return Equals((symbol_part)other);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Produce a hash code. */
	 public override int GetHashCode()
{
	return base.GetHashCode() ^ 
	(the_symbol()==null ? 0 : the_symbol().GetHashCode());
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Convert to a string. */
	  public  override string ToString()
{
	if (the_symbol() != null)
	return base.ToString() + the_symbol();
	else
	return base.ToString() + "$$MISSING-SYMBOL$$";
}

	/*-----------------------------------------------------------*/

}
}