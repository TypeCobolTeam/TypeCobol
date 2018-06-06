

namespace TUVienna.CS_CUP
{


	/** A specialized version of a production used when we split an existing
	 *  production in order to remove an embedded action.  Here we keep a bit 
	 *  of extra bookkeeping so that we know where we came from.
	 * @version last updated: 11/25/95
	 * @author  Scott Hudson
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class action_production : production 
								   {

									   /** Constructor.
										* @param base       the production we are being factored out of.
										* @param lhs_sym    the LHS symbol for this production.
										* @param rhs_parts  array of production parts for the RHS.
										* @param rhs_len    how much of the rhs_parts array is valid.
										* @param action_str the trailing reduce action for this production.
										*/ 
	   public action_production(
		production      cbase,
		non_terminal    lhs_sym, 
			production_part[] rhs_parts,
		int             rhs_len,
			string          action_str):base(lhs_sym, rhs_parts, rhs_len, action_str)
		
		{
		_base_production = cbase;
	}
	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** The production we were taken out of. */
	protected production _base_production;

	/** The production we were taken out of. */
	public production base_production() {return _base_production;}
}
}