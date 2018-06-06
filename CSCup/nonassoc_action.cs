
namespace TUVienna.CS_CUP
{

	/** This class represents a shift/reduce nonassociative error within the 
	 *  parse table.  If action_table element is assign to type
	 *  nonassoc_action, it cannot be changed, and signifies that there 
	 *  is a conflict between shifting and reducing a production and a
	 *  terminal that shouldn't be next to each other.
	 *
	 * @version last updated: 7/2/96
	 * @author  Frank Flannery
     * translated to C# 08.09.2003 by Samuel Imriska
	 */
	public class nonassoc_action : parse_action 
	{
 
		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Simple constructor. 
		 */
		public nonassoc_action() 
								 {
									 /* don't need to Set anything, since it signifies error */
								 }

		/*-----------------------------------------------------------*/
		/*--- General Methods ---------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Quick access to type of action. */
		  public override int kind() {return NONASSOC;}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Equality test. */
		 new  public  bool Equals(parse_action other)
		{
			return other != null && other.kind() == NONASSOC;
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Generic equality test. */
		 public override bool Equals(object other)
		{
			if (other.GetType()==typeof(parse_action))
												 return Equals((parse_action)other);
			else
				return false;
		}

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Compute a hash code. */
		public override  int GetHashCode()
		{
			/* all objects of this class hash together */
			return 0xCafe321;
		}



		/** Convert to string. */
		  public override  string ToString() 
		{
			return "NONASSOC";
		}

		/*-----------------------------------------------------------*/

	}
}