
namespace TUVienna.CS_CUP
{


	/* Defines integers that represent the associativity of terminals
	 * @version last updated: 7/3/96
	 * @author  Frank Flannery
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class assoc 
	{

		/* various associativities, no_prec being the default value */
		public const int left = 0;
		public const int right = 1;
		public const int nonassoc = 2;
		public const int no_prec = -1;

	}
}