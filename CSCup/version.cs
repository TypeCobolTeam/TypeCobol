

namespace TUVienna.CS_CUP
{

	/** This class contains version and authorship information. 
	 *  It contains only static data elements and basically just a central 
	 *  place to put this kind of information so it can be updated easily
	 *  for each release.  
	 *
	 *  Version numbers used here are broken into 3 parts: major, minor, and 
	 *  update, and are written as v<major>.<minor>.<update> (e.g. v0.10a).  
	 *  Major numbers will change at the time of major reworking of some 
	 *  part of the system.  Minor numbers for each public release or 
	 *  change big enough to cause incompatibilities.  Finally update
	 *  letter will be incremented for small bug fixes and changes that
	 *  probably wouldn't be noticed by a user.  
	 *
	 * @version last updated: 12/22/97 [CSA]
	 * @author  Frank Flannery
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class version 
	{
		/** The major version number. */
		public const int major = 0;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** The minor version number. */
		public const int minor = 1;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** The update letter. */
		public const string update = "";

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** string for the current version. */
		public static readonly string version_str = "v" + major + "." + minor + update;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Full title of the system */
		public static readonly string title_str = "C# CUP " + version_str;

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** Name of the author */
		public const string author_str =
		"Scott E. Hudson, Frank Flannery, and C. Scott Ananian\n translated to C# by Samuel Imriska";

		/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

		/** The command name normally used to invoke this program */ 
		public const string program_name = "TUVienna.CS_CUP";
	}
}