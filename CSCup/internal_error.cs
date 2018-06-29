

namespace TUVienna.CS_CUP
{
		using System;



	/** Exception subclass for reporting internal errors in JavaCup. */
	public class internal_error : System.Exception
	{
									/** Constructor with a message */
		public internal_error(string msg):base(msg) {;}
	/** Method called to do a forced error exit on an internal error
	for cases when we can't actually throw the exception.  */
	public void crash()
{
	Console.WriteLine("JavaCUP Fatal Internal Error Detected");
	Console.WriteLine(Message);
	System.Environment.Exit(-1);
}
}
}