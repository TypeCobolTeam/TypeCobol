using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.CupParser
{
    /// <summary>
    /// An interface for a Cup Parser Error Reporter.
    /// </summary>
    public interface ICupParserErrorReporter
    {
        /// <summary>
        /// Report a fatal error.  This method takes a  message string and an 
        /// additional object (to be used by specializations implemented in subclasses).  
        /// </summary>
        /// <param name="parser">the underlying parser.</param>
        /// <param name="message">message an error message.</param>
        /// <param name="info">an extra object reserved for use by specialized subclasses.</param>
        /// <returns>true if the error has been handle, false otherwise.
        /// if false is returned then a very simple implementation 
        /// is provided which reports the error then throws an exception. 
        /// </returns>
        bool ReportFatalError(TUVienna.CS_CUP.Runtime.lr_parser parser, string message, object info);

        /// <summary>
        /// Report a non fatal error (or warning).  This method takes a message 
        /// string and an additional object (to be used by specializations 
        /// implemented in subclasses). 
        /// </summary>
        /// <param name="parser">the underlying parser.</param>
        /// <param name="message">an error message.</param>
        /// <param name="info">an extra object reserved for use by specialized subclasses, by default the  the current lookahead Symbol.</param>
        /// <returns>true if the error has been handle, false otherwise.
        /// if false is returned then a very simple 
        /// implementation is provided which simply prints the message to System.err. 
        /// </returns>
        bool ReportError(TUVienna.CS_CUP.Runtime.lr_parser parser, string message, object info);

        /// <summary>
        /// This method is called when a syntax error has been detected and recovery 
        /// is about to be invoked.  Here in the base class we just emit a 
        /// "Syntax error" error message.  
        /// </summary>
        /// <param name="parser">the underlying parser.</param>
        /// <param name="curToken">the current lookahead Symbol.</param>
        /// <returns>true if the error has been handle, false otherwise.
        /// if false is returned then we just emit a "Syntax error" error message.  
        /// </returns>
        bool SyntaxError(TUVienna.CS_CUP.Runtime.lr_parser parser, TUVienna.CS_CUP.Runtime.Symbol curToken);

        /// <summary>
        /// This method is called if it is determined that syntax error recovery 
        /// has been unsuccessful. 
        /// </summary>
        /// <param name="parser">the underlying parser.</param>
        /// <param name="curToken">the current lookahead Symbol.</param>
        /// <returns>true if the error has been handle, false otherwise.
        /// if false is returned then we report a fatal error.   
        /// </returns>
        bool UnrecoveredSyntaxError(TUVienna.CS_CUP.Runtime.lr_parser parser, TUVienna.CS_CUP.Runtime.Symbol curToken);
    }
}
