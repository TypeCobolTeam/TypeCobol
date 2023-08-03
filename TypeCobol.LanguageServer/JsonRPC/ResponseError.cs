using System;

namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Error object returned in case a request has failed.
    /// </summary>
    public class ResponseResultOrError
    {
        /// <summary>
        /// Result of the execution of a request
        /// </summary>
        public object result { get; set; }
        
        /// <summary>
        /// A number indicating the error type that occured.
        /// </summary>
        public int? code { get; set; }

        /// <summary>
        /// A string providing a short decription of the error.
        /// </summary>
        public string message { get; set; }

        /// <summary>
        ///  A Primitive or Structured value that contains additional information about the error.
        ///  Can be omitted.
        /// </summary>
        public object data { get; set; }
    }
}
