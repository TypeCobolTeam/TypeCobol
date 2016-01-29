using System;

namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Description of a request RPC
    /// </summary>
    public class RequestType
    {
        /// <summary>
        /// Method name
        /// </summary>
        public string Method { get; private set; }

        /// <summary>
        /// Type for the method parameters 
        /// </summary>
        public Type ParamsType { get; private set; }

        /// <summary>
        /// Type for the method result 
        /// </summary>
        public Type ResultType { get; private set; }

        /// <summary>
        /// Type for the method error data
        /// </summary>
        public Type ErrorDataType { get; private set; }

        public RequestType(string method, Type paramsType, Type resultType, Type errorDataType)
        {
            this.Method = method;
            this.ParamsType = paramsType;
            this.ResultType = resultType;
            this.ErrorDataType = errorDataType;
        }
    }
}
