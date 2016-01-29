using System;

namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Description of a notification RPC
    /// </summary>
    public class NotificationType
    {
        /// <summary>
        /// Method name
        /// </summary>
        public string Method { get; private set; }
        
        /// <summary>
        /// Type for the method parameters 
        /// </summary>
        public Type ParamsType { get; private set; }

        public NotificationType(string method, Type paramsType)
        {
            this.Method = method;
            this.ParamsType = paramsType;
        }
    }
}
