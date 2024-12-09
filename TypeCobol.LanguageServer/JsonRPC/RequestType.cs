namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Description of a request RPC
    /// </summary>
    public class RequestType : LspMethodDefinition
    {
        /// <summary>
        /// Type for the method parameters 
        /// </summary>
        public Type ParamsType { get; }

        /// <summary>
        /// Type for the method result 
        /// </summary>
        public Type ResultType { get; }

        /// <summary>
        /// Type for the method error data
        /// </summary>
        public Type ErrorDataType { get; }

        public RequestType(string method, Type paramsType, Type resultType, Type errorDataType)
            : base(method)
        {
            this.ParamsType = paramsType;
            this.ResultType = resultType;
            this.ErrorDataType = errorDataType;
        }

        public override MethodKind Kind => MethodKind.RequestMethod;

        public override IEnumerable<Type> Types
        {
            get
            {
                yield return ParamsType;
                yield return ResultType;
            }
        }
    }
}
