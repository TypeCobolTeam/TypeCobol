namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Description of a notification RPC
    /// </summary>
    public class NotificationType : LspMethodDefinition
    {
        /// <summary>
        /// Type for the method parameters 
        /// </summary>
        public Type ParamsType { get; }

        public NotificationType(string method, Type paramsType)
            : base(method)
        {
            this.ParamsType = paramsType;
        }

        public override MethodKind Kind => MethodKind.NotificationMethod;

        public override IEnumerable<Type> Types
        {
            get
            {
                yield return ParamsType;
            }
        }
    }
}
