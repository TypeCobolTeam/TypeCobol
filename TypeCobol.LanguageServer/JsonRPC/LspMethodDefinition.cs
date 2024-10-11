namespace TypeCobol.LanguageServer.JsonRPC
{
    public enum MethodKind
    {
        NotificationMethod,
        RequestMethod
    }

    /// <summary>
    /// Definition of a LSP method.
    /// Base class for NotificationType and RequestType
    /// </summary>
    public abstract class LspMethodDefinition
    {
        /// <summary>
        /// Method name
        /// </summary>
        public string Method { get; }

        protected LspMethodDefinition(string method)
        {
            Method = method;
        }

        /// <summary>
        /// Method kind
        /// </summary>
        public abstract MethodKind Kind { get; }

        /// <summary>
        /// Types used in messages exchanged when this method is called
        /// </summary>
        public abstract IEnumerable<Type> Types { get; }
    }
}
