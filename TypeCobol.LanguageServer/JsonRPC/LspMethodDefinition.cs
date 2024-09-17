namespace TypeCobol.LanguageServer.JsonRPC
{
    public enum MethodKind
    {
        NotificationMethod,
        RequestMethod
    }

    public abstract class LspMethodDefinition
    {
        public string Method { get; }

        protected LspMethodDefinition(string method)
        {
            Method = method;
        }

        public abstract MethodKind Kind { get; }

        public abstract IEnumerable<Type> Types { get; }
    }
}
