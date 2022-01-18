namespace TypeCobol.Logging
{
    public static partial class LoggingSystem
    {
        /// <summary>
        /// Regroups commonly used keys for contextual data
        /// </summary>
        public static class ContextKeys
        {
            /// <summary>
            /// Name of the source being parsed
            /// </summary>
            public static readonly string TextSourceName = "textSourceName";
        }
    }
}
