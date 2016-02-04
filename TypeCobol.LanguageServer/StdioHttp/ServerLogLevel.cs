using System;

namespace TypeCobol.LanguageServer.StdioHttp
{
    /// <summary>
    /// Verbosity of the server traces
    /// </summary>
    public enum ServerLogLevel
    {
        /// <summary>
        /// Traces only important server lifecycle events (startup / shutdown)
        /// </summary>
        Lifecycle,
        /// <summary>
        /// Traces timestamp + content length for each message received and sent
        /// </summary>
        Message,
        /// <summary>
        /// Traces the complete text content of each message received and sent
        /// </summary>
        Protocol
    }
}
