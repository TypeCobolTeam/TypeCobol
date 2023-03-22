using System;

namespace TypeCobol.LanguageServer.StdioHttp
{
    /// <summary>
    /// Interface for a class which can handle messages
    /// </summary>
    public interface IMessageHandler
    {
        /// <summary>
        /// Do something useful with the message received, use the server interface to reply
        /// </summary>
        void HandleMessage(string message, IMessageServer server, LSPProfiling lspProfiling);
    }
}
