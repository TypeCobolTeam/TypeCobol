using System;

namespace TypeCobol.LanguageServer.StdioHttp
{
    /// <summary>
    /// Interface for a class which can send messages
    /// </summary>
    public interface IMessageServer
    {
        /// <summary>
        /// Send a message to the client
        /// </summary>
        void SendMessage(string message);

        /// <summary>
        /// Write a trace in the server log file
        /// </summary>
        void WriteServerLog(string trace);
    }
}
