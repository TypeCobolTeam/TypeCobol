using System;
using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The RemoteConsole interface contains all functions to interact with
    /// the developer console of VS Code.
    /// </summary>
    public class RemoteConsole
    {
        protected IRPCServer rpcServer;

        public RemoteConsole(IRPCServer rpcServer)
        {
            this.rpcServer = rpcServer;
        }

        /// <summary>
        /// Show an error message.
        /// 
        /// @param message The message to show.
        /// </summary>
        public void Error(string message)
        {
            send(MessageType.Error, message);
        }

        /// <summary>
        /// Show an warning message.
        /// 
        /// @param message The message to show.
        /// </summary>
        public void Warn(string message)
        {
            send(MessageType.Warning, message);
        }

        /// <summary>
        /// Are Log message notifications enabled ? false if yes, true otherwise.
        /// </summary>
        public bool NoLogsMessageNotification { get; set; }

        /// <summary>
        /// Log a message.
        /// 
        /// @param message The message to log.
        /// </summary>
        public void Log(string message)
        {
            if (!NoLogsMessageNotification)
            {
                send(MessageType.Log, message);
            }
        }

        private void send(MessageType type, string message)
        {
            rpcServer.SendNotification(LogMessageNotification.Type, new LogMessageParams() { MessageType = type, Message = message });
        }
    }   
}
