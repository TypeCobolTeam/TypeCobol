using System;
using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The RemoteWindow interface contains all functions to interact with
    /// the visual window of VS Code.
    /// </summary>
    public class RemoteWindow
    {
        private IRPCServer rpcServer;

        public RemoteWindow(IRPCServer rpcServer)
        {
            this.rpcServer = rpcServer;
        }

        /// <summary>
        /// Show an error message.
        /// 
        /// @param message The message to show.
        /// </summary>
        public void ShowErrorMessage(string message)
        {
            showMessage(MessageType.Error, message);
        }

        /// <summary>
        /// Show an warning message.
        /// 
        /// @param message The message to show.
        /// </summary>
        public void ShowWarningMessage(string message)
        {
            showMessage(MessageType.Warning, message);
        }

        /// <summary>
        /// Show an information message.
        /// 
        /// @param message The message to show.
        /// </summary>
        public void ShowInformationMessage(string message)
        {
            showMessage(MessageType.Info, message);
        }

        private void showMessage(MessageType type, string message)
        {
            rpcServer.SendNotification(ShowMessageNotification.Type, new ShowMessageParams() { MessageType = type, Message = message });
        }
    }
}
