using System;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// Custom TypeCobol  Remote Console
    /// </summary>
    public class TypeCobolRemoteConsole : RemoteConsole
    {
        public TypeCobolRemoteConsole(IRPCServer rpcServer) : base(rpcServer)
        {
        }

        /// <summary>
        /// Custom TypeCobol Log a message.
        /// 
        /// @param message The message to log.
        /// </summary>
        public void UriLog(string message, Uri objUri)
        {
            rpcServer.SendNotification(UriLogMessageNotification.Type, new UriLogMessageParams() { type = MessageType.Log, message = message, textDocument = new TextDocumentIdentifier(objUri.ToString()) });
        }

    }
}
