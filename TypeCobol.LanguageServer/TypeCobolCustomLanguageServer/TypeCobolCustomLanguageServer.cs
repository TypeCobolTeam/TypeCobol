using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class TypeCobolCustomLanguageServer : VsCodeProtocol.LanguageServer
    {
        public TypeCobolCustomLanguageServer(IRPCServer rpcServer) : base(rpcServer)
        {

        }

        /// <summary>
        /// Missing copies notifications are sent from the server to the client to signal
        /// that some copies where missing during TypeCobol parsing.
        /// </summary>
        public virtual void SendMissingCopies(MissingCopiesParams parameters)
        {
            this.rpcServer.SendNotification(MissingCopiesNotification.Type, parameters);
        }
    }
}
