using System;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using String = System.String;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class TypeCobolCustomLanguageServer : VsCodeProtocol.LanguageServer
    {
        public TypeCobolCustomLanguageServer(IRPCServer rpcServer) : base(rpcServer)
        {
            rpcServer.RegisterNotificationMethod(MissingCopiesNotification.Type, CallReceiveMissingCopies);
            rpcServer.RegisterNotificationMethod(NodeRefreshNotification.Type, ReceivedRefreshNodeDemand);
            rpcServer.RegisterNotificationMethod(SignatureHelpContextNotification.Type, ReceivedSignatureHelpContext);
        }

        private void CallReceiveMissingCopies(NotificationType notificationType, object parameters)
        {
            try
            {
                OnDidReceiveMissingCopies((MissingCopiesParams)parameters);
            }
            catch (Exception e)
            {
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        private void ReceivedRefreshNodeDemand(NotificationType notificationType, object parameters)
        {
            try
            {
                OnDidReceiveNodeRefresh((NodeRefreshParams) parameters);
            }
            catch (Exception e)
            {
                this.NotifyException(e);
            }
        }

        private void ReceivedSignatureHelpContext(NotificationType notificationType, object parameters)
        {
            try
            {
                OnDidReceiveSignatureHelpContext((SignatureHelpContextParams)parameters);
            }
            catch (Exception e)
            {
                this.NotifyException(e);
            }
        }

        /// <summary>
        /// The Missing copies notification is sent from the client to the server
        /// when the client failled to load copies, it send back a list of missing copies to the server.
        /// </summary>
        public virtual void OnDidReceiveMissingCopies(MissingCopiesParams parameter)
        {
            //Nothing to do for now, maybe add some telemetry here...
        }

        /// <summary>
        /// The Node Refresh notification is sent from the client to the server 
        /// It will force the server to do a Node Phase analyze. 
        /// </summary>
        /// <param name="parameter"></param>
        public virtual void OnDidReceiveNodeRefresh(NodeRefreshParams parameter)
        {
            
        }

        public virtual void OnDidReceiveSignatureHelpContext(SignatureHelpContextParams procedureHash)
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

        /// <summary>
        /// Loading Issue notification is sent from the server to the client
        /// Usefull to let the client knows that a error occured while trying to load Intrinsic/Dependencies. 
        /// </summary>
        public virtual void SendLoadingIssue(LoadingIssueParams parameters)
        {
            this.rpcServer.SendNotification(LoadingIssueNotification.Type, parameters);
        }
    }
}
