using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class TypeCobolCustomLanguageServer : TypeCobolServer
    {
        public TypeCobolCustomLanguageServer(IRPCServer rpcServer, Queue<MessageActionWrapper> messagesActionsQueue)
            : base(rpcServer, messagesActionsQueue)
        {
            rpcServer.RegisterNotificationMethod(MissingCopiesNotification.Type, CallReceiveMissingCopies);
            rpcServer.RegisterNotificationMethod(NodeRefreshNotification.Type, ReceivedRefreshNodeDemand);
            rpcServer.RegisterRequestMethod(NodeRefreshRequest.Type, ReceivedRefreshNodeRequest);
            rpcServer.RegisterNotificationMethod(SignatureHelpContextNotification.Type, ReceivedSignatureHelpContext);
            rpcServer.RegisterNotificationMethod(ExtractUseCopiesNotification.Type, ReceivedExtractUseCopiesNotification);
        }

        /// <summary>
        /// Are we using the Outline view in the client.    
        /// </summary>
        public bool UseOutlineRefresh { get; set; }

        protected override void OnShutdown()
        {
            if (UseOutlineRefresh && this.Workspace.DocumentModifiedEvent != null)
                this.Workspace.DocumentModifiedEvent -= DocumentModified;

            base.OnShutdown();
        }

        protected override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            if (UseOutlineRefresh && this.Workspace.DocumentModifiedEvent == null)
            {
                this.Workspace.DocumentModifiedEvent += DocumentModified;
            }
            base.OnDidOpenTextDocument(parameters);
        }

        protected override void OnDidCloseTextDocument(DidCloseTextDocumentParams parameters)
        {
            base.OnDidCloseTextDocument(parameters);
            if (UseOutlineRefresh && this.Workspace.DocumentModifiedEvent != null && this.Workspace.IsEmpty)
                this.Workspace.DocumentModifiedEvent -= DocumentModified;

        }

        protected override void OnDidSaveTextDocument(DidSaveTextDocumentParams parameters)
        {
            base.OnDidSaveTextDocument(parameters);
            if (parameters.text != null && UseOutlineRefresh)
            {
                OnDidReceiveRefreshOutline(parameters.textDocument.uri, true);
            }
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

        private ResponseResultOrError ReceivedRefreshNodeRequest(RequestType requestType, object parameters)
        {
            ResponseResultOrError resultOrError = null;
            try
            {
                OnDidReceiveNodeRefresh((NodeRefreshParams)parameters);
                resultOrError = new ResponseResultOrError() { result = true };
            }
            catch (Exception e)
            {
                NotifyException(e);
                resultOrError = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = e.Message};
            }
            return resultOrError;
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
        /// when the client failed to load copies, it send back a list of missing copies to the server.
        /// </summary>
        protected virtual void OnDidReceiveMissingCopies(MissingCopiesParams parameter)
        {
            this.Workspace.UpdateMissingCopies(new Uri(parameter.textDocument.uri), parameter.Copies);
        }

        /// <summary>
        /// The Node Refresh notification is sent from the client to the server 
        /// It will force the server to do a Node Phase analyze. 
        /// </summary>
        /// <param name="parameter"></param>
        protected virtual void OnDidReceiveNodeRefresh(NodeRefreshParams parameter)
        {
            var context = GetDocumentContextFromStringUri(parameter.textDocument.uri, false);
            if (context != null && context.FileCompiler != null)
            {
                this.Workspace.RefreshSyntaxTree(context.FileCompiler, true);
            }
        }

        private void ReceivedExtractUseCopiesNotification(NotificationType notificationType, object parameters)
        {
            try
            {
                OnDidReceiveExtractUseCopies((ExtractUseCopiesParams)parameters);
            }
            catch (Exception e)
            {
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        protected virtual void OnDidReceiveExtractUseCopies(ExtractUseCopiesParams parameter)
        {
            var docContext = GetDocumentContextFromStringUri(parameter.textDocument.uri, false);
            if (docContext?.FileCompiler?.CompilationResultsForProgram?.CopyTextNamesVariations != null)
            {
                var _customSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(this.Workspace.Configuration.Copies, this.Workspace.Configuration.Format, null); //Refresh Intrinsics
                IEnumerable<string> dependenciesMissingCopies = Tools.APIHelpers.Helpers.GetDependenciesMissingCopies(this.Workspace.Configuration, _customSymbols, null);

                List<string> copiesName = docContext.FileCompiler.CompilationResultsForProgram.CopyTextNamesVariations.Select(cp => cp.TextNameWithSuffix).Distinct().ToList();
                copiesName.AddRange(dependenciesMissingCopies);
                if (copiesName.Count > 0)
                {
                    var missingCopiesParam = new MissingCopiesParams();
                    missingCopiesParam.textDocument = parameter.textDocument;
                    missingCopiesParam.Copies = copiesName;
                    this.RpcServer.SendNotification(MissingCopiesNotification.Type, missingCopiesParam);
                }
            }
        }

        protected virtual void OnDidReceiveSignatureHelpContext(SignatureHelpContextParams parameters)
        {
            if (parameters?.signatureInformation == null) //Means that the client leave the context
            {
                //Make the context signature completion null
                this.SignatureCompletionContext = null;
                //Clean up the dictionary
                this.FunctionDeclarations.Clear();
                return;
            }

            var retrievedFuncDeclarationPair = this.FunctionDeclarations.FirstOrDefault(item => item.Key.Equals(parameters.signatureInformation));

            if (retrievedFuncDeclarationPair.Key != null)
                this.SignatureCompletionContext = retrievedFuncDeclarationPair.Value;
        }

        /// <summary>
        /// The refresh Outline notification is received for each document modification 
        /// It will update the main OutlineNode with the new information. 
        /// </summary>
        /// <param name="uri"></param>
        /// <param name="bForced">Force the server to send the program OutlineNodes</param>
        protected virtual void OnDidReceiveRefreshOutline(string uri, bool bForced)
        {
            var context = GetDocumentContextFromStringUri(uri, false);

            if (context != null && context.FileCompiler != null)
            {
                var refreshOutlineParams = context.LanguageServer.UpdateOutline(context.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot, bForced);
                if (refreshOutlineParams != null)
                {
                    SendOutlineData(refreshOutlineParams);
                }
            }

        }

        public void DocumentModified(object sender, EventArgs args)
        {
            OnDidReceiveRefreshOutline(sender.ToString(), false);
        }

        /// <summary>
        /// Outline data notification are sent from the server to the client to send data when changing focused document.
        /// </summary>
        public virtual void SendOutlineData(RefreshOutlineParams parameters)
        {
            this.RpcServer.SendNotification(RefreshOutlineNotification.Type, parameters);
        }
    }
}
