#if EUROINFO_RULES
using TypeCobol.CustomExceptions;
#endif
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
#if EUROINFO_RULES
using TypeCobol.Tools.Options_Config;
#endif

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class TypeCobolCustomLanguageServer : TypeCobolServer
    {
        public TypeCobolCustomLanguageServer(IRPCServer rpcServer, System.Collections.Concurrent.ConcurrentQueue<MessageActionWrapper> messagesActionsQueue)
            : base(rpcServer, messagesActionsQueue)
        {
            rpcServer.RegisterNotificationMethod(MissingCopiesNotification.Type, CallReceiveMissingCopies);
            rpcServer.RegisterNotificationMethod(NodeRefreshNotification.Type, ReceivedRefreshNodeDemand);
            rpcServer.RegisterRequestMethod(NodeRefreshRequest.Type, ReceivedRefreshNodeRequest);
            rpcServer.RegisterNotificationMethod(SignatureHelpContextNotification.Type, ReceivedSignatureHelpContext);
            rpcServer.RegisterNotificationMethod(ExtractUseCopiesNotification.Type, ReceivedExtractUseCopiesNotification);
            rpcServer.RegisterNotificationMethod(DidOpenProjectTextDocumentNotification.Type, ReceivedDidOpenProjectTextDocument);
            rpcServer.RegisterNotificationMethod(DidChangeProjectConfigurationNotification.Type, ReceivedDidChangeProjectConfiguration);
            rpcServer.RegisterRequestMethod(GetDataLayoutRequest.Type, ReceivedGetDataLayoutRequest);
#if EUROINFO_RULES
            rpcServer.RegisterRequestMethod(ExtractRemarksDataRequest.Type, ReceivedExtractRemarksDataRequest);
#endif
        }

        /// <summary>
        /// Are we using the Outline view in the client.    
        /// </summary>
        public bool UseOutlineRefresh { get; set; }

        /// <summary>
        /// Use Cfg Mode
        /// </summary>
        public enum UseCfgMode
        {
            No,         // No Cfg information
            AsFile,     // Dot content is emitted as file
            AsContent   // Dot content is emitted as as a String content.
        };

        /// <summary>
        /// Are we using the CFG view in the client.
        /// </summary>
        public UseCfgMode UseCfgDfaDataRefresh { get; set; }
        protected override InitializeResult OnInitialize(InitializeParams parameters)
        {
            var result = base.OnInitialize(parameters);
            this.Workspace.DocumentModifiedEvent += DocumentModified;
            this.Workspace.UseCfgDfaDataRefresh = UseCfgDfaDataRefresh != UseCfgMode.No;
            return result;
        }

        /// <summary>
        /// Handle -ol option from the client configuration change notification.
        /// </summary>
        /// <param name="options">Client's Options</param>
        protected override void OnDidChangeConfiguration(string[] options)
        {
            this.UseOutlineRefresh = !options.Contains("-dol");
            base.OnDidChangeConfiguration(options);
        }

        protected override void OnShutdown()
        {
            this.Workspace.DocumentModifiedEvent -= DocumentModified;
            base.OnShutdown();
        }


        public static readonly TimeSpan MaxQueueWaitForDidSave = TimeSpan.FromSeconds(2);

        protected override void OnDidSaveTextDocument(DidSaveTextDocumentParams parameters, LSPProfiling lspProfiling)
        {
            //Too much server lag: skip this notification. The only drawbacks is that diagnostics might not be totally accurate are we still have some incremental bugs
            if (lspProfiling.InQueueDuration > MaxQueueWaitForDidSave)
            {
                return;
            }
            base.OnDidSaveTextDocument(parameters, lspProfiling);
            if (parameters.text != null && UseOutlineRefresh)
            {
                OnDidReceiveRefreshOutline(parameters.textDocument.uri, true);
            }
        }

        private void CallReceiveMissingCopies(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            try
            {
                OnDidReceiveMissingCopies((MissingCopiesParams)parameters);
            }
            catch (Exception e)
            {
                NotifyException(e);
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        private void ReceivedRefreshNodeDemand(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            try
            {
                OnDidReceiveNodeRefresh((NodeRefreshParams)parameters);
            }
            catch (Exception e)
            {
                this.NotifyException(e);
            }
        }

        private ResponseResultOrError ReceivedRefreshNodeRequest(RequestType requestType, object parameters, LSPProfiling lspProfiling)
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
                resultOrError = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = e.Message };
            }
            return resultOrError;
        }

        private ResponseResultOrError ReceivedGetDataLayoutRequest(RequestType requestType, object parameters, LSPProfiling lspProfiling)
        {
            ResponseResultOrError resultOrError;
            try
            {
                var dataLayout = OnDidReceiveGetDataLayout((GetDataLayoutParams)parameters);
                resultOrError = new ResponseResultOrError() { result = dataLayout };
            }
            catch (Exception e)
            {
                NotifyException(e);
                resultOrError = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = e.Message };
            }
            return resultOrError;
        }

#if EUROINFO_RULES
        private ResponseResultOrError ReceivedExtractRemarksDataRequest(RequestType requestType, object parameters, LSPProfiling lspProfiling)
        {
            ResponseResultOrError resultOrError;
            try
            {
                var remarksData = OnDidReceiveExtractRemarksData((ExtractRemarksDataParams)parameters);
                resultOrError = new ResponseResultOrError() { result = remarksData };
            }
            catch (MissingCopyException missingCopyException)
            {
                //Called while COPYs were not all downloaded
                resultOrError = new ResponseResultOrError() { code = (int)ReturnCode.MissingCopy, message = missingCopyException.Message };
            }
            catch (Exception exception)
            {
                //Unexpected
                NotifyException(exception);
                resultOrError = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = exception.Message };
            }
            return resultOrError;
        }
#endif

        private void ReceivedSignatureHelpContext(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
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
        /// when the client has finished to load copies, it send back a list of missing copies that it fails to load to the server.
        /// The list can be empty if all copies has been loaded.
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
            var context = GetDocumentContextFromStringUri(parameter.textDocument.uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh);
            if (context != null && context.FileCompiler != null)
            {
                this.Workspace.RefreshSyntaxTree(context.FileCompiler, Workspace.SyntaxTreeRefreshLevel.ForceFullRefresh);
            }
        }

        /// <summary>
        /// The GetDataLayout request is sent from the client to the server
        /// It asks to the server to return the data layout of the document
        /// </summary>
        /// <param name="parameter"></param>
        protected virtual GetDataLayoutResult OnDidReceiveGetDataLayout(GetDataLayoutParams parameter)
        {
            string uri = parameter.textDocumentPosition.textDocument.uri;

            var context = GetDocumentContextFromStringUri(uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            if (context != null && context.FileCompiler != null)
            {
                // We only support CSV for now. In the future outputType should be checked here
                var position = parameter.textDocumentPosition.position;
                const string SEPARATOR = GetDataLayoutCSVResult.SEPARATOR;
                var csv = this.Workspace.GetDataLayoutAsCSV(context.FileCompiler.CompilationResultsForProgram, position, SEPARATOR);
                var csvResult = new GetDataLayoutCSVResult()
                {
                    header = csv.Header,
                    rows = csv.Rows,
                    separator = SEPARATOR
                };
                return new GetDataLayoutResult() { csvResult = csvResult };
            }

            throw new Exception($"Unknown document: '{uri}'");
        }

#if EUROINFO_RULES
        protected virtual RemarksData OnDidReceiveExtractRemarksData(ExtractRemarksDataParams parameter)
        {
            string uri = parameter.textDocument.uri;
            var context = GetDocumentContextFromStringUri(uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh);
            if (context != null && context.FileCompiler != null)
            {
                var compilationResults = context.FileCompiler.CompilationResultsForProgram;
                if (compilationResults.MissingCopies.Any())
                {
                    throw new MissingCopyException("Cannot find used copy before all copys are resolved", uri) { Logged = false };
                }

                var data = this.Workspace.GetRemarksData(compilationResults);
                return new RemarksData()
                       {
                           usedCPYs = data.Item1,
                           insertionLine = data.Item2
                       };
            }

            throw new Exception($"Unknown document: '{parameter.textDocument.uri}'");
        }
#endif

        private void ReceivedExtractUseCopiesNotification(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            try
            {
                OnDidReceiveExtractUseCopies((ExtractUseCopiesParams)parameters);
            }
            catch (Exception e)
            {
                NotifyException(e);
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        /// <summary>
        /// Receive a DidOpenProjectTextDocument notification from the client.
        /// </summary>
        /// <param name="notificationType"></param>
        /// <param name="parameters"></param>
        private void ReceivedDidOpenProjectTextDocument(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            DidOpenProjectTextDocumentParams didOpenParams = (DidOpenProjectTextDocumentParams)parameters;            
            try
            {
                // Open the document in the project whose key is given.
                OpenTextDocument(didOpenParams, didOpenParams.ProjectKey, didOpenParams.CopyFolders);
            }
            catch (Exception e)
            {
                NotifyException(e);
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        /// <summary>
        /// Handle Project Configuration Changed notification from the client
        /// </summary>
        /// <param name="notificationType">Notification's type</param>
        /// <param name="parameters">Notification's parameters</param>
        private void ReceivedDidChangeProjectConfiguration(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            DidChangeProjectConfigurationParams docChangeConfParams = (DidChangeProjectConfigurationParams)parameters;
            try
            {
                this.Workspace.UpdateWorkspaceProjectConfiguration(docChangeConfParams.ProjectKey, docChangeConfParams.CopyFolders);
            }
            catch (Exception e)
            {
                NotifyException(e);
                RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", notificationType.Method, e.Message));
            }
        }

        protected virtual void OnDidReceiveExtractUseCopies(ExtractUseCopiesParams parameter)
        {
            var docContext = GetDocumentContextFromStringUri(parameter.textDocument.uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh);
            if (docContext?.FileCompiler?.CompilationResultsForProgram?.CopyTextNamesVariations != null)
            {
                var intrinsicTable = this.Workspace.CustomSymbols?.EnclosingScope;
                IEnumerable<string> dependenciesMissingCopies = Tools.APIHelpers.Helpers.GetDependenciesMissingCopies(this.Workspace.Configuration, intrinsicTable);

                List<string> copiesName = docContext.FileCompiler.CompilationResultsForProgram.CopyTextNamesVariations.Select(cp => cp.TextNameWithSuffix).Distinct().ToList();
                copiesName.AddRange(dependenciesMissingCopies);
                MissingCopiesDetected(parameter.textDocument, copiesName);
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
            if (this.UseOutlineRefresh)
            {
                var context = GetDocumentContextFromStringUri(uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh);

                if (context != null && context.FileCompiler != null)
                {
                    var refreshOutlineParams = context.LanguageServer.UpdateOutline(context.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot, bForced);
                    if (refreshOutlineParams != null)
                    {
                        SendOutlineData(refreshOutlineParams);
                    }
                }
            }
        }

        /// <summary>
        /// Update Cfg/Dfa information to the client.
        /// </summary>
        /// <param name="uri">Uri of the document to update Cfg/Dfa Informations</param>
        private void UpdateCfgDfaInformation(string uri)
        {
            if (this.UseCfgDfaDataRefresh != UseCfgMode.No)
            {
                var context = GetDocumentContextFromStringUri(uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh);
                if (context != null && context.FileCompiler != null)
                {
                    var cfgDfaParams = context.LanguageServer.UpdateCfgDfaInformation(context, this.UseCfgDfaDataRefresh == UseCfgMode.AsFile);
                    if (cfgDfaParams != null)
                    {
                        SendCfgDfaData(cfgDfaParams);
                    }
                }
            }
        }

        public void DocumentModified(object sender, EventArgs args)
        {
            string uri = sender.ToString();
            OnDidReceiveRefreshOutline(uri, false);
            UpdateCfgDfaInformation(uri);
        }

        /// <summary>
        /// Outline data notification are sent from the server to the client to send data when changing focused document.
        /// </summary>
        public virtual void SendOutlineData(RefreshOutlineParams parameters)
        {
            this.RpcServer.SendNotification(RefreshOutlineNotification.Type, parameters);
        }

        /// <summary>
        /// CfgDfa data notification are sent from the server to the client.
        /// </summary>
        public virtual void SendCfgDfaData(CfgDfaParams parameters)
        {
            this.RpcServer.SendNotification(CfgDfaNotification.Type, parameters);
        }
    }
}
