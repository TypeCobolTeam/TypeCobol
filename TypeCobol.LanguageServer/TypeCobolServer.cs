using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.LanguageServer.Context;
using TypeCobol.LanguageServer.SignatureHelper;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.Tools;

using Range = TypeCobol.LanguageServer.VsCodeProtocol.Range;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : VsCodeProtocol.LanguageServer
    {
        private readonly System.Collections.Concurrent.ConcurrentQueue<MessageActionWrapper> _messagesActionsQueue;
        protected Dictionary<SignatureInformation, FunctionDeclaration> FunctionDeclarations { get; }

        public TypeCobolServer(IRPCServer rpcServer, System.Collections.Concurrent.ConcurrentQueue<MessageActionWrapper> messagesActionsQueue)
            : base(rpcServer)
        {
            this._messagesActionsQueue = messagesActionsQueue;
            this.FunctionDeclarations = new Dictionary<SignatureInformation, FunctionDeclaration>();
        }

        protected Workspace Workspace { get; private set; }

        protected FunctionDeclaration SignatureCompletionContext { get; set; }

        /// <summary>
        /// Are Log message notifications enabled ? false if yes, true otherwise.
        /// </summary>
        public bool NoLogsMessageNotification { get; set; }

        /// <summary>
        /// Lsr testing level (Source, Scan, Preprocess, Parse, Check, CodeAnalysis)
        /// </summary>
        public LsrTestingOptions LsrTestingLevel { get; set; }

        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        public bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// True to use ANTLR for parsing a program
        /// </summary>
        public bool UseAntlrProgramParsing { get; set; }

#if EUROINFO_RULES
        /// <summary>
        /// The Cpy Copy names file
        /// </summary>
        public string CpyCopyNamesMapFilePath { get; set; }
#endif
        /// <summary>
        /// Timer Disabled for TypeCobol.LanguageServer.
        /// </summary>
        public bool TimerDisabledOption { get; set; }

        /// <summary>
        /// Extension manager
        /// </summary>
        internal ExtensionManager ExtensionManager { get; set; }

        /// <summary>
        /// No Copy and Dependency files watchers.
        /// </summary>
        public bool NoCopyDependencyWatchers { get; set; }

        private bool Logger(string message, Uri uri)
        {
            if (uri == null)
            {
                RemoteConsole.Log(message);
            }
            else
            {
                var uriLogMessageParams = new UriLogMessageParams()
                {
                    type = MessageType.Log,
                    message = message,
                    textDocument = new TextDocumentIdentifier(uri)
                };
                this.RpcServer.SendNotification(UriLogMessageNotification.Type, uriLogMessageParams);
            }
            return true;
        }

        private List<CodeElementWrapper> CodeElementFinder(FileCompiler fileCompiler, Position position)
        {
            List<CodeElement> codeElements = new List<CodeElement>();
            List<CodeElement> ignoredCodeElements = new List<CodeElement>();
            int lineIndex = position.line;
            // Find the token located below the mouse pointer
            int linesCount = fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines.Count;
            if (linesCount != 0 && lineIndex < linesCount)
            {
                while (codeElements.Count == 0 && lineIndex >= 0)
                {
                    var codeElementsLine =
                        fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines[lineIndex];

                    if (codeElementsLine != null && codeElementsLine.CodeElements != null && !(codeElementsLine.CodeElements[0] is SentenceEnd))
                    {
                        //Ignore all the EndOfFile token 
                        var tempCodeElements = codeElementsLine.CodeElements.Where(c => c.ConsumedTokens.Any(t => t.TokenType != TokenType.EndOfFile)).ToArray();

                        foreach (var tempCodeElement in tempCodeElements.Reverse())
                        {
                            if (!tempCodeElement.ConsumedTokens.Any(t => /*CompletionElligibleTokens.IsCompletionElligibleToken(t) &&*/
                            ((t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line < position.line + 1)))
                                ignoredCodeElements.Add(tempCodeElement);
                            else
                                codeElements.Add(tempCodeElement);
                        }

                        if (tempCodeElements.Any(c => c.ConsumedTokens.Any(t => t.TokenType == TokenType.PeriodSeparator && !(t is Compiler.AntlrUtils.MissingToken))))
                            break;
                    }

                    lineIndex--; //decrease lineIndex to get the previous line of TypeCobol Tree.
                }

                codeElements.AddRange(ignoredCodeElements);
                //Add the previously ignored Code Elements, may be they are useful to help completion.

                if (!codeElements.Any(c => c.ConsumedTokens.Any(t => t.Line <= position.line + 1)))
                    return null; //If nothing is found near the cursor we can't do anything
            }

            //Create a list of CodeElementWrapper in order to loose the ConsumedTokens ref. 
            return codeElements.Select(c => new CodeElementWrapper(c)).ToList();
        }

        private void LoadingIssueDetected(object sender, LoadingIssueEvent loadingIssueEvent)
        {
            this.RpcServer.SendNotification(LoadingIssueNotification.Type, new LoadingIssueParams() { Message = loadingIssueEvent.Message });
        }

        private void ExceptionTriggered(object sender, ThreadExceptionEventArgs exception)
        {
            this.NotifyException(exception.Exception);
        }

        private void WarningTrigger(object sender, string message)
        {
            this.NotifyWarning(message);
        }

        protected void MissingCopiesDetected(TextDocumentIdentifier textDocument, List<string> copiesName)
        {
            if (copiesName.Count > 0)
            {
                var missingCopiesParam = new MissingCopiesParams();
                missingCopiesParam.textDocument = textDocument;

#if EUROINFO_RULES
                ILookup<bool, string> lookup = copiesName.ToLookup(s => Workspace.Configuration.IsCpyCopy(s));
                missingCopiesParam.Copies = lookup[false].ToList();
                missingCopiesParam.CpyCopies = lookup[true].ToList();
#else
                missingCopiesParam.Copies = copiesName;
                missingCopiesParam.CpyCopies = new List<string>();
#endif
                this.RpcServer.SendNotification(MissingCopiesNotification.Type, missingCopiesParam);
            }
        }

        /// <summary>
        /// Event Method triggered when missing copies are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="missingCopiesEvent">List of missing copies name</param>
        private void MissingCopiesDetected(object fileUri, MissingCopiesEvent missingCopiesEvent)
        {
            //Warning the main file could not be opened
            //This event can be used when a dependency have not been loaded

            //Send missing copies to client
            MissingCopiesDetected(new TextDocumentIdentifier(((Uri)fileUri)), missingCopiesEvent.Copies);
        }

        /// <summary>
        /// Event Method triggered when diagnostics are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="diagnosticEvent">List of TypeCobol compiler diagnostics</param>
        private void DiagnosticsDetected(object fileUri, DiagnosticEvent diagnosticEvent)
        {
            var diagParameter = new PublishDiagnosticsParams();
            var diagList = new List<Diagnostic>();

            foreach (var diag in diagnosticEvent.Diagnostics)
            {
                diagList.Add(new Diagnostic(new Range(diag.LineStart, diag.ColumnStart, diag.LineEnd, diag.ColumnEnd),
                    diag.Message, (DiagnosticSeverity)diag.Info.Severity, diag.Info.Code.ToString(),
                    diag.Info.ReferenceText));
            }

            // Gets the original URI (which was set by the client)
            // DON'T use ToString() as it returns the canonically unescaped form of the URI
            // (it may cause issue if the path contains some blanks which need to be escaped)
            diagParameter.uri = ((Uri)fileUri).OriginalString;
            diagParameter.diagnostics = diagList.ToArray();
            this.RpcServer.SendNotification(PublishDiagnosticsNotification.Type, diagParameter);
        }

        /// <summary>
        /// Put all experimental properties as an array of Tuple{string name, object value}.
        /// The JSON serialization of a tuple is of the form: {"Item1" : name, "Item2" : value}
        /// </summary>
        public virtual Tuple<string, object>[] ExperimentalProperties
        {
            get
            {
                //TypeCobol version
                return new Tuple<string, object>[] { new Tuple<string, object>("version", Parser.Version) };
            }
        }

        protected DocumentContext GetDocumentContextFromStringUri(string uri, Workspace.SyntaxTreeRefreshLevel refreshLevel)
        {
            Uri objUri = new Uri(uri);
            if (objUri.IsFile && this.Workspace.TryGetOpenedDocument(objUri, out var context))
            {
                System.Diagnostics.Debug.Assert(context.FileCompiler != null);
                //Refresh context
                this.Workspace.RefreshSyntaxTree(context.FileCompiler, refreshLevel);
                return context;
            }

            return null;
        }

        /// <summary>
        /// Creates workspace and returns language server capabilities.
        /// </summary>
        /// <param name="parameters">Initialization parameters</param>
        /// <returns>Init result describing server capabilities.</returns>
        protected override InitializeResult OnInitialize(InitializeParams parameters)
        {
            this.RemoteConsole.NoLogsMessageNotification = NoLogsMessageNotification;
            var rootDirectory = new DirectoryInfo(parameters.rootPath);
            string workspaceName = rootDirectory.Name + "#" + parameters.processId;

            // Initialize the workspace.
            this.Workspace = new Workspace(rootDirectory.FullName, workspaceName, _messagesActionsQueue, Logger);
            if (!NoCopyDependencyWatchers)
                this.Workspace.InitCopyDependencyWatchers();
#if EUROINFO_RULES
            this.Workspace.CpyCopyNamesMapFilePath = CpyCopyNamesMapFilePath;
#endif
            // Propagate LSR testing options.
            this.Workspace.LsrTestOptions = LsrTestingLevel;
            this.Workspace.UseSyntaxColoring = UseSyntaxColoring;
            this.Workspace.UseAntlrProgramParsing = UseAntlrProgramParsing;
            this.Workspace.TimerDisabledOption = TimerDisabledOption;

            // Attach event handlers.
            this.Workspace.LoadingIssueEvent += LoadingIssueDetected;
            this.Workspace.ExceptionTriggered += ExceptionTriggered;
            this.Workspace.WarningTrigger += WarningTrigger;
            this.Workspace.MissingCopiesEvent += MissingCopiesDetected;
            this.Workspace.DiagnosticsEvent += DiagnosticsDetected;
            this.Workspace.LoadCustomAnalyzers(ExtensionManager);

            // Return language server capabilities
            var initializeResult = base.OnInitialize(parameters);
            initializeResult.capabilities.textDocumentSync = TextDocumentSyncKind.Incremental;
            initializeResult.capabilities.hoverProvider = true;
            CompletionOptions completionOptions = new CompletionOptions();
            completionOptions.resolveProvider = false;
            completionOptions.triggerCharacters = new string[] { "::" };
            initializeResult.capabilities.completionProvider = completionOptions;
            SignatureHelpOptions sigHelpOptions = new SignatureHelpOptions { triggerCharacters = new string[0] };
            initializeResult.capabilities.signatureHelpProvider = sigHelpOptions;
            initializeResult.capabilities.experimental = ExperimentalProperties;
            return initializeResult;
        }

        protected override void OnShutdown()
        {
            this.Workspace.LoadingIssueEvent -= LoadingIssueDetected;
            this.Workspace.ExceptionTriggered -= ExceptionTriggered;
            this.Workspace.WarningTrigger -= WarningTrigger;
            this.Workspace.MissingCopiesEvent -= MissingCopiesDetected;
            this.Workspace.DiagnosticsEvent -= DiagnosticsDetected;

            base.OnShutdown();
        }

        protected virtual void OnDidChangeConfiguration(string[] arguments)
        {
            this.Workspace.DidChangeConfigurationParams(arguments);
        }
        protected override void OnDidChangeConfiguration(DidChangeConfigurationParams parameters)
        {
            if (parameters.settings is Newtonsoft.Json.Linq.JArray array)
            {
                IEnumerable<string> argsEnum = array.Select(t => t.ToString());
                string[] arguments = argsEnum.ToArray<string>();
                OnDidChangeConfiguration(arguments);
            }
            else
            {
                OnDidChangeConfiguration(parameters.settings.ToString().Split(' '));
            }
        }

        protected override void OnDidChangeWatchedFiles(DidChangeWatchedFilesParams parameters)
        {
            if (parameters.changes == null || parameters.changes.Length == 0) return;

            this.Workspace.AcknowledgeCopyChanges(GetChangedCopies().ToList());

            //Convert every file event change into a ChangedCopy object
            IEnumerable<ChangedCopy> GetChangedCopies()
            {
                foreach (var fileEvent in parameters.changes)
                {
                    if (ChangedCopy.TryParse(fileEvent.uri, out var changedCopy))
                    {
                        yield return changedCopy;
                    }
                }
            }
        }

        /// <summary>
        /// Open a Text Document
        /// </summary>
        /// <param name="parameters">LSP Open Document Parameters</param>
        /// <param name="projectKey">The target Project's key</param>
        /// <param name="copyFolders">List of copy folders associated to the project</param>
        protected void OpenTextDocument(DidOpenTextDocumentParams parameters, string projectKey, List<string> copyFolders)
        {
            DocumentContext docContext = new DocumentContext(parameters.textDocument);
            if (docContext.Uri.IsFile && !this.Workspace.TryGetOpenedDocument(docContext.Uri, out _))
            {
                //Create a ILanguageServer instance for the document.
                docContext.LanguageServer = new TypeCobolLanguageServer(this.RpcServer, docContext.TextDocument);
                docContext.LanguageServer.UseSyntaxColoring = UseSyntaxColoring;

                string text = parameters.text ?? parameters.textDocument.text;
                //These are no longer needed.
                parameters.text = null;
                parameters.textDocument.text = null;
                this.Workspace.OpenTextDocument(docContext, text, projectKey, copyFolders);

                // DEBUG information
                RemoteConsole.Log("Opened source file : " + docContext.Uri.LocalPath);
            }
        }

        protected override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            OpenTextDocument(parameters, null, null);
        }

        protected override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {
            var docContext = GetDocumentContextFromStringUri(parameters.uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh); //Text Change do not have to trigger node phase, it's only another event that will do it
            if (docContext == null)
                return;

            Uri objUri = new Uri(parameters.uri);
            var updates = new RangeUpdate[parameters.contentChanges.Length];
            
            // Convert change events into updates
            for (int i = 0; i < parameters.contentChanges.Length; i++)
            {
                var contentChange = parameters.contentChanges[i];

                // Document cleared
                if (contentChange.range == null)
                {
                    //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                    //To avoid crashes.
                    try
                    {
                        docContext.LanguageServerConnection(false);
                        this.Workspace.BindFileCompilerSourceTextDocument(docContext, contentChange.text, this.Workspace.LsrTestOptions);
                        return;
                    }
                    catch (Exception e)
                    {
                        // Don't rethrow an exception on save.
                        RemoteConsole.Error($"Error while handling notification textDocument/didChange : {e.Message}");
                        return;
                    }
                }

                var start = contentChange.range.start;
                var end = contentChange.range.end;
                updates[i] = new RangeUpdate(start.line, start.character, end.line, end.character, contentChange.text);
            }

            // Update the source file with the text updates
            this.Workspace.UpdateSourceFile(objUri, updates);

            // DEBUG information
            RemoteConsole.Log("Updated source file : " + objUri.LocalPath);
            foreach (var update in updates)
            {
                RemoteConsole.Log(" - " + update);
            }
        }

        protected override void OnDidCloseTextDocument(DidCloseTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile && this.Workspace.TryCloseSourceFile(objUri))
            {
                // DEBUG information
                RemoteConsole.Log("Closed source file : " + objUri.LocalPath);
            }
        }

        protected override void OnDidSaveTextDocument(DidSaveTextDocumentParams parameters, LSPProfiling lspProfiling)
        {
            if (parameters.text != null)
            {
                DidChangeTextDocumentParams dctdp = new DidChangeTextDocumentParams(parameters.textDocument.uri);
                TextDocumentContentChangeEvent tdcce = new TextDocumentContentChangeEvent();
                tdcce.text = parameters.text;
                dctdp.contentChanges = new TextDocumentContentChangeEvent[] { tdcce };
                OnDidChangeTextDocument(dctdp);
            }
        }

        protected override Hover OnHover(TextDocumentPosition parameters)
        {
            Hover resultHover = new Hover();

            var docContext = GetDocumentContextFromStringUri(parameters.uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            if (docContext == null)
                return resultHover;
            System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);

            var wrappedCodeElements = CodeElementFinder(docContext.FileCompiler, parameters.position);
            if (wrappedCodeElements == null)
                return resultHover;

            Token userFilterToken = null;
            Token lastSignificantToken = null;
            //Try to get a significant token for competion and return the codeelement containing the matching token.
            CodeElement matchingCodeElement = CodeElementMatcher.MatchCompletionCodeElement(parameters.position,
                wrappedCodeElements,
                out userFilterToken, out lastSignificantToken); //Magic happens here
            if (matchingCodeElement == null)
            {
                return resultHover;
            }

            docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers.TryGetValue(((CodeElementWrapper)matchingCodeElement).CodeElement, out var matchingNode);
            if (matchingNode == null)
                return resultHover;

            string message = string.Empty;

            //Switch between all nodes that can return information
            switch (matchingNode)
            {
                case DataDefinition data:
                    if (data.TypeDefinition != null)
                        message = data.TypeDefinition.ToString();
                    break;
                case ProcedureStyleCall call:
                    //don't show hover on params
                    if (call.FunctionDeclaration != null && lastSignificantToken.TokenType != TokenType.INPUT && lastSignificantToken.TokenType != TokenType.IN_OUT && lastSignificantToken.TokenType != TokenType.OUTPUT)
                    {
                        message = call.ToString();
                    }
                    break;
                case FunctionDeclaration fun:
                    //only for params of a function declaration
                    if (userFilterToken != null && (lastSignificantToken.TokenType == TokenType.QualifiedNameSeparator || lastSignificantToken.TokenType == TokenType.TYPE))
                    {
                        foreach (var param in fun.Profile.Parameters)
                        {
                            //if the hovered position is inside this parameter
                            //line + 1 : because start index is 0
                            if (param.CodeElement.Line == parameters.position.line + 1 &&
                                param.CodeElement.StartIndex < parameters.position.character &&
                                param.CodeElement.StopIndex > parameters.position.character)
                            {
                                if (param.TypeDefinition != null)
                                    message = param.TypeDefinition.ToString();
                            }
                        }
                    }
                    break;
                default:
                    // Use userFilterToken to find the variable under mouse cursor.
                    if (userFilterToken != null)
                    {
                        if (matchingNode.CodeElement != null && matchingNode.CodeElement.SymbolInformationForTokens.TryGetValue(userFilterToken, out var targetSymbolInformation))
                        {
                            // We have SymbolInformation for the Token, so now we look for the associated StorageArea.
                            // It is either among StorageAreaReads or StorageAreaWrites.
                            var targetStorageArea =
                                matchingNode.CodeElement.StorageAreaReads?
                                    .FirstOrDefault(storageArea => storageArea.SymbolReference == targetSymbolInformation) ??
                                matchingNode.CodeElement.StorageAreaWrites?
                                    .Select(receivingStorageArea => receivingStorageArea.StorageArea)
                                    .Where(storageArea => storageArea != null)
                                    .FirstOrDefault(storageArea => storageArea.SymbolReference == targetSymbolInformation);

                            // Trace back to the DataDefinition corresponding to the storage area.
                            var targetDataDefinition = matchingNode.GetDataDefinitionFromStorageAreaDictionary(targetStorageArea);
                            message = ToolTipHelper.GetToolTipText(targetDataDefinition);
                        }
                    }
                    break;
            }

            if (message != string.Empty)
            {
                resultHover.range = new Range(matchingCodeElement.Line, matchingCodeElement.StartIndex,
                    matchingCodeElement.LineEnd,
                    matchingCodeElement.StopIndex + 1);
                resultHover.contents =
                    new MarkedString[] { new MarkedString() { language = "Cobol", value = message } };
                return resultHover;
            }


            return resultHover;

        }

        /// <summary>
        /// Request to request completion at a given text document position. The request's
        /// parameter is of type[TextDocumentPosition](#TextDocumentPosition) the response
        /// is of type[CompletionItem[]](#CompletionItem) or a Thenable that resolves to such.
        /// </summary>
        protected override List<CompletionItem> OnCompletion(TextDocumentPosition parameters)
        {
            var docContext = GetDocumentContextFromStringUri(parameters.uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            if (docContext == null)
                return null;
            System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);

            List<CompletionItem> items;

            if (docContext.FileCompiler.CompilationResultsForProgram != null &&
                docContext.FileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
            {
                var wrappedCodeElements = CodeElementFinder(docContext.FileCompiler, parameters.position);
                if (wrappedCodeElements == null)
                    return new List<CompletionItem>();

                Token userFilterToken = null;
                Token lastSignificantToken = null;
                //Try to get a significant token for competion and return the codeelement containing the matching token.
                CodeElement matchingCodeElement = CodeElementMatcher.MatchCompletionCodeElement(parameters.position,
                    wrappedCodeElements,
                    out userFilterToken, out lastSignificantToken); //Magic happens here
                var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

                if (lastSignificantToken != null)
                {
                    switch (lastSignificantToken.TokenType)
                    {
                        case TokenType.PERFORM:
                            {
                                items = CompletionFactory.GetCompletionPerformParagraphAndSection(docContext.FileCompiler,
                                    matchingCodeElement, userFilterToken);
                                break;
                            }
                        case TokenType.CALL:
                            {
                                this.FunctionDeclarations.Clear(); //Clear to avoid key collision
                                items = CompletionFactory.GetCompletionForProcedure(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, this.FunctionDeclarations);
                                items.AddRange(CompletionFactory.GetCompletionForLibrary(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken));
                                break;
                            }
                        case TokenType.TYPE:
                            {
                                items = CompletionFactory.GetCompletionForType(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken);
                                items.AddRange(CompletionFactory.GetCompletionForLibrary(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken));
                                break;
                            }
                        case TokenType.QualifiedNameSeparator:
                            {
                                items = CompletionFactory.GetCompletionForQualifiedName(parameters.position,
                                    docContext.FileCompiler, matchingCodeElement, lastSignificantToken, userFilterToken, this.FunctionDeclarations);
                                break;
                            }
                        case TokenType.INPUT:
                        case TokenType.OUTPUT:
                        case TokenType.IN_OUT:
                            {
                                items = CompletionFactory.GetCompletionForProcedureParameter(parameters.position,
                                    docContext.FileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken, this.SignatureCompletionContext);
                                break;
                            }
                        case TokenType.DISPLAY:
                            {
                                Func<DataDefinition, bool> predicate = dataDefinition =>
                                    dataDefinition.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase) // keep only variables with matching name
                                    && dataDefinition.Usage != DataUsage.ProcedurePointer // invalid usages in DISPLAY statement
                                    && dataDefinition.Usage != DataUsage.FunctionPointer
                                    && dataDefinition.Usage != DataUsage.ObjectReference
                                    && dataDefinition.Usage != DataUsage.Index
                                    && (dataDefinition.CodeElement?.LevelNumber != null && dataDefinition.CodeElement.LevelNumber.Value < 88);
                                // Ignore level 88. Note that dataDefinition.CodeElement != null condition also filters out IndexDefinition which is invalid in the context of DISPLAY
                                // Filtering dataDefinition without LevelNumber also excludes FileDescription which are invalid for a DISPLAY
                                items = CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement, predicate);
                                break;
                            }
                        case TokenType.MOVE:
                            {
                                items = CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    da =>
                                        da.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase) &&
                                        ((da.CodeElement?.LevelNumber != null && da.CodeElement.LevelNumber.Value < 88)
                                         || (da.CodeElement == null && da is IndexDefinition)));
                                //Ignore 88 level variable
                                break;
                            }
                        case TokenType.TO:
                            {
                                items = CompletionFactory.GetCompletionForTo(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, lastSignificantToken);
                                break;
                            }
                        case TokenType.INTO:
                            {
                                items = CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    v => v.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase)
                                         && (v.CodeElement != null &&
                                             v.DataType == DataType.Alphabetic
                                             || v.DataType == DataType.Alphanumeric
                                             || v.DataType == DataType.AlphanumericEdited)
                                );
                                break;
                            }
                        case TokenType.SET:
                            {
                                items = CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    v => v.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase)
                                         &&
                                         ((v.CodeElement?.Type == CodeElementType.DataConditionEntry)
                                          //Level 88 Variable
                                          || v.DataType == DataType.Numeric //Numeric Integer Variable
                                          || v.Usage == DataUsage.Pointer || v.Usage == DataUsage.Pointer32) //Or usage is pointer/pointer-32 
                                );
                                break;
                            }
                        case TokenType.OF:
                            {
                                items = CompletionFactory.GetCompletionForOf(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, parameters.position);
                                break;
                            }
                        default:
                            // Unable to suggest anything
                            items = new List<CompletionItem>();
                            break;
                    }
                }
                else
                {
                    //If no known keyword has been found, let's try to get the context and return available variables. 
                    if (matchingCodeElement == null && wrappedCodeElements.Any())
                    {
                        userFilterToken =
                            wrappedCodeElements.First().ArrangedConsumedTokens.FirstOrDefault(
                                t =>
                                    parameters.position.character <= t.StopIndex + 1 && parameters.position.character > t.StartIndex
                                    && t.Line == parameters.position.line + 1
                                    && t.TokenType == TokenType.UserDefinedWord); //Get the userFilterToken to filter the results

                        userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text; //Convert token to text

                        items = CompletionFactory.GetCompletionForVariable(docContext.FileCompiler,
                           wrappedCodeElements.First(), da => da.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase));
                    }
                    else
                    {
                        //Return a default text to inform the user that completion is not available after the given token
                        items = new List<CompletionItem>(1)
                        {
                            new CompletionItem("Completion is not available in this context") { insertText = string.Empty }
                        };
                    }
                }

                if (userFilterToken != null)
                {
                    //Add the range object to let the client know the position of the user filter token
                    var range = new Range(userFilterToken.Line - 1, userFilterToken.StartIndex,
                        userFilterToken.Line - 1, userFilterToken.StopIndex + 1);
                    //-1 on lne to 0 based / +1 on stop index to include the last character
                    items.ForEach(c =>
                    {
                        if (c.data != null && c.data.GetType().IsArray)
                            ((object[])c.data)[0] = range;
                        else
                            c.data = range;
                    });
                }
            }
            else
            {
                items = null;
            }

            return items;
        }

        protected override SignatureHelp OnSignatureHelp(TextDocumentPosition parameters)
        {
            var docContext = GetDocumentContextFromStringUri(parameters.uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            if (docContext == null)
                return null;
            System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);

            if (docContext.FileCompiler?.CompilationResultsForProgram?.ProcessedTokensDocumentSnapshot == null) //Semantic snapshot is not available
                return null;

            var wrappedCodeElement = CodeElementFinder(docContext.FileCompiler, parameters.position)?.FirstOrDefault();
            if (wrappedCodeElement == null) //No codeelements found
                return null;

            var node = CompletionFactoryHelpers.GetMatchingNode(docContext.FileCompiler, wrappedCodeElement);

            //Get procedure name or qualified name
            string procedureName = CompletionFactoryHelpers.GetProcedureNameFromTokens(wrappedCodeElement.ArrangedConsumedTokens);

            //Try to get procedure by its name
            var calledProcedures =
                node.SymbolTable.GetFunctions(
                    p =>
                        p.Name.Equals(procedureName) ||
                        p.QualifiedName.ToString().Equals(procedureName),
                        SymbolTable.Scope.Intrinsic
                    )
                    .ToArray();
            var signatureHelp = new SignatureHelp();

            if (calledProcedures.Length == 1)
            {
                var calledProcedure = calledProcedures[0];
                //Create and return SignatureHelp object 
                signatureHelp.signatures = new SignatureInformation[1];
                signatureHelp.signatures[0] = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(calledProcedure);
                signatureHelp.activeSignature = 0; //Set the active signature as the one just created
                //Select the current parameter the user is expecting
                signatureHelp.activeParameter = ProcedureSignatureHelper.SignatureHelperParameterSelecter(calledProcedure, wrappedCodeElement, parameters.position);

                //There is only one possibility so the context can be set right now 
                this.SignatureCompletionContext = calledProcedure;

                return signatureHelp;
            }

            //Else try to find the best matching signature

            //Get all given INPUT
            var givenInputParameters = CompletionFactoryHelpers.AggregateTokens(
                wrappedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.INPUT)
                    .Skip(1) //Ignore the INPUT Token
                    .TakeWhile(t => !(t.TokenType == TokenType.OUTPUT || t.TokenType == TokenType.IN_OUT))).ToList();
            //Get all given OUTPUT
            var givenOutputParameters = CompletionFactoryHelpers.AggregateTokens(
               wrappedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.OUTPUT)
                   .Skip(1) //Ignore the INPUT Token
                   .TakeWhile(t => !(t.TokenType == TokenType.INPUT || t.TokenType == TokenType.IN_OUT))).ToList();
            //Get all given INOUT
            var givenInoutParameters = CompletionFactoryHelpers.AggregateTokens(
              wrappedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.IN_OUT)
                  .Skip(1) //Ignore the INPUT Token
                  .TakeWhile(t => !(t.TokenType == TokenType.OUTPUT || t.TokenType == TokenType.INPUT))).ToList();
            var totalGivenParameters = givenInputParameters.Count + givenInoutParameters.Count + givenOutputParameters.Count;

            var signatureInformation = new List<SignatureInformation>();
            this.FunctionDeclarations.Clear();
            FunctionDeclaration bestmatchingProcedure = null;
            int previousMatchingWeight = 0;

            if (totalGivenParameters == 0)
            {
                foreach (var procedure in calledProcedures) //No parameters given, return all possibilities
                {
                    var formattedSignatureInformation = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(procedure);
                    signatureInformation.Add(formattedSignatureInformation);
                    this.FunctionDeclarations.Add(formattedSignatureInformation, procedure);
                }
            }
            else
            {
                foreach (var procedure in calledProcedures)
                {
                    //The commented parts allow to restrict the potential compatible signature to return to the client
                    int matchingWeight = 0;
                    //Test INPUT parameters
                    matchingWeight = matchingWeight + ProcedureSignatureHelper.ParametersTester(procedure.Profile.InputParameters, givenInputParameters, node);

                    //Test OUTPUT parameters
                    matchingWeight = matchingWeight + ProcedureSignatureHelper.ParametersTester(procedure.Profile.OutputParameters, givenOutputParameters, node);

                    //Test INOUT parameters 
                    matchingWeight = matchingWeight + ProcedureSignatureHelper.ParametersTester(procedure.Profile.InoutParameters, givenInoutParameters, node);

                    if (matchingWeight > 0 && matchingWeight >= previousMatchingWeight && totalGivenParameters > 0)
                    {
                        if (matchingWeight > previousMatchingWeight)
                            signatureInformation.Clear();  //If the matchingWeight is superior than previous, it means that the previous signature is not precise enough.

                        var formattedSignatureInformation = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(procedure);
                        signatureInformation.Add(formattedSignatureInformation);
                        this.FunctionDeclarations.Add(formattedSignatureInformation, procedure);

                        previousMatchingWeight = matchingWeight;
                        bestmatchingProcedure = procedure;
                    }
                }
            }

            signatureHelp.signatures = signatureInformation.ToArray();

            if (signatureInformation.Count == 1)
            {
                this.SignatureCompletionContext = bestmatchingProcedure; //Set the completion context 
                signatureHelp.activeSignature = 0; //Select the only signature for the client
                signatureHelp.activeParameter = ProcedureSignatureHelper.SignatureHelperParameterSelecter(bestmatchingProcedure, wrappedCodeElement, parameters.position); //Select the current parameter
            }

            return signatureHelp;
        }

        protected override Definition OnDefinition(TextDocumentPosition parameters)
        {
            var defaultDefinition = new Definition(parameters.uri, new Range());
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile && this.Workspace.TryGetOpenedDocument(objUri, out var docContext))
            {
                var codeElementToNode = docContext.FileCompiler?.CompilationResultsForProgram.ProgramClassDocumentSnapshot?.NodeCodeElementLinkers;
                if (codeElementToNode != null)
                {
                    Token matchingToken = null;
                    var matchingCodeElement = codeElementToNode.Keys.FirstOrDefault(MatchPosition);
                    if (matchingCodeElement == null) return defaultDefinition;

                    var matchingNode = codeElementToNode[matchingCodeElement];
                    if (matchingNode == null) return defaultDefinition;

                    bool MatchPosition(CodeElement codeElement)
                    {
                        foreach (var token in codeElement.ConsumedTokens)
                        {
                            if (token.Line == parameters.position.line + 1 &&
                                parameters.position.character >= token.StartIndex &&
                                parameters.position.character <= token.StopIndex + 1 &&
                                token.TokenType != TokenType.QualifiedNameSeparator)
                            {
                                matchingToken = token;
                                return !codeElement.IsInsideCopy();
                            }
                        }

                        return false;
                    }

                    CodeElementMatcher.MatchCompletionCodeElement(parameters.position, new List<CodeElementWrapper>()
                        {
                            new CodeElementWrapper(matchingCodeElement)
                        }, out _, out var lastSignificantToken); //Magic happens here
                    var potentialDefinitionNodes = new List<Node>();

                    if (lastSignificantToken != null)
                    {
                        switch (lastSignificantToken.TokenType)
                        {
                            case TokenType.PERFORM:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetParagraphs(MatchName));
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetSections(MatchName));
                                break;
                            }

                            case TokenType.CALL:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetFunctions(MatchName, SymbolTable.Scope.Program));
                                break;
                            }

                            case TokenType.TYPE:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetTypes(MatchName, SymbolTable.Scope.Program));
                                break;
                            }

                            default: //INPUT, OUTPUT, IN_OUT, MOVE, TO, etc
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetVariables(MatchName, SymbolTable.Scope.Program));
                                break;
                            }
                        }

                        bool MatchName(Node node) => string.Equals(node.Name, matchingToken.Text, StringComparison.OrdinalIgnoreCase);
                    }

                    if (potentialDefinitionNodes.Count > 0)
                    {
                        var nodeDefinition = potentialDefinitionNodes[0];
                        if (nodeDefinition.CodeElement != null)
                            return new Definition(parameters.uri,
                                new Range() { start = new Position(nodeDefinition.CodeElement.Line - 1, 0) });
                    }
                }
            }

            return defaultDefinition;
        }
    }

    public class CodeElementWrapper : CodeElement
    {
        public CodeElement CodeElement { get; set; }
        public CodeElementWrapper(CodeElement codeElement)
            : base(codeElement.Type)
        {
            this.CodeElement = codeElement;
            ConsumedTokens = codeElement.ConsumedTokens;

            ArrangedConsumedTokens = new List<Token>();
            foreach (var token in ConsumedTokens)
            {
                if (token is Compiler.AntlrUtils.MissingToken)
                    continue;

                ArrangedConsumedTokens.Add(new Token(token.TokenType, token.StartIndex, token.StopIndex, token.TokensLine));
            }
        }

        public List<Token> ArrangedConsumedTokens { get; set; }
    }

    internal static class ToolTipHelper
    {
        /// <summary>
        /// Gives self and children text lines of a node, each text line is associated
        /// with its depth.
        /// </summary>
        /// <param name="node">Node to explore.</param>
        /// <param name="level">Starting level, default is 0.</param>
        /// <returns>IEnumerable of Tuple, Item1 is depth, Item2 is ITextLine.</returns>
        private static IEnumerable<Tuple<int, ITextLine>> GetLinesWithLevel(Node node, int level = 0)
        {
            if (node == null) yield break;

            // Lines of current level in node
            if (node.CodeElement?.ConsumedTokens != null)
            {
                ITokensLine lastReturnedLine = null;
                foreach (var token in node.CodeElement.ConsumedTokens)
                {
                    var line = token.TokensLine;
                    if (line != lastReturnedLine)
                    {
                        yield return new Tuple<int, ITextLine>(level, line);
                        lastReturnedLine = line;
                    }
                }
            }

            // Lines of children (recursive call)
            foreach (var child in node.Children)
            {
                foreach (var tuple in GetLinesWithLevel(child, level + 1))
                {
                    yield return tuple;
                }
            }
        }

        /// <summary>
        /// Builds tooltip text for a Node.
        /// </summary>
        /// <param name="node">Node to render as text.</param>
        /// <returns>A textual representation of the given node.</returns>
        public static string GetToolTipText(Node node)
        {
            if (node == null) return string.Empty;
            StringBuilder sb = new StringBuilder();
            foreach (var tuple in GetLinesWithLevel(node))
            {
                // Replace original indent with custom indent (3 spaces per level).
                sb.Append(' ', 3 * tuple.Item1);
                sb.AppendLine(tuple.Item2.Text.TrimStart());
            }
            return sb.ToString();
        }
    }
}
