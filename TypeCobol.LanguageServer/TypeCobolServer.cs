using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Analytics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.LanguageServer.Context;
using TypeCobol.LanguageServer.SignatureHelper;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : VsCodeProtocol.LanguageServer
    {
        private readonly Queue<MessageActionWrapper> _messagesActionsQueue;
        protected Dictionary<SignatureInformation, FunctionDeclaration> FunctionDeclarations { get; }

        public TypeCobolServer(IRPCServer rpcServer, Queue<MessageActionWrapper> messagesActionsQueue)
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

        /// <summary>
        /// true to use Euro-Information replacement rules
        /// </summary>
        public bool UseEuroInformationLegacyReplacingSyntax { get; set; }

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
        /// Custom Analyzers Dll Paths
        /// </summary>
        public List<string> CustomAnalyzerFiles { get; set; }

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
                    textDocument = new TextDocumentIdentifier(uri.ToString())
                };
                this.RpcServer.SendNotification(UriLogMessageNotification.Type, uriLogMessageParams);
            }
            return true;
        }

        private IEnumerable<CodeElementWrapper> CodeElementFinder(FileCompiler fileCompiler, Position position)
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
                        var tempCodeElements = codeElementsLine.CodeElements.Where(c => c.ConsumedTokens.Any(t => t.TokenType != TokenType.EndOfFile));

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
                //Add the previously ignored Code Elements, may be they are usefull to help completion.

                if (!codeElements.Any(c => c.ConsumedTokens.Any(t => t.Line <= position.line + 1)))
                    return null; //If nothing is found near the cursor we can't do anything
            }

            //Create a list of CodeElementWrapper in order to loose the ConsumedTokens ref. 
            return codeElements.Select(c => new CodeElementWrapper(c));
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
                ILookup<bool, string> lookup = copiesName.ToLookup<string, bool>(s => Workspace.CompilationProject.CompilationOptions.HasCpyCopy(s));
                missingCopiesParam.Copies = lookup[false].ToList();
                missingCopiesParam.cpyCopies = lookup[true].ToList();
#else
                missingCopiesParam.Copies = copiesName;
                missingCopiesParam.cpyCopies = new List<string>();
#endif
                this.RpcServer.SendNotification(MissingCopiesNotification.Type, missingCopiesParam);
            }
        }

        /// <summary>
        /// Event Method triggered when missing copies are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="missingCopies">List of missing copies name</param>
        private void MissingCopiesDetected(object fileUri, MissingCopiesEvent missingCopiesEvent)
        {
            //Warning the main file could not be opened
            //This event can be used when a dependency have not been loaded

            //Send missing copies to client
            MissingCopiesDetected(new TextDocumentIdentifier(fileUri.ToString()), missingCopiesEvent.Copies);
        }

        /// <summary>
        /// Event Method triggered when diagnostics are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="diagnostics">List of TypeCobol compiler diagnostics</param>
        private void DiagnosticsDetected(object fileUri, DiagnosticEvent diagnosticEvent)
        {
            var diagParameter = new PublishDiagnosticsParams();
            var diagList = new List<Diagnostic>();

            foreach (var diag in diagnosticEvent.Diagnostics)
            {
                diagList.Add(new Diagnostic(new Range(diag.Line, diag.ColumnStart, diag.Line, diag.ColumnEnd),
                    diag.Message, (DiagnosticSeverity)diag.Info.Severity, diag.Info.Code.ToString(),
                    diag.Info.ReferenceText));
            }

            diagParameter.uri = fileUri.ToString();
            diagParameter.diagnostics = diagList.ToArray();
            this.RpcServer.SendNotification(PublishDiagnosticsNotification.Type, diagParameter);
        }

        /// <summary>
        //Put all experimental properties as an array of Tuple<string name, object value>.
        //The JSON serialization of a tuple is of the form: {"Item1" : name, "Item2" : value}
        /// </summary>
        public virtual Tuple<string, object>[] ExperimentalProperties
        {
            get
            {
                //TypeCobol version
                return new Tuple<string, object>[] { new Tuple<string, object>("version", AnalyticsWrapper.Telemetry.TypeCobolVersion) };
            }
        }

        protected DocumentContext GetDocumentContextFromStringUri(string uri, Workspace.SyntaxTreeRefreshLevel refreshLevel)
        {
            Uri objUri = new Uri(uri);
            if (objUri.IsFile && this.Workspace.TryGetOpenedDocumentContext(objUri, out var context))
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
#if EUROINFO_RULES
            this.Workspace.CpyCopyNamesMapFilePath = CpyCopyNamesMapFilePath;
#endif
            // Propagate LSR testing options.
            this.Workspace.LsrTestOptions = LsrTestingLevel;
            this.Workspace.UseSyntaxColoring = UseSyntaxColoring;
            this.Workspace.UseAntlrProgramParsing = UseAntlrProgramParsing;
            this.Workspace.UseEuroInformationLegacyReplacingSyntax = UseEuroInformationLegacyReplacingSyntax;
            this.Workspace.TimerDisabledOption = TimerDisabledOption;

            // Attach event handlers.
            this.Workspace.LoadingIssueEvent += LoadingIssueDetected;
            this.Workspace.ExceptionTriggered += ExceptionTriggered;
            this.Workspace.WarningTrigger += WarningTrigger;
            this.Workspace.MissingCopiesEvent += MissingCopiesDetected;
            this.Workspace.LoadCustomAnalyzers(CustomAnalyzerFiles);

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

        protected override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            DocumentContext docContext = new DocumentContext(parameters.textDocument);
            if (docContext.Uri.IsFile && !this.Workspace.TryGetOpenedDocumentContext(docContext.Uri, out _))
            {
                //Subscribe to diagnostics event
                this.Workspace.DiagnosticsEvent += DiagnosticsDetected;

                //Create a ILanguageServer instance for the document.
                docContext.LanguageServer = new TypeCobolLanguageServer(this.RpcServer, parameters.textDocument);
                docContext.LanguageServer.UseSyntaxColoring = UseSyntaxColoring;

                string text = parameters.text ?? parameters.textDocument.text;
                //These are no longer needed.
                parameters.text = null;
                parameters.textDocument.text = null;
                this.Workspace.OpenTextDocument(docContext, text);

                // DEBUG information
                RemoteConsole.Log("Opened source file : " + docContext.Uri.LocalPath);
            }
        }

        protected override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {

            var docContext = GetDocumentContextFromStringUri(parameters.uri, Workspace.SyntaxTreeRefreshLevel.NoRefresh); //Text Change do not have to trigger node phase, it's only a another event that will do it
            if (docContext == null)
                return;

            Uri objUri = new Uri(parameters.uri);

#region Convert text changes format from multiline range replacement to single line updates

            TextChangedEvent textChangedEvent = new TextChangedEvent();
            foreach (var contentChange in parameters.contentChanges)
            {
                // Split the text updated into distinct lines
                List<string> lineUpdates = null;
                bool replacementTextStartsWithNewLine = false;

                if (!string.IsNullOrEmpty(contentChange.text))
                {
                    replacementTextStartsWithNewLine = contentChange.text[0] == '\r' ||
                                                       contentChange.text[0] == '\n';
                    //Allow to know if a new line was added
                    //Split on \r \n to know the number of lines added
                    lineUpdates = contentChange.text.Replace("\r", "").Split('\n').ToList();
                    if (string.IsNullOrEmpty(lineUpdates.FirstOrDefault()) && replacementTextStartsWithNewLine)
                        lineUpdates.RemoveAt(0);
                }

                // Document cleared
                if (contentChange.range == null)
                {
                    //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                    //To avoid crashes.
                    try
                    {
                        docContext.LanguageServerConnection(false);
                        this.Workspace.OpenTextDocument(docContext, contentChange.text);
                        return;
                    }
                    catch (Exception e)
                    {
                        //Don't rethow an exception on save.
                        RemoteConsole.Error(string.Format("Error while handling notification {0} : {1}",
                            "textDocument/didChange", e.Message));
                        return;
                    }
                }
                // Document updated
                else if (docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines.Count != 0)
                {
                    // Get original lines text before change
                    int lineCount = docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines.Count;
                    string originalFirstLineText = lineCount <= contentChange.range.start.line ? "" :
                        docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines[contentChange.range.start.line]
                            .Text;
                    string originalLastLineText = originalFirstLineText;


                    // Check if the first line was inserted
                    int firstLineIndex = contentChange.range.start.line;
                    int firstLineChar = contentChange.range.start.character;
                    if (replacementTextStartsWithNewLine &&
                        !(contentChange.range.start.character < originalLastLineText.Length))
                    {
                        // do not increment if line is inserted at the end of global text
                        if (firstLineIndex < lineCount) firstLineIndex++;
                        firstLineChar = 0;
                    }
                    else if (replacementTextStartsWithNewLine)
                    //Detected that the add line appeared inside an existing line
                    {
                        lineUpdates.Add(lineUpdates.First());
                        //Add the default 7 spaces + add lineUpdates in order to update the current line and add the new one. 
                    }

                    // Check if the last line was deleted
                    int lastLineIndex = contentChange.range.end.line;
                    if (contentChange.range.end.line > contentChange.range.start.line &&
                        contentChange.range.end.character == 0)
                    {
                        //Allows to detect if the next line was suppressed
                    }
                    if (contentChange.text?.Length == 0)
                    {
                        lineUpdates = new List<string>();
                    }

                    if (lastLineIndex > firstLineIndex)
                    {
                        originalLastLineText =
                            docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines[
                                Math.Min(lastLineIndex,
                                    docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines.Count - 1)].Text;
                    }

                    // Text not modified at the beginning of the first replaced line
                    string startOfFirstLine = null;
                    if (firstLineChar > 0)
                    {
                        if (originalFirstLineText.Length >= contentChange.range.start.character)
                            startOfFirstLine = originalFirstLineText.Substring(0,
                                contentChange.range.start.character);
                        else
                            startOfFirstLine = originalFirstLineText.Substring(0, originalFirstLineText.Length) +
                                               new string(' ',
                                                   contentChange.range.start.character - originalFirstLineText.Length);
                    }

                    // Text not modified at the end of the last replaced line
                    string endOfLastLine = null;
                    if (contentChange.range.end.character < originalLastLineText.Length)
                    {
                        endOfLastLine = originalLastLineText.Substring(contentChange.range.end.character);
                    }

                    // Remove all the old lines
                    for (int i = firstLineIndex; i <= lastLineIndex; i++)
                    {
                        var textChange = new TextChange(TextChangeType.LineRemoved, firstLineIndex, null);
                        textChangedEvent.TextChanges.Add(textChange);
                        //Mark the index line to be removed. The index will remains the same for each line delete, because text change are apply one after another
                    }

                    // Insert the updated lines
                    if (!(startOfFirstLine == null && lineUpdates == null && endOfLastLine == null))
                    {
                        int lineUpdatesCount = (lineUpdates != null && lineUpdates.Count > 0)
                            ? lineUpdates.Count
                            : 1;
                        for (int i = 0; i < lineUpdatesCount; i++)
                        {
                            string newLine = (lineUpdates != null && lineUpdates.Count > 0)
                                ? lineUpdates[i]
                                : string.Empty;
                            if (i == 0)
                            {
                                newLine = startOfFirstLine + newLine;
                            }
                            if (i == lineUpdatesCount - 1)
                            {
                                newLine = newLine + endOfLastLine;
                            }
                            var textChange = new TextChange(TextChangeType.LineInserted, firstLineIndex + i,
                                new TextLineSnapshot(firstLineIndex + i, newLine, null));
                            textChangedEvent.TextChanges.Add(textChange);
                        }
                    }
                }
            }

#endregion

            // Update the source file with the computed text changes
            this.Workspace.UpdateSourceFile(objUri, textChangedEvent);

            // DEBUG information
            RemoteConsole.Log("Udpated source file : " + objUri.LocalPath);
            foreach (var textChange in textChangedEvent.TextChanges)
            {
                RemoteConsole.Log(" - " + textChange.ToString());
            }
        }

        protected override void OnDidCloseTextDocument(DidCloseTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {
                this.Workspace.CloseSourceFile(objUri);
                this.Workspace.DiagnosticsEvent -= DiagnosticsDetected;

                // DEBUG information
                RemoteConsole.Log("Closed source file : " + objUri.LocalPath);
            }
        }

        protected override void OnDidSaveTextDocument(DidSaveTextDocumentParams parameters)
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

            //Commented because it's too slow
            //AnalyticsWrapper.Telemetry.TrackEvent(EventType.Hover, "Hover event", LogType.Completion);
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
                    matchingCodeElement.Line,
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

            List<CompletionItem> items = new List<CompletionItem>();

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
                    AnalyticsWrapper.Telemetry.TrackEvent(EventType.Completion, lastSignificantToken.TokenType.ToString(), LogType.Completion);
                    switch (lastSignificantToken.TokenType)
                    {
                        case TokenType.PERFORM:
                            {
                                items.AddRange(CompletionFactory.GetCompletionPerformParagraph(docContext.FileCompiler,
                                    matchingCodeElement, userFilterToken));
                                break;
                            }
                        case TokenType.CALL:
                            {
                                this.FunctionDeclarations.Clear(); //Clear to avoid key collision
                                items.AddRange(CompletionFactory.GetCompletionForProcedure(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, this.FunctionDeclarations));
                                items.AddRange(CompletionFactory.GetCompletionForLibrary(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken));
                                break;
                            }
                        case TokenType.TYPE:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForType(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken));
                                items.AddRange(CompletionFactory.GetCompletionForLibrary(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken));
                                break;
                            }
                        case TokenType.QualifiedNameSeparator:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForQualifiedName(parameters.position,
                                    docContext.FileCompiler, matchingCodeElement, lastSignificantToken, userFilterToken, this.FunctionDeclarations));
                                break;
                            }
                        case TokenType.INPUT:
                        case TokenType.OUTPUT:
                        case TokenType.IN_OUT:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForProcedureParameter(parameters.position,
                                    docContext.FileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken, this.SignatureCompletionContext));
                                break;
                            }
                        case TokenType.DISPLAY:
                            {
                                System.Linq.Expressions.Expression<Func<DataDefinition, bool>> predicate = dataDefinition =>
                                    dataDefinition.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase) // keep only variables with matching name
                                    && dataDefinition.Usage != DataUsage.ProcedurePointer // invalid usages in DISPLAY statement
                                    && dataDefinition.Usage != DataUsage.FunctionPointer
                                    && dataDefinition.Usage != DataUsage.ObjectReference
                                    && dataDefinition.Usage != DataUsage.Index
                                    && (dataDefinition.CodeElement != null && dataDefinition.CodeElement.LevelNumber.Value < 88);
                                // Ignore level 88. Note that dataDefinition.CodeElement != null condition also filters out IndexDefinition which is invalid in the context of DISPLAY
                                items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement, predicate));
                                break;
                            }
                        case TokenType.MOVE:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    da =>
                                        da.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase) &&
                                        ((da.CodeElement != null &&
                                          da.CodeElement.LevelNumber.Value < 88)
                                         || (da.CodeElement == null && da is IndexDefinition))));
                                //Ignore 88 level variable
                                break;
                            }
                        case TokenType.TO:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForTo(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, lastSignificantToken));
                                break;
                            }
                        case TokenType.INTO:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    v => v.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase)
                                         && (v.CodeElement != null &&
                                             v.DataType == DataType.Alphabetic
                                             || v.DataType == DataType.Alphanumeric
                                             || v.DataType == DataType.AlphanumericEdited)
                                ));
                                break;
                            }
                        case TokenType.SET:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                    v => v.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase)
                                         &&
                                         ((v.CodeElement != null &&
                                           v.CodeElement.LevelNumber.Value == 88)
                                          //Level 88 Variable
                                          || v.DataType == DataType.Numeric //Numeric Integer Variable
                                          || v.Usage == DataUsage.Pointer) //Or usage is pointer 
                                ));
                                break;
                            }
                        case TokenType.OF:
                            {
                                items.AddRange(CompletionFactory.GetCompletionForOf(docContext.FileCompiler, matchingCodeElement,
                                    userFilterToken, parameters.position));
                                break;
                            }
                        default:
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

                        items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler,
                           wrappedCodeElements.First(), da => da.Name.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase)));
                    }
                    else
                    {
                        //Return a default text to inform the user that completion is not available after the given token
                        items.Add(new CompletionItem("Completion is not available in this context")
                        {
                            insertText = ""
                        });
                    }
                }

                if (userFilterToken != null)
                {
                    //Add the range object to let the client know the position of the user filter token
                    var range = new Range(userFilterToken.Line - 1, userFilterToken.StartIndex,
                        userFilterToken.Line - 1, userFilterToken.StopIndex + 1);
                    //-1 on lne to 0 based / +1 on stop index to include the last character
                    items = items.Select(c =>
                    {
                        if (c.data != null && c.data.GetType().IsArray)
                            ((object[])c.data)[0] = range;
                        else
                            c.data = range;
                        return c;
                    }).ToList();
                }
            }


            return items;
        }

        protected override SignatureHelp OnSignatureHelp(TextDocumentPosition parameters)
        {
            AnalyticsWrapper.Telemetry.TrackEvent(EventType.SignatureHelp, "Signature help event", LogType.Completion); //Send event to analytics
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
                    );
            var signatureHelp = new SignatureHelp();

            if (calledProcedures == null)
                return null;

            if (calledProcedures.Count() == 1)
            {
                var calledProcedure = calledProcedures.First();
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
            int previousMatchingWeight = 0, selectedSignatureIndex = 0;

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
                    selectedSignatureIndex++;
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
            AnalyticsWrapper.Telemetry.TrackEvent(EventType.Definition, "Definition event",
                LogType.Completion); //Send event to analytics
            var defaultDefinition = new Definition(parameters.uri, new Range());
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile && this.Workspace.TryGetOpenedDocumentContext(objUri, out var docContext))
            {
                System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);

                if (docContext.FileCompiler.CompilationResultsForProgram != null &&
                    docContext.FileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    var matchingCodeElement =
                        docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot
                            .NodeCodeElementLinkers
                            .Keys.FirstOrDefault(c => c.ConsumedTokens.Any(
                                                          t => t.Line == parameters.position.line + 1 &&
                                                               parameters.position.character >= t.StartIndex &&
                                                               parameters.position.character <= t.StopIndex + 1) &&
                                                      !c.IsInsideCopy());
                    if (matchingCodeElement == null)
                        return defaultDefinition;

                    var matchingNode =
                        docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot
                            .NodeCodeElementLinkers[matchingCodeElement];
                    if (matchingNode == null)
                        return defaultDefinition;

                    var matchingToken = matchingCodeElement.ConsumedTokens.FirstOrDefault(t =>
                        t.Line == parameters.position.line + 1 &&
                        parameters.position.character >= t.StartIndex &&
                        parameters.position.character <= t.StopIndex + 1 &&
                        t.TokenType != TokenType.QualifiedNameSeparator);
                    if (matchingToken == null)
                        return defaultDefinition;

                    Token userFilterToken = null;
                    Token lastSignificantToken = null;
                    var potentialDefinitionNodes = new List<Node>();

                    CodeElementMatcher.MatchCompletionCodeElement(parameters.position,
                        new List<CodeElementWrapper>() {new CodeElementWrapper(matchingCodeElement)},
                        out userFilterToken, out lastSignificantToken); //Magic happens here
                    if (lastSignificantToken != null)
                    {
                        switch (lastSignificantToken.TokenType)
                        {
                            case TokenType.PERFORM:
                            {
                                potentialDefinitionNodes.AddRange(
                                    matchingNode.SymbolTable.GetParagraphs(
                                        p => p.Name.Equals(matchingToken.Text,
                                            StringComparison.OrdinalIgnoreCase)));
                                break;
                            }

                            case TokenType.CALL:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetFunctions(
                                    f => f.Name.Equals(matchingToken.Text, StringComparison.OrdinalIgnoreCase),
                                    SymbolTable.Scope.Program
                                ));
                                break;
                            }

                            case TokenType.TYPE:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetTypes(
                                    t => t.Name.Equals(matchingToken.Text, StringComparison.OrdinalIgnoreCase),
                                    SymbolTable.Scope.Program
                                ));
                                break;
                            }

                            case TokenType.INPUT:
                            case TokenType.OUTPUT:
                            case TokenType.IN_OUT:
                            case TokenType.MOVE:
                            case TokenType.TO:
                            default:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetVariables(
                                    v => v.Name.Equals(matchingToken.Text, StringComparison.OrdinalIgnoreCase),
                                    SymbolTable.Scope.Program));
                                break;
                            }
                        }
                    }

                    if (potentialDefinitionNodes.Count > 0)
                    {
                        var nodeDefinition = potentialDefinitionNodes.FirstOrDefault();
                        if (nodeDefinition != null)
                            return new Definition(parameters.uri,
                                new Range() {start = new Position(nodeDefinition.CodeElement.Line - 1, 0)});
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
