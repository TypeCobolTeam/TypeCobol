using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
using TypeCobol.LanguageServer.Interfaces;
using TypeCobol.LanguageServer.SignatureHelper;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : TypeCobolCustomLanguageServer
    {
        public TypeCobolServer(IRPCServer rpcServer, Queue<MessageActionWrapper> messagesActionsQueue) : base(rpcServer)
        {
            _MessagesActionsQueue = messagesActionsQueue;
            _FunctionDeclarationSignatureDictionary = new Dictionary<SignatureInformation, FunctionDeclaration>();
        }

        // -- Initialization : create workspace and return language server capabilities --
        private Workspace typeCobolWorkspace;
        private Queue<MessageActionWrapper> _MessagesActionsQueue;
        private FunctionDeclaration _SignatureCompletionContext;
        private Dictionary<SignatureInformation, FunctionDeclaration> _FunctionDeclarationSignatureDictionary;
        private TypeCobolRemoteConsole _TCobRemoteControl { get { return (TypeCobolRemoteConsole)RemoteConsole; } }

        /// <summary>
        /// Timer Disabled for TypeCobol.LanguageServer.
        /// </summary>
        public bool TimerDisabledOption
        {
            get; set;
        }

        /// <summary>
        /// True to use ANTLR for parsing a program
        /// </summary>
        public bool UseAntlrProgramParsing { get; set; }

        /// <summary>
        /// true to use Euro-Information replacement rules
        /// </summary>
        public bool UseEuroInformationLegacyReplacingSyntax { get; set; }

        /// <summary>
        /// Lstr Testing Source document
        /// </summary>
        public bool LsrSourceTesting { get; set; }
        /// <summary>
        /// Lstr Testing Source document
        /// </summary>
        public bool LsrScannerTesting { get; set; }
        /// <summary>
        /// Lstr Testing preprocessed Source document
        /// </summary>
        public bool LsrPreprocessTesting { get; set; }
        /// <summary>
        /// Lstr Testing parsing
        /// </summary>
        public bool LsrParserTesting { get; set; }
        /// <summary>
        /// Lstr Testing semantic phase
        /// </summary>
        public bool LsrSemanticTesting { get; set; }
        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        public bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// Are Log message notifications enabled ? false if yes, true otherwise.
        /// </summary>
        public bool NoLogsMessageNotification { get; set; }

        public Workspace Workspace
        {
            get { return typeCobolWorkspace; }
        }

        #region Override LSP Methods

        public override InitializeResult OnInitialize(InitializeParams parameters)
        {
            this.RemoteConsole.NoLogsMessageNotification = NoLogsMessageNotification;
            var rootDirectory = new DirectoryInfo(parameters.rootPath);
            string workspaceName = rootDirectory.Name + "#" + parameters.processId;

            // Initialize the workspace
            typeCobolWorkspace = new Workspace(rootDirectory.FullName, workspaceName, _MessagesActionsQueue, Logger);
            //Propagate LSR testing options.
            if (LsrSourceTesting) typeCobolWorkspace.IsLsrSourceTesting = LsrSourceTesting;
            if (LsrScannerTesting) typeCobolWorkspace.IsLsrScannerTesting = LsrScannerTesting;
            if (LsrPreprocessTesting) typeCobolWorkspace.IsLsrPreprocessinTesting = LsrPreprocessTesting;
            if (LsrParserTesting) typeCobolWorkspace.IsLsrParserTesting = LsrParserTesting;
            if (LsrSemanticTesting) typeCobolWorkspace.IsLsrSemanticTesting = LsrSemanticTesting;
            typeCobolWorkspace.UseSyntaxColoring = UseSyntaxColoring;

            typeCobolWorkspace.UseAntlrProgramParsing = UseAntlrProgramParsing;
            typeCobolWorkspace.UseEuroInformationLegacyReplacingSyntax = UseEuroInformationLegacyReplacingSyntax;
            typeCobolWorkspace.TimerDisabledOption = TimerDisabledOption;
            typeCobolWorkspace.LoadingIssueEvent += LoadingIssueDetected;
            typeCobolWorkspace.ExceptionTriggered += ExceptionTriggered;
            typeCobolWorkspace.WarningTrigger += WarningTrigger;
            // Return language server capabilities
            var initializeResult = base.OnInitialize(parameters);
            initializeResult.capabilities.textDocumentSync = TextDocumentSyncKind.Incremental;
            initializeResult.capabilities.hoverProvider = true;
            CompletionOptions completionOptions = new CompletionOptions();
            completionOptions.resolveProvider = false;
            completionOptions.triggerCharacters = new string[] {"::"};
            initializeResult.capabilities.completionProvider = completionOptions;
            SignatureHelpOptions sigHelpOptions = new SignatureHelpOptions {triggerCharacters = new string[0]};
            initializeResult.capabilities.signatureHelpProvider = sigHelpOptions;

            return initializeResult;
        }
        // -- Files synchronization : maintain a list of opened files, apply all updates to their content -- //

        private bool Logger(string message, Uri uri)
        {
            _TCobRemoteControl.UriLog(message, uri);
            return true;
        }

        public override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {            
            DocumentContext docContext = new DocumentContext(parameters.textDocument);
            if (docContext.Uri.IsFile)
            {
                //Subscribe to diagnostics event
                typeCobolWorkspace.MissingCopiesEvent += MissingCopiesDetected;
                typeCobolWorkspace.DiagnosticsEvent += DiagnosticsDetected;

                //Create a ILanguageServer instance for the document.
                docContext.LanguageServer = new TypeCobolLanguageServer(this.rpcServer, parameters.textDocument);
                docContext.LanguageServer.UseSyntaxColoring = UseSyntaxColoring;

                string text = parameters.text ?? parameters.textDocument.text;
                //These are no longer needed.
                parameters.text = null;
                parameters.textDocument.text = null;                
                typeCobolWorkspace.OpenTextDocument(docContext, text, Workspace.LsrTestOptions);

                // DEBUG information
                RemoteConsole.Log("Opened source file : " + docContext.Uri.LocalPath);
            }
        }

        public override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {

            var docContext = GetDocumentContextFromStringUri(parameters.uri, false); //Text Change do not have to trigger node phase, it's only a another event that will do it
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
                    if(string.IsNullOrEmpty(lineUpdates.FirstOrDefault()) && replacementTextStartsWithNewLine)
                        lineUpdates.RemoveAt(0);
                }

                // Document cleared
                if (contentChange.range == null || contentChange.rangeLength == -1)
                {
                    //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                    //To avoid crashes.
                    try
                    {
                        typeCobolWorkspace.OpenTextDocument(docContext, contentChange.text, this.Workspace.LsrTestOptions);
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
                    string originalFirstLineText =
                        docContext.FileCompiler.CompilationResultsForProgram.CobolTextLines[contentChange.range.start.line]
                            .Text;
                    string originalLastLineText = originalFirstLineText;


                    // Check if the first line was inserted
                    int firstLineIndex = contentChange.range.start.line;
                    int firstLineChar = contentChange.range.start.character;
                    if (replacementTextStartsWithNewLine &&
                        !(contentChange.range.start.character < originalLastLineText.Length))
                    {
                        firstLineIndex++;
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
            typeCobolWorkspace.UpdateSourceFile(objUri, textChangedEvent);

            // DEBUG information
            RemoteConsole.Log("Udpated source file : " + objUri.LocalPath);
            foreach (var textChange in textChangedEvent.TextChanges)
            {
                RemoteConsole.Log(" - " + textChange.ToString());
            }

        }

        public override void OnDidCloseTextDocument(DidCloseTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {
                typeCobolWorkspace.CloseSourceFile(objUri);

                // DEBUG information
                RemoteConsole.Log("Closed source file : " + objUri.LocalPath);
            }
        }

        public override void OnDidSaveTextDocument(DidSaveTextDocumentParams parameters)
        {
            if (parameters.text != null)
            {
                DidChangeTextDocumentParams dctdp = new DidChangeTextDocumentParams(parameters.textDocument.uri);
                TextDocumentContentChangeEvent tdcce = new TextDocumentContentChangeEvent();
                tdcce.text = parameters.text;
                dctdp.contentChanges = new TextDocumentContentChangeEvent[] {tdcce};
                OnDidChangeTextDocument(dctdp);
            }
        }

        public override void OnDidChangeConfiguration(DidChangeConfigurationParams parameters)
        {
            if (parameters.settings is Newtonsoft.Json.Linq.JArray)
            {
                Newtonsoft.Json.Linq.JArray array = parameters.settings as Newtonsoft.Json.Linq.JArray;
                IEnumerable<string> arguments = array.Select(t => t.ToString());
                typeCobolWorkspace.DidChangeConfigurationParams(arguments);
            }
            else
            {
                typeCobolWorkspace.DidChangeConfigurationParams(parameters.settings.ToString());
            }
        }
        // ----------------------------------------------------------------------------------------------- //

        public override Hover OnHover(TextDocumentPosition parameters)
        {
            Hover resultHover = new Hover();

            //Commented because it's too slow
            //AnalyticsWrapper.Telemetry.TrackEvent(EventType.Hover, "Hover event", LogType.Completion);
            var docContext = GetDocumentContextFromStringUri(parameters.uri);
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

            var matchingNode = docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers[((CodeElementWrapper)matchingCodeElement).CodeElement];
            if (matchingNode == null)
                return null;

            //OnHover for data declared with a type
            var dataDefinition = matchingNode as DataDefinition;
            if (dataDefinition?.TypeDefinition != null)
            {
                resultHover.range = new Range(matchingCodeElement.Line, matchingCodeElement.StartIndex,
                    matchingCodeElement.Line,
                    matchingCodeElement.StopIndex + 1);
                resultHover.contents =
                    new MarkedString[] { new MarkedString() { language = "Cobol", value = string.Join("", dataDefinition.TypeDefinition.SelfAndChildrenLines.Select(e => e.Text + "\r\n")) } };
                return resultHover;
            }


            return resultHover;

        }

        /// <summary>
        /// Request to request completion at a given text document position. The request's
        /// parameter is of type[TextDocumentPosition](#TextDocumentPosition) the response
        /// is of type[CompletionItem[]](#CompletionItem) or a Thenable that resolves to such.
        /// </summary>
        public override List<CompletionItem> OnCompletion(TextDocumentPosition parameters)
        {
            var docContext = GetDocumentContextFromStringUri(parameters.uri);
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
                            _FunctionDeclarationSignatureDictionary.Clear(); //Clear to avoid key collision
                            items.AddRange(CompletionFactory.GetCompletionForProcedure(docContext.FileCompiler, matchingCodeElement,
                                userFilterToken, _FunctionDeclarationSignatureDictionary));
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
                                docContext.FileCompiler, matchingCodeElement, lastSignificantToken, userFilterToken, _FunctionDeclarationSignatureDictionary));
                            break;
                        }
                        case TokenType.INPUT:
                        case TokenType.OUTPUT:
                        case TokenType.IN_OUT:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForProcedureParameter(parameters.position,
                                docContext.FileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken, _SignatureCompletionContext));
                            break;
                        }
                        case TokenType.MOVE:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForVariable(docContext.FileCompiler, matchingCodeElement,
                                da =>
                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase) &&
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
                                v => v.Name.StartsWith(userFilterText, StringComparison.CurrentCultureIgnoreCase)
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
                                v => v.Name.StartsWith(userFilterText, StringComparison.CurrentCultureIgnoreCase)
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
                           wrappedCodeElements.First(), da => da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)));
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

        public override Definition OnDefinition(TextDocumentPosition parameters)
        {
            AnalyticsWrapper.Telemetry.TrackEvent(EventType.Definition, "Definition event", LogType.Completion); //Send event to analytics
            var defaultDefinition = new Definition(parameters.uri, new Range());
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                var docContext = typeCobolWorkspace.OpenedDocumentContext[objUri];
                System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);


                if (docContext.FileCompiler.CompilationResultsForProgram != null &&
                    docContext.FileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    var matchingCodeElement =
                        docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers
                            .Keys.FirstOrDefault(c => c.ConsumedTokens.Any(
                                t => t.Line == parameters.position.line + 1 &&
                                     parameters.position.character >= t.StartIndex &&
                                     parameters.position.character <= t.StopIndex + 1) &&
                                     !c.IsInsideCopy());
                    if (matchingCodeElement == null)
                        return defaultDefinition;

                    var matchingNode = docContext.FileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers[matchingCodeElement];
                    if (matchingNode == null)
                        return defaultDefinition;

                    var matchingToken = matchingCodeElement.ConsumedTokens.FirstOrDefault(t =>
                                            t.Line == parameters.position.line + 1 &&
                                            parameters.position.character >= t.StartIndex &&
                                            parameters.position.character <= t.StopIndex+1);
                    if (matchingToken == null)
                        return defaultDefinition;

                    Token userFilterToken = null;
                    Token lastSignificantToken = null;
                    var potentialDefinitionNodes = new List<Node>();

                    CodeElementMatcher.MatchCompletionCodeElement(parameters.position, new List<CodeElementWrapper>() {new CodeElementWrapper(matchingCodeElement)}, out userFilterToken, out lastSignificantToken); //Magic happens here
                    if (lastSignificantToken != null)
                    {
                        switch (lastSignificantToken.TokenType)
                        {
                            case TokenType.PERFORM:
                            {
                                potentialDefinitionNodes.AddRange(
                                    matchingNode.SymbolTable.GetParagraphs(
                                        p => p.Name.Equals(matchingToken.Text, StringComparison.InvariantCultureIgnoreCase)));
                                break;
                            }
                            case TokenType.CALL:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetFunctions(
                                    f => f.Name.Equals(matchingToken.Text, StringComparison.InvariantCultureIgnoreCase),
                                        SymbolTable.Scope.GlobalStorage
                                    ));
                                break;
                            }
                            case TokenType.TYPE:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetTypes(
                                    t => t.Name.Equals(matchingToken.Text, StringComparison.InvariantCultureIgnoreCase),
                                        SymbolTable.Scope.GlobalStorage
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
                                    v => v.Name.Equals(matchingToken.Text, StringComparison.InvariantCultureIgnoreCase),
                                    SymbolTable.Scope.GlobalStorage));
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


        public override void OnDidReceiveSignatureHelpContext(SignatureHelpContextParams parameters)
        {
            if (parameters?.signatureInformation == null) //Means that the client leave the context
            {
                //Make the context signature completion null
                _SignatureCompletionContext = null;
                //Clean up the dictionary
                _FunctionDeclarationSignatureDictionary.Clear();
                return;
            }

            var retrievedFuncDeclarationPair =
                _FunctionDeclarationSignatureDictionary.FirstOrDefault(item => item.Key.Equals(parameters.signatureInformation));

            if (retrievedFuncDeclarationPair.Key != null)
                _SignatureCompletionContext = retrievedFuncDeclarationPair.Value;
        }

        public override SignatureHelp OnSignatureHelp(TextDocumentPosition parameters)
        {
            AnalyticsWrapper.Telemetry.TrackEvent(EventType.SignatureHelp, "Signature help event", LogType.Completion); //Send event to analytics
            var docContext = GetDocumentContextFromStringUri(parameters.uri);
            if (docContext == null)
                return null;
            System.Diagnostics.Debug.Assert(docContext.FileCompiler != null);

            if (docContext.FileCompiler?.CompilationResultsForProgram?.ProcessedTokensDocumentSnapshot == null) //Semantic snapshot is not available
                return null;

            var wrappedCodeElement = CodeElementFinder(docContext.FileCompiler, parameters.position).FirstOrDefault();
            if (wrappedCodeElement == null) //No codeelements found
                return null;

            var node = CompletionFactory.GetMatchingNode(docContext.FileCompiler, wrappedCodeElement);

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
                _SignatureCompletionContext = calledProcedure;

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
            _FunctionDeclarationSignatureDictionary.Clear();
            FunctionDeclaration bestmatchingProcedure = null;
            int previousMatchingWeight = 0, selectedSignatureIndex = 0;

            if (totalGivenParameters == 0)
            {
                foreach (var procedure in calledProcedures) //No parameters given, return all possibilities
                {
                    var formattedSignatureInformation = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(procedure);
                    signatureInformation.Add(formattedSignatureInformation);
                    _FunctionDeclarationSignatureDictionary.Add(formattedSignatureInformation, procedure);
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
                        _FunctionDeclarationSignatureDictionary.Add(formattedSignatureInformation, procedure);

                        previousMatchingWeight = matchingWeight;
                        bestmatchingProcedure = procedure;
                    }
                    selectedSignatureIndex++;
                }
            }

            signatureHelp.signatures = signatureInformation.ToArray();

            if (signatureInformation.Count == 1)
            {
                _SignatureCompletionContext = bestmatchingProcedure; //Set the completion context 
                signatureHelp.activeSignature = 0; //Select the only signature for the client
                signatureHelp.activeParameter = ProcedureSignatureHelper.SignatureHelperParameterSelecter(bestmatchingProcedure, wrappedCodeElement, parameters.position); //Select the current parameter
            }

            return signatureHelp;
        }

        public override void OnShutdown()
        {
            typeCobolWorkspace.MissingCopiesEvent -= MissingCopiesDetected;
            typeCobolWorkspace.DiagnosticsEvent -= DiagnosticsDetected;

            base.OnShutdown();
        }

        public override void OnDidReceiveMissingCopies(MissingCopiesParams parameter)
        {
            typeCobolWorkspace.UpdateMissingCopies(new Uri(parameter.textDocument.uri), parameter.Copies);
        }

        public override void OnDidReceiveNodeRefresh(NodeRefreshParams parameter)
        {
            var context = GetDocumentContextFromStringUri(parameter.textDocument.uri, false);
            if (context != null && context.FileCompiler != null)
            {
                typeCobolWorkspace.RefreshSyntaxTree(context.FileCompiler, true);
            }
        }

        #endregion

        /// <summary>
        /// Event Method triggered when missing copies are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="missingCopies">List of missing copies name</param>
        private void MissingCopiesDetected(object fileUri, MissingCopiesEvent missingCopiesEvent)
        {
            //Send missing copies to client
            var missingCopiesParam = new MissingCopiesParams();
            missingCopiesParam.Copies = missingCopiesEvent.Copies;
            missingCopiesParam.textDocument = new TextDocumentIdentifier(fileUri.ToString());

            SendMissingCopies(missingCopiesParam);
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
                    diag.Message, (DiagnosticSeverity) diag.Info.Severity, diag.Info.Code.ToString(),
                    diag.Info.ReferenceText));
            }

            diagParameter.uri = fileUri.ToString();
            diagParameter.diagnostics = diagList.ToArray();
            SendDiagnostics(diagParameter);
        }

        private void LoadingIssueDetected(object sender, LoadingIssueEvent loadingIssueEvent)
        {
            SendLoadingIssue(new LoadingIssueParams() {Message = loadingIssueEvent.Message});
        }

        private void ExceptionTriggered(object sender, ThreadExceptionEventArgs exception)
        {
            this.NotifyException(exception.Exception);
        }

        private void WarningTrigger(object sender, string message)
        {
            this.NotifyWarning(message);
        }


        private IEnumerable<CodeElementWrapper> CodeElementFinder(FileCompiler fileCompiler, Position position)
        {
            List<CodeElement> codeElements = new List<CodeElement>();
            List<CodeElement> ignoredCodeElements = new List<CodeElement>();
            int lineIndex = position.line;
            // Find the token located below the mouse pointer
            if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines.Count != 0)
            {
                while (codeElements.Count == 0)
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

        private DocumentContext GetDocumentContextFromStringUri(string uri, bool acceptNodeRefresh = true)
        {
            Uri objUri = new Uri(uri);
            if (objUri.IsFile)
            {
                var context = typeCobolWorkspace.OpenedDocumentContext[objUri];
                // Get compilation info for the current file
                if (acceptNodeRefresh)
                    typeCobolWorkspace.RefreshSyntaxTree(context.FileCompiler); //Do a Node Refresh
                return context;
            }

            return null;
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
}
