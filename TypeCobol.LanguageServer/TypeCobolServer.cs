using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Threading;
using Analytics;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServices.Editor;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.LanguageServer.SignatureHelper;
using String = System.String;

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

        public Workspace Workspace
        {
            get { return typeCobolWorkspace; }
        }

        #region Override LSP Methods

        public override InitializeResult OnInitialize(InitializeParams parameters)
        {
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
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {
                //Subscribe to diagnostics event
                typeCobolWorkspace.MissingCopiesEvent += MissingCopiesDetected;
                typeCobolWorkspace.DiagnosticsEvent += DiagnosticsDetected;

                typeCobolWorkspace.OpenSourceFile(objUri,
                    parameters.text != null ? parameters.text : parameters.textDocument.text, Workspace.LsrTestOptions);
                
                // DEBUG information
                RemoteConsole.Log("Opened source file : " + objUri.LocalPath);
            }
        }

        public override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {

            var fileCompiler = GetFileCompilerFromStringUri(parameters.uri, false); //Text Change do not have to trigger node phase, it's only a another event that will do it
            if (fileCompiler == null)
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
                if (contentChange.range == null)
                {
                    //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                    //To avoid crashes.
                    try
                    {
                        typeCobolWorkspace.OpenSourceFile(objUri, contentChange.text, this.Workspace.LsrTestOptions);
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
                else
                {
                    // Get original lines text before change
                    string originalFirstLineText =
                        fileCompiler.CompilationResultsForProgram.CobolTextLines[contentChange.range.start.line]
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
                            fileCompiler.CompilationResultsForProgram.CobolTextLines[
                                Math.Min(lastLineIndex,
                                    fileCompiler.CompilationResultsForProgram.CobolTextLines.Count - 1)].Text;
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
            typeCobolWorkspace.UpdateSourceFile(objUri, textChangedEvent, false);

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
            AnalyticsWrapper.Telemetry.TrackEvent("[LSP] Hover");

            var fileCompiler = GetFileCompilerFromStringUri(parameters.uri);
            if (fileCompiler == null)
                return null;

            // Find the token located below the mouse pointer
            var tokensLine =
                fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.Lines[
                    parameters.position.line];
            var hoveredToken =
                tokensLine.TokensWithCompilerDirectives.First(
                    token =>
                        token.StartIndex <= parameters.position.character &&
                        token.StopIndex >= parameters.position.character);

            // Return a text describing this token
            if (hoveredToken != null)
            {
                string tokenDescription = hoveredToken.TokenFamily.ToString() + " - " +
                                          hoveredToken.TokenType.ToString();
                return new Hover()
                {
                    range =
                        new Range(parameters.position.line, hoveredToken.StartIndex, parameters.position.line,
                            hoveredToken.StopIndex + 1),
                    contents =
                        new MarkedString[] {new MarkedString() {language = "Cobol", value = tokenDescription}}
                };
            }

            return null;
        }

        /// <summary>
        /// Request to request completion at a given text document position. The request's
        /// parameter is of type[TextDocumentPosition](#TextDocumentPosition) the response
        /// is of type[CompletionItem[]](#CompletionItem) or a Thenable that resolves to such.
        /// </summary>
        public override List<CompletionItem> OnCompletion(TextDocumentPosition parameters)
        {
            var fileCompiler = GetFileCompilerFromStringUri(parameters.uri);
            if (fileCompiler == null)
                return null;

            List<CompletionItem> items = new List<CompletionItem>();

            if (fileCompiler.CompilationResultsForProgram != null &&
                fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
            {
                var wrappedCodeElements = CodeElementFinder(fileCompiler, parameters.position);
                if (wrappedCodeElements == null)
                    return new List<CompletionItem>();

                Token userFilterToken = null;
                Token lastSignificantToken = null;
                //Try to get a significant token for competion and return the codeelement containing the matching token.
                CodeElement matchingCodeElement = CodeElementMatcher.MatchCompletionCodeElement(parameters.position,
                    wrappedCodeElements,
                    out userFilterToken, out lastSignificantToken); //Magic happens here
                if (lastSignificantToken != null)
                {
                    AnalyticsWrapper.Telemetry.TrackEvent("[Completion] " + lastSignificantToken.TokenType);
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    switch (lastSignificantToken.TokenType)
                    {
                        case TokenType.PERFORM:
                        {
                            items.AddRange(CompletionFactory.GetCompletionPerformParagraph(fileCompiler,
                                matchingCodeElement, userFilterToken));
                            break;
                        }
                        case TokenType.CALL:
                        {
                            _FunctionDeclarationSignatureDictionary.Clear(); //Clear to avoid key collision
                            items.AddRange(CompletionFactory.GetCompletionForProcedure(fileCompiler, matchingCodeElement,
                                userFilterToken, _FunctionDeclarationSignatureDictionary));
                            items.AddRange(CompletionFactory.GetCompletionForLibrary(fileCompiler, matchingCodeElement,
                                userFilterToken));
                            break;
                        }
                        case TokenType.TYPE:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForType(fileCompiler, matchingCodeElement,
                                userFilterToken));
                            items.AddRange(CompletionFactory.GetCompletionForLibrary(fileCompiler, matchingCodeElement,
                                userFilterToken));
                            break;
                        }
                        case TokenType.QualifiedNameSeparator:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForQualifiedName(parameters.position,
                                fileCompiler, matchingCodeElement, lastSignificantToken, userFilterToken, _FunctionDeclarationSignatureDictionary));
                            break;
                        }
                        case TokenType.INPUT:
                        case TokenType.OUTPUT:
                        case TokenType.IN_OUT:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForProcedureParameter(parameters.position,
                                fileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken, _SignatureCompletionContext));
                            break;
                        }
                        case TokenType.MOVE:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForVariable(fileCompiler, matchingCodeElement,
                                da =>
                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase) &&
                                    ((da.CodeElement != null &&
                                      (da.CodeElement as DataDefinitionEntry).LevelNumber.Value < 88)
                                     || (da.CodeElement == null && da is IndexDefinition))));
                            //Ignore 88 level variable
                            break;
                        }
                        case TokenType.TO:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForTo(fileCompiler, matchingCodeElement,
                                userFilterToken, lastSignificantToken));
                            break;
                        }
                        case TokenType.IF:
                        case TokenType.DISPLAY:
                        {

                            items.AddRange(CompletionFactory.GetCompletionForVariable(fileCompiler, matchingCodeElement,
                                da =>
                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)));
                            break;
                        }
                        case TokenType.SET:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForVariable(fileCompiler, matchingCodeElement,
                                v => v.Name.StartsWith(userFilterText, StringComparison.CurrentCultureIgnoreCase)
                                     &&
                                     (((v.CodeElement as DataDefinitionEntry) != null &&
                                       (v.CodeElement as DataDefinitionEntry).LevelNumber.Value == 88)
                                      //Level 88 Variable
                                      || v.DataType == DataType.Numeric //Numeric Integer Variable
                                      || v.Usage == DataUsage.Pointer) //Or usage is pointer 
                            ));
                            break;
                        }
                        case TokenType.OF:
                        {
                            items.AddRange(CompletionFactory.GetCompletionForOf(fileCompiler, matchingCodeElement,
                                userFilterToken, parameters.position));
                            break;
                        }
                        default:
                            break;
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
                                ((object[]) c.data)[0] = range;
                            else
                                c.data = range;
                            return c;
                        }).ToList();
                    }


                    return items;
                }
                else
                {
                    //Return a default text to inform the user that completion is not available after the given token
                    items.Add(new CompletionItem("Completion is not available in this context, it may come soon.")
                    {
                        insertText = ""
                    });
                    //Send the demand to analytics to let us know what the user wants. 
                    var wrappedCodeEl = wrappedCodeElements.FirstOrDefault();
                    if (wrappedCodeElements.Any() && wrappedCodeEl != null)
                        AnalyticsWrapper.Telemetry.TrackEvent("[Completion-Demand] Source Tokens : " + wrappedCodeEl.InputStream);
                }
            }

            return items;
        }

        public override Definition OnDefinition(TextDocumentPosition parameters)
        {
            AnalyticsWrapper.Telemetry.TrackEvent("[Definition]"); //Send event to analytics
            var defaultDefinition = new Definition(parameters.uri, new Range());
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

                if (fileCompiler.CompilationResultsForProgram != null &&
                    fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    var matchingCodeElement =
                        fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers
                            .Keys.FirstOrDefault(c => c.ConsumedTokens.Any(
                                t => t.Line == parameters.position.line + 1 &&
                                     parameters.position.character >= t.StartIndex &&
                                     parameters.position.character <= t.StopIndex + 1));
                    if (matchingCodeElement == null)
                        return defaultDefinition;

                    var matchingNode = fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers[matchingCodeElement];
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
                                    new List<SymbolTable.Scope>()
                                    {
                                        SymbolTable.Scope.Declarations,
                                        SymbolTable.Scope.Global
                                    }));
                                break;
                            }
                            case TokenType.TYPE:
                            {
                                potentialDefinitionNodes.AddRange(matchingNode.SymbolTable.GetTypes(
                                    t => t.Name.Equals(matchingToken.Text, StringComparison.InvariantCultureIgnoreCase),
                                    new List<SymbolTable.Scope>()
                                    {
                                        SymbolTable.Scope.Declarations,
                                        SymbolTable.Scope.Global
                                    }));
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
                                    new List<SymbolTable.Scope>()
                                    {
                                        SymbolTable.Scope.Declarations,
                                        SymbolTable.Scope.Global
                                    }));
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
            var fileCompiler = GetFileCompilerFromStringUri(parameters.uri);

            if (fileCompiler?.CompilationResultsForProgram?.ProcessedTokensDocumentSnapshot == null) //Semantic snapshot is not available
                return null;

            var wrappedCodeElement = CodeElementFinder(fileCompiler, parameters.position).FirstOrDefault();
            if (wrappedCodeElement == null) //No codeelements found
                return null;

            var node = CompletionFactory.GetMatchingNode(fileCompiler, wrappedCodeElement);

            //Get procedure name or qualified name
            string procedureName = CompletionFactoryHelpers.GetProcedureNameFromTokens(wrappedCodeElement.ArrangedConsumedTokens);

            //Try to get procedure by its name
            var calledProcedures =
                node.SymbolTable.GetFunctions(
                    p =>
                        p.Name.Equals(procedureName) ||
                        p.QualifiedName.ToString().Equals(procedureName), new List<SymbolTable.Scope>
                    {
                        SymbolTable.Scope.Declarations,
                        SymbolTable.Scope.Intrinsic,
                        SymbolTable.Scope.Namespace
                    });
            var signatureHelp = new SignatureHelp();

            if (calledProcedures == null)
                return null;

            if (calledProcedures.Count == 1)
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
            var fileCompiler = GetFileCompilerFromStringUri(parameter.textDocument.uri, false);
            typeCobolWorkspace.RefreshSyntaxTree(fileCompiler);
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
                        if (!tempCodeElement.ConsumedTokens.Any(t => CompletionElligibleTokens.IsCompletionElligibleToken(t) &&
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

            //Create a list of CodeElementWrapper in order to loose the ConsumedTokens ref. 
            return codeElements.Select(c => new CodeElementWrapper(c));
        }

        private FileCompiler GetFileCompilerFromStringUri(string uri, bool acceptNodeRefresh = true)
        {
            Uri objUri = new Uri(uri);
            if (objUri.IsFile)
            {
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];
                // Get compilation info for the current file
                if (acceptNodeRefresh)
                    typeCobolWorkspace.RefreshSyntaxTree(fileCompiler); //Do a Node Refresh
                return fileCompiler;
            }

            return null;
        }

    }
    public class CodeElementWrapper : CodeElement
    {
        public CodeElementWrapper(CodeElement codeElement)
            : base(codeElement.Type)
        {
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
