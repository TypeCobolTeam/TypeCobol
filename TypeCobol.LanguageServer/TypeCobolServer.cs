using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using Analytics;
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

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : TypeCobolCustomLanguageServer
    {
        public TypeCobolServer(IRPCServer rpcServer) : base(rpcServer)
        {
        }

        // -- Initialization : create workspace and return language server capabilities --
        private Workspace typeCobolWorkspace;

        #region Override LSP Methods

        public override InitializeResult OnInitialize(InitializeParams parameters)
        {
            var rootDirectory = new DirectoryInfo(parameters.rootPath);
            string workspaceName = rootDirectory.Name + "#" + parameters.processId;

            // Initialize the workspace
            typeCobolWorkspace = new Workspace(rootDirectory.FullName, workspaceName);
            typeCobolWorkspace.LoadingIssueEvent += LoadingIssueDetected;

            //Simulate Configuration change
            //typeCobolWorkspace.DidChangeConfigurationParams(@"-o C:\TypeCobol\Test.cee -d C:\TypeCobol\Test.xml -s C:\TypeCobol\Sources\##Latest_Release##\skeletons.xml -e rdz -y C:\TypeCobol\Sources\##Latest_Release##\Intrinsic\Intrinsic.txt --autoremarks --dependencies C:\TypeCobol\Sources\##Latest_Release##\Dependencies\*.tcbl");

            // DEBUG information
            RemoteWindow.ShowInformationMessage("TypeCobol language server was launched !");

            // Return language server capabilities
            var initializeResult = base.OnInitialize(parameters);
            initializeResult.capabilities.textDocumentSync = TextDocumentSyncKind.Incremental;
            initializeResult.capabilities.hoverProvider = true;
            CompletionOptions completionOptions = new CompletionOptions();
            completionOptions.resolveProvider = false;
            completionOptions.triggerCharacters = new string[] {"::"};
            initializeResult.capabilities.completionProvider = completionOptions;

            return initializeResult;
        }
        // -- Files synchronization : maintain a list of opened files, apply all updates to their content -- //
        public override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {

                typeCobolWorkspace.OpenSourceFile(objUri,
                    parameters.text != null ? parameters.text : parameters.textDocument.text);

                //Subscribe to diagnostics event
                typeCobolWorkspace.MissingCopiesEvent += MissingCopiesDetected;
                typeCobolWorkspace.DiagnosticsEvent += DiagnosticsDetected;

                // DEBUG information
                RemoteConsole.Log("Opened source file : " + objUri.LocalPath);
            }
        }

        public override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

                #region Convert text changes format from multiline range replacement to single line updates

                TextChangedEvent textChangedEvent = new TextChangedEvent();
                foreach (var contentChange in parameters.contentChanges)
                {
                    // Split the text updated into distinct lines
                    List<string> lineUpdates = null;
                    bool replacementTextStartsWithNewLine = false;

                    if (contentChange.text != null && contentChange.text.Length > 0)
                    {
                        replacementTextStartsWithNewLine = contentChange.text[0] == '\r' ||
                                                           contentChange.text[0] == '\n';
                            //Allow to know if a new line was added
                        lineUpdates =
                            contentChange.text.Split(new char[] {'\r', '\n'}, StringSplitOptions.RemoveEmptyEntries)
                                .ToList(); //Split on /r /n to know the number of lines added
                    }

                    // Document cleared
                    if (contentChange.range == null)
                    {
                        //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                        //To avoid crashes.
                        try
                        {
                            typeCobolWorkspace.OpenSourceFile(objUri, contentChange.text);
                        }
                        catch (Exception e)
                        {
//Don't rethow an exception on save.
                            RemoteConsole.Error(string.Format("Error while handling notification {0} : {1}",
                                "textDocument/didChange", e.Message));
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
                                ///Add the default 7 spaces + add lineUpdates in order to update the current line and add the new one. 
                        }

                        // Check if the last line was deleted
                        int lastLineIndex = contentChange.range.end.line;
                        if (contentChange.range.end.line > contentChange.range.start.line &&
                            contentChange.range.end.character == 0)
                        {
                           //Allows to detect if the next line was supressed
                        }
                        if (contentChange.text.Length == 0)
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
                            startOfFirstLine = originalFirstLineText.Substring(0, contentChange.range.start.character);
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
                                //Mark the index line to be removed. The index will remains the same for each line delete, beacause text change are apply one after another
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
//So Call OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
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
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                // Get compilation info for the current file
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

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
            Uri objUri = new Uri(parameters.uri);
            bool temp;
            if (objUri.IsFile)
            {
                // Get compilation info for the current file
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

                if (fileCompiler.CompilationResultsForProgram != null &&
                    fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    List<CodeElement> codeElements = new List<CodeElement>();
                    List<CodeElement> ignoredCodeElements = new List<CodeElement>();
                    int lineIndex = parameters.position.line;
                    // Find the token located below the mouse pointer
                    while (codeElements == null || codeElements.Count == 0)
                    {
                        var codeElementsLine =
                            fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines[lineIndex];

                        if (codeElementsLine != null && codeElementsLine.CodeElements != null)
                        {
                            //Ignore all the EndOfFile token 
                            var tempCodeElements =
                                codeElementsLine.CodeElements.Where(
                                    c => c.ConsumedTokens.Any(t => t.TokenType != TokenType.EndOfFile));

                            foreach (var tempCodeElement in tempCodeElements.Reverse())
                            {
                                if (!tempCodeElement.ConsumedTokens.Any(t => IsCompletionElligibleToken(t, out temp) &&
                                ((t.Line == parameters.position.line +1 && t.StopIndex +1 <= parameters.position.character) || t.Line < parameters.position.line + 1)))
                                    ignoredCodeElements.Add(tempCodeElement);
                                else
                                    codeElements.Add(tempCodeElement);
                            }

                            if(tempCodeElements.Any(c => c.ConsumedTokens.Any(t => t.TokenType == TokenType.PeriodSeparator && !(t is Compiler.AntlrUtils.MissingToken))))
                                break;
                        }

                        lineIndex--; //decrease lineIndex to get the previous line of TypeCobol Tree.
                    }

                    codeElements.AddRange(ignoredCodeElements);
                        //Add the previously ignored Code Elements, may be they are usefull to help completion.

                    if (!codeElements.Any(c => c.ConsumedTokens.Any(t => t.Line <= parameters.position.line + 1)))
                        return new List<CompletionItem>(); //If nothing is found near the cursor we can't do completion

                    var finalList = codeElements.Select(c => new CodeElementWrapper(c));
                        //Create a list of CodeElementWrapper in prder to loose the ConsumedTokens ref. 

                    Token userFilterToken = null;
                    Token lastSignificantToken = null;
                    //Try to get a significant token for competion and return the codeelement containing the matching token.
                    CodeElement matchingCodeElement = MatchCompletionCodeElement(parameters.position, finalList,
                        out userFilterToken, out lastSignificantToken); //Magic happens here
                    if (lastSignificantToken != null)
                    {
                        AnalyticsWrapper.Telemetry.TrackEvent("[Completion] " + lastSignificantToken.TokenType);
                        List<CompletionItem> items = new List<CompletionItem>();
                        switch (lastSignificantToken.TokenType)
                        {
                            case TokenType.PERFORM:
                            {
                                items.AddRange(GetCompletionPerformParagraph(fileCompiler, matchingCodeElement, userFilterToken));
                                break;
                            }
                            case TokenType.CALL:
                            {
                                items.AddRange(GetCompletionForProcedure(fileCompiler, matchingCodeElement, userFilterToken));
                                items.AddRange(GetCompletionForLibrary(fileCompiler, matchingCodeElement, userFilterToken));
                                break;
                            }
                            case TokenType.TYPE:
                            {
                                items.AddRange(GetCompletionForType(fileCompiler, matchingCodeElement, userFilterToken));
                                items.AddRange(GetCompletionForLibrary(fileCompiler, matchingCodeElement, userFilterToken));
                                break;
                            }
                            case TokenType.QualifiedNameSeparator:
                            {
                                items.AddRange(GetCompletionForQualifiedName(parameters.position, fileCompiler, matchingCodeElement, lastSignificantToken, userFilterToken));
                                break;
                            }
                            case TokenType.INPUT:
                            case TokenType.OUTPUT:
                            case TokenType.IN_OUT:
                            {
                                items.AddRange(GetCompletionForProcedureParameter(parameters.position, fileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken));
                                break;
                            }
                            case TokenType.MOVE:
                            {
                                var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                                items.AddRange(GetCompletionForVariable(fileCompiler, matchingCodeElement, da => da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)));
                                break;
                            }
                            case TokenType.TO:
                            {
                                items.AddRange(GetCompletionForTo(fileCompiler, matchingCodeElement, userFilterToken, lastSignificantToken));
                                break;
                            }
                            default:
                                break;
                        }

                        if (userFilterToken != null)
                        {
                            //Add the range object to let the client know the position of the user filter token
                            items = items.Select(c =>
                            {
                                //-1 on lne to 0 based / +1 on stop index to include the last character
                                c.data = new Range(userFilterToken.Line - 1, userFilterToken.StartIndex,
                                    userFilterToken.Line - 1, userFilterToken.StopIndex + 1);
                                return c;
                            }).ToList();
                        }


                        return items;
                    }
                }
            }
            return new List<CompletionItem>();
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

                    MatchCompletionCodeElement(parameters.position, new List<CodeElementWrapper>() {new CodeElementWrapper(matchingCodeElement)}, out userFilterToken, out lastSignificantToken); //Magic happens here
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

        #region Completion Methods

        /// <summary>
        /// Get the paragraph that can be associated to PERFORM Completion token.
        /// </summary>
        /// <param name="fileCompiler">The target FileCompiler instance</param>
        /// <param name="performToken">The PERFORM token</param>
        /// <returns></returns>
        private IEnumerable<CompletionItem> GetCompletionPerformParagraph(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var performNode = GetMatchingNode(fileCompiler, codeElement);
            List<Paragraph> pargraphs = null;
            List<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();

            if (performNode != null)
            {
                if (performNode.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    pargraphs = performNode.SymbolTable.GetParagraphs(p => p.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase));
                    variables = performNode.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                           da.DataType ==
                                                                           Compiler.CodeElements.DataType.Numeric &&
                                                                           da.Name.StartsWith(userFilterText,
                                                                               StringComparison
                                                                                   .InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope> {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global});
                }
            }

            if (pargraphs != null)
            {
                completionItems.AddRange(pargraphs.Select(para => new CompletionItem(para.Name)));
            }
            if (variables != null)
            {
                foreach (var variable in variables)
                {
                    var completionItem =
                        new CompletionItem(string.Format("{0} PIC{1}", variable.Name, variable.Picture.NormalizedValue));
                    completionItem.insertText = variable.Name;
                    completionItems.Add(completionItem);
                }
            }

            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForProcedure(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            var procedures = new List<FunctionDeclaration>();
            var variables = new List<DataDefinition>();
            var completionItems = new List<CompletionItem>();

            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    procedures =
                        node.SymbolTable.GetFunctions(
                            f =>
                                f.QualifiedName.ToString()
                                    .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                || f.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                            new List<SymbolTable.Scope>
                            {
                                SymbolTable.Scope.Declarations,
                                SymbolTable.Scope.Intrinsic,
                                SymbolTable.Scope.Namespace
                            });
                    variables = node.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                    da.DataType ==
                                                                    Compiler.CodeElements.DataType.Alphanumeric &&
                                                                    da.Name.StartsWith(userFilterText,
                                                                        StringComparison.InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope> {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global});
                }
            }



            completionItems = CreateCompletionItemsForProcedures(procedures, node).ToList();

            foreach (var variable in variables)
            {
                var completionItem = new CompletionItem(string.Format("{0}", variable.Name));
                completionItem.insertText = variable.Name;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForLibrary(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var callNode = GetMatchingNode(fileCompiler, codeElement);
            List<Program> programs = new List<Program>();
            if (callNode != null)
            {
                if (callNode.SymbolTable != null)
                {
                    programs =
                        callNode.SymbolTable.GetPrograms(userFilterToken != null ? userFilterToken.Text : string.Empty);
                }
            }

            var completionItems = new List<CompletionItem>();
            foreach (var prog in programs)
            {
                var completionItem = new CompletionItem(prog.Name);
                completionItem.kind = CompletionItemKind.Class;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            var types = new List<TypeDefinition>();
            var completionItems = new List<CompletionItem>();

            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    types =
                        node.SymbolTable.GetTypes(
                            t => t.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                 ||
                                 t.QualifiedName.ToString()
                                     .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                            new List<SymbolTable.Scope>
                            {
                                SymbolTable.Scope.Declarations,
                                SymbolTable.Scope.Global,
                                SymbolTable.Scope.Intrinsic,
                                SymbolTable.Scope.Namespace
                            });
                }
            }

            return CreateCompletionItemsForType(types, node);
        }

        private IEnumerable<CompletionItem> GetCompletionForQualifiedName(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token qualifiedNameSeparatorToken, Token userFilterToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;


            //Get the token before MatchingToken 
            var userTokenToSeek =
                arrangedCodeElement.ArrangedConsumedTokens.ElementAt(
                    arrangedCodeElement.ArrangedConsumedTokens.IndexOf(qualifiedNameSeparatorToken) - 1);
            var tokensToExcept = new List<Token>();
            tokensToExcept.AddRange(
                    arrangedCodeElement.ArrangedConsumedTokens.Where(
                        t => t.TokenType == TokenType.UserDefinedWord || t.TokenType == TokenType.QualifiedNameSeparator));
            //Remove all the userdefinedword token and also QualifiedNameToken
            arrangedCodeElement.ArrangedConsumedTokens = arrangedCodeElement.ArrangedConsumedTokens.Except(tokensToExcept).ToList();
            //We only wants the token that in front of any QualifiedName 
            //Get the first significant token (i.e CALL/TYPE/...)
            Token firstSignificantToken;
            MatchCompletionCodeElement(position, new List<CodeElementWrapper> {arrangedCodeElement}, out userFilterToken,
                out firstSignificantToken);

            //For MOVE INPUT OUTPUT variables etc.. , get all the childrens of a variable that are accessible
            //Try to find corresponding variables
            var possibleVariables = node.SymbolTable.GetVariables(
                                    v => v.Name.Equals(userTokenToSeek.Text, StringComparison.InvariantCultureIgnoreCase),
                                    new List<SymbolTable.Scope> {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global});

            if (possibleVariables != null && possibleVariables.Count > 0)
            {
                //Get childrens of a type to get completion possibilities
                foreach (var variable in possibleVariables)
                {
                    var childrens = new List<Node>();
                    if (variable.Children != null && variable.Children.Count > 0) //It's a variable with levels inside
                        childrens.AddRange(variable.Children);
                    else //It's a typed variable, we have to search for childrens in the type
                    {
                        var type =
                            node.SymbolTable.GetTypes(
                                t => t.Name.Equals(variable.DataType.Name, StringComparison.InvariantCultureIgnoreCase)
                                     ||
                                     t.QualifiedName.ToString()
                                         .Equals(variable.DataType.Name, StringComparison.InvariantCultureIgnoreCase),
                                new List<SymbolTable.Scope>
                                {
                                    SymbolTable.Scope.Declarations,
                                    SymbolTable.Scope.Global,
                                    SymbolTable.Scope.Intrinsic,
                                    SymbolTable.Scope.Namespace
                                }).FirstOrDefault();

                        if (type != null)
                            childrens.AddRange(type.Children);
                    }

                    completionItems.AddRange(childrens.Where(c => c.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)).Select(child => new CompletionItem(child.Name)));
                }
            }

            if (firstSignificantToken != null)
            {
                switch (firstSignificantToken.TokenType)
                {
                    case TokenType.CALL:
                    {
                        //On CALL get possible procedures and functions in the seeked program
                        var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                        if (programs != null && programs.Count > 0)
                        {
                            var procedures =
                                programs.First()
                                    .SymbolTable.GetFunctions(
                                        f =>
                                            f.Name.StartsWith(userFilterText,
                                                StringComparison.InvariantCultureIgnoreCase) ||
                                            f.QualifiedName.ToString()
                                                .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                        new List<SymbolTable.Scope>
                                        {
                                            SymbolTable.Scope.Declarations
                                        });
                            completionItems.AddRange(CreateCompletionItemsForProcedures(procedures, node, false));

                        }
                        break;
                    }
                    case TokenType.TYPE:
                    {
                        //On TYPE get possible public types in the seeked program
                        var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                        if (programs != null && programs.Count > 0)
                        {
                            var types =
                                programs.First()
                                    .SymbolTable.GetTypes(
                                        t =>
                                            t.Name.StartsWith(userFilterText,
                                                StringComparison.InvariantCultureIgnoreCase),
                                        new List<SymbolTable.Scope>
                                        {
                                            SymbolTable.Scope.Declarations,
                                            SymbolTable.Scope.Global
                                        });
                            completionItems.AddRange(CreateCompletionItemsForType(types, node, false));
                        }
                        break;
                    }
                }
            }

            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForProcedureParameter(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            //Get procedure name or qualified name
            string procedureName = string.Empty;
            if (arrangedCodeElement.ArrangedConsumedTokens.Any(t => t.TokenType == TokenType.QualifiedNameSeparator))
            {
                procedureName = arrangedCodeElement.ArrangedConsumedTokens
                    .TakeWhile(t => t.TokenType != TokenType.QualifiedNameSeparator).LastOrDefault().Text + ".";
            }

            procedureName = procedureName + arrangedCodeElement.ArrangedConsumedTokens
                                .TakeWhile( t => t.TokenType != TokenType.INPUT 
                                                && t.TokenType != TokenType.OUTPUT 
                                                && t.TokenType != TokenType.IN_OUT)
                                .LastOrDefault(t => t.TokenFamily != TokenFamily.Whitespace).Text;
           

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


            int alreadyGivenParametersCount = arrangedCodeElement.ArrangedConsumedTokens
                .SkipWhile(t => t != lastSignificantToken).Skip(1)
                .TakeWhile(t => t.TokenType != TokenType.OUTPUT && t.TokenType != TokenType.IN_OUT)
                .Except(new List<Token>() {userFilterToken})
                .Count(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line <= position.line + 1);

            var potentialVariablesForCompletion = new List<DataDefinition>();
            foreach (var procedure in calledProcedures)
            {
                IEnumerable<ParameterDescription> procParams = null;
                //Switch to select the correct parameters profile
                #region Selective parameters Switch
                switch (lastSignificantToken.TokenType)
                {
                    case TokenType.INPUT:
                    {
                        procParams = procedure.Profile.InputParameters;
                        break;
                    }
                    case TokenType.OUTPUT:
                    {
                        procParams = procedure.Profile.OutputParameters;
                        break;
                    }
                    case TokenType.IN_OUT:
                    {
                        procParams = procedure.Profile.InoutParameters;
                        break;
                    }
                    default:
                        procParams = new List<ParameterDescription>();
                        break;
                }

                #endregion

                //If the user already written all or more parameters than required let's check for an other proc signature
                if (alreadyGivenParametersCount >= procParams.Count())
                    continue;
                
                //Else see which parameter could be filled
                var parameterToFill = procParams.ToArray()[alreadyGivenParametersCount];
                //Get local/global variable that could correspond to the parameter
                potentialVariablesForCompletion.AddRange(
                    node.SymbolTable.GetVariables(v => v.DataType == parameterToFill.DataType &&
                                                       v.Name.StartsWith(userFilterText,
                                                           StringComparison.InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope>() {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global}));

            }


            //Add potential variables to completionItems
            completionItems.AddRange(potentialVariablesForCompletion.Distinct().Select(v => new CompletionItem(v.Name)));

            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForVariable(FileCompiler fileCompiler, CodeElement codeElement, Expression<Func<DataDefinition, bool>> predicate)
        {
            var completionItems = new List<CompletionItem>();
            var node = GetMatchingNode(fileCompiler, codeElement);
            List<DataDefinition> variables = null;

            variables = node.SymbolTable.GetVariables(predicate, new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
            completionItems.AddRange(variables.Select(v => new CompletionItem(v.Name)));

            return completionItems;
        }

        private IEnumerable<CompletionItem> GetCompletionForTo(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            List<DataDefinition> variables = new List<DataDefinition>();
            var firstReferences = new List<Node>();

            var firstReferenceToken = arrangedCodeElement.ArrangedConsumedTokens.TakeWhile(t => t != lastSignificantToken).LastOrDefault();
            if (firstReferenceToken == null) 
                return completionItems;

            firstReferences.AddRange(
                node.SymbolTable.GetVariables(
                    da => da.Name.Equals(firstReferenceToken.Text, StringComparison.InvariantCultureIgnoreCase),
                    new List<SymbolTable.Scope> {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global}));

            if (firstReferences != null && firstReferences.Count == 0) //No variable found, it mays come from a type.
            {
                var possibleTypes = node.SymbolTable.GetTypes(t => t.Children != null && t.Children.Any(da => da!=null && da.Name!=null && da.Name.Equals(firstReferenceToken.Text, StringComparison.InvariantCultureIgnoreCase)),
                                                            new List<SymbolTable.Scope>
                                                            {
                                                                SymbolTable.Scope.Declarations,
                                                                SymbolTable.Scope.Global,
                                                                SymbolTable.Scope.Intrinsic,
                                                                SymbolTable.Scope.Namespace
                                                            });

                firstReferences.AddRange(possibleTypes.SelectMany(t => t.Children.Where(c => c.Name.Equals(firstReferenceToken.Text, StringComparison.InvariantCultureIgnoreCase))));
            }

            foreach (var firstReference in firstReferences)
            {
                variables.AddRange(node.SymbolTable.GetVariables(da => da.DataType == (firstReference as DataDefinition).DataType, new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global }));
            }
         
            completionItems.AddRange(variables.Distinct().Select(v => new CompletionItem(v.Name)));

            return completionItems;
        }

        private IEnumerable<CompletionItem> CreateCompletionItemsForType(List<TypeDefinition> types, Node node, bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var type in types)
            {
                bool typeIsPublic = false;
                if (enablePublicFlag)
                    typeIsPublic = (type.CodeElement as DataTypeDescriptionEntry).Visibility ==
                                   Compiler.CodeElements.AccessModifier.Public
                                   &&
                                   !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations)
                                         .Types.Values.Any(t => t.Contains(type))
                                     //Ignore public if type is in the current program
                                     || type.IsIntrinsic); //Ignore public if type is in intrinsic

                var typeDisplayName = typeIsPublic ? type.QualifiedName.ToString() : type.Name;
                var completionItem = new CompletionItem(typeDisplayName);
                completionItem.insertText = typeIsPublic
                    ? string.Format("{0}::{1}", type.QualifiedName.Tail, type.QualifiedName.Head)
                    : type.Name;
                completionItem.kind = CompletionItemKind.Class;
                completionItems.Add(completionItem);
            }
            return completionItems;
        }

        private IEnumerable<CompletionItem> CreateCompletionItemsForProcedures(List<FunctionDeclaration> procedures, Node node, bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var proc in procedures)
            {
                string inputParams = null, outputParams = null, inoutParams = null;

                if (proc.Profile != null)
                {
                    if (proc.Profile.InputParameters != null && proc.Profile.InputParameters.Count > 0)
                        inputParams = string.Format("INPUT: {0}",
                            string.Join(", ",
                                proc.Profile.InputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.OutputParameters != null && proc.Profile.OutputParameters.Count > 0)
                        outputParams = string.Format("| OUTPUT: {0}",
                            string.Join(", ",
                                proc.Profile.OutputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.InoutParameters != null && proc.Profile.InoutParameters.Count > 0)
                        inoutParams = string.Format("| INOUT: {0}",
                            string.Join(", ",
                                proc.Profile.InoutParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                }
                bool procIsPublic = false;
                if (enablePublicFlag)
                    procIsPublic = (proc.CodeElement as FunctionDeclarationHeader).Visibility ==
                                   Compiler.CodeElements.AccessModifier.Public
                                   &&
                                   !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations)
                                         .Functions.Values.Any(t => t.Contains(proc))
                                     //Ignore public if proc is in the current program
                                     || proc.IsIntrinsic); //Ignore public if proc is in intrinsic;
                var procDisplayName = procIsPublic ? proc.QualifiedName.ToString() : proc.Name;
                var completionItem =
                    new CompletionItem(string.Format("{0} ({1} {2} {3})", procDisplayName, inputParams, outputParams,
                        inoutParams));
                completionItem.insertText = procIsPublic
                    ? string.Format("{0}::{1}", proc.QualifiedName.Tail, proc.QualifiedName.Head)
                    : proc.Name;
                completionItem.kind = proc.Profile.IsFunction ? CompletionItemKind.Function : CompletionItemKind.Method;
                completionItems.Add(completionItem);
            }

            return completionItems;
        }
        #endregion

        #region Helpers

        /// <summary>
        /// Get the matchig node for a given Token and a gien completion mode. Returning a matching Node or null.
        /// </summary>
        /// <param name="fileCompiler"></param>
        /// <param name="codeElement"></param>
        /// <returns></returns>
        private Node GetMatchingNode(FileCompiler fileCompiler, CodeElement codeElement)
        {
            if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot != null
                && fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers != null)
            {
                return
                    fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers
                        .FirstOrDefault(t => t.Key.Equals(codeElement)).Value;
            }

            return null;
        }

        /// <summary>
        /// Completion Tokens is an array of Tuple<TokenType, AllowLastPos> for each token that can target a completion.
        /// The Flag AllowLastPos says that if the cursor is at the last position of a token that the token can
        /// be conidered a completion token. For instance the for PERFORM token the last position is not allowed:
        /// PERFORM
        ///        ^
        /// that is to say if the cursor is just after the M that no completion should occurs.
        /// </summary>
        private static Tuple<TokenType, bool>[] elligibleCompletionTokens = {
            new Tuple<TokenType, bool>(TokenType.PERFORM, false),
            new Tuple<TokenType, bool>(TokenType.CALL, false),
            new Tuple<TokenType, bool>(TokenType.TYPE, false),
            new Tuple<TokenType, bool>(TokenType.QualifiedNameSeparator, true),
            new Tuple<TokenType, bool>(TokenType.INPUT, false),
            new Tuple<TokenType, bool>(TokenType.OUTPUT, false),
            new Tuple<TokenType, bool>(TokenType.IN_OUT, false),
            new Tuple<TokenType, bool>(TokenType.MOVE, false),
            new Tuple<TokenType, bool>(TokenType.TO, false)
        };

        /// <summary>
        /// Determines if the given token is elligible for a completion
        /// </summary>
        /// <param name="token"></param>
        /// <param name="bAllowLastPos"></param>
        /// <returns></returns>
        private static bool IsCompletionElligibleToken(Token token, out bool bAllowLastPos)
        {
            bAllowLastPos = false;
            for (int i = 0; i < elligibleCompletionTokens.Length; i++)
            {
                if (elligibleCompletionTokens[i].Item1 == token.TokenType)
                {
                    bAllowLastPos = elligibleCompletionTokens[i].Item2;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// This method will try to found the best significant token that code be used fo completion. It depends on the given CodeELements and Position. 
        /// It will also return the CodeElemet that contains the significant token detected. 
        /// </summary>
        /// <param name="position">Parameter that specifie the position of the cursor in the document</param>
        /// <param name="codeElements">List of codeElements that are concerned by the completion</param>
        /// <param name="userFilterToken">Out parameter that returns a UserDefinedWork token</param>
        /// <param name="lastSignificantToken">Out parameter that returns the Significant token detected for completion</param>
        /// <returns></returns>
        private static CodeElement MatchCompletionCodeElement(Position position,
            IEnumerable<CodeElementWrapper> codeElements, out Token userFilterToken, out Token lastSignificantToken)
        {
            bool bAllowLastPos = false;
            lastSignificantToken = null;
            CodeElement significantCodeElement = null;
            userFilterToken = null;

            //Filter CodeElements 
            codeElements =
                codeElements.Where(
                    c =>
                        c.ArrangedConsumedTokens.Any(
                            t =>
                                (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) ||
                                (t.Line <= position.line + 1))).ToList();
                //Select all the code elements that have a Consumed Tokens before the cursor.
            var closestTokenToCursor =
                codeElements.Select(
                        c =>
                            c.ArrangedConsumedTokens.LastOrDefault(
                                t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character))).Where(t => t!=null)
                    .OrderBy(t =>  Math.Abs(position.character - t.StopIndex + 1)) //Allows to get the token closest to the cursor
                    .FirstOrDefault();

            if (closestTokenToCursor != null && closestTokenToCursor.Line == position.line + 1 &&
                position.character > closestTokenToCursor.StartIndex &&
                closestTokenToCursor.StopIndex + 1 >= position.character)
                //the cursor is at the end or in the middle of a token. 
            {
                if (closestTokenToCursor.StopIndex + 1 == position.character &&
                    IsCompletionElligibleToken(closestTokenToCursor, out bAllowLastPos) && bAllowLastPos == true)
                    //Detect if token is eligible and if the cursor is at the end of the token
                {
                    //the completion has to start from this token and this codeElement
                    codeElements =
                        codeElements.ToList()
                            .SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor))
                            .ToList();
                }
                else
                {
                    var tempCodeElements =
                        codeElements.TakeWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList();

                    if (!tempCodeElements.Any())
                        //In case there is only one codeElement the TakeWhile will not be able to get the last CodeElement, so we have to do it manually.
                        tempCodeElements.Add(codeElements.FirstOrDefault());
                    else
                        codeElements = tempCodeElements;

                    //The closestToken to cursor as to be added to this codeElement as a userdefinedword
                    if (closestTokenToCursor.TokenType != TokenType.UserDefinedWord)
                    {
                        codeElements.LastOrDefault()
                            .ArrangedConsumedTokens.Add(new Token(TokenType.UserDefinedWord,
                                                        closestTokenToCursor.StartIndex,
                                                        closestTokenToCursor.StopIndex, closestTokenToCursor.TokensLine));

                        foreach (var codeElement in codeElements)
                        {
                            if (codeElement.ArrangedConsumedTokens.Contains(closestTokenToCursor) && closestTokenToCursor.TokenType != TokenType.UserDefinedWord)
                                codeElement.ArrangedConsumedTokens.Remove(closestTokenToCursor);
                        }
                       
                    }
               
                }
            }
            else if (closestTokenToCursor != null)
            {
                //the completion has to start from this token and this codeElement
                codeElements =
                    codeElements.ToList()
                        .SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor))
                        .ToList();
            }

            foreach (var codeElement in codeElements)
            {
                var consumedTokens =
                    codeElement.ArrangedConsumedTokens.Where(
                        t =>
                            ((t.StartIndex <= position.character && t.Line <= position.line + 1) ||
                            t.Line < position.line + 1) &&
                            t.TokenFamily != TokenFamily.Whitespace);

                

                if (consumedTokens != null && consumedTokens.Any())
                {
                    foreach (var finalToken in consumedTokens)
                    {
                        if (finalToken.StartIndex > position.character && !(finalToken.Line < position.line + 1))
                            break;

                        if (IsCompletionElligibleToken(finalToken, out bAllowLastPos) &&
                            (finalToken.StopIndex + 1 <= position.character || finalToken.Line <= position.line + 1))
                        {
                            lastSignificantToken = finalToken;
                                //If eveyrhing is Ok add the final token as LastSinificantToken
                            significantCodeElement = codeElement;
                        }
                    }

                    //Get the userdefinedword associated to the cursor position in the document
                    userFilterToken =
                        consumedTokens.FirstOrDefault(
                            t =>
                                position.character <= t.StopIndex + 1 && position.character > t.StartIndex
                                && t.Line == position.line + 1
                                && t.TokenType == TokenType.UserDefinedWord);

                    //Detect if the cursor is just after the token, in this case and if bAllowLastPos is false, set 
                    if ((lastSignificantToken != null &&
                         (!bAllowLastPos && lastSignificantToken.StopIndex + 1 == position.character && lastSignificantToken.Line == position.line + 1)) ||
                        (consumedTokens.LastOrDefault().TokenType == TokenType.UserDefinedWord &&
                         !(position.character <= consumedTokens.LastOrDefault().StopIndex + 1 &&
                           position.character >= consumedTokens.LastOrDefault().StartIndex) 
                           && !(lastSignificantToken.TokenType == TokenType.INPUT || lastSignificantToken.TokenType == TokenType.OUTPUT || lastSignificantToken.TokenType == TokenType.IN_OUT)))
                    {
                        lastSignificantToken = null;
                        userFilterToken = null;
                        significantCodeElement = null;
                    }
                }
            }


            return significantCodeElement;
        }

        #endregion
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
