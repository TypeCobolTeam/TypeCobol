using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
        public TypeCobolServer(IRPCServer rpcServer) : base(rpcServer) { }

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
            completionOptions.triggerCharacters = new string[] { "::" };
            initializeResult.capabilities.completionProvider = completionOptions;

            return initializeResult;
        }

       

        // -- Files synchronization : maintain a list of opened files, apply all updates to their content -- //
        public override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {
                
                typeCobolWorkspace.OpenSourceFile(objUri, parameters.text != null ? parameters.text : parameters.textDocument.text);

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
                        replacementTextStartsWithNewLine = contentChange.text[0] == '\r' || contentChange.text[0] == '\n'; //Allow to know if a new line was added
                        lineUpdates = contentChange.text.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).ToList(); //Split on /r /n to know the number of lines added
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
                        {//Don't rethow an exception on save.
                            RemoteConsole.Error(string.Format("Error while handling notification {0} : {1}", "textDocument/didChange", e.Message));
                        }
                    }
                    // Document updated
                    else
                    {
                        // Get original lines text before change
                        string originalFirstLineText = fileCompiler.CompilationResultsForProgram.CobolTextLines[contentChange.range.start.line].Text;
                        string originalLastLineText = originalFirstLineText;


                        // Check if the first line was inserted
                        int firstLineIndex = contentChange.range.start.line;
                        int firstLineChar = contentChange.range.start.character;
                        if (replacementTextStartsWithNewLine && !(contentChange.range.start.character < originalLastLineText.Length)) 
                        {
                            firstLineIndex++;
                            firstLineChar = 0;
                        }
                        else if (replacementTextStartsWithNewLine) //Detected that the add line appeared inside an existing line
                        {
                            lineUpdates.Add(lineUpdates.First()); ///Add the default 7 spaces + add lineUpdates in order to update the current line and add the new one. 
                        }

                        // Check if the last line was deleted
                        int lastLineIndex = contentChange.range.end.line;
                        bool lastLineDeleted = false;
                        if (contentChange.range.end.line > contentChange.range.start.line && contentChange.range.end.character == 0)
                        {
                            lastLineIndex--;
                            lastLineDeleted = true;
                        }
                        if (!lastLineDeleted && contentChange.text.Length == 0)
                        {
                            lineUpdates = new List<string>();
                        }
                       
                        if (lastLineIndex > firstLineIndex)
                        {
                            originalLastLineText = fileCompiler.CompilationResultsForProgram.CobolTextLines[Math.Min(lastLineIndex, fileCompiler.CompilationResultsForProgram.CobolTextLines.Count - 1)].Text;
                        }

                        // Text not modified at the beginning of the first replaced line
                        string startOfFirstLine = null;
                        if (firstLineChar > 0)
                        {
                            startOfFirstLine = originalFirstLineText.Substring(0, contentChange.range.start.character);
                        }

                        // Text not modified at the end of the last replaced line
                        string endOfLastLine = null;
                        if (!lastLineDeleted && contentChange.range.end.character < originalLastLineText.Length)
                        {
                            endOfLastLine = originalLastLineText.Substring(contentChange.range.end.character);
                        }

                        // Remove all the old lines
                        for (int i = firstLineIndex; i <= lastLineIndex; i++)
                        {
                            var textChange = new TextChange(TextChangeType.LineRemoved, firstLineIndex, null);
                            textChangedEvent.TextChanges.Add(textChange); //Mark the index line to be removed. The index will remains the same for each line delete, beacause text change are apply one after another
                        }

                        // Insert the updated lines
                        if (!(startOfFirstLine == null && lineUpdates == null && endOfLastLine == null))
                        {
                            int lineUpdatesCount = (lineUpdates != null && lineUpdates.Count > 0) ? lineUpdates.Count : 1;
                            for (int i = 0; i < lineUpdatesCount; i++)
                            {
                                string newLine = (lineUpdates != null && lineUpdates.Count > 0) ? lineUpdates[i] : string.Empty;
                                if (i == 0)
                                {
                                    newLine = startOfFirstLine + newLine;
                                }
                                if (i == lineUpdatesCount - 1)
                                {
                                    newLine = newLine + endOfLastLine;
                                    if (lastLineDeleted) break;
                                }
                                var textChange = new TextChange(TextChangeType.LineInserted, firstLineIndex + i, new TextLineSnapshot(firstLineIndex + i, newLine, null));
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
            {//So Call OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
                DidChangeTextDocumentParams dctdp = new DidChangeTextDocumentParams(parameters.textDocument.uri);
                TextDocumentContentChangeEvent tdcce = new TextDocumentContentChangeEvent();
                tdcce.text = parameters.text;
                dctdp.contentChanges = new TextDocumentContentChangeEvent[] { tdcce };
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
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                // Get compilation info for the current file
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

                // Find the token located below the mouse pointer
                var tokensLine = fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.Lines[parameters.position.line];
                var hoveredToken = tokensLine.TokensWithCompilerDirectives.First(token => token.StartIndex <= parameters.position.character && token.StopIndex >= parameters.position.character);

                // Return a text describing this token
                if (hoveredToken != null)
                {
                    string tokenDescription = hoveredToken.TokenFamily.ToString() + " - " + hoveredToken.TokenType.ToString();
                    return new Hover()
                    {
                        range = new Range(parameters.position.line, hoveredToken.StartIndex, parameters.position.line, hoveredToken.StopIndex + 1),
                        contents = new MarkedString[] { new MarkedString() { language = "Cobol", value = tokenDescription } }
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
            if (objUri.IsFile)
            {
                // Get compilation info for the current file
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[objUri];

                if (fileCompiler.CompilationResultsForProgram != null && fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    List<CodeElement> codeElements = new List<CodeElement>();
                    List<CodeElement> ignoredCodeElements = new List<CodeElement>();
                    int lineIndex = parameters.position.line;
                    // Find the token located below the mouse pointer
                    while (codeElements == null || codeElements.Count == 0)
                    {
                        var codeElementsLine = fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines[lineIndex];

                        if(codeElementsLine != null && codeElementsLine.CodeElements != null)
                        {
                            //Ignore all the EndOfFile token 
                            //TODO: Analyse why there is EndOfFile Token on incomplete CodeElement
                            var tempCodeElements = codeElementsLine.CodeElements.Where(c => c.ConsumedTokens.Any(t => t.TokenType != TokenType.EndOfFile));


                            //Rules of this condition statement :
                            //If a code element is on the same line as the cursor and that the cursor is inside or close to a ConsumedToken 
                            //and that ConsumedToken is different from UserDefinedWord, the current has to be ignored
                            if (tempCodeElements.Any(c => c.Line == parameters.position.line+1 
                                                     && c.ConsumedTokens.Any(t => t.Line == parameters.position.line + 1 && t.StopIndex + 1 >= parameters.position.character 
                                                     && t.TokenType != TokenType.UserDefinedWord)))
                            {
                                ignoredCodeElements.AddRange(tempCodeElements);
                            }
                            else
                            {
                                codeElements.AddRange(tempCodeElements);
                            }
                        }

                        lineIndex--; //decrease lineIndex to get the previous line of TypeCobol Tree.
                    }

                    codeElements.AddRange(ignoredCodeElements); //Add the previously ignored Code Elements, may be they are usefull to help completion.

                    if (!codeElements.Any(c => c.ConsumedTokens.Any(t => t.Line <= parameters.position.line + 1)))
                        return new List<CompletionItem>(); //If nothing is found near the cursor we can"t do completion

                    var finalList = codeElements.Select(c => new CodeElementWrapper(c)); //Create a list of CodeElementWrapper in prder to loose the ConsumedTokens ref. 

                    Token userFilterToken = null; 
                    Token lastSignificantToken = null;
                    //Try o get a significant token for competion and return the codeelement containing the matching token.
                    CodeElement matchingCodeElement = MatchCompletionCodeElement(parameters.position, finalList, out userFilterToken, out lastSignificantToken); //Magic happens here
                    if (lastSignificantToken != null)
                    {//We got a perform Token
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
                            default:
                                break;
                        }

                        if (userFilterToken != null)
                        { //Add the range object to let the client know the position of the user filter token
                            items = items.Select(c =>
                            {
                                //-1 on lne to 0 based / +1 on stop index to include the last character
                                c.data = new Range(userFilterToken.Line-1, userFilterToken.StartIndex, userFilterToken.Line-1, userFilterToken.StopIndex+1); 
                                return c;
                            }).ToList();
                        }
                            

                        return items;
                    }
                }
            }
            return new List<CompletionItem>();
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
        private void MissingCopiesDetected(object fileUri, List<string> missingCopies)
        {
            //Send missing copies to client
            var missingCopiesParam = new MissingCopiesParams();
            missingCopiesParam.Copies = missingCopies;
            missingCopiesParam.textDocument = new TextDocumentIdentifier(fileUri.ToString());

            SendMissingCopies(missingCopiesParam);
        }

        /// <summary>
        /// Event Method triggered when diagnostics are detected.
        /// </summary>
        /// <param name="fileUri">File URI to be send to the client</param>
        /// <param name="diagnostics">List of TypeCobol compiler diagnostics</param>
        private void DiagnosticsDetected(object fileUri, IEnumerable<Compiler.Diagnostics.Diagnostic> diagnostics)
        {
            var diagParameter = new PublishDiagnosticsParams();
            var diagList = new List<Diagnostic>();

            foreach (var diag in diagnostics)
            {
                diagList.Add(new Diagnostic(new Range(diag.Line, diag.ColumnStart, diag.Line, diag.ColumnEnd), diag.Message, (DiagnosticSeverity)diag.Info.Severity, diag.Info.Code.ToString(), diag.Info.ReferenceText));
            }

            diagParameter.uri = fileUri.ToString();
            diagParameter.diagnostics = diagList.ToArray();
            SendDiagnostics(diagParameter);
        }

        private void LoadingIssueDetected(object sender, string message)
        {
            SendLoadingIssue(new LoadingIssueParams() { Message = message });
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
            ICollection<string> pargraphs = null;
            List<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();

            if (performNode != null)
            {
                if (performNode.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    pargraphs = performNode.SymbolTable.GetParagraphNames(userFilterText);
                    variables = performNode.SymbolTable.GetVariables(da => da.Picture != null && 
                                                                    da.DataType == Compiler.CodeElements.DataType.Numeric && 
                                                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                                    new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
                }
            }
  
            if (pargraphs != null)
            {
                completionItems.AddRange(pargraphs.Select(para => new CompletionItem(para)));
            }
            if(variables != null)
            {
                foreach (var variable in variables)
                {
                    var completionItem = new CompletionItem(string.Format("{0} PIC{1}", variable.Name, variable.Picture.NormalizedValue));
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
            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    procedures = node.SymbolTable.GetFunctions(f => f.QualifiedName.ToString().StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase) 
                                                                        || f.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                                    new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Intrinsic, SymbolTable.Scope.Namespace });
                    variables = node.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                    da.DataType == Compiler.CodeElements.DataType.Alphanumeric &&
                                                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                                    new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
                }
            }

            var completionItems = new List<CompletionItem>();

            foreach (var proc in procedures)
            {
                string inputParams = null, outputParams = null, inoutParams = null;

                if (proc.Profile != null)
                {
                    if (proc.Profile.InputParameters != null && proc.Profile.InputParameters.Count > 0)
                        inputParams = string.Format("INPUT: {0}", string.Join(", ", proc.Profile.InputParameters.Select(p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.OutputParameters != null && proc.Profile.OutputParameters.Count > 0)
                        outputParams = string.Format("| OUTPUT: {0}", string.Join(", ", proc.Profile.OutputParameters.Select(p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.InoutParameters != null && proc.Profile.InoutParameters.Count > 0)
                        inoutParams = string.Format("| INOUT: {0}", string.Join(", ", proc.Profile.InoutParameters.Select(p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                }
                bool procIsPublic = (proc.CodeElement as Compiler.CodeElements.FunctionDeclarationHeader).Visibility == Compiler.CodeElements.AccessModifier.Public
                                    && !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations).Functions.Values.Any(t => t.Contains(proc))   //Ignore public if proc is in the current program
                                         || node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Intrinsic).Functions.Values.Any(t => t.Contains(proc))); //Ignore public if proc is in intrinsic;
                var procDisplayName = procIsPublic ? proc.QualifiedName.ToString() : proc.Name;
                var completionItem = new CompletionItem(string.Format("{0} ({1} {2} {3})", procDisplayName, inputParams, outputParams, inoutParams));
                completionItem.insertText = procIsPublic ? string.Format("{0}::{1}", proc.QualifiedName.Tail, proc.QualifiedName.Head) : proc.Name;
                completionItem.kind = proc.Profile.IsFunction ? CompletionItemKind.Function : CompletionItemKind.Method;
                completionItems.Add(completionItem);
            }

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
            IDictionary<string, List<Program>> programs = new Dictionary<string, List<Program>>(StringComparer.InvariantCultureIgnoreCase);
            if (callNode != null)
            {
                if (callNode.SymbolTable != null)
                {
                    programs = callNode.SymbolTable.GetPrograms(userFilterToken != null ? userFilterToken.Text : string.Empty);
                }
            }

            var completionItems = new List<CompletionItem>();
            foreach (var prog in programs)
            {
                var completionItem = new CompletionItem(prog.Key);
                completionItem.kind = CompletionItemKind.Class;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }
        private IEnumerable<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            var types = new List<TypeDefinition>();
            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    types = node.SymbolTable.GetTypes(t => t.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                                      || t.QualifiedName.ToString().StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                      new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global, SymbolTable.Scope.Intrinsic, SymbolTable.Scope.Namespace });
                }
            }
            var completionItems = new List<CompletionItem>();
            foreach (var type in types)
            {
                var typeIsPublic = (type.CodeElement as Compiler.CodeElements.DataTypeDescriptionEntry).Visibility == Compiler.CodeElements.AccessModifier.Public 
                                    && !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations).Types.Values.Any(t => t.Contains(type))   //Ignore public if type is in the current program
                                         || node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Intrinsic).Types.Values.Any(t => t.Contains(type))); //Ignore public if type is in intrinsic

                var typeDisplayName = typeIsPublic ? type.QualifiedName.ToString() : type.Name;
                var completionItem = new CompletionItem(typeDisplayName);
                completionItem.insertText = typeIsPublic ? string.Format("{0}::{1}", type.QualifiedName.Tail, type.QualifiedName.Head) : type.Name;
                completionItem.kind = CompletionItemKind.Class;
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
                return fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers.FirstOrDefault(t => t.Key.Equals(codeElement)).Value;
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
        private static Tuple<TokenType, bool>[] elligibleCompletionTokens = new Tuple<TokenType, bool>[]{
            new Tuple<TokenType, bool>(TokenType.PERFORM,false),
            new Tuple<TokenType, bool>(TokenType.CALL,false),
            new Tuple<TokenType, bool>(TokenType.TYPE,false),
            new Tuple<TokenType, bool>(TokenType.QualifiedNameSeparator,true)
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
        private static CodeElement MatchCompletionCodeElement(Position position, IEnumerable<CodeElementWrapper> codeElements, out Token userFilterToken, out Token lastSignificantToken)
        {
            bool bAllowLastPos = false;
            lastSignificantToken = null;
            CodeElement significantCodeElement = null;
            userFilterToken = null;

            //Filter CodeElements 
            codeElements = codeElements.Where(c => c.ArrangedConsumedTokens.Any(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || (t.Line <= position.line + 1))).ToList(); //Select all the code elements that have a Consumed Tokens before the cursor.
            var closestTokenToCursor = codeElements.Select(c => c.ArrangedConsumedTokens.LastOrDefault(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character))).LastOrDefault();

            if(closestTokenToCursor != null && closestTokenToCursor.Line == position.line+1 && position.character > closestTokenToCursor.StartIndex && closestTokenToCursor.StopIndex+1 >= position.character)
            //the cursor is at the end or in the middle of a token. 
            {
                if(closestTokenToCursor.StopIndex + 1 == position.character && IsCompletionElligibleToken(closestTokenToCursor, out bAllowLastPos) && bAllowLastPos == true) 
                //Detect if token is eligible and if the cursor is at the end of the token
                {
                    //the completion has to start from this token and this codeElement
                    codeElements = codeElements.ToList().SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList();
                }
                else
                {
                    var tempCodeElements = codeElements.TakeWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList(); 

                    if (!tempCodeElements.Any()) //In case there is only one codeElement the TakeWhile will not be able to get the last CodeElement, so we have to do it manually.
                        tempCodeElements.Add(codeElements.FirstOrDefault());
                    else
                        codeElements = tempCodeElements;

                    //The closestToken to cursor as to be added to this codeElement as a userdefinedword
                    codeElements.LastOrDefault().ArrangedConsumedTokens.Add(new Token(TokenType.UserDefinedWord, closestTokenToCursor.StartIndex, closestTokenToCursor.StopIndex, closestTokenToCursor.TokensLine));
                }
            }
            else if (closestTokenToCursor != null)
            {
                //the completion has to start from this token and this codeElement
                codeElements = codeElements.ToList().SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList();
            }
          
            foreach (var codeElement in codeElements)
            {
                var consumedTokens = codeElement.ArrangedConsumedTokens.Where(t => t.StartIndex <= position.character && t.Line <= position.line+1 && t.TokenFamily != TokenFamily.Whitespace);

                if(consumedTokens != null && consumedTokens.Any())
                {
                    foreach (var finalToken in consumedTokens)
                    {
                        if (finalToken.StartIndex > position.character)
                            break;

                        if (IsCompletionElligibleToken(finalToken, out bAllowLastPos) && (finalToken.StopIndex + 1 <= position.character || finalToken.Line <= position.line + 1))
                        {
                            lastSignificantToken = finalToken; //If eveyrhing is Ok add the final token as LastSinificantToken
                            significantCodeElement = codeElement;
                        }
                        else if (finalToken.TokenType != TokenType.UserDefinedWord)
                        {
                            lastSignificantToken = null; //If a token is detected that is different from UserDefinedWord and not recognize as a Eliible token, we cannot do completion
                            significantCodeElement = null;
                        }
                    }

                    //Get the userdefinedword associated to the cursor position in the document
                    userFilterToken = consumedTokens.FirstOrDefault(t => position.character <= t.StopIndex + 1 && position.character > t.StartIndex && t.TokenType == TokenType.UserDefinedWord);

                    //Detect if the cursor is just after the token, in this case and if bAllowLastPos is false, set 
                    if ((lastSignificantToken != null && ((!bAllowLastPos && lastSignificantToken.StopIndex + 1 == position.character) || (lastSignificantToken.StartIndex == position.character && lastSignificantToken.Line == position.line + 1)))
                        || (consumedTokens.LastOrDefault().TokenType == TokenType.UserDefinedWord && !(position.character <= consumedTokens.LastOrDefault().StopIndex + 1 && position.character >= consumedTokens.LastOrDefault().StartIndex)))
                    {
                        lastSignificantToken = null;
                        userFilterToken = null;
                        significantCodeElement = null;
                    }
                }
            }
            

            return significantCodeElement;
        }

        /// <summary>
        /// Atomic Varibale used by WaitProgramClassNotChanged()
        /// </summary>
        int m_unchanged = 0;
        /// <summary>
        /// Event handler used by WaitProgramClassNotChanged()
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void CompilationResultsForProgram_ProgramClassNotChanged(object sender, int e)
        {
            System.Threading.Interlocked.Exchange(ref m_unchanged, 1);
        }

        /// <summary>
        /// This Was used to detecy that no further ProgramClass change construction is performed
        /// during successive incremental snapshot refresh. The key idea was to listen to a new event
        /// ProgramClassNotChanged.
        /// But that problem when doing that is that the asynchonous Program Class construction
        /// is performed every 3s, a completion cannot wait 3s second before having an answer.
        /// </summary>
        /// <param name="fileCompiler"></param>
        private void WaitProgramClassNotChanged(FileCompiler fileCompiler)
        {
            fileCompiler.CompilationResultsForProgram.ProgramClassNotChanged += CompilationResultsForProgram_ProgramClassNotChanged;
            System.Threading.Interlocked.Exchange(ref m_unchanged, 0);
            try
            {
                while (System.Threading.Interlocked.CompareExchange(ref m_unchanged, 0, 1) == 0)
                {
                    System.Threading.Thread.Sleep(20);
                }
            }
            finally
            {
                fileCompiler.CompilationResultsForProgram.ProgramClassNotChanged -= CompilationResultsForProgram_ProgramClassNotChanged;
                System.Threading.Interlocked.Exchange(ref m_unchanged, 0);
            }
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
                ArrangedConsumedTokens.Add(new Token(token.TokenType, token.StartIndex, token.StopIndex, token.TokensLine));
            }
        }

        public List<Token> ArrangedConsumedTokens { get; set; }
    }
}
