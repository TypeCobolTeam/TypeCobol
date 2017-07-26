using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServices.Editor;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.Compiler.Nodes;
using static TypeCobol.LanguageServer.Utilities.CompletionNodeMatcher;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;

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
            typeCobolWorkspace.DidChangeConfigurationParams(parameters.settings.ToString());
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
                    // Find the token located below the mouse pointer
                    var tokensLine = fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.Lines[parameters.position.line];
                    var tokens = tokensLine.TokensWithCompilerDirectives;
                    Token userFilterToken = null;
                    Token matchingToken = MatchCompletionTokenPosition(parameters.position, tokens, out userFilterToken);
                    if (matchingToken != null)
                    {//We got a perform Token
                        List<CompletionItem> items = new List<CompletionItem>();
                        switch (matchingToken.TokenType)
                        {
                            case TokenType.PERFORM:
                                {
                                    items.AddRange(GetCompletionPerformParagraph(fileCompiler, matchingToken, userFilterToken));
                                    break;
                                }
                            case TokenType.CALL:
                                {
                                    items.AddRange(GetCompletionForProcedure(fileCompiler, matchingToken, userFilterToken));
                                    items.AddRange(GetCompletionForLibrary(fileCompiler, matchingToken, userFilterToken));
                                    break;
                                }
                            case TokenType.TYPE:
                                {
                                    items.AddRange(GetCompletionForType(fileCompiler, matchingToken, userFilterToken));
                                    items.AddRange(GetCompletionForLibrary(fileCompiler, matchingToken, userFilterToken));
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
            typeCobolWorkspace.UpdateMissingCopies(parameter.Copies);
        }
        #endregion

        /// <summary>
        /// Event Method triggered when missing copies are detected.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e">List of missing copies name</param>
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
        /// <param name="sender"></param>
        /// <param name="e">List of TypeCobol compiler diagnostics</param>
        private void DiagnosticsDetected(object fileUri, IList<Compiler.Diagnostics.Diagnostic> diagnostics)
        {
            var diagParameter = new PublishDiagnosticsParams();
            var diagList = new List<Diagnostic>();

            foreach (var diag in diagnostics.Take(100))
            {
                diagList.Add(new Diagnostic(new Range(diag.Line, diag.ColumnStart, diag.Line, diag.ColumnEnd), diag.Message, (DiagnosticSeverity)diag.Info.Severity, diag.Info.Code.ToString(), diag.Info.ReferenceText));
            }

            diagParameter.uri = fileUri.ToString();
            diagParameter.diagnostics = diagList.ToArray();
            SendDiagnostics(diagParameter);
        }

        #region Completion Methods
        /// <summary>
        /// Get the paragraph that can be associated to PERFORM Completion token.
        /// </summary>
        /// <param name="fileCompiler">The target FileCompiler instance</param>
        /// <param name="performToken">The PERFORM token</param>
        /// <returns></returns>
        private IEnumerable<CompletionItem> GetCompletionPerformParagraph(FileCompiler fileCompiler, Token performToken, Token userFilterToken)
        {
            var performNode = GetMatchingNode(fileCompiler, performToken, CompletionMode.Perform);
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
                foreach (var para in pargraphs)
                {
                    completionItems.Add(new CompletionItem(para));
                }
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
        private IEnumerable<CompletionItem> GetCompletionForProcedure(FileCompiler fileCompiler, Token callToken, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, callToken, CompletionMode.Call);
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
        private IEnumerable<CompletionItem> GetCompletionForLibrary(FileCompiler fileCompiler, Token token, Token userFilterToken)
        {
            var callNode = GetMatchingNode(fileCompiler, token, CompletionMode.Call);
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
        private IEnumerable<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, Token token, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, token, CompletionMode.Type);
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
        /// <param name="token"></param>
        /// <param name="completionMode"></param>
        /// <returns></returns>
        private Node GetMatchingNode(FileCompiler fileCompiler, Token token, CompletionMode completionMode)
        {
            if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot != null)
            {
                if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Root != null)
                {
                    CompletionNodeMatcher matcher = new CompletionNodeMatcher(completionMode, token);
                    fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Root.AcceptASTVisitor(matcher);
                    return matcher.MatchingNode;
                }
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
            new Tuple<TokenType, bool>(TokenType.TYPE,false)
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
        private static Token MatchCompletionTokenPosition(Position position, IEnumerable<Token> tokens, out Token userFilterToken)
        {
            bool bAllowLastPos = false;
            Token LastSignificantifToken = null;

            var finalTokens = tokens.Except(tokens.Where(t => t.TokenFamily == TokenFamily.Whitespace)); //Remove space tokens
            foreach (var finalToken in finalTokens)
            {
                if (finalToken.StartIndex > position.character)
                    break;

                
                if (IsCompletionElligibleToken(finalToken, out bAllowLastPos) && finalToken.EndColumn <= position.character)
                {
                    LastSignificantifToken = finalToken; //If eveyrhing is Ok add the final token as LastSinificantToken
                }
                else if(finalToken.TokenType != TokenType.UserDefinedWord)
                {
                    LastSignificantifToken = null; //If a token is detected that is different from UserDefinedWord and not recognize as a Eliible token, we cannot do completion
                }

                if (LastSignificantifToken != null && finalToken.TokenType == TokenType.UserDefinedWord && finalToken == finalTokens.LastOrDefault())
                    break; //If a LastSignificatnToken is found + the last token is a userdefinedword we do not have to continue
            }

            //Get the userdefinedword associated to the cursor position in the document
            userFilterToken = finalTokens.FirstOrDefault(t => position.character <= t.EndColumn && position.character > t.StartIndex  && t.TokenType == TokenType.UserDefinedWord);


            //Detect if the cursor is just after the token, in this case and if bAllowLastPos is false, set 
            if ((LastSignificantifToken != null && ((!bAllowLastPos && LastSignificantifToken.EndColumn == position.character) || LastSignificantifToken.Column == position.character)) 
                || (finalTokens.LastOrDefault().TokenType == TokenType.UserDefinedWord && !(position.character <= finalTokens.LastOrDefault().EndColumn && position.character >= finalTokens.LastOrDefault().StartIndex)))
            {
                LastSignificantifToken = null;
                userFilterToken = null;
            }

            return LastSignificantifToken;
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
}
