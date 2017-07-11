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
            completionOptions.triggerCharacters = new String[] { "::" };
            initializeResult.capabilities.completionProvider = completionOptions;

            return initializeResult;
        }

        // -- Files synchronization : maintain a list of opened files, apply all updates to their content -- //
        public override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.textDocument.uri);
            if (objUri.IsFile)
            {
                string fileName = Path.GetFileName(objUri.LocalPath);
                typeCobolWorkspace.OpenSourceFile(fileName, parameters.text != null ? parameters.text : parameters.textDocument.text);

                //Subscribe to diagnostics event
                typeCobolWorkspace.MissingCopiesEvent += MissingCopiesDetected;
                typeCobolWorkspace.DiagnosticsEvent += DiagnosticsDetected;

                // DEBUG information
                RemoteConsole.Log("Opened source file : " + fileName);
            }
        }

        public override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                string fileName = Path.GetFileName(objUri.LocalPath);
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[fileName];

                #region Convert text changes format from multiline range replacement to single line updates

                // THIS CONVERSION STILL NEEDS MORE WORK : much more complicated than you would think

                TextChangedEvent textChangedEvent = new TextChangedEvent();
                foreach (var contentChange in parameters.contentChanges)
                {
                    // Split the text updated into distinct lines
                    List<string> lineUpdates = null;
                    bool replacementTextStartsWithNewLine = false;

                    if (contentChange.text != null && contentChange.text.Length > 0)
                    {
                        replacementTextStartsWithNewLine = contentChange.text[0] == '\r' || contentChange.text[0] == '\n';
                        lineUpdates = contentChange.text.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).ToList();
                    }

                    // Document cleared
                    if (contentChange.range == null)
                    {
                        //JCM: I have noticed that if the entire text has changed, is better to reload the entire file
                        //To avoid crashes.
                        try
                        {
                            typeCobolWorkspace.OpenSourceFile(fileName, contentChange.text);
                        }
                        catch (Exception e)
                        {//Don't rethow an exception on save.
                            RemoteConsole.Error(String.Format("Error while handling notification {0} : {1}", "textDocument/didChange", e.Message));
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
                            textChangedEvent.TextChanges.Add(textChange);
                        }

                        // Insert the updated lines
                        if (!(startOfFirstLine == null && lineUpdates == null && endOfLastLine == null))
                        {
                            int lineUpdatesCount = (lineUpdates != null && lineUpdates.Count > 0) ? lineUpdates.Count : 1;
                            for (int i = 0; i < lineUpdatesCount; i++)
                            {
                                string newLine = (lineUpdates != null && lineUpdates.Count > 0) ? lineUpdates[i] : String.Empty;
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
                typeCobolWorkspace.UpdateSourceFile(fileName, textChangedEvent, false);

                // DEBUG information
                RemoteConsole.Log("Udpated source file : " + fileName);
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
                string fileName = Path.GetFileName(objUri.LocalPath);
                typeCobolWorkspace.CloseSourceFile(fileName);

                // DEBUG information
                RemoteConsole.Log("Closed source file : " + fileName);
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
                string fileName = Path.GetFileName(objUri.LocalPath);
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[fileName];

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
                string fileName = Path.GetFileName(objUri.LocalPath);
                var fileCompiler = typeCobolWorkspace.OpenedFileCompiler[fileName];

                if (fileCompiler.CompilationResultsForProgram != null && fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot != null)
                {
                    // Find the token located below the mouse pointer
                    var tokensLine = fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.Lines[parameters.position.line];
                    var tokens = tokensLine.TokensWithCompilerDirectives;
                    TypeCobol.Compiler.Scanner.Token matchingToken = MatchCompletionTokenPosition(parameters.position, tokens);
                    if (matchingToken != null)
                    {//We got a perform Token
                        switch (matchingToken.TokenType)
                        {
                            case Compiler.Scanner.TokenType.PERFORM:
                                {
                                    List<CompletionItem> items = new List<CompletionItem>();
                                    ICollection<String> paragraphs = GetCompletionPerformParagraph(fileCompiler, matchingToken);
                                    if (paragraphs != null)
                                    {
                                        foreach (String p in paragraphs)
                                        {
                                            CompletionItem item = new CompletionItem(p);
                                            items.Add(item);
                                        }
                                    }
                                    return items;
                                }
                            default:
                                break;
                        }
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
        #endregion

        /// <summary>
        /// Event Method triggered when missing copies are detected.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e">List of missing copies name</param>
        private void MissingCopiesDetected(object sender, List<string> e)
        {
            //Send missing copies to client
            var missingCopiesParam = new MissingCopiesParams();
            missingCopiesParam.Copies = e;

            SendMissingCopies(missingCopiesParam);
        }

        /// <summary>
        /// Event Method triggered when diagnostics are detected.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e">List of TypeCobol compiler diagnostics</param>
        private void DiagnosticsDetected(object sender, IList<Compiler.Diagnostics.Diagnostic> e)
        {
            var diagParameter = new PublishDiagnosticsParams();
            var diagList = new List<Diagnostic>();

            foreach (var diag in e.Take(100))
            {
                diagList.Add(new Diagnostic(new Range(diag.Line, diag.ColumnStart, diag.Line, diag.ColumnEnd), diag.Message, (DiagnosticSeverity)diag.Info.Severity, diag.Info.Code.ToString(), diag.Info.ReferenceText));
            }

            diagParameter.diagnostics = diagList.ToArray();
            SendDiagnostics(diagParameter);
        }

        /// <summary>
        /// Get the paragraph that can be associated to PERFORM Completion token.
        /// </summary>
        /// <param name="fileCompiler">The target FileCompiler instance</param>
        /// <param name="performToken">The PERFORM token</param>
        /// <returns></returns>
        private ICollection<string> GetCompletionPerformParagraph(FileCompiler fileCompiler, Compiler.Scanner.Token performToken)
        {
            if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot != null)
            {
                if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Root != null)
                {
                    CompletionNodeMatcher matcher = new CompletionNodeMatcher(CompletionNodeMatcher.CompletionMode.Perform, performToken);
                    fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Root.AcceptASTVisitor(matcher);
                    if (matcher.MatchingNode != null)
                    {
                        if (matcher.MatchingNode.SymbolTable != null)
                        {
                            ICollection<string> pargraphs = matcher.MatchingNode.SymbolTable.GetParagraphNames();
                            return pargraphs;
                        }
                        else
                        {
                            return null;
                        }
                    }
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
        private static Tuple<Compiler.Scanner.TokenType, bool>[] elligibleCompletionTokens = new Tuple<Compiler.Scanner.TokenType, bool>[]{
            new Tuple<Compiler.Scanner.TokenType, bool>(Compiler.Scanner.TokenType.PERFORM,false)
        };

        /// <summary>
        /// Determines if the given token is elligible for a completion
        /// </summary>
        /// <param name="token"></param>
        /// <param name="bAllowLastPos"></param>
        /// <returns></returns>
        private static bool IsCompletionElligibleToken(Compiler.Scanner.Token token, out bool bAllowLastPos)
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
        private static Compiler.Scanner.Token MatchCompletionTokenPosition(Position position, IList<Compiler.Scanner.Token> tokens)
        {
            bool bAllowLastPos = false;
            int count = tokens.Count;
            TypeCobol.Compiler.Scanner.Token lastToken = null;
            int character = position.character + 1;
            for (int i = 0; i < count; i++)
            {
                //Skip Whitespace
                int j = i;
                TypeCobol.Compiler.Scanner.Token token = null;
                for (j = i; j < count; j++)
                {
                    token = tokens[j];
                    if (token.TokenFamily != Compiler.Scanner.TokenFamily.Whitespace)
                    {
                        break;
                    }
                    else if (token.Column <= character && (token.EndColumn + 1) >= character)
                    {
                        if (lastToken != null)
                        {
                            if ((!bAllowLastPos && lastToken != null && (lastToken.EndColumn + 1) == character) || lastToken.Column == character)
                                lastToken = null;
                            else
                                return lastToken;
                        }
                    }
                }
                i = j;
                if (i >= count)
                    break;
                token = tokens[i];
                bool isElligible = IsCompletionElligibleToken(token, out bAllowLastPos);
                if (isElligible)
                    lastToken = token;

                if (token.Column == character)
                {
                    if (!isElligible && token.Column != character)
                    {   //We are on a token which is not a matching token and the the cursor position is not at the
                        //beginning of the character ==> cancel the previous matching token
                        lastToken = null;
                    }
                    break;
                }
                if (!isElligible)
                    lastToken = null;
            }
            if (lastToken != null && ((!bAllowLastPos && (lastToken.EndColumn + 1) == character) || lastToken.Column == character))
                lastToken = null;

            return lastToken;
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
        private void WaitProgramClassNotChanged(Compiler.FileCompiler fileCompiler)
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


    }
}
