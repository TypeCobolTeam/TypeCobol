﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServices.Editor;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : TypeCobol.LanguageServer.VsCodeProtocol.LanguageServer
    {
        public TypeCobolServer(IRPCServer rpcServer) : base(rpcServer) { }

        // -- Initialization : create workspace and return language server capabilities --

        private Workspace typeCobolWorkspace;
               
        public override InitializeResult OnInitialize(InitializeParams parameters)
        {
            // Initialize the workspace
            // TO DO : receive all these configuration properties from the client
            var rootDirectory = new DirectoryInfo(parameters.rootPath);
            string workspaceName = rootDirectory.Name + "#" + parameters.processId;
            typeCobolWorkspace = new Workspace(workspaceName, rootDirectory.FullName, new string[] { "*.cbl", "*.pgm", "*.cpy", "*.txt", },
                Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat, 
                new TypeCobolOptions());

            // DEBUG information
            RemoteWindow.ShowInformationMessage("TypeCobol language server was launched !");

            // Return language server capabilities
            var initializeResult = base.OnInitialize(parameters);
            initializeResult.capabilities.textDocumentSync = TextDocumentSyncKind.Incremental;
            initializeResult.capabilities.hoverProvider = true;
            return initializeResult;
        }

        // -- Files synchronization : maintain a list of opened files, apply all updates to their content --

        public override void OnDidOpenTextDocument(DidOpenTextDocumentParams parameters)
        {
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                string fileName = Path.GetFileName(objUri.LocalPath);
                typeCobolWorkspace.OpenSourceFile(fileName, parameters.text);

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
                var fileCompiler = typeCobolWorkspace.OpenedFileCompilers[fileName];

                #region Convert text changes format from multiline range replacement to single line updates

                // THIS CONVERSION STILL NEEDS MORE WORK : much more complicated than you would think

                TextChangedEvent textChangedEvent = new TextChangedEvent();
                foreach (var contentChange in parameters.contentChanges)
                {
                    // Split the text updated into distinct lines
                    string[] lineUpdates = null;
                    bool replacementTextStartsWithNewLine = false;
                    if(contentChange.text != null && contentChange.text.Length > 0)
                    {
                        replacementTextStartsWithNewLine = contentChange.text[0] == '\r' || contentChange.text[0] == '\n';
                        lineUpdates = contentChange.text.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
                    }

                    // Document cleared
                    if (contentChange.range == null)
                    {
                        var textChange = new TextChange(TextChangeType.DocumentCleared, 0, null);
                        textChangedEvent.TextChanges.Add(textChange);
                        if (lineUpdates != null)
                        {
                            for (int i = 0; i < lineUpdates.Length; i++)
                            {
                                textChange = new TextChange(TextChangeType.LineInserted, i, new TextLineSnapshot(i, lineUpdates[i], null));
                                textChangedEvent.TextChanges.Add(textChange);
                            }
                        }
                    }
                    // Document updated
                    else
                    {
                        // Check if the first line was inserted
                        int firstLineIndex = contentChange.range.start.line;
                        int firstLineChar = contentChange.range.start.character;
                        if (replacementTextStartsWithNewLine)
                        {
                            firstLineIndex++;
                            firstLineChar = 0;
                        }

                        // Check if the last line was deleted
                        int lastLineIndex = contentChange.range.end.line;
                        bool lastLineDeleted = false;
                        if (contentChange.range.end.line > contentChange.range.start.line && contentChange.range.end.character == 0)
                        {
                            lastLineIndex--;
                            lastLineDeleted = true;
                        }
                        if(!lastLineDeleted && contentChange.text.Length == 0)
                        {
                            lineUpdates = new string[0];
                        }

                        // Get original lines text before change
                        string originalFirstLineText = fileCompiler.CompilationResultsForProgram.CobolTextLines[contentChange.range.start.line].Text;
                        string originalLastLineText = originalFirstLineText;
                        if(lastLineIndex > firstLineIndex)
                        {
                            originalLastLineText = fileCompiler.CompilationResultsForProgram.CobolTextLines[lastLineIndex].Text;
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
                            int lineUpdatesCount = (lineUpdates != null && lineUpdates.Length > 0) ? lineUpdates.Length : 1;
                            for (int i = 0; i < lineUpdatesCount; i++)
                            {
                                string newLine = (lineUpdates != null && lineUpdates.Length > 0) ? lineUpdates[i] : String.Empty;
                                if (i == 0)
                                {
                                    newLine = startOfFirstLine + newLine;
                                }
                                if(i == lineUpdatesCount - 1)
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
                typeCobolWorkspace.UpdateSourceFile(fileName, textChangedEvent);

                // DEBUG information
                RemoteConsole.Log("Udpated source file : " + fileName);
                foreach(var textChange in textChangedEvent.TextChanges)
                {
                    RemoteConsole.Log(" - " + textChange.ToString());
                }
            }
        }

        public override void OnDidCloseTextDocument(TextDocumentIdentifier parameters)
        {
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                string fileName = Path.GetFileName(objUri.LocalPath);
                typeCobolWorkspace.CloseSourceFile(fileName);

                // DEBUG information
                RemoteConsole.Log("Closed source file : " + fileName);
            }
        }

        // -- Tooltip information on hover --

        public override Hover OnHover(TextDocumentPosition parameters)
        {
            Uri objUri = new Uri(parameters.uri);
            if (objUri.IsFile)
            {
                // Get compilation info for the current file
                string fileName = Path.GetFileName(objUri.LocalPath);
                var fileCompiler = typeCobolWorkspace.OpenedFileCompilers[fileName];

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
                        contents = new MarkedString[] { new MarkedString() { language="Cobol", value=tokenDescription } }
                    };
                }
            }
            return null;
        }
    }
}
