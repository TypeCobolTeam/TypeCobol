using System;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.Interfaces;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Context
{
    /// <summary>
    /// Document Context for a Language Server session.
    /// </summary>
    public class DocumentContext
    {
        /// <summary>
        /// The original TextDocumentItem instance from the client.
        /// </summary>
        public TextDocumentItem TextDocument { get; }

        /// <summary>
        /// The target FileCompiler instance
        /// </summary>
        public FileCompiler FileCompiler
        {
            get;
            set;
        }

        /// <summary>
        /// The underlying Language Server instance.
        /// </summary>
        public ILanguageServer LanguageServer
        {
            get;
            set;
        }

        /// <summary>
        /// Document's Uri.
        /// </summary>
        public Uri Uri { get; }

        /// <summary>
        /// Workspace Project owner.
        /// </summary>
        internal WorkspaceProject Project { get; set; }

        /// <summary>
        /// True if the text document is a Copy, False if it's a Program.
        /// </summary>
        public bool IsCopy => TextDocument.languageId == LanguageIds.Copybook;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="textDocument">The original Text Document instance from the client</param>
        public DocumentContext(TextDocumentItem textDocument)
        {
            System.Diagnostics.Debug.Assert(textDocument != null);
            this.Uri = new Uri(textDocument.uri);
            this.TextDocument = textDocument;
            this.FileCompiler = null;
            this.LanguageServer = null;
        }

        /// <summary>
        /// Connect or Disconnect a ILanguageServer instance to a FileCompiler instance.
        /// </summary>
        /// <param name="bConnect">true to connect, false to disconnect</param>
        public void LanguageServerConnection(bool bConnect)
        {
            if (LanguageServer != null && FileCompiler?.CompilationResultsForProgram != null)
            {
                if (bConnect)
                {
                    FileCompiler.CompilationResultsForProgram.TokensLinesChanged += LanguageServer.TokensLinesChanged;
                    FileCompiler.CompilationResultsForProgram.WholeDocumentChanged += LanguageServer.WholeDocumentChanged;
                }
                else
                {
                    FileCompiler.CompilationResultsForProgram.TokensLinesChanged -= LanguageServer.TokensLinesChanged;
                    FileCompiler.CompilationResultsForProgram.WholeDocumentChanged -= LanguageServer.WholeDocumentChanged;
                }
            }
        }
    }
}
