using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
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
        public TextDocumentItem TextDocument
        {
            get;
            set;
        }

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
        public Uri Uri
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="textDocument">The original Text Document instance from the client</param>
        /// <param name="fileCompiler">Target FileCompiler instance</param>
        /// <param name="languageServer">Underlying <see cref="ILanguageServer"/> instance.</param>
        public DocumentContext(TextDocumentItem textDocument, FileCompiler fileCompiler = null, ILanguageServer languageServer = null)
        {
            System.Diagnostics.Debug.Assert(textDocument != null);
            this.Uri = new Uri(textDocument.uri);
            this.TextDocument = textDocument;
            this.FileCompiler = fileCompiler;
            this.LanguageServer = languageServer;
        }

        /// <summary>
        /// Connect or Disconnect a ILanguageServer instance to a FileCompiler instance.
        /// </summary>
        /// <param name="fileCompiler">The ILanguageServer instance to be connected</param>
        /// <param name="languageServer">The ILanguageServer instance to be connected</param>
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
