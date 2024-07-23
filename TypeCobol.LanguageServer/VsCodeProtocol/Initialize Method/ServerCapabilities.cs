/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Defines the capabilities provided by a language
    /// server.
    /// </summary>
    public class ServerCapabilities
    {        
        /// <summary>
        /// Defines how text documents are synced.
        /// </summary>
        public TextDocumentSyncKind textDocumentSync { get; set; }

        /// <summary>
        /// The server provides hover support.
        /// </summary>
        public bool hoverProvider { get; set; }

        /// <summary>
        /// The server provides completion support.
        /// </summary>
        public CompletionOptions completionProvider { get; set; }

        /// <summary>
        /// The server provides signature help support.
        /// </summary>
        public SignatureHelpOptions signatureHelpProvider { get; set; }

        /// <summary>
        /// The server provides goto definition support.
        /// </summary>
        public bool definitionProvider { get; set; }

        /// <summary>
        /// The server provides find references support.
        /// </summary>
        public bool referencesProvider { get; set; }

        /// <summary>
        /// The server provides document highlight support.
        /// </summary>
        public bool documentHighlightProvider { get; set; }

        /// <summary>
        /// The server provides document symbol support.
        /// </summary>
        public bool documentSymbolProvider { get; set; }

        /// <summary>
        /// The server provides workspace symbol support.
        /// </summary>
        public bool workspaceSymbolProvider { get; set; }

        /// <summary>
        /// The server provides code actions.
        /// </summary>
        public bool codeActionProvider { get; set; }

        /// <summary>
        /// The server provides code lens.
        /// </summary>
        public CodeLensOptions codeLensProvider { get; set; }

        /// <summary>
        /// The server provides document formatting.
        /// </summary>
        public bool documentFormattingProvider { get; set; }

        /// <summary>
        /// The server provides document range formatting.
        /// </summary>
        public bool documentRangeFormattingProvider { get; set; }

        /// <summary>
        /// The server provides document formatting on typing.
        /// </summary>
        public DocumentOnTypeFormattingOptions documentOnTypeFormattingProvider { get; set; }

        /// <summary>
        /// The server provides rename support.
        /// </summary>
        public bool renameProvider { get; set; }

        /// <summary>
        /// Gets or sets the value which indicates if execute command is supported.
        /// </summary>
        public ExecuteCommandOptions executeCommandProvider { get; set; }

        /// <summary>
        /// Experimental value.
        /// </summary>
        public object experimental;
    }
}
