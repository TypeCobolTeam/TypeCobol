using System;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Override methods of the base language server to implement TypeCobol editor experiences
    /// </summary>
    class TypeCobolServer : TypeCobol.LanguageServer.VsCodeProtocol.LanguageServer
    {
        public TypeCobolServer(IRPCServer rpcServer) : base(rpcServer) { }

        public override InitializeResult OnInitialize(InitializeParams parameters)
        {
            RemoteWindow.ShowInformationMessage("TypeCobol language server was launched !");

            var initializeResult = base.OnInitialize(parameters);
            initializeResult.capabilities.textDocumentSync = TextDocumentSyncKind.Incremental;
            return initializeResult;
        }

        public override void OnDidChangeTextDocument(DidChangeTextDocumentParams parameters)
        {
            base.OnDidChangeTextDocument(parameters);
        }
    }
}
