using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServices.Editor;
using TokenType = TypeCobol.Compiler.Scanner.TokenType;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// The Implementation of a ILanguageServer for TypeCobol for one document.
    /// </summary>
    public class TypeCobolLanguageServer : ILanguageServer
    {
        /// <summary>
        /// The Internal IRPCServer instance for sending messages.
        /// </summary>
        public IRPCServer RpcServer { get; set; }

        /// <summary>
        /// The Underlying TypeCobol document
        /// </summary>
        public ITextDocument TextDocument { get; set; }

        /// <summary>
        /// The range of the current formalized token.
        /// </summary>
        private Range currentFormaCommentTokenRange;

        /// <summary>
        /// Collect all interesting token from a line.
        /// </summary>
        /// <param name="line"></param>
        /// <param name="tokens"></param>
        private void CollectTokens(ITokensLine line, List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token> tokens)
        {            
            foreach (TypeCobol.Compiler.Scanner.Token token in line.SourceTokens)
            {
                switch (token.TokenType)
                {
                    case TokenType.FORMALIZED_COMMENTS_START:
                    {//This a potential start of a formal comment.
                        int tline = token.Line;
                        int tcolumn = token.Column;

                        currentFormaCommentTokenRange = new Range();
                        currentFormaCommentTokenRange.start = new Position(tline - 1, tcolumn - 1);
                    }
                        break;
                    case TokenType.FORMALIZED_COMMENTS_STOP:
                        if (currentFormaCommentTokenRange != null)
                        {//We had a formal comment, then this is its end.
                            int tline = token.Line;
                            int tcolumnEnd = token.EndColumn;
                            currentFormaCommentTokenRange.end = new Position(tline - 1, tcolumnEnd - 1);
                            tokens.Add(new TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token(
                                TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.TokenType.FormalComment,
                                currentFormaCommentTokenRange));
                            currentFormaCommentTokenRange = null;
                        }                        
                        break;
                    default:
                        break;
                }
            }
        }

        public void UpdateTokensLines(IList<DocumentChange<ITokensLine>> changes, CompilationDocument compilationDocument)
        {
            Range docRange = null;
            List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token> tokens = null;
            if (changes != null)
            {
                if (changes.Count == 0)
                    return;
                //Determines the document's range.
                var firstChange = changes[0];
                var lastChange = changes[changes.Count - 1];
                Position firstPos = new Position(firstChange.LineIndex - 1, 0);
                Position lastPos = new Position(lastChange.LineIndex - 1, lastChange.NewLine.Length - 1);
                docRange  = new Range(firstPos, lastPos);

                tokens = new List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>();
                foreach (var change in changes)
                {
                    CollectTokens(change.NewLine, tokens);
                }
            }
            else
            {
                //Rescan the entire document
                tokens = new List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>();
                var lines = compilationDocument.CobolTextLines;
                foreach (var line in lines)
                {
                    CollectTokens((ITokensLine)line, tokens);
                }
            }
            SyntaxColoringParams scParams = new SyntaxColoringParams(this.LspTextDocument, docRange, tokens);
            //Now send the notification.
            this.RpcServer.SendNotification(SyntaxColoringNotification.Type, scParams);
        }

        /// <summary>
        /// The LSP Text Document Item.
        /// </summary>
        public TextDocumentItem LspTextDocument { get; private set; }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="lspTextDocument">The language server protocol Test Document Item</param>
        public TypeCobolLanguageServer(IRPCServer rpcServer, TextDocumentItem lspTextDocument)
        {
            this.RpcServer = rpcServer;
            this.LspTextDocument = lspTextDocument;
        }

        public void OnNext(DocumentChangedEvent<ITokensLine> value)
        {
            //throw new NotImplementedException();
        }

        public void OnNext(IList<CompilationError> value)
        {
            //throw new NotImplementedException();
        }

        void IObserver<IList<CompilationError>>.OnError(Exception error)
        {
            //throw new NotImplementedException();
        }

        void IObserver<IList<CompilationError>>.OnCompleted()
        {
            //throw new NotImplementedException();
        }

        void IObserver<DocumentChangedEvent<ITokensLine>>.OnError(Exception error)
        {
            //throw new NotImplementedException();
        }

        void IObserver<DocumentChangedEvent<ITokensLine>>.OnCompleted()
        {
            //throw new NotImplementedException();
        }        
    }
}
