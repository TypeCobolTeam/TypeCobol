using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;
using TypeCobol.LanguageServer.Interfaces;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServices.Editor;
using TokenType = TypeCobol.Compiler.Scanner.TokenType;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.Context;
using System.IO;

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
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        public bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// Internal map of collected token ranges.
        /// </summary>
        private Dictionary<int, List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>> _tokenRangeMap;

        /// <summary>
        /// Add a token associated to a line.
        /// </summary>
        /// <param name="line"></param>
        /// <param name="token"></param>
        /// <returns>The added token</returns>
        private TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token AddToken(int line,
            TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token token)
        {
            if (!_tokenRangeMap.ContainsKey(line))
            {
                _tokenRangeMap[line] = new List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>();
            }

            switch (token.Type)
            {
                case TypeCobolCustomLanguageServerProtocol.SyntaxColoring.TokenType.FormalComment:
                    _tokenRangeMap[line].Add((token));
                    break;
            }

            return token;
        }

        /// <summary>
        /// Remove a line
        /// </summary>
        /// <param name="line">The line to remove</param>
        private void RemoveLine(int line)
        {
            if (_tokenRangeMap != null && _tokenRangeMap.ContainsKey(line))
            {
                _tokenRangeMap.Remove(line);
            }
        }

        /// <summary>
        /// A line has been inserted
        /// </summary>
        /// <param name="line">The line number inserted</param>
        private void LineInserted(int line)
        {
            //So update any line in the document ggreater than the line number
        }

        /// <summary>
        /// Called when the document has been cleared
        /// </summary>
        private void DocumentCleared()
        {
            _tokenRangeMap?.Clear();
        }

        /// <summary>
        /// Checks if the model contains the given line number
        /// </summary>
        /// <param name="line">The line number to check.</param>
        /// <returns></returns>
        private bool HasLine(int line)
        {
            return _tokenRangeMap != null ? _tokenRangeMap.ContainsKey(line) : false;
        }
        /// <summary>
        /// Collect tokens to be notified.
        /// </summary>
        /// <returns></returns>
        private List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>
            CollectNotificationTokens()
        {
            List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>
            tokens = new List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>();

            if (_tokenRangeMap != null)
            {
                foreach (var lines in _tokenRangeMap.Values)
                {
                    tokens.AddRange(lines);
                }
            }

            return tokens;
        }

        /// <summary>
        /// Collect a token
        /// </summary>
        /// <param name="token">The token to be collected</param>
        private void CollectToken(TypeCobol.Compiler.Scanner.Token token)
        {
            int tline = token.Line;
            int tcolumn = token.Column;
            int tcolumnEnd = token.EndColumn;
            Range currentFormaCommentTokenRange = new Range();
            currentFormaCommentTokenRange.start = new Position(tline - 1, tcolumn - 1);
            currentFormaCommentTokenRange.end = new Position(tline - 1, tcolumnEnd - 1);
            AddToken(tline - 1, new TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token(
                TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.TokenType.FormalComment,
                currentFormaCommentTokenRange));
        }

        /// <summary>
        /// Collect all interesting token from a line.
        /// </summary>
        /// <param name="line"></param>
        /// <param name="tokens"></param>
        private void CollectTokens(ITokensLine line)
        {
            if (line is TokensLine)
            {
                TokensLine tlines = (TokensLine) line;
                if (tlines.ScanState != null &&
                    (tlines.ScanState.InsideFormalizedComment || tlines.ScanState.InsideMultilineComments))
                {//Collect all token inside multi line comments.
                    foreach (TypeCobol.Compiler.Scanner.Token token in line.SourceTokens)
                    {
                        CollectToken(token);
                    }
                }
                else if (tlines.SourceTokens != null && tlines.SourceTokens.Count >= 1 && 
                    (tlines.SourceTokens[0].Type == (int)TokenType.MULTILINES_COMMENTS_STOP || tlines.SourceTokens[0].Type == (int)TokenType.FORMALIZED_COMMENTS_STOP))
                {//This is the last multiline tokens stop which is not par of a MultiLine comments context.
                    CollectToken(tlines.SourceTokens[0]);
                }
            }
        }

        //The root OutlineNode is stored in memory to be able to update it with new nodes
        private OutlineNode _rootOutlineNode = null;
        /// <summary>
        /// Update the OutlineNodes of the root OutlineNode. Creates it if new document.
        /// </summary>
        /// <param name="programClassDocument"></param>
        /// <param name="bForced"></param>
        /// <returns></returns>
        public RefreshOutlineParams UpdateOutline(ProgramClassDocument programClassDocument, bool bForced)
        {
            if (programClassDocument != null)
            {
                if (_rootOutlineNode == null)
                {
                    _rootOutlineNode = new OutlineNode(programClassDocument.Root);
                }

                if (bForced || _rootOutlineNode.Update(programClassDocument.Root))
                    return new RefreshOutlineParams(new TextDocumentIdentifier(this.LspTextDocument.uri),
                        _rootOutlineNode);
            }
            return null;
        }


        /// <summary>
        /// Called when a token scanning has been performed.
        /// </summary>
        /// <param name="compilationDocument">The underlying CompilationDocument instance</param>
        /// <param name="changes">The list of document change instances, if this parameter is null then the whole document has been rescanned.</param>
        public void UpdateTokensLines(CompilationDocument compilationDocument, IEnumerable<DocumentChange<ITokensLine>> changes = null)
        {
            Range docRange = null;
            List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token> tokens = null;
            if (!UseSyntaxColoring)
                return;
            if (changes != null)
            {
                //Compute the update range so that client can optimize its rescanning
                DocumentChange<ITokensLine> minChange = null;
                DocumentChange<ITokensLine> maxChange = null;
                int minLine = Int32.MaxValue;
                int maxLine = Int32.MinValue;
                               
                foreach (var change in changes)
                {
                    minLine = Math.Min(minLine, change.LineIndex);
                    if (minLine == change.LineIndex)
                        minChange = change;
                    maxLine = Math.Max(maxLine, change.LineIndex);
                    if (maxLine == change.LineIndex)
                        maxChange = change;
                }

                //Determines the document's range.
                if (minChange != null && maxChange != null)
                {
                    var firstChange = minChange;
                    var lastChange = maxChange;
                    Position firstPos = new Position(firstChange.LineIndex, 0);
                    Position lastPos = new Position(lastChange.LineIndex, lastChange.NewLine.Length - 1);
                    docRange = new Range(firstPos, lastPos);
                }
            }
            //Compute all interesting tokens
            if (_tokenRangeMap == null)
                _tokenRangeMap = new Dictionary<int, List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>>();
            _tokenRangeMap.Clear();
            tokens = new List<TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring.Token>();
            var lines = compilationDocument.CobolTextLines;
            foreach (var line in lines)
            {
                CollectTokens((ITokensLine)line);
            }
            tokens = CollectNotificationTokens();
            SyntaxColoringParams scParams = new SyntaxColoringParams(this.LspTextDocument, docRange, tokens);
            //Now send the notification.
            this.RpcServer.SendNotification(SyntaxColoringNotification.Type, scParams);
        }

        /// <summary>
        /// Handler when some tokens lines has changed.
        /// </summary>
        /// <param name="sender">Must be an instance of CompilationDocument </param>
        /// <param name="eventArgs">Must be an instance of TypeCobol.Compiler.Concurrency.DocumentChangedEvent<TypeCobol.Compiler.Scanner.ITokensLine> </param>
        public void TokensLinesChanged(object sender, EventArgs eventArgs)
        {
            System.Diagnostics.Debug.Assert(sender is CompilationDocument);
            System.Diagnostics.Debug.Assert(eventArgs is DocumentChangedEvent<ITokensLine>);
            TypeCobol.Compiler.Concurrency.DocumentChangedEvent<TypeCobol.Compiler.Scanner.ITokensLine> changeEvent =
                (TypeCobol.Compiler.Concurrency.DocumentChangedEvent<TypeCobol.Compiler.Scanner.ITokensLine>) eventArgs;

            CompilationDocument compilationDocument = (CompilationDocument) sender;
            UpdateTokensLines(compilationDocument, changeEvent.DocumentChanges);
        }

        /// <summary>
        /// Handler when whole document has changed.
        /// </summary>
        /// <param name="sender">Must be an instance of CompilationDocument </param>
        /// <param name="eventArgs">Not used</param>
        public void WholeDocumentChanged(object sender, EventArgs eventArgs)
        {
            System.Diagnostics.Debug.Assert(sender is CompilationDocument);
            CompilationDocument compilationDocument = (CompilationDocument) sender;
            UpdateTokensLines(compilationDocument);
        }

        /// <summary>
        /// LSR Cfg Analyzer ID.
        /// </summary>
        public const string lspcfgId = "lsp-cfg";

        /// <summary>
        /// Method to update CFG/DFA information.
        /// </summary>
        /// <param name="fileCompiler">The underlying File Compiler</param>
        /// <param name="lsrMode)">True if this action is executed in a LSR Mode</param>
        /// <returns>CFG/DFA Data information</returns>
        public CfgDfaParams UpdateCfgDfaInformation(DocumentContext docContext, bool lsrMode)
        {
            CfgDfaParams result = null;
            docContext.FileCompiler.CompilationResultsForProgram.TryGetAnalyzerResult(lspcfgId, out IList<ControlFlowGraph<Node, object>> cfgs);
            if (cfgs != null)
            {                
                //Create a temporary dot file.
                string tempFile = Path.GetTempFileName();
                using (TextWriter writer = lsrMode ? (TextWriter)new StringWriter() : File.CreateText(tempFile))
                {
                    CfgDfaParamsBuilder builder = new CfgDfaParamsBuilder(new TextDocumentIdentifier(docContext.TextDocument.uri), lsrMode ? null : tempFile);
                    CfgDotFileForNodeGenerator<object> gen = new CfgDotFileForNodeGenerator<object>(cfgs[0]);
                    gen.FullInstruction = true;
                    gen.BlockEmittedEvent += (block, subgraph) => builder.AddBlock<object>(block, subgraph);
                    gen.Report(writer);
                    result = builder.Params;
                    if (lsrMode)
                    {
                        writer.Flush();
                        result.dotContent = writer.ToString();
                    }
                }
            }
            else
            {
                //An Empty
                result = new CfgDfaParams(new TextDocumentIdentifier(docContext.TextDocument.uri));
            }
            return result;
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
