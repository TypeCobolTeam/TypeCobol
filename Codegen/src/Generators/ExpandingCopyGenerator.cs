using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    public class ExpandingCopyGenerator : IGenerator
    {
        /// <summary>
        /// The Compilation Document.
        /// </summary>
        public TypeCobol.Compiler.CompilationDocument CompilationResults
        {
            get;
            private set;
        }

        /// <summary>
        /// The Destination.
        /// </summary>
        public StringBuilder Destination
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        public ExpandingCopyGenerator(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination)
        {
            System.Diagnostics.Contracts.Contract.Requires(destination != null);
            this.CompilationResults = document;
            Destination = destination ?? destination.Append(System.Console.Out);
            Actions = new List<Action>();
            CreateTargetDocument();
        }

        public List<Diagnostic> Diagnostics
        {get;}

        public IReadOnlyDictionary<string, TimeSpan> PerformanceReport { get; private set; }

        /// <summary>
        /// The Target Generated Document
        /// </summary>
        public TypeCobol.Compiler.Source.SourceDocument TargetDocument
        {
            get;
            private set;
        }

        private enum ActionKind
        {
            Insert,
            Replace,
            Delete,
        };

        /// <summary>
        /// The Action
        /// </summary>
        private struct Action
        {
            /// <summary>
            /// The Kind of action.
            /// </summary>
            public ActionKind Kind
            {
                get;set;
            }

            /// <summary>
            /// Position of the action
            /// </summary>
            public Position From
            {
                get;
                set;
            }

            /// <summary>
            /// Ending position of the action if any
            /// </summary>
            public Position To
            {
                get;
                set;
            }

            /// <summary>
            /// The Action's text.
            /// </summary>
            public Compiler.Source.StringSourceText Text
            {
                get;
                set;
            }                
        }

        /// <summary>
        /// The list of action
        /// </summary>
        private List<Action> Actions
        {
            get;
            set;
        }

        /// <summary>
        /// The Generator current Columns Layout
        /// </summary>
        public ColumnsLayout Layout
        {
            get;
            private set;
        }

        /// <summary>
        /// Create the Target Document.
        /// </summary>
        private void CreateTargetDocument()
        {
            TargetDocument = new Compiler.Source.SourceDocument(/*new StringSourceText()*/);
            //Insert all input lines
            StringBuilder sw = new StringBuilder();
            int lineNumber = 0;
            foreach (TypeCobol.Compiler.Scanner.ITokensLine line in this.CompilationResults.TokensLines)
            {
                lineNumber += 1;
                sw.AppendLine(line.Text);
            }
            //Load the Original source code
            TargetDocument.LoadSourceText(sw.ToString());
            //TargetDocument.Dump();
            EraseCompilerDirectiveTokens();
        }

        /// <summary>
        /// Remove all copy directives tokens.
        /// </summary>
        private void EraseCompilerDirectiveTokens()
        {
            foreach (TypeCobol.Compiler.Scanner.ITokensLine line in this.CompilationResults.TokensLines)
            {
                Compiler.Parser.CodeElementsLine codeElemnLine = line as Compiler.Parser.CodeElementsLine;
                if (codeElemnLine?.TokensWithCompilerDirectives != null)
                {
                    foreach (var token in codeElemnLine.TokensWithCompilerDirectives)
                    {
                        if (token is CompilerDirectiveToken)
                        {
                            CompilerDirectiveToken compDirToken = token as CompilerDirectiveToken;
                            if (compDirToken.OriginalTokens != null)
                            {
                                EraseCompilerDirectiveToken(compDirToken.OriginalTokens);
                            }
                        }
                    }
                }
            }
        }
        /// <summary>
        /// Get the From and To Positions of this Node based on the consumed Token, if no ConsumedTokens the return value is NULL.
        /// In the consumed tokens span over several lines then the size of the newline sequence is included for each line.
        /// The method also calculate the ending span offset from the beginning of the last line.
        /// It also get the list of Line numbers occupated by this node, and the offset of each line.
        /// Item1 is the start column
        /// Item2 is the en column
        /// Item3 is the span
        /// Item4 is the list of line numbers
        /// Item5 is the list of offset inside the line numbers.
        /// 
        /// </summary>
        public Tuple<int, int, int, List<int>, List<int>> FromToPositions(IList<Token> ConsumedTokens)
        {
            if (ConsumedTokens.Count > 0)
            {
                int ln_size = System.Environment.NewLine.Length;
                int from = -1;
                int to = 0;
                int i = 0;
                int span = 0;
                List<int> lineNumbers = new List<int>();
                List<int> lineOffsets = new List<int>();
                SourceDocument.SourceLine srcFirstLine = null;
                do
                {
                    var token = ConsumedTokens[i];
                    if (!(token is TypeCobol.Compiler.Preprocessor.ImportedToken))
                    {//Don't take in account imported tokens -> This avoid including lines that come from COPYs files.
                        int curLineIndex = ConsumedTokens[i].Line;
                        if (curLineIndex == 0)
                        {
                            return null;
                        }
                        if (lineNumbers.Count > 0)
                        {//Add lines between
                            int lastLine = lineNumbers[lineNumbers.Count - 1];
                            while (++lastLine < curLineIndex)
                            {
                                lineNumbers.Add(lastLine);
                                SourceDocument.SourceLine srcLine = TargetDocument[lastLine - 1];
                                if (srcFirstLine != null) lineOffsets.Add(srcLine.From - srcFirstLine.From);
                            }
                        }
                        SourceDocument.SourceLine curLine = TargetDocument[curLineIndex - 1];
                        if (srcFirstLine == null)
                            srcFirstLine = curLine;
                        lineNumbers.Add(curLineIndex);
                        lineOffsets.Add(curLine.From - srcFirstLine.From);
                        span = 0;
                        while ((i < ConsumedTokens.Count) && ((curLineIndex == ConsumedTokens[i].Line)
                            || (ConsumedTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken)))
                        {
                            if (!(ConsumedTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken))
                            {
                                if (from == -1)
                                    from = ConsumedTokens[i].Column;
                                span = ConsumedTokens[i].EndColumn;
                            }
                            i++;
                        }
                        to = (curLine.From + span) - srcFirstLine.From;
                    }
                    else
                    {
                        i++;
                    }
                } while (i < ConsumedTokens.Count);
                lineNumbers.TrimExcess();
                lineOffsets.TrimExcess();
                return new Tuple<int, int, int, List<int>, List<int>>(from, to, span, lineNumbers, lineOffsets);
            }
            return null;
        }

        /// <summary>
        /// Compute positions in the target document corresponding to a list of tokens.
        /// </summary>
        /// <param name="tokens">The list of token to compute positions</param>
        /// <param name="from">Output of the from position</param>
        /// <param name="to">Output of the to position</param>
        public Tuple<int, int, int, List<int>, List<int>> ComputePositions(IList<Token> tokens, out Position from, out Position to)
        {
            Tuple<int, int, int, List<int>, List<int>> positions = FromToPositions(tokens);
            SourceDocument.SourceLine firstLine = TargetDocument[positions.Item4[0] - 1];
            SourceDocument.SourceLine lastLine = TargetDocument[positions.Item4[positions.Item4.Count - 1] - 1];
            from = new Position(firstLine.From + positions.Item1 - 1);
            to = new Position(lastLine.From + positions.Item2);
            return positions;
        }

        /// <summary>
        /// Insert action to erase compiler directives
        /// </summary>
        /// <param name="tokens">The tokens to erase</param>
        private void EraseCompilerDirectiveToken(IList<Token> tokens)
        {
            Position from = null;
            Position to = null;
            Tuple<int, int, int, List<int>, List<int>> positions = ComputePositions(tokens, out from, out to);
            TargetDocument.Source.AddPosition(from);//from position
            TargetDocument.Source.AddPosition(to);//To Pos
            Compiler.Source.StringSourceText del_buffer = new Compiler.Source.StringSourceText(new string(' ', to.Pos - from.Pos));
            Action action = new Action() { Kind = ActionKind.Insert, From = from, To = to, Text = del_buffer };
            Actions.Add(action);
        }

        public const int LEGAL_COBOL_LINE_LENGTH = 72;
        /// <summary>
        /// Try to determine what can be erased after a copy instruction
        /// </summary>
        public Position ComputeErasureAfterCopyInstructionAction(TypeCobol.Compiler.Scanner.Token copyToken, TypeCobol.Compiler.Scanner.Token lastToken, out bool bAddNewLine)
        {            
            bAddNewLine = false;
            Position cfrom = null;
            Position cto = null;
            Tuple<int, int, int, List<int>, List<int>> positions = ComputePositions(new List<TypeCobol.Compiler.Scanner.Token>() { copyToken, lastToken }, out cfrom, out cto);
            TargetDocument.Source.AddPosition(cfrom);//from position
            if (this.Layout == ColumnsLayout.FreeTextFormat)
                return cfrom;

            SourceDocument.SourceLine sourceLine = TargetDocument[positions.Item4[positions.Item4.Count - 1] - 1];
            int lineLen = -1;
            int lineStartOffset = -1;
            int lineEndOffset = -1;
            int from = sourceLine.From;
            int to = sourceLine.To;
            lineLen = TargetDocument.Source.GetLineInfo(from, out lineStartOffset, out lineEndOffset);
            if (lineLen > LEGAL_COBOL_LINE_LENGTH)
            {   //Delete all characters in column 72-80
                int replace_len = lineLen - LEGAL_COBOL_LINE_LENGTH;
                Position efrom = new Position(from + LEGAL_COBOL_LINE_LENGTH);
                Position eto = new Position(from + lineLen);
                TargetDocument.Source.AddPosition(efrom);
                TargetDocument.Source.AddPosition(eto);
                Compiler.Source.StringSourceText del_buffer = new Compiler.Source.StringSourceText(new string(' ', replace_len));
                Action action = new Action() { Kind = ActionKind.Insert, From = efrom, To = eto, Text = del_buffer };
                Actions.Add(action);
            }
            bAddNewLine = true;
            return sourceLine.End;
        }

        /// <summary>
        /// Create the action of Inserting all Imported Tokens from the current iterator
        /// </summary>
        /// <param name="firstToken">The First Imported Token</param>
        /// <param name="tokensIterator"></param>
        private void InsertImportedTokensAction(TypeCobol.Compiler.Scanner.Token firstToken, Compiler.Scanner.ITokensLinesIterator tokensIterator)
        {
            TypeCobol.Compiler.Preprocessor.ImportedToken importedToken = (TypeCobol.Compiler.Preprocessor.ImportedToken)firstToken;
            TypeCobol.Compiler.Directives.CopyDirective copyDirective = importedToken.CopyDirective;
            Token copyToken = copyDirective.COPYToken;
            Token copyNameToken = copyDirective.TextNameSymbol;
            int startLine = copyToken.Line;
            int endLine = copyNameToken.Line;
            int startColumn = copyToken.Column;
            int endColumn = copyNameToken.EndColumn;
            List<Token> tokens = new List<Token>() { copyToken, copyNameToken };
            Position from = null;
            Position to = null;
            ComputePositions(tokens, out from, out to);
            TargetDocument.Source.AddPosition(from);//from position
            TargetDocument.Source.AddPosition(to);//To Pos
            //Delete the copy Instruction
            bool bAddNewLine = false;
            Position insertPos = ComputeErasureAfterCopyInstructionAction(copyToken, copyNameToken, out bAddNewLine);

            Compiler.Source.StringSourceText buffer = new Compiler.Source.StringSourceText();
            if (bAddNewLine)
                buffer.Insert(Environment.NewLine, 0, 0);//First new line
            int currentLine = firstToken.Line;
            int currentColumn = 1;
            TypeCobol.Compiler.Scanner.Token currentToken = firstToken;
            do
            {
                TypeCobol.Compiler.Preprocessor.ImportedToken impToken = currentToken as TypeCobol.Compiler.Preprocessor.ImportedToken;
                if (impToken.CopyDirective.COPYToken.Line != copyToken.Line ||
                    impToken.CopyDirective.COPYToken.Column != copyToken.Column)
                    break;//This is from another COPY
                int nInserPos = buffer.Size;
                int newLine = currentToken.Line;
                if (newLine != currentLine)
                {
                    currentColumn = 1;
                    currentLine = newLine;
                    buffer.Insert(Environment.NewLine, nInserPos, nInserPos);
                    nInserPos += Environment.NewLine.Length;
                }
                int nPadLength = currentToken.Column - currentColumn;
                if (nPadLength > 0)
                {
                    buffer.Insert(new string(' ', nPadLength), nInserPos, nInserPos);
                    nInserPos += nPadLength;
                }
                buffer.Insert(currentToken.Text, nInserPos, nInserPos);
                currentColumn = currentToken.EndColumn + 1;
                currentToken = tokensIterator.NextToken();
            } while (currentToken.TokenType != TokenType.EndOfFile && currentToken is TypeCobol.Compiler.Preprocessor.ImportedToken);
            buffer.Insert(Environment.NewLine, buffer.Size, buffer.Size);
            Action action = new Action() { Kind = ActionKind.Insert, From = to, To = to, Text = buffer };
            Actions.Add(action);
        }

        /// <summary>
        /// Create a Replace action
        /// </summary>
        /// <param name="token">The Replace token</param>
        private void ReplaceAction(TypeCobol.Compiler.Preprocessor.ReplacedPartialCobolWord token)
        {
            Position from = null;
            Position to = null;
            ComputePositions(new List<Token>() { token }, out from, out to);

            TargetDocument.Source.AddPosition(from);//from position
            TargetDocument.Source.AddPosition(to);//To Pos
            Compiler.Source.StringSourceText buffer = new Compiler.Source.StringSourceText();
            buffer.Insert(token.Text, 0, 0);//First new line
            //So pad the replacement if necessary      
            int nPadLen = token.SourceText.Length - token.Text.Length;
            if (nPadLen > 0)
            {
                buffer.Insert(new string(' ', nPadLen), buffer.Size, buffer.Size);
            }
            //Delete the copy Instruction
            Action action = new Action() { Kind = ActionKind.Replace, From = from, To = to, Text = buffer };
            Actions.Add(action);            
        }

        /// <summary>
        /// Perform all actiosn on the target buffer
        /// </summary>
        private void PerformActions()
        {
            foreach (Action action in this.Actions)
            {
                switch(action.Kind)
                {
                    case ActionKind.Delete:
                        {
                            TargetDocument.Source.Delete(action.From.Pos, action.To.Pos);
                        }
                        break;
                    case ActionKind.Insert:
                        {
                            TargetDocument.Source.Insert(action.Text, action.From.Pos, action.To.Pos);
                        }
                        break;
                    case ActionKind.Replace:
                        {
                            TargetDocument.Source.Insert(action.Text, action.From.Pos, action.To.Pos);
                        }
                        break;
                }
            }
        }

        /// <summary>
        /// Generate
        /// </summary>
        /// <param name="compilationUnit"></param>
        /// <param name="columns"></param>
        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            this.Layout = columns;
            Compiler.Preprocessor.ProcessedTokensDocument processedTokensDocument = compilationUnit.ProcessedTokensDocumentSnapshot;

            // Create a token iterator on top of pre-processed tokens lines
            Compiler.Scanner.ITokensLinesIterator tokensIterator = Compiler.Preprocessor.ProcessedTokensDocument.GetProcessedTokensIterator(
                compilationUnit.TextSourceInfo, processedTokensDocument.Lines, compilationUnit.CompilerOptions, false);

            var stopwatch = Stopwatch.StartNew();

            TypeCobol.Compiler.Scanner.Token curToken = null;
            while ((curToken = (curToken == null ? tokensIterator.NextToken() : curToken)) != null)
            {
                var token = curToken;
                curToken = null;
                if (token.TokenType == Compiler.Scanner.TokenType.EndOfFile)
                    break;
                else if (token is TypeCobol.Compiler.Preprocessor.ImportedToken)
                {
                    InsertImportedTokensAction(token, tokensIterator);
                    curToken = tokensIterator.CurrentToken;
                }
                else if (token is TypeCobol.Compiler.Preprocessor.ReplacedPartialCobolWord)
                {
                    ReplaceAction(token as TypeCobol.Compiler.Preprocessor.ReplacedPartialCobolWord);                        
                }
            }

            var addActionsElapsed = stopwatch.Elapsed;
            stopwatch.Restart();

            //Now Run Actions
            PerformActions();
            TargetDocument.Write(Destination);

            var performActionsElapsed = stopwatch.Elapsed;
            stopwatch.Reset();

            PerformanceReport = new Dictionary<string, TimeSpan>()
                                {
                                    {"AddActions", addActionsElapsed},
                                    {"PerformActions", performActionsElapsed}
                                };
        }

        public void GenerateLineMapFile(Stream stream)
        {            
        }

        public string TypeCobolVersion { get; }

        public bool HasLineMapData => false;
    }
}
