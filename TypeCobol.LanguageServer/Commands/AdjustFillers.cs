using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands
{
    internal class AdjustFillers : AbstractCommand
    {
        private class AdjustFillerVisitor : AbstractAstVisitor
        {
            private readonly Stack<DataRedefines> _toProcess = new();
            private readonly StringBuilder _newTextBuilder = new();

            public int ModifiedFillersCount { get; private set; }

            public List<TextEdit> TextEdits { get; } = new();

            public override bool Visit(DataRedefines dataRedefines)
            {
                // Push REDEFINES on stack and continue visit.
                // Each REDEFINES is processed when we end its visit, to ensure edits are created in document order.
                _toProcess.Push(dataRedefines);
                return true;
            }

            public override void EndNode(Node node)
            {
                if (_toProcess.Count > 0 && _toProcess.Peek() == node)
                {
                    // We are leaving the current REDEFINES, compute edits for it and remove it from stack.
                    Process(_toProcess.Pop());
                }
            }

            private void Process(DataRedefines dataRedefines)
            {
                if (dataRedefines.IsInsideCopy())
                {
                    // REDEFINES comes from an included COPY, do not alter !
                    return;
                }

                if (!dataRedefines.HasChildrenExcludingIndex())
                {
                    // Inline REDEFINES, nothing to do
                    return;
                }

                long? targetSize = dataRedefines.RedefinedVariable?.PhysicalLength;
                if (!targetSize.HasValue)
                {
                    // Unable to resolve target of the REDEFINES
                    return;
                }

                long delta = targetSize.Value - dataRedefines.PhysicalLength;
                if (delta == 0)
                {
                    // The REDEFINES size already matches the target size, no need to change anything
                    return;
                }

                // delta must be considered for a single occurence
                delta /= dataRedefines.MaxOccurencesCount;

                // Examine REDEFINES last child
                Debug.Assert(dataRedefines.Children[^1] is DataDefinition);
                var lastChild = (DataDefinition)dataRedefines.Children[^1];
                Debug.Assert(lastChild.CodeElement != null);
                bool lastChildIsFiller = lastChild.IsFiller();
                long fillerSize = lastChildIsFiller ? lastChild.PhysicalLength : 0;
                long adjustedFillerSize = fillerSize + delta;

                if (lastChildIsFiller)
                {
                    // adjustedFillerSize must be considered for a single occurence
                    adjustedFillerSize /= lastChild.MaxOccurencesCount;
                }

                if (adjustedFillerSize < 0)
                {
                    // The REDEFINES size exceeds its target size, unable to adjust
                    return;
                }

                ModifiedFillersCount++;
                if (adjustedFillerSize > 0)
                {
                    // Either increase the size of existing FILLER or create a new one
                    AddOrAdjust();
                }
                else if (lastChildIsFiller)
                {
                    // adjustedFillerSize is 0, we have to remove the FILLER
                    Remove();
                }

                void AddOrAdjust()
                {
                    TextEdit textEdit = null;
                    if (lastChildIsFiller)
                    {
                        // Try to adjust by modifying the PICTURE of the FILLER
                        var pictureCharacterString = lastChild.Picture?.Token;
                        if (pictureCharacterString != null)
                        {
                            string adjustedPictureCharacterString = $"X({adjustedFillerSize})";
                            int deltaChar = adjustedPictureCharacterString.Length - pictureCharacterString.Length; // Number of chars needed to accomodate the new picture character string
                            string sourceText = pictureCharacterString.TokensLine.SourceText;
                            int index = pictureCharacterString.StopIndex - (int)CobolFormatAreas.EndNumber; // Index marking the end of the picture character string in SourceText
                            const int maxSourceTextLength = (int)CobolFormatAreas.End_B - (int)CobolFormatAreas.Indicator; // 65 chars max

                            // Build new text: this is made of adjusted picture followed by everything previously written on the same line after the original picture character string
                            _newTextBuilder.Append(adjustedPictureCharacterString);
                            if (sourceText.Length + deltaChar > maxSourceTextLength && sourceText[^deltaChar..].All(c => c == ' '))
                            {
                                // New source text would go beyond column 72, but we have enough spaces at the end, so consume them
                                _newTextBuilder.Append(sourceText[index..^deltaChar]);
                            }
                            else
                            {
                                // Either new source text ends before column 72 -> ok
                                // Or we don't have spaces to delete, in that case the TextEdit will produce invalid code, dev will have to fix the source manually !
                                _newTextBuilder.Append(sourceText[index..]);
                            }

                            // Handle comment area
                            if (pictureCharacterString.TokensLine.CommentText != null)
                            {
                                if (deltaChar < 0)
                                {
                                    // The adjusted picture character string is smaller, add spaces to compensate
                                    _newTextBuilder.Append(' ', -deltaChar);
                                }

                                // Add comments
                                _newTextBuilder.Append(pictureCharacterString.TokensLine.CommentText);
                            }

                            string newText = _newTextBuilder.ToString();
                            _newTextBuilder.Clear();

                            var start = new Position() { line = pictureCharacterString.Line, character = pictureCharacterString.StartIndex };
                            var end = new Position() { line = pictureCharacterString.Line, character = pictureCharacterString.TokensLine.Length };
                            textEdit = TextEdit.Replace(start, end, newText);
                        }
                        // else PICTURE could not be found, unable to modify the FILLER (for example 05 FILLER USAGE POINTER)
                    }
                    else
                    {
                        // Last child is a regular data, add a new FILLER after
                        var levelNumber = lastChild.CodeElement.LevelNumber;
                        Debug.Assert(levelNumber != null);

                        // Detect level and indentation from last child
                        string level = levelNumber.Token.SourceText;
                        int indent = levelNumber.Token.StartIndex;
                        _newTextBuilder.AppendLine();
                        _newTextBuilder.Append(' ', indent);
                        _newTextBuilder.Append(level);
                        _newTextBuilder.Append(" FILLER PIC X(");
                        _newTextBuilder.Append(adjustedFillerSize);
                        _newTextBuilder.Append(").");
                        string newText = _newTextBuilder.ToString();
                        _newTextBuilder.Clear();

                        // Look for the last node (last child itself or the last node of its descendants for a group)
                        var lastNode = lastChild.GetLastNode();

                        // Insert at the end of last node line
                        Debug.Assert(lastNode.CodeElement != null);
                        var lastNodeLastToken = lastNode.CodeElement.ConsumedTokens.Last();
                        int line = lastNodeLastToken.Line;
                        int column = lastNodeLastToken.TokensLine.Length;
                        var position = new Position() { line = line, character = column };
                        textEdit = TextEdit.Insert(position, newText);
                    }

                    if (textEdit != null)
                    {
                        TextEdits.Add(textEdit);
                    }
                }

                void Remove()
                {
                    // To preserve format, the removal of a FILLER is implemented as erasing each of its token,
                    // while keeping the comments that may have been consumed.
                    var tokens = lastChild.CodeElement.ConsumedTokens;
                    Debug.Assert(tokens != null);
                    Debug.Assert(tokens.Count > 0);
                    var eraseGroups = tokens
                        .Where(t => t.TokenType != TokenType.CommentLine && t.TokenType != TokenType.FloatingComment)
                        .GroupBy(t => t.Line);

                    // Create one text edit per line
                    foreach (var eraseGroup in eraseGroups)
                    {
                        var start = new Position() { line = eraseGroup.Key, character = eraseGroup.First().StartIndex };
                        var end = new Position() { line = eraseGroup.Key, character = eraseGroup.Last().StopIndex + 1 };
                        string newText = new string(' ', end.character - start.character);
                        TextEdits.Add(TextEdit.Replace(start, end, newText));
                    }
                }
            }
        }

        public static AdjustFillers Create(TypeCobolServer typeCobolServer) => new(typeCobolServer);

        public AdjustFillers(TypeCobolServer typeCobolServer)
            : base(typeCobolServer)
        {

        }

        public override object Run(object[] arguments)
        {
            if (arguments == null || arguments.Length == 0 || !TryReadArgumentAs(arguments[0], out TextDocumentIdentifier textDocumentIdentifier))
            {
                throw new ArgumentException("Invalid arguments for command.", nameof(arguments));
            }

            var target = Server.GetDocumentContextFromStringUri(textDocumentIdentifier.uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            var programClassDocument = target.FileCompiler?.CompilationResultsForProgram?.ProgramClassDocumentSnapshot;
            if (programClassDocument == null)
            {
                throw new InvalidOperationException($"Could not get AST for document '{target.Uri}'.");
            }

            // Compute edits asynchronously: create a message for ourselves
            Server.Workspace.MessagesActionsQueue.Enqueue(new MessageActionWrapper(() => ComputeTextEdits(target.Uri, programClassDocument)));
            return new object(); // Non-null blank response
        }

        private void ComputeTextEdits(Uri documentUri, ProgramClassDocument programClassDocument)
        {
            // Compute edits using a visitor
            var visitor = new AdjustFillerVisitor();
            programClassDocument.Root?.AcceptASTVisitor(visitor);

            // Create WorkspaceApplyEditRequest and send to client
            string label = $"Adjust FILLERs: {visitor.ModifiedFillersCount} FILLER(s) modified";
            var workspaceEdit = new WorkspaceEdit() { changes = new Dictionary<string, IList<TextEdit>>() { { documentUri.OriginalString, visitor.TextEdits } } };
            var applyWorkspaceEditParams = new ApplyWorkspaceEditParams() { label = label, edit = workspaceEdit };
            Server.RpcServer.SendRequest(WorkspaceApplyEditRequest.Type, applyWorkspaceEditParams, out _)
                .ConfigureAwait(false); // No need to wait for response and therefore no need to bounce back on original thread
        }
    }
}
