using System.Diagnostics;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands
{
    internal class AdjustFillers : AbstractCommand
    {
        private class AdjustFillerVisitor(List<TextEdit> textEdits) : AbstractAstVisitor
        {
            public override bool Visit(DataRedefines dataRedefines)
            {
                if (dataRedefines.IsInsideCopy())
                {
                    // REDEFINES comes from an included COPY, do not alter !
                    return true;
                }

                if (dataRedefines.ChildrenCount == 0)
                {
                    // Inline REDEFINES, nothing to do
                    return true;
                }

                long? targetSize = dataRedefines.RedefinedVariable?.PhysicalLength;
                if (!targetSize.HasValue)
                {
                    // Unable to resolve target of the REDEFINES
                    return true;
                }

                long delta = targetSize.Value - dataRedefines.PhysicalLength;
                if (delta == 0)
                {
                    // The REDEFINES size already matches the target size, no need to change anything
                    return true;
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
                if (adjustedFillerSize < 0)
                {
                    // The REDEFINES size exceeds its target size, unable to adjust
                    return true;
                }

                if (adjustedFillerSize > 0)
                {
                    // Either increase the size of existing FILLER or create a new one
                    AddOrAdjust();
                }
                else if (lastChildIsFiller)
                {
                    // adjustedFillerSize is 0, we need to remove the FILLER
                    Remove();
                }

                return true;

                void AddOrAdjust()
                {
                    TextEdit textEdit = null;
                    if (lastChildIsFiller)
                    {
                        // Try to adjust by modifying the PICTURE of the FILLER
                        var pictureCharacterString = lastChild.Picture?.Token;
                        if (pictureCharacterString != null)
                        {
                            // Replace the picture with an adjusted PIC X(size)
                            var start = new Position(pictureCharacterString.Line, pictureCharacterString.StartIndex);
                            var end = new Position(pictureCharacterString.Line, pictureCharacterString.StopIndex + 1);
                            textEdit = new TextEdit(new VsCodeProtocol.Range(start, end), $"X({adjustedFillerSize})");
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
                        string newText = $"{Environment.NewLine}{new string(' ', indent)}{level} FILLER PIC X({adjustedFillerSize}).";

                        // Insert right after last child
                        int line = lastChild.CodeElement.LineEnd;
                        int column = lastChild.CodeElement.StopIndex + 1;
                        var start = new Position(line, column);
                        var end = new Position(line, column);
                        textEdit = new TextEdit(new VsCodeProtocol.Range(start, end), newText);
                    }

                    if (textEdit != null)
                    {
                        textEdits.Add(textEdit);
                    }
                }

                void Remove()
                {
                    // Remove the whole code element (including spaces at the beginning of the line)
                    var start = new Position(lastChild.CodeElement.Line, 0);
                    var end = new Position(lastChild.CodeElement.LineEnd, lastChild.CodeElement.StopIndex + 1);
                    var textEdit = new TextEdit(new VsCodeProtocol.Range() { start = start, end = end }, string.Empty);
                    textEdits.Add(textEdit);
                }
            }
        }

        public static AdjustFillers Create(TypeCobolServer typeCobolServer) => new AdjustFillers(typeCobolServer);

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
            var textEdits = new List<TextEdit>();
            var visitor = new AdjustFillerVisitor(textEdits);
            programClassDocument.Root?.AcceptASTVisitor(visitor);

            // Create WorkspaceApplyEditRequest and send to client
            string label = $"Adjust FILLERs: {textEdits.Count} FILLER(s) modified";
            var workspaceEdit = new WorkspaceEdit() { changes = { { documentUri.OriginalString, textEdits } } };
            var applyWorkspaceEditParams = new ApplyWorkspaceEditParams() { label = label, edit = workspaceEdit };
            Server.RpcServer.SendRequest(WorkspaceApplyEditRequest.Type, applyWorkspaceEditParams)
                .ConfigureAwait(false); // No need to wait for response and therefore no need to bounce back on original thread
        }
    }
}
