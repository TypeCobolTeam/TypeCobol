using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    /// <summary>
    /// Base class for Cobol document refactorings. This specialization of AbstractCommand
    /// delegates its processing to a refactoring processor.
    /// </summary>
    /// <typeparam name="TProcessor">Refactoring processor type</typeparam>
    /// <remarks>This class allows modifying only one document at a time. Multi-document refactorings are not supported.</remarks>
    internal class AbstractSingleDocumentRefactoring<TProcessor> : AbstractCommand
        where TProcessor : IRefactoringProcessor, new()
    {
        protected TProcessor Processor { get; }

        public AbstractSingleDocumentRefactoring(TypeCobolServer typeCobolServer)
            : base(typeCobolServer)
        {
            Processor = new TProcessor();
        }

        public override object Run(object[] arguments)
        {
            // Identify target
            var targetDocumentIdentifier = Processor.PrepareRefactoring(arguments);
            var target = Server.GetDocumentContextFromStringUri(targetDocumentIdentifier.uri, Workspace.SyntaxTreeRefreshLevel.RebuildNodes);
            var compilationUnit = target.FileCompiler?.CompilationResultsForProgram;
            if (compilationUnit == null)
            {
                throw new InvalidOperationException($"Could not find compilation results for document '{target.Uri}'.");
            }

            // Additional checks (if any)
            Processor.CheckTarget(compilationUnit);

            // Compute edits asynchronously: create a message for ourselves
            Server.Workspace.MessagesActionsQueue.Enqueue(new MessageActionWrapper(() => ComputeTextEdits(target.Uri, compilationUnit)));
            return new object(); // Non-null blank object means ok
        }

        private void ComputeTextEdits(Uri documentUri, CompilationUnit compilationUnit)
        {
            // Delegate text edit computation to our processor
            var refactoring = Processor.PerformRefactoring(compilationUnit);

            // Create WorkspaceApplyEditRequest and send to client
            var workspaceEdit = new WorkspaceEdit() { changes = new Dictionary<string, IList<TextEdit>>() { { documentUri.OriginalString, refactoring.TextEdits } } };
            var applyWorkspaceEditParams = new ApplyWorkspaceEditParams() { label = refactoring.Label, edit = workspaceEdit };
            Server.RpcServer.SendRequest(WorkspaceApplyEditRequest.Type, applyWorkspaceEditParams, out _)
                .ConfigureAwait(false); // No need to wait for response and therefore no need to bounce back on original thread
        }
    }
}
