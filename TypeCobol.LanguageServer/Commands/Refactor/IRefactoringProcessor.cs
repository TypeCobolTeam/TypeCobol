using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    /// <summary>
    /// A refactoring processor is responsible for implementing the refactoring logic
    /// independently of the language server currently running. This is an abstraction
    /// for an automated modification of source text.
    /// </summary>
    public interface IRefactoringProcessor
    {
        /// <summary>
        /// Collect and parse refactoring arguments.
        /// </summary>
        /// <param name="arguments">Untyped argument array, possibly null.</param>
        /// <returns>Non-null TextDocumentIdentifier found in args, an exception should be thrown if no identifier could be found.</returns>
        TextDocumentIdentifier PrepareRefactoring(object[] arguments);

        /// <summary>
        /// Perform validation of the refactoring target.
        /// </summary>
        /// <param name="compilationUnit">The program that should be modified by this refactoring.</param>
        void CheckTarget(CompilationUnit compilationUnit);

        /// <summary>
        /// Compute text edits corresponding to this refactoring.
        /// </summary>
        /// <param name="compilationUnit">Target of the refactoring.</param>
        /// <returns>A tuple made of a label describing the modification that has been performed and a list of TextEdits.</returns>
        (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit);
    }
}
