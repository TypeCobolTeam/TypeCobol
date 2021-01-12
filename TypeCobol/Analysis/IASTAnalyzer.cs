using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Contract for an analyzer based on complete syntax tree.
    /// </summary>
    public interface IASTAnalyzer : IAnalyzer, IASTVisitor
    {

    }
}
