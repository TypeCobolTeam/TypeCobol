using TypeCobol.Compiler.CupParser.NodeBuilder;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Contract for an analyzer based on the program syntax.
    /// </summary>
    public interface ISyntaxDrivenAnalyzer : IAnalyzer, IProgramClassBuilderNodeListener
    {

    }
}
