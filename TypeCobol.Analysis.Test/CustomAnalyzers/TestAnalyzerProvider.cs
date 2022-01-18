using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Analysis.Test.CustomAnalyzers
{
    /// <summary>
    /// This provider is loaded and called in CLI.Test, see TestCustomAnalyzers unit test.
    /// </summary>
    public class TestAnalyzerProvider : IAnalyzerProvider
    {
        public ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers(TypeCobolOptions options, TextSourceInfo textSourceInfo)
        {
            return new ISyntaxDrivenAnalyzer[] { new DummySyntaxDrivenAnalyzer(nameof(DummySyntaxDrivenAnalyzer)) };
        }

        public IQualityAnalyzer[] CreateQualityAnalyzers(TypeCobolOptions options)
        {
            return new IQualityAnalyzer[]
                   {
                       new DummyQualityAnalyzer(nameof(DummyQualityAnalyzer)),
                       new DummyQualityAnalyzerErrorInAnalysis(nameof(DummyQualityAnalyzerErrorInAnalysis)),
                       new DummyQualityAnalyzerWithCfg(nameof(DummyQualityAnalyzerWithCfg))
                   };
        }
    }
}
