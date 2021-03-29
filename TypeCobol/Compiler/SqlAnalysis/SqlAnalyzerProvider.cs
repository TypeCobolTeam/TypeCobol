using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.SqlAnalysis
{
    /// <summary>
    /// Class For Sql Analyzer Provider
    /// </summary>
    public class SqlAnalyzerProvider : IAnalyzerProvider
    {
        private readonly ISyntaxDrivenAnalyzer[] _syntaxDrivenAnalyzers;
        private readonly IQualityAnalyzer[] _qualityAnalyzers;

        public SqlAnalyzerProvider()
        {
            _syntaxDrivenAnalyzers = new ISyntaxDrivenAnalyzer[0];
            _qualityAnalyzers = new IQualityAnalyzer[] { new SqlQualityAnalyzer() };
        }
        public IQualityAnalyzer[] CreateQualityAnalyzers([NotNull] TypeCobolOptions options)
        {
            return _qualityAnalyzers;
        }

        public ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo)
        {
            return _syntaxDrivenAnalyzers;
        }
    }
}
