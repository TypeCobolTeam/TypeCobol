using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.SqlAnalysis
{
    /// <summary>
    /// SQL Quality Analyzer class.
    /// </summary>
    public class SqlQualityAnalyzer : QualityAnalyzerBase
    {
        private readonly SqlASTChecker _sqlAstChecker;
        public SqlQualityAnalyzer() : base(typeof(SqlQualityAnalyzer).FullName)
        {
            _sqlAstChecker = new SqlASTChecker(DiagnosticList.Add);
        }

        public override object GetResult()
        {
            return null;
        }

        /// <summary>
        /// Perform inspection of Nodes (after cross-check).
        /// </summary>
        /// <param name="programClassDocument">Current full semantic document.</param>
        public override void Inspect(ProgramClassDocument programClassDocument)
        {
            // Perform checking of rules during the visit of the AST
            programClassDocument.Root.AcceptASTVisitor(_sqlAstChecker);
        }
    }
}
