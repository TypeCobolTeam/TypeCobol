using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Test.CustomAnalyzers
{
    /// <summary>
    /// Simple SyntaxDrivenAnalyzer to demonstrate dynamic load and use during AST building
    /// </summary>
    internal class DummySyntaxDrivenAnalyzer : SyntaxDrivenAnalyzerBase
    {
        public DummySyntaxDrivenAnalyzer(string identifier)
            : base(identifier)
        {

        }

        public override object GetResult()
        {
            return null;
        }

        public override void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
            AddDiagnostic(new Diagnostic(MessageCode.Info, 0, 0, 0, $"Analyzer '{Identifier}': starting AST building..."));
        }

        public override void EndCobolProgram(ProgramEnd end)
        {
            AddDiagnostic(new Diagnostic(MessageCode.Info, 0, 0, 0, $"Analyzer '{Identifier}': finished AST building."));
        }
    }

    /// <summary>
    /// Simple ASTAnalyzer to demonstrate dynamic load and use in QualityCheck
    /// </summary>
    internal class DummyASTAnalyzer : ASTAnalyzerBase
    {
        public DummyASTAnalyzer(string identifier)
            : base(identifier)
        {

        }

        public override object GetResult()
        {
            return null;
        }

        public override bool Visit(SourceFile sourceFile)
        {
            AddDiagnostic(new Diagnostic(MessageCode.Info, 0, 0, 0, $"Analyzer '{Identifier}': visiting source file."));
            return false;
        }
    }
}
