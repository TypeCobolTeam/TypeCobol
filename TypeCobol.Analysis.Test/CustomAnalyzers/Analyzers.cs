using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;

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
    /// Simple QualityAnalyzer to demonstrate dynamic load and use in QualityCheck
    /// </summary>
    internal class DummyQualityAnalyzer : QualityAnalyzerBase
    {
        private class AstVisitor : AbstractAstVisitor
        {
            private readonly DummyQualityAnalyzer _owner;

            public AstVisitor(DummyQualityAnalyzer owner)
            {
                _owner = owner;
            }

            public override bool Visit(SourceFile sourceFile)
            {
                _owner.AddInfo($"source file has {sourceFile.Programs.Count()} program(s), main program is '{sourceFile.MainProgram.Name}'.");
                return false;
            }
        }

        private readonly AstVisitor _astVisitor;

        public DummyQualityAnalyzer(string identifier)
            : base(identifier)
        {
            _astVisitor = new AstVisitor(this);
        }

        public override object GetResult()
        {
            return null;
        }

        private void AddInfo(string info)
        {
            AddDiagnostic(new Diagnostic(MessageCode.Info, 0, 0, 0, $"Analyzer '{Identifier}': {info}"));
        }

        public override void Inspect(ProcessedTokensDocument processedTokensDocument)
        {
            AddInfo($"number of tokens (after preprocessing): {processedTokensDocument.GetProcessedTokens().Count()}");
        }

        public override void Inspect(CodeElementsDocument codeElementsDocument)
        {
            AddInfo($"number of code elements: {codeElementsDocument.CodeElements.Count()}");
        }

        public override void Inspect(ProgramClassDocument programClassDocument)
        {
            programClassDocument.Root.AcceptASTVisitor(_astVisitor);
        }
    }
}
