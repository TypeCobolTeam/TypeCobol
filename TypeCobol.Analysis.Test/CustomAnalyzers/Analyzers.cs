using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Symbols;

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
            AddDiagnostic(new Diagnostic(MessageCode.Info, Diagnostic.Position.Default, $"Analyzer '{Identifier}': starting AST building..."));
        }

        public override void EndCobolProgram(ProgramEnd end)
        {
            AddDiagnostic(new Diagnostic(MessageCode.Info, Diagnostic.Position.Default, $"Analyzer '{Identifier}': finished AST building."));
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
            AddDiagnostic(new Diagnostic(MessageCode.Info, Diagnostic.Position.Default, $"Analyzer '{Identifier}': {info}"));
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

    /// <summary>
    /// Used to demonstrate retrieval of CFG result in an external analyzer
    /// </summary>
    internal class DummyQualityAnalyzerWithCfg : QualityAnalyzerBase
    {
        public DummyQualityAnalyzerWithCfg(string identifier)
            : base(identifier)
        {

        }

        public override object GetResult()
        {
            return null;
        }

        public override void Inspect(TemporarySemanticDocument temporarySemanticDocument)
        {
            string id = CfgDfaAnalyzerFactory.GetIdForMode(CfgBuildingMode.WithDfa);//Expect CFG analysis is activated and with DFA too.
            if (temporarySemanticDocument.AnalyzerResults.TryGetResult(id, out IList<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>> graphs))
            {
                foreach (var controlFlowGraph in graphs)
                {
                    var programNode = controlFlowGraph.ProgramOrFunctionNode;
                    Assert.IsTrue(programNode is Program);
                    var programNameToken = ((Program) programNode).CodeElement.ProgramName.NameLiteral.Token;
                    var diagnostic = new Diagnostic(MessageCode.Info, programNameToken.Position(), $"CFG/DFA analysis: control flow graphs contains {controlFlowGraph.AllBlocks.Count} blocks.");
                    AddDiagnostic(diagnostic);
                }
            }
            else
            {
                var diagnostic = new Diagnostic(MessageCode.Info, Diagnostic.Position.Default, "CFG/DFA analysis: no control flow graph found for this file.");
                AddDiagnostic(diagnostic);
            }
        }
    }
}
