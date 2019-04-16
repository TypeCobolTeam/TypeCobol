using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Cfg
{
    /// <summary>
    /// The Control Flow Graph Builder for a TypeCobol Program.
    /// </summary>
    public class ControlFlowGraphBuilder<D> : ProgramClassBuilderNodeListener
    {
        /// <summary>
        /// BasicBlock which instruction are Nodes.
        /// </summary>
        /// <typeparam name="D"></typeparam>
        public class BasicBlockForNode : BasicBlock<Node, D>
        {
        }
        /// <summary>
        /// All Cfg graphs created during the building phase, so it contains Cfg for nested programs and nested procedures,
        /// but also for stacked proprams.
        /// </summary>
        public Dictionary<ProgramSymbol, ControlFlowGraph<Node, D>> AllCfg
        {
            get;
            internal set;
        }

        public ControlFlowGraphBuilder()
        {
        }

        /// <summary>
        /// Called when a Node has been completly parsed.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="program"></param>
        public override void OnNode(Node node, Program program)
        {
        }

        /// <summary>
        /// Called when a node is entered
        /// </summary>
        /// <param name="node">The entered node.</param>
        public override void Enter(Node node)
        {
        }

        public override void Exit(Node node)
        {
        }

        /// <summary>
        /// Start of a Compilation Unit
        /// </summary>
        public override void StartCobolCompilationUnit()
        {
        }

        public override void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
        }

        public override void EndCobolProgram(TypeCobol.Compiler.CodeElements.ProgramEnd end)
        {
        }

        /// <summary>
        /// Start a Function Declaration
        /// </summary>
        /// <param name="header"></param>
        public override void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {
        }

        public override void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
        }

        /// <summary>
        /// Starting a PROCEDURE DIVISION => Collect all parameters.
        /// </summary>
        /// <param name="header"></param>
        public override void StartProcedureDivision(ProcedureDivisionHeader header)
        {
        }

        public override void EndProcedureDivision()
        {
        }

        /// <summary>
        /// Start a Paragraph
        /// </summary>
        /// <param name="header">The Paragraph header code element</param>
        public override void StartParagraph([NotNull] ParagraphHeader header)
        {
        }

        /// <summary>
        /// End a paragraph
        /// </summary>
        public override void EndParagraph()
        {
        }

        /// <summary>
        /// Start a sentence
        /// </summary>
        public override void StartSentence()
        {
        }

        /// <summary>
        /// End a sentence
        /// </summary>
        public override void EndSentence(SentenceEnd end, bool bCheck = false)
        {
        }
    }
}
