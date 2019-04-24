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
using TypeCobol.Compiler.Scopes;
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
        /// The parent rogram Control Flow Builder, for nested Program..
        /// </summary>
        public ControlFlowGraphBuilder<D> ParentProgramCfgBuilder
        {
            get;
            private set;
        }


        /// <summary>
        /// All Cfg graphs builder created during the building phase, so it contains Cfg for nested programs and nested procedures,
        /// but also for stacked proprams.
        /// </summary>
        public List<ControlFlowGraphBuilder<D>> AllCfgBuilder
        {
            get;
            internal set;
        }

        /// <summary>
        /// The current Program Cfg being built.
        /// </summary>
        public ControlFlowGraphBuilder<D> CurrentProgramCfgBuilder
        {
            get;
            private set;
        }

        /// <summary>
        /// The Control Flow Graph Build for the main Program
        /// </summary>
        public ControlFlowGraph<Node, D> Cfg
        {
            get;
            private set;
        }

        /// <summary>
        /// The Current Program symbol being built as a Scope
        /// </summary>
        public Program Program
        {
            get;
            private set;
        }

        /// <summary>
        /// The Current Program symbol being built as a Scope
        /// </summary>
        private Program CurrentProgram
        {
            get;
            set;
        }

        /// <summary>
        /// The current entered node.
        /// </summary>
        private Node CurrentNode
        {
            get;
            set;
        }

        /// <summary>
        /// The Last exited node.
        /// </summary>
        private Node LastExitedNode
        {
            get;
            set;
        }

        /// <summary>
        /// The Basic Block List Stack during Graph Construction.
        ///  Used for IF-THEN-ELSE or EVALUATE
        /// </summary>
        protected Stack<List<BasicBlockForNode>> BasicBlockListStack
        {
            get;
            set;
        }

        /// <summary>
        /// The current basic block
        /// </summary>
        protected BasicBlockForNode CurrentBasicBlock
        {
            get;
            set;
        }

        /// <summary>
        /// Section and Paragraph Symbols.
        /// </summary>
        protected Scope<Symbol> SectionsParagraphs
        {
            get;
            set;
        }

        /// <summary>
        /// Dicionary of Basic Blocks associated to Section and Paragraphs.
        /// </summary>
        protected Dictionary<Symbol, BasicBlockForNode> SectionParagraphBlocks
        {
            get;
            set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public ControlFlowGraphBuilder()
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parentCfgBuilder">Parent Control Flow Builder for a nested program</param>
        public ControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null)
        {
            this.ParentProgramCfgBuilder = parentCfgBuilder;
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
            if (node.CodeElement != null)
            {
                switch(node.CodeElement.Type)
                {
                    case CodeElementType.ProgramIdentification:
                        EnterProgram((Program)node);
                        break;
                    case CodeElementType.FunctionDeclarationHeader:
                        EnterFunction((FunctionDeclaration)node);
                        break;
                    case CodeElementType.ProcedureDivisionHeader:
                        EnterProcedureDivision((ProcedureDivision)node);
                        break;
                    case CodeElementType.SectionHeader:
                        break;
                    case CodeElementType.ParagraphHeader:
                        break;
                    //Decision
                    case CodeElementType.IfStatement:
                        break;
                    case CodeElementType.ElseCondition:
                        break;
                }
            }
        }

        /// <summary>
        /// Called when a node is exited.
        /// </summary>
        /// <param name="node"></param>
        public override void Exit(Node node)
        {
            if (node.CodeElement != null)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElementType.ProgramIdentification:
                        LeaveProgram((Program)node);
                        break;
                    case CodeElementType.FunctionDeclarationHeader:
                        LeaveFunction((FunctionDeclaration)node);
                        break;
                    case CodeElementType.ProcedureDivisionHeader:
                        LeaveProcedureDivision((ProcedureDivision)node);
                        break;
                    case CodeElementType.SectionHeader:
                        break;
                    case CodeElementType.ParagraphHeader:
                        break;
                    //Decision
                    case CodeElementType.IfStatement:
                        break;
                    case CodeElementType.ElseCondition:
                        break;
                    case CodeElementType.EvaluateStatement:
                        break;
                    //Procedure-Branching
                    case CodeElementType.AlterStatement:
                        break;
                    case CodeElementType.ExitStatement:
                        break;
                    case CodeElementType.GotoStatement:
                        break;
                    case CodeElementType.NextSentenceStatement:
                        break;
                    case CodeElementType.PerformStatement:
                        break;
                    //Ending
                    case CodeElementType.StopStatement:
                        break;
                    case CodeElementType.ExitProgramStatement:
                        break;
                    case CodeElementType.ExitMethodStatement:
                        break;
                    case CodeElementType.GobackStatement:
                        break;
                    //Other statementts
                    case CodeElementType.AcceptStatement:
                    case CodeElementType.AddStatement:
                    //case CodeElementType.AlterStatement:
                    case CodeElementType.CallStatement:
                    case CodeElementType.CancelStatement:
                    case CodeElementType.CloseStatement:
                    case CodeElementType.ComputeStatement:
                    case CodeElementType.ContinueStatement:
                    case CodeElementType.DeleteStatement:
                    case CodeElementType.DisplayStatement:
                    case CodeElementType.DivideStatement:
                    case CodeElementType.EntryStatement:
                    //case CodeElementType.EvaluateStatement:
                    case CodeElementType.ExecStatement:
                    //case CodeElementType.ExitMethodStatement:
                    //case CodeElementType.ExitProgramStatement:
                    //case CodeElementType.ExitStatement:
                    //case CodeElementType.GobackStatement:
                    //case CodeElementType.GotoStatement:
                    //case CodeElementType.IfStatement:
                    case CodeElementType.InitializeStatement:
                    case CodeElementType.InspectStatement:
                    case CodeElementType.InvokeStatement:
                    case CodeElementType.MergeStatement:
                    case CodeElementType.MoveStatement:
                    case CodeElementType.MultiplyStatement:
                    //case CodeElementType.NextSentenceStatement:
                    case CodeElementType.OpenStatement:
                    case CodeElementType.PerformProcedureStatement:
                    //case CodeElementType.PerformStatement:
                    case CodeElementType.ReadStatement:
                    case CodeElementType.ReleaseStatement:
                    case CodeElementType.ReturnStatement:
                    case CodeElementType.RewriteStatement:
                    case CodeElementType.SearchStatement:
                    case CodeElementType.SetStatement:
                    case CodeElementType.SortStatement:
                    case CodeElementType.StartStatement:
                    //case CodeElementType.StopStatement:
                    case CodeElementType.StringStatement:
                    case CodeElementType.SubtractStatement:
                    case CodeElementType.UnstringStatement:
                    case CodeElementType.UseStatement:
                    case CodeElementType.WriteStatement:
                    case CodeElementType.XmlGenerateStatement:
                    case CodeElementType.XmlParseStatement:
                        break;
                    default:
                        break;

                }
            }
        }

        /// <summary>
        /// Start of a Compilation Unit
        /// </summary>
        public override void StartCobolCompilationUnit()
        {
        }

        /// <summary>
        /// Start a Program.
        /// </summary>
        /// <param name="programIdentification"></param>
        /// <param name="libraryCopy"></param>
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

        /// <summary>
        /// Enter a program.
        /// </summary>
        /// <param name="program"></param>
        protected virtual void EnterProgram(Program program)
        {
            if (this.CurrentProgram == null)
            {//This is the main program or a stacked program with no parent.           
                if (CurrentProgramCfgBuilder == null)
                {//The Main program
                    this.CurrentProgramCfgBuilder = this;                    
                }
                else
                {//Stacked Program.         
                    //New Control Flow Graph
                    this.CurrentProgramCfgBuilder = CreateFreshControlFlowGraphBuilder();
                    if (AllCfgBuilder == null)
                        AllCfgBuilder = new List<ControlFlowGraphBuilder<D>>();
                    this.AllCfgBuilder.Add(this.CurrentProgramCfgBuilder);
                }
            }
            else
            {//Nested program.
             //New Control Flow Graph
                if (this.CurrentProgramCfgBuilder.AllCfgBuilder == null)
                {
                    this.CurrentProgramCfgBuilder.AllCfgBuilder = new List<ControlFlowGraphBuilder<D>>();
                }
                var nestedCfg = CreateFreshControlFlowGraphBuilder(this.CurrentProgramCfgBuilder);
                this.CurrentProgramCfgBuilder.AllCfgBuilder.Add(nestedCfg);
                this.CurrentProgramCfgBuilder = nestedCfg;
            }
            this.CurrentProgramCfgBuilder.InitializeCfg(program);
            this.CurrentProgram = program;
        }

        /// <summary>
        /// Leave a program.
        /// </summary>
        /// <param name="program"></param>
        protected virtual void LeaveProgram(Program program)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder != null);
            if (this.CurrentProgramCfgBuilder.ParentProgramCfgBuilder == null)
            {//We are leaving the main program or the stacked program.
                this.CurrentProgram = null;
            }
            else
            {//Nested program get the parent control Flow Builder.
                this.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder.ParentProgramCfgBuilder;
                this.CurrentProgram = this.CurrentProgramCfgBuilder.CurrentProgram;
            }            
        }

        /// <summary>
        /// Enter a function declaration
        /// </summary>
        /// <param name="funDecl">Function declaration entered</param>
        protected virtual void EnterFunction(FunctionDeclaration funDecl)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder != null);
            if (this.CurrentProgramCfgBuilder.AllCfgBuilder == null)
            {
                this.CurrentProgramCfgBuilder.AllCfgBuilder = new List<ControlFlowGraphBuilder<D>>();
            }
            var nestedCfg = CreateFreshControlFlowGraphBuilder(this.CurrentProgramCfgBuilder);
            this.CurrentProgramCfgBuilder.AllCfgBuilder.Add(nestedCfg);
            this.CurrentProgramCfgBuilder = nestedCfg;
            this.CurrentProgramCfgBuilder.InitializeCfg(funDecl);
        }

        /// <summary>
        /// Leave a function declaration
        /// </summary>
        /// <param name="funDecl">Function declaration left</param>
        protected virtual void LeaveFunction(FunctionDeclaration funDecl)
        {
            this.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder.ParentProgramCfgBuilder;
        }


        /// <summary>
        /// Entering a PROCEDURE DIVISION here real things begin.
        /// </summary>
        /// <param name="procDiv">The Entered Procedure division</param>
        protected virtual void EnterProcedureDivision(ProcedureDivision procDiv)
        {

        }

        /// <summary>
        /// Enter a section declaration
        /// </summary>
        /// <param name="section"></param>
        protected virtual void EnterSection(Section section)
        {

        }

        /// <summary>
        /// /Leave a section declaration.
        /// </summary>
        /// <param name="section"></param>
        protected virtual void LeaveSection(Section section)
        {

        }

        /// <summary>
        /// Leaving a PROCEDURE DIVISION
        /// </summary>
        /// <param name="procDiv">The procedure Division</param>
        protected virtual void LeaveProcedureDivision(ProcedureDivision procDiv)
        {

        }

        /// <summary>
        /// Create a Fresh Control Flow Graph Builder.
        /// </summary>
        /// <returns>The fresh Control Flow Graph Builder</returns>
        protected virtual ControlFlowGraphBuilder<D>  CreateFreshControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null)
        {
            return new ControlFlowGraphBuilder<D>();
        }

        /// <summary>
        /// Initialize the Cfg by a Program.
        /// </summary>
        /// <param name="program">The target program of the Cfg</param>
        protected virtual void InitializeCfg(Program program)
        {
            Cfg = new ControlFlowGraph<Node, D>();
            Cfg.ProgramNode = program;
        }

        /// <summary>
        /// Initialize the Cfg by a Function.
        /// </summary>
        /// <param name="funDecl">The target function declaration of the Cfg</param>
        protected virtual void InitializeCfg(FunctionDeclaration funDecl)
        {
            Cfg = new ControlFlowGraph<Node, D>();
            Cfg.ProgramNode = funDecl;
        }

        /// <summary>
        /// Start the Cfg construction for a ProcedureDivision node
        /// </summary>
        /// <param name="procDiv">The Procedure Division</param>
        protected virtual void StartCfg(ProcedureDivision procDiv)
        {
            System.Diagnostics.Debug.Assert(Cfg != null);
            Cfg.ProcedureNode = procDiv;
            Cfg.Initialize();
            //Create the starting block.
            var startBlock = new BasicBlockForNode();
            Cfg.BlockFor[procDiv] = startBlock;
            Cfg.RootBlocks.Add(startBlock);
            CurrentBasicBlock = startBlock;
        }

        /// <summary>
        /// End the Cfg construction for a ProcedureDivision Node
        /// </summary>
        /// <param name="procDiv">The Procedure Division</param>
        protected virtual void EndCfg(ProcedureDivision procDiv)
        {

        }
    }
}
