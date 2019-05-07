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
using TypeCobol.Compiler.Diagnostics;
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
        /// A Basic Block which contains a list of BasicBlocks.
        /// Such Group is used for PERFORM Procedure instruction block,
        /// to group all BasicBlock of the taget Sentences or Paragraphs.
        /// </summary>
        public class BasicBlockForNodeGroup : BasicBlockForNode
        {
            public LinkedList<BasicBlock<Node, D>> Group
            {
                get;
                set;
            }

            /// <summary>
            /// Constructor.
            /// </summary>
            public BasicBlockForNodeGroup()
            {
                Group = new LinkedList<BasicBlock<Node, D>>();
            }

            /// <summary>
            /// Add a block to this Group.
            /// </summary>
            /// <param name="block">The block to be added.</param>
            public void AddBlock(BasicBlockForNode block)
            {
                Group.AddLast(block);
            }
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
        /// The current basic block
        /// </summary>
        protected BasicBlockForNode CurrentBasicBlock
        {
            get;
            set;
        }

        /// <summary>
        /// The current section symbol.
        /// </summary>
        internal SectionSymbol CurrentSection
        {
            get;
            set;
        }

        /// <summary>
        /// The current paragraph symbol.
        /// </summary>
        internal ParagraphSymbol CurrentParagraph
        {
            get;
            set;
        }

        /// <summary>
        /// The current Sentence in the current Paragraph.
        /// </summary>
        internal BuilderSentence CurrentSentence
        {
            get;
            set;
        }

        /// <summary>
        /// The Section and Paragraph Domain of this program.
        /// </summary>
        internal Dictionary<string, Scope<Symbol>.MultiSymbols> SectionsParagraphs
        {
            get;
            set;
        }

        /// <summary>
        /// Ordered list of all Sections an Paragraphs encountered in order.
        /// </summary>
        internal List<Symbol> AllSectionsParagraphs
        {
            get;
            set;
        }

        /// <summary>
        /// Dictionary of Basic Blocks associated to Sections or Paragraphs.
        /// Each block is a sentence.
        /// </summary>
        internal Dictionary<Symbol, LinkedList<BuilderSentence> > SectionParagraphBlocks
        {
            get;
            set;
        }

        /// <summary>
        /// All pending Goto instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        protected LinkedList<Tuple<Goto, BasicBlockForNode>> PendingGOTOs;
        /// <summary>
        /// All encountered PERFORM procedure instructions
        /// </summary>
        protected LinkedList<Tuple<PerformProcedure, BasicBlockForNodeGroup>> PendingPERFORMProcedures;
        /// <summary>
        /// Pending ALTER instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        protected LinkedList<Tuple<Alter, BasicBlockForNode>> PendingALTERs;

        /// <summary>
        /// All pending Next Sentence instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        internal LinkedList<Tuple<NextSentence, BasicBlockForNode, BuilderSentence>> PendingNextSentences;

        /// <summary>
        /// All encoutered sentences
        /// </summary>
        internal List<BuilderSentence> AllSentences;

        public IList<Diagnostic> Diagnostics { get; private set; }
        /// <summary>
        /// Empty constructor.
        /// </summary>
        public ControlFlowGraphBuilder() : this (null)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parentCfgBuilder">Parent Control Flow Builder for a nested program</param>
        public ControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null)
        {
            this.ParentProgramCfgBuilder = parentCfgBuilder;
            this.SectionParagraphBlocks = new Dictionary<Symbol, LinkedList<BuilderSentence>>();
            this.Diagnostics = new List<Diagnostic>();
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
        /// Determines if the given Node is a statement.
        /// </summary>
        /// <param name="node">The node to be checked</param>
        /// <returns>Return true if the node is a statement, false otherwise.</returns>
        public static bool IsStatement(Node node)
        {
            if (node == null)
                return false;
            var ce = node.CodeElement;
            if (ce == null)
                return false;
            switch(ce.Type)
            {
                //Decision
                case CodeElementType.IfStatement:
                case CodeElementType.ElseCondition:
                case CodeElementType.EvaluateStatement:
                //Procedure-Branching
                case CodeElementType.AlterStatement:
                case CodeElementType.ExitStatement:
                case CodeElementType.GotoStatement:
                case CodeElementType.NextSentenceStatement:
                case CodeElementType.PerformStatement:
                //Ending
                case CodeElementType.StopStatement:
                case CodeElementType.ExitProgramStatement:
                case CodeElementType.ExitMethodStatement:
                case CodeElementType.GobackStatement:
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
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Called when a node is entered
        /// </summary>
        /// <param name="node">The entered node.</param>
        public override void Enter(Node node)
        {
            CurrentNode = node;
            if (IsStatement(node))
                this.CurrentProgramCfgBuilder.CheckStartSentence(node);
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
                        this.CurrentProgramCfgBuilder.EnterProcedureDivision((ProcedureDivision)node);
                        break;
                    case CodeElementType.SectionHeader:
                        this.CurrentProgramCfgBuilder.EnterSection((Section)node);
                        break;
                    case CodeElementType.ParagraphHeader:
                        this.CurrentProgramCfgBuilder.EnterParagraph((Paragraph)node);
                        break;
                    //Decision
                    case CodeElementType.IfStatement:
                        this.CurrentProgramCfgBuilder.EnterIf((If)node);
                        break;
                    case CodeElementType.ElseCondition:
                        this.CurrentProgramCfgBuilder.EnterElse((Else)node);
                        break;
                    case CodeElementType.EvaluateStatement:
                        this.CurrentProgramCfgBuilder.EnterEvaluate((Evaluate)node);
                        break;
                    //Procedure-Branching
                    case CodeElementType.AlterStatement:
                        break;
                    case CodeElementType.ExitStatement:
                        this.CurrentProgramCfgBuilder.EnterExit((Exit)node);
                        break;
                    case CodeElementType.GotoStatement:
                        this.CurrentProgramCfgBuilder.EnterGoto((Goto)node);
                        break;
                    case CodeElementType.NextSentenceStatement:
                        this.CurrentProgramCfgBuilder.EnterNextSentence((NextSentence)node);
                        break;
                    case CodeElementType.PerformProcedureStatement:
                        this.CurrentProgramCfgBuilder.EnterPerformProcedure((PerformProcedure)node);
                        break;
                    //Ending
                    case CodeElementType.StopStatement:
                    case CodeElementType.ExitProgramStatement:
                    case CodeElementType.ExitMethodStatement:
                    case CodeElementType.GobackStatement:
                        this.CurrentProgramCfgBuilder.EnterEnding(node);
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
                    //case CodeElementType.PerformProcedureStatement:                    
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
                    case CodeElementType.PerformStatement:
                        this.CurrentProgramCfgBuilder.EnterPerformLoop((Perform)node);
                        break;
                    case CodeElementType.WhenCondition:
                        this.CurrentProgramCfgBuilder.EnterWhen((When)node);
                        break;
                    case CodeElementType.WhenOtherCondition:
                        this.CurrentProgramCfgBuilder.EnterWhenOther((WhenOther)node);
                        break;
                    default:
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
                        this.CurrentProgramCfgBuilder.LeaveProcedureDivision((ProcedureDivision)node);
                        break;
                    case CodeElementType.SectionHeader:
                        this.CurrentProgramCfgBuilder.LeaveSection((Section)node);
                        break;
                    case CodeElementType.ParagraphHeader:
                        this.CurrentProgramCfgBuilder.LeaveParagraph((Paragraph)node);
                        break;
                    //Decision
                    case CodeElementType.IfStatement:
                        this.CurrentProgramCfgBuilder.LeaveIf((If)node);
                        break;
                    case CodeElementType.ElseCondition:
                        this.CurrentProgramCfgBuilder.LeaveElse((Else)node);
                        break;
                    case CodeElementType.EvaluateStatement:
                        this.CurrentProgramCfgBuilder.LeaveEvaluate((Evaluate)node);
                        break;
                    //Procedure-Branching
                    case CodeElementType.AlterStatement:
                        break;
                    case CodeElementType.ExitStatement:
                        this.CurrentProgramCfgBuilder.LeaveExit((Exit)node);
                        break;
                    case CodeElementType.GotoStatement:
                        this.CurrentProgramCfgBuilder.LeaveGoto((Goto)node);
                        break;
                    case CodeElementType.NextSentenceStatement:
                        this.CurrentProgramCfgBuilder.LeaveNextSentence((NextSentence)node);
                        break;
                    case CodeElementType.PerformProcedureStatement:
                        this.CurrentProgramCfgBuilder.LeavePerformProcedure((PerformProcedure)node);
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
                    //case CodeElementType.PerformProcedureStatement:                    
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
                    case CodeElementType.PerformStatement:
                        this.CurrentProgramCfgBuilder.LeavePerformLoop((Perform)node);
                        break;
                    case CodeElementType.WhenCondition:
                        this.CurrentProgramCfgBuilder.LeaveWhen((When)node);
                        break;
                    case CodeElementType.WhenOtherCondition:
                        this.CurrentProgramCfgBuilder.LeaveWhenOther((WhenOther)node);
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
        /// Link this sentence to the current section or paragraph if any.
        /// </summary>
        /// <param name="block">The block to link</param>
        private void LinkBlockToCurrentSectionParagraph(BuilderSentence block)
        {
            Symbol curSecorPara = ((Symbol)this.CurrentProgramCfgBuilder.CurrentParagraph) ?? this.CurrentProgramCfgBuilder.CurrentSection;
            if (curSecorPara != null)
            {
                LinkedList<BuilderSentence> blocks;
                this.CurrentProgramCfgBuilder.SectionParagraphBlocks.TryGetValue(curSecorPara, out blocks);
                if (blocks == null)
                {
                    blocks = new LinkedList<BuilderSentence>();
                    this.CurrentProgramCfgBuilder.SectionParagraphBlocks[curSecorPara] = blocks;
                }
                blocks.AddLast(block);
            }
        }

        /// <summary>
        /// A Sentence used for aour builder
        /// </summary>
        internal class BuilderSentence : Sentence
        {
            /// <summary>
            /// Any index associated to the Sentence
            /// </summary>
            internal int Index
            {
                get;
                set;
            }

            /// <summary>
            /// First Block asociated to this sentence.
            /// </summary>
            internal BasicBlockForNode Block
            {
                get;
                set;
            }

            /// <summary>
            /// The Block Index Associated to the Block.
            /// </summary>
            internal int BlockIndex
            {
                get;
                set;
            }

            /// <summary>
            /// ctor
            /// </summary>
            public BuilderSentence()
            {

            }
        }

        /// <summary>
        /// Starts a new Block Sentence
        /// </summary>
        private void StartBlockSentence()
        {
            this.CurrentProgramCfgBuilder.CurrentSentence = new BuilderSentence();
            if (AllSentences == null)
            {
                AllSentences = new List<BuilderSentence>();
            }
            CurrentSentence.Index = AllSentences.Count;
            AllSentences.Add(CurrentSentence);

            var block = this.CurrentProgramCfgBuilder.CreateBlock(this.CurrentProgramCfgBuilder.CurrentSentence);
            CurrentSentence.Block = block;

            if (this.CurrentProgramCfgBuilder.CurrentBasicBlock != null)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence.BlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(block);
            }
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = block;
            //Link this Block to its paragraph if any.
            this.CurrentProgramCfgBuilder.LinkBlockToCurrentSectionParagraph(CurrentSentence);
        }

        /// <summary>
        /// Start a sentence: In fact in Cobol a basic block is a sentence.
        /// </summary>
        public override void StartSentence()
        {
        }

        /// <summary>
        /// End a sentence
        /// </summary>
        public override void EndSentence(SentenceEnd end, bool bCheck = false)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.CurrentSentence == null)
            {//This is an empty sentence sequence ==> Create an empty Block.
                this.CurrentProgramCfgBuilder.StartBlockSentence();
            }
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Check a start sentence on a node.
        /// </summary>
        /// <param name="node">The node which participate to the sentence</param>
        internal void CheckStartSentence(Node node)
        {
            //If we are not in a sentence start a sentence.
            if (this.CurrentProgramCfgBuilder.CurrentSentence == null)
            {
                StartBlockSentence();                
            }
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
                    this.CurrentProgramCfgBuilder.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder;
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
                this.CurrentProgramCfgBuilder.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder;
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
            this.CurrentProgramCfgBuilder.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder;
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
        /// Enter in the domain a section or a paragraph symbol.
        /// </summary>
        /// <param name="sym">The symbol to enter.</param>
        private void EnterSectionOrParagraphSymbol(Symbol sym)
        {
            System.Diagnostics.Debug.Assert(sym.Kind == Symbol.Kinds.Section || sym.Kind == Symbol.Kinds.Paragraph);
            if (this.CurrentProgramCfgBuilder.SectionsParagraphs == null)
                this.CurrentProgramCfgBuilder.SectionsParagraphs = new Dictionary<string, Scope<Symbol>.MultiSymbols>(StringComparer.OrdinalIgnoreCase);
            string name = sym.Name;
            Scope<Symbol>.MultiSymbols scope = null;
            this.CurrentProgramCfgBuilder.SectionsParagraphs.TryGetValue(name, out scope);
            if (scope == null)
            {
                scope = new Scope<Symbol>.MultiSymbols();
                this.CurrentProgramCfgBuilder.SectionsParagraphs[name] = scope;
            }
            scope.Add(sym);
            if (AllSectionsParagraphs == null)
            {
                AllSectionsParagraphs = new List<Symbol>();
            }
            sym.Number = AllSectionsParagraphs.Count;
            AllSectionsParagraphs.Add(sym);
        }

        /// <summary>
        /// Resolve a section or a paragraph symbol reference
        /// </summary>
        /// <param name="symRef">The Symbol Reference instance to a section or a pargraph.</param>
        /// <returns>The scope of symbols found</returns>
        internal Scope<Symbol>.MultiSymbols ResolveSectionOrParagraphSymbol(SymbolReference symRef)
        {
            System.Diagnostics.Debug.Assert(symRef != null);
            Scope<Symbol>.MultiSymbols results = new Scope<Symbol>.MultiSymbols();

            string[] paths = AbstractScope.SymbolReferenceToPath(symRef);
            string name = paths[0];
            Scope<Symbol>.MultiSymbols candidates = null;
            this.CurrentProgramCfgBuilder.SectionsParagraphs.TryGetValue(name, out candidates);
            if (candidates == null)
                return results;
            foreach (var candidate in candidates)
            {
                if (candidate.IsMatchingPath(paths))
                    results.Add(candidate);
            }
            return results;
        }

        /// <summary>
        /// Enter a section declaration
        /// </summary>
        /// <param name="section"></param>
        protected virtual void EnterSection(Section section)
        {
            string name = section.Name;
            SectionSymbol sym = new SectionSymbol(name);
            this.CurrentProgramCfgBuilder.EnterSectionOrParagraphSymbol(sym);
            //The new current section.
            this.CurrentProgramCfgBuilder.CurrentSection = sym;
        }

        /// <summary>
        /// /Leave a section declaration.
        /// </summary>
        /// <param name="section"></param>
        protected virtual void LeaveSection(Section section)
        {
            this.CurrentProgramCfgBuilder.CurrentSection = null;
            //Current sentence is also null now
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Enter a paragraph
        /// </summary>
        /// <param name="p">The paragraph to be entered</param>
        protected virtual void EnterParagraph(Paragraph p)
        {
            string name = p.Name;
            ParagraphSymbol sym = new ParagraphSymbol(name);
            this.CurrentProgramCfgBuilder.EnterSectionOrParagraphSymbol(sym);
            if (CurrentSection != null)
            {//Add the paragraph to the current section if any.
                this.CurrentProgramCfgBuilder.CurrentSection.AddParagraph(sym);
            }
            this.CurrentProgramCfgBuilder.CurrentParagraph = sym;
            //Current sentence is also null now
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Leave a paragraph
        /// </summary>
        /// <param name="p">The paragraph to be left</param>
        protected virtual void LeaveParagraph(Paragraph p)
        {
            this.CurrentProgramCfgBuilder.CurrentParagraph = null;
            //Current sentence is also null now
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Entering a PROCEDURE DIVISION here real things begin.
        /// </summary>
        /// <param name="procDiv">The Entered Procedure division</param>
        protected virtual void EnterProcedureDivision(ProcedureDivision procDiv)
        {
            //Start Cfg Construction
            this.CurrentProgramCfgBuilder.StartCfg(procDiv);
        }

        /// <summary>
        /// Resolve all pending NEXT SENTENCE instruction.
        /// </summary>
        private void ResolvePendingNextSentences()
        {
            if (this.CurrentProgramCfgBuilder.PendingNextSentences != null)
            {
                foreach (var next in this.CurrentProgramCfgBuilder.PendingNextSentences)
                {
                    BasicBlockForNode block = next.Item2;
                    BuilderSentence sentence = next.Item3;
                    if (sentence.Index < this.CurrentProgramCfgBuilder.AllSentences.Count - 1)
                    {
                        BuilderSentence nextSentence = AllSentences[sentence.Index + 1];
                        System.Diagnostics.Debug.Assert(!block.SuccessorEdges.Contains(nextSentence.BlockIndex));
                        block.SuccessorEdges.Add(nextSentence.BlockIndex);
                    }
                }
                this.CurrentProgramCfgBuilder.PendingNextSentences = null;
                this.CurrentProgramCfgBuilder.AllSentences = null;
            }
        }

        /// <summary>
        /// Resolve all pending GOTOs
        /// </summary>
        private void ResolvePendingGOTOs()
        {
            if (this.CurrentProgramCfgBuilder.PendingGOTOs != null)
            {
                foreach (var item in this.CurrentProgramCfgBuilder.PendingGOTOs)
                {
                    Goto @goto = item.Item1;
                    BasicBlockForNode block = item.Item2;
                    SymbolReference[] target = null;
                    switch (@goto.CodeElement.StatementType)
                    {
                        case StatementType.GotoSimpleStatement:
                            {
                                GotoSimpleStatement simpleGoto = (GotoSimpleStatement)@goto.CodeElement;
                                target = new SymbolReference[] { simpleGoto.ProcedureName };
                                ResolveGoto(@goto, block, target, true);
                            }
                            break;
                        case StatementType.GotoConditionalStatement:
                            {
                                GotoConditionalStatement condGoto = (GotoConditionalStatement)@goto.CodeElement;
                                target = condGoto.ProcedureNames;
                                ResolveGoto(@goto, block, target, false);
                            }
                            break;
                    }
                    System.Diagnostics.Debug.Assert(target != null);
                }
                this.CurrentProgramCfgBuilder.PendingGOTOs = null;
            }
        }

        private Symbol CheckSectionOrParagraph(SymbolReference symRef)
        {
            //Resolve the target Section or Paragraph.
            //Scope<Symbol>.MultiSymbols start = ResolveSectionOrParagraphSymbol(symRef);

            //{
            //    Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
            //        p.CodeElement.Column,
            //        p.CodeElement.Column,
            //        p.CodeElement.Line,
            //        string.Format(Resource.UnknownSectionOrParagraph, symRef.ToString()));
            //    Diagnostics.Add(d);
            //    return null;
            //}
            //if (start.Count > 1)
            //{
            //    Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
            //        p.CodeElement.Column,
            //        p.CodeElement.Column,
            //        p.CodeElement.Line,
            //        string.Format(Resource.AmbiguousSectionOrParagraph, symRef.ToString()));
            //    Diagnostics.Add(d);
            //    return null;
            //}
            return null;
        }

        /// <summary>
        /// Resolve a pending PERFORM procedure
        /// </summary>
        /// <param name="p">The procedure node</param>
        /// <param name="group">The Basic Block Group asociated to the procedure</param>
        /// <returns>True if the PERFORM has been resolved, false otherwise</returns>
        private bool ResolvePendingPERFORMProcedure(PerformProcedure p, BasicBlockForNodeGroup group)
        {
            SymbolReference procedure = p.CodeElement.Procedure;
            SymbolReference throughProcedure = p.CodeElement.ThroughProcedure;

            if (throughProcedure != null)
            {

            }
            return true;
        }

        /// <summary>
        /// Resolve all pending PERFORM procedure.
        /// </summary>
        private void ResolvePendingPERFORMProcedures()
        {
            if (this.CurrentProgramCfgBuilder.PendingPERFORMProcedures != null)
            {
                foreach (var item in this.CurrentProgramCfgBuilder.PendingPERFORMProcedures)
                {
                    PerformProcedure p = item.Item1;
                    BasicBlockForNodeGroup group = item.Item2;
                    ResolvePendingPERFORMProcedure(p, group);
                }
            }
        }

        /// <summary>
        /// Leaving a PROCEDURE DIVISION
        /// </summary>
        /// <param name="procDiv">The procedure Division</param>
        protected virtual void LeaveProcedureDivision(ProcedureDivision procDiv)
        {
            if (this.CurrentProgramCfgBuilder.CurrentSentence != null)
            {
                //Close any alive sentence
                this.CurrentProgramCfgBuilder.EndSentence(null, false);
            }
            //Link pending Next Sentences
            ResolvePendingNextSentences();
            //Resolve and Link Pending GOTOs
            ResolvePendingGOTOs();
            //Resolve Pending PERFORMs Procedure
            ResolvePendingPERFORMProcedures();

            this.CurrentProgramCfgBuilder.EndCfg(procDiv);            
        }

        /// <summary>
        /// Resolve a Goto from a block
        /// </summary>
        /// <param name="target">The target section or paragraph
        /// <returns>The List of sentences associated to the target, null otherwise</returns>
        private LinkedList<BuilderSentence> ResolveSectionOrParagraph(Node node, SymbolReference target)
        {
            var candidates = CurrentProgramCfgBuilder.ResolveSectionOrParagraphSymbol(target);
            if (candidates.Count == 0)
            {//No target
                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                    node.CodeElement.Column,
                    node.CodeElement.Column,
                    node.CodeElement.Line,
                    string.Format(Resource.UnknownSectionOrParagraph, target.Name));
                Diagnostics.Add(d);
                return null;

            }
            else if (candidates.Count > 1)
            {//Multiple References
                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                    node.CodeElement.Column,
                    node.CodeElement.Column,
                    node.CodeElement.Line,
                    string.Format(Resource.AmbiguousSectionOrParagraph, target.Name));
                Diagnostics.Add(d);
                return null;
            }
            Symbol sym = candidates[0];
            LinkedList<BuilderSentence> sentences = null;
            //Get the target 
            CurrentProgramCfgBuilder.SectionParagraphBlocks.TryGetValue(sym, out sentences);
            if (sentences == null  || sentences.Count == 0)
            {
                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                    node.CodeElement.Column,
                    node.CodeElement.Column,
                    node.CodeElement.Line,
                    string.Format(Resource.UnknownSectionOrParagraph, target.Name));
                Diagnostics.Add(d);
                return null;
            }

            return sentences;
        }

        /// <summary>
        /// Resolve a Goto from a block
        /// </summary>
        /// <param name="goto">The target goto</param>
        /// <param name="block">The source block</param>
        /// <param name="target">The target sections or paragraphs
        /// </param>
        /// <param name="v">True for a simple Goto</param>
        /// <returns>true if all targets have been resolved, false otherwise.</returns>
        private bool ResolveGoto(Goto @goto, BasicBlockForNode block, SymbolReference[] target, bool simpleGoto)
        {
            bool bResult = true;
            foreach(var sref in target)
            {
                var sequences = ResolveSectionOrParagraph(@goto, target[0]);
                if (sequences == null)
                {//No target
                    bResult = false;
                }
                else
                {//Multiple References
                    System.Diagnostics.Debug.Assert(sequences.Count > 0);
                    if (sequences.Count > 0)
                    {
                        var targetSentence = sequences.First.Value;
                        if (!block.SuccessorEdges.Contains(targetSentence.BlockIndex))
                        {
                            block.SuccessorEdges.Add(targetSentence.BlockIndex);
                        }
                    }
                }
            }
            return bResult;
        }


        /// <summary>
        /// Enter a Goto Instruction.
        /// </summary>
        /// <param name="node">The Goto instruction node</param>
        protected virtual void EnterGoto(Goto node)
        {
            GotoStatement gotoStmt = node.CodeElement;

            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.PendingGOTOs == null)
            {
                this.CurrentProgramCfgBuilder.PendingGOTOs = new LinkedList<Tuple<Goto, BasicBlockForNode>>();
            }
            Tuple<Goto, BasicBlockForNode> item = new Tuple<Goto, BasicBlockForNode>(node, this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            this.CurrentProgramCfgBuilder.PendingGOTOs.AddLast(item);

            this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.AddLast(node);
            this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
            //Mark the block as being an EndingBlock if it is a simple goto
            if (node.CodeElement.StatementType == StatementType.GotoSimpleStatement)
            {
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SetFlag(BasicBlock<Node, D>.Flags.Ending, true);
            }
            BasicBlockForNode nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            if (node.CodeElement.StatementType == StatementType.GotoConditionalStatement)
            {//For a Conditional Statement the next block can be a continuation block.
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(nextBlock);
            }
            //Create a new current block unreachable.
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Leave a Goto Instruction.
        /// </summary>
        /// <param name="node">The Goto instruction node</param>
        protected virtual void LeaveGoto(Goto node)
        {
        }

        /// <summary>
        /// Enter an ending instruction node
        /// </summary>
        /// <param name="node">The ending node</param>
        protected virtual void EnterEnding(Node node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            System.Diagnostics.Debug.Assert(node.CodeElement != null);
            System.Diagnostics.Debug.Assert(node.CodeElement.Type == CodeElementType.StopStatement ||
                node.CodeElement.Type == CodeElementType.ExitProgramStatement ||
                node.CodeElement.Type == CodeElementType.ExitMethodStatement ||
                node.CodeElement.Type == CodeElementType.GobackStatement
                );
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.AddLast(node);
            this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
            //Mark the block as being an EndingBlock
            this.CurrentProgramCfgBuilder.CurrentBasicBlock.SetFlag(BasicBlock<Node, D>.Flags.Ending, true);

            //Create a new current block unreachable.
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
        }

        /// <summary>
        /// The context of a Multi Branch instruction like IF or Evaluate.
        /// </summary>
        internal class MultiBranchContext
        {
            /// <summary>
            /// Current Block befaore multi branches
            /// </summary>
            internal BasicBlockForNode CurrentBlock;
            /// <summary>
            /// List of multi branch blocks
            /// </summary>
            internal List<BasicBlockForNode> Branches;
            /// <summary>
            /// The Target Control Flow Graph Builder
            /// </summary>
            internal ControlFlowGraphBuilder<D> Builder;
            /// <summary>
            /// Indices in the SuccessorEdges of all blocks in Branches
            /// </summary>
            internal List<int> BranchIndices;

            /// <summary>
            /// Any contextual Data.
            /// </summary>
            internal object ContextualData;

            internal MultiBranchContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder)
            {
                Branches = new List<BasicBlockForNode>();
                BranchIndices = new List<int>();
                Builder = currentProgramCfgBuilder;
            }

            /// <summary>
            /// Start multi branching
            /// </summary>
            /// <param name="CurrentBlock"></param>
            internal void Start(BasicBlockForNode currentBlock)
            {
                this.CurrentBlock = currentBlock;
            }

            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(bool branchToNext, BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(Builder != null);
                System.Diagnostics.Debug.Assert(CurrentBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);
                //Add the next bloc to the successors.
                int nbIndex = Builder.Cfg.SuccessorEdges.Count;
                Builder.Cfg.SuccessorEdges.Add(nextBlock);
                //Link current block to all branches.
                foreach (var b in Branches)
                {
                    //Add branch to the successors
                    BranchIndices.Add(Builder.Cfg.SuccessorEdges.Count);
                    CurrentBlock.SuccessorEdges.Add(Builder.Cfg.SuccessorEdges.Count);
                    Builder.Cfg.SuccessorEdges.Add(b);
                    //Next Block is a successor of the branch.
                    AddTerminalSuccessorEdge(b, nbIndex);
                }
                if (branchToNext)
                {
                    CurrentBlock.SuccessorEdges.Add(nbIndex);
                }
            }

            /// <summary>
            /// Add a block as branch
            /// </summary>
            /// <param name="block">The block to add</param>
            internal void AddBranch(BasicBlockForNode block)
            {
                this.Branches.Add(block);
            }

            /// <summary>
            /// Add to all terminal block, from a given block b, a given successor index.
            /// </summary>
            /// <param name="b">The starting block</param>
            /// <param name="nbIndex">The terminal successor index</param>
            internal void AddTerminalSuccessorEdge(BasicBlockForNode b, int nbIndex)
            {
                if (b.SuccessorEdges.Count == 0)
                {
                    //Ending block has no successors.
                    if (!b.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                    {
                        b.SuccessorEdges.Add(nbIndex);
                    }
                }
                else foreach (var s in b.SuccessorEdges)
                    {
                        AddTerminalSuccessorEdge((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], nbIndex);
                    }
            }

            /// <summary>
            /// Get all teeminal blocks from the given block.
            /// </summary>
            /// <param name="b">The starting block</param>
            /// <param name="accumulator">Accumulator of  terminal blocks</param>
            internal void GetTerminalSuccessorEdges(BasicBlockForNode b, List<BasicBlockForNode> accumulator)
            {
                if (b.SuccessorEdges.Count == 0)
                {
                    accumulator.Add(b);
                }
                else foreach (var s in b.SuccessorEdges)
                    {
                        GetTerminalSuccessorEdges((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], accumulator);
                    }
            }

        }

        /// <summary>
        /// The Muli Branch Stack during Graph Construction.
        ///  Used for IF-THEN-ELSE or EVALUATE
        /// </summary>
        internal Stack<MultiBranchContext> MultiBranchContextStack
        {
            get;
            set;
        }

        /// <summary>
        /// Enter a If instruction
        /// </summary>
        /// <param name="_if">The If instruction</param>
        protected virtual void EnterIf(If _if)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder);            
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the if context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the If
            var ifBlock = this.CurrentProgramCfgBuilder.CreateBlock(_if);
            ctx.AddBranch(ifBlock);
            //The new Current Block is the If block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = ifBlock;
        }

        /// <summary>
        /// Leave a If instruction
        /// </summary>
        /// <param name="_if">The If instruction</param>
        protected virtual void LeaveIf(If _if)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
            System.Diagnostics.Debug.Assert(ctx.Branches != null);
            System.Diagnostics.Debug.Assert(ctx.Branches.Count > 0);

            bool branchToNext = ctx.Branches.Count == 1;//No Else
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);             
            ctx.End(branchToNext, nextBlock);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Enter a Else instruction
        /// </summary>
        /// <param name="_else">The Else instruction</param>
        protected virtual void EnterElse(Else _else)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            //So the current block is now the Else
            var elseBlock = this.CurrentProgramCfgBuilder.CreateBlock(_else);
            ctx.AddBranch(elseBlock);
            //The new Current Block is the else block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = elseBlock;
        }

        /// <summary>
        /// Leave a Else instruction
        /// </summary>
        /// <param name="_else">The Else instruction</param>
        protected virtual void LeaveElse(Else _else)
        {

        }

        /// <summary>
        /// Enter an Evaluate statement
        /// </summary>
        /// <param name="evaluate">The Evaluate node</param>
        protected virtual void EnterEvaluate(Evaluate evaluate)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder);
            //Create a liste of node of contextual When and WhenOther nodes.
            ctx.ContextualData = new List<Node>();
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the if context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the evaluate
            var evalBlock = this.CurrentProgramCfgBuilder.CreateBlock(evaluate);
            ctx.AddBranch(evalBlock);
            //The new Current Block is the Evaluate block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = evalBlock;
        }

        /// <summary>
        /// Leave an Evaluate statement
        /// </summary>
        /// <param name="evaluate">The Evaluate node</param>
        protected virtual void LeaveEvaluate(Evaluate evaluate)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
            System.Diagnostics.Debug.Assert(ctx.Branches != null);

            bool branchToNext = true;
            if (ctx.Branches.Count > 0)
            {
                branchToNext = !ctx.Branches[ctx.Branches.Count - 1].HasFlag(BasicBlock<Node, D>.Flags.Default);
            }
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            ctx.End(branchToNext, nextBlock);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Enter a When condition node
        /// </summary>
        /// <param name="node">The when condition node</param>
        protected virtual void EnterWhen(When node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
            System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

            List<Node> data = (List<Node>)ctx.ContextualData;
            data.Add(node);

        }

        /// <summary>
        /// Leave a When condition node
        /// </summary>
        /// <param name="node">The when condition node</param>
        protected virtual void LeaveWhen(When node)
        {

        }

        /// <summary>
        /// Enter a WhenOther cndition ode.
        /// </summary>
        /// <param name="node">The WhenOther node</param>
        protected virtual void EnterWhenOther(WhenOther node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
            System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

            List<Node> data = (List<Node>)ctx.ContextualData;
            data.Add(node);
        }

        /// <summary>
        /// Leave a WhenOther cndition ode.
        /// </summary>
        /// <param name="node">The WhenOther node</param>
        protected virtual void LeaveWhenOther(WhenOther node)
        {
        }

        /// <summary>
        /// Here is when we can capture the beginning of a set of WhenConditionClause so we can start a new Basic Block. 
        /// </summary>
        /// <param name="conditions"></param>
        public override void StartWhenConditionClause(List<TypeCobol.Compiler.CodeElements.CodeElement> conditions)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
            System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

            var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            //Associate all When Conditions to the block.
            List<Node> data = (List<Node>)ctx.ContextualData;
            foreach(var node in data)
            {
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = whenCondBlock;
            }

            ctx.AddBranch(whenCondBlock);
            //The new Current Block is the When condition block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
            //Clear the current data
            data.Clear();
        }

        /// <summary>
        /// Here is when we can capture the beginning of a set of WhenOtherClause so we can start a new Basic Block. 
        /// </summary>
        /// <param name="cond"></param>
        public override void StartWhenOtherClause(TypeCobol.Compiler.CodeElements.WhenOtherCondition cond)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
            System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

            var whenOtherCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            whenOtherCondBlock.SetFlag(BasicBlock<Node, D>.Flags.Default, true);
            //Associate WhenOther Condition to the block.
            List<Node> data = (List<Node>)ctx.ContextualData;
            System.Diagnostics.Debug.Assert(data.Count == 1);//Only one WhenOther clause.
            foreach (var node in data)
            {
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = whenOtherCondBlock;
            }

            ctx.AddBranch(whenOtherCondBlock);
            //The new Current Block is the When condition block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenOtherCondBlock;
            //Clear the current data
            data.Clear();
        }

        /// <summary>
        /// Enter a Perform which is a loop.
        /// </summary>
        /// <param name="perform">The perform node</param>
        public virtual void EnterPerformLoop(Perform perform)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder);
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the Perform context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the If
            var performBlock = this.CurrentProgramCfgBuilder.CreateBlock(perform);
            ctx.AddBranch(performBlock);
            //The new Current Block is the perform block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = performBlock;
        }

        /// <summary>
        /// Determine if a Perform loop can be skipped that is to say may not be entered.
        /// </summary>
        /// <param name="perform">The perform loop to check</param>
        /// <returns>True if the perform can be skipped, false otherwise</returns>
        private static bool CanBeSkipped(Perform perform)
        {
            if (perform.CodeElement.TerminationConditionTestTime != null && perform.CodeElement.TerminationConditionTestTime.Value == TerminationConditionTestTime.AfterIteration)
                return false;//ATER  cannot be skipped.
            return true;
        }

        /// <summary>
        /// Leave a Perform which is a loop.
        /// </summary>
        /// <param name="perform">The perform node</param>
        public virtual void LeavePerformLoop(Perform perform)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
            System.Diagnostics.Debug.Assert(ctx.Branches != null);
            System.Diagnostics.Debug.Assert(ctx.Branches.Count == 1);
            System.Diagnostics.Debug.Assert(ctx.BranchIndices.Count == 1);

            //Firt Get here all terminal blocks befor eending the context
            List<BasicBlockForNode> terminals = new List<BasicBlockForNode>();
            ctx.GetTerminalSuccessorEdges(ctx.Branches[0], terminals);
            bool branchToNext = CanBeSkipped(perform);
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            ctx.End(branchToNext, nextBlock);
            // we must loop all terminal blocks if there are not ending blocks
            int loopIndex = ctx.BranchIndices[0];
            foreach (var term in terminals)
            {
                if (!term.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                {
                    term.SuccessorEdges.Add(loopIndex);
                }
            }

            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Enter a Next Sentence node
        /// </summary>
        /// <param name="node">the Neext Sentence node</param>
        protected virtual void EnterNextSentence(NextSentence node)
        {
            //This is an invariant there is always one sentence
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentSentence != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.CurrentSentence != null)
            {//So we must create a new Block
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.AddLast(node);
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
                //Mark the block as being an EndingBlock
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SetFlag(BasicBlock<Node, D>.Flags.Ending, true);

                if (this.CurrentProgramCfgBuilder.PendingNextSentences == null)
                {
                    this.CurrentProgramCfgBuilder.PendingNextSentences = new LinkedList<Tuple<NextSentence, BasicBlockForNode, BuilderSentence>>();
                }
                //Track pending Next Sentences.
                Tuple<NextSentence, BasicBlockForNode, BuilderSentence> item = new Tuple<NextSentence, BasicBlockForNode, BuilderSentence>(
                    node, this.CurrentProgramCfgBuilder.CurrentBasicBlock, this.CurrentProgramCfgBuilder.CurrentSentence
                    );
                this.CurrentProgramCfgBuilder.PendingNextSentences.AddLast(item);

                //Create a new current block unreachable.
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = this.CurrentProgramCfgBuilder.CreateBlock(null);
            }

        }

        /// <summary>
        /// Leave a Next Sentence node
        /// </summary>
        /// <param name="node">the Neext Sentence node</param>
        protected virtual void LeaveNextSentence(NextSentence node)
        {

        }

        /// <summary>
        /// Enter an EXIT Statement
        /// </summary>
        /// <param name="node">The EXIT node</param>
        protected virtual void EnterExit(Exit node)
        {
            AddCurrentBlockNode(node);
        }

        /// <summary>
        /// Leave an EXIT Statement
        /// </summary>
        /// <param name="node">The EXIT node</param>
        protected virtual void LeaveExit(Exit node)
        {

        }

        /// <summary>
        /// Enter a PERFORM procedure statement
        /// </summary>
        /// <param name="node">The PERFORM Procedure node</param>
        protected virtual void EnterPerformProcedure(PerformProcedure node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            //Create a Group Block Node
            BasicBlockForNodeGroup group = CreateGroupBlock(node);

            //Link the current block to the Group
            int edgeIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(group);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(edgeIndex);

            //Create a next Current Block.
            BasicBlockForNode nextBlock = CreateBlock(null);
            int nextIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(nextBlock);
            group.SuccessorEdges.Add(nextIndex);

            //The next block becomes the new one.
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;

            //Add the Group to the Pending PERFORMS to be handled at the end of the PROCEDURE DIVISION.
            if (this.CurrentProgramCfgBuilder.PendingPERFORMProcedures == null)
            {
                this.CurrentProgramCfgBuilder.PendingPERFORMProcedures = new LinkedList<Tuple<PerformProcedure, BasicBlockForNodeGroup>>();
            }
            this.CurrentProgramCfgBuilder.PendingPERFORMProcedures.AddLast(new Tuple<PerformProcedure, BasicBlockForNodeGroup>(node, group));
        }

        /// <summary>
        /// Leave a PERFORM procedure statement
        /// </summary>
        /// <param name="node">The PERFORM Procedure node</param>
        protected virtual void LeavePerformProcedure(PerformProcedure node)
        {

        }

        /// <summary>
        /// Add a Node to the current block.
        /// </summary>
        /// <param name="node">The nod etobe added</param>
        protected virtual void AddCurrentBlockNode(Node node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.CurrentBasicBlock != null)
            {
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.AddLast(node);
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
            }
        }

        /// <summary>
        /// Create a basic block for a node.
        /// </summary>
        /// <param name="node">The leading node of the block</param>
        /// <returns>The new Block</returns>
        internal BasicBlockForNode CreateBlock(Node node)
        {
            var block = new BasicBlockForNode();
            if (node != null)
            {
                Cfg.BlockFor[node] = block;
                block.Instructions.AddLast(node);
            }
            return block;
        }

        /// <summary>
        /// Create a Group Basic Block for a node
        /// </summary>
        /// <param name="node">The leading node of the block</param>
        /// <returns>The new Block</returns>
        internal BasicBlockForNodeGroup CreateGroupBlock(Node node)
        {
            var block = new BasicBlockForNodeGroup();
            if (node != null)
            {
                Cfg.BlockFor[node] = block;
                block.Instructions.AddLast(node);
            }
            return block;
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
            //Create a Root Section
            SectionSymbol sym = new SectionSymbol("<<RootSection>>");
            EnterSectionOrParagraphSymbol(sym);
            //The new current section.
            CurrentSection = sym;        
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
