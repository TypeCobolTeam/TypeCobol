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
            /// <summary>
            /// Any tag associated to this Node.
            /// </summary>
            public object Tag
            {
                get;
                set;
            }
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
        internal CfgSectionSymbol CurrentSection
        {
            get;
            set;
        }

        /// <summary>
        /// The current paragraph symbol.
        /// </summary>
        internal CfgParagraphSymbol CurrentParagraph
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
                    case CodeElementType.DeclarativesHeader:
                        EnterDeclaratives((Declaratives)node);
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
                    //Other statements
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
                        this.CurrentProgramCfgBuilder.EnterStatement(node);
                        break;
                    case CodeElementType.SearchStatement:
                        this.CurrentProgramCfgBuilder.EnterSearch((Search)node);
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
                    case CodeElementType.WhenSearchCondition:
                        this.CurrentProgramCfgBuilder.EnterWhenSearch((WhenSearch)node);
                        break;
                    // Statement conditions
                    case CodeElementType.AtEndCondition:
                    case CodeElementType.NotAtEndCondition:
                    case CodeElementType.AtEndOfPageCondition:
                    case CodeElementType.NotAtEndOfPageCondition:
                    case CodeElementType.OnExceptionCondition:
                    case CodeElementType.NotOnExceptionCondition:
                    case CodeElementType.OnOverflowCondition:
                    case CodeElementType.NotOnOverflowCondition:
                    case CodeElementType.InvalidKeyCondition:
                    case CodeElementType.NotInvalidKeyCondition:
                    case CodeElementType.OnSizeErrorCondition:
                    case CodeElementType.NotOnSizeErrorCondition:
                        this.CurrentProgramCfgBuilder.EnterExceptionCondition(node);
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
                    case CodeElementType.DeclarativesHeader:
                        LeaveDeclaratives((Declaratives)node);
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
                        this.CurrentProgramCfgBuilder.LeaveStatement(node);
                        break;
                    case CodeElementType.SearchStatement:
                        this.CurrentProgramCfgBuilder.LeaveSearch((Search)node);
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
                    case CodeElementType.WhenSearchCondition:
                        this.CurrentProgramCfgBuilder.LeaveWhenSearch((WhenSearch)node);
                        break;
                    // Statement conditions
                    case CodeElementType.AtEndCondition:
                    case CodeElementType.NotAtEndCondition:
                    case CodeElementType.AtEndOfPageCondition:
                    case CodeElementType.NotAtEndOfPageCondition:
                    case CodeElementType.OnExceptionCondition:
                    case CodeElementType.NotOnExceptionCondition:
                    case CodeElementType.OnOverflowCondition:
                    case CodeElementType.NotOnOverflowCondition:
                    case CodeElementType.InvalidKeyCondition:
                    case CodeElementType.NotInvalidKeyCondition:
                    case CodeElementType.OnSizeErrorCondition:
                    case CodeElementType.NotOnSizeErrorCondition:
                        this.CurrentProgramCfgBuilder.LeaveExceptionCondition(node);
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
        private void LinkBlockSentenceToCurrentSectionParagraph(BuilderSentence block)
        {
            Symbol curSecorPara = ((Symbol)this.CurrentProgramCfgBuilder.CurrentParagraph) ?? this.CurrentProgramCfgBuilder.CurrentSection;
            if (curSecorPara != null)
            {
                if (curSecorPara.Kind == Symbol.Kinds.Section)
                {
                    this.CurrentProgramCfgBuilder.CurrentSection.AddSentence(block);
                }
                else
                {
                    this.CurrentProgramCfgBuilder.CurrentParagraph.AddSentence(block);
                }
            }
        }

        /// <summary>
        /// A Sentence used for our builder. A Senetence is a special symbol.
        /// </summary>
        internal class BuilderSentence : Symbol
        {
            /// <summary>
            /// Private symbol owner.
            /// </summary>
            internal void SetOwner(Symbol owner)
            {
                Owner = owner;
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
            /// All blocks in this Sentence.
            /// </summary>
            internal LinkedList<BasicBlockForNode> AllBlocks
            {
                get;
                set;
            }

            /// <summary>
            /// Add a block in this sequence.
            /// </summary>
            /// <param name="block">The block to be added</param>
            internal void AddBlock(BasicBlockForNode block)
            {
                if (AllBlocks == null)
                {
                    AllBlocks = new LinkedList<BasicBlockForNode>();
                }
                AllBlocks.AddLast(block);
            }

            /// <summary>
            /// Sentence counter
            /// </summary>
            private static int SentenceCounter = 0;
            /// <summary>
            /// ctor
            /// </summary>
            public BuilderSentence()
            {
                BlockIndex = -1;
                Kind = Kinds.Sentence;
                Name = "<<Sentence>>(" + (SentenceCounter++) + ")";
            }

            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The falg to be set</param>
            /// <param name="value">The value to set</param>
            internal void SetFlag(Symbol.Flags flag, bool value)
            {
                base.SetFlag(flag, value, false);
            }

        }

        /// <summary>
        /// Starts a new Block Sentence
        /// </summary>
        private void StartBlockSentence()
        {
            this.CurrentProgramCfgBuilder.CurrentSentence = new BuilderSentence();
            if (this.CurrentProgramCfgBuilder.AllSentences == null)
            {
                this.CurrentProgramCfgBuilder.AllSentences = new List<BuilderSentence>();
            }
            this.CurrentProgramCfgBuilder.CurrentSentence.Number = this.CurrentProgramCfgBuilder.AllSentences.Count;
            this.CurrentProgramCfgBuilder.AllSentences.Add(this.CurrentProgramCfgBuilder.CurrentSentence);

            if (this.CurrentProgramCfgBuilder.CurrentDeclarativesContext != null)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence.SetFlag(Symbol.Flags.Declaratives, true);
            }

            var block = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            this.CurrentProgramCfgBuilder.CurrentSentence.Block = block;

            if (this.CurrentProgramCfgBuilder.CurrentBasicBlock != null)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence.BlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(block);
            }
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = block;
            //Link this Sentence to its section or paragraph if any.
            this.CurrentProgramCfgBuilder.LinkBlockSentenceToCurrentSectionParagraph(this.CurrentProgramCfgBuilder.CurrentSentence);
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
            if (this.CurrentProgramCfgBuilder.AllSectionsParagraphs == null)
            {
                this.CurrentProgramCfgBuilder.AllSectionsParagraphs = new List<Symbol>();
            }
            sym.Number = this.CurrentProgramCfgBuilder.AllSectionsParagraphs.Count;
            this.CurrentProgramCfgBuilder.AllSectionsParagraphs.Add(sym);
            //Special case Section or Pargraphe inside a Declarative
            if (this.CurrentProgramCfgBuilder.CurrentDeclarativesContext != null)
            { 
                switch (sym.Kind)
                {
                    case Symbol.Kinds.Paragraph:
                    {
                        CfgParagraphSymbol cfgPara = (CfgParagraphSymbol)sym;
                        cfgPara.SetFlag(Symbol.Flags.Declaratives, true);
                    }
                    break;
                    case Symbol.Kinds.Section:
                    {
                        CfgSectionSymbol cfgSymbol = (CfgSectionSymbol)sym;
                        cfgSymbol.SetFlag(Symbol.Flags.Declaratives, true);
                        this.CurrentProgramCfgBuilder.CurrentDeclarativesContext.AddSection(cfgSymbol);
                    }
                    break;
                }
            }
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
        /// A Section symbol used by Cfg : it contains Sentences and Paragraphs in order of appearance.
        /// </summary>
        internal class CfgSectionSymbol : SectionSymbol
        {
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="name">Section's name</param>
            public CfgSectionSymbol(string name) : base(name)
            {
                SentencesParagraphs = new Scope<Symbol>(this);
            }

            /// <summary>
            /// All sentences and paragraph in this section in the order of appearance.
            /// </summary>
            public Scope<Symbol> SentencesParagraphs
            {
                get;
                protected set;
            }

            /// <summary>
            /// Enters a sentence symbol in this Paragraph.
            /// </summary>
            /// <param name="p">The paragraph to enter</param>
            public void AddSentence(BuilderSentence s)
            {
                s.SetOwner(this);
                SentencesParagraphs.Enter(s);
            }

            /// <summary>
            /// Enters a paragraph symbol in this Sections
            /// </summary>
            /// <param name="p">The paragraph to enter</param>
            public override void AddParagraph(ParagraphSymbol p)
            {
                base.AddParagraph(p);
                SentencesParagraphs.Enter(p);
            }

            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The falg to be set</param>
            /// <param name="value">The value to set</param>
            internal void SetFlag(Symbol.Flags flag, bool value)
            {
                base.SetFlag(flag, value, false);
            }
        }

        /// <summary>
        /// Enter a section declaration
        /// </summary>
        /// <param name="section"></param>
        protected virtual void EnterSection(Section section)
        {
            string name = section.Name;
            CfgSectionSymbol sym = new CfgSectionSymbol(name);
            this.CurrentProgramCfgBuilder.EnterSectionOrParagraphSymbol(sym);
            //The new current section.
            this.CurrentProgramCfgBuilder.CurrentSection = sym;
            //No more Paragraph
            this.CurrentProgramCfgBuilder.CurrentParagraph = null;
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
        /// A Paragraph symbol used by a CFG, it contains Sentence symbols.
        /// </summary>
        internal class CfgParagraphSymbol : ParagraphSymbol
        {
            /// <summary>
            /// All sentences in this paragraph in the order of appearance
            /// </summary>
            public Scope<BuilderSentence> Sentences
            {
                get;
                protected set;
            }

            /// <summary>
            /// Enters a sentence symbol in this Paragraph.
            /// </summary>
            /// <param name="p">The paragraph to enter</param>
            public void AddSentence(BuilderSentence s)
            {
                s.SetOwner(this);
                Sentences.Enter(s);
            }

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="name">Pargarph's name</param>
            public CfgParagraphSymbol(string name) : base(name)
            {
                Sentences = new Scope<BuilderSentence>(this);
            }
            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The falg to be set</param>
            /// <param name="value">The value to set</param>
            internal void SetFlag(Symbol.Flags flag, bool value)
            {
                base.SetFlag(flag, value, false);
            }
        }

        /// <summary>
        /// Enter a paragraph
        /// </summary>
        /// <param name="p">The paragraph to be entered</param>
        protected virtual void EnterParagraph(Paragraph p)
        {
            string name = p.Name;
            CfgParagraphSymbol sym = new CfgParagraphSymbol(name);
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
                    if (sentence.Number < this.CurrentProgramCfgBuilder.AllSentences.Count - 1)
                    {
                        BuilderSentence nextSentence = AllSentences[sentence.Number + 1];
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

        /// <summary>
        /// Check that a Section or a Paragraph is resolvable
        /// </summary>
        /// <param name="node">A reference node for the check, used as position.</param>
        /// <param name="symRef">The Symbol Reference to the Section os Paragraph</param>
        /// <returns>The Symbol of the section or Paragraph if resolved, null otherwise.</returns>
        private Symbol CheckSectionOrParagraph(Node node, SymbolReference symRef)
        {
            //Resolve the target Section or Paragraph.
            Scope<Symbol>.MultiSymbols symbols = ResolveSectionOrParagraphSymbol(symRef);

            if (symbols.Count == 0)
            {
                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                    node.CodeElement.Column,
                    node.CodeElement.Column,
                    node.CodeElement.Line,
                    string.Format(Resource.UnknownSectionOrParagraph, symRef.ToString()));
                Diagnostics.Add(d);
                return null;
            }
            if (symbols.Count > 1)
            {
                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                    node.CodeElement.Column,
                    node.CodeElement.Column,
                    node.CodeElement.Line,
                    string.Format(Resource.AmbiguousSectionOrParagraph, symRef.ToString()));
                Diagnostics.Add(d);
                return null;
            }
            return symbols.Symbol;
        }

        /// <summary>
        /// Store all procedure's sentence blocks in a group.
        /// </summary>
        /// <param name="procedureSymbol">The procedure symbol</param>
        /// <param name="group">The Group in which to store all blocsk.</param>
        private void StoreProcedureSentenceBlocks(Symbol procedureSymbol, BasicBlockForNodeGroup group)
        {
            IEnumerable<BuilderSentence> procedureSentences = YieldSectionOrParagraphSentences(procedureSymbol);
            foreach (var sentence in procedureSentences)
            {
                //A Sentence has at least one block
                System.Diagnostics.Debug.Assert(sentence.AllBlocks != null);
                System.Diagnostics.Debug.Assert(sentence.AllBlocks.First.Value == sentence.Block);
                foreach (var block in sentence.AllBlocks)
                {//We must clone each block of the sequence and add them to the group.
                    var cloneBlock = (BasicBlockForNode)block.Clone();
                    //Give to the cloned a new Index, and added to all block.
                    cloneBlock.Index = this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Count;
                    this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Add(block);
                    group.AddBlock(block);
                }
            }
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

            Symbol procedureSymbol = CheckSectionOrParagraph(p, procedure);
            if (procedureSymbol == null)
                return false;
            StoreProcedureSentenceBlocks(procedureSymbol, group);
            if (throughProcedure != null)
            {
                Symbol throughProcedureSymbol = CheckSectionOrParagraph(p, throughProcedure);
                if (throughProcedureSymbol == null)
                    return false;
                if (throughProcedureSymbol != procedureSymbol)
                {
                    if(procedureSymbol.Number > throughProcedureSymbol.Number)
                    {// the second procedure name is before the first one.
                        Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                            p.CodeElement.Column,
                            p.CodeElement.Column,
                            p.CodeElement.Line,
                            string.Format(Resource.BadPerformProcedureThru, procedure.ToString(), throughProcedure.ToString()));
                        Diagnostics.Add(d);
                        return false;
                    }
                    StoreProcedureSentenceBlocks(throughProcedureSymbol, group);
                }
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
        /// Resolve all sentences targeted by the given symbol reference.
        /// </summary>
        /// <param name="target">The target section or paragraph
        /// <returns>The Enumeration of sentences associated to the target, null otherwise</returns>
        private IEnumerable<BuilderSentence> ResolveSectionOrParagraphSentences(Node node, SymbolReference target)
        {
            var symbol = CheckSectionOrParagraph(node, target);
            return YieldSectionOrParagraphSentences(symbol);
        }

        /// <summary>
        /// Yield the all sentences associated to a Symbol which is a Section or a Paragraph.
        /// </summary>
        /// <param name="sectionOrParagraphSymbol">The Section or Paragraph symbol</param>
        /// <returns>The Enumeration of sentences associated to the symbol, null otherwise</returns>
        private IEnumerable<BuilderSentence> YieldSectionOrParagraphSentences(Symbol sectionOrParagraphSymbol)
        {
            if (sectionOrParagraphSymbol != null)
            {
                if (sectionOrParagraphSymbol.Kind == Symbol.Kinds.Paragraph)
                {
                    CfgParagraphSymbol cfgParaSymbol = (CfgParagraphSymbol)sectionOrParagraphSymbol;
                    foreach (var sentence in cfgParaSymbol.Sentences)
                    {
                        yield return sentence;
                    }
                }
                else
                {//This is a section.  
                    CfgSectionSymbol cfgSection = (CfgSectionSymbol)sectionOrParagraphSymbol;
                    foreach (var part in cfgSection.SentencesParagraphs)
                    {
                        System.Diagnostics.Debug.Assert(part.Kind == Symbol.Kinds.Sentence || part.Kind == Symbol.Kinds.Paragraph);
                        if (part.Kind == Symbol.Kinds.Paragraph)
                        {
                            CfgParagraphSymbol cfgParaSymbol = (CfgParagraphSymbol)part;
                            foreach (var sentence in cfgParaSymbol.Sentences)
                            {
                                yield return sentence;
                            }
                        }
                        else
                        {
                            BuilderSentence sentence = (BuilderSentence)part;
                            yield return sentence;
                        }
                    }
                }
            }
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
            foreach(var sref in target)
            {
                bool bHasOne = false;
                IEnumerable<BuilderSentence> sentences = ResolveSectionOrParagraphSentences(@goto, target[0]);
                foreach(var targetSentence in sentences)
                {
                    if (!block.SuccessorEdges.Contains(targetSentence.BlockIndex))
                    {
                        block.SuccessorEdges.Add(targetSentence.BlockIndex);
                    }
                    bHasOne = true;
                    break;
                }
                if (!bHasOne)
                    return false;
            }
            return true;
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
            BasicBlockForNode nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
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
        /// The Multi Branch Stack during Graph Construction.
        ///  Used for IF-THEN-ELSE or EVALUATE
        /// </summary>
        internal Stack<MultiBranchContext> MultiBranchContextStack
        {
            get;
            set;
        }

        /// <summary>
        /// Declarative context class.
        /// </summary>
        internal class DeclarativesContext
        {
            /// <summary>
            /// Current Block before all declaratives sections and paragraphs.
            /// </summary>
            internal BasicBlockForNode CurrentBlock;

            /// <summary>
            /// All sections inside this Declaratives.
            /// </summary>
            internal LinkedList<CfgSectionSymbol> Sections;

            /// <summary>
            /// The related CFG builder
            /// </summary>
            internal ControlFlowGraphBuilder<D> Builder;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
            internal DeclarativesContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder)
            {                
                Sections = new LinkedList<CfgSectionSymbol>();
                Builder = currentProgramCfgBuilder;
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="currentBlock"></param>
            internal void Start(BasicBlockForNode currentBlock)
            {
                CurrentBlock = currentBlock;
            }
            internal void AddSection(CfgSectionSymbol section)
            {
                Sections.AddLast(section);
            }

            /// <summary>
            /// End the declaratives sections.
            /// Each Section becomes a branch from the current block.
            /// </summary>
            internal void End(BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(Builder != null);
                System.Diagnostics.Debug.Assert(CurrentBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);

                //First Current block is linked to the next block.
                int nbIndex = Builder.Cfg.SuccessorEdges.Count;
                Builder.Cfg.SuccessorEdges.Add(nextBlock);
                CurrentBlock.SuccessorEdges.Add(nbIndex);

                //For each section, link the current block to the first block of the section.
                bool bFirstsection = true;
                foreach(var section in Sections)
                {
                    var sentences = Builder.YieldSectionOrParagraphSentences(section);
                    foreach(var sentence in sentences)
                    {
                        //Ensure that every first block of the section is linked.
                        System.Diagnostics.Debug.Assert(sentence.BlockIndex >= 0);
                        if (sentence.BlockIndex >= 0)
                        {
                            var block = sentence.Block;
                            if (bFirstsection)
                            {//The first block of the first section, should have been already linked to the Current Block.
                                System.Diagnostics.Debug.Assert(CurrentBlock.SuccessorEdges.Contains(sentence.BlockIndex));
                                if (!CurrentBlock.SuccessorEdges.Contains(sentence.BlockIndex))
                                {
                                    CurrentBlock.SuccessorEdges.Add(sentence.BlockIndex);
                                }
                            }
                            else
                            {
                                CurrentBlock.SuccessorEdges.Add(sentence.BlockIndex);
                            }
                        }
                        break;//We only need the first sentence.
                    }
                }
            }
        }

        /// <summary>
        /// The Current Declarative context if any.
        /// </summary>
        internal DeclarativesContext CurrentDeclarativesContext;


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
            var ifBlock = this.CurrentProgramCfgBuilder.CreateBlock(_if, true);
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
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);             
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
            var elseBlock = this.CurrentProgramCfgBuilder.CreateBlock(_else, true);
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
            //Push and start the Evaluate context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the evaluate
            var evalBlock = this.CurrentProgramCfgBuilder.CreateBlock(evaluate, true);
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
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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

            var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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

            var whenOtherCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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
        /// Enter a Search Statement.
        /// </summary>
        /// <param name="node">The Search node</param>
        public virtual void EnterSearch(Search node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder);
            //Create a liste of node of contextual When or AtEnd nodes.
            ctx.ContextualData = new List<Node>();
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the Search context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the Search
            var evalBlock = this.CurrentProgramCfgBuilder.CreateBlock(node, true);
            ctx.AddBranch(evalBlock);
            //The new Current Block is the Search block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = evalBlock;
        }

        /// <summary>
        /// Leave a Search Statement.
        /// </summary>
        /// <param name="node">The Search node</param>
        public virtual void LeaveSearch(Search node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
            System.Diagnostics.Debug.Assert(ctx.Branches != null);

            bool branchToNext = true;
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            ctx.End(branchToNext, nextBlock);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Enter a When Search condition node
        /// </summary>
        /// <param name="node">The when search condition node</param>
        protected virtual void EnterWhenSearch(WhenSearch node)
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
        /// Leave a When Serach condition node
        /// </summary>
        /// <param name="node">The when search condition node</param>
        protected virtual void LeaveWhenSearch(WhenSearch node)
        {

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
            //So the current block is now the Perform
            var performBlock = this.CurrentProgramCfgBuilder.CreateBlock(perform, true);
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

            //Firt Get here all terminal blocks before ending the context
            List<BasicBlockForNode> terminals = new List<BasicBlockForNode>();
            ctx.GetTerminalSuccessorEdges(ctx.Branches[0], terminals);
            bool branchToNext = CanBeSkipped(perform);
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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
        /// <param name="node">the Next Sentence node</param>
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
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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
        /// Enter an Exception condition
        /// </summary>
        /// <param name="node">The exception condition to be entered</param>
        protected virtual void EnterExceptionCondition(Node node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder);
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the Exception condition context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //So the current block is now the the exception condition
            var excCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(node, true);
            ctx.AddBranch(excCondBlock);
            //The new Current Block is the Exception condition block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = excCondBlock;
        }

        /// <summary>
        /// Leave an Exception condition
        /// </summary>
        /// <param name="node">The exception condition to be leave</param>
        protected virtual void LeaveExceptionCondition(Node node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
            System.Diagnostics.Debug.Assert(ctx.Branches != null);
            System.Diagnostics.Debug.Assert(ctx.Branches.Count > 0);

            bool branchToNext = true;
            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            ctx.End(branchToNext, nextBlock);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
        }

        /// <summary>
        /// Enter a PERFORM procedure statement
        /// </summary>
        /// <param name="node">The PERFORM Procedure node</param>
        protected virtual void EnterPerformProcedure(PerformProcedure node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            //Create a Group Block Node
            BasicBlockForNodeGroup group = CreateGroupBlock(node, true);

            //Link the current block to the Group
            int edgeIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(group);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(edgeIndex);

            //Create a next Current Block.
            BasicBlockForNode nextBlock = CreateBlock(null, true);
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
        /// Ennter a Declarative
        /// </summary>
        /// <param name="node">The Declarative node</param>
        protected virtual void EnterDeclaratives(Declaratives node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentDeclarativesContext == null);
            this.CurrentProgramCfgBuilder.CurrentDeclarativesContext = new DeclarativesContext(this.CurrentProgramCfgBuilder);
            this.CurrentProgramCfgBuilder.CurrentDeclarativesContext.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);            
        }

        /// <summary>
        /// Leave a Declarative
        /// </summary>
        /// <param name="node">The Declarative node</param>
        protected virtual void LeaveDeclaratives(Declaratives node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentDeclarativesContext != null);

            //The next block.
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            this.CurrentProgramCfgBuilder.CurrentDeclarativesContext.End(nextBlock);
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
            
            this.CurrentProgramCfgBuilder.CurrentDeclarativesContext = null;
        }

        /// <summary>
        /// Enter any Statement
        /// </summary>
        /// <param name="node">The Statement node to be entered</param>
        protected virtual void EnterStatement(Node node)
        {
            AddCurrentBlockNode(node);
        }

        /// <summary>
        /// Leave any Statement
        /// </summary>
        /// <param name="node">The Statement node to be leaves</param>
        protected virtual void LeaveStatement(Node node)
        {

        }

        /// <summary>
        /// Add a Node to the current block.
        /// </summary>
        /// <param name="node">The node to be added</param>
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
        /// <param name="addToCurrentSentence">true if the block must be added to the current Sentence, false otherwise.</param>
        /// <returns>The new Block</returns>
        internal BasicBlockForNode CreateBlock(Node node, bool addToCurrentSentence)
        {
            var block = new BasicBlockForNode();
            block.Index = this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Count;
            this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Add(block);

            if (node != null)
            {
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = block;
                block.Instructions.AddLast(node);
            }
            if (addToCurrentSentence && this.CurrentProgramCfgBuilder.CurrentSentence != null)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence.AddBlock(block);
            }
            if (CurrentDeclarativesContext != null)
            {//This block is created in the context of a Declaratives.
                block.SetFlag(BasicBlock<Node, D>.Flags.Declaratives, true);
            }
            return block;
        }

        /// <summary>
        /// Create a Group Basic Block for a node
        /// </summary>
        /// <param name="node">The leading node of the block</param>
        /// <param name="addToCurrentSentence">true if the block must be added to the current Sentence, false otherwise.</param>
        /// <returns>The new Block</returns>
        internal BasicBlockForNodeGroup CreateGroupBlock(Node node, bool addToCurrentSentence)
        {
            var block = new BasicBlockForNodeGroup();
            block.Index = this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Count;
            this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Add(block);
            if (node != null)
            {
                this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = block;
                block.Instructions.AddLast(node);
            }
            if (addToCurrentSentence && this.CurrentProgramCfgBuilder.CurrentSentence != null)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence.AddBlock(block);
            }
            if (CurrentDeclarativesContext != null)
            {//This group is created in the context of a Declaratives.
                block.SetFlag(BasicBlock<Node, D>.Flags.Declaratives, true);
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
            var startBlock = CreateBlock(null, false); 
            Cfg.BlockFor[procDiv] = startBlock;
            Cfg.RootBlocks.Add(startBlock);
            startBlock.SetFlag(BasicBlock<Node, D>.Flags.Start, true);
            CurrentBasicBlock = startBlock;
            //Create a Root Section
            CfgSectionSymbol sym = new CfgSectionSymbol("<<RootSection>>");
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
