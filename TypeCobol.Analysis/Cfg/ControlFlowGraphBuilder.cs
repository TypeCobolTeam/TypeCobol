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

            /// <summary>
            /// Set whether full instruction must be generated are not.
            /// If not only the instruction name will be generated.
            /// </summary>
            public bool FullInstruction
            {
                get;
                set;
            }

            protected override string InstructionToString(Node instruction)
            {
                return (instruction == null || instruction.CodeElement == null) ? "<null>" :
                    FullInstruction ? instruction.CodeElement.SourceText :
                    System.Enum.GetName(typeof(CodeElementType), instruction.CodeElement.Type);
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

            public int GroupIndex
            {
                get;
                internal set;
            }

            /// <summary>
            /// Terminal blocks within this Group.
            /// </summary>
            internal List<BasicBlockForNode> TerminalBlocks
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
        /// CFG Modes.
        /// Normal: is the normal mode, Paragraph blocs target by a PERFORM are not expanded.
        /// Extended: Paragraph blocs target by a PERFORM are expanded.
        /// </summary>
        public enum CfgMode
        {
            Normal,
            Extended,
        }

        /// <summary>
        /// Set the CFG Mode
        /// </summary>
        public CfgMode Mode
        {
            get;
            set;
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
        internal CfgSentence CurrentSentence
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
        /// More Symbol Reference associated to altered GOTO statement.
        /// </summary>
        protected Dictionary<Goto, HashSet<SymbolReference>> PendingAlteredGOTOS;

        /// <summary>
        /// All pending Next Sentence instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        internal LinkedList<Tuple<NextSentence, BasicBlockForNode, CfgSentence>> PendingNextSentences;

        /// <summary>
        /// All encoutered sentences
        /// </summary>
        internal List<CfgSentence> AllSentences;

        public IList<Diagnostic> Diagnostics { get; private set; }
        /// <summary>
        /// Empty constructor.
        /// </summary>
        public ControlFlowGraphBuilder() : this(null)
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
            this.UseEvaluateCascade = true;
            this.UseSearchCascade = true;
            Mode = CfgMode.Normal;
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
            switch (ce.Type)
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
                switch (node.CodeElement.Type)
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
                        this.CurrentProgramCfgBuilder.EnterDeclaratives((Declaratives)node);
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
                        this.CurrentProgramCfgBuilder.EnterAlter((Alter)node);
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
                        this.CurrentProgramCfgBuilder.LeaveDeclaratives((Declaratives)node);
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
                        this.CurrentProgramCfgBuilder.LeaveAlter((Alter)node);
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
        private void LinkBlockSentenceToCurrentSectionParagraph(CfgSentence block)
        {
            Symbol curSecOrPara = ((Symbol)this.CurrentProgramCfgBuilder.CurrentParagraph) ?? this.CurrentProgramCfgBuilder.CurrentSection;
            if (curSecOrPara != null)
            {
                if (curSecOrPara.Kind == Symbol.Kinds.Section)
                {
                    this.CurrentProgramCfgBuilder.CurrentSection.AddSentence(block);
                }
                else
                {
                    this.CurrentProgramCfgBuilder.CurrentParagraph.AddSentence(block);
                }
                //Give to this block the name of its paragraph as tag.
                block.Block.Tag = curSecOrPara.Name;
            }
        }

        /// <summary>
        /// A Sentence used for our builder. A Sentence is a special symbol.
        /// </summary>
        internal class CfgSentence : Symbol
        {
            /// <summary>
            /// Private symbol owner.
            /// </summary>
            internal void SetOwner(Symbol owner)
            {
                Owner = owner;
            }
            /// <summary>
            /// First Block associated to this sentence.
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
            public CfgSentence()
            {
                BlockIndex = -1;
                Kind = Kinds.Sentence;
                Name = "<<Sentence>>(" + (SentenceCounter++) + ")";
            }

            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The flag to be set</param>
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
            this.CurrentProgramCfgBuilder.CurrentSentence = new CfgSentence();
            if (this.CurrentProgramCfgBuilder.AllSentences == null)
            {
                this.CurrentProgramCfgBuilder.AllSentences = new List<CfgSentence>();
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
        /// Propagate properties to the given ControlFrowGraphBuilder
        /// </summary>
        /// <param name="currentProgramCfgBuilder">The CFG Builder to propagate properties to</param>
        private void PropagateProperties(ControlFlowGraphBuilder<D> currentProgramCfgBuilder)
        {
            currentProgramCfgBuilder.Mode = Mode;
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
                    if (AllCfgBuilder == null)
                        AllCfgBuilder = new List<ControlFlowGraphBuilder<D>>();
                    this.AllCfgBuilder.Add(this);
                    this.CurrentProgramCfgBuilder = this;
                }
                else
                {//Stacked Program.         
                    //New Control Flow Graph
                    this.CurrentProgramCfgBuilder = CreateFreshControlFlowGraphBuilder();
                    PropagateProperties(this.CurrentProgramCfgBuilder);
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
                PropagateProperties(this.CurrentProgramCfgBuilder);
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
            PropagateProperties(nestedCfg);
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
            //Special case Section or Paragraph inside a Declarative
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
        /// <param name="symRef">The Symbol Reference instance to a section or a paragraph.</param>
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
            /// All sentences and paragraphs in this section in the order of appearance.
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
            public void AddSentence(CfgSentence s)
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
            //Reset any current sentence
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
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
            public Scope<CfgSentence> Sentences
            {
                get;
                protected set;
            }

            /// <summary>
            /// Enters a sentence symbol in this Paragraph.
            /// </summary>
            /// <param name="p">The paragraph to enter</param>
            public void AddSentence(CfgSentence s)
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
                Sentences = new Scope<CfgSentence>(this);
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
                    CfgSentence sentence = next.Item3;
                    if (sentence.Number < this.CurrentProgramCfgBuilder.AllSentences.Count - 1)
                    {
                        CfgSentence nextSentence = AllSentences[sentence.Number + 1];
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
                                HashSet<SymbolReference> alteredSymbolRefs = null;
                                //Check if we have altered GOTOs to take in account.
                                if (PendingAlteredGOTOS != null && PendingAlteredGOTOS.TryGetValue(@goto, out alteredSymbolRefs))
                                {
                                    int i = 0;
                                    target = new SymbolReference[alteredSymbolRefs.Count + 1];
                                    foreach (SymbolReference sr in alteredSymbolRefs)
                                        target[++i] = sr;
                                }
                                else
                                {
                                    target = new SymbolReference[1];
                                }
                                target[0] = simpleGoto.ProcedureName;
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
                this.CurrentProgramCfgBuilder.PendingAlteredGOTOS = null;
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
        /// <param name="p">The procedure node</param>
        /// <param name="procedureSymbol">The procedure symbol</param>
        /// <param name="group">The Group in which to store all blocks.</param>
        /// <param name="clonedBlockIndexMap">The Map of cloned map indices from the original indices to the new indicess of block</param>
        private void StoreProcedureSentenceBlocks(PerformProcedure p, Symbol procedureSymbol, BasicBlockForNodeGroup group, Dictionary<int, int> clonedBlockIndexMap)
        {
            IEnumerable<CfgSentence> procedureSentences = YieldSectionOrParagraphSentences(procedureSymbol);
            foreach (var sentence in procedureSentences)
            {
                //A Sentence has at least one block
                System.Diagnostics.Debug.Assert(sentence.AllBlocks != null);
                System.Diagnostics.Debug.Assert(sentence.AllBlocks.First.Value == sentence.Block);
                foreach (var block in sentence.AllBlocks)
                {//We must clone each block of the sequence and add them to the group.                    
                    //System.Diagnostics.Debug.Assert(!clonedBlockIndexMap.ContainsKey(block.Index));
                    if (!clonedBlockIndexMap.ContainsKey(block.Index))
                    {//If this block has been already add, this mean there are recursive GOTOs
                        clonedBlockIndexMap[block.Index] = block.Index;
                        group.AddBlock(block);
                    }
                    else
                    {//Recursive blocks detection.
                        block.FullInstruction = true;
                        string strBlock = block.ToString();
                        Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                            p.CodeElement.Column,
                            p.CodeElement.Column,
                            p.CodeElement.Line,
                            string.Format(Resource.RecursiveBlockOnPerformProcedure, procedureSymbol.ToString(), strBlock));
                        Diagnostics.Add(d);
                    }
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
            Dictionary<int, int> clonedBlockIndexMap = new Dictionary<int, int>();
            if (throughProcedure != null)
            {
                Symbol throughProcedureSymbol = CheckSectionOrParagraph(p, throughProcedure);
                if (throughProcedureSymbol == null)
                    return false;
                if (throughProcedureSymbol != procedureSymbol)
                {
                    if (procedureSymbol.Number > throughProcedureSymbol.Number)
                    {// the second procedure name is before the first one.
                        Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                            p.CodeElement.Column,
                            p.CodeElement.Column,
                            p.CodeElement.Line,
                            string.Format(Resource.BadPerformProcedureThru, procedure.ToString(), throughProcedure.ToString()));
                        Diagnostics.Add(d);
                        return false;
                    }
                    StoreProcedureSentenceBlocks(p, procedureSymbol, group, clonedBlockIndexMap);
                    //Store all sentences or paragraphs between.
                    for (int i = procedureSymbol.Number + 1; i < throughProcedureSymbol.Number; i++)
                    {
                        Symbol subSectionOrParagraph = this.CurrentProgramCfgBuilder.AllSectionsParagraphs[i];
                        StoreProcedureSentenceBlocks(p, subSectionOrParagraph, group, clonedBlockIndexMap);
                    }
                    StoreProcedureSentenceBlocks(p, throughProcedureSymbol, group, clonedBlockIndexMap);
                }
                else
                {
                    StoreProcedureSentenceBlocks(p, procedureSymbol, group, clonedBlockIndexMap);
                }
            }
            else
            {
                StoreProcedureSentenceBlocks(p, procedureSymbol, group, clonedBlockIndexMap);
            }
            //Now Clone the Graph.
            if (!RelocateBasicBlockForNodeGroupGraph(p, group, clonedBlockIndexMap))
            {
                return false;
            }
            return true;
        }

        /// <summary>
        /// Relocate the graph generated by a BasicBlockForNodeGroup
        /// </summary>
        /// <param name="p">The Perform Procedure node source of the call</param>
        /// <param name="group">The Group to be relocated</param>
        /// <param name="clonedBlockIndexMap">The map between the old block indices to their new block indices.</param>
        /// <returns>true if the relocation is successfull, false if the relocation goes beyond the group limit, 
        /// this often means that de target paragraphs of the PERFORM goes out of the paragraph.</returns>
        private bool RelocateBasicBlockForNodeGroupGraph(PerformProcedure p, BasicBlockForNodeGroup group, Dictionary<int, int> clonedBlockIndexMap)
        {
            //New successor edge map.
            Dictionary<int, int> clonedEdgeIndexMap = new Dictionary<int, int>();
            foreach (var b in group.Group)
            {
                List<int> successors = b.SuccessorEdges;
                b.SuccessorEdges = new List<int>();
                foreach (var edge in successors)
                {
                    int newEdge = -1;
                    if (!clonedEdgeIndexMap.TryGetValue(edge, out newEdge))
                    {
                        var block = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges[edge];
                        int cloneBlockIndex = 0;
                        if (!clonedBlockIndexMap.TryGetValue(block.Index, out cloneBlockIndex))
                        {
                            if (b != group.Group.Last.Value)
                            {//Hum this Group is not the last group and it goes beyond the group limit ==> we don't support that.
                                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                                    p.CodeElement.Column,
                                    p.CodeElement.Column,
                                    p.CodeElement.Line,
                                    string.Format(Resource.BasicBlockGroupGoesBeyondTheLimit, ((BasicBlockForNode)block).Tag != null ? ((BasicBlockForNode)block).Tag.ToString() : "???", block.Index));
                                Diagnostics.Add(d);
                                //So in this case in order to not break the graph and to see the target branch that went out, add it as well....
                                b.SuccessorEdges.Add(edge);
                                continue;
                            }
                            else
                            {//Don't add the continuation edge
                                continue;
                            }
                        }
                        var clonedBlock = this.CurrentProgramCfgBuilder.Cfg.AllBlocks[cloneBlockIndex];
                        newEdge = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                        this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(clonedBlock);
                        clonedEdgeIndexMap[edge] = newEdge;
                    }
                    b.SuccessorEdges.Add(newEdge);
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
                if (Mode == CfgMode.Extended)
                {
                    foreach (var item in this.CurrentProgramCfgBuilder.PendingPERFORMProcedures)
                    {
                        PerformProcedure p = item.Item1;
                        BasicBlockForNodeGroup group = item.Item2;
                        GraftBasicBlockGroup(group);
                    }
                }
            }
        }

        /// <summary>
        /// Compute the terminal blocks associated to Group
        /// 
        /// </summary>
        /// <param name="group">The group to compute the terminal blocks</param>
        private void ComputeBasicBlockGroupTerminalBlocks(BasicBlockForNodeGroup group)
        {
            if (group.Group.Count > 0)
            {
                LinkedListNode<BasicBlock<Node, D>> first = group.Group.First;
                MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, null);
                List<BasicBlockForNode> terminals = new List<BasicBlockForNode>();
                ctx.GetTerminalSuccessorEdges((BasicBlockForNode)first.Value, terminals);
                group.TerminalBlocks = terminals;
            }
        }

        /// <summary>
        /// Extend BasicBlockForNodeGroup instance to point to the first block and make all terminal blocks
        /// have for successor the block successor.
        /// </summary>
        /// <param name="group">The group to be continuted</param>
        private void ContinueBasicBlockGroup(BasicBlockForNodeGroup group)
        {
            if (group.Group.Count > 0)
            {
                System.Diagnostics.Debug.Assert(group.SuccessorEdges.Count == 1);
                int succIndex = group.SuccessorEdges[0];
                group.SuccessorEdges.Clear();

                //Make the First block the successor of the group
                LinkedListNode<BasicBlock<Node, D>> first = group.Group.First;
                int firstIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                group.SuccessorEdges.Add(firstIndex);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(first.Value);

                //Make all terminal blocks of the group have the original successor of the block.
                foreach(var termBlock in group.TerminalBlocks)
                {
                    if (!termBlock.SuccessorEdges.Contains(succIndex))
                    {
                        if (!termBlock.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                        {
                            termBlock.SuccessorEdges.Add(succIndex);
                        }
                    }
                }
                group.SetFlag(BasicBlock<Node, D>.Flags.GroupGrafted, true);
            }            
        }

        /// <summary>
        /// Graft the content of a Basic Block group by duplicating all its blocks and connecting all blocks to the CFG continuation.
        /// </summary>
        /// <param name="group">The group to be grafted</param>
        private void GraftBasicBlockGroup(BasicBlockForNodeGroup group)
        {
            if (group.Group.Count == 0)
                return;
            //The new group to build.
            LinkedList<BasicBlock<Node, D>> newGroup = new LinkedList<BasicBlock<Node, D>>();
            //Map of block : (Original Block Index, [new Block Index, Successor Edge Index])
            Dictionary<int, int[]> BlockMap = new Dictionary<int, int[]>();
            //Fill the map
            foreach(var b in group.Group)
            {                
                //Clone a new block.
                BasicBlock<Node, D> newBlock = (BasicBlock<Node, D>)b.Clone();
                if (newBlock is BasicBlockForNodeGroup)
                {//We are Cloning a Group ==> recursion
                    BasicBlockForNodeGroup newBG = (BasicBlockForNodeGroup)newBlock;
                    GraftBasicBlockGroup(newBG);
                }
                newBlock.Index = this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Count;
                this.CurrentProgramCfgBuilder.Cfg.AllBlocks.Add(newBlock);
                BlockMap[b.Index] = new int[2] { newBlock.Index, -1 };
                newGroup.AddLast(newBlock);
            }
            //Handle successors
            foreach (var b in group.Group)
            {
                int[] desc = BlockMap[b.Index];
                BasicBlock<Node, D> newBlock = this.CurrentProgramCfgBuilder.Cfg.AllBlocks[desc[0]];
                newBlock.SuccessorEdges = new List<int>(b.SuccessorEdges.Count);//new Block will have new successors
                foreach(var s in b.SuccessorEdges)
                {
                    BasicBlock<Node, D> succBlock = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges[s];
                    int[] succDesc = null;
                    if (!BlockMap.TryGetValue(succBlock.Index, out succDesc))
                    {//Successor block is not in our scope ==> just add it
                        newBlock.SuccessorEdges.Add(s);
                    }
                    else
                    {
                        if (succDesc[1] == -1)
                        {//Create a successor entry
                            succDesc[1] = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                            BasicBlock<Node, D> newSuccBlock = this.CurrentProgramCfgBuilder.Cfg.AllBlocks[succDesc[0]];
                            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(newSuccBlock);
                        }
                        newBlock.SuccessorEdges.Add(succDesc[1]);
                    }
                }
            }
            group.Group = newGroup;
            ComputeBasicBlockGroupTerminalBlocks(group);
            ContinueBasicBlockGroup(group);
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
            //First resolve ALTERS before resolving Pending GOTOs
            ResolvePendingALTERs();
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
        /// <paramref name="symbol"/>The Symbol which resolved to the target
        /// <returns>The Enumeration of sentences associated to the target, null otherwise</returns>
        private IEnumerable<CfgSentence> ResolveSectionOrParagraphSentences(Node node, SymbolReference target, out Symbol symbol)
        {
            symbol = CheckSectionOrParagraph(node, target);
            return YieldSectionOrParagraphSentences(symbol);
        }

        /// <summary>
        /// Yield the all sentences associated to a Symbol which is a Section or a Paragraph.
        /// </summary>
        /// <param name="sectionOrParagraphSymbol">The Section or Paragraph symbol</param>
        /// <returns>The Enumeration of sentences associated to the symbol, null otherwise</returns>
        private IEnumerable<CfgSentence> YieldSectionOrParagraphSentences(Symbol sectionOrParagraphSymbol)
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
                            CfgSentence sentence = (CfgSentence)part;
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
            HashSet<Symbol> targetSymbols = new HashSet<Symbol>();
            foreach (var sref in target)
            {
                bool bHasOne = false;
                Symbol targetSymbol = null;
                IEnumerable<CfgSentence> sentences = ResolveSectionOrParagraphSentences(@goto, sref, out targetSymbol);
                if (targetSymbol == null)
                {
                    Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                        @goto.CodeElement.Column,
                        @goto.CodeElement.Column,
                        @goto.CodeElement.Line,
                        string.Format(Resource.UnknownSectionOrParagraph, sref.ToString()));
                    Diagnostics.Add(d);
                    continue;
                }
                if (!targetSymbols.Contains(targetSymbol))
                {
                    targetSymbols.Add(targetSymbol);
                    foreach (var targetSentence in sentences)
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
            /// Current Block before multi branches
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
            /// Root Block of the multi branches
            /// </summary>
            internal BasicBlockForNode RootBlock;
            /// <summary>
            /// The Index in the SuccessorEdge of the RootBlock if any, -1 otherwise.
            /// </summary>
            internal int RootBlockSuccessorIndex;

            /// <summary>
            /// The instruction associated to this context.
            /// </summary>
            internal Node Instruction;

            /// <summary>
            /// Any contextual Data.
            /// </summary>
            internal object ContextualData;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
            /// <param name="instruction">The instruction associated to this context </param>
            internal MultiBranchContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder, Node instruction)
            {
                Branches = new List<BasicBlockForNode>();
                BranchIndices = new List<int>();
                Builder = currentProgramCfgBuilder;
                Instruction = instruction;
                RootBlockSuccessorIndex = -1;
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
                End(branchToNext, CurrentBlock, nextBlock);
            }
            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="rootBlock">Root block of the multi branch</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(bool branchToNext, BasicBlockForNode rootBlock, BasicBlockForNode nextBlock)            
            {
                System.Diagnostics.Debug.Assert(Builder != null);
                System.Diagnostics.Debug.Assert(rootBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);
                //Add the next block to the successors.
                int nbIndex = Builder.Cfg.SuccessorEdges.Count;
                Builder.Cfg.SuccessorEdges.Add(nextBlock);
                //Link current block to all branches.
                foreach (var b in Branches)
                {
                    //Add branch to the successors
                    BranchIndices.Add(Builder.Cfg.SuccessorEdges.Count);
                    rootBlock.SuccessorEdges.Add(Builder.Cfg.SuccessorEdges.Count);
                    Builder.Cfg.SuccessorEdges.Add(b);
                    //Next Block is a successor of the branch.
                    AddTerminalSuccessorEdge(b, nbIndex);
                }
                if (branchToNext)
                {
                    rootBlock.SuccessorEdges.Add(nbIndex);
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
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void AddTerminalSuccessorEdge(BasicBlockForNode b, int nbIndex, HashSet<int> visitedBlockIndex = null)
            {
                if (visitedBlockIndex == null)
                {
                    visitedBlockIndex = new HashSet<int>();
                }
                if (visitedBlockIndex.Contains(b.Index))
                    return;
                visitedBlockIndex.Add(b.Index);

                if (b.SuccessorEdges.Count == 0)
                {
                    //Ending block has no successors.
                    if (!b.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                    {
                        if (b != Builder.Cfg.SuccessorEdges[nbIndex])
                        {//Don't create recurssion to ourselves
                            b.SuccessorEdges.Add(nbIndex);
                        }
                    }
                }
                else
                {
                    foreach (var s in b.SuccessorEdges)
                    {
                        AddTerminalSuccessorEdge((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], nbIndex, visitedBlockIndex);
                    }
                }
            }

            /// <summary>
            /// Get all teeminal blocks from the given block.
            /// </summary>
            /// <param name="b">The starting block</param>
            /// <param name="accumulator">Accumulator of  terminal blocks</param>
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void GetTerminalSuccessorEdges(BasicBlockForNode b, List<BasicBlockForNode> accumulator, HashSet<int> visitedBlockIndex = null)
            {
                if (visitedBlockIndex == null)
                {
                    visitedBlockIndex = new HashSet<int>();
                }
                if (visitedBlockIndex.Contains(b.Index))
                    return;
                visitedBlockIndex.Add(b.Index);
                if (b.SuccessorEdges.Count == 0)
                {
                    accumulator.Add(b);
                }
                else foreach (var s in b.SuccessorEdges)
                {
                    GetTerminalSuccessorEdges((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], accumulator, visitedBlockIndex);
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
                foreach (var section in Sections)
                {
                    var sentences = Builder.YieldSectionOrParagraphSentences(section);
                    foreach (var sentence in sentences)
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
                                bFirstsection = false;
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
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, _if);
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the if context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //Add the if instruction in the current block.
            AddCurrentBlockNode(_if);
            //So the current block is now the If            
            var ifBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
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
        /// Set wheither or not the EVALUATE statement shall be translated using cascading IF-THEN-ELSE, false
        /// otherwise.
        /// </summary>
        public bool UseEvaluateCascade
        {
            get;
            set;
        }

        /// <summary>
        /// Set wheither or not the SEARCH statement shall be translated using cascading IF-THEN-ELSE, false
        /// otherwise.
        /// </summary>
        public bool UseSearchCascade
        {
            get;
            set;
        }

        /// <summary>
        /// Enter an Evaluate statement
        /// </summary>
        /// <param name="evaluate">The Evaluate node</param>
        protected virtual void EnterEvaluate(Evaluate evaluate)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, evaluate);
            //Create a liste of node of contextual When and WhenOther nodes.
            ctx.ContextualData = new List<Node>();
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the Evaluate context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //Add the evaluate instruction to the current block
            AddCurrentBlockNode(evaluate);
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

            if (UseEvaluateCascade)
            {   //Pop each MultiBranchContextStack instance till to the EVALUATE one
                //and close each one.
                while (ctx.Instruction == null)
                {
                    System.Diagnostics.Debug.Assert(ctx.Branches.Count > 0);

                    bool branchToNext = ctx.Branches.Count == 1;//No Else
                                                                //The next block.
                    var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                    ctx.End(branchToNext, nextBlock);
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;

                    ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
                }
            }
            else
            {
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
            if (UseEvaluateCascade)
            {
                StartWhenConditionClauseCascade(conditions);
            }
            else
            {
                System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
                System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
                MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
                System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
                System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

                var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                //Associate all When Conditions to the block.
                List<Node> data = (List<Node>)ctx.ContextualData;
                foreach (var node in data)
                {
                    whenCondBlock.Instructions.AddLast(node);
                    this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = whenCondBlock;
                }

                ctx.AddBranch(whenCondBlock);
                //The new Current Block is the When condition block
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
                //Clear the current data
                data.Clear();
            }
        }

        /// <summary>
        /// Here is when we can capture the beginning of a set of WhenConditionClause so we can start a new Basic Block. 
        /// But were it is the cascading version.
        /// </summary>
        /// <param name="conditions"></param>
        public void StartWhenConditionClauseCascade(List<TypeCobol.Compiler.CodeElements.CodeElement> conditions)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            if (!(ctx.Instruction != null && ctx.Instruction.CodeElement.Type == CodeElementType.EvaluateStatement))
            {  //Create the else alternatives
                EnterElse(null);
            }

            //Create Whens context
            MultiBranchContext ctxWhens = new MultiBranchContext(this.CurrentProgramCfgBuilder, null);
            ctxWhens.ContextualData = new List<Node>();
            //Push and start the Whens context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctxWhens);
            ctxWhens.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);

            //Associate all When Conditions to the block.
            List<Node> data = (List<Node>)ctx.ContextualData;
            foreach (var node in data)
            {
                AddCurrentBlockNode(node);
            }

            //So the current block is now the whenBlock            
            var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            ctxWhens.AddBranch(whenCondBlock);
            //The new Current Block is the When condition block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
            //Clear the current data
            data.Clear();
        }

        public override void EndWhenConditionClause()
        {

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
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, node);
            //Create a liste of node of contextual When or AtEnd nodes.
            ctx.ContextualData = new List<Node>();
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            if (UseSearchCascade)
            {
                var searchBlock = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
                if (this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.Count > 0)
                {//Create a Search Block if the previous one is not empty.               
                    searchBlock = this.CurrentProgramCfgBuilder.CreateBlock(node, true);
                    ctx.RootBlockSuccessorIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(ctx.RootBlockSuccessorIndex);
                    this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(searchBlock);
                }
                else
                {
                    AddCurrentBlockNode(node);
                }
                ctx.RootBlock = searchBlock;

                //Create a new empty block for other instructions
                var bodyBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                searchBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(bodyBlock);
                
                //Push and start the Search context.
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
                ctx.Start(bodyBlock);
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = bodyBlock;
            }
            else
            {
                //Push and start the Search context.
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
                ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
                //Add the search instruction to the current block
                AddCurrentBlockNode(node);
            }
        }

        /// <summary>
        /// Handle a When Search Condition for a Search instruction.
        /// </summary>
        /// <param name="condition">The condition, if null then this means the AT END condition</param>
        public override void StartWhenSearchConditionClause(TypeCobol.Compiler.CodeElements.WhenSearchCondition condition)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ContextualData != null);
            System.Diagnostics.Debug.Assert(ctx.ContextualData is List<Node>);

            if (UseSearchCascade)
            {
                if (condition == null)
                {
                    var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                    //This is like a default condition.
                    whenCondBlock.SetFlag(BasicBlock<Node, D>.Flags.Default, true);
                    //Associate all When SearchConditions to the block.
                    List<Node> data = (List<Node>)ctx.ContextualData;
                    foreach (var node in data)
                    {
                        whenCondBlock.Instructions.AddLast(node);
                        this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = whenCondBlock;
                    }
                    ctx.AddBranch(whenCondBlock);
                    //The new Current Block is the When condition block
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
                    //Clear the current data
                    data.Clear();
                }
                else
                {
                    if (ctx.Instruction == null || ctx.Instruction.CodeElement.Type != CodeElementType.SearchStatement)
                    {  //Create the else alternatives
                        EnterElse(null);
                    }
                    else if (ctx.Branches.Count == 1)
                    {//We had an At END Condition ==> So the Current basick block is the Search Block
                        this.CurrentProgramCfgBuilder.CurrentBasicBlock = ctx.CurrentBlock;
                    }
                    //Create Whens context
                    MultiBranchContext ctxWhens = new MultiBranchContext(this.CurrentProgramCfgBuilder, null);
                    ctxWhens.ContextualData = new List<Node>();
                    ctxWhens.RootBlock = ctx.RootBlock;
                    ctxWhens.RootBlockSuccessorIndex = ctx.RootBlockSuccessorIndex;
                    //Push and start the Whens context.
                    this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctxWhens);
                    ctxWhens.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);

                    //Associate all When Conditions to the block.
                    List<Node> data = (List<Node>)ctx.ContextualData;
                    foreach (var node in data)
                    {
                        AddCurrentBlockNode(node);
                    }

                    //So the current block is now the whenBlock            
                    var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                    ctxWhens.AddBranch(whenCondBlock);
                    //The new Current Block is the When condition block
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
                    //Clear the current data
                    data.Clear();
                }
            }
            else
            {
                var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                if (condition == null)
                {//This is like a default condition.
                    whenCondBlock.SetFlag(BasicBlock<Node, D>.Flags.Default, true);
                }
                //Associate all When SearchConditions to the block.
                List<Node> data = (List<Node>)ctx.ContextualData;
                foreach (var node in data)
                {
                    whenCondBlock.Instructions.AddLast(node);
                    this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = whenCondBlock;
                }
                ctx.AddBranch(whenCondBlock);
                //The new Current Block is the When condition block
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = whenCondBlock;
                //Clear the current data
                data.Clear();
            }
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
            if (UseSearchCascade)
            {
                //Pop each MultiBranchContextStack instance till to the SEARCH one
                //and close each one.
                bool bLastBranch = true;
                int rootNodeIndex = ctx.RootBlockSuccessorIndex;
                while (ctx.Instruction == null)
                {
                    System.Diagnostics.Debug.Assert(ctx.Branches.Count > 0);

                    bool branchToNext = ctx.Branches.Count == 1;//No Else
                                                                //The next block.
                    var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                    if (bLastBranch)
                    {//This is the last branch of the cascade, next block is the SearchBlock, thus the root.
                        bLastBranch = false;
                        ctx.End(false, nextBlock);
                        //Branch this terminal block to the search block
                        if (rootNodeIndex == -1)
                        {
                            rootNodeIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(ctx.RootBlock);
                        }
                        ctx.CurrentBlock.SuccessorEdges.Add(rootNodeIndex);
                    }
                    else
                    {
                        ctx.End(branchToNext, nextBlock);                                               
                    }
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
                    ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
                }
                //If we have and AT Condition haandle it
                ctx.End(ctx.Branches.Count == 0, ctx.RootBlock, this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            }
            else
            {
                bool branchToNext = true;
                if (ctx.Branches.Count > 0)
                {//If there is an AT END Condition
                    branchToNext = !ctx.Branches[0].HasFlag(BasicBlock<Node, D>.Flags.Default);
                }
                //The next block.
                var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                ctx.End(branchToNext, nextBlock);
                this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
            }
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
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, perform);
            if (this.CurrentProgramCfgBuilder.MultiBranchContextStack == null)
            {
                this.CurrentProgramCfgBuilder.MultiBranchContextStack = new Stack<MultiBranchContext>();
            }
            //Push and start the Perform context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctx);
            ctx.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            //Creare a Perfom standalone instruction block.
            var performBlock = this.CurrentProgramCfgBuilder.CreateBlock(perform, true);
            ctx.AddBranch(performBlock);
            //Add a branch for the Loop Body
            var bodyBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            ctx.AddBranch(bodyBlock);

            int bodyBlockIndex = -1;
            int performBlockIndex = -1;
            if (IsAfter(perform))
            {
                bodyBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(bodyBlock);
            }
            else
            {
                performBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(performBlock);
            }
            if (bodyBlockIndex == -1)
            {
                bodyBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(bodyBlock);
            }
            else
            {
                performBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(performBlock);
            }
            performBlock.SuccessorEdges.Add(bodyBlockIndex);
            ctx.BranchIndices.Add(performBlockIndex);
            ctx.BranchIndices.Add(bodyBlockIndex);

            //The new Current Block is the body block
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = bodyBlock;
        }

        /// <summary>
        /// Determine if a Perform is iterative or not.
        /// </summary>
        /// <param name="perform">The Perform instruction to be checked</param>
        /// <returns>true if the Perform is iterative, false otherwise</returns>
        private static bool IsNonIterative(Perform perform)
        {
            var element = perform.CodeElement;
            return element.IterationType == null || element.IterationType.Value == PerformIterationType.None;
        }

        /// <summary>
        /// Determine if a Perform loop can be skipped that is to say may not be entered.
        /// </summary>
        /// <param name="perform">The perform loop to check</param>
        /// <returns>True if the perform can be skipped, false otherwise</returns>
        private static bool CanBeSkipped(Perform perform)
        {
            var element = perform.CodeElement;
            if (IsNonIterative(perform))
                return false;//Non Iterative Perform canot be skipped.

            if (perform.CodeElement.TerminationConditionTestTime != null && perform.CodeElement.TerminationConditionTestTime.Value == TerminationConditionTestTime.AfterIteration)
                return false;//ATER  cannot be skipped.
            return true;
        }

        /// <summary>
        /// Test if the a perform loop is an AFTER
        /// </summary>
        /// <param name="perform"></param>
        /// <returns>true if the PERFORM loop is an AFTER, fals eotherwise</returns>
        private static bool IsAfter(Perform perform)
        {
            return perform.CodeElement.TerminationConditionTestTime != null && perform.CodeElement.TerminationConditionTestTime.Value == TerminationConditionTestTime.AfterIteration;
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
            System.Diagnostics.Debug.Assert(ctx.Branches.Count == 2);
            System.Diagnostics.Debug.Assert(ctx.BranchIndices.Count == 2);

            //Firt Get here all terminal blocks of the loop body
            List<BasicBlockForNode> terminals = new List<BasicBlockForNode>();
            ctx.GetTerminalSuccessorEdges(ctx.Branches[1], terminals);

            int performBlockIndex = ctx.BranchIndices[0];
            System.Diagnostics.Debug.Assert(performBlockIndex >= 0);
            int bodyBlockIndex = ctx.BranchIndices[1];
            System.Diagnostics.Debug.Assert(bodyBlockIndex >= 0);

            //The next block, add it as a successor
            var nextBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            int nextBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
            this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(nextBlock);


            int transBlockIndex = -1;
            if (!IsNonIterative(perform))
            {   //For an Iterative perform, body transition is the perform instruction
                //the nextblock is a transition for the perform. 
                ctx.Branches[0].SuccessorEdges.Add(nextBlockIndex);
                transBlockIndex = performBlockIndex;
            }
            else
            {//For a non iterative perform body transition is the next block
                transBlockIndex = nextBlockIndex;
            }
            foreach (var term in terminals)
            {
                if (!term.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                {
                    term.SuccessorEdges.Add(transBlockIndex);
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
                    this.CurrentProgramCfgBuilder.PendingNextSentences = new LinkedList<Tuple<NextSentence, BasicBlockForNode, CfgSentence>>();
                }
                //Track pending Next Sentences.
                Tuple<NextSentence, BasicBlockForNode, CfgSentence> item = new Tuple<NextSentence, BasicBlockForNode, CfgSentence>(
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
        /// Resolve all pending ALTERs
        /// </summary>
        private void ResolvePendingALTERs()
        {
            if (this.CurrentProgramCfgBuilder.PendingALTERs != null)
            {
                foreach (var item in this.CurrentProgramCfgBuilder.PendingALTERs)
                {
                    Alter alter = item.Item1;
                    AlterGotoInstruction[] gotos = alter.CodeElement.AlterGotoInstructions;
                    foreach (AlterGotoInstruction alterGoto in gotos)
                    {
                        SymbolReference alterProc = alterGoto.AlteredProcedure;
                        SymbolReference targetProc = alterGoto.NewTargetProcedure;
                        //So lookup the paragraph
                        Symbol alterProcSymbol = CheckSectionOrParagraph(alter, alterProc);
                        if (alterProcSymbol == null)
                            continue;

                        //So also Resolve the target.
                        Symbol targetProcSymbol = CheckSectionOrParagraph(alter, targetProc);
                        if (targetProcSymbol == null)
                            continue;

                        Symbol resolveAlterProcSymbol = null;
                        IEnumerable<CfgSentence> sectionOrPara = this.CurrentProgramCfgBuilder.ResolveSectionOrParagraphSentences(alter, alterProc, out resolveAlterProcSymbol);
                        System.Diagnostics.Debug.Assert(resolveAlterProcSymbol == alterProcSymbol);
                        //So Look for the first Goto Instruction
                        foreach (var sb in sectionOrPara)
                        {
                            bool bResolved = false;
                            if (sb.Block != null && sb.Block.Instructions != null && sb.Block.Instructions.Count > 0)
                            {//The first instruction must be a GOTO Instruction.
                                Node first = sb.Block.Instructions.First.Value;
                                if (first.CodeElement != null && first.CodeElement.Type == CodeElementType.GotoStatement)
                                {//StatementType.GotoSimpleStatement
                                    Goto @goto = (Goto)first;
                                    if (@goto.CodeElement.StatementType == StatementType.GotoSimpleStatement)
                                    {
                                        if (this.CurrentProgramCfgBuilder.PendingAlteredGOTOS == null)
                                            this.CurrentProgramCfgBuilder.PendingAlteredGOTOS = new Dictionary<Goto, HashSet<SymbolReference>>();

                                        HashSet<SymbolReference> targetSymbols = null;
                                        if (!this.CurrentProgramCfgBuilder.PendingAlteredGOTOS.TryGetValue(@goto, out targetSymbols))
                                        {
                                            targetSymbols = new HashSet<SymbolReference>();
                                            this.CurrentProgramCfgBuilder.PendingAlteredGOTOS[@goto] = targetSymbols;
                                        }
                                        targetSymbols.Add(targetProc);
                                        bResolved = true;
                                    }
                                }
                            }
                            if (!bResolved)
                            {
                                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                                    alter.CodeElement.Column,
                                    alter.CodeElement.Column,
                                    alter.CodeElement.Line,
                                    Resource.BadAlterIntrWithNoSiblingGotoInstr);
                                Diagnostics.Add(d);
                            }
                        }
                    }
                }
                this.CurrentProgramCfgBuilder.PendingALTERs = null;
            }
        }

        /// <summary>
        /// Enter and ALTER Statement
        /// </summary>
        /// <param name="node">The ALTER node</param>
        protected virtual void EnterAlter(Alter node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.PendingALTERs == null)
            {
                this.CurrentProgramCfgBuilder.PendingALTERs = new LinkedList<Tuple<Alter, BasicBlockForNode>>();
            }
            Tuple<Alter, BasicBlockForNode> item = new Tuple<Alter, BasicBlockForNode>(node, this.CurrentProgramCfgBuilder.CurrentBasicBlock);
            this.CurrentProgramCfgBuilder.PendingALTERs.AddLast(item);

            this.CurrentProgramCfgBuilder.CurrentBasicBlock.Instructions.AddLast(node);
            this.CurrentProgramCfgBuilder.Cfg.BlockFor[node] = this.CurrentProgramCfgBuilder.CurrentBasicBlock;
        }

        /// <summary>
        /// Enter and LEAVE Statement
        /// </summary>
        /// <param name="node">The ALTER node</param>
        protected virtual void LeaveAlter(Alter node)
        {

        }

        /// <summary>
        /// Enter an Exception condition
        /// </summary>
        /// <param name="node">The exception condition to be entered</param>
        protected virtual void EnterExceptionCondition(Node node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            //Special case AT END in a SEARCH Instruction
            if (node.CodeElement.Type == CodeElementType.AtEndCondition &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack != null &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0 &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek().Instruction.CodeElement.Type == CodeElementType.SearchStatement)
            {//So in this case just think that it is a null condition
                MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
                List<Node> data = (List<Node>)ctx.ContextualData;
                data.Add(node);
                //Call StartWhenSearchContionClause with null, this will mean AT END condition.
                StartWhenSearchConditionClause(null);
            }
            else
            {
                MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, node);
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
        }

        /// <summary>
        /// Leave an Exception condition
        /// </summary>
        /// <param name="node">The exception condition to be leave</param>
        protected virtual void LeaveExceptionCondition(Node node)
        {
            //Special case AT END in a SEARCH Instruction
            if (node.CodeElement.Type == CodeElementType.AtEndCondition &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack != null &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0 &&
                this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek().Instruction.CodeElement.Type == CodeElementType.SearchStatement)
            {//Nothing todo.                
            }
            else
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
            //Indicate the the Cfg will have subgraphs.
            this.CurrentProgramCfgBuilder.Cfg.SetFlag(ControlFlowGraph<Node, D>.Flags.Compound, true);

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
        /// Group Index Counter
        /// </summary>
        private int GroupCounter = 0;
        /// <summary>
        /// Create a Group Basic Block for a node
        /// </summary>
        /// <param name="node">The leading node of the block</param>
        /// <param name="addToCurrentSentence">true if the block must be added to the current Sentence, false otherwise.</param>
        /// <returns>The new Block</returns>
        internal BasicBlockForNodeGroup CreateGroupBlock(Node node, bool addToCurrentSentence)
        {
            var block = new BasicBlockForNodeGroup();
            block.GroupIndex = ++GroupCounter;
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
        protected virtual ControlFlowGraphBuilder<D> CreateFreshControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null)
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

        public static readonly string ROOT_SECTION_NAME = "<< RootSection >>";

        /// <summary>
        /// Start the Cfg construction for a ProcedureDivision node
        /// </summary>
        /// <param name="procDiv">The Procedure Division</param>
        protected virtual void StartCfg(ProcedureDivision procDiv)
        {
            System.Diagnostics.Debug.Assert(Cfg != null);
            Cfg.ProcedureNode = procDiv;
            Cfg.Initialize();
            //Create a Root Section
            CfgSectionSymbol sym = new CfgSectionSymbol(ROOT_SECTION_NAME);
            EnterSectionOrParagraphSymbol(sym);
            //The new current section.
            CurrentSection = sym;
            //Create a starting Section
            StartBlockSentence();
            //Make the starting block of the Root section a root block.            
            Cfg.BlockFor[procDiv] = CurrentBasicBlock;
            Cfg.RootBlocks.Add(CurrentBasicBlock);
            CurrentBasicBlock.SetFlag(BasicBlock<Node, D>.Flags.Start, true);
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
