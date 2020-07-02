using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

using SectionNode = TypeCobol.Compiler.Nodes.Section;

namespace TypeCobol.Analysis.Cfg
{
    /// <summary>
    /// The Control Flow Graph Builder for a TypeCobol Program.
    /// </summary>
    public partial class ControlFlowGraphBuilder<D> : SyntaxDrivenAnalyzerBase
    {
        /// <summary>
        /// Control how the blocks targeted by a PERFORM are handled.
        /// True : blocks are cloned each time a perform uses them, False we keep reference to a block group
        /// without extending it.
        /// </summary>
        public bool ExtendPerformTargets { get; }

        /// <summary>
        /// The parent program Control Flow Builder, for nested Program.
        /// </summary>
        private ControlFlowGraphBuilder<D> ParentProgramCfgBuilder { get; }

        /// <summary>
        /// All graphs built for a source file (CFG for main program and
        /// CFG for stacked programs if any).
        /// </summary>
        private IList<ControlFlowGraph<Node, D>> Graphs { get; }

        /// <summary>
        /// The current Program Cfg being built.
        /// </summary>
        private ControlFlowGraphBuilder<D> CurrentProgramCfgBuilder
        {
            get;
            set;
        }

        /// <summary>
        /// The Control Flow Graph Build for the main Program
        /// </summary>
        private ControlFlowGraph<Node, D> Cfg
        {
            get;
            set;
        }

        /// <summary>
        /// The Current Program node.
        /// </summary>
        private Program CurrentProgram
        {
            get;
            set;
        }

        /// <summary>
        /// The current section node.
        /// </summary>
        private SectionNode CurrentSectionNode
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
        /// The current section.
        /// </summary>
        private Section CurrentSection
        {
            get;
            set;
        }

        /// <summary>
        /// The current paragraph.
        /// </summary>
        private Paragraph CurrentParagraph
        {
            get;
            set;
        }

        /// <summary>
        /// The current Sentence in the current Paragraph.
        /// </summary>
        private Sentence CurrentSentence
        {
            get;
            set;
        }

        /// <summary>
        /// List of all Sections and Paragraphs encountered in order.
        /// </summary>
        private List<Procedure> AllProcedures { get; }

        /// <summary>
        /// All pending Goto instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        protected LinkedList<Tuple<Goto, SectionNode, BasicBlockForNode>> PendingGOTOs;

        /// <summary>
        /// All encountered PERFORM procedure instructions
        /// </summary>
        protected LinkedList<Tuple<PerformProcedure, SectionNode, BasicBlockForNodeGroup>> PendingPERFORMProcedures;

        /// <summary>
        /// Pending ALTER instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        protected LinkedList<Tuple<Alter, SectionNode>> PendingALTERs;

        /// <summary>
        /// More Symbol Reference associated to altered GOTO statement.
        /// </summary>
        protected Dictionary<Goto, HashSet<SymbolReference>> PendingAlteredGOTOS;

        /// <summary>
        /// All pending Next Sentence instructions that will be handled at the end of the Procedure Division.
        /// </summary>
        private LinkedList<Tuple<NextSentence, BasicBlockForNode, Sentence>> PendingNextSentences;

        /// <summary>
        /// All encountered sentences
        /// </summary>
        private List<Sentence> AllSentences;

        /// <summary>
        /// Delegate to add a new Diagnostic to the root collection of diagnostics.
        /// </summary>
        new private Action<Diagnostic> AddDiagnostic;

        /// <summary>
        /// Initial constructor. Allows to configure CFG building.
        /// </summary>
        /// <param name="identifier">String identifier of this analyzer-builder.</param>
        /// <param name="extendPerformTargets">True to extend the blocks targeted by PERFORM statements.</param>
        /// <param name="useEvaluateCascade">True to convert EVALUATE statements into cascaded-IFs.</param>
        /// <param name="useSearchCascade">True to convert SEARCH statements into cascaded-IFs.</param>
        protected ControlFlowGraphBuilder(string identifier, bool extendPerformTargets, bool useEvaluateCascade, bool useSearchCascade)
            : base(identifier)
        {
            this.Graphs = new List<ControlFlowGraph<Node, D>>();
            this.AddDiagnostic = base.AddDiagnostic;
            this.AllProcedures = new List<Procedure>();
            this.ParentProgramCfgBuilder = null;
            this.ExtendPerformTargets = extendPerformTargets;
            this.UseEvaluateCascade = useEvaluateCascade;
            this.UseSearchCascade = useSearchCascade;
        }

        /// <summary>
        /// Secondary constructor, for builders made by a builder itself.
        /// </summary>
        /// <param name="builder">Either the main builder or a parent builder.</param>
        /// <param name="asParent">Supplied builder must be registered as parent of the new one.</param>
        private ControlFlowGraphBuilder(ControlFlowGraphBuilder<D> builder, bool asParent)
            : base(null) // Identifier of a child CFG builder won't be used so it's ok to pass null.
        {
            this.Graphs = builder.Graphs;
            this.AddDiagnostic = builder.AddDiagnostic;
            this.AllProcedures = new List<Procedure>();
            this.ParentProgramCfgBuilder = asParent ? builder : null;
            this.ExtendPerformTargets = builder.ExtendPerformTargets;
            this.UseEvaluateCascade = builder.UseEvaluateCascade;
            this.UseSearchCascade = builder.UseSearchCascade;
        }

        /// <summary>
        /// As an analyzer, the root CFG builder returns all graphs
        /// built for a source file.
        /// </summary>
        /// <returns>A non-null IList of ControlFlowGraph.</returns>
        public override object GetResult() => Graphs;

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
                //Other statements
                case CodeElementType.AcceptStatement:
                case CodeElementType.AddStatement:
                case CodeElementType.AllocateStatement:
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
                case CodeElementType.FreeStatement:
                //case CodeElementType.GobackStatement:
                //case CodeElementType.GotoStatement:
                //case CodeElementType.IfStatement:
                case CodeElementType.InitializeStatement:
                case CodeElementType.InspectStatement:
                case CodeElementType.InvokeStatement:
                case CodeElementType.JsonGenerateStatement:
                case CodeElementType.JsonParseStatement:
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
                case CodeElementType.ProcedureStyleCall:
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
                        this.CurrentProgramCfgBuilder.EnterSection((SectionNode)node);
                        break;
                    case CodeElementType.ParagraphHeader:
                        this.CurrentProgramCfgBuilder.EnterParagraph((Compiler.Nodes.Paragraph)node);
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
                    case CodeElementType.AllocateStatement:
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
                    case CodeElementType.FreeStatement:
                    //case CodeElementType.GobackStatement:
                    //case CodeElementType.GotoStatement:
                    //case CodeElementType.IfStatement:
                    case CodeElementType.InitializeStatement:
                    case CodeElementType.InspectStatement:
                    case CodeElementType.InvokeStatement:
                    case CodeElementType.JsonGenerateStatement:
                    case CodeElementType.JsonParseStatement:
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
                    case CodeElementType.ProcedureStyleCall:
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
                        this.CurrentProgramCfgBuilder.LeaveSection((SectionNode)node);
                        break;
                    case CodeElementType.ParagraphHeader:
                        this.CurrentProgramCfgBuilder.LeaveParagraph((Compiler.Nodes.Paragraph)node);
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
                    //Other statements
                    case CodeElementType.AcceptStatement:
                    case CodeElementType.AddStatement:
                    case CodeElementType.AllocateStatement:
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
                    case CodeElementType.FreeStatement:
                    //case CodeElementType.GobackStatement:
                    //case CodeElementType.GotoStatement:
                    //case CodeElementType.IfStatement:
                    case CodeElementType.InitializeStatement:
                    case CodeElementType.InspectStatement:
                    case CodeElementType.InvokeStatement:
                    case CodeElementType.JsonGenerateStatement:
                    case CodeElementType.JsonParseStatement:
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
                    case CodeElementType.ProcedureStyleCall:
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
                }
            }
        }

        /// <summary>
        /// Link this sentence to the current section or paragraph if any.
        /// </summary>
        /// <param name="sentence">The sentence to link.</param>
        private void LinkBlockSentenceToCurrentSectionParagraph(Sentence sentence)
        {
            var currentProcedure = (Procedure) this.CurrentProgramCfgBuilder.CurrentParagraph ??
                                   this.CurrentProgramCfgBuilder.CurrentSection;

            if (currentProcedure != null)
            {
                currentProcedure.AddSentence(sentence);
                
                //Give to this block the name of its paragraph/section as tag.
                sentence.FirstBlock.Tag = currentProcedure.Name;
            }
        }

        /// <summary>
        /// Starts a new Block Sentence
        /// </summary>
        private void StartBlockSentence()
        {
            if (this.CurrentProgramCfgBuilder.AllSentences == null)
                this.CurrentProgramCfgBuilder.AllSentences = new List<Sentence>();

            int number = this.CurrentProgramCfgBuilder.AllSentences.Count;
            var firstBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, false);
            int? firstBlockIndex = null;
            if (this.CurrentProgramCfgBuilder.CurrentBasicBlock != null)
            {
                firstBlockIndex = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                this.CurrentProgramCfgBuilder.CurrentBasicBlock.SuccessorEdges.Add(firstBlockIndex.Value);
                this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(firstBlock);
            }
            Sentence sentence = new Sentence(number, firstBlock, firstBlockIndex);
            this.CurrentProgramCfgBuilder.AllSentences.Add(sentence);

            this.CurrentProgramCfgBuilder.CurrentSentence = sentence;
            this.CurrentProgramCfgBuilder.CurrentBasicBlock = firstBlock;

            //Link this Sentence to its section or paragraph if any.
            this.CurrentProgramCfgBuilder.LinkBlockSentenceToCurrentSectionParagraph(sentence);
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
            {
                //This is the main program or a stacked program with no parent.           
                if (CurrentProgramCfgBuilder == null)
                {
                    //The Main program
                    this.CurrentProgramCfgBuilder = this;
                }
                else
                {
                    //Stacked Program.         
                    //New Control Flow Graph
                    this.CurrentProgramCfgBuilder = new ControlFlowGraphBuilder<D>(this.CurrentProgramCfgBuilder, false);
                    this.CurrentProgramCfgBuilder.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder;
                }
                this.CurrentProgramCfgBuilder.Cfg = new ControlFlowGraph<Node, D>(program, null);
            }
            else
            {
                //Nested program.
                EnterNestedProgramOrFunction(program);
            }
            
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
                this.Graphs.Add(this.CurrentProgramCfgBuilder.Cfg);
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
            EnterNestedProgramOrFunction(funDecl);
        }

        /// <summary>
        /// Leave a function declaration
        /// </summary>
        /// <param name="funDecl">Function declaration left</param>
        protected virtual void LeaveFunction(FunctionDeclaration funDecl)
        {
            this.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder.ParentProgramCfgBuilder;
        }

        private void EnterNestedProgramOrFunction(Node programOrFunctionNode)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder != null);
            var parentBuilder = this.CurrentProgramCfgBuilder;
            var parentGraph = parentBuilder.Cfg;
            this.CurrentProgramCfgBuilder = new ControlFlowGraphBuilder<D>(parentBuilder, true)
                                            {
                                                Cfg = new ControlFlowGraph<Node, D>(programOrFunctionNode, parentGraph)
                                            };

            this.CurrentProgramCfgBuilder.CurrentProgramCfgBuilder = this.CurrentProgramCfgBuilder;
        }

        /// <summary>
        /// Enter a section declaration
        /// </summary>
        /// <param name="section"></param>
        protected virtual void EnterSection(SectionNode section)
        {
            int number = this.CurrentProgramCfgBuilder.AllProcedures.Count;
            string name = section.Name;
            Section cfgSection = new Section(number, name);

            this.CurrentProgramCfgBuilder.AllProcedures.Add(cfgSection);
            _nodeToProcedure.Add(section, cfgSection);

            //The new current section.
            this.CurrentProgramCfgBuilder.CurrentSection = cfgSection;
            this.CurrentProgramCfgBuilder.CurrentSectionNode = section;
            //No more Paragraph
            this.CurrentProgramCfgBuilder.CurrentParagraph = null;
            //Reset any current sentence
            this.CurrentProgramCfgBuilder.CurrentSentence = null;

            //Add section to current DeclarativesContext if any
            if (this.CurrentProgramCfgBuilder.CurrentDeclarativesContext != null)
            {
                this.CurrentProgramCfgBuilder.CurrentDeclarativesContext.AddSection(cfgSection);
            }
        }

        /// <summary>
        /// /Leave a section declaration.
        /// </summary>
        /// <param name="section"></param>
        protected virtual void LeaveSection(SectionNode section)
        {
            this.CurrentProgramCfgBuilder.CurrentSection = null;
            this.CurrentProgramCfgBuilder.CurrentSectionNode = null;
            //Current sentence is also null now
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Enter a paragraph
        /// </summary>
        /// <param name="paragraph">The paragraph to be entered</param>
        protected virtual void EnterParagraph(Compiler.Nodes.Paragraph paragraph)
        {
            int number = this.CurrentProgramCfgBuilder.AllProcedures.Count;
            string name = paragraph.Name;
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentSection != null);
            Section parentSection = this.CurrentProgramCfgBuilder.CurrentSection;
            Paragraph cfgParagraph = new Paragraph(number, name, parentSection);
            //Note that Paragraph constructor adds the newly created paragraph into its parent section.

            this.CurrentProgramCfgBuilder.AllProcedures.Add(cfgParagraph);
            _nodeToProcedure.Add(paragraph, cfgParagraph);

            //Set current paragraph
            this.CurrentProgramCfgBuilder.CurrentParagraph = cfgParagraph;
            //Current sentence is also null now
            this.CurrentProgramCfgBuilder.CurrentSentence = null;
        }

        /// <summary>
        /// Leave a paragraph
        /// </summary>
        /// <param name="p">The paragraph to be left</param>
        protected virtual void LeaveParagraph(Compiler.Nodes.Paragraph p)
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
                    Sentence sentence = next.Item3;
                    if (sentence.Number < this.CurrentProgramCfgBuilder.AllSentences.Count - 1)
                    {
                        Sentence nextSentence = AllSentences[sentence.Number + 1];
                        System.Diagnostics.Debug.Assert(nextSentence.FirstBlockIndex.HasValue);
                        int blockIndex = nextSentence.FirstBlockIndex.Value;
                        System.Diagnostics.Debug.Assert(!block.SuccessorEdges.Contains(blockIndex));
                        block.SuccessorEdges.Add(blockIndex);
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
                    SectionNode sectionNode = item.Item2;
                    BasicBlockForNode block = item.Item3;
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
                                ResolveGoto(@goto, sectionNode, block, target);
                            }
                            break;
                        case StatementType.GotoConditionalStatement:
                            {
                                GotoConditionalStatement condGoto = (GotoConditionalStatement)@goto.CodeElement;
                                target = condGoto.ProcedureNames;
                                ResolveGoto(@goto, sectionNode, block, target);
                            }
                            break;
                    }
                    System.Diagnostics.Debug.Assert(target != null);
                }
                this.CurrentProgramCfgBuilder.PendingGOTOs = null;
                this.CurrentProgramCfgBuilder.PendingAlteredGOTOS = null;
            }
        }

        #region Section/Paragraph resolution

        private readonly Dictionary<Node, Procedure> _nodeToProcedure = new Dictionary<Node, Procedure>();

        /// <summary>
        /// Resolve a Procedure identified by a SymbolReference.
        /// </summary>
        /// <param name="node">The node using the target procedure.</param>
        /// <param name="sectionNode">The section node in which the caller node appears.</param>
        /// <param name="symRef">The SymbolReference to resolve.</param>
        /// <returns>An instance of Procedure or null if no matching procedure has been found.</returns>
        private Procedure ResolveProcedure(Node node, SectionNode sectionNode, SymbolReference symRef)
        {
            var candidates = node.SymbolTable.GetSectionOrParagraph(symRef, sectionNode);
            var section = GetUnique(candidates.Item1);
            var paragraph = GetUnique(candidates.Item2);

            Node procedureNode = null;
            if (section == null)
            {
                procedureNode = paragraph;
            }
            else if (paragraph == null)
            {
                procedureNode = section;
            }

            if (procedureNode == null) return null;

            System.Diagnostics.Debug.Assert(_nodeToProcedure.ContainsKey(procedureNode));
            return _nodeToProcedure[procedureNode];

            T GetUnique<T>(IList<T> list) where T : Node
            {
                switch (list?.Count)
                {
                    case 1:
                        return list[0];
                    default:
                        return null;
                }
            }
        }

        #endregion

        /// <summary>
        /// Store all procedure's sentence blocks in a group.
        /// </summary>
        /// <param name="p">The procedure node</param>
        /// <param name="procedure">The procedure in CFG</param>
        /// <param name="group">The Group in which to store all blocks.</param>
        /// <param name="storedBlockIndices">Set of indices of blocks already stored</param>
        private void StoreProcedureSentenceBlocks(PerformProcedure p, Procedure procedure, BasicBlockForNodeGroup group, HashSet<int> storedBlockIndices)
        {
            foreach (var sentence in procedure)
            {
                //A Sentence has at least one block
                System.Diagnostics.Debug.Assert(sentence.Blocks != null);
                System.Diagnostics.Debug.Assert(sentence.Blocks.First() == sentence.FirstBlock);
                foreach (var block in sentence.Blocks)
                {//We must clone each block of the sequence and add them to the group.                    
                    //System.Diagnostics.Debug.Assert(!clonedBlockIndexMap.ContainsKey(block.Index));
                    if (!storedBlockIndices.Contains(block.Index))
                    {//If this block has been already add, this mean there are recursive GOTOs
                        storedBlockIndices.Add(block.Index);
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
                            string.Format(Resource.RecursiveBlockOnPerformProcedure, procedure.Name, strBlock));
                        AddDiagnostic(d);
                    }
                }
            }
        }
        /// <summary>
        /// Resolve a pending PERFORM procedure
        /// </summary>
        /// <param name="p">The procedure node</param>
        /// <param name="sectionNode">The section in which the PERFORM node appears</param>
        /// <param name="group">The Basic Block Group associated to the procedure</param>
        /// <returns>True if the PERFORM has been resolved, false otherwise</returns>
        private bool ResolvePendingPERFORMProcedure(PerformProcedure p, SectionNode sectionNode, BasicBlockForNodeGroup group)
        {
            SymbolReference procedureReference = p.CodeElement.Procedure;
            SymbolReference throughProcedureReference = p.CodeElement.ThroughProcedure;

            Procedure procedure = ResolveProcedure(p, sectionNode, procedureReference);
            if (procedure == null)
                return false;
            HashSet<int> storedBlocks = new HashSet<int>();
            if (throughProcedureReference != null)
            {
                Procedure throughProcedure = ResolveProcedure(p, sectionNode, throughProcedureReference);
                if (throughProcedure == null)
                    return false;
                if (throughProcedure != procedure)
                {
                    if (procedure.Number > throughProcedure.Number)
                    {// the second procedure name is before the first one.
                        Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                            p.CodeElement.Column,
                            p.CodeElement.Column,
                            p.CodeElement.Line,
                            string.Format(Resource.BadPerformProcedureThru, procedure.Name, throughProcedure.Name));
                        AddDiagnostic(d);
                        return false;
                    }
                    StoreProcedureSentenceBlocks(p, procedure, group, storedBlocks);
                    //Store all sentences or paragraphs between.
                    for (int i = procedure.Number + 1; i < throughProcedure.Number; i++)
                    {
                        Procedure subSectionOrParagraph = this.CurrentProgramCfgBuilder.AllProcedures[i];
                        StoreProcedureSentenceBlocks(p, subSectionOrParagraph, group, storedBlocks);
                    }
                    StoreProcedureSentenceBlocks(p, throughProcedure, group, storedBlocks);
                }
                else
                {
                    StoreProcedureSentenceBlocks(p, procedure, group, storedBlocks);
                }
            }
            else
            {
                StoreProcedureSentenceBlocks(p, procedure, group, storedBlocks);
            }
            //Now Clone the Graph.
            if (!RelocateBasicBlockForNodeGroupGraph(p, group, storedBlocks))
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
        /// <param name="storedBlockIndices">The set of all block indices contained in the group.</param>
        /// <returns>true if the relocation is successful, false if the relocation goes beyond the group limit, 
        /// this often means that de target paragraphs of the PERFORM goes out of the paragraph.</returns>
        private bool RelocateBasicBlockForNodeGroupGraph(PerformProcedure p, BasicBlockForNodeGroup group, HashSet<int> storedBlockIndices)
        {
            //New successor edge map.
            Dictionary<int, int> newEdgeIndexMap = new Dictionary<int, int>();
            foreach (var b in group.Group)
            {
                List<int> successors = b.SuccessorEdges;
                b.SuccessorEdges = new List<int>();
                foreach (var edge in successors)
                {
                    if (!newEdgeIndexMap.TryGetValue(edge, out int newEdge))
                    {
                        var block = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges[edge];
                        if (!storedBlockIndices.Contains(block.Index))
                        {
                            if (b != group.Group.Last.Value)
                            {//Hum this block is not the last of the group and its successor is outside of the group ==> we don't support that.
                                Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                                    p.CodeElement.Column,
                                    p.CodeElement.Column,
                                    p.CodeElement.Line,
                                    string.Format(Resource.BasicBlockGroupGoesBeyondTheLimit, ((BasicBlockForNode)block).Tag != null ? ((BasicBlockForNode)block).Tag.ToString() : "???", block.Index));
                                AddDiagnostic(d);
                                //So in this case in order to not break the graph and to see the target branch that went out, add it as well....
                                b.SuccessorEdges.Add(edge);
                                continue;
                            }
                            else
                            {//Don't add the continuation edge
                                continue;
                            }
                        }
                        newEdge = this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Count;
                        this.CurrentProgramCfgBuilder.Cfg.SuccessorEdges.Add(block);
                        newEdgeIndexMap[edge] = newEdge;
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
                    SectionNode sectionNode = item.Item2;
                    BasicBlockForNodeGroup group = item.Item3;
                    ResolvePendingPERFORMProcedure(p, sectionNode, group);
                }
                if (ExtendPerformTargets)
                {
                    foreach (var item in this.CurrentProgramCfgBuilder.PendingPERFORMProcedures)
                    {
                        BasicBlockForNodeGroup group = item.Item3;
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
        /// <param name="group">The group to be continued</param>
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
                foreach (var termBlock in group.TerminalBlocks)
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
            foreach (var b in group.Group)
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
                foreach (var s in b.SuccessorEdges)
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
        /// Resolve a Goto from a block
        /// </summary>
        /// <param name="goto">The target goto</param>
        /// <param name="sectionNode">The section in which the goto appears</param>
        /// <param name="block">The source block</param>
        /// <param name="target">The target sections or paragraphs</param>
        private void ResolveGoto(Goto @goto, SectionNode sectionNode, BasicBlockForNode block, SymbolReference[] target)
        {
            HashSet<Procedure> targetProcedures = new HashSet<Procedure>();
            foreach (var sref in target)
            {
                Procedure targetProcedure = ResolveProcedure(@goto, sectionNode, sref);
                if (targetProcedure != null && !targetProcedures.Contains(targetProcedure))
                {
                    targetProcedures.Add(targetProcedure);

                    //Link block to target
                    int? targetBlockIndex = targetProcedure.FirstOrDefault()?.FirstBlockIndex;
                    if (targetBlockIndex.HasValue)
                    {
                        if (!block.SuccessorEdges.Contains(targetBlockIndex.Value))
                        {
                            block.SuccessorEdges.Add(targetBlockIndex.Value);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Enter a Goto Instruction.
        /// </summary>
        /// <param name="node">The Goto instruction node</param>
        protected virtual void EnterGoto(Goto node)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            if (this.CurrentProgramCfgBuilder.PendingGOTOs == null)
            {
                this.CurrentProgramCfgBuilder.PendingGOTOs = new LinkedList<Tuple<Goto, SectionNode, BasicBlockForNode>>();
            }
            var item = new Tuple<Goto, SectionNode, BasicBlockForNode>(node, this.CurrentProgramCfgBuilder.CurrentSectionNode, this.CurrentProgramCfgBuilder.CurrentBasicBlock);
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
        /// The Multi Branch Stack during Graph Construction.
        ///  Used for IF-THEN-ELSE or EVALUATE
        /// </summary>
        internal Stack<MultiBranchContext> MultiBranchContextStack
        {
            get;
            set;
        }

        /// <summary>
        /// The Current Declarative context if any.
        /// </summary>
        private DeclarativesContext CurrentDeclarativesContext;

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
        /// Set whether or not the EVALUATE statement shall be translated using cascading IF-THEN-ELSE, false
        /// otherwise.
        /// </summary>
        public bool UseEvaluateCascade { get; }

        /// <summary>
        /// Set whether or not the SEARCH statement shall be translated using cascading IF-THEN-ELSE, false
        /// otherwise.
        /// </summary>
        public bool UseSearchCascade { get; }

        /// <summary>
        /// Enter an Evaluate statement
        /// </summary>
        /// <param name="evaluate">The Evaluate node</param>
        protected virtual void EnterEvaluate(Evaluate evaluate)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.CurrentBasicBlock != null);
            MultiBranchContext ctx = new MultiBranchContext(this.CurrentProgramCfgBuilder, evaluate);
            //Create a list of node of contextual When and WhenOther nodes.
            ctx.ConditionNodes = new List<Node>();
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
            System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);

            ctx.ConditionNodes.Add(node);
        }

        /// <summary>
        /// Leave a When condition node
        /// </summary>
        /// <param name="node">The when condition node</param>
        protected virtual void LeaveWhen(When node)
        {

        }

        /// <summary>
        /// Enter a WhenOther condition ode.
        /// </summary>
        /// <param name="node">The WhenOther node</param>
        protected virtual void EnterWhenOther(WhenOther node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);
            
            ctx.ConditionNodes.Add(node);
        }

        /// <summary>
        /// Leave a WhenOther condition ode.
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
                System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);

                var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                //Associate all When Conditions to the block.
                List<Node> data = ctx.ConditionNodes;
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
            ctxWhens.ConditionNodes = new List<Node>();
            //Push and start the Whens context.
            this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctxWhens);
            ctxWhens.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);

            //Associate all When Conditions to the block.
            List<Node> data = ctx.ConditionNodes;
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

        /// <summary>
        /// Here is when we can capture the beginning of a set of WhenOtherClause so we can start a new Basic Block. 
        /// </summary>
        /// <param name="cond"></param>
        public override void StartWhenOtherClause(TypeCobol.Compiler.CodeElements.WhenOtherCondition cond)
        {
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack != null);
            System.Diagnostics.Debug.Assert(this.CurrentProgramCfgBuilder.MultiBranchContextStack.Count > 0);
            MultiBranchContext ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Peek();
            System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);

            var whenOtherCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
            whenOtherCondBlock.SetFlag(BasicBlock<Node, D>.Flags.Default, true);
            //Associate WhenOther Condition to the block.
            List<Node> data = ctx.ConditionNodes;
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
            //Create a list of node of contextual When or AtEnd nodes.
            ctx.ConditionNodes = new List<Node>();
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
            System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);

            if (UseSearchCascade)
            {
                if (condition == null)
                {
                    var whenCondBlock = this.CurrentProgramCfgBuilder.CreateBlock(null, true);
                    //This is like a default condition.
                    whenCondBlock.SetFlag(BasicBlock<Node, D>.Flags.Default, true);
                    //Associate all When SearchConditions to the block.
                    List<Node> data = ctx.ConditionNodes;
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
                    {//We had an At END Condition ==> So the Current basic block is the Search Block
                        this.CurrentProgramCfgBuilder.CurrentBasicBlock = ctx.OriginBlock;
                    }
                    //Create Whens context
                    MultiBranchContext ctxWhens = new MultiBranchContext(this.CurrentProgramCfgBuilder, null);
                    ctxWhens.ConditionNodes = new List<Node>();
                    ctxWhens.RootBlock = ctx.RootBlock;
                    ctxWhens.RootBlockSuccessorIndex = ctx.RootBlockSuccessorIndex;
                    //Push and start the Whens context.
                    this.CurrentProgramCfgBuilder.MultiBranchContextStack.Push(ctxWhens);
                    ctxWhens.Start(this.CurrentProgramCfgBuilder.CurrentBasicBlock);

                    //Associate all When Conditions to the block.
                    List<Node> data = ctx.ConditionNodes;
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
                List<Node> data = ctx.ConditionNodes;
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
                        ctx.OriginBlock.SuccessorEdges.Add(rootNodeIndex);
                    }
                    else
                    {
                        ctx.End(branchToNext, nextBlock);
                    }
                    this.CurrentProgramCfgBuilder.CurrentBasicBlock = nextBlock;
                    ctx = this.CurrentProgramCfgBuilder.MultiBranchContextStack.Pop();
                }
                //If we have and AT Condition handle it
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
            System.Diagnostics.Debug.Assert(ctx.ConditionNodes != null);

            ctx.ConditionNodes.Add(node);
        }

        /// <summary>
        /// Leave a When Search condition node
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
            //Create a Perform standalone instruction block.
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
        /// Test if the a perform loop is an AFTER
        /// </summary>
        /// <param name="perform"></param>
        /// <returns>true if the PERFORM loop is an AFTER, false otherwise</returns>
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

            //First Get here all terminal blocks of the loop body
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
                //the next block is a transition for the perform. 
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
                    this.CurrentProgramCfgBuilder.PendingNextSentences = new LinkedList<Tuple<NextSentence, BasicBlockForNode, Sentence>>();
                }
                //Track pending Next Sentences.
                Tuple<NextSentence, BasicBlockForNode, Sentence> item = new Tuple<NextSentence, BasicBlockForNode, Sentence>(
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
        /// <param name="node">the Next Sentence node</param>
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
                    SectionNode sectionNode = item.Item2;
                    AlterGotoInstruction[] gotos = alter.CodeElement.AlterGotoInstructions;
                    foreach (AlterGotoInstruction alterGoto in gotos)
                    {
                        SymbolReference alterProcReference = alterGoto.AlteredProcedure;
                        SymbolReference targetProcReference = alterGoto.NewTargetProcedure;
                        
                        //So lookup the paragraph
                        Procedure alterProc = ResolveProcedure(alter, sectionNode, alterProcReference);
                        if (alterProc == null)
                            continue;

                        //So also Resolve the target.
                        Procedure targetProc = ResolveProcedure(alter, sectionNode, targetProcReference);
                        if (targetProc == null)
                            continue;

                        //So Look for the first Goto Instruction
                        //The first instruction of the altered procedure must be a GOTO instruction (without DEPENDING ON)
                        bool targetGotoResolved = false;
                        var instructions = alterProc.FirstOrDefault()?.FirstBlock?.Instructions;
                        if (instructions != null && instructions.Count > 0)
                        {
                            Node firstInstruction = instructions.First.Value;
                            if (firstInstruction.CodeElement?.Type == CodeElementType.GotoStatement)
                            {
                                Goto @goto = (Goto) firstInstruction;
                                if (@goto.CodeElement.StatementType == StatementType.GotoSimpleStatement)
                                {
                                    if (this.CurrentProgramCfgBuilder.PendingAlteredGOTOS == null)
                                        this.CurrentProgramCfgBuilder.PendingAlteredGOTOS = new Dictionary<Goto, HashSet<SymbolReference>>();

                                    if (!this.CurrentProgramCfgBuilder.PendingAlteredGOTOS.TryGetValue(@goto, out var targetSymbols))
                                    {
                                        targetSymbols = new HashSet<SymbolReference>();
                                        this.CurrentProgramCfgBuilder.PendingAlteredGOTOS.Add(@goto, targetSymbols);
                                    }

                                    targetSymbols.Add(targetProcReference);
                                    targetGotoResolved = true;
                                }
                            }
                        }

                        if (!targetGotoResolved)
                        {
                            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                                alter.CodeElement.Column,
                                alter.CodeElement.Column,
                                alter.CodeElement.Line,
                                Resource.BadAlterIntrWithNoSiblingGotoInstr);
                            AddDiagnostic(d);
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
                this.CurrentProgramCfgBuilder.PendingALTERs = new LinkedList<Tuple<Alter, SectionNode>>();
            }
            var item = new Tuple<Alter, SectionNode>(node, this.CurrentProgramCfgBuilder.CurrentSectionNode);
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
                ctx.ConditionNodes.Add(node);
                //Call StartWhenSearchConditionClause with null, this will mean AT END condition.
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
                this.CurrentProgramCfgBuilder.PendingPERFORMProcedures = new LinkedList<Tuple<PerformProcedure, SectionNode, BasicBlockForNodeGroup>>();
            }

            var item = new Tuple<PerformProcedure, SectionNode, BasicBlockForNodeGroup>(node, this.CurrentProgramCfgBuilder.CurrentSectionNode, group);
            this.CurrentProgramCfgBuilder.PendingPERFORMProcedures.AddLast(item);
        }

        /// <summary>
        /// Leave a PERFORM procedure statement
        /// </summary>
        /// <param name="node">The PERFORM Procedure node</param>
        protected virtual void LeavePerformProcedure(PerformProcedure node)
        {
        }

        /// <summary>
        /// Enter a Declarative
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
            if (addToCurrentSentence)
            {
                this.CurrentProgramCfgBuilder.CurrentSentence?.AddBlock(block);
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

        public static readonly string ROOT_SECTION_NAME = "<< RootSection >>";

        /// <summary>
        /// Start the Cfg construction for a ProcedureDivision node
        /// </summary>
        /// <param name="procDiv">The Procedure Division</param>
        protected virtual void StartCfg(ProcedureDivision procDiv)
        {
            System.Diagnostics.Debug.Assert(Cfg != null);
            Cfg.Initialize(procDiv);

            //Create a Root Section
            System.Diagnostics.Debug.Assert(AllProcedures.Count == 0);
            Section rootSection = new Section(0, ROOT_SECTION_NAME);
            AllProcedures.Add(rootSection);

            //The new current section.
            CurrentSection = rootSection;
            CurrentSectionNode = null;
            //Create a starting sentence
            StartBlockSentence();
            //Make the starting block of the Root section the root block.            
            Cfg.RootBlock = CurrentBasicBlock;
            Cfg.BlockFor[procDiv] = CurrentBasicBlock;
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
