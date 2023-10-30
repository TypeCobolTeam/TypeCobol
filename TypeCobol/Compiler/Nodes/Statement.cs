using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Nodes {

    public interface Statement { }

    public interface StatementWithBody : Statement
    {
        CodeElementType EndType { get; }
    }

    public class Accept: GenericNode<AcceptStatement>, Statement {
        public Accept(AcceptStatement statement): base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Allocate : GenericNode<AllocateStatement>, Statement
    {
        public Allocate(AllocateStatement statement)
            : base(statement)
        {

        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Alter: GenericNode<AlterStatement>, Statement {
        public Alter(AlterStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Call: GenericNode<CallStatement>, StatementWithBody {
        public Call(CallStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public CodeElementType EndType => CodeElementType.CallStatementEnd;
    }
    public class ProcedureStyleCall : GenericNode<ProcedureStyleCallStatement>, StatementWithBody, FunctionCaller {
        public ProcedureStyleCall(ProcedureStyleCallStatement statement) : base(statement) { }

        public FunctionCall FunctionCall
        {
            get { return CodeElement.ProcedureCall; }
        }

        public FunctionDeclaration FunctionDeclaration {get; set;}

        /// <summary>
        /// True if this Procedure call in case of External call is not performed by COBOL EXTERNAL POINTER,
        /// false otherwise.
        /// </summary>
        public bool IsNotByExternalPointer
        {
            get;
            set;
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public override string ToString()
        {
            var doc = FunctionDeclaration.CodeElement.FormalizedCommentDocumentation;
            if (doc != null)
                return doc.ToString();

            return string.Empty;
        }

        public CodeElementType EndType => CodeElementType.CallStatementEnd;
    }

    public class Cancel: GenericNode<CancelStatement>, Statement {
        public Cancel(CancelStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Continue: GenericNode<ContinueStatement>, Statement {
        public Continue(ContinueStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Delete: GenericNode<DeleteStatement>, StatementWithBody {
        public Delete(DeleteStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.DeleteStatementEnd;
    }

    public class Display: GenericNode<DisplayStatement>, Statement {
        public Display(DisplayStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Entry: GenericNode<EntryStatement>, Statement {
        public Entry(EntryStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Exec: GenericNode<ExecStatement>, StatementWithBody {
        public Exec(ExecStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.ExecStatementEnd;
    }

    public class ExecText : GenericNode<ExecStatementText>, Statement
    {
        public ExecText(ExecStatementText statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Exit: GenericNode<ExitStatement>, Statement {
        public Exit(ExitStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ExitMethod: GenericNode<ExitMethodStatement>, Statement {
        public ExitMethod(ExitMethodStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ExitParagraph : GenericNode<ExitParagraphStatement>, Statement
    {
        public ExitParagraph(ExitParagraphStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ExitPerform : GenericNode<ExitPerformStatement>, Statement
    {
        public ExitPerform(ExitPerformStatement statement) : base(statement) { }

        public bool Cycle => CodeElement.Cycle?.Value ?? false;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ExitProgram: GenericNode<ExitProgramStatement>, Statement {
        public ExitProgram(ExitProgramStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ExitSection : GenericNode<ExitSectionStatement>, Statement
    {
        public ExitSection(ExitSectionStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Free : GenericNode<FreeStatement>, Statement
    {
        public Free(FreeStatement statement)
            : base(statement)
        {

        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Goback: GenericNode<GobackStatement>, Statement {
        public Goback(GobackStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Goto: GenericNode<GotoStatement>, Statement {
        public Goto(GotoStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Initialize: GenericNode<InitializeStatement>, Statement {
        public Initialize(InitializeStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Inspect: GenericNode<InspectStatement>, Statement, VariableWriter {
        public Inspect(InspectStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
    }

    public class Invoke: GenericNode<InvokeStatement>, StatementWithBody {
        public Invoke(InvokeStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.InvokeStatementEnd;
    }

    public class JsonGenerate : GenericNode<JsonGenerateStatement>, StatementWithBody
    {
        public JsonGenerate(JsonGenerateStatement statement)
            : base(statement)
        {

        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public CodeElementType EndType => CodeElementType.JsonStatementEnd;
    }

    public class JsonParse : GenericNode<JsonParseStatement>, StatementWithBody
    {
        public JsonParse(JsonParseStatement statement)
            : base(statement)
        {

        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public CodeElementType EndType => CodeElementType.JsonStatementEnd;
    }

    public class Merge: GenericNode<MergeStatement>, Statement {
        public Merge(MergeStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Move: GenericNode<MoveStatement>, Statement, VariableWriter,FunctionCaller {
        public Move(MoveStatement statement): base(statement) { }
        public FunctionCall FunctionCall { get { return this.CodeElement.FunctionCall; } }
       
        public IDictionary<StorageArea, object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }

        public FunctionDeclaration FunctionDeclaration { get; set; }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
    }

    public class Release: GenericNode<ReleaseStatement>, Statement {
        public Release(ReleaseStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Return: GenericNode<ReturnStatement>, StatementWithBody {
        public Return(ReturnStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.ReturnStatementEnd;
    }

    public class Set: GenericNode<SetStatement>, Statement, VariableWriter {
        public Set(SetStatement statement): base(statement) { }
        public IDictionary<StorageArea, object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
    }

    public enum SortNature
    {
        Undefined,
        FileSort,
        TableSort
    }

    public class Sort: GenericNode<SortStatement>, Statement {
        public Sort(SortStatement statement): base(statement) { Nature = SortNature.Undefined; }

        public ParagraphSymbol InputProcedureParagraphSymbol { get; internal set; }
        public ParagraphSymbol InputThroughProcedureParagraphSymbol { get; internal set; }
        public SectionSymbol InputProcedureSectionSymbol { get; internal set; }
        public SectionSymbol InputThroughProcedureSectionSymbol { get; internal set; }

        public SortNature Nature { get; internal set; }
        public ParagraphSymbol OutputProcedureParagraphSymbol { get; internal set; }
        public ParagraphSymbol OutputThroughProcedureParagraphSymbol { get; internal set; }
        public SectionSymbol OutputProcedureSectionSymbol { get; internal set; }
        public SectionSymbol OutputThroughProcedureSectionSymbol { get; internal set; }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Start: GenericNode<StartStatement>, StatementWithBody {
        public Start(StartStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.StartStatementEnd;
    }

    public class Stop: GenericNode<StopStatement>, Statement {
        public Stop(StopStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class String: GenericNode<StringStatement>, StatementWithBody, VariableWriter {
        public String(StringStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.StringStatementEnd;
    }

    public class Unstring: GenericNode<UnstringStatement>, StatementWithBody, VariableWriter {
        public Unstring(UnstringStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.UnstringStatementEnd;
    }

    public class XmlGenerate: GenericNode<XmlGenerateStatement>, StatementWithBody {
        public XmlGenerate(XmlGenerateStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.XmlStatementEnd;
    }

    public class XmlParse: GenericNode<XmlParseStatement>, StatementWithBody {
        public XmlParse(XmlParseStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.XmlStatementEnd;
    }



    // --- ARITHMETIC STATEMENTS ---

    public class Add: GenericNode<AddStatement>, StatementWithBody, VariableWriter {
        public Add(AddStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }

        public CodeElementType EndType => CodeElementType.AddStatementEnd;
    }

    public class Use : GenericNode<UseStatement>, Statement
    {
        public Use(UseStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Subtract: GenericNode<SubtractStatement>, StatementWithBody, VariableWriter {
        public Subtract(SubtractStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.SubtractStatementEnd;
    }

    public class Multiply: GenericNode<MultiplyStatement>, StatementWithBody, VariableWriter {
        public Multiply(MultiplyStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.MultiplyStatementEnd;
    }

    public class Divide: GenericNode<DivideStatement>, StatementWithBody, VariableWriter {
        public Divide(DivideStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.DivideStatementEnd;
    }

    public class Compute: GenericNode<ComputeStatement>, StatementWithBody, VariableWriter {
        public Compute(ComputeStatement statement): base(statement) { }
        public IDictionary<StorageArea,object> VariablesWritten { get { return this.CodeElement.VariablesWritten; } }
        public bool IsUnsafe { get { return this.CodeElement.IsUnsafe; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && astVisitor.VisitVariableWriter(this);
        }
        public CodeElementType EndType => CodeElementType.ComputeStatementEnd;
    }



    // --- FILE STATEMENTS ---

    public class Open: GenericNode<OpenStatement>, Statement {
        public Open(OpenStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Close: GenericNode<CloseStatement>, Statement {
        public Close(CloseStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Read: GenericNode<ReadStatement>, StatementWithBody {
        public Read(ReadStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.ReadStatementEnd;
    }

    public class Write: GenericNode<WriteStatement>, StatementWithBody {
        public Write(WriteStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.WriteStatementEnd;
    }

    public class Rewrite: GenericNode<RewriteStatement>, StatementWithBody {
        public Rewrite(RewriteStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.RewriteStatementEnd;
    }



    // --- FLOW CONTROL STATEMENTS ---

    public interface StatementCondition { }

    // TODO#248
    // IF
    //  |---> THEN
    //  |      \--> statements
    //  \---> ELSE
    //         \--> statements

    public class If: GenericNode<IfStatement>, StatementWithBody {
        public If(IfStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.IfStatementEnd;
    }
    public class Then: Node, StatementCondition {
        public Then()
        {
            SetFlag(Node.Flag.GeneratorCanIgnoreIt, true);
        }

        protected override CodeElement InternalCodeElement => null;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class Else: GenericNode<ElseCondition>, StatementCondition {
        public Else(ElseCondition statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NextSentence: GenericNode<NextSentenceStatement>, Statement {
        public NextSentence(NextSentenceStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    // TODO#248
    // EVALUATE
    //  |---> WHEN
    //  |      \--> conditions
    //  |---> THEN
    //  |      \--> statements
    //  |---> WHEN
    //  |      \--> conditions
    //  |---> THEN
    //  |      \--> statements
    // ...
    //  \---> WHEN-OTHER
    //         \--> statements
    //
    // or maybe:
    // EVALUATE
    //  |---> WHEN
    //  |      |--> conditions
    //  |      \--> THEN
    //  |            \--> statements
    //  |---> WHEN
    //  |      |--> conditions
    //  |      \--> THEN
    //  |            \--> statements
    // ...
    //  \---> WHEN-OTHER
    //         \--> statements

    public class Evaluate: GenericNode<EvaluateStatement>, StatementWithBody {
        public Evaluate(EvaluateStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.EvaluateStatementEnd;
    }
    public class WhenGroup: Node, StatementCondition {
        public WhenGroup()
        {
            SetFlag(Node.Flag.GeneratorCanIgnoreIt, true);
        }

        protected override CodeElement InternalCodeElement => null;
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class When: GenericNode<WhenCondition>, StatementCondition {
        public When(WhenCondition statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class WhenOther: GenericNode<WhenOtherCondition>, StatementCondition {
        public WhenOther(WhenOtherCondition statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    // TODO#248
    // PERFORM
    //  \---> statements

        public class Perform: GenericNode<PerformStatement>, StatementWithBody {
            public Perform(PerformStatement statement): base(statement) { }

            public override bool VisitNode(IASTVisitor astVisitor) {
                return astVisitor.Visit(this);
            }
            public CodeElementType EndType => CodeElementType.PerformStatementEnd;
    }
    public class PerformProcedure: GenericNode<PerformProcedureStatement>, Statement {
        public PerformProcedure(PerformProcedureStatement statement): base(statement) { }

            public ParagraphSymbol ProcedureParagraphSymbol { get; internal set; }
            public ParagraphSymbol ThroughProcedureParagraphSymbol { get; internal set; }
            public SectionSymbol ProcedureSectionSymbol { get; internal set; }
            public SectionSymbol ThroughProcedureSectionSymbol { get; internal set; }

            public override bool VisitNode(IASTVisitor astVisitor)
            {
                return astVisitor.Visit(this);
            }
        }

    // TODO#248
    // SEARCH
    //  |---> WHEN
    //  |      \--> conditions
    //  |---> THEN
    //  |      \--> statements
    //  |---> WHEN
    //  |      \--> conditions
    //  |---> THEN
    //         \--> statements
    //
    // or maybe:
    // SEARCH
    //  |---> WHEN
    //  |      |--> conditions
    //  |      \--> THEN
    //  |            \--> statements
    //  |---> WHEN
    //         |--> conditions
    //         \--> THEN
    //               \--> statements
    public class Search: GenericNode<SearchStatement>, StatementWithBody {
        public Search(SearchStatement statement): base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
        public CodeElementType EndType => CodeElementType.SearchStatementEnd;
    }
} // end of namespace TypeCobol.Compiler.Nodes
