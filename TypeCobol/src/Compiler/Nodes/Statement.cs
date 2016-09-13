namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface Statement { }



public class Accept: CodeElementNode<AcceptStatement>, Statement {
	public Accept(AcceptStatement statement): base(statement) { }
}

public class Alter: CodeElementNode<AlterStatement>, Statement {
	public Alter(AlterStatement statement): base(statement) { }
}

public class Call: CodeElementNode<CallStatement>, Statement {
	public Call(CallStatement statement): base(statement) { }
}

public class Cancel: CodeElementNode<CancelStatement>, Statement {
	public Cancel(CancelStatement statement): base(statement) { }
}

public class Continue: CodeElementNode<ContinueStatement>, Statement {
	public Continue(ContinueStatement statement): base(statement) { }
}

public class Delete: CodeElementNode<DeleteStatement>, Statement {
	public Delete(DeleteStatement statement): base(statement) { }
}

public class Display: CodeElementNode<DisplayStatement>, Statement {
	public Display(DisplayStatement statement): base(statement) { }
}

public class Entry: CodeElementNode<EntryStatement>, Statement {
	public Entry(EntryStatement statement): base(statement) { }
}

public class Exec: CodeElementNode<ExecStatement>, Statement {
	public Exec(ExecStatement statement): base(statement) { }
}

public class Exit: CodeElementNode<ExitStatement>, Statement {
	public Exit(ExitStatement statement): base(statement) { }
}

public class ExitMethod: CodeElementNode<ExitMethodStatement>, Statement {
	public ExitMethod(ExitMethodStatement statement): base(statement) { }
}

public class ExitProgram: CodeElementNode<ExitProgramStatement>, Statement {
	public ExitProgram(ExitProgramStatement statement): base(statement) { }
}

public class Goback: CodeElementNode<GobackStatement>, Statement {
	public Goback(GobackStatement statement): base(statement) { }
}

public class Goto: CodeElementNode<GotoStatement>, Statement {
	public Goto(GotoStatement statement): base(statement) { }
}

public class Initialize: CodeElementNode<InitializeStatement>, Statement {
	public Initialize(InitializeStatement statement): base(statement) { }
}

public class Inspect: CodeElementNode<InspectStatement>, Statement {
	public Inspect(InspectStatement statement): base(statement) { }
}

public class Invoke: CodeElementNode<InvokeStatement>, Statement {
	public Invoke(InvokeStatement statement): base(statement) { }
}

public class Merge: CodeElementNode<MergeStatement>, Statement {
	public Merge(MergeStatement statement): base(statement) { }
}

public class Move: CodeElementNode<MoveStatement>, Statement {
	public Move(MoveStatement statement): base(statement) { }
}

public class Release: CodeElementNode<ReleaseStatement>, Statement {
	public Release(ReleaseStatement statement): base(statement) { }
}

public class Return: CodeElementNode<ReturnStatement>, Statement {
	public Return(ReturnStatement statement): base(statement) { }
}

public class Set: CodeElementNode<SetStatement>, Statement {
	public Set(SetStatement statement): base(statement) { }
}

public class Sort: CodeElementNode<SortStatement>, Statement {
	public Sort(SortStatement statement): base(statement) { }
}

public class Start: CodeElementNode<StartStatement>, Statement {
	public Start(StartStatement statement): base(statement) { }
}

public class Stop: CodeElementNode<StopStatement>, Statement {
	public Stop(StopStatement statement): base(statement) { }
}

public class String: CodeElementNode<StringStatement>, Statement {
	public String(StringStatement statement): base(statement) { }
}

public class Unstring: CodeElementNode<UnstringStatement>, Statement {
	public Unstring(UnstringStatement statement): base(statement) { }
}

public class XmlGenerate: CodeElementNode<XmlGenerateStatement>, Statement {
	public XmlGenerate(XmlGenerateStatement statement): base(statement) { }
}

public class XmlParse: CodeElementNode<XmlParseStatement>, Statement {
	public XmlParse(XmlParseStatement statement): base(statement) { }
}



// --- ARITHMETIC STATEMENTS ---

public class Add: CodeElementNode<AddStatement>, Statement {
	public Add(AddStatement statement): base(statement) { }
}

public class Subtract: CodeElementNode<SubtractStatement>, Statement {
	public Subtract(SubtractStatement statement): base(statement) { }
}

public class Multiply: CodeElementNode<MultiplyStatement>, Statement {
	public Multiply(MultiplyStatement statement): base(statement) { }
}

public class Divide: CodeElementNode<DivideStatement>, Statement {
	public Divide(DivideStatement statement): base(statement) { }
}

public class Compute: CodeElementNode<ComputeStatement>, Statement {
	public Compute(ComputeStatement statement): base(statement) { }
}



// --- FILE STATEMENTS ---

public class Open: CodeElementNode<OpenStatement>, Statement {
	public Open(OpenStatement statement): base(statement) { }
}

public class Close: CodeElementNode<CloseStatement>, Statement {
	public Close(CloseStatement statement): base(statement) { }
}

public class Read: CodeElementNode<ReadStatement>, Statement {
	public Read(ReadStatement statement): base(statement) { }
}

public class Write: CodeElementNode<WriteStatement>, Statement {
	public Write(WriteStatement statement): base(statement) { }
}

public class Rewrite: CodeElementNode<RewriteStatement>, Statement {
	public Rewrite(RewriteStatement statement): base(statement) { }
}



// --- FLOW CONTROL STATEMENTS ---

public interface StatementCondition { }

// TODO#248
// IF
//  |---> THEN
//  |      \--> statements
//  \---> ELSE
//         \--> statements

public class If: CodeElementNode<IfStatement>, Statement {
	public If(IfStatement statement): base(statement) { }
}
public class Then: CodeElementNode<CodeElement>, StatementCondition {
	public Then(): base(null) { }
}
public class Else: CodeElementNode<ElseCondition>, StatementCondition {
	public Else(ElseCondition statement): base(statement) { }
}
public class NextSentence: CodeElementNode<NextSentenceStatement>, Statement {
	public NextSentence(NextSentenceStatement statement): base(statement) { }
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

public class Evaluate: CodeElementNode<EvaluateStatement>, Statement {
	public Evaluate(EvaluateStatement statement): base(statement) { }
}
public class When: CodeElementNode<WhenConditionalExpression>, StatementCondition {
	public When(WhenConditionalExpression statement): base(statement) { }
}
public class WhenOther: CodeElementNode<WhenOtherCondition>, StatementCondition {
	public WhenOther(WhenOtherCondition statement): base(statement) { }
}

// TODO#248
// PERFORM
//  \---> statements

public class Perform: CodeElementNode<PerformStatement>, Statement {
	public Perform(PerformStatement statement): base(statement) { }
}
public class PerformProcedure: CodeElementNode<PerformProcedureStatement>, Statement {
	public PerformProcedure(PerformProcedureStatement statement): base(statement) { }
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
public class Search: CodeElementNode<SearchStatement>, Statement {
	public Search(SearchStatement statement): base(statement) { }
}

} // end of namespace TypeCobol.Compiler.Nodes
