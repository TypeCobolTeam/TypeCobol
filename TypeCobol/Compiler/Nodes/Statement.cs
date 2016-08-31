namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;



public interface Statement { }



public class Accept: Node, CodeElementHolder<AcceptStatement>, Statement {
	public Accept(AcceptStatement statement): base(statement) { }
}

public class Alter: Node, CodeElementHolder<AlterStatement>, Statement {
	public Alter(AlterStatement statement): base(statement) { }
}

public class Call: Node, CodeElementHolder<CallStatement>, Statement {
	public Call(CallStatement statement): base(statement) { }
}

public class Cancel: Node, CodeElementHolder<CancelStatement>, Statement {
	public Cancel(CancelStatement statement): base(statement) { }
}

public class Continue: Node, CodeElementHolder<ContinueStatement>, Statement {
	public Continue(ContinueStatement statement): base(statement) { }
}

public class Delete: Node, CodeElementHolder<DeleteStatement>, Statement {
	public Delete(DeleteStatement statement): base(statement) { }
}

public class Display: Node, CodeElementHolder<DisplayStatement>, Statement {
	public Display(DisplayStatement statement): base(statement) { }
}

public class Entry: Node, CodeElementHolder<EntryStatement>, Statement {
	public Entry(EntryStatement statement): base(statement) { }
}

public class Exec: Node, CodeElementHolder<ExecStatement>, Statement {
	public Exec(ExecStatement statement): base(statement) { }
}

public class Exit: Node, CodeElementHolder<ExitStatement>, Statement {
	public Exit(ExitStatement statement): base(statement) { }
}

public class ExitMethod: Node, CodeElementHolder<ExitMethodStatement>, Statement {
	public ExitMethod(ExitMethodStatement statement): base(statement) { }
}

public class ExitProgram: Node, CodeElementHolder<ExitProgramStatement>, Statement {
	public ExitProgram(ExitProgramStatement statement): base(statement) { }
}

public class Goback: Node, CodeElementHolder<GobackStatement>, Statement {
	public Goback(GobackStatement statement): base(statement) { }
}

public class Goto: Node, CodeElementHolder<GotoStatement>, Statement {
	public Goto(GotoStatement statement): base(statement) { }
}

public class Initialize: Node, CodeElementHolder<InitializeStatement>, Statement {
	public Initialize(InitializeStatement statement): base(statement) { }
}

public class Inspect: Node, CodeElementHolder<InspectStatement>, Statement {
	public Inspect(InspectStatement statement): base(statement) { }
}

public class Invoke: Node, CodeElementHolder<InvokeStatement>, Statement {
	public Invoke(InvokeStatement statement): base(statement) { }
}

public class Merge: Node, CodeElementHolder<MergeStatement>, Statement {
	public Merge(MergeStatement statement): base(statement) { }
}

public class Move: Node, CodeElementHolder<MoveStatement>, Statement, VariableUser {
	public Move(MoveStatement statement): base(statement) { }
	public IList<QualifiedName> Variables { get { return this.CodeElement().Variables; } }
}

public class Release: Node, CodeElementHolder<ReleaseStatement>, Statement {
	public Release(ReleaseStatement statement): base(statement) { }
}

public class Return: Node, CodeElementHolder<ReturnStatement>, Statement {
	public Return(ReturnStatement statement): base(statement) { }
}

public class Set: Node, CodeElementHolder<SetStatement>, Statement, VariableUser {
	public Set(SetStatement statement): base(statement) { }
	public IList<QualifiedName> Variables { get { return this.CodeElement().Variables; } }
}

public class Sort: Node, CodeElementHolder<SortStatement>, Statement {
	public Sort(SortStatement statement): base(statement) { }
}

public class Start: Node, CodeElementHolder<StartStatement>, Statement {
	public Start(StartStatement statement): base(statement) { }
}

public class Stop: Node, CodeElementHolder<StopStatement>, Statement {
	public Stop(StopStatement statement): base(statement) { }
}

public class String: Node, CodeElementHolder<StringStatement>, Statement {
	public String(StringStatement statement): base(statement) { }
}

public class Unstring: Node, CodeElementHolder<UnstringStatement>, Statement {
	public Unstring(UnstringStatement statement): base(statement) { }
}

public class XmlGenerate: Node, CodeElementHolder<XmlGenerateStatement>, Statement {
	public XmlGenerate(XmlGenerateStatement statement): base(statement) { }
}

public class XmlParse: Node, CodeElementHolder<XmlParseStatement>, Statement {
	public XmlParse(XmlParseStatement statement): base(statement) { }
}



// --- ARITHMETIC STATEMENTS ---

public class Add: Node, CodeElementHolder<AddStatement>, Statement {
	public Add(AddStatement statement): base(statement) { }
}

public class Subtract: Node, CodeElementHolder<SubtractStatement>, Statement {
	public Subtract(SubtractStatement statement): base(statement) { }
}

public class Multiply: Node, CodeElementHolder<MultiplyStatement>, Statement {
	public Multiply(MultiplyStatement statement): base(statement) { }
}

public class Divide: Node, CodeElementHolder<DivideStatement>, Statement {
	public Divide(DivideStatement statement): base(statement) { }
}

public class Compute: Node, CodeElementHolder<ComputeStatement>, Statement {
	public Compute(ComputeStatement statement): base(statement) { }
}



// --- FILE STATEMENTS ---

public class Open: Node, CodeElementHolder<OpenStatement>, Statement {
	public Open(OpenStatement statement): base(statement) { }
}

public class Close: Node, CodeElementHolder<CloseStatement>, Statement {
	public Close(CloseStatement statement): base(statement) { }
}

public class Read: Node, CodeElementHolder<ReadStatement>, Statement {
	public Read(ReadStatement statement): base(statement) { }
}

public class Write: Node, CodeElementHolder<WriteStatement>, Statement {
	public Write(WriteStatement statement): base(statement) { }
}

public class Rewrite: Node, CodeElementHolder<RewriteStatement>, Statement {
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

public class If: Node, CodeElementHolder<IfStatement>, Statement {
	public If(IfStatement statement): base(statement) { }
}
public class Then: Node, CodeElementHolder<CodeElement>, StatementCondition {
	public Then(): base(null) { }
}
public class Else: Node, CodeElementHolder<ElseCondition>, StatementCondition {
	public Else(ElseCondition statement): base(statement) { }
}
public class NextSentence: Node, CodeElementHolder<NextSentenceStatement>, Statement {
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

public class Evaluate: Node, CodeElementHolder<EvaluateStatement>, Statement {
	public Evaluate(EvaluateStatement statement): base(statement) { }
}
public class WhenGroup: Node, CodeElementHolder<CodeElement>, StatementCondition {
	public WhenGroup(): base(null) { }
}
public class When: Node, CodeElementHolder<WhenCondition>, StatementCondition {
	public When(WhenCondition statement): base(statement) { }
}
public class WhenOther: Node, CodeElementHolder<WhenOtherCondition>, StatementCondition {
	public WhenOther(WhenOtherCondition statement): base(statement) { }
}

// TODO#248
// PERFORM
//  \---> statements

public class Perform: Node, CodeElementHolder<PerformStatement>, Statement {
	public Perform(PerformStatement statement): base(statement) { }
}
public class PerformProcedure: Node, CodeElementHolder<PerformProcedureStatement>, Statement {
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
public class Search: Node, CodeElementHolder<SearchStatement>, Statement {
	public Search(SearchStatement statement): base(statement) { }
}
public class WhenSearch: Node, CodeElementHolder<WhenSearchCondition>, StatementCondition {
	public WhenSearch(WhenSearchCondition statement): base(statement) { }
}

} // end of namespace TypeCobol.Compiler.Nodes
