using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;
using Antlr4.Runtime;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a Program or Class object while visiting its parse tree
    /// </summary>
    public class ProgramClassBuilder : CobolProgramClassBaseListener
    {
        /// <summary>
        /// Program object resulting of the visit the parse tree
        /// </summary>
        public Program Program { get; private set; }

        // Programs can be nested => track current programs being analyzed
        private Stack<Program> programsStack = null;

        private Program CurrentProgram {
            get { return programsStack.Peek(); }
            set { programsStack.Push(value); }
        }

        /// <summary>Class object resulting of the visit the parse tree</summary>
        public Class Class { get; private set; }

        private SymbolTable TableOfExternals = new SymbolTable(null, SymbolTable.Scope.External);
        private SymbolTable TableOfGlobals;

        public ProgramDispatcher Dispatcher { get; internal set; }

        private void _add(Node node) { Program.SyntaxTree.Add(node); }
        private void _enter(CodeElement e, ParserRuleContext context) {
            _enter(new Node(e));
            if (e!=null) Dispatcher.OnCodeElement(e, context, CurrentProgram);
        }
        private void _enter(Node node) { Program.SyntaxTree.Attach(node); }
        private void _exit() { Program.SyntaxTree.Detach(); }

        /// <summary>
        /// Initialization code run before parsing each new Program or Class
        /// </summary>
        public override void EnterCobolCompilationUnit(CobolProgramClassParser.CobolCompilationUnitContext context)
        {
            TableOfGlobals = new SymbolTable(TableOfExternals, SymbolTable.Scope.Global);
            Program = null;
            Class = null;
        }

        public override void EnterCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            if (Program == null) {
                Program = new SourceProgram(TableOfGlobals);
                programsStack = new Stack<Program>();
                CurrentProgram = Program;
            } else {
                var enclosing = CurrentProgram;
                CurrentProgram = new NestedProgram(enclosing);
                _enter(CurrentProgram.SyntaxTree.Root);
            }
            CurrentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;
            _enter(new Node(AsCodeElement(context.ProgramIdentification())));
        }

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            programsStack.Pop();
            _exit();
        }

        public override void EnterEnvironmentDivision(CobolProgramClassParser.EnvironmentDivisionContext context) {
            _enter(new Node(AsCodeElement(context.EnvironmentDivisionHeader())));
        }
        public override void ExitEnvironmentDivision(CobolProgramClassParser.EnvironmentDivisionContext context) {
            _exit();
        }

        public override void EnterDataDivision(CobolProgramClassParser.DataDivisionContext context) {
            _enter(new Node(AsCodeElement(context.DataDivisionHeader())));
        }
        public override void ExitDataDivision(CobolProgramClassParser.DataDivisionContext context) {
            _exit();
        }

        public override void EnterWorkingStorageSection(CobolProgramClassParser.WorkingStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.WorkingStorageSectionHeader(), entries);
            UpdateSymbolsTable(entries, SymbolTable.Section.Working);
        }

        public override void EnterLocalStorageSection(CobolProgramClassParser.LocalStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LocalStorageSectionHeader(), entries);
            UpdateSymbolsTable(entries, SymbolTable.Section.Local);
        }

        public override void EnterLinkageSection(CobolProgramClassParser.LinkageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LinkageSectionHeader(), entries);
            UpdateSymbolsTable(entries, SymbolTable.Section.Linkage);
        }

        private void AddStorageNode(Antlr4.Runtime.Tree.ITerminalNode terminal, IList<DataDescriptionEntry> entries) {
            var node = new Node(AsCodeElement(terminal));
            AddEntries(node, entries);
            _add(node);
        }
        private void AddEntries(Node root, IList<DataDescriptionEntry> entries) {
            foreach(var entry in entries) {
                var child = new Node(entry);
                AddEntries(child, entry.Subordinates);
                root.Add(child);
            }
        }

        /// <summary>Update toplevel/subordinate relations of data description entries.</summary>
        /// <param name="nodes">DataDescriptionEntry[] array -typically <section context>.DataDescriptionEntry()</param>
        /// <returns>nodes parameter, but with each element having its TopLevel and Subordinates properties initialized</returns>
        private IList<DataDescriptionEntry> CreateDataDescriptionEntries(Antlr4.Runtime.Tree.ITerminalNode[] nodes) {
            IList<DataDescriptionEntry> result = new List<DataDescriptionEntry>();
            if (nodes == null) return result;
            Stack<DataDescriptionEntry> groups = new Stack<DataDescriptionEntry>();
            foreach (var node in nodes) {
                DataDescriptionEntry data = node.Symbol as DataDescriptionEntry;
                bool okay = false;
                while(!okay && groups.Count > 0) {
                    var toplevel = groups.Peek();
                    if (data.LevelNumber <= toplevel.LevelNumber) groups.Pop();
                    else {
                        toplevel.Subordinates.Add(data);
                        data.TopLevel = toplevel;
                        okay = true;
                    }
                }
                if (data.IsGroup) groups.Push(data);
                if (!okay) result.Add(data);
            }
            return result;
        }

        private void UpdateSymbolsTable(IList<DataDescriptionEntry> data, SymbolTable.Section section) {
            foreach(var d in data) CurrentProgram.SymbolTable.Add(section, d);
        }

        public override void EnterProcedureDivision(CobolProgramClassParser.ProcedureDivisionContext context) {
            _enter(new Node(AsCodeElement(context.ProcedureDivisionHeader())));
        }
        public override void ExitProcedureDivision(CobolProgramClassParser.ProcedureDivisionContext context) {
            _exit();
        }

        public override void EnterSection(CobolProgramClassParser.SectionContext context) {
            var terminal = context.SectionHeader();
            if (terminal == null) terminal = context.ParagraphHeader();
            var node = new Node(AsCodeElement(terminal));
            _enter(node);
        }
        public override void ExitSection(CobolProgramClassParser.SectionContext context) {
            _exit();
        }

        public override void EnterParagraph(CobolProgramClassParser.ParagraphContext context) {
            _enter(new Node(AsCodeElement(context.ParagraphHeader())));
        }
        public override void ExitParagraph(CobolProgramClassParser.ParagraphContext context) {
            _exit();
        }

        public override void EnterSentence(CobolProgramClassParser.SentenceContext context) {
            _enter(new Node(null));
        }
        public override void ExitSentence(CobolProgramClassParser.SentenceContext context) {
            Program.SyntaxTree.Add(new Node(AsCodeElement(context.SentenceEnd())));
            _exit();
        }

        public override void EnterStatement(CobolProgramClassParser.StatementContext context) {
            CodeElement statement = AsStatement(context);
            _enter(statement, context);
        }
        public override void ExitStatement(CobolProgramClassParser.StatementContext context) {
            _exit();
        }

        public override void EnterIfStatementWithBody(CobolProgramClassParser.IfStatementWithBodyContext context) {
            _enter(new Node(AsCodeElement(context.IfStatement())));
            _enter(new Node(null));//THEN
        }
        public override void EnterElseClause(CobolProgramClassParser.ElseClauseContext context) {
            _exit();// we want ELSE to be child of IF, not THEN, so exit IF
            var node = new Node(AsCodeElement(context.ElseCondition()));
            _enter(node);
            var next = AsCodeElement(context.NextSentenceStatement());
            if (next != null) node.Add(new Node(next));
        }
        public override void ExitElseClause(CobolProgramClassParser.ElseClauseContext context) {
            _exit();
        }
        public override void ExitIfStatementWithBody(CobolProgramClassParser.IfStatementWithBodyContext context) {
            var end = AsCodeElement(context.IfStatementEnd());
            if (end != null) _add(new Node(end));
            _exit();
        }


        private CodeElement AsCodeElement(Antlr4.Runtime.Tree.ITerminalNode node) {
            return node != null? (CodeElement)node.Symbol : null;
        }

        private CodeElement AsStatement(CobolProgramClassParser.StatementContext context)
        {
            return
                (CodeElement)AsCodeElement(context.ContinueStatement()) ??
/* TODO
	| evaluateStatementExplicitScope
	| ifStatementExplicitScope
	| searchStatementExplicitScope
 */
// -- arithmetic --
                (CodeElement)AsCodeElement(context.AddStatement()) ??
                (CodeElement)AsCodeElement(context.ComputeStatement()) ??
                (CodeElement)AsCodeElement(context.DivideStatement()) ??
                (CodeElement)AsCodeElement(context.MultiplyStatement()) ??
                (CodeElement)AsCodeElement(context.SubtractStatement()) ??
/* TODO
	| addStatementExplicitScope
	| computeStatementExplicitScope
	| divideStatementExplicitScope
	| multiplyStatementExplicitScope
	| subtractStatementExplicitScope
 */

// -- data movement --
                (CodeElement)AsCodeElement(context.AcceptStatement()) ?? // (DATE, DAY, DAY-OF-WEEK, TIME)
                (CodeElement)AsCodeElement(context.InitializeStatement()) ??
                (CodeElement)AsCodeElement(context.InspectStatement()) ??
                (CodeElement)AsCodeElement(context.MoveStatement()) ??
                (CodeElement)AsCodeElement(context.SetStatement()) ?? // "table-handling" too
                (CodeElement)AsCodeElement(context.StringStatement()) ??
                (CodeElement)AsCodeElement(context.UnstringStatement()) ??
                (CodeElement)AsCodeElement(context.XmlGenerateStatement()) ??
                (CodeElement)AsCodeElement(context.XmlParseStatement()) ??
/* TODO
	| stringStatementExplicitScope
	| unstringStatementExplicitScope
	| xmlGenerateStatementExplicitScope
	| xmlParseStatementExplicitScope
 */
// -- ending --
                (CodeElement)AsCodeElement(context.StopStatement()) ?? // RUN
                (CodeElement)AsCodeElement(context.ExitMethodStatement()) ??
                (CodeElement)AsCodeElement(context.ExitProgramStatement()) ??
                (CodeElement)AsCodeElement(context.GobackStatement()) ??
// -- input-output --
//              (CodeElement)AsCodeElement(context.AcceptStatement()) ?? // identifier
                (CodeElement)AsCodeElement(context.CloseStatement()) ??
                (CodeElement)AsCodeElement(context.DeleteStatement()) ??
                (CodeElement)AsCodeElement(context.DisplayStatement()) ??
                (CodeElement)AsCodeElement(context.OpenStatement()) ??
                (CodeElement)AsCodeElement(context.ReadStatement()) ??
                (CodeElement)AsCodeElement(context.RewriteStatement()) ??
                (CodeElement)AsCodeElement(context.StartStatement()) ??
//              (CodeElement)AsCodeElement(context.StopStatement()) ?? // literal
                (CodeElement)AsCodeElement(context.WriteStatement()) ??
/* TODO
	| deleteStatementExplicitScope
	| readStatementExplicitScope
	| rewriteStatementExplicitScope
	| startStatementExplicitScope
	| writeStatementExplicitScope
 */
// -- ordering --
                (CodeElement)AsCodeElement(context.MergeStatement()) ??
                (CodeElement)AsCodeElement(context.ReleaseStatement()) ??
                (CodeElement)AsCodeElement(context.ReturnStatement()) ??
                (CodeElement)AsCodeElement(context.SortStatement()) ??
/* TODO
	| returnStatementExplicitScope
 */
// -- procedure-branching --
                (CodeElement)AsCodeElement(context.AlterStatement()) ??
                (CodeElement)AsCodeElement(context.ExitStatement()) ??
                (CodeElement)AsCodeElement(context.GotoStatement()) ??
                (CodeElement)AsCodeElement(context.PerformProcedureStatement()) ??
/* TODO
	| performStatementWithBody
 */
// -- program or method linkage --
                (CodeElement)AsCodeElement(context.CallStatement()) ??
                (CodeElement)AsCodeElement(context.CancelStatement()) ??
                (CodeElement)AsCodeElement(context.InvokeStatement()) ??
/* TODO
	| callStatementExplicitScope
	| invokeStatementExplicitScope
 */
                (CodeElement)AsCodeElement(context.ExecStatement()) ??
                null;
        }
    }
}
