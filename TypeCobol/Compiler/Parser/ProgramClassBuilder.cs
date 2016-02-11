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

        private AST AST = null;

        public ProgramDispatcher Dispatcher { get; internal set; }

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
                Program = new SourceProgram();
                programsStack = new Stack<Program>();
                CurrentProgram = Program;
                CurrentProgram.Data = new SymbolTable(TableOfGlobals);
                AST = new AST(new ProgramN());
            } else {
                var enclosing = CurrentProgram;
                CurrentProgram = new NestedProgram(enclosing);
                CurrentProgram.Data = new SymbolTable(enclosing.Data);
                _enter(new ProgramN());
            }
            CurrentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;
        }

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            if(programsStack != null) programsStack.Pop();
            AST.Detach();
        }

        public override void EnterWorkingStorageSection(CobolProgramClassParser.WorkingStorageSectionContext context) {
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Working);
        }

        public override void EnterLocalStorageSection(CobolProgramClassParser.LocalStorageSectionContext context) {
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Local);
        }

        public override void EnterLinkageSection(CobolProgramClassParser.LinkageSectionContext context) {
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Linkage);
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
            foreach(var d in data) CurrentProgram.Data.Add(section, d);
        }

        private void _enter(CodeElement e, ParserRuleContext context) {
            _enter(new Node(e));
            if (e!=null) Dispatcher.OnCodeElement(e, context, CurrentProgram);
        }
        private void _enter(Node node) { AST.Attach(node); }
        private void _exit() { AST.Detach(); }

        public override void EnterSection(CobolProgramClassParser.SectionContext context) {
            _enter(new Section());
        }
        public override void ExitSection(CobolProgramClassParser.SectionContext context) {
            _exit();
        }

        public override void EnterParagraph(CobolProgramClassParser.ParagraphContext context) {
            _enter(new Paragraph());
        }
        public override void ExitParagraph(CobolProgramClassParser.ParagraphContext context) {
            _exit();
        }

        public override void EnterSentence(CobolProgramClassParser.SentenceContext context) {
            _enter(new Sentence());
        }
        public override void ExitSentence(CobolProgramClassParser.SentenceContext context) {
            _exit();
        }

        public override void EnterStatement(CobolProgramClassParser.StatementContext context) {
            CodeElement statement = AsStatement(context);
            _enter(statement, context);
        }
        public override void ExitStatement(CobolProgramClassParser.StatementContext context) {
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
