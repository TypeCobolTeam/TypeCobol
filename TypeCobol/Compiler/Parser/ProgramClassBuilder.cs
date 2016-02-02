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

        private Stack<Node> Branch = new Stack<Node>();

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
            } else {
                var enclosing = CurrentProgram;
                CurrentProgram = new NestedProgram(enclosing);
                CurrentProgram.Data = new SymbolTable(enclosing.Data);
            }
            CurrentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;
        }

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            if(programsStack != null) programsStack.Pop();
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
            foreach(var d in data) GetScope(d).Add(section, d);
        }

        private SymbolTable GetScope(DataDescriptionEntry data) {
            if (data.IsExternal) return TableOfExternals;
            if (data.IsGlobal) return TableOfGlobals;
            return CurrentProgram.Data;
        }

        private void Attach(CodeElement e, ParserRuleContext context) {
            if (Branch.Count > 0)
                Branch.Peek().Add(e);
            var node = e as Node;
            if (node != null)
                Branch.Push(node);
            Dispatcher.OnCodeElement(e, context, CurrentProgram);
        }
        private void Detach() {
            Branch.Pop();
        }

        public override void EnterSection(CobolProgramClassParser.SectionContext context) {
            Attach(new Section(), context);
        }
        public override void ExitSection(CobolProgramClassParser.SectionContext context) {
            Detach();
        }

        public override void EnterParagraph(CobolProgramClassParser.ParagraphContext context) {
            Attach(new Paragraph(), context);
        }
        public override void ExitParagraph(CobolProgramClassParser.ParagraphContext context) {
            Detach();
        }

        public override void EnterSentence(CobolProgramClassParser.SentenceContext context) {
            Attach(new Sentence(), context);
        }
        public override void ExitSentence(CobolProgramClassParser.SentenceContext context) {
            Detach();
        }

        public override void EnterStatement(CobolProgramClassParser.StatementContext context) {
            CodeElement statement = null;
            statement = AsStatement(context);
            if (statement != null) {
                Attach(statement, context);
            }
        }

        private CodeElement AsStatement(Antlr4.Runtime.Tree.ITerminalNode node) {
            return node != null? (CodeElement)node.Symbol : null;
        }

        private CodeElement AsStatement(CobolProgramClassParser.StatementContext context)
        {
            return
                (CodeElement)AsStatement(context.ContinueStatement()) ??
/* TODO
	| evaluateStatementExplicitScope
	| ifStatementExplicitScope
	| searchStatementExplicitScope
 */
// -- arithmetic --
                (CodeElement)AsStatement(context.AddStatement()) ??
                (CodeElement)AsStatement(context.ComputeStatement()) ??
                (CodeElement)AsStatement(context.DivideStatement()) ??
                (CodeElement)AsStatement(context.MultiplyStatement()) ??
                (CodeElement)AsStatement(context.SubtractStatement()) ??
/* TODO
	| addStatementExplicitScope
	| computeStatementExplicitScope
	| divideStatementExplicitScope
	| multiplyStatementExplicitScope
	| subtractStatementExplicitScope
 */

// -- data movement --
                (CodeElement)AsStatement(context.AcceptStatement()) ?? // (DATE, DAY, DAY-OF-WEEK, TIME)
                (CodeElement)AsStatement(context.InitializeStatement()) ??
                (CodeElement)AsStatement(context.InspectStatement()) ??
                (CodeElement)AsStatement(context.MoveStatement()) ??
                (CodeElement)AsStatement(context.SetStatement()) ?? // "table-handling" too
                (CodeElement)AsStatement(context.StringStatement()) ??
                (CodeElement)AsStatement(context.UnstringStatement()) ??
                (CodeElement)AsStatement(context.XmlGenerateStatement()) ??
                (CodeElement)AsStatement(context.XmlParseStatement()) ??
/* TODO
	| stringStatementExplicitScope
	| unstringStatementExplicitScope
	| xmlGenerateStatementExplicitScope
	| xmlParseStatementExplicitScope
 */
// -- ending --
                (CodeElement)AsStatement(context.StopStatement()) ?? // RUN
                (CodeElement)AsStatement(context.ExitMethodStatement()) ??
                (CodeElement)AsStatement(context.ExitProgramStatement()) ??
                (CodeElement)AsStatement(context.GobackStatement()) ??
// -- input-output --
//              (CodeElement)AsStatement(context.AcceptStatement()) ?? // identifier
                (CodeElement)AsStatement(context.CloseStatement()) ??
                (CodeElement)AsStatement(context.DeleteStatement()) ??
                (CodeElement)AsStatement(context.DisplayStatement()) ??
                (CodeElement)AsStatement(context.OpenStatement()) ??
                (CodeElement)AsStatement(context.ReadStatement()) ??
                (CodeElement)AsStatement(context.RewriteStatement()) ??
                (CodeElement)AsStatement(context.StartStatement()) ??
//              (CodeElement)AsStatement(context.StopStatement()) ?? // literal
                (CodeElement)AsStatement(context.WriteStatement()) ??
/* TODO
	| deleteStatementExplicitScope
	| readStatementExplicitScope
	| rewriteStatementExplicitScope
	| startStatementExplicitScope
	| writeStatementExplicitScope
 */
// -- ordering --
                (CodeElement)AsStatement(context.MergeStatement()) ??
                (CodeElement)AsStatement(context.ReleaseStatement()) ??
                (CodeElement)AsStatement(context.ReturnStatement()) ??
                (CodeElement)AsStatement(context.SortStatement()) ??
/* TODO
	| returnStatementExplicitScope
 */
// -- procedure-branching --
                (CodeElement)AsStatement(context.AlterStatement()) ??
                (CodeElement)AsStatement(context.ExitStatement()) ??
                (CodeElement)AsStatement(context.GotoStatement()) ??
                (CodeElement)AsStatement(context.PerformProcedureStatement()) ??
/* TODO
	| performStatementWithBody
 */
// -- program or method linkage --
                (CodeElement)AsStatement(context.CallStatement()) ??
                (CodeElement)AsStatement(context.CancelStatement()) ??
                (CodeElement)AsStatement(context.InvokeStatement()) ??
/* TODO
	| callStatementExplicitScope
	| invokeStatementExplicitScope
 */
                (CodeElement)AsStatement(context.ExecStatement()) ??
                null;
        }
    }
}
