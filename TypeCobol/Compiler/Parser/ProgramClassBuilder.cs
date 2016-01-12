using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;

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

        private SymbolTable TableOfExternals = new SymbolTable(null, SymbolTable.Scope.External);
        private SymbolTable TableOfGlobals;

        /// <summary>
        /// Class object resulting of the visit the parse tree
        /// </summary>
        public Class Class { get; private set; }

        /// <summary>
        /// List of syntax diagnostics gathered while transversing the parse tree
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }

        /// <summary>
        /// Initialization code run before parsing each new Program or Class
        /// </summary>
        public override void EnterCobolCompilationUnit(CobolProgramClassParser.CobolCompilationUnitContext context)
        {
            TableOfGlobals = new SymbolTable(TableOfExternals, SymbolTable.Scope.Global);
            Program = null;
            Class = null;
            Diagnostics = new List<Diagnostic>();
        }

        public override void EnterCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            if (Program == null) {
                Program = new SourceProgram();
                programsStack = new Stack<Program>();
                CurrentProgram = Program;
            } else {
                CurrentProgram = new NestedProgram(CurrentProgram);
            }
            CurrentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;
        }

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            if(programsStack != null) programsStack.Pop();
        }

        public override void EnterWorkingStorageSection(CobolProgramClassParser.WorkingStorageSectionContext context) {
            if (CurrentProgram.Data == null) CreateSymbolsTable();
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Working);
        }

        public override void EnterLocalStorageSection(CobolProgramClassParser.LocalStorageSectionContext context) {
            if (CurrentProgram.Data == null) CreateSymbolsTable();
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Local);
        }

        public override void EnterLinkageSection(CobolProgramClassParser.LinkageSectionContext context) {
            if (CurrentProgram.Data == null) CreateSymbolsTable();
            UpdateSymbolsTable(CreateDataDescriptionEntries(context.DataDescriptionEntry()), SymbolTable.Section.Linkage);
        }

        private void CreateSymbolsTable() {
            NestedProgram nested = CurrentProgram as NestedProgram;
            if (nested == null) CurrentProgram.Data = new SymbolTable(TableOfGlobals);
            else CurrentProgram.Data = new SymbolTable(nested.ContainingProgram.Data);
        }

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
    }
}
