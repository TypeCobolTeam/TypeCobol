using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    public class SymbolTable
    {
        // TODO: should have one map/list per data set.

        /// <summary>
        /// The WORKING-STORAGE SECTION describes data records that are not part
        /// of data files but are developed and processed by a program or method.
        /// The WORKING-STORAGE SECTION also describes data items whose values
        /// are assigned in the source program or method and do not change
        /// during execution of the object program.
        /// The WORKING-STORAGE SECTION for programs (and methods) can also
        /// describe external data records, which are shared by programs
        /// and methods throughout the run unit.
        ///
        /// The LOCAL-STORAGE SECTION defines storage that is allocated
        /// and freed on a per-invocation basis. On each invocation,
        /// data items defined in the LOCAL-STORAGE SECTION are reallocated.
        /// Each data item that has a VALUE clause is initialized to the value
        /// specified in that clause.
        /// For nested programs, data items defined in the LOCAL-STORAGE SECTION
        /// are allocated upon each invocation of the containing outermost program.
        /// However, each data item is reinitialized to the value specified
        /// in its VALUE clause each time the nested program is invoked.
        ///
        /// The LINKAGE SECTION describes data made available from another
        /// program or method.
        /// Record description entries and data item description entries in the
        /// LINKAGE SECTION provide names and descriptions, but storage within
        /// the program or method is not reserved because the data area exists elsewhere.
        /// Data items defined in the LINKAGE SECTION of the called program or invoked
        /// method can be referenced within the PROCEDURE DIVISION of that program if
        /// and only if they satisfy one of the conditions as listed in the topic.
        /// - They are operands of the USING phrase of the PROCEDURE DIVISION header
        ///   or the ENTRY statement.
        /// - They are operands of SET ADDRESS OF, CALL ... BY REFERENCE ADDRESS
        ///   OF, or INVOKE ... BY REFERENCE ADDRESS OF.
        /// - They are defined with a REDEFINES or RENAMES clause, the object of which
        ///   satisfies the above conditions.
        /// - They are items subordinate to any item that satisfies the condition in the rules
        ///   above.
        /// - They are condition-names or index-names associated with data items that satisfy
        ///   any of the above conditions.
        /// </summary>
        public Dictionary<string,List<DataDescriptionEntry>> DataEntries = new Dictionary<string,List<DataDescriptionEntry>>();

        public Scope CurrentScope { get; internal set; }
        public SymbolTable EnclosingScope { get; internal set; }

        public SymbolTable(SymbolTable enclosing = null, Scope current = Scope.Program) {
            CurrentScope = current;
            EnclosingScope = enclosing;
            if (EnclosingScope == null && CurrentScope != Scope.External)
                throw new System.InvalidOperationException("Only Table of EXTERNAL symbols don't have any enclosing scope.");
        }

        public void Add(Section section, DataDescriptionEntry symbol) {
            if (symbol.Name == null) return; // fillers and uncomplete ones don't have any name to be referenced by in the symbol table
            var entries = Get(section, symbol.Name.Name);
            entries.Add(symbol);
            foreach(var sub in symbol.Subordinates) Add(section, sub);
        }

        public List<DataDescriptionEntry> Get(Section section, string name) {
            if (!DataEntries.ContainsKey(name))
                DataEntries[name] = new List<DataDescriptionEntry>();
            return DataEntries[name];
        }

        internal IList<DataDescriptionEntry> Get(CodeElements.Expressions.QualifiedName name) {
            IList<DataDescriptionEntry> found;
            if (name.DataNames.Count > 0) {
                IList<DataDescriptionEntry> values = Get(name.DataNames[0].Name);
                for (int c = 1; c < name.DataNames.Count; c++) {
                    values = Filter(values, name.DataNames[c].Name);
                }
                found = Filter(values, name.Symbol.Name);
            } else {
                found = Get(name.Symbol.Name);
            }
            return found;
        }

        private IList<DataDescriptionEntry> Filter(IList<DataDescriptionEntry> values, string subordinate) {
            var filtered = new List<DataDescriptionEntry>();
            foreach (var data in values) {
                foreach (var sub in data.Subordinates) {
                    if (sub.Name == null) continue;//TODO issue #179
                    if (sub.Name.Name.Equals(subordinate)) {
                        filtered.Add(data);
                        break;
                    }
                }
            }
            return filtered;
        }
        private IList<DataDescriptionEntry> Get(string key) {
            var values = new List<DataDescriptionEntry>();
            if (DataEntries.ContainsKey(key))
                values.AddRange(DataEntries[key]);
            return values;
        }

        /// <summary>
        /// Cobol has compile time binding for variables,
        /// sometimes called static scope.
        /// Within that, Cobol supports several layers of scope:
        /// External, Global and Program scope.
        /// </summary>
        public enum Scope {
            /// <summary>
            /// Variables declared as EXTERNAL are truly global.
            /// </summary>
            External,
            /// <summary>
            /// Variables declared in WORKING STORAGE as GLOBAL are visible
            /// to the entire program in which they are declared and
            /// in all nested subprograms contained in that program.
            /// </summary>
            Global,
            /// <summary>
            /// Variables declared in WORKING STORAGE are visible
            /// to the entire program in which they are declared.
            /// Variables declared in LOCAL STORAGE are visible
            /// to the entire program in which they are declared,
            /// but are deleted and reinitialized on every invocation.
            /// An infinite number of programs can be contained within a program,
            /// and the variables of each are visible only within the scope
            /// of that individual program.
            /// Cobol does not distinguish between programs and functions/procedures.
            /// </summary>
            Program,
        }

        public enum Section {
            Working,
            Local,
            Linkage,
        }
    }
}
