using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CodeModel
{
	public class SymbolTable {
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
		public Dictionary<string,List<DataDescriptionEntry>> DataEntries = new Dictionary<string,List<DataDescriptionEntry>>(System.StringComparer.InvariantCultureIgnoreCase);

		public Scope CurrentScope { get; internal set; }
		public SymbolTable EnclosingScope { get; internal set; }

		public SymbolTable(SymbolTable enclosing = null, Scope current = Scope.Program) {
			CurrentScope = current;
			EnclosingScope = enclosing;
			if (EnclosingScope == null && CurrentScope != Scope.Intrinsic)
				throw new System.InvalidOperationException("Only Table of INTRISIC symbols don't have any enclosing scope.");
		}

		public void Add(DataDescriptionEntry symbol) {
//			if (symbol.Name == null) return; // fillers and uncomplete ones don't have any name to be referenced by in the symbol table
//			Get(symbol).Add(symbol);
//			foreach(var sub in symbol.Subordinates) Add(sub);
		}

		private Scope GetScope(DataDescriptionEntry data) {
//			if (data.IsGlobal) return Scope.Global;
			//External is not a global or above global scope, so use current scope for this kind of data
			return CurrentScope;
		}
		private SymbolTable GetTable(SymbolTable.Scope scope) {
			if (CurrentScope == scope) return this;
			SymbolTable table = this.EnclosingScope;
			while(table != null) {
				if (table.CurrentScope == scope) return table;
				table = table.EnclosingScope;
			}
			return null;
		}

		public List<DataDescriptionEntry> Get(DataDescriptionEntry data) {
//			string key = data.Name.Name;
//			var table = GetTable(GetScope(data)).DataEntries;
//			if (!table.ContainsKey(key))
//				table[key] = new List<DataDescriptionEntry>();
//			return table[key];
			return null;
		}

		internal IList<DataDescriptionEntry> Get(QualifiedName name) {
			var found = Get(name.Head);
			int max = name.Count-1;
			if (name.IsExplicit) {
				for(int c=0; c<max; c++) {
					string pname = name[max-c-1];
					found = Filter(found, pname, c+1);
				}
			} else {
				var matches = new List<DataDescriptionEntry>();
				foreach(var entry in found) {
					int c=0, generation=0;
					bool okay = true;
					while (okay && c<max) {
						string pname = name[max-c-1];
						c++;
						generation++;
						okay = Filter(entry, pname, ref generation);
					}
					if (okay) matches.Add(entry);
				}
				found = matches;
			}
			return found;
		}
		/// <summary>
		/// Attempts to find in a data description entry top level items a parent with a specific name.
		/// The <see cref="generation"/> parameter indicates a "generation" where to begin the search:
		/// <code>1</code> for <code>entry.TopLevel</code>, <code>2</code> for <code>entry.TopLevel.TopLevel</code> and so on.
		/// If the appropriate item name is not found at the given <see cref="generation"/>, the method will run
		/// up the tree until it finds:
		///  - either a <code>null</code> top level item: in that case, the method returns <code>false</code>
		///  - either an appropriately named top level item: in that case, the method returns <code>true</code>
		///    and increases <see cref="generation"/> to the "generation" of the approprietaly named top level item.
		/// </summary>
		/// <param name="entry">Data description entry being searched</param>
		/// <param name="pname">Top level item name being searched for</param>
		/// <param name="generation">"Generation" where to begin the search</param>
		/// <returns><code>true</code> if an appropriately named top level item was found.</returns>
		private bool Filter(DataDescriptionEntry entry, string pname, ref int generation) {
//			var parent = entry.GetAncestor(generation);
//			while(parent != null) {
//				if (parent.Name.Name.Equals(pname)) return true;
//				parent = parent.TopLevel;
//				generation++;
//			}
			return false;
		}
		/// <summary>
		/// Filters out of a list of data descriptions entries all elements
		/// with parent element named differently than what is expected.
		/// </summary>
		/// <param name="values">List of entries to filter</param>
		/// <param name="pname">Expected parent name</param>
		/// <param name="generation">"Generation" of the parent name (1 for TopLevel, 2 for TopLevel.TopLevel and so on)</param>
		/// <returns>Filtered list</returns>
		private IList<DataDescriptionEntry> Filter(IList<DataDescriptionEntry> values, string pname, int generation) {
			var filtered = new List<DataDescriptionEntry>();
			foreach(var entry in values) {
//				var parent = entry.GetAncestor(generation);
//				if (parent == null) continue;
//				if (parent.Name.Name.Equals(pname)) filtered.Add(entry);
			}
			return filtered;
		}

		public IList<DataDescriptionEntry> Get(string key) {
			var values = new List<DataDescriptionEntry>();
			if (DataEntries.ContainsKey(key))
				values.AddRange(DataEntries[key]);
			if (EnclosingScope!= null)
				values.AddRange(EnclosingScope.Get(key));
			return values;
		}

		/// <summary>
		/// Cobol has compile time binding for variables,
		/// sometimes called static scope.
		/// Within that, Cobol supports several layers of scope:
		/// Global and Program scope.
		/// 
		/// In TypeCobol we have an intrisic scope which is used for intrinsic types and
		/// variables.
		/// </summary>
		public enum Scope {
			/// <summary>
			/// Intrinsic scope is a specific to TypeCobol.
			/// </summary>
			Intrinsic,
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
// [TYPECOBOL]
			Function,
// [/TYPECOBOL]
		}



		/// <summary>Functions repository</summary>
		protected Dictionary<QualifiedName,IList<Function>> functions = new Dictionary<QualifiedName,IList<Function>>();

		public IList<Function> Functions {
			get {
				var copy = new List<Function>();
				foreach(var funs in functions.Values)
					foreach(var fun in funs)
						copy.Add(fun);
				return copy;
			}
		}

		internal IList<Function> GetFunction(QualifiedName name, Profile profile = null, bool searchInDefaultLib = true) {
			foreach(var function in functions)
				if (function.Key.Matches(name))
					return Filter(function.Value, profile);
			if (searchInDefaultLib && CurrentScope == Scope.Intrinsic)
				return GetFunction(new URI("TC-DEFAULT."+name.ToString()), profile, false);
			if (EnclosingScope == null) return new List<Function>();
			return EnclosingScope.GetFunction(name, profile, searchInDefaultLib);
		}
		private IList<Function> Filter(IList<Function> functions, Profile profile) {
			if (profile == null) return functions;
			var filtered = new List<Function>();
			foreach(var function in functions)
				if (profile.Equals(function.Profile))
					filtered.Add(function);
			return filtered;
		}
		/// <summary>Make a function definied in the current scope.</summary>
		/// <param name="function">Function definition</param>
		internal void Register(Function function) {
			IList<Function> functions = new List<Function>();
			try { functions = this.functions[function.QualifiedName]; }
			catch(KeyNotFoundException ex) { this.functions[function.QualifiedName] = functions; }
			functions.Add(function);
		}



		/// <summary>Custom types defined in the current scope and usable in this table of symbols.</summary>
		protected Dictionary<string,TypeDescription> types = new Dictionary<string,TypeDescription>(System.StringComparer.InvariantCultureIgnoreCase);

		public IEnumerable<TypeDescription> CustomTypes {
			get { return new List<TypeDescription>(types.Values); }
		}

		/// <summary>Register a data description as a custom type.</summary>
		/// <param name="data">A TYPEDEF data description</param>
		public void RegisterCustomType(TypeDescription data) {
			var name = ((Named)data.CodeElement).Name;
			types[name] = data;
		}

		public TypeDescription GetCustomType(string type) {
			SymbolTable table = this;
			while (table != null) {
				try { return table.types[type]; }
				catch(KeyNotFoundException ex) { } // should be in parent scope
				table = table.EnclosingScope;
			}
			throw new System.ArgumentException(type+" is not a custom type for this scope");
		}



		public override string ToString() {
			var str = new StringBuilder();
			bool verbose = true;
			if (verbose) str.AppendLine("--- "+scope2str());
			foreach(var line in DataEntries) {
				var key = line.Key;
				foreach (var data in line.Value) {
					str.Append(key+":");
					Dump(str, data, 1);
					str.Append('\n');
				}
			}
			foreach(var funs in functions) {
				str.Append(funs.Key+":");
				foreach(var fun in funs.Value) {
					Dump(str, fun, 1);
					str.Append("; ");
				}
				if (funs.Value.Count > 0) str.Length -= 2;
				str.AppendLine();
			}
			if (verbose) {
				if (EnclosingScope != null)
					str.Append(EnclosingScope.ToString());
			}// else no enclosing scope dump
			return str.ToString();
		}
		private static StringBuilder Dump(StringBuilder str, DataDescriptionEntry data, int indent = 0) {
			for (int c=0; c<indent; c++) str.Append("  ");
			str.Append(data);
			return str;
		}
		private static StringBuilder Dump(StringBuilder str, Function fun, int indent = 0) {
			for (int c=0; c<indent; c++) str.Append("  ");
			str.Append(fun);
			return str;
		}
		private string scope2str() {
			var str = new StringBuilder();
			var current = this;
			while(current != null) {
				str.Insert(0,current.CurrentScope+":");
				current = current.EnclosingScope;
			}
			str.Length -= 1;
			return str.ToString();
		}
	}
}
