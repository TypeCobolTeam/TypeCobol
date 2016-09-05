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
		public Dictionary<string,List<Named>> DataEntries = new Dictionary<string,List<Named>>(System.StringComparer.InvariantCultureIgnoreCase);

		public Scope CurrentScope { get; internal set; }
		public SymbolTable EnclosingScope { get; internal set; }

		public SymbolTable(SymbolTable enclosing = null, Scope current = Scope.Program) {
			CurrentScope = current;
			EnclosingScope = enclosing;
			if (EnclosingScope == null && CurrentScope != Scope.Intrinsic)
				throw new System.InvalidOperationException("Only Table of INTRISIC symbols don't have any enclosing scope.");
		}





		  //////////////////
		 // DATA SYMBOLS //
		//////////////////

		internal void AddVariable(Named symbol) { Add(DataEntries, symbol); }

		private void Add(Dictionary<string,List<Named>> table, Named symbol) {
			 // TODO: generate a name for FILLERs and anonymous data to be referenced by in the symbol table
			if (symbol.Name == null) return;
			List<Named> found;
			if (table == DataEntries)
				 found = GetVariable(symbol.QualifiedName);
			else found = GetType(symbol.QualifiedName);
			if (found.Count == 0) {
				List<Named> samenamesymbols = null;
				try { samenamesymbols = table[symbol.QualifiedName.Head]; }
				catch (KeyNotFoundException ex) {
					samenamesymbols = new List<Named>();
					table.Add(symbol.QualifiedName.Head, samenamesymbols);
				}
				found = samenamesymbols;
			}
			found.Add(symbol);
		}

		internal List<Named> GetVariable(QualifiedName name) {
			var found = GetVariable(name.Head);
			return Get(found, name);
		}

		private List<Named> GetVariable(string name) {
			var values = new List<Named>();
			if (DataEntries.ContainsKey(name))
				values.AddRange(DataEntries[name]);
			if (EnclosingScope!= null)
				values.AddRange(EnclosingScope.GetVariable(name));
			return values;
		}

		private List<Named> Get(List<Named> found, QualifiedName name) {
			if (found.Count < 1) return found;
			int max = name.Count-1;
			if (name.IsExplicit) {
				for(int c=0; c<max; c++) {
					string pname = name[max-c-1];
					found = Filter(found, pname, c+1);
					if (found.Count < 1) return found;
				}
			} else {
				var matches = new List<Named>();
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
		private bool Filter(Named symbol, string pname, ref int generation) {
			var parent = GetAncestor(symbol, generation);
			while(parent != null) {
				if (parent.Name.Equals(pname)) return true;
				parent = parent.Parent;
				generation++;
			}
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
		private List<Named> Filter(IList<Named> values, string pname, int generation) {
			var filtered = new List<Named>();
			foreach(var symbol in values) {
				var parent = GetAncestor(symbol, generation);
				if (parent == null) continue;
				if (parent.Name.Equals(pname)) filtered.Add(symbol);
			}
			return filtered;
		}
		private Node GetAncestor(Named symbol, int generation) {
			if (symbol is Node)
				return GetAncestor((Node)symbol, generation);
			return null;
		}
        /// <param name="generation">0 for node, 1 for node.Parent, 2 for node.Parent.Parent and so on.</param>
        /// <returns>Appropriate Parent item, or null if generation <0 or generation too high.</returns>
        private Node GetAncestor(Node node, int generation) {
            if (generation < 0) return null;
            if (generation == 0) return node;
            if (node.Parent == null) return null;
            return GetAncestor(node.Parent, generation-1);
        }





		  ///////////
		 // TYPES //
		///////////

		public Dictionary<string,List<Named>> Types = new Dictionary<string,List<Named>>(System.StringComparer.InvariantCultureIgnoreCase);

		internal void AddType(TypeDefinition type) { Add(Types, type); }

		public List<TypeDefinition> GetTypes(Typed symbol) {
			var types = new List<TypeDefinition>();
			var list = GetType(symbol.DataType.Name);
			foreach(var type in list) types.Add((TypeDefinition)type);
			return types;
		}

		internal List<Named> GetType(QualifiedName name) {
			var found = GetType(name.Head);
			return Get(found, name);
		}

		private List<Named> GetType(string name) {
			var values = new List<Named>();
			if (Types.ContainsKey(name))
				values.AddRange(Types[name]);
			if (EnclosingScope!= null)
				values.AddRange(EnclosingScope.GetType(name));
			return values;
		}

		  ///////////////
		 // FUNCTIONS //
		///////////////

		public Dictionary<string,List<Named>> Functions = new Dictionary<string,List<Named>>(System.StringComparer.InvariantCultureIgnoreCase);

		internal void AddFunction(FunctionDeclaration function) { Add(Functions, function); }

		internal List<Named> GetFunction(QualifiedName name) {
			var found = GetFunction(name.Head);
			return Get(found, name);
		}

		private List<Named> GetFunction(string name) {
			var values = new List<Named>();
			if (Functions.ContainsKey(name))
				values.AddRange(Functions[name]);
			if (EnclosingScope!= null)
				values.AddRange(EnclosingScope.GetFunction(name));
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
/*
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
*/


		/// <summary>Custom types defined in the current scope and usable in this table of symbols.</summary>
		protected Dictionary<string,TypeDefinition> types = new Dictionary<string,TypeDefinition>(System.StringComparer.InvariantCultureIgnoreCase);

		public IEnumerable<TypeDefinition> CustomTypes {
			get { return new List<TypeDefinition>(types.Values); }
		}

//TODO#249? remove
		/// <summary>Register a data description as a custom type.</summary>
		/// <param name="data">A TYPEDEF data description</param>
		public void RegisterCustomType(TypeDefinition data) {
			var name = ((Named)data.CodeElement).Name;
			types[name] = data;
		}
//TODO#249? remove
		public TypeDefinition GetCustomType(string type) {
			SymbolTable table = this;
			while (table != null) {
				try { return table.types[type]; }
				catch(KeyNotFoundException ex) { } // should be in parent scope
				table = table.EnclosingScope;
			}
			throw new System.ArgumentException(type+" is not a custom type for this scope");
		}



		public override string ToString() { return this.ToString(false); }
		public          string ToString(bool verbose, int indent = 1) {
			var str = new StringBuilder();
			if (verbose && (DataEntries.Count > 0 || Types.Count > 0))
				str.AppendLine("--- "+scope2str());
			if (DataEntries.Count > 0) {
				str.AppendLine("-- DATA --------");
				foreach(var line in DataEntries)
					foreach(var item in line.Value)
						Dump(str, item, indent);
			}
			if (Types.Count > 0) {
				str.AppendLine("-- TYPES -------");
				foreach(var line in Types)
					foreach(var item in line.Value)
						Dump(str, item, indent);
			}
			if (Functions.Count > 0) {
				str.AppendLine("-- FUNCTIONS ---");
				foreach(var line in Functions)
					foreach(var item in line.Value)
						Dump(str, item, indent);
			}
			if (verbose && EnclosingScope != null)
				str.Append(EnclosingScope.ToString(verbose, indent+1));
			return str.ToString().TrimEnd(System.Environment.NewLine.ToCharArray());;
		}
		private static StringBuilder Dump(StringBuilder str, Named symbol, int indent = 0) {
			for (int c=0; c<indent; c++) str.Append("  ");
			str.Append(symbol.Name);
			if (symbol is Typed) str.Append(':').Append(((Typed)symbol).DataType);
			return str.AppendLine();
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
