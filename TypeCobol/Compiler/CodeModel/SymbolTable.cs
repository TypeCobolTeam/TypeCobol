using JetBrains.Annotations;
using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Castle.Core.Internal;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using String = System.String;

namespace TypeCobol.Compiler.CodeModel
{

    public class SymbolTable
    {
        public Scope CurrentScope { get; internal set; }
        public SymbolTable EnclosingScope { get; internal set; }

        public SymbolTable(SymbolTable enclosing = null, Scope current = Scope.Program)
        {
            CurrentScope = current;
            EnclosingScope = enclosing;
            if (EnclosingScope == null && CurrentScope != Scope.Intrinsic)
                throw new InvalidOperationException("Only Table of INTRINSIC symbols don't have any enclosing scope.");
        }

        private List<T> GetFromTableAndEnclosing<T>(string head,
            Func<SymbolTable, IDictionary<string, List<T>>> getTableFunction) where T : Node
        {
            var table = getTableFunction.Invoke(this);
            var values = GetFromTable(head, table);
            if (EnclosingScope != null)
            {
                values.AddRange(EnclosingScope.GetFromTableAndEnclosing(head, getTableFunction));
            }
            return values;
        }

        private List<T> GetFromTable<T>(string head, IDictionary<string, List<T>> table) where T : Node
        {
            List<T> values;
            table.TryGetValue(head, out values);
            if (values != null) return values.ToList();
            return new List<T>();
        }

        #region DATA SYMBOLS

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
        public IDictionary<string, List<DataDefinition>> DataEntries =
            new Dictionary<string, List<DataDefinition>>(StringComparer.InvariantCultureIgnoreCase);

        internal void AddVariable([NotNull] DataDefinition symbol)
        {
            // TODO: generate a name for FILLERs and anonymous data to be referenced by in the symbol table
            if (symbol.Name == null)
            {
                return;
            }
            Add(DataEntries, symbol);
        }

        internal void AddVariable(string name, DataDefinition data)
        {
            string key = name;
            List<DataDefinition> found;
            bool present = DataEntries.TryGetValue(key, out found);
            if (!present)
            {
                found = new List<DataDefinition>();
                DataEntries.Add(key, found);
            }
            found.Add(data);
        }

        private void Add<T>([NotNull] IDictionary<string, List<T>> table, [NotNull] T symbol) where T : Node
        {
            string key = symbol.QualifiedName.Head;
            List<T> found;
            bool present = table.TryGetValue(key, out found);
            if (!present)
            {
                found = new List<T>();
                table.Add(key, found);
            }
            found.Add(symbol);
        }

        public List<DataDefinition> GetVariable(VariableBase variable)
        {
            if (variable.StorageArea != null)
            {
                return GetVariable(variable.StorageArea);
            }
            return GetVariable(new URI(variable.ToString()));
        }

        public List<DataDefinition> GetVariable(StorageArea storageArea)
        {
            URI uri;
            if (storageArea.SymbolReference != null)
            {
                uri = storageArea.SymbolReference.URI;
            }
            else
            {
                uri = new URI(storageArea.ToString());
            }
            return GetVariable(uri);
        }

        public List<DataDefinition> GetVariable(SymbolReference symbolReference)
        {
            return GetVariable(symbolReference.URI);
        }

        public DataDefinition GetRedefinedVariable(DataRedefines redefinesNode, SymbolReference symbolReference)
        {
            var childrens = redefinesNode.Parent.Children;
            int index =
                childrens.Select((child, position) => new {child, position})
                    .First(c => c.child.CodeElement == redefinesNode.CodeElement)
                    .position - 1;
            bool redefinedVariableFound = false;

            while (!redefinedVariableFound && index >= 0)
            {
                CommonDataDescriptionAndDataRedefines child = childrens[index].CodeElement as DataDescriptionEntry ??
                                                              (CommonDataDescriptionAndDataRedefines)
                                                              (childrens[index].CodeElement as DataRedefinesEntry);

                if (child != null && (child is DataDescriptionEntry || child is DataRedefinesEntry))
                {
                    if (child.DataName != null &&
                        string.Equals(child.DataName.Name, symbolReference.Name,
                            StringComparison.InvariantCultureIgnoreCase))
                        return childrens[index] as DataDefinition;
                    else if (child.DataName != null && child is DataDescriptionEntry &&
                             !string.Equals(child.DataName.Name, symbolReference.Name,
                                 StringComparison.InvariantCultureIgnoreCase))
                        return null;
                }
                else
                    return null;

                index--;
            }
            return null;
        }

        public List<DataDefinition> GetVariable(QualifiedName name)
        {
            return new List<DataDefinition>(GetVariableExplicit(name).Keys.Cast<DataDefinition>());
        }

        private IList<DataDefinition> GetVariable(string name)
        {
            return GetFromTableAndEnclosing(name, GetDataDefinitionTable);
        }

        private IDictionary<string, List<DataDefinition>> GetDataDefinitionTable(SymbolTable symbolTable)
        {
            return symbolTable.DataEntries;
        }

        public Dictionary<Node, List<LinkedList<Node>>> GetVariableExplicit(QualifiedName name)
        {
            var candidates = new List<Node>();
            if (name.Count > 1) candidates.AddRange(GetCustomTypesSubordinatesNamed(name.Head));
            candidates.AddRange(GetVariable(name.Head));
            //TODO candidates.AddRange(GetFunction(name.Head));

            var map = new Dictionary<Node, List<LinkedList<Node>>>();
            foreach (var candidate in candidates)
            {
                var link = new LinkedList<Node>();
                link.AddFirst(new LinkedListNode<Node>(candidate));
                map.Add(candidate, new List<LinkedList<Node>> {link});
            }

            for (int i = name.Count - 2; i >= 0; --i)
            {
                var winners = new Dictionary<Node, List<LinkedList<Node>>>();
                foreach (var original in map.Keys)
                {
                    // for each original node
                    var toplevels = new List<LinkedList<Node>>();
                    foreach (var link in map[original])
                    {
                        var node = link.First.Value;
                        var ancestors = GetTopLevel(node, name[i]);
                        foreach (var ancestor in ancestors) MergeLink(ancestor, link);
                        toplevels.AddRange(ancestors);
                    }
                    toplevels = new List<LinkedList<Node>>(new HashSet<LinkedList<Node>>(toplevels));
                        // remove duplicates
                    if (toplevels.Count > 0) winners.Add(original, toplevels);
                }
                map.Clear();
                if (winners.Count < 1) break; // early exit
                foreach (var winner in winners) map.Add(winner.Key, winner.Value);
            }

            foreach (var winner in map)
            {
                if (winner.Value.Count != 1)
                {
                    var str =
                        new StringBuilder().Append(winner.Key.QualifiedName).Append(" expected:1-sized list, got: [");
                    foreach (var v in winner.Value) str.Append(' ').Append(ToString(v)).Append(',');
                    if (winner.Value.Count > 0) str.Length -= 1;
                    throw new NotImplementedException(str.Append(" ]").ToString());
                }
            }

            return map;
        }

        /// <summary>Merges second LinkedList with first LinkedList.
        /// If last items in first LinkedList are equal to first items in second LinkedList, these common items are not duplicated.
        /// </summary>
        /// <param name="begin"></param>
        /// <param name="end"></param>
        private void MergeLink(LinkedList<Node> first, LinkedList<Node> second)
        {
            foreach (var item in second) if (item != first.Last.Value) first.AddLast(item);
        }

        public static string ToString(IEnumerable<Node> names)
        {
            var str = new System.Text.StringBuilder().Append('[');
            foreach (var name in names) str.Append(' ').Append(name.Name).Append(',');
            if (str.Length > 1) str.Length -= 1;
            return str.Append(' ').Append(']').ToString();
        }

        /// <summary>Gets direct or indirect toplevel item for a node.</summary>
        /// <param name="node">We want the toplevel item for this node</param>
        /// <param name="name">We want a toplevel item named like that</param>
        /// <returns>List of LinkedLists of items ; last item of each LinkedList is always node</returns>
        private List<LinkedList<Node>> GetTopLevel(Node node, string name)
        {
            var toplevel = new List<LinkedList<Node>>();
            if (node.Parent == null) return toplevel;
            var typedef = node.Parent as TypeDefinition;
            if (typedef != null)
            {
                var vars = GetVariablesTyped(typedef.QualifiedName);
                var typs = GetCustomTypesSubordinatesTyped(typedef.QualifiedName);
                foreach (var item in vars)
                {
                    if (name.Equals(item.Name, System.StringComparison.InvariantCultureIgnoreCase))
                    {
                        var link = CreateLinkedList(node);
                        link.AddFirst(item);
                        toplevel.Add(link);
                    }
                }
                foreach (var item in typs)
                {
                    if (name.Equals(item.Name, System.StringComparison.InvariantCultureIgnoreCase))
                    {
                        var link = CreateLinkedList(node);
                        link.AddFirst(item);
                        toplevel.Add(link);
                    }
                }
                return toplevel;
            }
            if (name.Equals(node.Parent.Name))
            {
                var link = CreateLinkedList(node);
                link.AddFirst(node.Parent);
                toplevel.Add(link);
            }
            // as name can be implicit, don't stop at first properly named parent encountered
            var other = GetTopLevel(node.Parent, name);
            foreach (var o in other) o.AddLast(node);
            toplevel.AddRange(other);
            return toplevel;
        }

        private LinkedList<N> CreateLinkedList<N>(N item)
        {
            var link = new LinkedList<N>();
            link.AddFirst(new LinkedListNode<N>(item));
            return link;
        }

        /// <summary>Get all items with a specific name that are subordinates of a custom type</summary>
        /// <param name="name">Name of items we search for</param>
        /// <returns>Direct or indirect subordinates of a custom type</returns>
        private List<Node> GetCustomTypesSubordinatesNamed(string name)
        {
            var types = new List<Node>();
            var scope = this;
            while (scope != null)
            {
                foreach (var type in scope.Types) types.AddRange(type.Value);
                scope = scope.EnclosingScope;
            }
            var subs = new List<Node>();
            foreach (var type in types) subs.AddRange(type.GetChildren(name, true));
            return subs;
        }

        /// <summary>Get all items of a specific type that are subordinates of a custom type</summary>
        /// <param name="name">Type name we search for</param>
        /// <returns>Direct or indirect subordinates of a custom type</returns>
        private List<Node> GetCustomTypesSubordinatesTyped(QualifiedName typename)
        {
            var types = new List<Node>();
            var scope = this;
            while (scope != null)
            {
                foreach (var type in scope.Types) types.AddRange(type.Value);
                scope = scope.EnclosingScope;
            }
            var subs = new List<Node>();
            foreach (var type in types)
            {
                subs.AddRange(GetChildrenOfDataType(type, typename, true));
            }
            return subs;
        }

        /// <summary>Searches all children of a given node that are of a given type</summary>
        /// <param name="node">We search among this Node's children</param>
        /// <param name="typename">Name of the type we want</param>
        /// <param name="deep">True for deep search, false for shallow search</param>
        /// <returns>All children of a given type</returns>
        private List<Node> GetChildrenOfDataType(Node node, QualifiedName typename, bool deep)
        {
            var results = new List<Node>();
            foreach (var child in node.Children)
            {
                var typed = child as ITypedNode;
                if (typed != null)
                {
                    if (typename.Head.Equals(typed.DataType.Name, System.StringComparison.InvariantCultureIgnoreCase))
                        results.Add(child);
                }
                if (deep) results.AddRange(GetChildrenOfDataType(child, typename, true));
            }
            return results;
        }

        /// <summary>Gets all data items of a specific type, accross all scopes.</summary>
        /// <param name="typename">Name of type we search for</param>
        /// <returns>All data items of type typename</returns>
        private IList<DataDefinition> GetVariablesTyped(QualifiedName typename)
        {
            var variables = new List<DataDefinition>();
            foreach (var items in DataEntries.Values)
            {
                foreach (var item in items)
                {
                    if (typename.Head.Equals(item.DataType.Name, System.StringComparison.InvariantCultureIgnoreCase))
                        variables.Add(item);
                }
            }
            if (EnclosingScope != null)
                variables.AddRange(EnclosingScope.GetVariablesTyped(typename));
            return variables;
        }


        private List<T> Get<T>(List<T> found, QualifiedName name) where T : Node
        {
            if (found.Count < 1) return found;
            int max = name.Count - 1;
            if (name.IsExplicit)
            {
                for (int c = 0; c < max; c++)
                {
                    string pname = name[max - c - 1];
                    found = Filter(found, pname, c + 1);
                    if (found.Count < 1) return found;
                }
            }
            else
            {
                var matches = new List<T>();
                foreach (var candidate in found)
                {
                    if (candidate == null) continue;
                    if (Match(candidate.QualifiedName, name)) matches.Add(candidate);
                }
                found = matches;
            }
            return found;
        }

        private bool Match(QualifiedName name1, QualifiedName name2)
        {
            int offset = 0;
            for (int c = 0; c < name1.Count; c++)
            {
                string part1 = name1[c];
                string part2 = name2[offset];
                if (part1.Equals(part2, StringComparison.InvariantCultureIgnoreCase)) offset++;
                else if (name1.IsExplicit) return false;
                if (offset == name2.Count) return true;
            }
            return offset == name2.Count;
        }



        /// <summary>
        /// Filters out of a list of data descriptions entries all elements
        /// with parent element named differently than what is expected.
        /// </summary>
        /// <param name="values">List of entries to filter</param>
        /// <param name="pname">Expected parent name</param>
        /// <param name="generation">"Generation" of the parent name (1 for TopLevel, 2 for TopLevel.TopLevel and so on)</param>
        /// <returns>Filtered list</returns>
        private List<T> Filter<T>(IList<T> values, string pname, int generation) where T : Node
        {
            var filtered = new List<T>();
            foreach (var symbol in values)
            {
                var parent = GetAncestor(symbol, generation);
                if (parent == null) continue;
                if (parent.Name.Equals(pname, StringComparison.InvariantCultureIgnoreCase)) filtered.Add(symbol);
            }
            return filtered;
        }

        /// <param name="generation">0 for node, 1 for node.Parent, 2 for node.Parent.Parent and so on.</param>
        /// <returns>Appropriate Parent item, or null if generation <0 or generation too high.</returns>
        private Node GetAncestor(Node node, int generation)
        {
            if (generation < 0) return null;
            if (generation == 0) return node;
            if (node.Parent == null) return null;
            return GetAncestor(node.Parent, generation - 1);
        }

        #endregion

        #region SECTIONS

        private IDictionary<string, List<Section>> Sections =
            new Dictionary<string, List<Section>>(StringComparer.InvariantCultureIgnoreCase);

        internal void AddSection(Section section)
        {
            Add(Sections, section);
        }

        public IList<Section> GetSection(string name)
        {
            return GetFromTableAndEnclosing(name, GetSectionTable);
        }

        private IDictionary<string, List<Section>> GetSectionTable(SymbolTable symbolTable)
        {
            return symbolTable.Sections;
        }

        #endregion

        #region PARAGRAPHS

        private IDictionary<string, List<Paragraph>> Paragraphs =
            new Dictionary<string, List<Paragraph>>(StringComparer.InvariantCultureIgnoreCase);

        internal void AddParagraph(Paragraph paragraph)
        {
            Add(Paragraphs, paragraph);
        }

        public IList<Paragraph> GetParagraph(string name)
        {
            return GetFromTableAndEnclosing(name, GetParagraphTable);
        }

        private IDictionary<string, List<Paragraph>> GetParagraphTable(SymbolTable symbolTable)
        {
            return symbolTable.Paragraphs;
        }

        #endregion

        #region TYPES

        public IDictionary<string, List<TypeDefinition>> Types =
            new Dictionary<string, List<TypeDefinition>>(StringComparer.InvariantCultureIgnoreCase);

        public void AddType(TypeDefinition type)
        {
            Add(Types, type);
        }

        public IList<TypeDefinition> GetTypes(ITypedNode symbol)
        {
            var types = new List<TypeDefinition>();
            var list = GetType(symbol.DataType.Name);
            foreach (var type in list) types.Add(type);
            return types;
        }

        public List<TypeDefinition> GetType(SymbolReference symbolReference)
        {
            return GetType(symbolReference.URI);
        }

        public List<TypeDefinition> GetType(DataType dataType)
        {
            var uri = new URI(dataType.Name);
            return GetType(uri);
        }

        public List<TypeDefinition> GetType(QualifiedName name)
        {
            var found = GetType(name.Head);
            return Get(found, name);
        }

        private List<TypeDefinition> GetType(string name)
        {
            return GetFromTableAndEnclosing(name, GetTypeTable);
        }

        private IDictionary<string, List<TypeDefinition>> GetTypeTable(SymbolTable symbolTable)
        {
            return symbolTable.Types;
        }

        #endregion

        #region FUNCTIONS

        public IDictionary<string, List<FunctionDeclaration>> Functions =
            new Dictionary<string, List<FunctionDeclaration>>(StringComparer.InvariantCultureIgnoreCase);

        public void AddFunction(FunctionDeclaration function)
        {
            Add(Functions, function);
        }

        public List<FunctionDeclaration> GetFunction(StorageArea storageArea, ParameterList profile = null)
        {
            return GetFunction(storageArea.SymbolReference, profile);
        }

        public List<FunctionDeclaration> GetFunction(VariableBase variable, ParameterList profile = null)
        {
            return GetFunction(new URI(variable.ToString()), profile);
        }

        public List<FunctionDeclaration> GetFunction(SymbolReference symbolReference, ParameterList profile = null)
        {
            var uri = new URI(symbolReference.Name);
            return GetFunction(uri, profile);
        }

        public List<FunctionDeclaration> GetFunction(QualifiedName name, ParameterList profile = null, string nameSpace = null)
        {
            var found = GetFunction(name.Head, nameSpace);
            found = Get(found, name);
            if (profile != null)
            {
                var filtered = new List<FunctionDeclaration>();
                foreach (var function in found)
                {
                    if (Matches(function.Profile, profile))
                        filtered.Add(function);
                }
                found = filtered;
            }
            return found;
        }

        private bool Matches(ParameterList p1, ParameterList p2)
        {
            //		if (p1.ReturningParameter == null && p2.ReturningParameter != null) return false;
            //		if (p1.ReturningParameter != null && p2.ReturningParameter == null) return false;
            //		if (p1.ReturningParameter != p2.ReturningParameter) return false;
            if (p1.InputParameters.Count != p2.InputParameters.Count) return false;
            if (p1.InoutParameters.Count != p2.InoutParameters.Count) return false;
            if (p1.OutputParameters.Count != p2.OutputParameters.Count) return false;

            for (int c = 0; c < p1.InputParameters.Count; c++)
                if (p1.InputParameters[c] != p2.InputParameters[c]) return false;
            for (int c = 0; c < p1.InoutParameters.Count; c++)
                if (p1.InoutParameters[c] != p2.InoutParameters[c]) return false;
            for (int c = 0; c < p1.OutputParameters.Count; c++)
                if (p1.OutputParameters[c] != p2.OutputParameters[c]) return false;
            return true;
        }

        [NotNull]
        private List<FunctionDeclaration> GetFunction(string head, string nameSpace)
        {
            List<FunctionDeclaration> result;
            result = GetFromTableAndEnclosing(head, GetFunctionTable);
            if (result != null && (result.Count != 0 || nameSpace.IsNullOrEmpty())) return result;

            var programs = GetProgram(nameSpace); //If no results found and Namespace != null, then search program in the given namespace
            if (programs.Count == 0)
                return result;

            var programFunctions = programs.FirstOrDefault().CurrentTable.Functions; //Get the first program (will change with the real use of namespace) and functions
            var publicFunctions =
                programFunctions.Where(p => p.Value.All(f => (f.CodeElement as FunctionDeclarationHeader).Visibility == AccessModifier.Public)).ToDictionary(f => f.Key, f => f.Value); //Sort functions to get only the one with public AccessModifier

            result = GetFromTable(head, publicFunctions); //Check if there is a function that correspond to the given name (head)

            return result;
        }


        private IDictionary<string, List<FunctionDeclaration>> GetFunctionTable(SymbolTable symbolTable)
        {
            return symbolTable.Functions;
        }

       

        #endregion

        #region PROGRAMS

        public IDictionary<string, List<Program>> Programs =
            new Dictionary<string, List<Program>>(StringComparer.InvariantCultureIgnoreCase);

        public void AddProgram(Program program)
        {
            Add(Programs, program);
        }

        public List<Program> GetProgram(StorageArea storageArea, ParameterList profile = null)
        {
            return GetProgram(storageArea.SymbolReference, profile);
        }

        public List<Program> GetProgram(VariableBase variable, ParameterList profile = null)
        {
            return GetProgram(new URI(variable.ToString()), profile);
        }

        public List<Program> GetProgram(SymbolReference symbolReference, ParameterList profile = null)
        {
            var uri = new URI(symbolReference.Name);
            return GetProgram(uri, profile);
        }

        public List<Program> GetProgram(QualifiedName name, ParameterList profile = null)
        {
            var found = GetProgram(name.Head);
            found = Get(found, name);

            return found;
        }

        [NotNull]
        private List<Program> GetProgram(string name)
        {
            return GetFromTableAndEnclosing(name, GetProgramsTable);
        }

        private IDictionary<string, List<Program>> GetProgramsTable(SymbolTable symbolTable)
        {
            return symbolTable.Programs;
        }

       

        #endregion


        #region Helpers

        /// <summary>
        /// Cobol has compile time binding for variables, sometimes called static scope.
        /// Within that, Cobol supports several layers of scope: Global and Program scope.
        ///
        /// TypeCobol has Intrisic scope used for standard library types and variables.
        /// TypeCobol has Function scope used for types and variables declared inside a function.
        /// </summary>
        public enum Scope
        {
            /// <summary>
            /// Intrinsic scope is a specific to TypeCobol.
            /// </summary>
            Intrinsic,

            /// <summary>
            /// Namespace scope is a specific to TypeCobol. It registers all the different parsed programs. 
            /// </summary>
            Namespace,

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





        public override string ToString()
        {
            return this.ToString(false);
        }

        public string ToString(bool verbose, int indent = 1)
        {
            var str = new StringBuilder();
            if (verbose && (DataEntries.Count > 0 || Types.Count > 0))
                str.AppendLine("--- " + scope2str());
            if (DataEntries.Count > 0)
            {
                str.AppendLine("-- DATA --------");
                foreach (var line in DataEntries)
                foreach (var item in line.Value)
                    Dump(str, item, indent);
            }
            if (Sections.Count > 0)
            {
                str.AppendLine("-- SECTIONS ----");
                foreach (var line in Sections)
                foreach (var item in line.Value)
                    Dump(str, item, indent);
            }
            if (Paragraphs.Count > 0)
            {
                str.AppendLine("-- PARAGRAPHS --");
                foreach (var line in Paragraphs)
                foreach (var item in line.Value)
                    Dump(str, item, indent);
            }
            if (Types.Count > 0)
            {
                str.AppendLine("-- TYPES -------");
                foreach (var line in Types)
                foreach (var item in line.Value)
                    Dump(str, item, indent);
            }
            if (Functions.Count > 0)
            {
                str.AppendLine("-- FUNCTIONS ---");
                foreach (var line in Functions)
                foreach (var item in line.Value)
                    Dump(str, item, indent);
            }
            if (verbose && EnclosingScope != null)
                str.Append(EnclosingScope.ToString(verbose, indent + 1));
            return str.ToString().TrimEnd(Environment.NewLine.ToCharArray());
            ;
        }

        private static StringBuilder Dump(StringBuilder str, Node symbol, int indent = 0)
        {
            for (int c = 0; c < indent; c++) str.Append("  ");
            str.Append(symbol.Name);
            if (symbol is ITypedNode) str.Append(':').Append(((ITypedNode) symbol).DataType);
            var fun = symbol as FunctionDeclaration;
            if (fun != null)
            {
                if (fun.Profile.ReturningParameter != null || fun.Profile.Parameters.Count > 0) str.AppendLine();
                foreach (var p in fun.CodeElement().Profile.InputParameters)
                {
                    str.Append("        in: ");
                    Dump(str, new ParameterDescription(p), 0);
                }
                foreach (var p in fun.CodeElement().Profile.OutputParameters)
                {
                    str.Append("       out: ");
                    Dump(str, new ParameterDescription(p), 0);
                }
                foreach (var p in fun.CodeElement().Profile.InoutParameters)
                {
                    str.Append("     inout: ");
                    Dump(str, new ParameterDescription(p), 0);
                }
                if (fun.Profile.ReturningParameter != null)
                {
                    str.Append("    return: ");
                    Dump(str, new ParameterDescription(fun.Profile.ReturningParameter), 0);
                }
                if (fun.Profile.ReturningParameter == null && fun.Profile.Parameters.Count == 0) str.AppendLine();
            }
            else str.AppendLine();
            return str;
        }

        private string scope2str()
        {
            var str = new StringBuilder();
            var current = this;
            while (current != null)
            {
                str.Insert(0, current.CurrentScope + ":");
                current = current.EnclosingScope;
            }
            str.Length -= 1;
            return str.ToString();
        }

        #endregion
    }
}
