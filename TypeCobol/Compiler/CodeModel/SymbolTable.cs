using JetBrains.Annotations;
using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;
using System.Linq.Expressions;

namespace TypeCobol.Compiler.CodeModel
{

    public class SymbolTable
    {
        public Scope CurrentScope { get; internal set; }
        public SymbolTable EnclosingScope { get; internal set; }

        public SymbolTable(SymbolTable enclosing, Scope current)
        {
            CurrentScope = current;
            EnclosingScope = enclosing;
            if (EnclosingScope == null && CurrentScope != Scope.Intrinsic)
                throw new InvalidOperationException("Only Table of INTRINSIC symbols don't have any enclosing scope.");
        }

        private List<T> GetFromTableAndEnclosing<T>(string head,
            Func<SymbolTable, IDictionary<string, List<T>>> getTableFunction, SymbolTable symbolTable = null) where T : Node
        {
            symbolTable = symbolTable ?? this;
            var table = getTableFunction.Invoke(symbolTable);
            var values = GetFromTable(head, table);
            if (EnclosingScope != null)
            {
                values.AddRange(EnclosingScope.GetFromTableAndEnclosing(head, getTableFunction));
            }
            return values;
        }

        private List<T> GetFromTable<T>(string head, IDictionary<string, List<T>> table) where T : Node
        {
            if (head != null)
            {
                List<T> values;
                table.TryGetValue(head, out values);
                if (values != null) return values.ToList();
            }
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
            int index = redefinesNode.Parent.IndexOf(redefinesNode);

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
            return GetVariableExplicit(name);
        }

        private IList<DataDefinition> GetVariable(string name)
        {
            //Try to et variable in the current program
            var found = GetFromTableAndEnclosing(name, GetDataDefinitionTable);
           
            return found;
        }

        public List<DataDefinition> GetVariables(Expression<Func<DataDefinition, bool>> predicate, List<Scope> scopes)
        {
            var foundedVariables = new List<DataDefinition>();
            scopes.Insert(0, this.CurrentScope); //Insert the current scope 

            foreach (var scope in scopes)
            {
                if (scope == Scope.Namespace || scope == Scope.Intrinsic)
                    throw new NotSupportedException();

                var dataToSeek = this.GetTableFromScope(scope).DataEntries.Values.SelectMany(t => t);
                var results = dataToSeek.AsQueryable().Where(predicate);
                foundedVariables.AddRange(results);
            }

            return foundedVariables.Distinct().ToList(); //Distinct on object not on variable name
        }


        private IDictionary<string, List<DataDefinition>> GetDataDefinitionTable(SymbolTable symbolTable)
        {
            return symbolTable.DataEntries;
        }

        public List<DataDefinition> GetVariableExplicit(QualifiedName name)
        {
            var found = new List<DataDefinition>();
            var candidates = GetCustomTypesSubordinatesNamed(name.Head); //Get variable name declared into typedef declaration
            candidates.AddRange(GetVariable(name.Head)); //Get all variables that corresponds to the given head of QualifiedName
            

            foreach (var candidate in candidates) {
                //if name doesn't match then name.Head match one property inside the DataDefinition
                if (!name.Head.Equals(candidate.Name, StringComparison.InvariantCultureIgnoreCase)) {

                    //we're with an Index. 
                    if (candidate.IsTableOccurence) {
                        //Index can't be qualified name.Count must be == 1
                        //But that's a job to checker to check that
                        TypeDefinition parentTypeDef = candidate.GetParentTypeDefinition;
                        if (parentTypeDef != null) {
                            //If index is inside a Type, then add all variables which used this type as found
                            AddAllReference(found, candidate, parentTypeDef);
                        } else {
                            //If we are on a variable, add it
                            found.Add(candidate);
                        }
                        break;
                    }
                    throw new NotImplementedException();
                    
                }
                MatchVariable(found, candidate, name, name.Count-1, candidate);
            }
            
            return found;
        }

        public void MatchVariable(IList<DataDefinition> found, DataDefinition heaDataDefinition, QualifiedName name,
            int nameIndex, DataDefinition currentDataDefinition) {


            var currentTypeDef = currentDataDefinition as TypeDefinition;

            //Name match ?
            if (currentTypeDef == null && //Do not try to match a TYPEDEF name
                name[nameIndex].Equals(currentDataDefinition.Name, StringComparison.InvariantCultureIgnoreCase)) {

                nameIndex--;
                if (nameIndex < 0) { //We reached the end of the name : it's a complete match

                    var parentTypeDef = currentDataDefinition.GetParentTypeDefinition;
                    if (parentTypeDef != null) { //We are under a TypeDefinition
                        //For each variable declared with this type (or a type that use this type), we need to add the headDataDefinition
                        AddAllReference(found, heaDataDefinition, parentTypeDef);
                    } else { //we are on a variable
                        found.Add(heaDataDefinition);
                    }

                    //End here
                    return;
                } 

                //else it's not the end of name, let's continue with next part of QualifiedName
            }


            //Either we have a match or not, we need to continue to the parent or DataDefinition that use this TypeDefinition
            var parent = currentDataDefinition.Parent as DataDefinition;
            if (parent != null)
            {
                MatchVariable(found, heaDataDefinition, name, nameIndex, parent);
                return;
            }

            
            if (currentTypeDef != null)
            {
                foreach (var reference in currentTypeDef.References)
                {
                    //references property of a TypeDefinition can lead to variable in totally others scopes, like in another program
                    //So we need to check if we can access this variable
                    if (reference.IsPartOfATypeDef || GetVariable(reference.Name).Contains(reference))
                        MatchVariable(found, heaDataDefinition, name, nameIndex, reference);
                }
                return;
            }


            //If we reach here, it means we are on a DataDefinition with no parent
            //==> End of treatment, there is no match
        }


        /// <summary>
        /// For all usage of this type by a variable (outside a TypeDefinition), add heaDataDefinition to found
        /// 
        /// 
        /// Technical note: this method should be declared under MatchVariable because there is no use for it outside.
        /// </summary>
        /// <param name="found"></param>
        /// <param name="heaDataDefinition"></param>
        /// <param name="currentDataDefinition"></param>
        private void AddAllReference(IList<DataDefinition> found, DataDefinition heaDataDefinition, [NotNull] TypeDefinition currentDataDefinition) {

            foreach (var reference in currentDataDefinition.References) {
                var parentTypeDef = reference.GetParentTypeDefinition;
                if (parentTypeDef != null) {
                    AddAllReference(found, heaDataDefinition, parentTypeDef);
                } else { 
                    //we are on a variable but ... references property of a TypeDefinition can lead to variable in totally others scopes, like in another program
                    //So we need to check if we can access this variable
                    if (GetVariable(reference.Name).Contains(reference)) {
                        found.Add(heaDataDefinition);
                    }
                }
            }
        }

        /// <summary>Get all items with a specific name that are subordinates of a custom type</summary>
        /// <param name="name">Name of items we search for</param>
        /// <returns>Direct or indirect subordinates of a custom type</returns>
        private List<DataDefinition> GetCustomTypesSubordinatesNamed(string name)
        {
            var subs = new List<DataDefinition>();

            //Get programs from Namespace table
            var programList = this.GetProgramsTable(GetTableFromScope(Scope.Namespace));
            foreach (var programs in programList) {

                //Get Custom Types from program 
                foreach (var pgm in programs.Value) { //we shouldn't have more than one program with the same name
                                                      //but just in case it changes 
                    seekSymbolTable(pgm.SymbolTable, name, subs);
                }
            }


            //Get Custom Types from Intrinsic table 
            var intrinsicTable = this.GetTableFromScope(Scope.Intrinsic);
            foreach (var type in intrinsicTable.Types)
            {
                foreach (var type2 in type.Value)
                {
                    subs.AddRange(type2.GetChildren<DataDefinition>(name, true));
                }
            }

            return subs;
        }

        private void seekSymbolTable(SymbolTable symbolTable, string name, List<DataDefinition> datadefinitions)
        {
            var currSymbolTable = symbolTable;
            //Don't search into Intrinsic table because it's shared between all programs
            while (currSymbolTable != null && currSymbolTable.CurrentScope != Scope.Intrinsic)
            {
                foreach (var type in currSymbolTable.Types)
                {
                    foreach (var type2 in type.Value)
                    {
                        datadefinitions.AddRange(type2.GetChildren<DataDefinition>(name, true));
                    }
                }
                currSymbolTable = currSymbolTable.EnclosingScope;
            }
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

        /// <summary>
        /// Get all paragraphs in the current scope.
        /// </summary>
        /// <returns>The collection of paragraph names</returns>
        public List<Paragraph> GetParagraphs(Expression<Func<Paragraph, bool>> predicate)
        {
            return Paragraphs.Values.SelectMany(p => p).AsQueryable().Where(predicate).Distinct().ToList();
        }

        #endregion

        #region TYPES

        public IDictionary<string, List<TypeDefinition>> Types =
            new Dictionary<string, List<TypeDefinition>>(StringComparer.InvariantCultureIgnoreCase);

        public void AddType(TypeDefinition type)
        {
            Add(Types, type);
        }

        public IList<TypeDefinition> GetType(ITypedNode symbol)
        {
            return GetType(symbol.DataType);
        }

        public List<TypeDefinition> GetType(SymbolReference symbolReference)
        {
            return GetType(symbolReference.URI);
        }

      

        public List<TypeDefinition> GetType(DataType dataType, string pgmName = null)
        {
            var uri = new URI(dataType.Name);
            var types = GetType(uri);
            if (types.Count > 0)
                return types; //If Types found return it

            if (!string.IsNullOrEmpty(pgmName)) //If Program is specified seek type into given program
                types = GetType(uri, pgmName); //Try to find the types in specific program

            return types;
        }

        /// <summary>
        /// Get type for the current program, then check in other program by using QualifiedName Tail propertie
        /// </summary>
        /// <param name="name">Qualified name of the wated type</param>
        /// <returns></returns>
        [NotNull]
        public List<TypeDefinition> GetType(QualifiedName name)
        {
            var found = GetType(name.Head);

            if (string.IsNullOrEmpty(name.Tail) || found.Any(f => string.Compare(f.QualifiedName.Tail, name.Tail, StringComparison.InvariantCultureIgnoreCase) == 0))
                return Get(found, name);

            found = GetType(name, name.Tail, found); //Pass name.Tail as a program name 

            return found;
        }

        public List<TypeDefinition> GetTypes(Expression<Func<TypeDefinition, bool>> predicate, List<Scope> scopes)
        {
            var foundedTypes = new List<TypeDefinition>();
            
            foreach (var scope in scopes)
            {
                var dataToSeek = this.GetTableFromScope(scope).Types.Values.SelectMany(t => t);
                if (scope == Scope.Namespace)
                {
                    //For namespace scope, we need to browse every program
                    dataToSeek = this.GetTableFromScope(scope)
                                    .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Declarations)
                                    .Types.Values.SelectMany(t => t));

                    dataToSeek.Concat(this.GetTableFromScope(scope)
                                    .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Global)
                                    .Types.Values.SelectMany(t => t)));
                }

                var results = dataToSeek.AsQueryable().Where(predicate);

                if (scope == Scope.Intrinsic || scope == Scope.Namespace)
                    results = results.Where(tp => (tp.CodeElement as DataTypeDescriptionEntry) != null && (tp.CodeElement as DataTypeDescriptionEntry).Visibility == AccessModifier.Public);

                foundedTypes.AddRange(results);
            }

            return foundedTypes.Distinct().ToList();
        }



        /// <summary>
        /// Get type into a specific program by giving program name
        /// </summary>
        /// <param name="name">Qualified name of the wated type</param>
        /// <param name="pgmName">Name of the program tha tmay contains the type</param>
        /// <returns></returns>
        public List<TypeDefinition> GetType(QualifiedName name, string pgmName, List<TypeDefinition> found = null)
        {
            found = found ?? new List<TypeDefinition>();
            var program = GetProgramHelper(pgmName); //Get the program corresponding to the given namespace
            if (program != null)
            {
                //Get all TYPEDEF PUBLIC from this program
                var programTypes = GetPublicTypes(program.SymbolTable.GetTableFromScope(Scope.Declarations).Types);

                found = GetFromTable(name.Head, programTypes); //Check if there is a type that correspond to the given name (head)

                //Get all GLOBAL TYPEDEF PUBLIC from this program
                var globalTypedef = GetPublicTypes(program.SymbolTable.GetTableFromScope(Scope.Global).Types);

                found.AddRange(GetFromTable(name.Head, globalTypedef)); //Check if there is a type that correspond to the given name (head)
            }

            return found;
        }

        /// <summary>
        /// Get all Public TYPEDEF from the specified SymbolTable
        /// </summary>
        /// <param name="programTypes"></param>
        /// <returns></returns>
        private static Dictionary<string, List<TypeDefinition>> GetPublicTypes(IDictionary<string, List<TypeDefinition>> programTypes) {
            return programTypes
                .Where(p =>
                    p.Value.All(f => ((DataTypeDescriptionEntry) f.CodeElement).Visibility == AccessModifier.Public)) 
                .ToDictionary(f => f.Key, f => f.Value, StringComparer.InvariantCultureIgnoreCase); //Sort types to get only the ones with public AccessModifier
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


        public List<FunctionDeclaration> GetFunctions(Expression<Func<FunctionDeclaration, bool>> predicate, List<Scope> scopes)
        {
            var foundedFunctions = new List<FunctionDeclaration>();

            foreach (var scope in scopes)
            {
                var dataToSeek = this.GetTableFromScope(scope).Functions.Values.SelectMany(t => t);
                if (scope == Scope.Namespace)
                {
                    //For namespace scope, we need to browse every program
                    dataToSeek = this.GetTableFromScope(scope)
                                    .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Declarations)
                                    .Functions.Values.SelectMany(t => t));
                }

                var results = dataToSeek.AsQueryable().Where(predicate);

                if (scope == Scope.Intrinsic || scope == Scope.Namespace)
                    results = results.Where(tp => (tp.CodeElement as FunctionDeclarationHeader) != null && (tp.CodeElement as FunctionDeclarationHeader).Visibility == AccessModifier.Public);

                foundedFunctions.AddRange(results);
            }

            return foundedFunctions.Distinct().ToList();
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
                    if (Matches(function.Profile, profile, function.Library))
                        filtered.Add(function);
                }
                found = filtered;
            }
            return found;
        }

        private bool Matches(ParameterList p1, ParameterList p2, string pgmName)
        {
            if (p1.InputParameters.Count != p2.InputParameters.Count) return false;
            if (p1.InoutParameters.Count != p2.InoutParameters.Count) return false;
            if (p1.OutputParameters.Count != p2.OutputParameters.Count) return false;

            for (int c = 0; c < p1.InputParameters.Count; c++)
                if (!TypeCompare(p1.InputParameters[c], p2.InputParameters[c], pgmName)) return false;
            for (int c = 0; c < p1.InoutParameters.Count; c++)
                if (!TypeCompare(p1.InoutParameters[c], p2.InoutParameters[c], pgmName)) return false;
            for (int c = 0; c < p1.OutputParameters.Count; c++)
                if (!TypeCompare(p1.OutputParameters[c], p2.OutputParameters[c], pgmName)) return false;
            return true;
        }
        /// <summary>
        /// Type comparaison
        /// </summary>
        /// <param name="p1">Represent the DataType from the called function/procedure</param>
        /// <param name="p2">Represent the DataType from the caller function/procedure</param>
        /// <param name="pgmName">Corresponds to the progam containing the called function/procedure</param>
        /// <returns></returns>
        private bool TypeCompare(DataType p1, DataType p2, string pgmName)
        {
            var p1Types = p1.Name.Contains(".") ? this.GetType(p1) : this.GetType(p1, pgmName);
            var p2Types = this.GetType(p2);

            if (p1Types.Count > 1 || p2Types.Count > 1)
                return false; //Means that a type is declare many times. Case already handle by checker.
            var p1Type = p1Types.FirstOrDefault();
            var p2Type = p2Types.FirstOrDefault();

            if (p1Type != p2Type)
                return false;

            return true;
        }

        [NotNull]
        private List<FunctionDeclaration> GetFunction(string head, string nameSpace)
        {
            var result = GetFromTableAndEnclosing(head, GetFunctionTable);

            if (string.IsNullOrEmpty(nameSpace) || result.Any(f => string.Compare(f.QualifiedName.Tail, nameSpace, StringComparison.InvariantCultureIgnoreCase) == 0))
                return result;

            var program = GetProgramHelper(nameSpace); //Get the program corresponding to the given namespace
            if(program != null)
            {
                var programFunctions = program.SymbolTable.GetTableFromScope(Scope.Declarations).Functions; //Get all function from this program
                programFunctions = programFunctions
                                    .Where(p =>
                                            p.Value.All(f => ((FunctionDeclarationHeader) f.CodeElement).Visibility == AccessModifier.Public))
                                            .ToDictionary(f => f.Key, f => f.Value, StringComparer.InvariantCultureIgnoreCase); //Sort functions to get only the one with public AccessModifier

                result = GetFromTable(head, programFunctions); //Check if there is a function that correspond to the given name (head)
            }

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

        public List<Program> GetPrograms(string filter)
        {
            var programs = this.GetTableFromScope(Scope.Namespace)
                .Programs.Values.SelectMany(t => t).Where(fd => fd.Name.StartsWith(filter, StringComparison.InvariantCultureIgnoreCase));
            return programs.ToList();
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
            /// Variables and TYPEDEF declared in DATA DIVISION as GLOBAL are visible
            /// to the entire program in which they are declared and
            /// in all nested subprograms contained in that program.
            /// </summary>
            Global,

            /// <summary>
            /// Declaration of PROCEDURES/FUNCTIONS and TYPEDEF (not marked as Global).
            /// </summary>
            Declarations,

            /// <summary>
            /// Contains variables declared in DATA DIVISION.
            /// 
            /// Variables declared in WORKING STORAGE are visible
            /// to the entire program in which they are declared.
            /// Variables declared in LOCAL STORAGE are visible
            /// to the entire program in which they are declared,
            /// but are deleted and reinitialized on every invocation.
            /// An infinite number of programs can be contained within a program,
            /// and the variables of each are visible only within the scope
            /// of that individual program.
            /// </summary>
            Program,
            // [TYPECOBOL]
            Function,
            // [/TYPECOBOL]
        }

        private Program GetProgramHelper(string nameSpace)
        {
            var programs = GetProgram(nameSpace);
  
            if (programs.Count > 1)
                throw new Exception(string.Format("Program with identifier {0} is defined multiple times.", programs.FirstOrDefault().Name));

            return programs.FirstOrDefault();
        }



        public override string ToString()
        {
            return this.ToString(false);
        }

        public string ToString(bool verbose)
        {
            var str = new StringBuilder();
            if (verbose && (DataEntries.Count > 0 || Types.Count > 0))
                str.AppendLine("--- " + scope2str());
            if (DataEntries.Count > 0)
            {
                str.AppendLine("-- DATA --------");
                foreach (var line in DataEntries)
                foreach (var item in line.Value)
                    Dump(str, item, new string(' ', 2));
            }
            if (Sections.Count > 0)
            {
                str.AppendLine("-- SECTIONS ----");
                foreach (var line in Sections)
                foreach (var item in line.Value)
                    Dump(str, item, new string(' ', 2));
            }
            if (Paragraphs.Count > 0)
            {
                str.AppendLine("-- PARAGRAPHS --");
                foreach (var line in Paragraphs)
                foreach (var item in line.Value)
                    Dump(str, item, new string(' ', 2));
            }
            if (Types.Count > 0)
            {
                str.AppendLine("-- TYPES -------");
                foreach (var line in Types)
                foreach (var item in line.Value)
                    Dump(str, item, new string(' ', 2));
            }
            if (Functions.Count > 0)
            {
                str.AppendLine("-- FUNCTIONS ---");
                foreach (var line in Functions)
                foreach (var item in line.Value)
                    Dump(str, item, new string(' ', 2));
            }
            if (verbose && EnclosingScope != null)
                str.Append(EnclosingScope.ToString(verbose));
            return str.ToString().TrimEnd(Environment.NewLine.ToCharArray());
        }

        private static StringBuilder Dump(StringBuilder str, Node symbol, string indent = null)
        {
            str.Append(indent);
            str.Append(symbol.Name);
            if (symbol is ITypedNode) str.Append(':').Append(((ITypedNode) symbol).DataType);
            var fun = symbol as FunctionDeclaration;
            if (fun != null)
            {
                if (fun.Profile.ReturningParameter != null || fun.Profile.Parameters.Count > 0) str.AppendLine();
                foreach (var p in fun.Profile.InputParameters)
                {
                    str.Append("        in: ");
                    Dump(str, p);
                }
                foreach (var p in fun.Profile.OutputParameters)
                {
                    str.Append("       out: ");
                    Dump(str, p);
                }
                foreach (var p in fun.Profile.InoutParameters)
                {
                    str.Append("     inout: ");
                    Dump(str, p);
                }
                if (fun.Profile.ReturningParameter != null)
                {
                    str.Append("    return: ");
                    Dump(str, fun.Profile.ReturningParameter);
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

        /// <summary>
        /// Helper to add all the DataEntries from a SymbolTable to the current one. 
        /// </summary>
        /// <param name="DataEntries"></param>
        public void CopyAllDataEntries(ICollection<List<DataDefinition>> DataEntries)
        {
            foreach (var values in DataEntries)
                foreach (var data in values)
                    this.AddVariable(data);
        }

        /// <summary>
        /// Helper to add all the Types from a SymbolTable to the current one.
        /// </summary>
        /// <param name="Types"></param>
        public void CopyAllTypes(IDictionary<string, List<TypeDefinition>> Types)
        {
            foreach (var types in Types)
                foreach (var type in types.Value)
                    this.AddType(type);
        }

        /// <summary>
        /// Helper to add Functions from a SymbolTable to the curret one, depending on the given access modifier
        /// </summary>
        /// <param name="Functions">Functions to add</param>
        /// <param name="accessModifier">AccessModifier is nullable. If null, all functions will be added otherwise only functions with the specified AccessModifier will be added</param>
        public void CopyAllFunctions(IDictionary<string, List<FunctionDeclaration>> Functions, AccessModifier? accessModifier = null)
        {
            foreach (var functions in Functions)
                foreach (var function in functions.Value)
                {
                    if (accessModifier != null && ((FunctionDeclarationHeader)function.CodeElement).Visibility == accessModifier)
                        this.AddFunction(function); //Add function depending on the specified AccessModifier
                    else if(accessModifier == null)
                        this.AddFunction(function); //If no AccessModifier given, add all the functions
                }
        }

        /// <summary>
        /// Helper to add Programs from a SymbolTable to the current one
        /// </summary>
        /// <param name="Programs">Programs to add</param>
        public void CopyAllPrograms(ICollection<List<Program>> Programs)
        {
            foreach (var values in Programs)
                foreach (var program in values)
                    this.AddProgram(program);
        }

        /// <summary>
        /// Helper to get the SymbolTable with a given scope. 
        /// </summary>
        /// <param name="scope"></param>
        /// <returns></returns>
        public SymbolTable GetTableFromScope(Scope scope)
        {
            SymbolTable tableToReturn = this;
            while (tableToReturn != null && tableToReturn.CurrentScope != scope)
                tableToReturn = tableToReturn.EnclosingScope;

            return tableToReturn;
        }



        #endregion
    }
}
