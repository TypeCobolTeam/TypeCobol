using JetBrains.Annotations;
using System;
using System.Text;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;
using System.Linq.Expressions;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Concurrency;

namespace TypeCobol.Compiler.CodeModel
{
    public class SymbolTable
    {
        private static readonly IList<Section> EmptySectionList = new ImmutableList<Section>();
        private static readonly IList<Paragraph> EmptyParagraphList = new ImmutableList<Paragraph>();
        private static readonly IList<TypeDefinition> EmptyTypeDefinitionList = new ImmutableList<TypeDefinition>();

        public Scope CurrentScope { get; }
        public SymbolTable EnclosingScope { get; }

        public SymbolTable(SymbolTable enclosing, Scope current)
        {
            CurrentScope = current;
            EnclosingScope = enclosing;
            TypesReferences = new Dictionary<TypeDefinition, List<DataDefinition>>();
            if (EnclosingScope == null && CurrentScope != Scope.Intrinsic)
                throw new InvalidOperationException("Only Table of INTRINSIC symbols don't have any enclosing scope.");
        }

        private delegate void MatchDelegate<TNode>(List<TNode> found,
            in TNode head,
            in QualifiedName name,
            int nameIndex,
            in TNode current)
            where TNode : Node;

        private List<T> GetFromTableAndEnclosing<T>(QualifiedName name,
            Func<SymbolTable, IDictionary<string, List<T>>> getEntries,
            MatchDelegate<T> match,
            Scope maxScope,
            Scope? requiredScope = null)
            where T : Node
        {
            System.Diagnostics.Debug.Assert(requiredScope == null || requiredScope <= maxScope);

            var result = new List<T>();
            bool skipParentGlobal = false;

            // Iterate over scopes from current up to maxScope
            var currentSymbolTable = this;
            while (currentSymbolTable != null && currentSymbolTable.CurrentScope >= maxScope)
            {
                // Retrieve candidates from current SymbolTable
                getEntries(currentSymbolTable).TryGetValue(name.Head, out var candidates);

                // We don't have to perform matching if we already have found a result (except if current scope is required)
                if (!skipParentGlobal || currentSymbolTable.CurrentScope == requiredScope)
                {
                    // Proceed to matching
                    if (candidates != null)
                    {
                        foreach (var candidate in candidates)
                        {
                            match(result, candidate, name, name.Count - 1, candidate);
                        }
                    }

                    // Skip Global SymbolTable from parent if we already have found a match
                    skipParentGlobal = currentSymbolTable.CurrentScope == Scope.Global && result.Count > 0;
                }

                // Jump to parent SymbolTable
                currentSymbolTable = currentSymbolTable.EnclosingScope;
            }

            return result;
        }

        private static void MatchUsingName<T>(List<T> found, in T head, in QualifiedName name, int nameIndex, in T current)
            where T : Node
        {
            if (Match(head, name))
            {
                found.Add(head);
            }
        }

        [NotNull]
        private static List<T> GetFromTable<T>(string head, IDictionary<string, List<T>> table)
        {
            if (head != null)
            {
                table.TryGetValue(head, out List<T> values);
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
        public readonly IDictionary<string, List<DataDefinition>> DataEntries =
            new Dictionary<string, List<DataDefinition>>(StringComparer.OrdinalIgnoreCase);

        //Dictionary for Type data entries
        private IDictionary<string, List<DataDefinition>> DataTypeEntries =
            new Dictionary<string, List<DataDefinition>>(StringComparer.OrdinalIgnoreCase);

        internal void AddVariable([NotNull] DataDefinition symbol)
        {
            // TODO: generate a name for FILLERs and anonymous data to be referenced by in the symbol table
            if (symbol.Name == null)
            {
                return;
            }
            if (!symbol.IsPartOfATypeDef)
            {
                AddNode(DataEntries, symbol);
            }
            else
            {
                AddVariableUnderTypeDefinition(symbol);
            }
        }

        /// <summary>
        /// Add DataDefinition under a TypeDefinition in the DataTypeEntries dictionary
        /// </summary>
        /// <param name="data"></param>
        private void AddVariableUnderTypeDefinition([NotNull] DataDefinition data)
        {
            //TypeDefinition must NOT be added to DataTypeEntries, only its children
            Debug.Assert(!(data is TypeDefinition)); 

            //Add symbol to the dictionary
            AddNode(this.DataTypeEntries, data);
        }

        public IEnumerable<DataDefinition> GetVariables(SymbolReference symbolReference)
        {
            return GetVariablesExplicit(symbolReference.URI);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="predicate">Predicate to search variable(s)</param>
        /// <param name="maximalScope">The maximal symboltable scope to search in</param>
        /// <returns></returns>
        public IEnumerable<DataDefinition> GetVariables(Expression<Func<DataDefinition, bool>> predicate, Scope maximalScope)
        {
            var foundedVariables = new List<DataDefinition>();

            SymbolTable currentTable = this;
            while (currentTable != null && currentTable.CurrentScope >= maximalScope)
            {
                if (currentTable.CurrentScope == Scope.Namespace || currentTable.CurrentScope == Scope.Intrinsic)
                    throw new NotSupportedException(); //There is no variable stored in those scopes
             
                var dataToSeek = currentTable.DataEntries.Values.SelectMany(t => t);
                var results = dataToSeek.AsQueryable().Where(predicate);
                foundedVariables.AddRange(results);

                currentTable = currentTable.EnclosingScope;
            }

            return foundedVariables.Distinct(); //Distinct on object not on variable name
        }

        public List<DataDefinition> GetVariablesByType(DataType dataType, IEnumerable<DataDefinition> existingVariables, Scope maximalScope)
        {

            var foundedVariables = new List<DataDefinition>();
            if (existingVariables != null && existingVariables.Any()) foundedVariables.AddRange(existingVariables);

            SymbolTable currentTable = this;
            while (currentTable != null && currentTable.CurrentScope >= maximalScope)
            {
                if (currentTable.CurrentScope == Scope.Namespace || currentTable.CurrentScope == Scope.Intrinsic)
                    throw new NotSupportedException();


                if (dataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85)
                {
                    var references = currentTable.TypesReferences.Where(t => t.Key.DataType == dataType).SelectMany(t => t.Value);
                    foundedVariables.AddRange(references);
                }
                else
                {
                    foreach (var variable in currentTable.DataEntries.Values.SelectMany(t => t))
                    {
                        SeekVariableType(dataType, variable, ref foundedVariables);
                    }
                }

                currentTable = currentTable.EnclosingScope;

            }
            
            return foundedVariables;
        }

        private void SeekVariableType(DataType dataType, DataDefinition variable, ref List<DataDefinition> foundedVariables)
        {
            if (Regex.Match(variable.DataType.Name, @"\b" + dataType.Name + @"\b", RegexOptions.IgnoreCase).Success) //TODO: need to evolve this check with type comparison not just text..
            {
                if(!foundedVariables.Any(v => v == variable))
                    foundedVariables.Add(variable);
                return;
            }

            if (variable.DataType != null && variable.DataType != DataType.Boolean && variable.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85)
            {
                var types = GetTypes(t => t.DataType == variable.DataType, Scope.Intrinsic);

                foreach (var type in types)
                {
                    if (type.Children != null && type.Children.Count > 0)
                    {
                        foreach (var childrenType in type.Children)
                        {
                            if (childrenType is DataDefinition && childrenType.Name != null)
                            {
                                SeekVariableType(dataType, childrenType as DataDefinition, ref foundedVariables);
                            }
                        }
                    }
                }
            }


            if (variable.Children != null && variable.Children.Count > 0)
            {
                foreach (var children in variable.Children)
                {
                    if (children is DataDefinition && children.Name != null)
                    {
                        SeekVariableType(dataType, children as DataDefinition, ref foundedVariables);
                    }
                }
            }
        }

        public DataDefinition GetRedefinedVariable(DataRedefines redefinesNode, SymbolReference symbolReference)
        {
            var childrens = redefinesNode.Parent.Children;
            int index = redefinesNode.Parent.IndexOf(redefinesNode);

            while (index >= 0)
            {
                CommonDataDescriptionAndDataRedefines child = childrens[index].CodeElement as CommonDataDescriptionAndDataRedefines;

                if (child != null && (child is DataDescriptionEntry || child is DataRedefinesEntry))
                {
                    if (child.DataName != null &&
                        string.Equals(child.DataName.Name, symbolReference.Name,
                            StringComparison.OrdinalIgnoreCase))
                        return childrens[index] as DataDefinition;
                    else if (child.DataName != null && child is DataDescriptionEntry &&
                             !string.Equals(child.DataName.Name, symbolReference.Name,
                                 StringComparison.OrdinalIgnoreCase))
                        return null;
                }
                else
                    return null;

                index--;
            }
            return null;
        }

        /// <summary>
        /// Try to get variable base on the given qualifiedName. 
        /// It will search in DataDefinition or Type (using references) to know is variable is declared and initialized
        /// </summary>
        /// <param name="name">QualifiedName of the variable looked for</param>
        /// <returns></returns>
        public IEnumerable<DataDefinition> GetVariablesExplicit(QualifiedName name)
        {
            return GetVariablesExplicitWithQualifiedName(name).Select(v => v.Value); //Just Ignore CompleteQualifiedName stored as a key
        }

        /// <summary>
        /// Try to get variable base on the given qualifiedName. It could also scope onto typedef context. 
        /// </summary>
        /// <param name="name">Qualified name of the seeked variable</param>
        /// <param name="typeDefContext">Allow to force the algorithm to look only inside the given typeDefContext</param>
        /// <returns>Returns a list of Key Value with Key = Complete qualifiedName path (string) AND Value = Founded variable (DataDefinition)</returns>
        /// Example for this algorithm (USE-CASE)
        //  Regarding to the following context

        //	01 FinalVar TYPE Bool.
        //	01 MyType TYPEDEF STRICT.
        //		05 FinalVar TYPE Bool.

        //	01 MyGroup.
        //		05 TypedVar TYPE MyType.

        //    SET TypedVar::FinalVar TO TRUE.


        //The Cobol85Checker is certainly going to ask the algorithm is 'TypedVar::FinalVar' exists.
        //So the method GetVariablesExplicitWithQualifiedName will receive the following Qualified Name 'TypedVar.FinalVar'
        //The algorithm's objective is to return the potential variables found and the path to reach them. 
        //It will use boh the given QualifiedName to track a good path but also the variables and types declared into the symboltable.

        //Fist of all, it's going to find all the potential candidates that could fit the qualifiedName.Head -> in this case FinalVar. 
        //For this current situation it will get 2 possibilities.FinalVar declared inside the TypeDef and FinalVar declared as a variable.

        //Then it will loop on those candidates and try to find a path that works in adequation to the given QualifiedName 'TypedVar::FinalVar'.
        //For each candidate it will call the MatchVariable() method.

        //First step of this method is to add the currentDataDefinition.Name to the last list of string representing the complete path to reach a matched variable. 
        //After that, it's going to check if currentDateDefinition is a TypeDef or not. 

        //In this case currentTypedef is going to be null. 
        //Then nameIndex will be equals to 0 after nameIndex--; so the algorithm continues

        //The algorithm is going to check currentDataDefinition.Parent as DataDefinition
        //And rerun the MatchVariable() mechanics. 

        //This time, the currentTypeDef will not be null because our algorithm is placed at 01 MyType TYPEDEF STRICT. 
        //It is now going to enter the condition currentTypeDef != null. 
        //Inside this condition, the algorithm is going to get all the references of the currentTypeDef -> so all the references of MyType.
        //It will only find one reference which is TypedVar. 
        //Then the algorithm is going to recall it self but this time using TypedVar as currentDataDefinition.
        //NameIndex will be negative after nameIndex--;
        //The variable is not inside a typeDef, so it's obviously a terminal variable. 
        //We can add it to the found list and also finalize the completeQualifiedName.

        //After that, the algorithm will rapidly return to the first foreach done on candidates inside GetVariablesExplicitWithQualifiedName
        //And launch the same mechanic, but this time with no success, because we are going to immediately find a terminal variable. 
        public List<KeyValuePair<DataDefinitionPath, DataDefinition>> GetVariablesExplicitWithQualifiedName(QualifiedName name,
            TypeDefinition typeDefContext = null)
        {
            var foundedVariables = new List<KeyValuePair<DataDefinitionPath, DataDefinition>>();

            #region Get variables declared under Type
            //Get all variables that corresponds to the given head of QualifiedName
            bool ignoreGlobal = false;
            foreach (var candidate in GetCustomTypesSubordinatesNamed(name.Head))
            {
                DataDefinitionPath dataDefinitionPath = new DataDefinitionPath(candidate);
                if (typeDefContext != null)
                {
                    MatchVariable(foundedVariables, candidate, name, name.Count - 1, candidate, dataDefinitionPath, typeDefContext, this);
                }
                else
                {
                    var symbolTable = this;
                    while (symbolTable.CurrentScope >= Scope.Program)
                    {
                        if (!ignoreGlobal || symbolTable.CurrentScope == Scope.Program)
                        {
                            MatchVariable(foundedVariables, candidate, name, name.Count - 1, candidate, dataDefinitionPath, null, symbolTable);
                            ignoreGlobal = symbolTable.CurrentScope == Scope.Global && foundedVariables.Count > 0;
                        }

                        symbolTable = symbolTable.EnclosingScope;
                    }
                }
            }
            #endregion

            #region Get variables declared outside types
            //If we are in the context of a typedef, it will be handled by "Get variables declared under Type"
            if (typeDefContext == null)
            {
                var found = GetFromTableAndEnclosing(name, st => st.DataEntries, MatchVariableOutsideType, Scope.Program, Scope.Program);
                foreach (var dataDefinition in found)
                {
                    foundedVariables.Add(new KeyValuePair<DataDefinitionPath, DataDefinition>(null, dataDefinition));
                }
            }
            #endregion

            return foundedVariables;
        }

        /// <summary>
        /// Recursively try to find the path for the given QualifiedName name. 
        /// Algorithm allows to browse every potential path to find where the variable QualifiedName is. 
        /// It only browse DataDefinition (Var + Group) outside TypeDef. 
        /// The algorithm will search as deep as possible in every direction until the path comes to an end. 
        /// </summary>
        /// <param name="found">List of compatible variable found regarding to the given name</param>
        /// <param name="headDataDefinition">Given potential variable candidate</param>
        /// <param name="name">QualifiedName of the symbol looked for</param>
        /// <param name="nameIndex">Total count of the parts of the qualifiedName 'name' </param>
        /// <param name="currentDataDefinition">Currently checked DataDefinition</param>
        private static void MatchVariableOutsideType(List<DataDefinition> found, in DataDefinition headDataDefinition, in QualifiedName name,
            int nameIndex, in DataDefinition currentDataDefinition)
        {
            //Name match ?
            if (name[nameIndex].Equals(currentDataDefinition.Name, StringComparison.OrdinalIgnoreCase))
            {
                nameIndex--;
                if (nameIndex < 0)
                { //We reached the end of the name : it's a complete match
                    
                    //we are on a variable
                    found.Add(headDataDefinition);

                    //End here
                    return;
                }
                //else it's not the end of name, let's continue with next part of QualifiedName
            }

            //Either we have a match or not, we need to continue to the parent
            if (currentDataDefinition.Parent is DataDefinition parent)
            {
                //Go deeper to check the rest of the QualifiedName 'name'
                MatchVariableOutsideType(found, headDataDefinition, name, nameIndex, parent);
            }         

            //If we reach here, it means we are on a DataDefinition with no parent
            //==> End of treatment, there is no match
        }

        /// <summary>
        /// Recursively try to find the path for the given QualifiedName name. 
        /// Algorithm allows to browse every potential path to find where the variable QualifiedName is. 
        /// It's able to browse DataDefinition (Var + Group) but also TypeDef. 
        /// The algorithm will search as deep as possible in every direction until the path comes to an end. 
        /// </summary>
        /// <param name="found">List of compatible variable found regarding to the given name</param>
        /// <param name="headDataDefinition">The result to add to found is the full path is matched</param>
        /// <param name="name">QualifiedName of the symbol looked for</param>
        /// <param name="nameIndex">Total count of the parts of the qualifiedName 'name' </param>
        /// <param name="currentDataDefinition">Currently checked DataDefinition</param>
        /// <param name="dataDefinitionPath"></param>
        /// <param name="typeDefContext">TypeDefinition context to force the algorithm to only work inside the typedef scope</param>
        private static void MatchVariable(List<KeyValuePair<DataDefinitionPath, DataDefinition>> foundedVariables, in DataDefinition headDataDefinition, in QualifiedName name,
            int nameIndex, in DataDefinition currentDataDefinition, DataDefinitionPath dataDefinitionPath, TypeDefinition typeDefContext, SymbolTable symbolTable) {

            var currentTypeDef = currentDataDefinition as TypeDefinition;

            //Name match ?
            if (currentTypeDef == null && //Do not try to match a TYPEDEF name
                name[nameIndex].Equals(currentDataDefinition.Name, StringComparison.OrdinalIgnoreCase)) {

                nameIndex--;
                if (nameIndex < 0) { //We reached the end of the name : it's a complete match

                    var parentTypeDef = currentDataDefinition.ParentTypeDefinition;
                    
                    if ((parentTypeDef != null && parentTypeDef != typeDefContext))
                    //Ok we found out that we are in a typedef. BUT if TypeDefContext is set and different that the context, we can check deeper inside parentTypeDef. 
                    {
                        //For each variable declared with this type (or a type that use this type), we need to add the headDataDefinition
                        AddAllReference(foundedVariables, headDataDefinition, parentTypeDef, dataDefinitionPath, typeDefContext, symbolTable);
                    }
                    else if (parentTypeDef == null && typeDefContext != null)
                    {
                        return; //The variable is not inside a typedef and typeDefContext is set so we have to ignore this variable
                    }
                    else
                    //If nameIndex < 0 AND there is no more parentTypeDef OR we are inside the defined TypeDefContext we have reach our destination, the variable is found. 
                    {
                        //we are on a variable
                        foundedVariables.Add(new KeyValuePair<DataDefinitionPath, DataDefinition>(dataDefinitionPath, headDataDefinition));
                    }

                    //End here
                    return;
                }

                //else it's not the end of name, let's continue with next part of QualifiedName
            }


            //Either we have a match or not, we need to continue to the parent or DataDefinition that use this TypeDefinition
            if (currentDataDefinition.Parent is DataDefinition parent)
            {
                //Go deeper to check the rest of the QualifiedName 'name'
                MatchVariable(foundedVariables, headDataDefinition, name, nameIndex, parent, dataDefinitionPath, typeDefContext, symbolTable); 
                return;
            }

            
            if (currentTypeDef != null) //We've found that we are currently onto a typedef. 
            {
                IEnumerable<DataDefinition> references = GetTypeReferences(symbolTable, currentTypeDef); //Let's get typeReferences (built by TypeCobolLinker phase)

                //If typeDefContext is set : Ignore references of this typedefContext to avoid loop seeking
                //                           Only takes variable references that are declared inside the typeDefContext
                if (typeDefContext != null)
                    references = references.Where(r => r.TypeDefinition != typeDefContext && r.ParentTypeDefinition == typeDefContext);


                foreach (var reference in references)
                {
                    //references property of a TypeDefinition can lead to variable in totally others scopes, like in another program
                    //So we need to check if we can access this variable
                    var newDataDefinitionPath = new DataDefinitionPath(dataDefinitionPath, reference);//Add the reference found to the dataDefinitionPath

                    MatchVariable(foundedVariables, headDataDefinition, name, nameIndex, reference, newDataDefinitionPath, typeDefContext, symbolTable);
                }
            }

            //If we reach here, it means we are on a DataDefinition with no parent
            //==> End of treatment, there is no match
        }

        /// <summary>
        /// For all usage of this type by a variable (outside a TypeDefinition), add headDataDefinition to found
        /// 
        /// 
        /// Technical note: this method should be declared under MatchVariable because there is no use for it outside.
        /// </summary>
        /// <param name="found"></param>
        /// <param name="headDataDefinition"></param>
        /// <param name="currentDataDefinition"></param>
        private static void AddAllReference(List<KeyValuePair<DataDefinitionPath, DataDefinition>> foundedVariables, DataDefinition headDataDefinition, [NotNull] TypeDefinition currentDataDefinition, DataDefinitionPath dataDefinitionPath, TypeDefinition typeDefContext, SymbolTable symbolTable)
        {
            IEnumerable<DataDefinition> references = GetTypeReferences(symbolTable, currentDataDefinition);

            //If typedefcontext is setted : Ignore references of this typedefContext to avoid loop seeking
            //                              + Only takes variable references that are declared inside the typeDefContext
            if (typeDefContext != null)
                references = references.Where(r => r.TypeDefinition != typeDefContext.TypeDefinition && r.ParentTypeDefinition == typeDefContext);

            foreach (var reference in references)
            {
                var parentTypeDef = reference.ParentTypeDefinition;
                if (parentTypeDef == null || parentTypeDef == typeDefContext)
                {
                    var newDataDefinitionPath = new DataDefinitionPath(dataDefinitionPath, reference);//Add the reference found to the dataDefinitionPath
                    foundedVariables.Add(new KeyValuePair<DataDefinitionPath, DataDefinition>(newDataDefinitionPath, headDataDefinition));

                }
                else
                {
                    var newDataDefinitionPath = new DataDefinitionPath(dataDefinitionPath, reference);//Add the reference found to the dataDefinitionPath
                    AddAllReference(foundedVariables, headDataDefinition, parentTypeDef, newDataDefinitionPath, typeDefContext, symbolTable);
                }
            }
        }

        /// <summary>
        /// Get all items with a specific name that are subordinates of a type
        /// </summary>
        /// <param name="name">Name of items we search for</param>
        /// <returns>Direct or indirect subordinates of a custom type</returns>
        private List<DataDefinition> GetCustomTypesSubordinatesNamed(string name)
        {
            //Search through local types if we are on a Function
            var foundDataDef = new List<DataDefinition>();
            if (CurrentScope == Scope.Function)
            {
                var functionLocalCandidates = GetVariablesUnderTypeDefinition(name, this);
                if (functionLocalCandidates != null) foundDataDef.AddRange(functionLocalCandidates);
            }

            //Get programs from Namespace table
            var programList = GetTableFromScope(Scope.Namespace).Programs;
            foreach (var programs in programList)
            {
                //Get Custom Types from program 
                foreach (var pgm in programs.Value)
                {
                    //we shouldn't have more than one program with the same name
                    //but just in case it changes 
                    GetVariablesUnderTypeDefinition(pgm.SymbolTable, name, pgm.IsNested, foundDataDef);
                }
            }

            //Get Custom Types from Intrinsic table 
            var intrinsicTable = this.GetTableFromScope(Scope.Intrinsic);
            var variablesUnderTypeDefinition = GetVariablesUnderTypeDefinition(name, intrinsicTable);
            if (variablesUnderTypeDefinition != null) {
                foundDataDef.AddRange(variablesUnderTypeDefinition);
            }

            return foundDataDef;
        }

        /// <summary>
        /// Search for variables with the specified name under a TypeDefinition in the specified SymbolTable.
        /// Search only in DataTypeEntries.
        /// </summary>
        /// <param name="name">name of the variable to search for</param>
        /// <param name="table">SymbolTable that can have the searched DataDefinition</param>
        /// <returns>List of found DataDefinitions that correspond to the passed name</returns>
        [CanBeNull]
        private static List<DataDefinition> GetVariablesUnderTypeDefinition(string name, SymbolTable table)
        {
            table.DataTypeEntries.TryGetValue(name, out var dataDef);
            return dataDef;
        }

        /// <summary>
        /// Search for variables with the specified name under a TypeDefinition in all enclosing SymbolTable.
        /// Search only in DataTypeEntries.
        /// 
        /// Intrinsic SymbolTable is not searched.
        /// </summary>
        /// <param name="symbolTable">Current SymbolTable</param>
        /// <param name="name">name of the variable to search for</param>
        /// <param name="resultDataDefinitions">Data definitions found with the specified name and under a type</param>
        /// <returns>List of found DataDefinition</returns>
        private static void GetVariablesUnderTypeDefinition(SymbolTable symbolTable, string name, bool isNested, List<DataDefinition> resultDataDefinitions)
        {
            var currSymbolTable = symbolTable;

            //Stop at namespace because Namespace don't contains any type and Intrinsic is shared between all programs
            //and will be searched separately
            while (currSymbolTable != null && currSymbolTable.CurrentScope != Scope.Namespace)
            {
                var temp = GetVariablesUnderTypeDefinition(name, currSymbolTable);
                if (temp != null)
                {
                    resultDataDefinitions.AddRange(temp);
                }

                //When searching in Nested, we stop at first Global level to avoid duplicates in Global and Program from Main
                if (currSymbolTable.CurrentScope == Scope.Global && isNested)
                {
                    break;
                }

                currSymbolTable = currSymbolTable.EnclosingScope;
            }
        }

        /// <summary>
        /// Indicates whether a given Node correspond to a QualifiedName.
        /// A node matches a name when every part of both names are equal, starting from the innermost part.
        /// </summary>
        /// <param name="node">The node being tested</param>
        /// <param name="name">The name being tested</param>
        /// <returns>True when node matches, false otherwise.</returns>
        private static bool Match([NotNull] Node node, [NotNull] QualifiedName name)
        {
            Node current = node;
            int index = name.Count - 1;
            while (current != null && index >= 0) // Stop on shortest name, either node name or supplied name.
            {
                // Skip unnamed levels in node hierarchy (i.e. data div, working storage, etc)
                if (!string.IsNullOrEmpty(current.Name))
                {
                    if (current.Name.Equals(name[index], StringComparison.OrdinalIgnoreCase))
                    {
                        // Name parts match, go one level up in QualifiedName
                        index--;
                    }
                    else
                    {
                        // As soon as a difference is detected, we can leave the method returning False value
                        return false;
                    }
                }

                // Go to upper level in node hierarchy
                current = current.Parent;
            }

            // No more parts to compare and all parts tested are equal, we return True
            return true;
        }

        #endregion

        #region SECTIONS

        private readonly IDictionary<string, List<Section>> Sections =
            new Dictionary<string, List<Section>>(StringComparer.OrdinalIgnoreCase);

        internal void AddSection(Section section)
        {
            AddNode(Sections, section);
        }

        public IList<Section> GetSection(string name)
        {
            Sections.TryGetValue(name, out var values);
            if (values != null) return values.ToList();  //.ToList so the caller cannot modify our stored list

            return EmptySectionList;
        }

        #endregion

        #region PARAGRAPHS

        private readonly IDictionary<string, List<Paragraph>> Paragraphs =
            new Dictionary<string, List<Paragraph>>(StringComparer.OrdinalIgnoreCase);

        internal void AddParagraph(Paragraph paragraph)
        {
            AddNode(Paragraphs, paragraph);
        }

        public IList<Paragraph> GetParagraph(SymbolReference symbolRef, Section sectionNode)
        {
            //First extract expected paragraph and section names from symbol ref
            string paragraphName, parentSectionName;
            if (symbolRef.IsQualifiedReference)
            {
                //If paragraph is qualified we get a paragraph and a section name
                var qualifiedSymbolReference = (QualifiedSymbolReference) symbolRef;
                paragraphName = qualifiedSymbolReference.NameLiteral.Value;
                parentSectionName = qualifiedSymbolReference.Tail.Name;
            }
            else
            {
                //Otherwise expected parent section is null
                paragraphName = symbolRef.Name;
                parentSectionName = null;
            }

            //Retrieve all paragraphs with matching name, then apply additional filters
            if (Paragraphs.TryGetValue(paragraphName, out var candidates))
            {
                if (parentSectionName != null)
                {
                    //We're looking for a paragraph inside a specific section, so we keep only paragraphs whose parent section matches
                    return candidates.FindAll(p => p.Parent.Name.Equals(parentSectionName, StringComparison.OrdinalIgnoreCase));
                }

                //Priority is given to paragraphs located in the same section as the caller node
                Predicate<Paragraph> matchParentSection;
                if (sectionNode != null)
                {
                    //Match paragraphs located inside the given section node
                    matchParentSection = p => p.Parent == sectionNode;
                }
                else
                {
                    //Match paragraphs located directly in the PROCEDURE DIVISION
                    matchParentSection = p => p.Parent.CodeElement.Type == CodeElementType.ProcedureDivisionHeader;
                }

                //If we get results in the same section, we return them. Otherwise, just return candidates with correct name.
                var inSameSection = candidates.FindAll(matchParentSection);
                return inSameSection.Count > 0 ? inSameSection : candidates.ToList();
            }

            return EmptyParagraphList;
        }

        /// <summary>
        /// Get all paragraphs in the current scope.
        /// </summary>
        /// <returns>The collection of paragraph names</returns>
        public IEnumerable<Paragraph> GetParagraphs(Expression<Func<Paragraph, bool>> predicate)
        {
            return Paragraphs.Values.SelectMany(p => p).AsQueryable().Where(predicate).Distinct();
        }

        /// <summary>
        /// Get all paragraphs with the given name and declared as a child of the specified node
        /// </summary>
        /// <returns>The collection of paragraph</returns>
        public IEnumerable<Paragraph> GetParagraphs(string paragraphName, Node owner)
        {
            if (Paragraphs.TryGetValue(paragraphName, out var paragraphs))
            {
                return paragraphs.Where(p => p.Parent == owner);
            }
            return null;
        }

        #endregion

        /// <summary>
        /// Try to disambiguate between Section or Paragraph reference.
        /// </summary>
        /// <param name="target">A non-null SymbolReference.</param>
        /// <param name="callerNodeSection">The Section node in which the statement making the reference appears.</param>
        /// <returns>A tuple made of both list of sections and list of paragraphs. The lists may be null,
        /// this indicates that the search has not been performed for the corresponding type.
        /// They also may be empty, this means the search has been performed but yielded no results.</returns>
        public (IList<Section>, IList<Paragraph>) GetSectionOrParagraph([NotNull] SymbolReference target, Section callerNodeSection)
        {
            IList<Section> sections = null;
            IList<Paragraph> paragraphs = null;

            //Check target type to avoid useless search if the type is already known
            if (target.IsAmbiguous)
            {
                //Have to search for both sections and paragraphs
                sections = GetSections();
                paragraphs = GetParagraphs();
            }
            else
            {
                switch (target.Type)
                {
                    case SymbolType.SectionName:
                        sections = GetSections();
                        break;
                    case SymbolType.ParagraphName:
                        paragraphs = GetParagraphs();
                        break;
                }
            }

            return (sections, paragraphs);

            IList<Section> GetSections() => GetSection(target.Name);

            IList<Paragraph> GetParagraphs() => GetParagraph(target, callerNodeSection);
        }

        #region TYPES

        public readonly IDictionary<string, List<TypeDefinition>> Types =
            new Dictionary<string, List<TypeDefinition>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// References of a type. This property is scope sensitive.
        /// </summary>
        public Dictionary<TypeDefinition, List<DataDefinition>> TypesReferences { get; }

        [NotNull]
        private static List<DataDefinition> GetTypeReferences([NotNull] SymbolTable symbolTable, [NotNull] TypeDefinition typeDefinition)
        {
            if (!symbolTable.TypesReferences.TryGetValue(typeDefinition, out var result))
            {
                result = new List<DataDefinition>();
            }

            return result;
        }

        public void AddType(TypeDefinition type)
        {
            AddNode(Types, type);
        }

        /// <summary>
        /// Copy all children of this DataDefinition to dictionary DataTypeEntries
        /// 
        /// Note:
        /// When this method is called with a TypeDefinition, don't add the 
        /// TypeDefinition node to DataTypeEntries.
        /// 
        /// DataTypeEntries only contains potential variables.
        /// Children of TypeDefinition can be variable if a variable is declared with that type.
        /// But a TypeDefinition can never be a variable itself.
        /// </summary>
        /// <param name="data"></param>
        public void AddDataDefinitionsUnderType([NotNull] DataDefinition data)
        {
            //data outside a typedef must NOT be copied into DataTypeEntries
            Debug.Assert(data.IsPartOfATypeDef);

            foreach (var dataChild in data.Children)
            {
                var childDataDefinition = (DataDefinition) dataChild;

                AddNode(DataTypeEntries, childDataDefinition);

                //add child data definition
                AddDataDefinitionsUnderType(childDataDefinition);
            }
        }

        [NotNull]
        public IList<TypeDefinition> GetType(DataDefinition symbol)
        {
            return GetType(symbol.DataType);
        }

        [NotNull]
        public IList<TypeDefinition> GetType(DataType dataType, string pgmName = null)
        {
            if (dataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85)
            {
                return EmptyTypeDefinitionList;
            }

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
            var found = GetFromTableAndEnclosing(name, st => st.Types, MatchUsingName, Scope.Intrinsic, Scope.Intrinsic);

            if (string.IsNullOrEmpty(name.Tail) || found.Count > 0)
                return found;

            found = GetType(name, name.Tail, found); //Pass name.Tail as a program name 

            return found;
        }

        public IEnumerable<TypeDefinition> GetTypes(Expression<Func<TypeDefinition, bool>> predicate, Scope maximalScope)
        {
            var foundedTypes = new List<TypeDefinition>();

            SymbolTable currentTable = this;
            while (currentTable != null && currentTable.CurrentScope >= maximalScope)
            {
                var dataToSeek = currentTable.Types.Values.SelectMany(t => t);
                if (currentTable.CurrentScope == Scope.Namespace)
                {
                    // For namespace scope, we collect public types from every programs
                    dataToSeek = currentTable.Programs.SelectMany(p => p.Value) // all programs in Namespace scope
                        .Select(program => program.SymbolTable.GetTableFromScope(Scope.Program)) // access to Program scope which contains private and public types
                        .SelectMany(symbolTable => symbolTable.Types)
                        .SelectMany(p => p.Value) // all types
                        .Where(typeDefinition => typeDefinition.CodeElement.Visibility == AccessModifier.Public); // all public types
                }
                else if (currentTable.CurrentScope == Scope.Intrinsic)
                {
                    // For Intrinsic scope, we collect public types only
                    dataToSeek = dataToSeek.Where(typeDefinition => typeDefinition.CodeElement.Visibility == AccessModifier.Public);
                }

                var results = dataToSeek.AsQueryable().Where(predicate);

                foundedTypes.AddRange(results);

                currentTable = currentTable.EnclosingScope;
            }

            return foundedTypes.Distinct();
        }

        /// <summary>
        /// Get type into a specific program by giving program name
        /// </summary>
        /// <param name="name">Qualified name of the wanted type</param>
        /// <param name="pgmName">Name of the program that may contains the type</param>
        /// <returns></returns>
        private List<TypeDefinition> GetType(QualifiedName name, string pgmName, List<TypeDefinition> found = null)
        {
            found = found ?? new List<TypeDefinition>();
            var programs = GetProgramsHelper(pgmName); //Get the program corresponding to the given namespace

            if (programs != null)
            {
                var types = new List<TypeDefinition>();
                foreach (var program in programs)
                {
                    //Get all TYPEDEF PUBLIC from this program
                    var programTypes = GetPublicTypes(program.SymbolTable.GetTableFromScope(Scope.Program).Types);

                    //Check if there is a type that correspond to the given name (head)
                    var typeList =  GetFromTable(name.Head, programTypes);
                    if (typeList.Count > 0) types.AddRange(typeList);
                }

                return types;
            }

            return found;
        }

        /// <summary>
        /// Get all Public TYPEDEF from the specified SymbolTable
        /// </summary>
        /// <param name="programTypes"></param>
        /// <returns></returns>
        private static Dictionary<string, List<TypeDefinition>> GetPublicTypes(IDictionary<string, List<TypeDefinition>> programTypes)
        {
            return programTypes
                .Where(p =>
                    p.Value.All(f => f.CodeElement.Visibility == AccessModifier.Public))
                .ToDictionary(f => f.Key, f => f.Value, StringComparer.OrdinalIgnoreCase); //Sort types to get only the ones with public AccessModifier
        }

        #endregion

        #region FUNCTIONS

        public readonly IDictionary<string, List<FunctionDeclaration>> Functions =
            new Dictionary<string, List<FunctionDeclaration>>(StringComparer.OrdinalIgnoreCase);

        public void AddFunction(FunctionDeclaration function)
        {
            AddNode(Functions, function);
        }

        public List<FunctionDeclaration> GetFunction(StorageArea storageArea, IProfile profile = null)
        {
            return GetFunction(storageArea.SymbolReference, profile);
        }

        private List<FunctionDeclaration> GetFunction(SymbolReference symbolReference, IProfile profile = null)
        {
            var uri = new URI(symbolReference.Name);
            return GetFunction(uri, profile);
        }

        public IEnumerable<FunctionDeclaration> GetFunctions(Expression<Func<FunctionDeclaration, bool>> predicate, Scope maximalScope)
        {
            var foundedFunctions = new List<FunctionDeclaration>();

            SymbolTable currentTable = this;
            while (currentTable != null && currentTable.CurrentScope >= maximalScope)
            {
                var dataToSeek = currentTable.Functions.Values.SelectMany(t => t);
                if (currentTable.CurrentScope == Scope.Namespace)
                {
                    //For namespace scope, we need to browse every program
                    dataToSeek = currentTable
                        .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Program)
                            .Functions.Values.SelectMany(t => t));
                }

                var results = dataToSeek.AsQueryable().Where(predicate);

                if (currentTable.CurrentScope == Scope.Intrinsic || currentTable.CurrentScope == Scope.Namespace)
                    results = results.Where(tp =>
                        tp.CodeElement != null &&
                        tp.CodeElement.Visibility == AccessModifier.Public);

                foundedFunctions.AddRange(results);


                currentTable = currentTable.EnclosingScope;
            }

            return foundedFunctions.Distinct();
        }

        public List<FunctionDeclaration> GetFunction(QualifiedName name, IProfile profile = null, string nameSpace = null)
        {
            var found = GetFunction(name, nameSpace);
            
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

        private bool Matches(ParametersProfileNode expected, IProfile actual, string pgmName)
        {
            if (expected.InputParameters.Count != actual.Inputs.Count) return false;
            if (expected.InoutParameters.Count != actual.Inouts.Count) return false;
            if (expected.OutputParameters.Count != actual.Outputs.Count) return false;

            for (int c = 0; c < expected.InputParameters.Count; c++)
                if (!TypeCompare(expected.InputParameters[c], actual.Inputs[c], pgmName)) return false;
            for (int c = 0; c < expected.InoutParameters.Count; c++)
                if (!TypeCompare(expected.InoutParameters[c], actual.Inouts[c], pgmName)) return false;
            for (int c = 0; c < expected.OutputParameters.Count; c++)
                if (!TypeCompare(expected.OutputParameters[c], actual.Outputs[c], pgmName)) return false;
            return true;
        }

        private bool TypeCompare(ParameterDescription parameter, TypeInfo argumentTypeInfo, string pgmName)
        {
            //Omitted accepted only if parameter is Omittable
            if (argumentTypeInfo.DataType == DataType.Omitted)
            {
                return parameter.IsOmittable;
            }

            var parameterType = parameter.TypeDefinition;
            if (parameterType == null)
            {
                var parameterTypes = parameter.Name.Contains(".") ? this.GetType(parameter) : this.GetType(parameter.DataType, pgmName);
                if (parameterTypes.Count > 1)
                {
                    return false;
                }
                parameterType = parameterTypes.FirstOrDefault();
            }

            var argumentType = argumentTypeInfo.TypeDefinition;
            if (argumentType == null)
            {
                return parameterType == null && parameter.DataType == argumentTypeInfo.DataType;
            }

            return parameterType == argumentType;
        }

        [NotNull]
        private List<FunctionDeclaration> GetFunction(QualifiedName name, string nameSpace)
        {
            var result = GetFromTableAndEnclosing(name, st => st.Functions, MatchUsingName, Scope.Intrinsic);
            if (string.IsNullOrEmpty(nameSpace) || result.Count > 0)
                return result;

            var programs = GetProgramsHelper(nameSpace); //Get the programs corresponding to the given namespace
            if (programs != null)
            {
                result = new List<FunctionDeclaration>();
                foreach (var program in programs)
                {
                    var programFunctions = program.SymbolTable.GetTableFromScope(Scope.Program)
                            .Functions; //Get all function from this program
                    programFunctions = programFunctions
                                        .Where(p =>
                                               p.Value.All(f => (f.CodeElement).Visibility == AccessModifier.Public))
                                               .ToDictionary(f => f.Key, f => f.Value, StringComparer.OrdinalIgnoreCase); //Sort functions to get only the one with public AccessModifier

                    var res = GetFromTable(name.Head, programFunctions); //Check if there is a function that correspond to the given name (head)
                    if (res.Count > 0)
                    {
                        result.AddRange(res);
                    }
                }
            }

            return result;
        }

        #endregion

        #region PROGRAMS

        public readonly IDictionary<string, List<Program>> Programs =
            new Dictionary<string, List<Program>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Add a program to this symbolTable
        /// </summary>
        /// <param name="program"></param>
        public void AddProgram(Program program)
        {
            AddNode(Programs, program);
        }

        [NotNull]
        private List<Program> GetPrograms(QualifiedName name)
        {
            return GetFromTableAndEnclosing(name, st => st.Programs, MatchUsingName, Scope.Namespace);
        }

        public IEnumerable<Program> GetPrograms(string filter, bool exactMatch = false)
        {
            Func<Program, bool> predicate;
            if (exactMatch)
            {
                predicate = program => program.Name.Equals(filter, StringComparison.OrdinalIgnoreCase);
            }
            else
            {
                predicate = program => program.Name.StartsWith(filter, StringComparison.OrdinalIgnoreCase);
            }
            return this.GetTableFromScope(Scope.Namespace)
                .Programs.Values.SelectMany(t => t)
                .Where(predicate);
        }

        public IEnumerable<Program> GetPrograms()
        {
            return this.GetTableFromScope(Scope.Namespace)
                .Programs.Values.SelectMany(t => t);
        }

        #endregion

        #region SPECIAL NAMES

        private readonly IDictionary<string, List<SymbolDefinition>> EnvironmentMnemonics =
            new Dictionary<string, List<SymbolDefinition>>(StringComparer.OrdinalIgnoreCase);

        private SymbolTable GetRootGlobalTable()
        {
            var globalTable = GetTableFromScope(Scope.Global);
            if (globalTable == null)
            {
                return null;
            }

            while (globalTable.EnclosingScope != null && globalTable.EnclosingScope.CurrentScope == Scope.Global)
            {
                globalTable = globalTable.EnclosingScope;
            }

            return globalTable;
        }

        /// <summary>
        /// Add symbols defined by SPECIAL-NAMES paragraph.
        /// </summary>
        /// <param name="specialNamesParagraph">Special names code element</param>
        public void AddSpecialNames([NotNull] SpecialNamesParagraph specialNamesParagraph)
        {
            var targetTable = GetRootGlobalTable();
            Debug.Assert(targetTable != null); // Mnemonics can be declared only by root level programs which all have a Global SymbolTable
            
            if (specialNamesParagraph.MnemonicsForEnvironmentNames != null)
            {
                foreach (var mnemonicForEnvironmentName in specialNamesParagraph.MnemonicsForEnvironmentNames.Keys)
                {
                    Add(targetTable.EnvironmentMnemonics, mnemonicForEnvironmentName, s => s.Name);
                }
            }

            //TODO create dedicated dictionaries and add other special names
        }

        /// <summary>
        /// Retrieve environment mnemonic definitions for given environment mnemonic reference.
        /// </summary>
        /// <param name="symbolReference">Reference to resolve.</param>
        /// <returns>A non-null list of definitions. May be empty when the reference could not be resolved,
        /// may contain more than one element when the reference is ambiguous.</returns>
        public IList<SymbolDefinition> GetEnvironmentMnemonics(SymbolReference symbolReference)
        {
            var targetTable = GetRootGlobalTable();
            return targetTable != null
                ? GetFromTable(symbolReference.Name, targetTable.EnvironmentMnemonics)
                : new List<SymbolDefinition>();
        }

        /// <summary>
        /// Return whether a environment mnemonic name has corresponding definition or not.
        /// </summary>
        /// <param name="mnemonicName">Name to test.</param>
        /// <returns>True if definition(s) exist, False otherwise.</returns>
        public bool IsEnvironmentMnemonicNameDefined(string mnemonicName)
        {
            if (mnemonicName == null) return false; // Stay consistent with GetFromTable which allows entries with empty name but exclude nulls
            var targetTable = GetRootGlobalTable();
            return targetTable != null && targetTable.EnvironmentMnemonics.ContainsKey(mnemonicName);
        }

        #endregion

        #region Helpers

        /// <summary>
        /// Generic Add method
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="table"></param>
        /// <param name="symbol"></param>
        /// <param name="getName"></param>
        private static void Add<T>([NotNull] IDictionary<string, List<T>> table, [NotNull] T symbol, [NotNull] Func<T, string> getName)
        {
            string key = getName(symbol);
            //QualifiedName of symbol can be null - if we have a filler in the type definition
            if (key == null)
            {
                return;
            }

            List<T> found;
            bool present = table.TryGetValue(key, out found);
            if (!present)
            {
                found = new List<T>();
                table.Add(key, found);
            }
            found.Add(symbol);
        }

        private static void AddNode<TNode>([NotNull] IDictionary<string, List<TNode>> table, [NotNull] TNode symbol) where TNode : Node
            => Add(table, symbol, node => node.Name);

        /// <summary>
        /// Cobol has compile time binding for variables, sometimes called static scope.
        /// Within that, Cobol supports several layers of scope: Global and Local scope.
        /// TypeCobol introduces :
        /// - Intrinsic scope that is used for standard library types and variables.
        /// - Namespace is used for programs from external dependencies.
        /// - Program which contains private and public types shared across the whole program definition.
        /// - Function scope is used to store types and variables declared inside a function.
        /// </summary>
        public enum Scope
        {
            /// <summary>
            /// TC specific, contains external types and procedures loaded from intrinsic files.
            /// </summary>
            Intrinsic,

            /// <summary>
            /// TC specific, contains all known programs at compile time
            /// (that includes current main, nesteds and stackeds and also external dependencies).
            /// </summary>
            Namespace,

            /// <summary>
            /// TC specific, contains private and public types/procedures and also variables declared in global-storage section.
            /// Public and private types/procedures from Nested are bound to Program Scope from Main
            /// which means they are visible from Main itself and all Nesteds.
            /// </summary>
            Program,

            /// <summary>
            /// Cobol, contains variables flagged with GLOBAL keyword. 
            /// </summary>
            Global,

            /// <summary>
            /// Cobol, contains types, procedures and variables declared without any modifier.
            /// </summary>
            Local,

            /// <summary>
            /// TC specific, contains types and variables declared within a procedure.
            /// </summary>
            Function
        }

        private List<Program> GetProgramsHelper(string nameSpace)
        {
            return GetPrograms(new URI(nameSpace));
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
            if (EnvironmentMnemonics.Count > 0)
            {
                str.AppendLine("-- ENVIRONMENT MNEMONICS ---");
                foreach (var line in EnvironmentMnemonics)
                {
                    foreach (var item in line.Value)
                    {
                        str.Append(new string(' ', 2));
                        str.Append(item.Name);
                        str.AppendLine();
                    }
                }
            }
            if (verbose && EnclosingScope != null)
                str.Append(EnclosingScope.ToString(verbose));
            return str.ToString().TrimEnd(Environment.NewLine.ToCharArray());
        }

        private static void Dump(StringBuilder str, Node symbol, string indent = null)
        {
            str.Append(indent);
            str.Append(symbol.Name);
            if (symbol is DataDefinition) str.Append(':').Append(((DataDefinition) symbol).DataType);
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
            foreach (var type in Types.SelectMany(elem=>elem.Value))
            {
                this.AddType(type);
                AddDataDefinitionsUnderType(type);
            }
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
                    if (accessModifier == null  //If no AccessModifier given, add all the functions
                        || (function.CodeElement).Visibility == accessModifier) {
                        this.AddFunction(function); //Add function depending on the specified AccessModifier
                    }
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

    /// <summary>
    /// This class serves to follow the path of SymbolReference from a variable under a typedef till a variable outside a typedef.
    ///
    /// Group1                  TypeA               TypeB                 TypeC
    ///   var1                    A11 type typeB       B1                    C1
    ///    var11 type typeA                             B2                    C2
    ///                                                  B3 type typeC         C3
    /// 
    ///  Group2                  TypeO
    ///    var22                    Z11 type typeB
    ///     var222 type typeZ                      
    ///
    ///
    ///
    /// with an incomplete SymbolReference:
    /// var11::A11::B2::B3::C3
    /// we want to retrieve the full path:
    /// Group1::var1::var11::A11::B1::B2::B3::C1::C2::C3
    ///
    /// This full path is currently use for:
    /// - Codegen : Index under type used with qualification need to prefixed with a hash
    /// - When a SymbolReference is ambiguous we want to display all path that match the SymbolReference
    ///
    /// DataDefinitionPath optimize the data and the calculation necessary to get the full path.
    /// DataDefinitionPath only need to store the DataDefinition Node that reference a Type.
    /// In the previous example:
    /// var11 -> A11 -> B3 -> C3
    /// Then DataDefinitionPath will use the FixedVisualQualifiedName on each one of these DataDefinition Node to get the full path.
    ///
    /// Note:
    /// This is the job of the caller of DataDefinitionPath to store only Node that reference a Type.
    /// DataDefinitionPath will check that only in debug mode for performance reason.
    ///
    /// 
    /// When there are multiple path this class share DataDefinitionPath instance across path.
    /// Eg: For incomplete SymbolReference B1::C3
    /// We can have:
    /// Group1::var1::var11::A11::B1::B2::B3::C1::C2::C3
    /// Group2::var22::var222::O11::B1::B2::B3::C1::C2::C3
    ///
    /// We'll then have the same root for both DataDefinitionPath tree:
    /// var11  -> A11  \
    ///                 -> B3 -> C3   
    /// var222 -> Z11  /
    /// </summary>
    public class DataDefinitionPath
    {

        public DataDefinitionPath(DataDefinition currentDataDefinition) : this(null/*this is the head*/, currentDataDefinition)
        {
        }

        public DataDefinitionPath([CanBeNull] DataDefinitionPath parent, [NotNull] DataDefinition currentDataDefinition)
        {
            this.Parent = parent;
            this.CurrentDataDefinition = currentDataDefinition;

            //Check that our Typed CurrentDataDefinition is linked to a parent with the same type
            System.Diagnostics.Debug.Assert(this.Parent == null 
                                            || this.Parent.CurrentDataDefinition.ParentTypeDefinition == this.CurrentDataDefinition.TypeDefinition);
        }

        /// <summary>
        /// Parent which is the head or toward the head of this data definition path.
        /// 
        /// </summary>
        /// <returns>Null if this the head</returns>
        [CanBeNull] 
        public DataDefinitionPath Parent { get; }

        [NotNull]
        public DataDefinition CurrentDataDefinition { get; }


        public string ToString(string separator)
        {
            StringBuilder sb = new StringBuilder();

            //Start the tail with the qualifiedName (including the pgm name)
            sb.Append(this.CurrentDataDefinition.QualifiedName);
            var currentDataDefinitionPath = this.Parent;
            while (currentDataDefinitionPath != null)
            {
                //We don't want the pgm name here
                sb.Append(separator).Append(string.Join(separator, currentDataDefinitionPath.CurrentDataDefinition.VisualQualifiedNameWithoutProgram));
                currentDataDefinitionPath = currentDataDefinitionPath.Parent;
            }

            return sb.ToString();
        }
        public override string ToString()
        {
            return ToString("::");
        }
    }
}
