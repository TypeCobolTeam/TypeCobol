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
using Castle.Core.Internal;

namespace TypeCobol.Compiler.CodeModel
{

    public class SymbolTable
    {
        public Scope CurrentScope { get; internal set; }
        public SymbolTable EnclosingScope { get; internal set; }
        /// <summary>
        /// References of a type. This property is scope sensitive. To get all TypeReferences from any scope, use GetAllEnclosingTypeReferences().
        /// </summary>
        public Dictionary<TypeDefinition, List<DataDefinition>> TypesReferences { get; set; }

        /// <summary>
        /// Allow to get all the Type's references from any Enclosing Scope or Program
        /// </summary>
        public Dictionary<Node, List<DataDefinition>> GetAllEnclosingTypeReferences()
        {
            var result = new Dictionary<Node, List<DataDefinition>>();
            SymbolTable scope = this;//By default set this symboltable as the starting point

            while (scope != null) //Loop on enclosing scope until null scope. 
            {
                foreach (var typeReference in scope.TypesReferences.Select(pt => new KeyValuePair<TypeDefinition, List<DataDefinition>>(pt.Key, pt.Value.ToArray().ToList()))) //new KeyValuePair allow to loose object ref
                {
                    if (!result.ContainsKey(typeReference.Key)) //Avoid duplicate key
                        result.Add(typeReference.Key, typeReference.Value);
                }

                if (scope.CurrentScope == Scope.Namespace && scope.Programs.Any())
                    //Some TypeReferences are stored only in program's symbolTable, need to seek into them. 
                {
                    foreach (var program in scope.Programs.SelectMany(t => t.Value))
                    {
                        if (program != null && program.SymbolTable != null &&
                            !program.SymbolTable.TypesReferences.IsNullOrEmpty())
                        {
                            foreach (var progTypeRef in program.SymbolTable.TypesReferences.Select(pt =>
                                        new KeyValuePair<TypeDefinition, List<DataDefinition>>(pt.Key, pt.Value.ToArray().ToList()))) //new KeyValuePair allow to loose object ref
                            {
                                if (!result.ContainsKey(progTypeRef.Key)) //Avoid duplicate key
                                    result.Add(progTypeRef.Key, progTypeRef.Value);
                                else
                                {
                                    foreach (var reference in progTypeRef.Value) //Add the reference values not already discovered
                                    {
                                        if (!result[progTypeRef.Key].Contains(reference))
                                            result[progTypeRef.Key].Add(reference);
                                    }
                                }
                                    
                            }
                        }
                    }
                }

                scope = scope.EnclosingScope; //Go to the next enclosing scope. 
            }
            return result;
        }


        public SymbolTable(SymbolTable enclosing, Scope current)
        {
            CurrentScope = current;
            EnclosingScope = enclosing;
            TypesReferences = new Dictionary<TypeDefinition, List<DataDefinition>>();
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
            new Dictionary<string, List<DataDefinition>>(StringComparer.OrdinalIgnoreCase);

        //Dictionary for Type data entries
        public IDictionary<string, List<DataDefinition>> DataTypeEntries =
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
                Add(DataEntries, symbol);
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
        void AddVariableUnderTypeDefinition([NotNull] DataDefinition data)
        {
            //TypeDefinition must NOT be added to DataTypeEntries, only its children
            Debug.Assert(!(data is TypeDefinition)); 

            //Types are declared in the Declarations SymbolTable
            var table = GetTableFromScope(Scope.Declarations);
            
            //Add symbol to the dictionary
            Add(table.DataTypeEntries, data);
        }

        public IEnumerable<DataDefinition> GetVariables(SymbolReference symbolReference)
        {
            return GetVariablesExplicit(symbolReference.URI);
        }

        private IList<DataDefinition> GetVariables(string name)
        {
            //Try to get variable in the current program
            var found = GetFromTableAndEnclosing(name, GetDataDefinitionTable);
           
            return found;
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

        //---------------------------------------------


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

            bool redefinedVariableFound = false;

            while (!redefinedVariableFound && index >= 0)
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


        private IDictionary<string, List<DataDefinition>> GetDataDefinitionTable(SymbolTable symbolTable)
        {
            return symbolTable.DataEntries;
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
        public List<KeyValuePair<string, DataDefinition>> GetVariablesExplicitWithQualifiedName(QualifiedName name,
            TypeDefinition typeDefContext = null)
        {
            List<KeyValuePair<string, DataDefinition>> foundedVariables = new List<KeyValuePair<string, DataDefinition>>();
            //Get variable name declared into typedef declaration
            var candidates = GetCustomTypesSubordinatesNamed(name.Head);
            //Get all variables that corresponds to the given head of QualifiedName    
            candidates.AddRange(GetVariables(name.Head));
            
            var found = new List<DataDefinition>();
            int foundCount = 0;
            var completeQualifiedNames = new List<List<string>>();
            foreach (var candidate in candidates.Distinct())
            {
                completeQualifiedNames.Add(new List<string>());
                MatchVariable(found, candidate, name, name.Count - 1, candidate, completeQualifiedNames,
                    typeDefContext);

                if (foundCount == found.Count && completeQualifiedNames.Count > 0)
                    //No changes detected so delete the last completeQualifiedName tested.
                    completeQualifiedNames.Remove(completeQualifiedNames.Last());

                foundCount = found.Count;
            }
                
            int i = 0;
            foreach (var foundedVar in found)
            {
                completeQualifiedNames[i].Reverse();
                foundedVariables.Add(
                    new KeyValuePair<string, DataDefinition>(string.Join(".", completeQualifiedNames[i]),
                        foundedVar));
                i++;

                if (completeQualifiedNames.Count == i)
                    break;
            }

            return foundedVariables;
        }

        /// <summary>
        /// Recursively try to find the path for the given QualifiedName name. 
        /// Algorithm allows to browse every potential path to find where the variable QualifiedName is. 
        /// It's able to browse DataDefinition (Var + Group) but also TypeDef. 
        /// The algorithm will search as deep as possible in every direction until the path comes to an end. 
        /// </summary>
        /// <param name="found">List of compatible variable found regarding to the given name</param>
        /// <param name="headDataDefinition">Given potential variable candidate</param>
        /// <param name="name">QualifiedName of the symbol looked for</param>
        /// <param name="nameIndex">Total count of the parts of the qualifiedName 'name' </param>
        /// <param name="currentDataDefinition">Currently checked DataDefinition</param>
        /// <param name="completeQualifiedNames">List of list of string that allows to store all the different path for the founded possibilities</param>
        /// <param name="typeDefContext">TypeDefinition context to force the algorithm to only work inside the typedef scope</param>
        public void MatchVariable(IList<DataDefinition> found, DataDefinition headDataDefinition, QualifiedName name,
            int nameIndex, DataDefinition currentDataDefinition, List<List<string>> completeQualifiedNames, TypeDefinition typeDefContext) {

            completeQualifiedNames.Last().Add(currentDataDefinition.Name);
            var currentTypeDef = currentDataDefinition as TypeDefinition;

            //Name match ?
            if (currentTypeDef == null && //Do not try to match a TYPEDEF name
                name[nameIndex].Equals(currentDataDefinition.Name, StringComparison.OrdinalIgnoreCase)) {

                nameIndex--;
                if (nameIndex < 0) { //We reached the end of the name : it's a complete match

                    var qualifiedPath = new List<string>();
                    var parentTypeDef = currentDataDefinition.GetParentTypeDefinitionWithPath(qualifiedPath);
                    
                    if ((parentTypeDef != null && parentTypeDef != typeDefContext))
                    //Ok we found out that we are in a typedef. BUT if TypeDefContext is set and different that the context, we can check deeper inside parentTypeDef. 
                    {
                        //We are under a TypeDefinition
                        completeQualifiedNames.Last().Remove(currentDataDefinition.Name);
                        completeQualifiedNames.Last().AddRange(qualifiedPath);
                        //For each variable declared with this type (or a type that use this type), we need to add the headDataDefinition
                        AddAllReference(found, headDataDefinition, parentTypeDef, completeQualifiedNames, typeDefContext);
                    }
                    else if (parentTypeDef == null && typeDefContext != null)
                    {
                        return; //The variable is not inside a typedef and typeDefContext is set so we have to ignore this variable
                    }
                    else
                    //If nameIndex < 0 AND there is no more parentTypeDef OR we are inside the defined TypeDefContext we have reach our destination, the variable is found. 
                    {
                        //we are on a variable
                        found.Add(headDataDefinition);
                        completeQualifiedNames.Last().Remove(currentDataDefinition.Name);//Without this it will duplicate the name currentDataDefinitionName
                        completeQualifiedNames.Last().AddRange(currentDataDefinition.QualifiedName.Reverse());//Add the complete qualifiedName of this founded variable 
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
                //Go deeper to check the rest of the QualifiedName 'name'
                MatchVariable(found, headDataDefinition, name, nameIndex, parent, completeQualifiedNames, typeDefContext); 
                return;
            }

            
            if (currentTypeDef != null) //We've found that we are currently onto a typedef. 
            {
                var dataType = GetAllEnclosingTypeReferences().FirstOrDefault(k => k.Key == currentTypeDef); //Let's get typereferences (built by TypeCobolLinker phase)
                if (dataType.Key == null || dataType.Value == null)
                    return;
                var references = dataType.Value;

                //If typedefcontext is set : Ignore references of this typedefContext to avoid loop seeking
                //                           Only takes variable references that are declared inside the typeDefContext
                if (typeDefContext != null)
                    references = references.Where(r => r.DataType != typeDefContext.DataType && r.ParentTypeDefinition == typeDefContext).ToList();

                var primaryPath = completeQualifiedNames.Last().ToArray(); //PrmiaryPath that will be added in front of every reference's path found
                foreach (var reference in references)
                {
                    //references property of a TypeDefinition can lead to variable in totally others scopes, like in another program
                    //So we need to check if we can access this variable
                    if (reference.IsPartOfATypeDef || GetVariables(reference.Name).Contains(reference))
                    {
                        if (found.Count == 0)
                            completeQualifiedNames.Remove(completeQualifiedNames.Last());//InCase nothing was found after first reference checked, we need to reset completeQualifiedName to it's primary value.
                        completeQualifiedNames.Add(new List<string>(primaryPath));

                        MatchVariable(found, headDataDefinition, name, nameIndex, reference, completeQualifiedNames, typeDefContext);
                    }
                       
                }
                return;
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
        /// <param name="heaDataDefinition"></param>
        /// <param name="currentDataDefinition"></param>
        private void AddAllReference(IList<DataDefinition> found, DataDefinition heaDataDefinition, [NotNull] TypeDefinition currentDataDefinition, List<List<string>> completeQualifiedNames, TypeDefinition typeDefContext)
        {
            completeQualifiedNames.Last().Add(currentDataDefinition.Name);
            var dataType = GetAllEnclosingTypeReferences().FirstOrDefault(k => k.Key == currentDataDefinition);
            if (dataType.Key == null || dataType.Value == null)
                return;
            var references = dataType.Value;
            //If typedefcontext is setted : Ignore references of this typedefContext to avoid loop seeking
            //                              + Only takes variable references that are declared inside the typeDefContext
            if (typeDefContext != null)
                references = references.Where(r => r.DataType != typeDefContext.DataType && r.ParentTypeDefinition == typeDefContext).ToList();
            var typePath = completeQualifiedNames.Last().ToArray();
            var referenceCounter = 0;
            foreach (var reference in references)
            {
                var primaryPath = new List<string>();
                var parentTypeDef = reference.GetParentTypeDefinitionWithPath(primaryPath);
                if (parentTypeDef != null && parentTypeDef != typeDefContext) {
                    completeQualifiedNames.Last().AddRange(primaryPath);
                    AddAllReference(found, heaDataDefinition, parentTypeDef, completeQualifiedNames, typeDefContext);
                    referenceCounter++;
                } else { 
                    //we are on a variable but ... references property of a TypeDefinition can lead to variable in totally others scopes, like in another program
                    //So we need to check if we can access this variable OR check if the variable is declared inside the typeDefContext
                    if (GetVariables(reference.Name).Contains(reference) || (typeDefContext != null && typeDefContext.GetChildren<DataDefinition>(reference.Name, true).Contains(reference)))
                    {
                        found.Add(heaDataDefinition);
                        referenceCounter++;
                        if (referenceCounter == 1) //If first reference found, add it to the top item of list
                            completeQualifiedNames.Last().AddRange(reference.QualifiedName.Reverse());
                        else
                        {
                            var newPath = new List<string>(typePath);
                            newPath.AddRange(reference.QualifiedName.Reverse());
                            completeQualifiedNames.Add(newPath);
                        }
                    }
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
            var foundDataDef = new List<DataDefinition>();

            //Get programs from Namespace table
            var programList = this.GetProgramsTable(GetTableFromScope(Scope.Namespace));
            foreach (var programs in programList) {

                //Get Custom Types from program 
                foreach (var pgm in programs.Value) { //we shouldn't have more than one program with the same name
                    //but just in case it changes 
                    foundDataDef.AddRange(GetVariablesUnderTypeDefFromTableAndEnclosing(pgm.SymbolTable, name));
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
        private static IEnumerable<DataDefinition> GetVariablesUnderTypeDefinition(string name, SymbolTable table) {
            List<DataDefinition> dataDef;
            table.DataTypeEntries.TryGetValue(name, out dataDef);
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
        /// <returns>List of found DataDefinition</returns>
        private static IEnumerable<DataDefinition> GetVariablesUnderTypeDefFromTableAndEnclosing(SymbolTable symbolTable, string name)
        {
            var currSymbolTable = symbolTable;
            var datadefinitions = new List<DataDefinition>();
            //Don't search into Intrinsic table because it's shared between all programs
            while (currSymbolTable != null && currSymbolTable.CurrentScope != Scope.Intrinsic) {
                var result = GetVariablesUnderTypeDefinition(name, currSymbolTable);
                if (result != null) {
                    datadefinitions.AddRange(result);
                }
                currSymbolTable = currSymbolTable.EnclosingScope;
            }

            return datadefinitions;
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
                if (part1.Equals(part2, StringComparison.OrdinalIgnoreCase)) offset++;
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
                if (parent.Name.Equals(pname, StringComparison.OrdinalIgnoreCase)) filtered.Add(symbol);
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
            new Dictionary<string, List<Section>>(StringComparer.OrdinalIgnoreCase);

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
            new Dictionary<string, List<Paragraph>>(StringComparer.OrdinalIgnoreCase);

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
        public IEnumerable<Paragraph> GetParagraphs(Expression<Func<Paragraph, bool>> predicate)
        {
            return Paragraphs.Values.SelectMany(p => p).AsQueryable().Where(predicate).Distinct();
        }

        #endregion

        #region TYPES

        public IDictionary<string, List<TypeDefinition>> Types =
            new Dictionary<string, List<TypeDefinition>>(StringComparer.OrdinalIgnoreCase);

        public void AddType(TypeDefinition type)
        {
            Add(Types, type);
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

                Add(DataTypeEntries, childDataDefinition);

                //add child data definition
                AddDataDefinitionsUnderType(childDataDefinition);
            }
        }

        public IList<TypeDefinition> GetType(DataDefinition symbol)
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

            if (string.IsNullOrEmpty(name.Tail) || found.Any(f => string.Compare(f.QualifiedName.Tail, name.Tail, StringComparison.OrdinalIgnoreCase) == 0))
                return Get(found, name);

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
                    //For namespace scope, we need to browse every program
                    dataToSeek = currentTable
                        .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Declarations)
                            .Types.Values.SelectMany(t => t));

                    dataToSeek = dataToSeek.Concat(currentTable
                        .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Global)
                            .Types.Values.SelectMany(t => t)));
                }

                var results = dataToSeek.AsQueryable().Where(predicate);

                if (currentTable.CurrentScope == Scope.Intrinsic || currentTable.CurrentScope == Scope.Namespace)
                    results = results.Where(tp =>
                        tp.CodeElement != null &&
                        tp.CodeElement.Visibility == AccessModifier.Public);

                foundedTypes.AddRange(results);

                currentTable = currentTable.EnclosingScope;
            }

            return foundedTypes.Distinct();
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
                    p.Value.All(f => f.CodeElement.Visibility == AccessModifier.Public)) 
                .ToDictionary(f => f.Key, f => f.Value, StringComparer.OrdinalIgnoreCase); //Sort types to get only the ones with public AccessModifier
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
            new Dictionary<string, List<FunctionDeclaration>>(StringComparer.OrdinalIgnoreCase);

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
                        .Programs.SelectMany(p => p.Value.First().SymbolTable.GetTableFromScope(Scope.Declarations)
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

        private bool Matches(ParametersProfileNode p1, ParameterList p2, string pgmName)
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
        private bool TypeCompare(ParameterDescription p1, DataType p2, string pgmName)
        {
            //Omitted accepted only if parameter is Omittable
            if (p2 == DataType.Omitted) {
                return p1.IsOmittable;
            }
            
            var p1Types = p1.Name.Contains(".") ? this.GetType(p1) : this.GetType(p1.DataType, pgmName);
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

            if (string.IsNullOrEmpty(nameSpace) || result.Any(f => string.Compare(f.QualifiedName.Tail, nameSpace, StringComparison.OrdinalIgnoreCase) == 0))
                return result;

            var program = GetProgramHelper(nameSpace); //Get the program corresponding to the given namespace
            if(program != null)
            {
                var programFunctions = program.SymbolTable.GetTableFromScope(Scope.Declarations).Functions; //Get all function from this program
                programFunctions = programFunctions
                                    .Where(p =>
                                            p.Value.All(f => (f.CodeElement).Visibility == AccessModifier.Public))
                                            .ToDictionary(f => f.Key, f => f.Value, StringComparer.OrdinalIgnoreCase); //Sort functions to get only the one with public AccessModifier

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
            new Dictionary<string, List<Program>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Add a program to this symbolTable
        /// </summary>
        /// <param name="program"></param>
        public void AddProgram(Program program)
        {
            Add(Programs, program);
        }

        /// <summary>
        /// Add Multiple programs to SymbolTable
        /// </summary>
        /// <param name="programs"></param>
        public void AddPrograms(List<Program> programs)
        {
            foreach (var program in programs)
            {
                AddProgram(program);
            }
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

        public IEnumerable<Program> GetPrograms(string filter)
        {
            return this.GetTableFromScope(Scope.Namespace)
                .Programs.Values.SelectMany(t => t)
                .Where(fd => fd.Name.StartsWith(filter, StringComparison.OrdinalIgnoreCase));
        }

        public List<Program> GetPrograms()
        {
            return this.GetTableFromScope(Scope.Namespace)
                .Programs.Values.SelectMany(t => t).ToList();
        }


        #endregion


        #region Helpers

        /// <summary>
        /// Generic Add method
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="table"></param>
        /// <param name="symbol"></param>
        private void Add<T>([NotNull] IDictionary<string, List<T>> table, [NotNull] T symbol) where T : Node
        {
            //QualifiedName of symbol can be null - if we have a filler in the type definition
            if (symbol.QualifiedName == null)
            {
                return;
            }
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

        
        /// <summary>
        /// Cobol has compile time binding for variables, sometimes called static scope.
        /// Within that, Cobol supports several layers of scope: Global and Program scope.
        ///
        /// TypeCobol has Intrinsic scope used for standard library types and variables.
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
            /// GlobalStorage scope is TypeCobol specific. It will store all the DataEntry data are decalred inside a GLOBAL-STORAGE SECTION
            /// These variables will be accessible by all the nested programs. All the procedures will also have access to these variables. 
            /// Issue #805
            /// SymbolTable Enclosing Scopes allows to respect the rules : 
            ///     - GLOBALSS_RANGE 
            ///     - GLOBALSS_NOT_FOR_STACKED 
            /// </summary>
            GlobalStorage,

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
                throw new Exception(string.Format("Program with identifier {0} is defined multiple times.", programs.FirstOrDefault()?.Name));

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
}
