using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Domain
{
    /// <summary>
    /// A Symbol table builder for a program.
    /// 
    /// THESE ARE COBOL Rules
    /// ---------------------
    /// -"Global Program Scope" variables declared in working storage as global are visible to the entire program 
    ///     in which they are declared AND in all nested subprograms contained in that program.
    /// -"Local Scope" variables declared in working storage are visible to the entire program in which they are declared.
    /// -"Local Scope" variables declared in local storage are visible to the entire program in which they are declared,
    ///     but are deleted and reinitialized on every invocation.
    /// -"Nested Program Scope" Cobol does not distinguish between programs and functions/procedures, 
    /// its equivalent of a procedure or function is called a program.An infinite number of programs can be contained within a program, 
    /// and the variables of each are visible only within the scope of that individual program.
    /// You could think of this as function/procedure scope.
    /// 
    /// TypeCobol Rules can be read at: https://github.com/TypeCobolTeam/TypeCobol/issues/1081
    /// --------------------------------
    /// </summary>
    public class ProgramSymbolTableBuilder : ProgramClassBuilderNodeListener
    {
        private class DataDivisionSection
        {
            public Symbol.Flags Flag { get; }
            public Domain<VariableSymbol> Variables { get; }

            public DataDivisionSection(Symbol.Flags flag, Domain<VariableSymbol> variables)
            {
                Flag = flag;
                Variables = variables;
            }
        }

        /// <summary>
        /// The List of Stacked Program symbol built as a Scope.
        /// The first main program will be the first element of the list, 
        /// followed by remaining stacked programs.
        /// </summary>
        public List<ProgramSymbol> Programs { get; }

        /// <summary>
        /// The current ProgramSymbol being built as a Scope.
        /// It is either a Program or a Function.
        /// </summary>
        private ProgramSymbol CurrentScope
        {
            get;
            set;
        }

        /// <summary>
        /// The current node.
        /// </summary>
        private Node CurrentNode
        {
            get;
            set;
        }

        /// <summary>
        /// The Current DataDivision section
        /// </summary>
        private DataDivisionSection CurrentDataDivisionSection
        {
            get;
            set;
        }

        /// <summary>
        /// Keeps track of visited FunctionDeclaration nodes
        /// </summary>
        private readonly Stack<FunctionDeclaration> _functionDeclStack = new Stack<FunctionDeclaration>();

        private SectionSymbol CurrentSection
        {
            get;
            set;
        }

        /// <summary>
        /// Creates a new ProgramSymbolTableBuilder.
        /// </summary>
        public ProgramSymbolTableBuilder()
        {
            Programs = new List<ProgramSymbol>();
        }

        public override void OnNode(Node node, Program program)
        {
            
        }

        /// <summary>
        /// Called when a node is entered
        /// </summary>
        /// <param name="node">The entered node.</param>
        public override void Enter(Node node)
        {
            CurrentNode = node;
            //Default SemanticData for a Node is its owner program or function.
            //It is later refined while building DataDefinitions
            node.SemanticData = CurrentScope;
        }

        public override void Exit(Node node)
        {
            System.Diagnostics.Debug.Assert(CurrentNode == node);
            CurrentNode = node.Parent;
        }

        public override void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
            System.Diagnostics.Debug.Assert(CurrentNode != null);
            System.Diagnostics.Debug.Assert(object.ReferenceEquals(CurrentNode.CodeElement, programIdentification));

            //Create the program, its type will be created after collecting parameters and return variable
            var program  = new ProgramSymbol(programIdentification.ProgramName.Name);
            if (this.CurrentScope == null)
            {
                //This is the main program or a stacked program.
                //TODO SemanticDomain: test for duplicate and enter program into Root.

                //Add the new Stacked program into our result list.
                Programs.Add(CurrentScope);
            }
            else
            {
                //This is a nested program.
                //TODO SemanticDomain: store nestedProgram into the root table.
                System.Diagnostics.Debug.Assert(CurrentNode.Parent != null);
                System.Diagnostics.Debug.Assert(CurrentNode.Parent.CodeElement != null);
                System.Diagnostics.Debug.Assert(CurrentNode.Parent.CodeElement.Type == CodeElementType.ProgramIdentification);

                //Enter the program as nested here and set the owner.
                this.CurrentScope.Programs.Enter(program);
                program.Owner = this.CurrentScope;
            }

            //Set the current scope
            this.CurrentScope = program;

            //Semantic data on the node
            CurrentNode.SemanticData = this.CurrentScope;
        }

        public override void EndCobolProgram(ProgramEnd end)
        {
            //For a stacked program the Owner is its Namespace so this will correctly
            //reset the CurrentScope to null, for a nested Program, the Owner is the enclosing Program.
            this.CurrentScope = CurrentScope.Owner as ProgramSymbol;
        }

        public override void StartDataDivision(DataDivisionHeader header)
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartFileSection(FileSectionHeader header)
        {
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.FILE, CurrentScope.FileData);
        }

        public override void EndFileSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.GLOBAL_STORAGE, CurrentScope.GlobalStorageData);
        }

        public override void EndGlobalStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.WORKING_STORAGE, CurrentScope.WorkingStorageData);
        }

        public override void EndWorkingStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartLocalStorageSection(LocalStorageSectionHeader header)
        {
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.LOCAL_STORAGE, CurrentScope.LocalStorageData);
        }

        public override void EndLocalStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartLinkageSection(LinkageSectionHeader header)
        {
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.LINKAGE, CurrentScope.LinkageData);
        }

        public override void EndLinkageSection()
        {
            CurrentDataDivisionSection = null;
        }

        /// <summary>
        /// Resolve in the current program linkage section, the given parameter.
        /// </summary>
        /// <param name="p">The parameter to resolve.</param>
        /// <returns>The resolved variable if any, null otherwise</returns>
        private VariableSymbol ResolveUsingParameter(CallTargetParameter p)
        {
            System.Diagnostics.Debug.Assert(p.StorageArea != null);
            System.Diagnostics.Debug.Assert(p.StorageArea.SymbolReference != null);
            string parameterName = p.StorageArea.SymbolReference.Name;
            var parameterCandidateSymbols = this.CurrentScope.LinkageData.Lookup(parameterName);
            if (parameterCandidateSymbols != null)
            {
                System.Diagnostics.Debug.Assert(parameterCandidateSymbols.Count == 1);
                var parameterSymbol = parameterCandidateSymbols.Symbol;
                if (p.SharingMode == null)
                    parameterSymbol.SetFlag(Symbol.Flags.ByReference, true);
                else
                    switch (p.SharingMode.Value)
                    {
                        case ParameterSharingMode.ByContent:
                            parameterSymbol.SetFlag(Symbol.Flags.ByContent, true);
                            break;
                        case ParameterSharingMode.ByReference:
                            parameterSymbol.SetFlag(Symbol.Flags.ByReference, true);
                            break;
                        case ParameterSharingMode.ByValue:
                            parameterSymbol.SetFlag(Symbol.Flags.ByValue, true);
                            break;
                    }

                if (p.PassingDirection != null)
                    switch (p.PassingDirection.Value)
                    {
                        case ParameterPassingDirection.Input:
                            parameterSymbol.SetFlag(Symbol.Flags.Input, true);
                            break;
                        case ParameterPassingDirection.Output:
                            parameterSymbol.SetFlag(Symbol.Flags.Output, true);
                            break;
                        case ParameterPassingDirection.InOut:
                            parameterSymbol.SetFlag(Symbol.Flags.Inout, true);
                            break;
                        case ParameterPassingDirection.Returning:
                            parameterSymbol.SetFlag(Symbol.Flags.Returning, true);
                            break;
                    }

                return parameterSymbol;
            }

            return null;
        }

        /// <summary>
        /// Starting a PROCEDURE DIVISION => Collect all parameters.
        /// </summary>
        /// <param name="header"></param>
        public override void StartProcedureDivision(ProcedureDivisionHeader header)
        {
            System.Diagnostics.Debug.Assert(CurrentScope != null);

            //Collect parameters and return variable and create the type for current program
            List<VariableSymbol> usings = new List<VariableSymbol>();
            IList<CallTargetParameter> usingParams = header.UsingParameters;
            if (usingParams != null)
            {
                foreach (var p in usingParams)
                {
                    var parameterSymbol = ResolveUsingParameter(p);
                    if (parameterSymbol != null)
                    {
                        usings.Add(parameterSymbol);
                    }
                }
            }
            CallTargetParameter retParam = header.ReturningParameter;
            VariableSymbol returnVar = null;
            if (retParam != null)
            {
                returnVar = ResolveUsingParameter(retParam);
            }

            CurrentScope.Type = new ScopeType(usings, returnVar);
        }

        /// <summary>
        /// Start a Function Declaration
        /// </summary>
        /// <param name="header"></param>
        public override void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {
            System.Diagnostics.Debug.Assert(CurrentNode != null);
            System.Diagnostics.Debug.Assert(object.ReferenceEquals(CurrentNode.CodeElement, header));
            FunctionDeclaration funDecl = (FunctionDeclaration) CurrentNode;
            _functionDeclStack.Push(funDecl);
            //Create a function symbol (the FunctionType will be set at end of declaration)
            FunctionSymbol funSym = new FunctionSymbol(header.FunctionName.Name);
            funDecl.SemanticData = funSym;
            //Enter the function in the current scope
            this.CurrentScope.Functions.Enter(funSym);
            //TODO SemanticDomain: store function Symbol into the root table.
            //Its owner is the current scope.
            funSym.Owner = this.CurrentScope;
            //What about function visibility.
            SetSymbolAccessModifer(funSym, header.Visibility);
            //The current scope is now the function.
            this.CurrentScope = funSym;
        }

        /// <summary>
        /// Creates a VariableSymbol for the supplied ParameterDescription node.
        /// </summary>
        /// <param name="parameter">The parameter to be handled.</param>
        /// <param name="linkageData">Linkage data symbol set.</param>
        private VariableSymbol FunctionParameter2Symbol(ParameterDescription parameter, Domain<VariableSymbol> linkageData)
        {
            VariableSymbol p = DataDefinition2Symbol(parameter, linkageData, null);
            
            //Enter the symbol in the linkage section domain
            linkageData.Enter(p);
            p.Owner = linkageData.Owner;
            return p;
        }

        public override void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
            System.Diagnostics.Debug.Assert(_functionDeclStack.Count > 0);
            FunctionDeclaration funDecl = _functionDeclStack.Peek();
            System.Diagnostics.Debug.Assert(CurrentScope is FunctionSymbol);
            FunctionSymbol funSym = (FunctionSymbol) CurrentScope;

            //Collect Function parameters.
            ParametersProfileNode funcProfile = funDecl.Profile;
            List<VariableSymbol> parameters = new List<VariableSymbol>();
            //Input
            foreach (ParameterDescription input in funcProfile.InputParameters)
            {
                VariableSymbol p = FunctionParameter2Symbol(input, funSym.LinkageData);
                p.SetFlag(Symbol.Flags.Parameter | Symbol.Flags.Input | Symbol.Flags.LINKAGE, true);
                parameters.Add(p);
            }
            foreach (ParameterDescription inout in funcProfile.InoutParameters)
            {
                VariableSymbol p = FunctionParameter2Symbol(inout, funSym.LinkageData);
                p.SetFlag(Symbol.Flags.Parameter | Symbol.Flags.Inout | Symbol.Flags.LINKAGE, true);
                parameters.Add(p);
            }
            foreach (ParameterDescription output in funcProfile.OutputParameters)
            {
                VariableSymbol p = FunctionParameter2Symbol(output, funSym.LinkageData);
                p.SetFlag(Symbol.Flags.Parameter | Symbol.Flags.Output | Symbol.Flags.LINKAGE, true);
                parameters.Add(p);
            }

            VariableSymbol retVar = null;
            if (funcProfile.ReturningParameter != null)
            {
                ParameterDescription ret = funcProfile.ReturningParameter;
                retVar = FunctionParameter2Symbol(ret, funSym.LinkageData);
                retVar.SetFlag(Symbol.Flags.Returning | Symbol.Flags.LINKAGE, true);
            }

            //Create the Function type.
            parameters.TrimExcess();
            funSym.Type = new ScopeType(parameters, retVar);

            //Pop the Function declaration context
            _functionDeclStack.Pop();
            //Also Pop CurrentScope
            System.Diagnostics.Debug.Assert(funSym.Owner is ProgramSymbol);
            CurrentScope = (ProgramSymbol) funSym.Owner;
        }

        /// <summary>
        /// Checks if the given DataDefinition instance has a single Usage definition.
        /// </summary>
        /// <param name="dataDef">The Data Definition to be checked.</param>
        /// <param name="dataUsage">The DataUsage if it has a single usage definition, None otherwise.</param>
        /// <returns>True if it has a single usage definition, False otherwise.</returns>
        private static bool HasSingleUsageDefinition(DataDefinition dataDef, out DataUsage dataUsage)
        {
            if (dataDef.Picture == null && dataDef.Usage.HasValue)
            {
                dataUsage = dataDef.Usage.Value;
                return dataUsage != DataUsage.None;
            }

            dataUsage = DataUsage.None;
            return false;
        }

        /// <summary>
        /// Checks if the given DataDefinition is a Type Definition
        /// </summary>
        /// <param name="dataDef">The Data Definition to be checked</param>
        /// <param name="entry">The DataTypeDescriptionEntry if the given instance is a Typedef, null otherwise</param>
        /// <returns>true if yes, false otherwise</returns>
        private static bool IsTypedefDefinition(DataDefinition dataDef, out DataTypeDescriptionEntry entry)
        {
            if (dataDef.CodeElement?.Type == CodeElementType.DataDescriptionEntry && dataDef.CodeElement is DataTypeDescriptionEntry dtde)
            {
                entry = dtde;
                return true;
            }

            entry = null;
            return false;
        }

        /// <summary>
        /// Determines if the given DataDefinition instance is REDEFINES
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance to be checked</param>
        /// <param name="entry">The DataRedefinesEntry if the given instance is a REDEFINES, null otherwise</param>
        /// <returns>true if yes, false otherwise</returns>
        private static bool IsRedefinedDataDefinition(DataDefinition dataDef, out DataRedefinesEntry entry)
        {
            if (dataDef.CodeElement?.Type == CodeElementType.DataRedefinesEntry)
            {
                entry = (DataRedefinesEntry) dataDef.CodeElement;
                return true;
            }

            entry = null;
            return false;
        }

        /// <summary>
        /// Create the Usage type corresponding to a DataUsage.
        /// </summary>
        /// <param name="dataUsage">The DataUsage to create the usage type.</param>
        /// <returns>The usage type</returns>
        private static Type CreateUsageType(DataUsage dataUsage)
        {
            System.Diagnostics.Debug.Assert(dataUsage != DataUsage.None);
            Type.UsageFormat usage = Type.DataUsage2UsageFormat(dataUsage);
            Type type = Builtins.GetUsageType(usage);
            return type;
        }

        /// <summary>
        /// This method handles the case if a symbol is a redefines symbol if so it creates a RedefinesSymbol instance
        /// otherwise it creates a VariableSymbol instance.
        /// The symbol will be registered in the current program if the typedef instance is null, otherwise it will
        /// be put in the typedef instance.
        /// </summary>
        /// <param name="type">The type of the Symbol to be created</param>
        /// <param name="dataDef">The DataDefinition instance from which the Symbol is created</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">The TypedefSymbol instance if the symbol to be created is a field of a Typedef symbol, null otherwise</param>
        /// <returns>The Symbol created</returns>
        private VariableSymbol CreateAndAddRedefinesOrVariableSymbol(Type type, DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            VariableSymbol sym = IsRedefinedDataDefinition(dataDef, out _)
                ? new RedefinesSymbol(dataDef.Name)
                : new VariableSymbol(dataDef.Name);

            sym.Type = type;
            DecorateSymbol(dataDef, sym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(sym);

            return sym;
        }

        /// <summary>
        /// Create a Symbol instance for a variable of a single usage type.
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance</param>
        /// <param name="dataUsage">The DataUsage of the DataDefinition instance</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Symbol instance of usage type.</returns>
        private VariableSymbol CreateUsageSymbol(DataDefinition dataDef, DataUsage dataUsage, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            Type type = CreateUsageType(dataUsage);
            return CreateAndAddRedefinesOrVariableSymbol(type, dataDef, currentDomain, typedef);
        }

        /// <summary>
        /// Create the Picture Type of the Given DataDefinition
        /// </summary>
        /// <param name="dataDef">The DataDefinition to create the Picture Type</param>
        /// <param name="picture">Picture of the DataDefinition, must be non-null</param>
        /// <returns>The Picture Type</returns>
        private static PictureType CreatePictureType(DataDefinition dataDef, AlphanumericValue picture)
        {
            System.Diagnostics.Debug.Assert(picture != null);
            Type.UsageFormat usage = dataDef.Usage.HasValue ? Type.DataUsage2UsageFormat(dataDef.Usage.Value) : Type.UsageFormat.None;
            PictureValidator pictureValidator = new PictureValidator(picture.Value, dataDef.SignIsSeparate);
            PictureType type = new PictureType(pictureValidator);
            //Use permissive Usage setter which allows COMP1 and COMP2
            type.Usage = usage;
            return type;
        }

        /// <summary>
        /// Create a Symbol instance for a variable of a single picture type.
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance</param>
        /// <param name="picture">Picture of the DataDefinition, must be non-null</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Symbol instance of usage type.</returns>
        private VariableSymbol CreatePictureSymbol(DataDefinition dataDef, AlphanumericValue picture, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            Type type = CreatePictureType(dataDef, picture);
            return CreateAndAddRedefinesOrVariableSymbol(type, dataDef, currentDomain, typedef);
        }

        /// <summary>
        /// Create an untyped symbol
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The untyped symbol</returns>
        private VariableSymbol CreateSymbolWithoutType(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            Type type = Builtins.GetUsageType(Type.UsageFormat.None);
            return CreateAndAddRedefinesOrVariableSymbol(type, dataDef, currentDomain, typedef);
        }

        /// <summary>
        /// Create a Group Symbol
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns></returns>
        private VariableSymbol CreateGroupSymbol(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            //We create a group symbol having the group type
            VariableSymbol sym = IsRedefinedDataDefinition(dataDef, out _)
                ? new RedefinesSymbol(dataDef.Name)
                : new VariableSymbol(dataDef.Name);

            //We create the group type
            GroupType recType = new GroupType(sym);
            //Set type of the symbol
            sym.Type = recType;
            //Set any leading type.
            if (HasSingleUsageDefinition(dataDef, out var dataUsage))
            {
                Type leadingType = CreateUsageType(dataUsage);
                recType.LeadingType = leadingType;
            }
            else if (dataDef.Picture != null)
            {
                Type leadingType = CreatePictureType(dataDef, dataDef.Picture);
                recType.LeadingType = leadingType;
            }

            DecorateSymbol(dataDef, sym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(sym);

            //We build the GroupType fields
            foreach (var child in dataDef.Children)
            {
                DataDefinition df = (DataDefinition)child;
                VariableSymbol dfSym = DataDefinition2Symbol(df, recType.Fields, typedef);
                //if df_sym == null this may be a bad symbol.
                if (dfSym != null)
                {
                    recType.Fields.Enter(dfSym);
                    dfSym.Owner = sym;
                }
            }

            return sym;
        }

        /// <summary>
        /// Set the access modifiers for the given symbol.
        /// </summary>
        /// <param name="symbol">The Symbol to set the access modifier</param>
        /// <param name="modifier">The access modifier</param>
        private static void SetSymbolAccessModifer(Symbol symbol, AccessModifier modifier)
        {
            switch (modifier)
            {
                case AccessModifier.Public:
                    symbol.SetFlag(Symbol.Flags.Public, true);
                    break;
                case AccessModifier.Private:
                    symbol.SetFlag(Symbol.Flags.Private, true);
                    break;
                default:
                    //Local access modifier is translated to 'no modifier' in SemanticDomain.
                    break;
            }
        }

        /// <summary>
        /// Create a type definition
        /// </summary>
        /// <param name="dataDef">The Data Definition to convert</param>
        /// <param name="entry">The associated DataTypeDescriptionEntry, must be non-null</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <returns>The Type definition symbol</returns>
        private TypedefSymbol CreateTypeDefinition(DataDefinition dataDef, DataTypeDescriptionEntry entry, Domain<VariableSymbol> currentDomain)
        {
            System.Diagnostics.Debug.Assert(entry != null);

            //TODO SemanticDomain: should we check if the type has already been defined here ?

            //We create the type definition symbol            
            var tdSym = new TypedefSymbol(dataDef.Name);
            tdSym.Type = new TypedefType(tdSym);
            if (entry.Strict != null && entry.Strict.Value)
            {
                tdSym.SetFlag(Symbol.Flags.Strict, true);
            }
            if (entry.Strong != null && entry.Strong.Value)
            {
                tdSym.SetFlag(Symbol.Flags.Strong, true);
            }
            SetSymbolAccessModifer(tdSym, entry.Visibility);
            
            //A Typedef goes in the Types domain of their declaring program.
            //Enter it right now to allow recursive type definition to be possible here.
            CurrentScope.Types.Enter(tdSym);

            //The owner of the type should also be the CurrentScope but if the typedef is
            //wrongly declared inside a group this may not be the case.
            //TypeDefinitionChecker should check that the owner is also the declaring program.
            tdSym.Owner = currentDomain.Owner;
            
            //TODO SemanticDomain: store the type into the root table.

            VariableSymbol varSym = DataDefinition2Symbol(dataDef, currentDomain, tdSym);
            //Ignore the variable symbol, but only take the underlying type.
            System.Diagnostics.Debug.Assert(varSym.Type != null);
            System.Diagnostics.Debug.Assert(tdSym.Type != null);
            System.Diagnostics.Debug.Assert(tdSym.Type.Tag == Type.Tags.Typedef);
            Type targetType = varSym.Type;
            ((TypedefType) tdSym.Type).TargetType = targetType;
            
            //Important if the target Type is a Group Type we must set the owner to the TypedefSymbol.
            if (targetType != null && targetType.Tag == Type.Tags.Group)
            {
                GroupType groupType = (GroupType) targetType;
                groupType.Fields.Owner = tdSym;
            }

            //Mark all symbol has belonging to a TYPEDEF
            tdSym.SetFlag(Symbol.Flags.InsideTypedef, true, true);
            //We do not register typedef yet but we propagate any decoration.
            DecorateSymbol(dataDef, tdSym, currentDomain);
            return tdSym;
        }

        /// <summary>
        /// Create a Symbol whose type is a type defined as a TypeDef
        /// </summary>
        /// <param name="dataDef">The Data Definition to convert</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Created Symbol</returns>
        private VariableSymbol CreateDataTypeSymbol(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            var varTypeSym = new TypedVariableSymbol(dataDef.Name);
            DecorateSymbol(dataDef, varTypeSym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(varTypeSym);

            //If we have created a TypedVariableSymbol Symbol instance then sure the underlying Program should be completed from the Top Program.
            //This can be an optimization to avoid pur Cobol85 program to be completed, they don't have TYPEDEF.
            if (!CurrentScope.HasFlag(Symbol.Flags.NeedTypeCompletion))
            {
                CurrentScope.SetFlag(Symbol.Flags.NeedTypeCompletion, true);
                ProgramSymbol toProgram = (ProgramSymbol)CurrentScope.TopParent(Symbol.Kinds.Program);
                if (toProgram != CurrentScope)
                    toProgram.SetFlag(Symbol.Flags.NeedTypeCompletion, true);
            }
            return varTypeSym;
        }

        /// <summary>
        /// Create a condition variable symbol
        /// </summary>
        /// <param name="dataDef">The DataDefinition which is a DataConditionEntry</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">The current TypedefSymbol if the DataDef is part of a TypeDefinition.</param>
        /// <returns>The Condition symbol</returns>
        private VariableSymbol CreateConditionSymbol(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            System.Diagnostics.Debug.Assert(dataDef.CodeElement?.Type == CodeElementType.DataConditionEntry);
            VariableSymbol sym = new VariableSymbol(dataDef.Name);
            sym.Type = Builtins.DataConditionType;
            DecorateSymbol(dataDef, sym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(sym);

            return sym;
        }

        /// <summary>
        /// Create a Variable Symbol that represents an Index.
        /// </summary>
        /// <param name="dataDef"></param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Index symbol</returns>
        private IndexSymbol CreateIndexSymbol(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            IndexSymbol sym = new IndexSymbol(dataDef.Name);
            DecorateSymbol(dataDef, sym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(sym);
            return sym;
        }

        /// <summary>
        /// Checks if the given DataDefinition maybe a group, that is to say
        /// It has Children but none of them are IndexDefinition.
        /// </summary>
        /// <param name="dataDef"></param>
        /// <returns></returns>
        private static bool MaybeGroup(DataDefinition dataDef)
        {
            if (dataDef.ChildrenCount == 0)
                return false;
            foreach (var child in dataDef.Children)
            {
                //IndexDefinition doesn't have CodeElement.
                if (child.CodeElement != null)
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Convert a Data Definition to a Symbol
        /// </summary>
        /// <param name="dataDef">The Data Definition to convert</param>
        /// <param name="currentDomain">The current domain of variables being built, used to look for target REDEFINES or RENAMES symbols.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns></returns>
        private VariableSymbol DataDefinition2Symbol(DataDefinition dataDef, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            VariableSymbol sym = null;
            if (dataDef.CodeElement != null)
            {
                switch (dataDef.CodeElement.Type)
                {
                    case CodeElementType.DataDescriptionEntry:
                    case CodeElementType.DataRedefinesEntry:                    
                        {
                            //Special case Typedef
                            //CreateTypeDefinition will call us back with typedef set so we test it to avoid stack overflow !
                            if (IsTypedefDefinition(dataDef, out var dataTypeDescriptionEntry) && typedef == null)
                            {
                                sym = CreateTypeDefinition(dataDef, dataTypeDescriptionEntry, currentDomain);
                            }
                            else if (dataDef.ChildrenCount == 0 && HasSingleUsageDefinition(dataDef, out var dataUsage))
                            {
                                sym = CreateUsageSymbol(dataDef, dataUsage, currentDomain, typedef);
                            }
                            else if (dataDef.ChildrenCount == 0 && dataDef.Picture != null)
                            {
                                sym = CreatePictureSymbol(dataDef, dataDef.Picture, currentDomain, typedef);
                            }
                            else
                            {
                                if (!MaybeGroup(dataDef))
                                {
                                    //Check for TYPE clause
                                    var entry = (CommonDataDescriptionAndDataRedefines) dataDef.CodeElement;
                                    if (entry.UserDefinedDataType != null)
                                    {
                                        sym = CreateDataTypeSymbol(dataDef, currentDomain, typedef);
                                    }
                                    else
                                    {
                                        sym = CreateSymbolWithoutType(dataDef, currentDomain, typedef);
                                    }
                                }
                                else
                                {//This is a group Type
                                    sym = CreateGroupSymbol(dataDef, currentDomain, typedef);
                                }
                            }
                        }
                        break;
                    case CodeElementType.DataRenamesEntry:
                        {
                            //TODO SemanticDomain: CreateRenamesSymbol
                        }
                        break;
                    case CodeElementType.DataConditionEntry:
                        {
                            sym = CreateConditionSymbol(dataDef, currentDomain, typedef);
                        }
                        break;
                }
            }

            if (sym != null)
            {
                //Consider an array
                if (dataDef.IsTableOccurence)
                {
                    //Create the ArrayType
                    ArrayType arrayType = new ArrayType(sym);
                    arrayType.MinOccur = dataDef.MinOccurencesCount;
                    arrayType.MaxOccur = dataDef.HasUnboundedNumberOfOccurences ? default(long?) : dataDef.MaxOccurencesCount;
                    arrayType.ElementType = sym.Type;
                    //Build indexes
                    foreach (var indexDef in dataDef.Children.OfType<IndexDefinition>())
                    {
                        //An index definition symbol
                        var indexSym = CreateIndexSymbol(indexDef, currentDomain, typedef);
                        //Attach the Indexed
                        indexSym.Indexed = sym;
                        //Add the index in the arrayType.
                        arrayType.Indexes.Enter(indexSym);
                        indexSym.Owner = sym;
                        if (sym.HasFlag(Symbol.Flags.Global))
                        {
                            //For a Global Index
                            indexSym.SetFlag(Symbol.Flags.Global, true);
                        }
                    }

                    //Now that the ArrayType is complete, change symbol type
                    sym.Type = arrayType;
                }                
            }
            
            return sym;
        }

        /// <summary>
        /// Decorate the given Variable Symbol, with some flags
        /// </summary>
        /// <param name="dataDef">Symbol's Data Definition</param>
        /// <param name="sym">The Symbol to be decorated</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        private void DecorateSymbol(DataDefinition dataDef, VariableSymbol sym, Domain<VariableSymbol> currentDomain)
        {
            //Set SemanticData for the DataDef
            dataDef.SemanticData = sym;
            if (sym.Owner == null)
                sym.Owner = currentDomain.Owner;
            //Section flag
            bool inGlobalStorageSection;
            if (CurrentDataDivisionSection != null)
            {
                sym.SetFlag(CurrentDataDivisionSection.Flag, true);
                inGlobalStorageSection = CurrentDataDivisionSection.Flag == Symbol.Flags.GLOBAL_STORAGE;
            }
            else
            {
                inGlobalStorageSection = false;
            }

            //Global variable ?
            if (dataDef.CodeElement != null)
            {
                switch (dataDef.CodeElement.Type)
                {
                    case CodeElementType.DataConditionEntry:
                    {
                        sym.Level = 88;
                        if (currentDomain.Owner.Kind != Symbol.Kinds.Program && currentDomain.Owner.Kind != Symbol.Kinds.Function)
                            sym.SetFlag(currentDomain.Owner.Flag & Symbol.Flags.Global , currentDomain.Owner.HasFlag(Symbol.Flags.Global));
                    }
                        break;
                    case CodeElementType.DataRenamesEntry:
                    {
                        sym.Level = 66;
                        if (currentDomain.Owner.Kind != Symbol.Kinds.Program && currentDomain.Owner.Kind != Symbol.Kinds.Function)
                            sym.SetFlag(currentDomain.Owner.Flag & Symbol.Flags.Global, currentDomain.Owner.HasFlag(Symbol.Flags.Global));
                    }
                        break;
                    case CodeElementType.DataDescriptionEntry:
                    case CodeElementType.DataRedefinesEntry:
                    {
                        CommonDataDescriptionAndDataRedefines dataDescEntry =
                            (CommonDataDescriptionAndDataRedefines)dataDef.CodeElement;                            
                        sym.Level = dataDescEntry.LevelNumber != null ? (int)dataDescEntry.LevelNumber.Value : 0;
                        sym.IsFiller = dataDescEntry.IsFiller;
                        if (dataDescEntry.IsGlobal || currentDomain.Owner.HasFlag(Symbol.Flags.Global))
                        {//No Global inside GLOBAL-STORAGE.
                            if (!inGlobalStorageSection)
                            {
                                //This a global symbol
                                sym.SetFlag(Symbol.Flags.Global, true);
                            }
                        }

                        //Other interesting flags that apply to a symbol.
                        if (dataDescEntry.IsBlankWhenZero != null && dataDescEntry.IsBlankWhenZero.Value)
                            sym.SetFlag(Symbol.Flags.BlankWhenZero, true);
                        if (dataDescEntry.IsJustified != null && dataDescEntry.IsJustified.Value)
                            sym.SetFlag(Symbol.Flags.Justified, true);
                        if (dataDescEntry.IsGroupUsageNational != null && dataDescEntry.IsGroupUsageNational.Value)
                            sym.SetFlag(Symbol.Flags.GroupUsageNational, true);
                        if (dataDescEntry.SignIsSeparate != null && dataDescEntry.SignIsSeparate.Value)
                            sym.SetFlag(Symbol.Flags.SeparateSign, true);
                        if (dataDescEntry.SignPosition != null)
                        {
                            if (dataDescEntry.SignPosition.Value == SignPosition.Leading)
                                sym.SetFlag(Symbol.Flags.LeadingSign, true);
                            if (dataDescEntry.SignPosition.Value == SignPosition.Trailing)
                                sym.SetFlag(Symbol.Flags.TrailingSign, true);
                        }
                        if (dataDescEntry.IsSynchronized != null && dataDescEntry.IsSynchronized.Value)
                            sym.SetFlag(Symbol.Flags.Sync, true);
                        if (dataDef.CodeElement.Type == CodeElementType.DataDescriptionEntry)
                        {
                            DataDescriptionEntry dataDesc = (DataDescriptionEntry) dataDef.CodeElement;
                            if (dataDesc.IsExternal)
                                sym.SetFlag(Symbol.Flags.External, true);
                        }
                    }
                        break;
                    default:
                        System.Diagnostics.Debug.Fail("Unsupported CodeElement type in DecorateSymbol !");
                        break;
                }
            }
        }

        /// <summary>
        /// Level1 Definition Tracker, This tracker is used to create all DataDefinition symbols.
        /// </summary>
        /// <param name="level1Node">The level 1 definition node</param>
        public override void OnLevel1Definition(DataDefinition level1Node)
        {
            var sectionVariables = CurrentDataDivisionSection.Variables;
            var sectionFlag = CurrentDataDivisionSection.Flag;

            VariableSymbol dataDefSym = DataDefinition2Symbol(level1Node, sectionVariables, null);
            if (dataDefSym != null)
            {
                //TODO SemanticDomain: we must validate all RENAMES at a 01 Level definition

                System.Diagnostics.Debug.Assert(CurrentScope != null);

                if (dataDefSym.Owner == null) //Because Symbols as TYPEDEF already have their parent.
                    dataDefSym.Owner = CurrentScope;

                if (dataDefSym.Kind != Symbol.Kinds.Typedef) //Typedef are already entered at creation time.
                {
                    sectionVariables.Enter(dataDefSym);
                    dataDefSym.SetFlag(sectionFlag, true);
                }
            }
        }

        public override void StartSection(SectionHeader header)
        {
            //Create Section symbol and enter it into current scope
            var section = new SectionSymbol(header.SectionName.Name) {Owner = CurrentScope};
            CurrentScope.Sections.Enter(section);

            //Update CurrentNode SemanticData
            System.Diagnostics.Debug.Assert(CurrentNode != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement.Type == CodeElementType.SectionHeader);
            CurrentNode.SemanticData = section;

            //Track current section
            System.Diagnostics.Debug.Assert(CurrentSection == null);
            CurrentSection = section;
        }

        public override void EndSection()
        {
            System.Diagnostics.Debug.Assert(CurrentSection != null);
            CurrentSection = null;
        }

        public override void StartParagraph(ParagraphHeader header)
        {
            var paragraph = new ParagraphSymbol(header.ParagraphName.Name);
            if (CurrentSection != null)
            {
                //Attach paragraph to section
                CurrentSection.AddParagraph(paragraph);
            }
            else
            {
                //Attach paragraph to program/function
                paragraph.Owner = CurrentScope;
                CurrentScope.Paragraphs.Enter(paragraph);
            }

            //Update CurrentNode SemanticData
            System.Diagnostics.Debug.Assert(CurrentNode != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement.Type == CodeElementType.ParagraphHeader);
            CurrentNode.SemanticData = paragraph;
        }
    }
}
