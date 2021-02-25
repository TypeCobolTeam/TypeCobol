using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
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
        /// The current ProgramSymbol being built.
        /// </summary>
        private ProgramSymbol CurrentProgram
        {
            get;
            set;
        }

        /// <summary>
        /// The current FunctionSymbol being built.
        /// </summary>
        private FunctionSymbol CurrentFunction
        {
            get;
            set;
        }

        /// <summary>
        /// The current scope : this is the CurrentFunction if any, otherwise the CurrentProgram.
        /// </summary>
        private ScopeSymbol CurrentScope => CurrentFunction ?? (ScopeSymbol) CurrentProgram;

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
        /// Keeps track of last visited FunctionDeclaration node
        /// </summary>
        private FunctionDeclaration LastFunctionDeclaration
        {
            get;
            set;
        }
        
        /// <summary>
        /// Current section in the procedure division.
        /// </summary>
        private SectionSymbol CurrentProcedureDivisionSection
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

            //Create the program
            var program = new ProgramSymbol(programIdentification.ProgramName.Name)
                          {
                              //Set empty ScopeType for now, the parameters and return variable will be resolved later
                              Type = new ScopeType(new List<VariableSymbol>(), null)
                          };
            if (this.CurrentProgram == null)
            {
                //This is the main program or a stacked program.
                //TODO SemanticDomain: test for duplicate and enter program into Root.

                //Add the new Stacked program into our result list.
                Programs.Add(program);
            }
            else
            {
                //This is a nested program.
                //TODO SemanticDomain: store nestedProgram into the root table.
                System.Diagnostics.Debug.Assert(CurrentNode.Parent != null);
                System.Diagnostics.Debug.Assert(CurrentNode.Parent.CodeElement != null);
                System.Diagnostics.Debug.Assert(CurrentNode.Parent.CodeElement.Type == CodeElementType.ProgramIdentification);

                //Enter the program as nested here.
                this.CurrentProgram.Programs.Enter(program);
            }

            //Set the current program
            this.CurrentProgram = program;

            //Semantic data on the node
            CurrentNode.SemanticData = program;
        }

        public override void EndCobolProgram(ProgramEnd end)
        {
            //For a stacked program the Owner is its Namespace so this will correctly
            //reset the CurrentProgram to null, for a nested Program, the Owner is the enclosing Program.
            this.CurrentProgram = CurrentProgram.Owner as ProgramSymbol;
        }

        public override void StartDataDivision(DataDivisionHeader header)
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartFileSection(FileSectionHeader header)
        {
            //No FILE SECTION in a procedure, look into CurrentProgram
            if (CurrentFunction == null)
            {
                System.Diagnostics.Debug.Assert(CurrentProgram != null);
                CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.FILE, CurrentProgram.FileData);
            }
        }

        public override void EndFileSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {
            //No GLOBAL STORAGE SECTION in a procedure, look into CurrentProgram
            if (CurrentFunction == null)
            {
                System.Diagnostics.Debug.Assert(CurrentProgram != null);
                CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.GLOBAL_STORAGE, CurrentProgram.GlobalStorageData);
            }
        }

        public override void EndGlobalStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {
            System.Diagnostics.Debug.Assert(CurrentScope != null);
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.WORKING_STORAGE, CurrentScope.WorkingStorageData);
        }

        public override void EndWorkingStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartLocalStorageSection(LocalStorageSectionHeader header)
        {
            System.Diagnostics.Debug.Assert(CurrentScope != null);
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.LOCAL_STORAGE, CurrentScope.LocalStorageData);
        }

        public override void EndLocalStorageSection()
        {
            CurrentDataDivisionSection = null;
        }

        public override void StartLinkageSection(LinkageSectionHeader header)
        {
            System.Diagnostics.Debug.Assert(CurrentScope != null);
            CurrentDataDivisionSection = new DataDivisionSection(Symbol.Flags.LINKAGE, CurrentScope.LinkageData);
        }

        public override void EndLinkageSection()
        {
            CurrentDataDivisionSection = null;
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
            System.Diagnostics.Debug.Assert(LastFunctionDeclaration == null);
            LastFunctionDeclaration = funDecl;
            //Create a function symbol
            FunctionSymbol funSym = new FunctionSymbol(header.FunctionName.Name);
            funDecl.SemanticData = funSym;
            //Enter the function in the current program
            this.CurrentProgram.Functions.Enter(funSym);
            //TODO SemanticDomain: store function Symbol into the root table.
            //What about function visibility.
            SetSymbolAccessModifer(funSym, header.Visibility);
            //The current scope is now the function.
            this.CurrentFunction = funSym;


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

            return p;
        }

        public override void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
            System.Diagnostics.Debug.Assert(LastFunctionDeclaration != null);
            System.Diagnostics.Debug.Assert(CurrentFunction != null);
            
            //Pop the Function declaration context
            LastFunctionDeclaration = null;
            //Also Pop CurrentScope
            CurrentFunction = null;
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
        /// Create the Usage type corresponding to a UsageFormat.
        /// </summary>
        /// <param name="usage">The UsageFormat to create the usage type, expected to be different from None.</param>
        /// <returns>The corresponding usage type.</returns>
        private static Type CreateUsageType(Type.UsageFormat usage)
        {
            System.Diagnostics.Debug.Assert(usage != Type.UsageFormat.None);
            return Builtins.GetUsageType(usage);
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
        /// <param name="usage">The Usage of the DataDefinition instance</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Symbol instance of usage type.</returns>
        private VariableSymbol CreateUsageSymbol(DataDefinition dataDef, Type.UsageFormat usage, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            Type type = CreateUsageType(usage);
            return CreateAndAddRedefinesOrVariableSymbol(type, dataDef, currentDomain, typedef);
        }

        /// <summary>
        /// Create the Picture Type of the Given DataDefinition
        /// </summary>
        /// <param name="dataDef">The DataDefinition to create the Picture Type</param>
        /// <param name="picture">Picture of the DataDefinition, must be non-null</param>
        /// <param name="usage">Usage of the DataDefinition, may be None to reflect the absence of usage clause</param>
        /// <returns>The Picture Type</returns>
        private static PictureType CreatePictureType(DataDefinition dataDef, AlphanumericValue picture, Type.UsageFormat usage)
        {
            System.Diagnostics.Debug.Assert(picture != null);
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
        /// <param name="usage">Usage of the DataDefinition, may be None</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns>The Symbol instance of usage type.</returns>
        private VariableSymbol CreatePictureSymbol(DataDefinition dataDef, AlphanumericValue picture, Type.UsageFormat usage, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
        {
            Type type = CreatePictureType(dataDef, picture, usage);
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
            return CreateAndAddRedefinesOrVariableSymbol(Builtins.NoType, dataDef, currentDomain, typedef);
        }

        /// <summary>
        /// Create a Group Symbol
        /// </summary>
        /// <param name="dataDef">The DataDefinition instance</param>
        /// <param name="picture">Picture value of the DataDefinition</param>
        /// <param name="usage">Usage value of the DataDefinition</param>
        /// <param name="currentDomain">The current domain of variables being built.</param>
        /// <param name="typedef">not null if  we have been called by a TYPEDEF declaration, null otherwise</param>
        /// <returns></returns>
        private VariableSymbol CreateGroupSymbol(DataDefinition dataDef, AlphanumericValue picture, Type.UsageFormat usage, Domain<VariableSymbol> currentDomain, TypedefSymbol typedef)
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
            if (picture != null)
            {
                recType.LeadingType = CreatePictureType(dataDef, picture, usage);
            }
            else if (usage != Type.UsageFormat.None)
            {
                recType.LeadingType = CreateUsageType(usage);
            }

            DecorateSymbol(dataDef, sym, currentDomain);
            if (typedef == null)
                CurrentScope.Add(sym);

            //We build the GroupType fields
            foreach (var child in dataDef.Children)
            {
                DataDefinition df = (DataDefinition) child;
                VariableSymbol dfSym = DataDefinition2Symbol(df, recType.Fields, typedef);
                //if df_sym == null this may be a bad symbol.
                if (dfSym != null)
                {
                    recType.Fields.Enter(dfSym);
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
            switch (entry.RestrictionLevel)
            {
                case RestrictionLevel.STRICT:
                    tdSym.SetFlag(Symbol.Flags.Strict, true);
                    break;
                case RestrictionLevel.STRONG:
                    tdSym.SetFlag(Symbol.Flags.Strong, true);
                    break;
                case RestrictionLevel.WEAK:
                    tdSym.SetFlag(Symbol.Flags.Weak, true);
                    break;
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
                GroupType groupType = (GroupType)targetType;
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
                            //Extract Picture and Usage once to avoid multiple redundant calls
                            var picture = dataDef.Picture;
                            var dataUsage = dataDef.Usage;
                            var usage = dataUsage.HasValue ? Type.DataUsage2UsageFormat(dataUsage.Value) : Type.UsageFormat.None;

                            //Special case Typedef
                            //CreateTypeDefinition will call us back with typedef set so we test it to avoid stack overflow !
                            if (IsTypedefDefinition(dataDef, out var dataTypeDescriptionEntry) && typedef == null)
                            {
                                sym = CreateTypeDefinition(dataDef, dataTypeDescriptionEntry, currentDomain);
                            }
                            else if (dataDef.ChildrenCount == 0 && picture == null && usage != Type.UsageFormat.None)
                            {
                                //Single usage definition
                                sym = CreateUsageSymbol(dataDef, usage, currentDomain, typedef);
                            }
                            else if (dataDef.ChildrenCount == 0 && picture != null)
                            {
                                //Single picture (and possibly usage) definition
                                sym = CreatePictureSymbol(dataDef, picture, usage, currentDomain, typedef);
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
                                    sym = CreateGroupSymbol(dataDef, picture, usage, currentDomain, typedef);
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
                    ArrayType arrayType = new ArrayType();
                    arrayType.MinOccur = dataDef.MinOccurencesCount;
                    arrayType.MaxOccur = dataDef.HasUnboundedNumberOfOccurences ? default(long?) : dataDef.MaxOccurencesCount;
                    arrayType.ElementType = sym.Type;
                    //Now that the ArrayType is complete, change symbol type
                    sym.Type = arrayType;

                    //Build indexes
                    foreach (var indexDef in dataDef.Children.OfType<IndexDefinition>())
                    {
                        //An index definition symbol
                        var indexSym = CreateIndexSymbol(indexDef, currentDomain, typedef);
                        //Add the index in the variable.
                        sym.Indexes.Enter(indexSym);
                        //Copy Global flag for a Global Index
                        if (sym.HasFlag(Symbol.Flags.Global))
                        {
                            indexSym.SetFlag(Symbol.Flags.Global, true);
                        }
                    }
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

            //Section flag
            if (CurrentDataDivisionSection != null)
                sym.SetFlag(CurrentDataDivisionSection.Flag, true);

            //Apply flags read from Node
            if (dataDef.IsJustified)
                sym.SetFlag(Symbol.Flags.Justified, true);
            if (dataDef.IsGroupUsageNational)
                sym.SetFlag(Symbol.Flags.GroupUsageNational, true);
            if (dataDef.SignIsSeparate)
                sym.SetFlag(Symbol.Flags.SeparateSign, true);
            switch (dataDef.SignPosition)
            {
                case SignPosition.Leading:
                    sym.SetFlag(Symbol.Flags.LeadingSign, true);
                    break;
                case SignPosition.Trailing:
                    sym.SetFlag(Symbol.Flags.TrailingSign, true);
                    break;
            }
            if (dataDef.Synchronized != null)
                sym.SetFlag(Symbol.Flags.Sync, true);

            //Set level and other flags: Global, BlankWhenZero and External are read from CodeElement
            if (dataDef.CodeElement != null)
            {
                switch (dataDef.CodeElement.Type)
                {
                    case CodeElementType.DataConditionEntry:
                    {
                        sym.Level = 88;
                        //Does the variable inherits from parent its Global flag ?
                        if (currentDomain.Owner.Kind != Symbol.Kinds.Program && currentDomain.Owner.Kind != Symbol.Kinds.Function)
                            sym.SetFlag(currentDomain.Owner.Flag & Symbol.Flags.Global, currentDomain.Owner.HasFlag(Symbol.Flags.Global));
                        //Store Condition Values.
                        sym.Value = ((DataConditionEntry) dataDef.CodeElement).ConditionValues;
                    }
                        break;
                    case CodeElementType.DataRenamesEntry:
                    {
                        sym.Level = 66;
                        //Does the variable inherits from parent its Global flag ?
                        if (currentDomain.Owner.Kind != Symbol.Kinds.Program && currentDomain.Owner.Kind != Symbol.Kinds.Function)
                            sym.SetFlag(currentDomain.Owner.Flag & Symbol.Flags.Global, currentDomain.Owner.HasFlag(Symbol.Flags.Global));
                    }
                        break;
                    case CodeElementType.DataDescriptionEntry:
                    case CodeElementType.DataRedefinesEntry:
                    {
                        var dataDescOrRedefines = (CommonDataDescriptionAndDataRedefines) dataDef.CodeElement;
                        sym.Value = dataDescOrRedefines.InitialValue;
                        sym.Level = dataDescOrRedefines.LevelNumber != null ? (int) dataDescOrRedefines.LevelNumber.Value : 0;
                        sym.IsFiller = dataDescOrRedefines.IsFiller;

                        //Global flag explicitly set or inherited
                        if (dataDescOrRedefines.IsGlobal || currentDomain.Owner.HasFlag(Symbol.Flags.Global))
                        {
                            sym.SetFlag(Symbol.Flags.Global, true);
                        }

                        //BlankWhenZero flag
                        if (dataDescOrRedefines.IsBlankWhenZero != null && dataDescOrRedefines.IsBlankWhenZero.Value)
                            sym.SetFlag(Symbol.Flags.BlankWhenZero, true);

                        //External flag
                        if (dataDescOrRedefines.Type == CodeElementType.DataDescriptionEntry)
                        {
                            DataDescriptionEntry dataDesc = (DataDescriptionEntry) dataDescOrRedefines;
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
            if (CurrentDataDivisionSection != null)
            {
                var sectionVariables = CurrentDataDivisionSection.Variables;
                var sectionFlag = CurrentDataDivisionSection.Flag;

                VariableSymbol dataDefSym = DataDefinition2Symbol(level1Node, sectionVariables, null);
                if (dataDefSym != null)
                {
                    //TODO SemanticDomain: we must validate all RENAMES at a 01 Level definition

                    if (dataDefSym.Kind != Symbol.Kinds.Typedef) //Typedef are already entered at creation time.
                    {
                        sectionVariables.Enter(dataDefSym);
                        dataDefSym.SetFlag(sectionFlag, true);
                    }
                }
            }
        }

        public override void StartSection(SectionHeader header)
        {
            //Create Section symbol and enter it into current scope
            var section = new SectionSymbol(header.SectionName.Name);
            CurrentScope.Sections.Enter(section);

            //Update CurrentNode SemanticData
            System.Diagnostics.Debug.Assert(CurrentNode != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement != null);
            System.Diagnostics.Debug.Assert(CurrentNode.CodeElement.Type == CodeElementType.SectionHeader);
            CurrentNode.SemanticData = section;

            //Track current section
            System.Diagnostics.Debug.Assert(CurrentProcedureDivisionSection == null);
            CurrentProcedureDivisionSection = section;
        }

        public override void EndSection()
        {
            System.Diagnostics.Debug.Assert(CurrentProcedureDivisionSection != null);
            CurrentProcedureDivisionSection = null;
        }

        public override void StartParagraph(ParagraphHeader header)
        {
            var paragraph = new ParagraphSymbol(header.ParagraphName.Name);
            if (CurrentProcedureDivisionSection != null)
            {
                //Enter paragraph into current section
                CurrentProcedureDivisionSection.Paragraphs.Enter(paragraph);
            }
            else
            {
                //Attach paragraph directly to program/function
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
