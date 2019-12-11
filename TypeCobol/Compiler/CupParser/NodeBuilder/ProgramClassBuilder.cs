﻿using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// The Program Class Builder
    /// </summary>
    public class ProgramClassBuilder : IProgramClassBuilder
    {
        public SyntaxTree SyntaxTree { get; set; }

        private TypeDefinition _CurrentTypeDefinition
        {
            get => _currentTypeDefinition;
            set
            {
                _currentTypeDefinition = value;
                //Reset that this type was already added to the list TypeThatNeedTypeLinking
                typeAlreadyAddedToTypeToLink = false;
            }
        }

        private bool typeAlreadyAddedToTypeToLink = false;

        private bool _IsInsideWorkingStorageContext;
        private bool _IsInsideLinkageSectionContext;
        private bool _IsInsideLocalStorageSectionContext;
        private bool _IsInsideFileSectionContext;
        private bool _IsInsideGlobalStorageSection;
        private FunctionDeclaration _ProcedureDeclaration;

        public List<DataDefinition> TypedVariablesOutsideTypedef { get; } = new List<DataDefinition>();
        public List<TypeDefinition> TypeThatNeedTypeLinking { get; } = new List<TypeDefinition>();

        // Programs can be nested => track current programs being analyzed
        private Stack<Program> programsStack;

        private Program CurrentProgram
        {
            get { return programsStack.Count > 0 ? programsStack.Peek() : null; }
            set { programsStack.Push(value); }
        }

        /// <summary>Class object resulting of the visit the parse tree</summary>
        public CodeModel.Class Class { get; private set; }

        private readonly SymbolTable TableOfIntrinsics;
        private readonly SymbolTable TableOfNamespaces;

        public ProgramClassBuilder()
        {
            // Intrinsics and Namespaces always exist. Intrinsic table has no enclosing scope.
            TableOfIntrinsics = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
            TableOfNamespaces = new SymbolTable(TableOfIntrinsics, SymbolTable.Scope.Namespace);

            programsStack = new Stack<Program>();
        }

        public SymbolTable CustomSymbols
        {
            set
            {
                if (value != null)
                {
                    SymbolTable intrinsicTable = value.GetTableFromScope(SymbolTable.Scope.Intrinsic);
                    SymbolTable nameSpaceTable = value.GetTableFromScope(SymbolTable.Scope.Namespace);

                    intrinsicTable.DataEntries.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));
                    intrinsicTable.Types.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));
                    intrinsicTable.Functions.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));

                    TableOfIntrinsics.CopyAllDataEntries(intrinsicTable.DataEntries.Values);
                    TableOfIntrinsics.CopyAllTypes(intrinsicTable.Types);
                    TableOfIntrinsics.CopyAllFunctions(intrinsicTable.Functions, AccessModifier.Public);

                    if (nameSpaceTable != null)
                    {
                        TableOfNamespaces.CopyAllPrograms(nameSpaceTable.Programs.Values);
                    }

                }

                // TODO#249: use a COPY for these
                foreach (var type in DataType.BuiltInCustomTypes)
                {
                    var createdType = DataType.CreateBuiltIn(type);
                    TableOfIntrinsics.AddType(createdType); //Add default TypeCobol types BOOLEAN and DATE
                    //Add type and children to DataTypeEntries dictionary in Intrinsic symbol table
                    TableOfIntrinsics.AddDataDefinitionsUnderType(createdType);
                }
            }
        }



        public NodeDispatcher Dispatcher { get; internal set; }

        public Node CurrentNode { get { return SyntaxTree.CurrentNode; } }

        public Dictionary<CodeElement, Node> NodeCodeElementLinkers = new Dictionary<CodeElement, Node>();

        /// <summary>
        /// The Last entered node.
        /// </summary>
        public Node LastEnteredNode
        {
            get;
            private set;
        }

        private void Enter(Node node, CodeElement context = null, SymbolTable table = null)
        {
            node.SymbolTable = table ?? SyntaxTree.CurrentNode.SymbolTable;
            if (_ProcedureDeclaration != null)
            {
                node.SetFlag(Node.Flag.InsideProcedure, true);      //Set flag to know that this node belongs a Procedure or Function
            }
            SyntaxTree.Enter(node);

            if (node.CodeElement != null)
                NodeCodeElementLinkers.Add(node.CodeElement, node);
        }

        private void Exit()
        {
            var node = SyntaxTree.CurrentNode;
            Dispatcher.OnNode(node, CurrentProgram);
            SyntaxTree.Exit();
            LastEnteredNode = node;
        }

        private void AttachEndIfExists(CodeElementEnd end)
        {
            if (end != null)
            {
                Enter(new End(end));
                Exit();
            }
        }

        private Node GetTopLevelItem(long level)
        {
            var parent = CurrentNode;
            while (parent != null)
            {
                var data = parent.CodeElement as DataDefinitionEntry;
                if (data == null) return null;
                if (data.LevelNumber == null || data.LevelNumber.Value < level) return parent;
                parent = parent.Parent;
            }
            return null;
        }

        /// <summary>Exit() every Node that is not the top-level item for a data of a given level.</summary>
        /// <param name="levelnumber">
        /// Level number of the next data definition that will be Enter()ed.
        /// If null, a value of 1 is assumed.
        /// </param>
        private void SetCurrentNodeToTopLevelItem(IntegerValue levelnumber)
        {
            long level = levelnumber != null ? levelnumber.Value : 1;
            Node parent;

            if (level == 1 || level == 77)
            {
                parent = null;
            }
            else
            {
                parent = GetTopLevelItem(level);
            }
            if (parent != null)
            {
                // Exit() previous sibling and all of its last children
                while (parent != CurrentNode) Exit();
            }
            else
            {
                ExitLastLevel1Definition();
            }
        }

        /// <summary>Exit last level-01 data definition entry, as long as all its subordinates.</summary>
        private void ExitLastLevel1Definition()
        {
            _CurrentTypeDefinition = null;
            while (CurrentNode.CodeElement != null && CurrentNode.CodeElement is DataDefinitionEntry) Exit();
        }

        public virtual void StartCobolCompilationUnit()
        {
            SyntaxTree.Root.SymbolTable = TableOfNamespaces; //Set SymbolTable of SourceFile Node, Limited to NameSpace and Intrinsic scopes
        }

        public virtual void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
            if (CurrentProgram == null)
            {
                if (SyntaxTree.Root.MainProgram == null)
                {
                    // Main
                    var mainProgram = new SourceProgram(TableOfNamespaces, programIdentification);
                    SyntaxTree.Root.MainProgram = mainProgram;
                    CurrentProgram = mainProgram;
                }
                else
                {
                    // Stacked
                    CurrentProgram = new StackedProgram(TableOfNamespaces, programIdentification);                    
                }
            }
            else
            {
                // Nested
                CurrentProgram = new NestedProgram(CurrentProgram, programIdentification);
            }

            Enter(CurrentProgram, programIdentification, CurrentProgram.SymbolTable);

            if (libraryCopy != null)
            { // TCRFUN_LIBRARY_COPY
                var cnode = new LibraryCopy(libraryCopy);
                Enter(cnode, libraryCopy, CurrentProgram.SymbolTable);
                Exit();
            }

            TableOfNamespaces.AddProgram(CurrentProgram); //Add Program to Namespace table. 
        }

        public virtual void EndCobolProgram(TypeCobol.Compiler.CodeElements.ProgramEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            programsStack.Pop();
        }

        public virtual void StartEnvironmentDivision(EnvironmentDivisionHeader header)
        {
            Enter(new EnvironmentDivision(header), header);
        }

        public virtual void EndEnvironmentDivision()
        {
            Exit();
        }

        public virtual void StartConfigurationSection(ConfigurationSectionHeader header)
        {
            Enter(new ConfigurationSection(header), header);
        }

        public virtual void EndConfigurationSection()
        {
            Exit();
        }

        public virtual void StartSourceComputerParagraph(SourceComputerParagraph paragraph)
        {
            Enter(new SourceComputer(paragraph));
        }

        public virtual void EndSourceComputerParagraph()
        {
            Exit();
        }

        public virtual void StartObjectComputerParagraph(ObjectComputerParagraph paragraph)
        {
            Enter(new ObjectComputer(paragraph));
        }

        public virtual void EndObjectComputerParagraph()
        {
            Exit();
        }

        public virtual void StartSpecialNamesParagraph(SpecialNamesParagraph paragraph)
        {
            Enter(new SpecialNames(paragraph));
        }

        public virtual void EndSpecialNamesParagraph()
        {
            Exit();
        }

        public virtual void StartRepositoryParagraph(RepositoryParagraph paragraph)
        {
            Enter(new Repository(paragraph));
        }

        public virtual void EndRepositoryParagraph()
        {
            Exit();
        }

        public virtual void StartInputOutputSection(InputOutputSectionHeader header)
        {
            Enter(new InputOutputSection(header), header);
        }

        public virtual void EndInputOutputSection()
        {
            Exit();
        }

        public virtual void StartFileControlParagraph(FileControlParagraphHeader header)
        {
            Enter(new FileControlParagraphHeaderNode(header), header);
        }

        public virtual void EndFileControlParagraph()
        {
            Exit();
        }

        public virtual void StartFileControlEntry(FileControlEntry entry)
        {
            var fileControlEntry = new FileControlEntryNode(entry);
            Enter(fileControlEntry, entry);
        }

        public virtual void EndFileControlEntry()
        {
            Exit();
        }

        public virtual void StartDataDivision(DataDivisionHeader header)
        {
            Enter(new DataDivision(header), header);
        }

        public virtual void EndDataDivision()
        {
            Exit();
        }

        public virtual void StartFileSection(FileSectionHeader header)
        {
            Enter(new FileSection(header), header);
            _IsInsideFileSectionContext = true;
        }

        public virtual void EndFileSection()
        {
            ExitLastLevel1Definition();
            Exit();
            _IsInsideFileSectionContext = false;
        }

        public virtual void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {
            GlobalStorageSection gs = new GlobalStorageSection(header);
            Enter(gs, header, SyntaxTree.Root.MainProgram.SymbolTable.GetTableFromScope(SymbolTable.Scope.Program));
            _IsInsideGlobalStorageSection = true;
        }

        public virtual void EndGlobalStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit GlobalStorageSection
            _IsInsideGlobalStorageSection = false;
        }

        public virtual void StartFileDescriptionEntry(FileDescriptionEntry entry)
        {
            ExitLastLevel1Definition();
            Enter(new FileDescriptionEntryNode(entry), entry);
        }

        public virtual void EndFileDescriptionEntry()
        {            
            Exit();
        }

        public virtual void EndFileDescriptionEntryIfAny()
        {
            if (this.CurrentNode is FileDescriptionEntryNode)
            {
                EndFileDescriptionEntry();
            }
        }

        public virtual void StartDataDescriptionEntry(DataDescriptionEntry entry)
        {
            var dataTypeDescriptionEntry = entry as DataTypeDescriptionEntry;
            if (dataTypeDescriptionEntry != null) StartTypeDefinitionEntry(dataTypeDescriptionEntry);
            else
            {
                SetCurrentNodeToTopLevelItem(entry.LevelNumber);

                var symbolTable = SyntaxTree.CurrentNode.SymbolTable;
                if (entry.IsGlobal)
                    symbolTable = symbolTable.GetTableFromScope(SymbolTable.Scope.Global);

                //Update DataType of CodeElement by searching info on the declared Type into SymbolTable.
                //Note that the AST is not complete here, but you can only refer to a Type that has previously been defined.
                var node = new DataDescription(entry);
                if (_CurrentTypeDefinition != null)
                    node.ParentTypeDefinition = _CurrentTypeDefinition;
                Enter(node, null, symbolTable);

                if (entry.Indexes != null && entry.Indexes.Any())
                {
                    
                    foreach (var index in entry.Indexes)
                    {
                        var indexNode = new IndexDefinition(index);
                        Enter(indexNode, null, symbolTable);
                        if (_CurrentTypeDefinition != null)
                            indexNode.ParentTypeDefinition = _CurrentTypeDefinition;
                        symbolTable.AddVariable(indexNode);
                        Exit();
                    }
                }


                if(_IsInsideWorkingStorageContext)
                    node.SetFlag(Node.Flag.WorkingSectionNode, true);      //Set flag to know that this node belongs to Working Storage Section
                if(_IsInsideLinkageSectionContext)               
                    node.SetFlag(Node.Flag.LinkageSectionNode, true);      //Set flag to know that this node belongs to Linkage Section
                if(_IsInsideLocalStorageSectionContext)
                    node.SetFlag(Node.Flag.LocalStorageSectionNode, true); //Set flag to know that this node belongs to Local Storage Section
                if (_IsInsideFileSectionContext)
                    node.SetFlag(Node.Flag.FileSectionNode, true);         //Set flag to know that this node belongs to File Section
                if (_IsInsideGlobalStorageSection)
                    node.SetFlag(Node.Flag.GlobalStorageSection, true);         //Set flag to know that this node belongs to Global Storage Section

                node.SymbolTable.AddVariable(node);
                CheckIfItsTyped(node, node.CodeElement);
            }
        }

        private void CheckIfItsTyped(DataDefinition dataDefinition, CommonDataDescriptionAndDataRedefines commonDataDescriptionAndDataRedefines)
        {
            //Is a type referenced
            if (commonDataDescriptionAndDataRedefines.UserDefinedDataType != null)
            {
                if (_CurrentTypeDefinition != null)
                {
                    _CurrentTypeDefinition.TypedChildren.Add(dataDefinition);
                }
                else
                {
                    TypedVariablesOutsideTypedef.Add(dataDefinition);
                }
            }

            //Special case for Depending On.
            //Depending on inside typedef can reference other variable declared in type referenced in this typedef
            //To resolve variable after "depending on" we first have to resolve all types used in this typedef.
            //This resolution must be recursive until all sub types have been resolved.
            if (commonDataDescriptionAndDataRedefines.OccursDependingOn != null)
            {
                if (_CurrentTypeDefinition != null && !typeAlreadyAddedToTypeToLink)
                {
                    TypeThatNeedTypeLinking.Add(_CurrentTypeDefinition);
                    typeAlreadyAddedToTypeToLink = true;
                }
            }
        }


        public virtual void StartDataRedefinesEntry(DataRedefinesEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var symbolTable = SyntaxTree.CurrentNode.SymbolTable;
            if (entry.IsGlobal)
                symbolTable = symbolTable.GetTableFromScope(SymbolTable.Scope.Global);

            var node = new DataRedefines(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node, null, symbolTable);
            node.SymbolTable.AddVariable(node);

            CheckIfItsTyped(node, node.CodeElement);
        }

        public virtual void StartDataRenamesEntry(DataRenamesEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var node = new DataRenames(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node);
            node.SymbolTable.AddVariable(node);
        }

        public virtual void StartDataConditionEntry(DataConditionEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var node = new DataCondition(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node);
            node.SymbolTable.AddVariable(node);
        }

        public virtual void StartTypeDefinitionEntry(DataTypeDescriptionEntry typedef)
        {
            SetCurrentNodeToTopLevelItem(typedef.LevelNumber);

            // TCTYPE_GLOBAL_TYPEDEF
            SymbolTable symbolTable;
            if (typedef.IsGlobal && _ProcedureDeclaration == null)
            {
                // nearest global scope
                symbolTable = CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.Global);
            }
            else if (typedef.HasExplicitVisibility)
            {
                // public or private types are stored in the nearest Program scope.
                // In Main or Nested Program it will be Program Scope from Main and in Stacked it will be Program Scope from Stacked itself.
                symbolTable = CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.Program);
            }
            else
            {
                // Local types are directly stored in the SymbolTable of the Node that declares them.
                // It can be a Local scope (in Main, Nested or Stacked programs) or a Function scope (in procedures).
                symbolTable = CurrentNode.SymbolTable;
            }

            var node = new TypeDefinition(typedef);
            Enter(node, null, symbolTable);

            //GLOBALSS_NOTYPEDEF
            //No TypeDefs are allowed to be defined in the Global Storage Section
            if (_IsInsideGlobalStorageSection)
            {
                node.SetFlag(Node.Flag.GlobalStorageSection, true);
            }
            else
            {
                symbolTable.AddType(node);
            }


            _CurrentTypeDefinition = node;
            CheckIfItsTyped(node, node.CodeElement);
        }

        public virtual void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {
            Enter(new WorkingStorageSection(header), header);
            _IsInsideWorkingStorageContext = true;
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
        }

        public virtual void EndWorkingStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit WorkingStorageSection
            _IsInsideWorkingStorageContext = false;
        }

        public virtual void StartLocalStorageSection(LocalStorageSectionHeader header)
        {
            Enter(new LocalStorageSection(header), header);
            _IsInsideLocalStorageSectionContext = true;
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
        }

        public virtual void EndLocalStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LocalStorageSection
            _IsInsideLocalStorageSectionContext = false;
        }

        public virtual void StartLinkageSection(LinkageSectionHeader header)
        {
            Enter(new LinkageSection(header), header);
            _IsInsideLinkageSectionContext = true;
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
        }

        public virtual void EndLinkageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LinkageSection
            _IsInsideLinkageSectionContext = false;
        }

        public virtual void StartProcedureDivision(ProcedureDivisionHeader header)
        {
            Enter(new ProcedureDivision(header), header);
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
        }

        public virtual void EndProcedureDivision()
        {
            Exit();
        }

        public virtual void StartDeclaratives(DeclarativesHeader header)
        {
            Enter(new Declaratives(header), header);
        }

        public virtual void EndDeclaratives(DeclarativesEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void EnterUseStatement(UseStatement useStatement)
        {
            Enter(new Use(useStatement), useStatement);
            Exit();
        }

        private Tools.UIDStore uidfactory = new Tools.UIDStore();
        private TypeDefinition _currentTypeDefinition;

        public virtual void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {
            header.SetLibrary(CurrentProgram.Identification.ProgramName.Name);
            var node = new FunctionDeclaration(header)
            {
                Label = uidfactory.FromOriginal(header?.FunctionName.Name),
                Library = CurrentProgram.Identification.ProgramName.Name
            };
            _ProcedureDeclaration = node;
            CurrentProgram.Root.SetFlag(Node.Flag.ContainsProcedure, true);
            //DO NOT change this without checking all references of Library. 
            // (SymbolTable - function, type finding could be impacted) 

            var enclosing = CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.Program);
            switch (header.Visibility)
            {
                case AccessModifier.Local:
                    CurrentNode.SymbolTable.AddFunction(node);
                    break;
                default:
                    enclosing.AddFunction(node);
                    break;
            }
            Enter(node, header, new SymbolTable(enclosing, SymbolTable.Scope.Function));

            var declaration = node.CodeElement;
            var funcProfile = node.Profile; //Get functionprofile to set parameters

            foreach (var parameter in declaration.Profile.InputParameters) //Set Input Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                paramNode.PassingType = ParameterDescription.PassingTypes.Input;
                funcProfile.InputParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
                CheckIfItsTyped(paramNode, paramNode.CodeElement);
            }
            foreach (var parameter in declaration.Profile.OutputParameters) //Set Output Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                paramNode.PassingType = ParameterDescription.PassingTypes.Output;
                funcProfile.OutputParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
                CheckIfItsTyped(paramNode, paramNode.CodeElement);
            }
            foreach (var parameter in declaration.Profile.InoutParameters) //Set Inout Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                paramNode.PassingType = ParameterDescription.PassingTypes.InOut;
                funcProfile.InoutParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
                CheckIfItsTyped(paramNode, paramNode.CodeElement);
            }

            if (declaration.Profile.ReturningParameter != null) //Set Returning Parameters
            {
                var paramNode = new ParameterDescription(declaration.Profile.ReturningParameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                paramNode.PassingType = ParameterDescription.PassingTypes.Returning;
                node.Profile.ReturningParameter = paramNode;

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
                CheckIfItsTyped(paramNode, paramNode.CodeElement);
            }
        }

        public virtual void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
            Enter(new FunctionEnd(end), end);
            Exit();
            Exit();// exit DECLARE FUNCTION
            _ProcedureDeclaration = null;
        }

        public virtual void StartFunctionProcedureDivision(ProcedureDivisionHeader header)
        {
            if (header.UsingParameters != null && header.UsingParameters.Count > 0)
                DiagnosticUtils.AddError(header, "TCRFUN_DECLARATION_NO_USING");//TODO#249

            Enter(new ProcedureDivision(header), header);
        }

        public virtual void EndFunctionProcedureDivision()
        {
            Exit();
        }

        public virtual void StartSection(SectionHeader header)
        {
            var section = new Section(header);
            Enter(section, header);
            section.SymbolTable.AddSection(section);
        }

        public virtual void EndSection()
        {
            Exit();
        }

        public virtual void StartParagraph(ParagraphHeader header)
        {
            var paragraph = new Paragraph(header);
            Enter(paragraph, header);
            paragraph.SymbolTable.AddParagraph(paragraph);
        }

        public virtual void EndParagraph()
        {
            Exit();
        }

        public virtual void StartSentence()
        {
            Enter(new Sentence(), null);
        }

        public virtual void EndSentence(SentenceEnd end, bool bCheck)
        {
            //if (bCheck)
            //{
            //    if (CurrentNode is ProcedureDivision || CurrentNode is Paragraph)
            //        return;
            //    else
            //        return;
            //}
            AttachEndIfExists(end);
            if (CurrentNode is Sentence) Exit();//TODO remove this and check what happens when exiting last CALL in FIN-STANDARD in BigBatch file (ie. CheckPerformance test)
        }

        /// <summary>
        /// Checks if the last statement being entered must leads to the start of a sentence.
        /// </summary>
        public virtual void CheckStartSentenceLastStatement()
        {
            if (LastEnteredNode != null)
            {
                Node parent = LastEnteredNode.Parent;
                if (parent is Paragraph || parent is ProcedureDivision)
                {   //JCM Hack: So in this case the statement must be given as parent a Sentence Node.
                    //This hack is perform because using the Enter/Exit mechanism to create a Syntax Tree
                    //Is not appropriate with LALR(1) grammar, because there is not start token that can telle
                    //The beginning of  list of statement that belongs to a sentence.
                    //So we check if the statement is part of a paragraph or a procedure division.
                    parent.Remove(LastEnteredNode);
                    //Start a sentence
                    StartSentence();
                    CurrentNode.Add(LastEnteredNode);                    
                }
            }
        }

        public virtual void StartExecStatement(ExecStatement execStmt)
        {
            ExitLastLevel1Definition();
            Enter(new Exec(execStmt), execStmt);
        }

        public virtual void EndExecStatement()
        {
            //Code duplicated in OnExecStatement
            //EndExecStatement (therefore StartExecStatement) is fired if the exec is in a procedure division and is the first instruction
            //OnExecStatement is fired if the exec is in a procedure division and is not the first instruction

            //Code to generate a specific ProcedureDeclaration as Nested when an Exec Statement is spotted. See Issue #1209
            //This might be helpful for later
            //if (_ProcedureDeclaration != null)
            //{
            //    _ProcedureDeclaration.SetFlag(Node.Flag.GenerateAsNested, true);
            //}

            //Code to generate all ProcedureDeclarations as Nested when an Exec Statement is spotted. See Issue #1209
            //This is the selected solution until we determine the more optimal way to generate a program that contains Exec Statements
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.Root.MainProgram.SetFlag(Node.Flag.GenerateAsNested, true);
            }
            Exit();
        }

        public virtual void OnContinueStatement(ContinueStatement stmt)
        {
            Enter(new Continue(stmt), stmt);
            Exit();
        }

        public virtual void OnEntryStatement(EntryStatement stmt)
        {
            Enter(new Entry(stmt), stmt);
            Exit();
        }

        public virtual void OnAcceptStatement(AcceptStatement stmt)
        {
            Enter(new Accept(stmt), stmt);
            Exit();
        }

        public virtual void OnInitializeStatement(InitializeStatement stmt)
        {
            Enter(new Initialize(stmt), stmt);
            Exit();
        }

        public virtual void OnInspectStatement(InspectStatement stmt)
        {
            Enter(new Inspect(stmt), stmt);
            Exit();
        }

        public virtual void OnMoveStatement(MoveStatement stmt)
        {
            Enter(new Move(stmt), stmt);
            Exit();
        }

        public virtual void OnSetStatement(SetStatement stmt)
        {
            Enter(new Set(stmt), stmt);
            Exit();
        }

        public virtual void OnStopStatement(StopStatement stmt)
        {
            Enter(new Stop(stmt), stmt);
            Exit();
        }

        public virtual void OnExitMethodStatement(ExitMethodStatement stmt)
        {
            Enter(new ExitMethod(stmt), stmt);
            Exit();
        }

        public virtual void OnExitProgramStatement(ExitProgramStatement stmt)
        {
            Enter(new ExitProgram(stmt), stmt);
            Exit();
        }

        public virtual void OnAllocateStatement(AllocateStatement stmt)
        {
            Enter(new Allocate(stmt), stmt);
            Exit();
        }

        public virtual void OnFreeStatement(FreeStatement stmt)
        {
            Enter(new Free(stmt), stmt);
            Exit();
        }

        public virtual void OnGobackStatement(GobackStatement stmt)
        {
            Enter(new Goback(stmt), stmt);
            Exit();
        }

        public virtual void OnCloseStatement(CloseStatement stmt)
        {
            Enter(new Close(stmt), stmt);
            Exit();
        }

        public virtual void OnDisplayStatement(DisplayStatement stmt)
        {
            Enter(new Display(stmt), stmt);
            Exit();
        }

        public virtual void OnOpenStatement(OpenStatement stmt)
        {
            Enter(new Open(stmt), stmt);
            Exit();
        }

        public virtual void OnMergeStatement(MergeStatement stmt)
        {
            Enter(new Merge(stmt), stmt);
            Exit();
        }

        public virtual void OnReleaseStatement(ReleaseStatement stmt)
        {
            Enter(new Release(stmt), stmt);
            Exit();
        }

        public virtual void OnSortStatement(SortStatement stmt)
        {
            Enter(new Sort(stmt), stmt);
            Exit();
        }

        public virtual void OnAlterStatement(AlterStatement stmt)
        {
            Enter(new Alter(stmt), stmt);
            Exit();
        }

        public void OnExitStatement(ExitStatement stmt)
        {
            Enter(new Exit(stmt), stmt);
            Exit();
        }

        public virtual void OnGotoStatement(GotoStatement stmt)
        {
            Enter(new Goto(stmt), stmt);
            Exit();
        }

        public virtual void OnPerformProcedureStatement(PerformProcedureStatement stmt)
        {
            Enter(new PerformProcedure(stmt), stmt);
            Exit();
        }

        public virtual void OnCancelStatement(CancelStatement stmt)
        {
            Enter(new Cancel(stmt), stmt);
            Exit();
        }

        public virtual void OnProcedureStyleCall(ProcedureStyleCallStatement stmt, CallStatementEnd end)
        {
            Enter(new ProcedureStyleCall(stmt), stmt);            
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void OnExecStatement(ExecStatement stmt)
        {
            Enter(new Exec(stmt), stmt);

            //Code duplicated in OnExecStatement
            //EndExecStatement (therefore StartExecStatement) is fired if the exec is in a procedure division and is the first instruction
            //OnExecStatement is fired if the exec is in a procedure division and is not the first instruction

            //Code to generate a specific ProcedureDeclaration as Nested when an Exec Statement is spotted. See Issue #1209
            //This might be helpful for later
            //if (_ProcedureDeclaration != null)
            //{
            //    _ProcedureDeclaration.SetFlag(Node.Flag.GenerateAsNested, true);
            //}

            //Code to generate all ProcedureDeclarations as Nested when an Exec Statement is spotted. See Issue #1209
            //This is the selected solution until we determine the more optimal way to generate a program that contains Exec Statements
            if (_ProcedureDeclaration != null)
            {
                CurrentNode.Root.MainProgram.SetFlag(Node.Flag.GenerateAsNested, true);
            }
            Exit();
        }

        public virtual void StartAddStatementConditional(TypeCobol.Compiler.CodeElements.AddStatement stmt)
        {
            Enter(new Add(stmt), stmt);
        }

        public virtual void EndAddStatementConditional(TypeCobol.Compiler.CodeElements.AddStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartCallStatementConditional(TypeCobol.Compiler.CodeElements.CallStatement stmt)
        {
            Enter(new Call(stmt), stmt);
        }

        public virtual void EndCallStatementConditional(TypeCobol.Compiler.CodeElements.CallStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartComputeStatementConditional(TypeCobol.Compiler.CodeElements.ComputeStatement stmt)
        {
            Enter(new Compute(stmt), stmt);
        }

        public virtual void EndComputeStatementConditional(TypeCobol.Compiler.CodeElements.ComputeStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartOnSizeError(TypeCobol.Compiler.CodeElements.OnSizeErrorCondition cond)
        {
            Enter(new OnSizeError(cond), cond);
        }

        public virtual void EndOnSizeError()
        {
            Exit();
        }

        public virtual void StartNoSizeError(TypeCobol.Compiler.CodeElements.NotOnSizeErrorCondition cond)
        {
            Enter(new NoSizeError(cond), cond);
        }

        public virtual void EndNoSizeError()
        {
            Exit();
        }

        public virtual void StartOnException(TypeCobol.Compiler.CodeElements.OnExceptionCondition cond)
        {
            Enter(new OnException(cond), cond);
        }

        public virtual void EndOnException()
        {
            Exit();
        }

        public virtual void StartNoException(TypeCobol.Compiler.CodeElements.NotOnExceptionCondition cond)
        {
            Enter(new NoException(cond), cond);
        }

        public virtual void EndNoException()
        {
            Exit();
        }

        public virtual void StartOnOverflow(TypeCobol.Compiler.CodeElements.OnOverflowCondition cond)
        {
            Enter(new OnOverflow(cond), cond);
        }

        public virtual void EndOnOverflow()
        {
            Exit();
        }

        public virtual void StartNoOverflow(TypeCobol.Compiler.CodeElements.NotOnOverflowCondition cond)
        {
            Enter(new NoOverflow(cond), cond);
        }

        public virtual void EndNoOverflow()
        {
            Exit();
        }

        public virtual void StartOnInvalidKey(TypeCobol.Compiler.CodeElements.InvalidKeyCondition cond)
        {
            Enter(new OnInvalidKey(cond), cond);
        }

        public virtual void EndOnInvalidKey()
        {
            Exit();
        }

        public virtual void StartNoInvalidKey(TypeCobol.Compiler.CodeElements.NotInvalidKeyCondition cond)
        {
            Enter(new NoInvalidKey(cond), cond);
        }

        public virtual void EndNoInvalidKey()
        {
            Exit();
        }

        public virtual void StartOnAtEnd(TypeCobol.Compiler.CodeElements.AtEndCondition cond)
        {
            Enter(new OnAtEnd(cond), cond);
        }

        public virtual void EndOnAtEnd()
        {
            Exit();
        }

        public virtual void StartNoAtEnd(TypeCobol.Compiler.CodeElements.NotAtEndCondition cond)
        {
            Enter(new NoAtEnd(cond), cond);
        }

        public virtual void EndNoAtEnd()
        {
            Exit();
        }

        public virtual void StartDeleteStatementConditional(TypeCobol.Compiler.CodeElements.DeleteStatement stmt)
        {
            Enter(new Delete(stmt), stmt);
        }

        public virtual void EndDeleteStatementConditional(TypeCobol.Compiler.CodeElements.DeleteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartDivideStatementConditional(TypeCobol.Compiler.CodeElements.DivideStatement stmt)
        {
            Enter(new Divide(stmt), stmt);
        }

        public virtual void EndDivideStatementConditional(TypeCobol.Compiler.CodeElements.DivideStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartEvaluateStatementWithBody(TypeCobol.Compiler.CodeElements.EvaluateStatement stmt)
        {
            Enter(new Evaluate(stmt), stmt);// enter EVALUATE
        }

        public virtual void EndEvaluateStatementWithBody(TypeCobol.Compiler.CodeElements.EvaluateStatementEnd end)
        {
            AttachEndIfExists(end);// exit EVALUATE
            Exit();
        }

        public virtual void StartWhenConditionClause(List<TypeCobol.Compiler.CodeElements.CodeElement> conditions)
        {
            Enter(new WhenGroup(), null);// enter WHEN group
            foreach(var cond in conditions)
            {
                TypeCobol.Compiler.CodeElements.WhenCondition condition = null;
                if (cond is TypeCobol.Compiler.CodeElements.WhenSearchCondition)
                {
                    TypeCobol.Compiler.CodeElements.WhenSearchCondition whensearch =
                        cond as TypeCobol.Compiler.CodeElements.WhenSearchCondition;
                    condition = new WhenCondition();
                    whensearch.ApplyPropertiesToCE(condition);

                    condition.SelectionObjects = new EvaluateSelectionObject[1];
                    condition.SelectionObjects[0] = new EvaluateSelectionObject();
                    condition.SelectionObjects[0].BooleanComparisonVariable = new BooleanValueOrExpression(whensearch.Condition);
                }
                else
                {
                    condition = cond as TypeCobol.Compiler.CodeElements.WhenCondition;
                }
                Enter(new When(condition), condition);
                Exit();
            }
            Exit();// exit WHEN group
            Enter(new Then(), conditions[0]);// enter THEN
        }


        public virtual void EndWhenConditionClause()
        {
            Exit();// exit THEN
        }

        public virtual void StartWhenOtherClause(TypeCobol.Compiler.CodeElements.WhenOtherCondition cond)
        {
            Enter(new WhenOther(cond), cond);// enter WHEN OTHER
        }

        public virtual void EndWhenOtherClause()
        {
            Exit();// exit WHEN OTHER
        }

        public virtual void StartIfStatementWithBody(TypeCobol.Compiler.CodeElements.IfStatement stmt)
        {
            Enter(new If(stmt), stmt);
            Enter(new Then(), stmt);
        }
        public virtual void EnterElseClause(TypeCobol.Compiler.CodeElements.ElseCondition clause)
        {
            Exit();// we want ELSE to be child of IF, not THEN, so exit THEN
            Enter(new Else(clause), clause);// ELSE
        }
        public virtual void EndIfStatementWithBody(TypeCobol.Compiler.CodeElements.IfStatementEnd end)
        {
            Exit(); // Exit ELSE (if any) or THEN
            AttachEndIfExists(end);
            // DO NOT Exit() IF node because this will be done in ExitStatement
            Exit();//JCM exit any way ???
        }

        public virtual void AddNextSentenceStatement(TypeCobol.Compiler.CodeElements.NextSentenceStatement stmt)
        {
            Enter(new NextSentence(stmt));
            Exit();
        }

        public virtual void StartInvokeStatementConditional(TypeCobol.Compiler.CodeElements.InvokeStatement stmt)
        {
            Enter(new Invoke(stmt), stmt);
        }

        public virtual void EndInvokeStatementConditional(TypeCobol.Compiler.CodeElements.InvokeStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartJsonGenerateStatementConditional(TypeCobol.Compiler.CodeElements.JsonGenerateStatement stmt)
        {
            Enter(new JsonGenerate(stmt), stmt);
        }

        public virtual void EndJsonGenerateStatementConditional(TypeCobol.Compiler.CodeElements.JsonStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartMultiplyStatementConditional(TypeCobol.Compiler.CodeElements.MultiplyStatement stmt)
        {
            Enter(new Multiply(stmt), stmt);
        }

        public virtual void EndMultiplyStatementConditional(TypeCobol.Compiler.CodeElements.MultiplyStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartPerformStatementWithBody(TypeCobol.Compiler.CodeElements.PerformStatement stmt)
        {
            Enter(new Perform(stmt), stmt);
        }

        public virtual void EndPerformStatementWithBody(TypeCobol.Compiler.CodeElements.PerformStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartSearchStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.SearchStatement stmt)
        {
            Enter(new Search(stmt), stmt);
        }

        public virtual void EndSearchStatementWithBody(TypeCobol.Compiler.CodeElements.SearchStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartWhenSearchConditionClause(TypeCobol.Compiler.CodeElements.WhenSearchCondition condition)
        {
            Enter(new WhenSearch(condition), condition);
        }

        public virtual void EndWhenSearchConditionClause()
        {
            Exit(); // WHEN
        }

        public virtual void EnterReadStatementConditional(TypeCobol.Compiler.CodeElements.ReadStatement stmt)
        {
            Enter(new Read(stmt), stmt);
        }

        public virtual void EndReadStatementConditional(TypeCobol.Compiler.CodeElements.ReadStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void EnterReturnStatementConditional(TypeCobol.Compiler.CodeElements.ReturnStatement stmt)
        {
            Enter(new Return(stmt), stmt);
        }

        public virtual void EndReturnStatementConditional(TypeCobol.Compiler.CodeElements.ReturnStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartRewriteStatementConditional(TypeCobol.Compiler.CodeElements.RewriteStatement stmt)
        {
            Enter(new Rewrite(stmt), stmt);
        }

        public virtual void EndRewriteStatementConditional(TypeCobol.Compiler.CodeElements.RewriteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartStartStatementConditional(TypeCobol.Compiler.CodeElements.StartStatement stmt)
        {
            Enter(new Start(stmt), stmt);
        }

        public virtual void EndStartStatementConditional(TypeCobol.Compiler.CodeElements.StartStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartStringStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.StringStatement stmt)
        {
            Enter(new Nodes.String(stmt), stmt);
        }

        public virtual void EndStringStatementConditional(TypeCobol.Compiler.CodeElements.StringStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartSubtractStatementConditional(TypeCobol.Compiler.CodeElements.SubtractStatement stmt)
        {
            Enter(new Subtract(stmt), stmt);
        }

        public virtual void EndSubtractStatementConditional(TypeCobol.Compiler.CodeElements.SubtractStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartUnstringStatementConditional(TypeCobol.Compiler.CodeElements.UnstringStatement stmt)
        {
            Enter(new Unstring(stmt), stmt);
        }

        public virtual void EndUnstringStatementConditional(TypeCobol.Compiler.CodeElements.UnstringStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartWriteStatementConditional(TypeCobol.Compiler.CodeElements.WriteStatement stmt)
        {
            Enter(new Write(stmt), stmt);
        }

        public virtual void EndWriteStatementConditional(TypeCobol.Compiler.CodeElements.WriteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartXmlGenerateStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlGenerateStatement stmt)
        {
            Enter(new XmlGenerate(stmt), stmt);
        }

        public virtual void EndXmlGenerateStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

        public virtual void StartXmlParseStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlParseStatement stmt)
        {
            Enter(new XmlParse(stmt), stmt);
        }

        public virtual void EndXmlParseStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
        }

    }
}
