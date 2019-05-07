using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using Analytics;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// The Program Class Builder
    /// </summary>
    public class ProgramClassBuilder : IProgramClassBuilder
    {
        /// <summary>
        /// Program object resulting of the visit the parse tree
        /// </summary>
        private Program Program { get; set; }
        public SyntaxTree SyntaxTree { get; set; }

        private TypeDefinition _CurrentTypeDefinition;
        private bool _IsInsideWorkingStorageContext;
        private bool _IsInsideLinkageSectionContext;
        private bool _IsInsideLocalStorageSectionContext;
        private bool _IsInsideFileSectionContext;
        private bool _IsInsideProcedure;
        private bool _IsInsideGlobalStorageSection;

        // Programs can be nested => track current programs being analyzed
        private Stack<Program> programsStack = null;

        private Program CurrentProgram
        {
            get { return programsStack.Peek(); }
            set { programsStack.Push(value); }
        }

        /// <summary>Class object resulting of the visit the parse tree</summary>
        public CodeModel.Class Class { get; private set; }

        private readonly SymbolTable TableOfIntrisic = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
        private SymbolTable TableOfGlobals;
        private SymbolTable TableOfNamespaces;
        private SymbolTable TableOfGlobalStorage;

        public SymbolTable CustomSymbols
        {
            private get { throw new InvalidOperationException(); }
            set
            {
                if (value != null)
                {
                    SymbolTable intrinsicTable = value.GetTableFromScope(SymbolTable.Scope.Intrinsic);
                    SymbolTable nameSpaceTable = value.GetTableFromScope(SymbolTable.Scope.Namespace);

                    intrinsicTable.DataEntries.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));
                    intrinsicTable.Types.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));
                    intrinsicTable.Functions.Values.ToList().ForEach(d => d.ForEach(da => da.SetFlag(Node.Flag.NodeIsIntrinsic, true)));

                    TableOfIntrisic.CopyAllDataEntries(intrinsicTable.DataEntries.Values);
                    TableOfIntrisic.CopyAllTypes(intrinsicTable.Types);
                    TableOfIntrisic.CopyAllFunctions(intrinsicTable.Functions, AccessModifier.Public);

                    if (nameSpaceTable != null)
                    {
                        TableOfNamespaces = new SymbolTable(TableOfIntrisic, SymbolTable.Scope.Namespace); //Set TableOfNamespace with program if dependencies were given. (See CLI.cs runOnce2() LoadDependencies())
                        TableOfNamespaces.CopyAllPrograms(nameSpaceTable.Programs.Values);
                    }

                }
                // TODO#249: use a COPY for these
                foreach (var type in DataType.BuiltInCustomTypes)
                {
                    var createdType = DataType.CreateBuiltIn(type);
                    TableOfIntrisic.AddType(createdType); //Add default TypeCobol types BOOLEAN and DATE
                    //Add type and children to DataTypeEntries dictionary in Intrinsic symbol table
                    TableOfIntrisic.AddDataDefinitionsUnderType(createdType);
                }
            }
        }



        public ProgramClassBuilderNodeDispatcher Dispatcher { get; internal set; }

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
            if (_IsInsideProcedure)
            {
                node.SetFlag(Node.Flag.InsideProcedure, true);      //Set flag to know that this node belongs a Procedure or Function
            }
            SyntaxTree.Enter(node);

            if (node.CodeElement != null)
                NodeCodeElementLinkers.Add(node.CodeElement, node);
            Dispatcher.Enter(node);
        }

        private void Exit()
        {
            var node = SyntaxTree.CurrentNode;
            Dispatcher.OnNode(node, CurrentProgram);
            SyntaxTree.Exit();
            LastEnteredNode = node;
            Dispatcher.Exit(node);
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
            Node LastLevel1Def = null;
            while (CurrentNode.CodeElement != null && CurrentNode.CodeElement is DataDefinitionEntry)
            {
                LastLevel1Def = CurrentNode;
                Exit();
            }
            if (LastLevel1Def != null)
                Dispatcher.OnLevel1Definition((DataDefinition)LastLevel1Def);
        }

        private void AddToSymbolTable(DataDescription node)
        {
            var table = node.SymbolTable;
            if (node.CodeElement.IsGlobal && table.CurrentScope != SymbolTable.Scope.GlobalStorage)
                table = table.GetTableFromScope(SymbolTable.Scope.Global);
            else
            {
                var parent = node.Parent as DataDescription;
                while (parent != null)
                {
                    if (parent.CodeElement.IsGlobal && table.CurrentScope != SymbolTable.Scope.GlobalStorage)
                        table = table.GetTableFromScope(SymbolTable.Scope.Global);
                    parent = parent.Parent as DataDescription;
                }
            }

            table.AddVariable(node);
        }

        public virtual void StartCobolCompilationUnit()
        {
            if (TableOfNamespaces == null)
                TableOfNamespaces = new SymbolTable(TableOfIntrisic, SymbolTable.Scope.Namespace);

            TableOfGlobalStorage = new SymbolTable(TableOfNamespaces, SymbolTable.Scope.GlobalStorage);
            TableOfGlobals = new SymbolTable(TableOfGlobalStorage, SymbolTable.Scope.Global);
            Program = null;

            SyntaxTree.Root.SymbolTable = TableOfNamespaces; //Set SymbolTable of SourceFile Node, Limited to NameSpace and Intrinsic scopes
            Dispatcher.StartCobolCompilationUnit();
        }

        public virtual void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {

            if (Program == null)
            {
                if (SyntaxTree.Root.MainProgram == null)
                {
                    SyntaxTree.Root.MainProgram = new SourceProgram(TableOfGlobals, programIdentification);
                    Program = SyntaxTree.Root.MainProgram;
                }
                else
                {
                    Program = new StackedProgram(TableOfGlobals, programIdentification);
                }

                programsStack = new Stack<Program>();
                CurrentProgram = Program;
                Enter(CurrentProgram, programIdentification, CurrentProgram.SymbolTable);
            }
            else
            {
                var enclosing = CurrentProgram;
                CurrentProgram = new NestedProgram(enclosing, programIdentification);
                Enter(CurrentProgram, programIdentification, CurrentProgram.SymbolTable);
            }

            if (libraryCopy != null)
            { // TCRFUN_LIBRARY_COPY
                var cnode = new LibraryCopy(libraryCopy);
                Enter(cnode, libraryCopy, CurrentProgram.SymbolTable);
                Exit();
            }

            TableOfNamespaces.AddProgram(CurrentProgram); //Add Program to Namespace table. 
            Dispatcher.StartCobolProgram(programIdentification, libraryCopy);
        }

        public virtual void EndCobolProgram(TypeCobol.Compiler.CodeElements.ProgramEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            programsStack.Pop();

            if (programsStack.Count == 0) //Means that we ended a main program, reset Table and program in case of a new program declaration before EOF. 
            {
                TableOfGlobals = new SymbolTable(TableOfNamespaces, SymbolTable.Scope.Global);
                Program = null;
            }
            Dispatcher.EndCobolProgram(end);
        }

        public virtual void StartEnvironmentDivision(EnvironmentDivisionHeader header)
        {
            Enter(new EnvironmentDivision(header), header);
            Dispatcher.StartEnvironmentDivision(header);
        }

        public virtual void EndEnvironmentDivision()
        {
            Exit();
            Dispatcher.EndEnvironmentDivision();
        }

        public virtual void StartConfigurationSection(ConfigurationSectionHeader header)
        {
            Enter(new ConfigurationSection(header), header);
            Dispatcher.StartConfigurationSection(header);
        }

        public virtual void EndConfigurationSection()
        {
            Exit();
            Dispatcher.EndConfigurationSection();
        }

        public virtual void StartSourceComputerParagraph(SourceComputerParagraph paragraph)
        {
            Enter(new SourceComputer(paragraph));
            Dispatcher.StartSourceComputerParagraph(paragraph);
        }

        public virtual void EndSourceComputerParagraph()
        {
            Exit();
            Dispatcher.EndSourceComputerParagraph();
        }

        public virtual void StartObjectComputerParagraph(ObjectComputerParagraph paragraph)
        {
            Enter(new ObjectComputer(paragraph));
            Dispatcher.StartObjectComputerParagraph(paragraph);
        }

        public virtual void EndObjectComputerParagraph()
        {
            Exit();
            Dispatcher.EndObjectComputerParagraph();
        }

        public virtual void StartSpecialNamesParagraph(SpecialNamesParagraph paragraph)
        {
            Enter(new SpecialNames(paragraph));
            Dispatcher.StartSpecialNamesParagraph(paragraph);
        }

        public virtual void EndSpecialNamesParagraph()
        {
            Exit();
            Dispatcher.EndSpecialNamesParagraph();
        }

        public virtual void StartRepositoryParagraph(RepositoryParagraph paragraph)
        {
            Enter(new Repository(paragraph));
            Dispatcher.StartRepositoryParagraph(paragraph);
        }

        public virtual void EndRepositoryParagraph()
        {
            Exit();
            Dispatcher.EndRepositoryParagraph();
        }

        public virtual void StartInputOutputSection(InputOutputSectionHeader header)
        {
            Enter(new InputOutputSection(header), header);
            Dispatcher.StartInputOutputSection(header);
        }

        public virtual void EndInputOutputSection()
        {
            Exit();
            Dispatcher.EndInputOutputSection();
        }

        public virtual void StartFileControlParagraph(FileControlParagraphHeader header)
        {
            Enter(new FileControlParagraphHeaderNode(header), header);
            Dispatcher.StartFileControlParagraph(header);
        }

        public virtual void EndFileControlParagraph()
        {
            Exit();
            Dispatcher.EndFileControlParagraph();
        }

        public virtual void StartFileControlEntry(FileControlEntry entry)
        {
            var fileControlEntry = new FileControlEntryNode(entry);
            Enter(fileControlEntry, entry);
            Dispatcher.StartFileControlEntry(entry);
        }

        public virtual void EndFileControlEntry()
        {
            Exit();
            Dispatcher.EndFileControlEntry();
        }

        public virtual void StartDataDivision(DataDivisionHeader header)
        {
            Enter(new DataDivision(header), header);
            Dispatcher.StartDataDivision(header);
        }

        public virtual void EndDataDivision()
        {
            Exit();
            Dispatcher.EndDataDivision();
        }

        public virtual void StartFileSection(FileSectionHeader header)
        {
            Enter(new FileSection(header), header);
            _IsInsideFileSectionContext = true;
            Dispatcher.StartFileSection(header);
        }

        public virtual void EndFileSection()
        {
            ExitLastLevel1Definition();
            Exit();
            _IsInsideFileSectionContext = false;
            Dispatcher.EndFileSection();
        }

        public virtual void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {
            Enter(new GlobalStorageSection(header), header, SyntaxTree.CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.GlobalStorage));
            _IsInsideGlobalStorageSection = true;
            Dispatcher.StartGlobalStorageSection(header);
        }

        public virtual void EndGlobalStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit GlobalStorageSection
            _IsInsideGlobalStorageSection = false;
            Dispatcher.EndGlobalStorageSection();
        }

        public virtual void StartFileDescriptionEntry(FileDescriptionEntry entry)
        {
            ExitLastLevel1Definition();
            Enter(new FileDescriptionEntryNode(entry), entry);
            Dispatcher.StartFileDescriptionEntry(entry);
        }

        public virtual void EndFileDescriptionEntry()
        {
            Exit();
            Dispatcher.EndFileDescriptionEntry();
        }

        public virtual void EndFileDescriptionEntryIfAny()
        {
            if (this.CurrentNode is FileDescriptionEntryNode)
            {
                EndFileDescriptionEntry();
            }
            Dispatcher.EndFileDescriptionEntryIfAny();
        }

        public virtual void StartDataDescriptionEntry(DataDescriptionEntry entry)
        {
            var dataTypeDescriptionEntry = entry as DataTypeDescriptionEntry;
            if (dataTypeDescriptionEntry != null) StartTypeDefinitionEntry(dataTypeDescriptionEntry);
            else
            {
                SetCurrentNodeToTopLevelItem(entry.LevelNumber);

                //Update DataType of CodeElement by searching info on the declared Type into SymbolTable.
                //Note that the AST is not complete here, but you can only refer to a Type that has previously been defined.
                var node = new DataDescription(entry);
                if (_CurrentTypeDefinition != null)
                    node.ParentTypeDefinition = _CurrentTypeDefinition;
                Enter(node);

                if (entry.Indexes != null && entry.Indexes.Any())
                {
                    var table = node.SymbolTable;
                    foreach (var index in entry.Indexes)
                    {
                        if (node.CodeElement.IsGlobal)
                            table = table.GetTableFromScope(SymbolTable.Scope.Global);

                        var indexNode = new IndexDefinition(index);
                        Enter(indexNode, null, table);
                        if (_CurrentTypeDefinition != null)
                            indexNode.ParentTypeDefinition = _CurrentTypeDefinition;
                        table.AddVariable(indexNode);
                        Exit();
                    }
                }

                var types = node.SymbolTable.GetType(node);
                if (types.Count == 1)
                {
                    entry.DataType.RestrictionLevel = types[0].DataType.RestrictionLevel;
                }
                //else do nothing, it's an error that will be handled by Cobol2002Checker

                if (_IsInsideWorkingStorageContext)
                    node.SetFlag(Node.Flag.WorkingSectionNode, true);      //Set flag to know that this node belongs to Working Storage Section
                if (_IsInsideLinkageSectionContext)
                    node.SetFlag(Node.Flag.LinkageSectionNode, true);      //Set flag to know that this node belongs to Linkage Section
                if (_IsInsideLocalStorageSectionContext)
                    node.SetFlag(Node.Flag.LocalStorageSectionNode, true); //Set flag to know that this node belongs to Local Storage Section
                if (_IsInsideFileSectionContext)
                    node.SetFlag(Node.Flag.FileSectionNode, true);         //Set flag to know that this node belongs to File Section
                if (_IsInsideGlobalStorageSection)
                    node.SetFlag(Node.Flag.GlobalStorageSection, true);         //Set flag to know that this node belongs to Global Storage Section

                AddToSymbolTable(node);
                Dispatcher.StartDataDescriptionEntry(entry);
            }
        }

        public virtual void StartDataRedefinesEntry(DataRedefinesEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var node = new DataRedefines(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node);
            node.SymbolTable.AddVariable(node);
            Dispatcher.StartDataRedefinesEntry(entry);
        }

        public virtual void StartDataRenamesEntry(DataRenamesEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var node = new DataRenames(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node);
            node.SymbolTable.AddVariable(node);
            Dispatcher.StartDataRenamesEntry(entry);
        }

        public virtual void StartDataConditionEntry(DataConditionEntry entry)
        {
            SetCurrentNodeToTopLevelItem(entry.LevelNumber);
            var node = new DataCondition(entry);
            if (_CurrentTypeDefinition != null)
                node.ParentTypeDefinition = _CurrentTypeDefinition;
            Enter(node);
            node.SymbolTable.AddVariable(node);
            Dispatcher.StartDataConditionEntry(entry);
        }

        public virtual void StartTypeDefinitionEntry(DataTypeDescriptionEntry typedef)
        {
            SetCurrentNodeToTopLevelItem(typedef.LevelNumber);
            var node = new TypeDefinition(typedef);
            Enter(node);
            // TCTYPE_GLOBAL_TYPEDEF
            var table = node.SymbolTable.GetTableFromScope(node.CodeElement.IsGlobal ? SymbolTable.Scope.Global : SymbolTable.Scope.Declarations);
            table.AddType(node);

            _CurrentTypeDefinition = node;

            AnalyticsWrapper.Telemetry.TrackEvent(EventType.TypeDeclared, node.Name, LogType.TypeCobolUsage);
            Dispatcher.StartTypeDefinitionEntry(typedef);
        }

        public virtual void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {
            Enter(new WorkingStorageSection(header), header);
            _IsInsideWorkingStorageContext = true;
            if (_IsInsideProcedure)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
            Dispatcher.StartWorkingStorageSection(header);
        }

        public virtual void EndWorkingStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit WorkingStorageSection
            _IsInsideWorkingStorageContext = false;
            Dispatcher.EndWorkingStorageSection();
        }

        public virtual void StartLocalStorageSection(LocalStorageSectionHeader header)
        {
            Enter(new LocalStorageSection(header), header);
            _IsInsideLocalStorageSectionContext = true;
            if (_IsInsideProcedure)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
            Dispatcher.StartLocalStorageSection(header);
        }

        public virtual void EndLocalStorageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LocalStorageSection
            _IsInsideLocalStorageSectionContext = false;
            Dispatcher.EndLocalStorageSection();
        }

        public virtual void StartLinkageSection(LinkageSectionHeader header)
        {
            Enter(new LinkageSection(header), header);
            _IsInsideLinkageSectionContext = true;
            if (_IsInsideProcedure)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
            Dispatcher.StartLinkageSection(header);
        }

        public virtual void EndLinkageSection()
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LinkageSection
            _IsInsideLinkageSectionContext = false;
            Dispatcher.EndLinkageSection();
        }

        public virtual void StartProcedureDivision(ProcedureDivisionHeader header)
        {
            Enter(new ProcedureDivision(header), header);
            if (_IsInsideProcedure)
            {
                CurrentNode.SetFlag(Node.Flag.ForceGetGeneratedLines, true);
            }
            Dispatcher.StartProcedureDivision(header);
        }

        public virtual void EndProcedureDivision()
        {
            Exit();
            Dispatcher.EndProcedureDivision();
        }

        public virtual void StartDeclaratives(DeclarativesHeader header)
        {
            Enter(new Declaratives(header), header);
            Dispatcher.StartDeclaratives(header);
        }

        public virtual void EndDeclaratives(DeclarativesEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndDeclaratives(end);
        }

        public virtual void EnterUseStatement(UseStatement useStatement)
        {
            Enter(new Use(useStatement), useStatement);
            Exit();
            Dispatcher.EnterUseStatement(useStatement);
        }

        private Tools.UIDStore uidfactory = new Tools.UIDStore();
        public virtual void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {
            header.SetLibrary(CurrentProgram.Identification.ProgramName.Name);
            var node = new FunctionDeclaration(header)
            {
                Label = uidfactory.FromOriginal(header?.FunctionName.Name),
                Library = CurrentProgram.Identification.ProgramName.Name
            };
            _IsInsideProcedure = true;
            CurrentProgram.Root.SetFlag(Node.Flag.ContainsProcedure, true);
            //DO NOT change this without checking all references of Library. 
            // (SymbolTable - function, type finding could be impacted) 

            //Function must be added to Declarations scope
            var declarationSymbolTable = SyntaxTree.CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);
            declarationSymbolTable.AddFunction(node);
            Enter(node, header, new SymbolTable(declarationSymbolTable, SymbolTable.Scope.Function));

            var declaration = (FunctionDeclarationHeader)CurrentNode.CodeElement;
            var funcProfile = ((FunctionDeclaration)CurrentNode).Profile; //Get functionprofile to set parameters

            foreach (var parameter in declaration.Profile.InputParameters) //Set Input Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                paramNode.PassingType = ParameterDescription.PassingTypes.Input;
                funcProfile.InputParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
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
            }

            if (declaration.Profile.ReturningParameter != null) //Set Returning Parameters
            {
                var paramNode = new ParameterDescription(declaration.Profile.ReturningParameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                ((FunctionDeclaration)CurrentNode).Profile.ReturningParameter = paramNode;

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
            }

            AnalyticsWrapper.Telemetry.TrackEvent(EventType.FunctionDeclared, declaration.FunctionName.ToString(), LogType.TypeCobolUsage);
            Dispatcher.StartFunctionDeclaration(header);
        }

        public virtual void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
            Enter(new FunctionEnd(end), end);
            Exit();
            Exit();// exit DECLARE FUNCTION
            _IsInsideProcedure = false;
            Dispatcher.EndFunctionDeclaration(end);
        }

        public virtual void StartFunctionProcedureDivision(ProcedureDivisionHeader header)
        {
            if (header.UsingParameters != null && header.UsingParameters.Count > 0)
                DiagnosticUtils.AddError(header, "TCRFUN_DECLARATION_NO_USING");//TODO#249

            Enter(new ProcedureDivision(header), header);
            Dispatcher.StartFunctionProcedureDivision(header);
        }

        public virtual void EndFunctionProcedureDivision()
        {
            Exit();
            Dispatcher.EndFunctionProcedureDivision();
        }

        public virtual void StartSection(SectionHeader header)
        {
            var section = new Section(header);
            Enter(section, header);
            section.SymbolTable.AddSection(section);
            Dispatcher.StartSection(header);
        }

        public virtual void EndSection()
        {
            Exit();
            Dispatcher.EndSection();
        }

        public virtual void StartParagraph(ParagraphHeader header)
        {
            var paragraph = new Paragraph(header);
            Enter(paragraph, header);
            paragraph.SymbolTable.AddParagraph(paragraph);
            Dispatcher.StartParagraph(header);
        }

        public virtual void EndParagraph()
        {
            Exit();
            Dispatcher.EndParagraph();
        }

        public virtual void StartSentence()
        {
            Enter(new Sentence(), null);
            Dispatcher.StartSentence();
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
            Dispatcher.EndSentence(end, bCheck);
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
            Dispatcher.StartExecStatement(execStmt);
        }

        public virtual void EndExecStatement()
        {
            Exit();
            Dispatcher.EndExecStatement();
        }

        public virtual void OnContinueStatement(ContinueStatement stmt)
        {
            Enter(new Continue(stmt), stmt);
            Exit();
            Dispatcher.OnContinueStatement(stmt);
        }

        public virtual void OnEntryStatement(EntryStatement stmt)
        {
            Enter(new Entry(stmt), stmt);
            Exit();
            Dispatcher.OnEntryStatement(stmt);
        }

        public virtual void OnAcceptStatement(AcceptStatement stmt)
        {
            Enter(new Accept(stmt), stmt);
            Exit();
            Dispatcher.OnAcceptStatement(stmt);
        }

        public virtual void OnInitializeStatement(InitializeStatement stmt)
        {
            Enter(new Initialize(stmt), stmt);
            Exit();
            Dispatcher.OnInitializeStatement(stmt);
        }

        public virtual void OnInspectStatement(InspectStatement stmt)
        {
            Enter(new Inspect(stmt), stmt);
            Exit();
            Dispatcher.OnInspectStatement(stmt);
        }

        public virtual void OnMoveStatement(MoveStatement stmt)
        {
            Enter(new Move(stmt), stmt);
            Exit();
            Dispatcher.OnMoveStatement(stmt);
        }

        public virtual void OnSetStatement(SetStatement stmt)
        {
            Enter(new Set(stmt), stmt);
            Exit();
            Dispatcher.OnSetStatement(stmt);
        }

        public virtual void OnStopStatement(StopStatement stmt)
        {
            Enter(new Stop(stmt), stmt);
            Exit();
            Dispatcher.OnStopStatement(stmt);
        }

        public virtual void OnExitMethodStatement(ExitMethodStatement stmt)
        {
            Enter(new ExitMethod(stmt), stmt);
            Exit();
            Dispatcher.OnExitMethodStatement(stmt);
        }

        public virtual void OnExitProgramStatement(ExitProgramStatement stmt)
        {
            Enter(new ExitProgram(stmt), stmt);
            Exit();
            Dispatcher.OnExitProgramStatement(stmt);
        }

        public virtual void OnGobackStatement(GobackStatement stmt)
        {
            Enter(new Goback(stmt), stmt);
            Exit();
            Dispatcher.OnGobackStatement(stmt);
        }

        public virtual void OnCloseStatement(CloseStatement stmt)
        {
            Enter(new Close(stmt), stmt);
            Exit();
            Dispatcher.OnCloseStatement(stmt);
        }

        public virtual void OnDisplayStatement(DisplayStatement stmt)
        {
            Enter(new Display(stmt), stmt);
            Exit();
            Dispatcher.OnDisplayStatement(stmt);
        }

        public virtual void OnOpenStatement(OpenStatement stmt)
        {
            Enter(new Open(stmt), stmt);
            Exit();
            Dispatcher.OnOpenStatement(stmt);
        }

        public virtual void OnMergeStatement(MergeStatement stmt)
        {
            Enter(new Merge(stmt), stmt);
            Exit();
            Dispatcher.OnMergeStatement(stmt);
        }

        public virtual void OnReleaseStatement(ReleaseStatement stmt)
        {
            Enter(new Release(stmt), stmt);
            Exit();
            Dispatcher.OnReleaseStatement(stmt);
        }

        public virtual void OnSortStatement(SortStatement stmt)
        {
            Enter(new Sort(stmt), stmt);
            Exit();
            Dispatcher.OnSortStatement(stmt);
        }

        public virtual void OnAlterStatement(AlterStatement stmt)
        {
            Enter(new Alter(stmt), stmt);
            Exit();
            Dispatcher.OnAlterStatement(stmt);
        }

        public void OnExitStatement(ExitStatement stmt)
        {
            Enter(new Exit(stmt), stmt);
            Exit();
            Dispatcher.OnExitStatement(stmt);
        }

        public virtual void OnGotoStatement(GotoStatement stmt)
        {
            Enter(new Goto(stmt), stmt);
            Exit();
            Dispatcher.OnGotoStatement(stmt);
        }

        public virtual void OnPerformProcedureStatement(PerformProcedureStatement stmt)
        {
            Enter(new PerformProcedure(stmt), stmt);
            Exit();
            Dispatcher.OnPerformProcedureStatement(stmt);
        }

        public virtual void OnCancelStatement(CancelStatement stmt)
        {
            Enter(new Cancel(stmt), stmt);
            Exit();
            Dispatcher.OnCancelStatement(stmt);
        }

        public virtual void OnProcedureStyleCall(ProcedureStyleCallStatement stmt, CallStatementEnd end)
        {
            Enter(new ProcedureStyleCall(stmt), stmt);
            AttachEndIfExists(end);
            Exit();
            Dispatcher.OnProcedureStyleCall(stmt, end);
        }

        public virtual void OnExecStatement(ExecStatement stmt)
        {
            Enter(new Exec(stmt), stmt);
            Exit();
            Dispatcher.OnExecStatement(stmt);
        }

        public virtual void StartAddStatementConditional(TypeCobol.Compiler.CodeElements.AddStatement stmt)
        {
            Enter(new Add(stmt), stmt);
            Dispatcher.StartAddStatementConditional(stmt);
        }

        public virtual void EndAddStatementConditional(TypeCobol.Compiler.CodeElements.AddStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndAddStatementConditional(end);
        }

        public virtual void StartCallStatementConditional(TypeCobol.Compiler.CodeElements.CallStatement stmt)
        {
            Enter(new Call(stmt), stmt);
            Dispatcher.StartCallStatementConditional(stmt);
        }

        public virtual void EndCallStatementConditional(TypeCobol.Compiler.CodeElements.CallStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndCallStatementConditional(end);
        }

        public virtual void StartComputeStatementConditional(TypeCobol.Compiler.CodeElements.ComputeStatement stmt)
        {
            Enter(new Compute(stmt), stmt);
            Dispatcher.StartComputeStatementConditional(stmt);
        }

        public virtual void EndComputeStatementConditional(TypeCobol.Compiler.CodeElements.ComputeStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndComputeStatementConditional(end);
        }

        public virtual void StartOnSizeError(TypeCobol.Compiler.CodeElements.OnSizeErrorCondition cond)
        {
            Enter(new OnSizeError(cond), cond);
            Dispatcher.StartOnSizeError(cond);
        }

        public virtual void EndOnSizeError()
        {
            Exit();
            Dispatcher.EndOnSizeError();
        }

        public virtual void StartNoSizeError(TypeCobol.Compiler.CodeElements.NotOnSizeErrorCondition cond)
        {
            Enter(new NoSizeError(cond), cond);
            Dispatcher.StartNoSizeError(cond);
        }

        public virtual void EndNoSizeError()
        {
            Exit();
            Dispatcher.EndNoSizeError();
        }

        public virtual void StartOnException(TypeCobol.Compiler.CodeElements.OnExceptionCondition cond)
        {
            Enter(new OnException(cond), cond);
            Dispatcher.StartOnException(cond);
        }

        public virtual void EndOnException()
        {
            Exit();
            Dispatcher.EndOnException();
        }

        public virtual void StartNoException(TypeCobol.Compiler.CodeElements.NotOnExceptionCondition cond)
        {
            Enter(new NoException(cond), cond);
            Dispatcher.StartNoException(cond);
        }

        public virtual void EndNoException()
        {
            Exit();
            Dispatcher.EndNoException();
        }

        public virtual void StartOnOverflow(TypeCobol.Compiler.CodeElements.OnOverflowCondition cond)
        {
            Enter(new OnOverflow(cond), cond);
            Dispatcher.StartOnOverflow(cond);
        }

        public virtual void EndOnOverflow()
        {
            Exit();
            Dispatcher.EndOnOverflow();
        }

        public virtual void StartNoOverflow(TypeCobol.Compiler.CodeElements.NotOnOverflowCondition cond)
        {
            Enter(new NoOverflow(cond), cond);
            Dispatcher.StartNoOverflow(cond);
        }

        public virtual void EndNoOverflow()
        {
            Exit();
            Dispatcher.EndNoOverflow();
        }

        public virtual void StartOnInvalidKey(TypeCobol.Compiler.CodeElements.InvalidKeyCondition cond)
        {
            Enter(new OnInvalidKey(cond), cond);
            Dispatcher.StartOnInvalidKey(cond);
        }

        public virtual void EndOnInvalidKey()
        {
            Exit();
            Dispatcher.EndOnInvalidKey();
        }

        public virtual void StartNoInvalidKey(TypeCobol.Compiler.CodeElements.NotInvalidKeyCondition cond)
        {
            Enter(new NoInvalidKey(cond), cond);
            Dispatcher.StartNoInvalidKey(cond);
        }

        public virtual void EndNoInvalidKey()
        {
            Exit();
            Dispatcher.EndNoInvalidKey();
        }

        public virtual void StartOnAtEnd(TypeCobol.Compiler.CodeElements.AtEndCondition cond)
        {
            Enter(new OnAtEnd(cond), cond);
            Dispatcher.StartOnAtEnd(cond);
        }

        public virtual void EndOnAtEnd()
        {
            Exit();
            Dispatcher.EndOnAtEnd();
        }

        public virtual void StartNoAtEnd(TypeCobol.Compiler.CodeElements.NotAtEndCondition cond)
        {
            Enter(new NoAtEnd(cond), cond);
            Dispatcher.StartNoAtEnd(cond);
        }

        public virtual void EndNoAtEnd()
        {
            Exit();
            Dispatcher.EndNoAtEnd();
        }

        public virtual void StartDeleteStatementConditional(TypeCobol.Compiler.CodeElements.DeleteStatement stmt)
        {
            Enter(new Delete(stmt), stmt);
            Dispatcher.StartDeleteStatementConditional(stmt);
        }

        public virtual void EndDeleteStatementConditional(TypeCobol.Compiler.CodeElements.DeleteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndDeleteStatementConditional(end);
        }

        public virtual void StartDivideStatementConditional(TypeCobol.Compiler.CodeElements.DivideStatement stmt)
        {
            Enter(new Divide(stmt), stmt);
            Dispatcher.StartDivideStatementConditional(stmt);
        }

        public virtual void EndDivideStatementConditional(TypeCobol.Compiler.CodeElements.DivideStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndDivideStatementConditional(end);
        }

        public virtual void StartEvaluateStatementWithBody(TypeCobol.Compiler.CodeElements.EvaluateStatement stmt)
        {
            Enter(new Evaluate(stmt), stmt);// enter EVALUATE
            Dispatcher.StartEvaluateStatementWithBody(stmt);
        }

        public virtual void EndEvaluateStatementWithBody(TypeCobol.Compiler.CodeElements.EvaluateStatementEnd end)
        {
            AttachEndIfExists(end);// exit EVALUATE
            Exit();
            Dispatcher.EndEvaluateStatementWithBody(end);
        }

        public virtual void StartWhenConditionClause(List<TypeCobol.Compiler.CodeElements.CodeElement> conditions)
        {
            Enter(new WhenGroup(), null);// enter WHEN group
            foreach (var cond in conditions)
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

                    var conditionNameConditionOrSwitchStatusCondition = whensearch.Condition as ConditionNameConditionOrSwitchStatusCondition;
                    if (conditionNameConditionOrSwitchStatusCondition != null)
                        condition.StorageAreaReads = new List<StorageArea>
                        {
                            conditionNameConditionOrSwitchStatusCondition.ConditionReference
                        };
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
            Dispatcher.StartWhenConditionClause(conditions);
        }


        public virtual void EndWhenConditionClause()
        {
            Exit();// exit THEN
            Dispatcher.EndWhenConditionClause();
        }

        public virtual void StartWhenOtherClause(TypeCobol.Compiler.CodeElements.WhenOtherCondition cond)
        {
            Enter(new WhenOther(cond), cond);// enter WHEN OTHER
            Dispatcher.StartWhenOtherClause(cond);
        }

        public virtual void EndWhenOtherClause()
        {
            Exit();// exit WHEN OTHER
            Dispatcher.EndWhenOtherClause();
        }

        public virtual void StartIfStatementWithBody(TypeCobol.Compiler.CodeElements.IfStatement stmt)
        {
            Enter(new If(stmt), stmt);
            Enter(new Then(), stmt);
            Dispatcher.StartIfStatementWithBody(stmt);
        }
        public virtual void EnterElseClause(TypeCobol.Compiler.CodeElements.ElseCondition clause)
        {
            Exit();// we want ELSE to be child of IF, not THEN, so exit THEN
            Enter(new Else(clause), clause);// ELSE
            Dispatcher.EnterElseClause(clause);
        }
        public virtual void EndIfStatementWithBody(TypeCobol.Compiler.CodeElements.IfStatementEnd end)
        {
            Exit(); // Exit ELSE (if any) or THEN
            AttachEndIfExists(end);
            // DO NOT Exit() IF node because this will be done in ExitStatement
            Exit();//JCM exit any way ???
            Dispatcher.EndIfStatementWithBody(end);
        }

        public virtual void AddNextSentenceStatement(TypeCobol.Compiler.CodeElements.NextSentenceStatement stmt)
        {
            Enter(new NextSentence(stmt));
            Exit();
            Dispatcher.AddNextSentenceStatement(stmt);
        }

        public virtual void StartInvokeStatementConditional(TypeCobol.Compiler.CodeElements.InvokeStatement stmt)
        {
            Enter(new Invoke(stmt), stmt);
            Dispatcher.StartInvokeStatementConditional(stmt);
        }

        public virtual void EndInvokeStatementConditional(TypeCobol.Compiler.CodeElements.InvokeStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndInvokeStatementConditional(end);
        }

        public virtual void StartMultiplyStatementConditional(TypeCobol.Compiler.CodeElements.MultiplyStatement stmt)
        {
            Enter(new Multiply(stmt), stmt);
            Dispatcher.StartMultiplyStatementConditional(stmt);
        }

        public virtual void EndMultiplyStatementConditional(TypeCobol.Compiler.CodeElements.MultiplyStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndMultiplyStatementConditional(end);
        }

        public virtual void StartPerformStatementWithBody(TypeCobol.Compiler.CodeElements.PerformStatement stmt)
        {
            Enter(new Perform(stmt), stmt);
            Dispatcher.StartPerformStatementWithBody(stmt);
        }

        public virtual void EndPerformStatementWithBody(TypeCobol.Compiler.CodeElements.PerformStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndPerformStatementWithBody(end);
        }

        public virtual void StartSearchStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.SearchStatement stmt)
        {
            Enter(new Search(stmt), stmt);
            Dispatcher.StartSearchStatementWithBody(stmt);
        }

        public virtual void EndSearchStatementWithBody(TypeCobol.Compiler.CodeElements.SearchStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndSearchStatementWithBody(end);
        }

        public virtual void StartWhenSearchConditionClause(TypeCobol.Compiler.CodeElements.WhenSearchCondition condition)
        {
            Enter(new WhenSearch(condition), condition);
            Dispatcher.StartWhenSearchConditionClause(condition);
        }

        public virtual void EndWhenSearchConditionClause()
        {
            Exit(); // WHEN
            Dispatcher.EndWhenSearchConditionClause();
        }

        public virtual void EnterReadStatementConditional(TypeCobol.Compiler.CodeElements.ReadStatement stmt)
        {
            Enter(new Read(stmt), stmt);
            Dispatcher.EnterReadStatementConditional(stmt);
        }

        public virtual void EndReadStatementConditional(TypeCobol.Compiler.CodeElements.ReadStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndReadStatementConditional(end);
        }

        public virtual void EnterReturnStatementConditional(TypeCobol.Compiler.CodeElements.ReturnStatement stmt)
        {
            Enter(new Return(stmt), stmt);
            Dispatcher.EnterReturnStatementConditional(stmt);
        }

        public virtual void EndReturnStatementConditional(TypeCobol.Compiler.CodeElements.ReturnStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndReturnStatementConditional(end);
        }

        public virtual void StartRewriteStatementConditional(TypeCobol.Compiler.CodeElements.RewriteStatement stmt)
        {
            Enter(new Rewrite(stmt), stmt);
            Dispatcher.StartRewriteStatementConditional(stmt);
        }

        public virtual void EndRewriteStatementConditional(TypeCobol.Compiler.CodeElements.RewriteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndRewriteStatementConditional(end);
        }

        public virtual void StartStartStatementConditional(TypeCobol.Compiler.CodeElements.StartStatement stmt)
        {
            Enter(new Start(stmt), stmt);
            Dispatcher.StartStartStatementConditional(stmt);
        }

        public virtual void EndStartStatementConditional(TypeCobol.Compiler.CodeElements.StartStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndStartStatementConditional(end);
        }

        public virtual void StartStringStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.StringStatement stmt)
        {
            Enter(new Nodes.String(stmt), stmt);
            Dispatcher.StartStringStatementConditional(stmt);
        }

        public virtual void EndStringStatementConditional(TypeCobol.Compiler.CodeElements.StringStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndStringStatementConditional(end);
        }

        public virtual void StartSubtractStatementConditional(TypeCobol.Compiler.CodeElements.SubtractStatement stmt)
        {
            Enter(new Subtract(stmt), stmt);
            Dispatcher.StartSubtractStatementConditional(stmt);
        }

        public virtual void EndSubtractStatementConditional(TypeCobol.Compiler.CodeElements.SubtractStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndSubtractStatementConditional(end);
        }

        public virtual void StartUnstringStatementConditional(TypeCobol.Compiler.CodeElements.UnstringStatement stmt)
        {
            Enter(new Unstring(stmt), stmt);
            Dispatcher.StartUnstringStatementConditional(stmt);
        }

        public virtual void EndUnstringStatementConditional(TypeCobol.Compiler.CodeElements.UnstringStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndUnstringStatementConditional(end);
        }

        public virtual void StartWriteStatementConditional(TypeCobol.Compiler.CodeElements.WriteStatement stmt)
        {
            Enter(new Write(stmt), stmt);
            Dispatcher.StartWriteStatementConditional(stmt);
        }

        public virtual void EndWriteStatementConditional(TypeCobol.Compiler.CodeElements.WriteStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndWriteStatementConditional(end);
        }

        public virtual void StartXmlGenerateStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlGenerateStatement stmt)
        {
            Enter(new XmlGenerate(stmt), stmt);
            Dispatcher.StartXmlGenerateStatementConditional(stmt);
        }

        public virtual void EndXmlGenerateStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndXmlGenerateStatementConditional(end);
        }

        public virtual void StartXmlParseStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlParseStatement stmt)
        {
            Enter(new XmlParse(stmt), stmt);
            Dispatcher.StartXmlParseStatementConditional(stmt);
        }

        public virtual void EndXmlParseStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end)
        {
            AttachEndIfExists(end);
            Exit();
            Dispatcher.EndXmlParseStatementConditional(end);
        }

    }
}
