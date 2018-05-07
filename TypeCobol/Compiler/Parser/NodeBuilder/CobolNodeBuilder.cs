using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Tools;
using Analytics;
using Castle.Core.Internal;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a Program or Class object while visiting its parse tree
    /// </summary>
    public class CobolNodeBuilder : ProgramClassBaseListener
    {
        /// <summary>
        /// Program object resulting of the visit the parse tree
        /// </summary>
        private Program Program { get; set; }
        public SyntaxTree<ParserRuleContext> SyntaxTree { get; set; }

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

                    if(nameSpaceTable != null)
                    {
                        TableOfNamespaces = new SymbolTable(TableOfIntrisic, SymbolTable.Scope.Namespace); //Set TableOfNamespace with program if dependencies were given. (See CLI.cs runOnce2() LoadDependencies())
                        TableOfNamespaces.CopyAllPrograms(nameSpaceTable.Programs.Values);
                    }
                   
                }
                // TODO#249: use a COPY for these
                foreach (var type in DataType.BuiltInCustomTypes)
                    TableOfIntrisic.AddType(DataType.CreateBuiltIn(type)); //Add default TypeCobol types BOOLEAN and DATE
            }
        }



        public NodeDispatcher<ParserRuleContext> Dispatcher { get; internal set; }



        public Node CurrentNode { get { return SyntaxTree.CurrentNode; } }

        public Dictionary<CodeElement, Node> NodeCodeElementLinkers = new Dictionary<CodeElement, Node>();

        private void Enter([NotNull] Node node, ParserRuleContext context = null, SymbolTable table = null)
        {
            node.SymbolTable = table ?? SyntaxTree.CurrentNode.SymbolTable;
            SyntaxTree.Enter(node, context);

            if (node.CodeElement != null)
                NodeCodeElementLinkers.Add(node.CodeElement, node);
        }
        private void Exit()
        {
            var node = SyntaxTree.CurrentNode;
            var context = SyntaxTree.CurrentContext;
            Dispatcher.OnNode(node, context, CurrentProgram);
            SyntaxTree.Exit();
        }

        public List<Diagnostic> GetDiagnostics(ProgramClassParser.CobolCompilationUnitContext context)
        {
            List<Diagnostic> diagnostics = new List<Diagnostic>();
            AddDiagnosticsAttachedInContext(diagnostics, context);
            return diagnostics.Count > 0 ? diagnostics : null;
        }

        private void AddDiagnosticsAttachedInContext(List<Diagnostic> diagnostics, ParserRuleContext context)
        {
            var ruleNodeWithDiagnostics = (ParserRuleContextWithDiagnostics)context;
            if (ruleNodeWithDiagnostics.Diagnostics != null)
            {
                foreach (var ruleDiagnostic in ruleNodeWithDiagnostics.Diagnostics)
                {
                    diagnostics.Add(ruleDiagnostic);
                }
            }
            if (context.children != null)
            {
                foreach (var childNode in context.children)
                {
                    if (childNode is IRuleNode)
                    {
                        AddDiagnosticsAttachedInContext(diagnostics, (ParserRuleContext)((IRuleNode)childNode).RuleContext);
                    }
                }
            }
        }

        /// <summary>
        /// Initialization code run before parsing each new Program or Class
        /// </summary>
        public override void EnterCobolCompilationUnit(ProgramClassParser.CobolCompilationUnitContext context)
        {
            if(TableOfNamespaces == null)
                TableOfNamespaces = new SymbolTable(TableOfIntrisic, SymbolTable.Scope.Namespace);

            TableOfGlobals = new SymbolTable(TableOfNamespaces, SymbolTable.Scope.Global);
            Program = null;
           
            SyntaxTree.Root.SymbolTable = TableOfNamespaces; //Set SymbolTable of SourceFile Node, Limited to NameSpace and Intrinsic scopes
        }

        public override void EnterCobolProgram(ProgramClassParser.CobolProgramContext context)
        {
            ProgramIdentification programIdentification = null;
            var pgm = context.programAttributes();
            if (pgm != null)
                programIdentification = (ProgramIdentification)pgm.ProgramIdentification().Symbol;

            if (Program == null)
            {
                Program = new SourceProgram(TableOfGlobals, programIdentification);
                programsStack = new Stack<Program>();
                CurrentProgram = Program;
                Enter(CurrentProgram, context, CurrentProgram.SymbolTable);
            }
            else
            {
                var enclosing = CurrentProgram;
                CurrentProgram = new NestedProgram(enclosing, programIdentification);
                Enter(CurrentProgram, context, CurrentProgram.SymbolTable);
            }
            
            if (pgm != null && pgm.LibraryCopy() != null)
            { // TCRFUN_LIBRARY_COPY
                var cnode = new LibraryCopy((LibraryCopyCodeElement)pgm.LibraryCopy().Symbol);
                Enter(cnode, pgm, CurrentProgram.SymbolTable);
                Exit();
            }

            TableOfNamespaces.AddProgram(CurrentProgram); //Add Program to Namespace table. 
        }

        public override void ExitCobolProgram(ProgramClassParser.CobolProgramContext context)
        {
            AttachEndIfExists(context.ProgramEnd());
            Exit();
            programsStack.Pop();

            if (programsStack.Count == 0) //Means that we ended a main program, reset Table and program in case of a new program declaration before EOF. 
            {
                TableOfGlobals = new SymbolTable(TableOfNamespaces, SymbolTable.Scope.Global);
                Program = null;
            }
        }

        public override void EnterEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context)
        {
            var terminal = context.EnvironmentDivisionHeader();
            var header = terminal != null ? (EnvironmentDivisionHeader)terminal.Symbol : null;
            Enter(new EnvironmentDivision(header), context);
        }
        public override void ExitEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context)
        {
            Exit();
        }

        public override void EnterConfigurationSection(ProgramClassParser.ConfigurationSectionContext context)
        {
            var terminal = context.ConfigurationSectionHeader();
            var header = terminal != null ? (ConfigurationSectionHeader)terminal.Symbol : null;
            Enter(new ConfigurationSection(header), context);
            foreach (var paragraph in context.configurationParagraph())
            {
                if (paragraph.SourceComputerParagraph() != null)
                {
                    Enter(new SourceComputer((SourceComputerParagraph)paragraph.SourceComputerParagraph().Symbol));
                    Exit();
                }
                if (paragraph.ObjectComputerParagraph() != null)
                {
                    Enter(new ObjectComputer((ObjectComputerParagraph)paragraph.ObjectComputerParagraph().Symbol));
                    Exit();
                }
                if (paragraph.SpecialNamesParagraph() != null)
                {
                    Enter(new SpecialNames((SpecialNamesParagraph)paragraph.SpecialNamesParagraph().Symbol));
                    Exit();
                }
                if (paragraph.RepositoryParagraph() != null)
                {
                    Enter(new Repository((RepositoryParagraph)paragraph.RepositoryParagraph().Symbol));
                    Exit();
                }
            }
        }
        /// <summary>Retrieve currency characters from SPECIAL NAMES paragraph.</summary>
        /// <returns>Currency characters array</returns>
        private char[] GetCurrencies()
        {
            IDictionary<AlphanumericValue, CharacterValue> currencies = null;
            var special = SyntaxTree.Root.Get<SpecialNames>("special-names");
            if (special != null) currencies = special.CodeElement().CurrencySymbols;
            if (currencies == null || currencies.Count < 1) return new[] { '$' };
            var chars = new List<char>();
            foreach (var key in currencies.Keys)
                if (key.Value.Length == 1) chars.Add(key.Value[0]);
            return chars.ToArray();
        }
        public override void ExitConfigurationSection(ProgramClassParser.ConfigurationSectionContext context)
        {
            Exit(); // exit ConfigurationSection node
        }

        public override void EnterDataDivision(ProgramClassParser.DataDivisionContext context)
        {
            var terminal = context.DataDivisionHeader();
            var header = terminal != null ? (DataDivisionHeader)terminal.Symbol : null;
            Enter(new DataDivision(header), context);
        }
        public override void ExitDataDivision(ProgramClassParser.DataDivisionContext context)
        {
            Exit();
        }


        public override void EnterInputOutputSection([NotNull] ProgramClassParser.InputOutputSectionContext context)
        {
            var terminal = context.InputOutputSectionHeader();
            var header = terminal != null ? (InputOutputSectionHeader)terminal.Symbol : null;
            Enter(new InputOutputSection(header), context);
        }

        public override void ExitInputOutputSection([NotNull] ProgramClassParser.InputOutputSectionContext context)
        {
            Exit();
        }

        public override void EnterFileControlParagraph([NotNull] ProgramClassParser.FileControlParagraphContext context)
        {
            var terminal = context.FileControlParagraphHeader();
            var header = terminal != null ? (FileControlParagraphHeader)terminal.Symbol : null;
            Enter(new FileControlParagraphHeaderNode(header), context);


            var entries = context.FileControlEntry();
            if (entries != null)
            {
                foreach (ITerminalNode entry in entries)
                {
                    var fileControlEntry = new FileControlEntryNode((FileControlEntry)entry.Symbol);
                    Enter(fileControlEntry, context);
                    Exit(); //Exit here, so next FileControlEtry will be child of FileControlParagraph
                }
            }

        }

        public override void ExitFileControlParagraph([NotNull] ProgramClassParser.FileControlParagraphContext context)
        {
            Exit();
        }


        /// <summary>parent: DATA DIVISION</summary>
        /// <param name="context">FILE SECTION</param>
        public override void EnterFileSection(ProgramClassParser.FileSectionContext context)
        {
            var terminal = context.FileSectionHeader();
            var header = terminal != null ? (FileSectionHeader)terminal.Symbol : null;
            Enter(new FileSection(header), context);

            //FileDescriptionEntry and DataDescriptionEntry are set with their own methods:
            //EnterFileDescriptionEntry and EnterDataDescriptionEntry
        }

        public override void EnterFileDescriptionNode([NotNull] ProgramClassParser.FileDescriptionNodeContext context)
        {
            var terminal = context.FileDescriptionEntry();
            var entry = terminal != null ? (FileDescriptionEntry)terminal.Symbol : null;
            Enter(new FileDescriptionEntryNode(entry), context);
        }

        public override void ExitFileDescriptionNode([NotNull] ProgramClassParser.FileDescriptionNodeContext context)
        {
            Exit();
        }


        public override void ExitFileSection(ProgramClassParser.FileSectionContext context)
        {
            ExitLastLevel1Definition();
            Exit();
        }
        /// <summary>parent: DATA DIVISION</summary>
        /// <param name="context">WORKING-STORAGE SECTION</param>
        public override void EnterWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context)
        {
            var terminal = context.WorkingStorageSectionHeader();
            var header = terminal != null ? (WorkingStorageSectionHeader)terminal.Symbol : null;
            Enter(new WorkingStorageSection(header), context);
        }
        public override void ExitWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context)
        {
            ExitLastLevel1Definition();
            Exit(); // Exit WorkingStorageSection
        }
        /// <summary>parent: DATA DIVISION</summary>
        /// <param name="context">LOCAL-STORAGE SECTION</param>
        public override void EnterLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context)
        {
            var terminal = context.LocalStorageSectionHeader();
            var header = terminal != null ? (LocalStorageSectionHeader)terminal.Symbol : null;
            Enter(new LocalStorageSection(header), context);
        }
        public override void ExitLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context)
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LocalStorageSection
        }
        /// <summary>parent: DATA DIVISION</summary>
        /// <param name="context">LINKAGE SECTION</param>
        public override void EnterLinkageSection(ProgramClassParser.LinkageSectionContext context)
        {
            var terminal = context.LinkageSectionHeader();
            var header = terminal != null ? (LinkageSectionHeader)terminal.Symbol : null;
            Enter(new LinkageSection(header), context);
        }
        public override void ExitLinkageSection(ProgramClassParser.LinkageSectionContext context)
        {
            ExitLastLevel1Definition();
            Exit(); // Exit LinkageSection
        }

        public override void EnterDataDefinitionEntry(ProgramClassParser.DataDefinitionEntryContext context)
        {
            if (context.DataDescriptionEntry() != null)
            {
                var data = (DataDescriptionEntry)context.DataDescriptionEntry().Symbol;
                var dataTypeDescriptionEntry = data as DataTypeDescriptionEntry;
                if (dataTypeDescriptionEntry != null) EnterTypeDefinitionEntry(dataTypeDescriptionEntry);
                else EnterDataDescriptionEntry(data);
            }
            if (context.DataConditionEntry() != null)
                EnterDataConditionEntry((DataConditionEntry)context.DataConditionEntry().Symbol);
            if (context.DataRedefinesEntry() != null)
                EnterDataRedefinesEntry((DataRedefinesEntry)context.DataRedefinesEntry().Symbol);
            if (context.DataRenamesEntry() != null)
                EnterDataRenamesEntry((DataRenamesEntry)context.DataRenamesEntry().Symbol);
        }
        // [COBOL 2002]
        private void EnterTypeDefinitionEntry(DataTypeDescriptionEntry typedef)
        {
            SetCurrentNodeToTopLevelItem(typedef.LevelNumber);
            var node = new TypeDefinition(typedef);
            Enter(node);
            SymbolTable table;
            if (node.CodeElement().IsGlobal) // TCTYPE_GLOBAL_TYPEDEF
                table = node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Global);
            else
                table = node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);
            table.AddType(node);

            AnalyticsWrapper.Telemetry.TrackEvent("[Type-Declared] " + node.Name, EventType.TypeCobolUsage);
        }
        // [/COBOL 2002]

        private void AddToSymbolTable(DataDescription node)
        {
            if (node.IsPartOfATypeDef) return;
            var table = node.SymbolTable;
            if (node.CodeElement().IsGlobal)
                table = table.GetTableFromScope(SymbolTable.Scope.Global);

            table.AddVariable(node);
        }

        private void EnterDataDescriptionEntry(DataDescriptionEntry data)
        {
            SetCurrentNodeToTopLevelItem(data.LevelNumber);

            //Update DataType of CodeElement by searching info on the declared Type into SymbolTable.
            //Note that the AST is not complete here, but you can only refer to a Type that has previously been defined.
            var node = new DataDescription(data);
            Enter(node);

            if (data.Indexes != null && data.Indexes.Any())
            {
                var table = node.SymbolTable;
                foreach (var index in data.Indexes)
                {
                    if (node.CodeElement().IsGlobal)
                        table = table.GetTableFromScope(SymbolTable.Scope.Global);

                    var indexNode = new IndexDefinition(index);
                    Enter(indexNode, null, table);
                    if (!indexNode.IsPartOfATypeDef) //If index is inside a Typedef do not add to symboltable
                        table.AddVariable(indexNode);
                    Exit();
                }
            }

            var types = node.SymbolTable.GetType(node);
            if (types.Count == 1)
            {
                data.DataType.RestrictionLevel = types[0].DataType.RestrictionLevel;
            }
            //else do nothing, it's an error that will be handled by Cobol2002Checker

            var parent = node.Parent;
            while(parent !=null)
            {
                if (parent is WorkingStorageSection)
                {
                    //Set flag to know that this node belongs to working storage section
                    node.SetFlag(Node.Flag.WorkingSectionNode, true); 
                    break;
                }
                else if (parent is LinkageSection)
                {
                    //Set flag to know that this node belongs to linkage section
                    node.SetFlag(Node.Flag.LinkageSectionNode, true);
                    break;
                }
                else if (parent is LocalStorageSection)
                {
                    //Set flag to know that this node belongs to local storage section
                    node.SetFlag(Node.Flag.LocalStorageSectionNode, true);
                    break;
                }
                else if (parent is FileSection)
                {
                    node.SetFlag(Node.Flag.FileSectionNode, true);
                    break;
                }
                parent = parent.Parent;
            }

            AddToSymbolTable(node);
        }

        private void EnterDataConditionEntry(DataConditionEntry data)
        {
            SetCurrentNodeToTopLevelItem(data.LevelNumber);
            var node = new DataCondition(data);
            Enter(node);
            if (!node.IsPartOfATypeDef) node.SymbolTable.AddVariable(node);
        }

        private void EnterDataRedefinesEntry(DataRedefinesEntry data)
        {
            SetCurrentNodeToTopLevelItem(data.LevelNumber);
            var node = new DataRedefines(data);
            Enter(node);
            if (!node.IsPartOfATypeDef) node.SymbolTable.AddVariable(node);
        }

        private void EnterDataRenamesEntry(DataRenamesEntry data)
        {
            SetCurrentNodeToTopLevelItem(data.LevelNumber);
            var node = new DataRenames(data);
            Enter(node);
            if (!node.IsPartOfATypeDef) node.SymbolTable.AddVariable(node);
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

        /// <summary>Exit last level-01 data definition entry, as long as all its subordinates.</summary>
        private void ExitLastLevel1Definition()
        {
            while (CurrentNode.CodeElement != null && TypeCobol.Tools.Reflection.IsTypeOf(CurrentNode.CodeElement.GetType(), typeof(DataDefinitionEntry))) Exit();
        }



        public override void EnterProcedureDivision(ProgramClassParser.ProcedureDivisionContext context)
        {
            var terminal = context.ProcedureDivisionHeader();
            var header = terminal != null ? (ProcedureDivisionHeader)terminal.Symbol : null;
            Enter(new ProcedureDivision(header), context);
        }
        public override void ExitProcedureDivision(ProgramClassParser.ProcedureDivisionContext context)
        {
            Exit();
        }

        public override void EnterDeclaratives(ProgramClassParser.DeclarativesContext context)
        {
            var terminal = context.DeclarativesHeader();
            var header = terminal != null ? (DeclarativesHeader)terminal.Symbol : null;
  
            var node = new Declaratives(header);
            Enter(node, context);
        }

        public override void ExitDeclaratives(ProgramClassParser.DeclarativesContext context)
        {
            AttachEndIfExists(context.DeclarativesEnd());
            Exit();
        }


        private Tools.UIDStore uidfactory = new Tools.UIDStore();
        /// <summary>Parent node: PROCEDURE DIVISION</summary>
        /// <param name="context">DECLARE FUNCTION</param>
        public override void EnterFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context)
        {
            var terminal = context.FunctionDeclarationHeader();
            var header = (FunctionDeclarationHeader) terminal?.Symbol;
            header?.SetLibrary(CurrentProgram.Identification.ProgramName.Name);
            var node = new FunctionDeclaration(header)
            {
                Label = uidfactory.FromOriginal(header?.FunctionName.Name),
                Library = CurrentProgram.Identification.ProgramName.Name
            };
            //DO NOT change this without checking all references of Library. 
            // (SymbolTable - function, type finding could be impacted) 

            //Function must be added to Declarations scope
            var declarationSymbolTable = SyntaxTree.CurrentNode.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);
            declarationSymbolTable.AddFunction(node);
            Enter(node, context, new SymbolTable(declarationSymbolTable, SymbolTable.Scope.Function));

            var declaration = (FunctionDeclarationHeader)CurrentNode.CodeElement;
            var funcProfile = ((FunctionDeclaration)CurrentNode).Profile; //Get functionprofile to set parameters

            foreach (var parameter in declaration.Profile.InputParameters) //Set Input Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                funcProfile.InputParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
            }
            foreach (var parameter in declaration.Profile.OutputParameters) //Set Output Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
                funcProfile.OutputParameters.Add(paramNode);

                paramNode.SetParent(CurrentNode);
                CurrentNode.SymbolTable.AddVariable(paramNode);
            }
            foreach (var parameter in declaration.Profile.InoutParameters) //Set Inout Parameters
            {
                var paramNode = new ParameterDescription(parameter);
                paramNode.SymbolTable = CurrentNode.SymbolTable;
                paramNode.SetFlag(Node.Flag.LinkageSectionNode, true);
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

            AnalyticsWrapper.Telemetry.TrackEvent("[Function-Declared] " + declaration.FunctionName, EventType.TypeCobolUsage);
        }
        public override void ExitFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context)
        {
            var terminal = context.FunctionDeclarationEnd();
            var end = terminal != null ? terminal.Symbol as FunctionDeclarationEnd : null;
            Enter(new FunctionEnd(end), context);
            Exit();
            Exit();// exit DECLARE FUNCTION
        }

       
        /// <summary>Parent node: DECLARE FUNCTION</summary>
        /// <param name="context">PROCEDURE DIVISION</param>
        public override void EnterFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context)
        {
            var header = (ProcedureDivisionHeader)context.ProcedureDivisionHeader().Symbol;
            if (header.UsingParameters != null && header.UsingParameters.Count > 0)
                DiagnosticUtils.AddError(header, "TCRFUN_DECLARATION_NO_USING");//TODO#249

            Enter(new ProcedureDivision(header), context);
        }
        public override void ExitFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context)
        {
            Exit();
        }



        public override void EnterSection(ProgramClassParser.SectionContext context)
        {
            // if we Enter(..) a node here, it will be detached by ExitParagraph
            // if we do not, no need to detach anything in ExitSection
            if (context.SectionHeader() != null)
            {
                SectionHeader header = (SectionHeader)context.SectionHeader().Symbol;
                var section = new Section(header);
                Enter(section, context);
                section.SymbolTable.AddSection(section);
            }
            else
            if (context.ParagraphHeader() != null)
            {
                ParagraphHeader header = (ParagraphHeader)context.ParagraphHeader().Symbol;
                var paragraph = new Paragraph(header);
                Enter(paragraph, context);
                paragraph.SymbolTable.AddParagraph(paragraph);
            }
        }

        public override void EnterParagraph(ProgramClassParser.ParagraphContext context)
        {
            if (context.ParagraphHeader() != null)
            {
                ParagraphHeader header = (ParagraphHeader)context.ParagraphHeader().Symbol;
                var paragraph = new Paragraph(header);
                Enter(paragraph, context);
                paragraph.SymbolTable.AddParagraph(paragraph);
            }
        }
        public override void ExitParagraph(ProgramClassParser.ParagraphContext context)
        {
            if (SyntaxTree.CurrentNode is Paragraph) Exit();
        }

        public override void EnterSentence(ProgramClassParser.SentenceContext context)
        {
            Enter(new Sentence(), context);
        }
        public override void ExitSentence(ProgramClassParser.SentenceContext context)
        {
            AttachEndIfExists(context.SentenceEnd());
            if (CurrentNode is Sentence) Exit();//TODO remove this and check what happens when exiting last CALL in FIN-STANDARD in BigBatch file (ie. CheckPerformance test)
        }

        public override void EnterExecStatementNode([NotNull] ProgramClassParser.ExecStatementNodeContext context)
        {
            if (context.ExecStatement() != null)
            {
                ExecStatement terminal = (ExecStatement)context.ExecStatement().Symbol;
                Enter(new Exec(terminal), context);
            }
        }

        public override void ExitExecStatementNode([NotNull] ProgramClassParser.ExecStatementNodeContext context)
        {
            Exit();
        }


        public override void EnterStatement(ProgramClassParser.StatementContext context)
        {
            if (context.ExecStatement() != null) Enter(new Exec((ExecStatement)context.ExecStatement().Symbol), context);
            else if (context.evaluateStatementWithBody() != null) { }// Node will be created in EnterEvaluateStatementWithBody
            else if (context.ifStatementWithBody() != null) { }// Node will be created in EnterIfStatementWithBody
            else if (context.performStatementWithBody() != null) { }// Node will be created in EnterPerformStatementWithBody
            else if (context.PerformProcedureStatement() != null) Enter(new PerformProcedure((PerformProcedureStatement)context.PerformProcedureStatement().Symbol), context);
            else if (context.searchStatementWithBody() != null) { }// Node will be created in EnterSearchStatementWithBody
                                                                   // -- arithmetic --
            else if (context.AddStatement() != null) Enter(new Add((AddStatement)context.AddStatement().Symbol), context);
            else if (context.addStatementConditional() != null) { }// Node will be created in EnterAddStatementConditional
            else if (context.ComputeStatement() != null) Enter(new Compute((ComputeStatement)context.ComputeStatement().Symbol), context);
            else if (context.computeStatementConditional() != null) { }// Node will be created in EnterComputeStatementConditional
            else if (context.DivideStatement() != null) Enter(new Divide((DivideStatement)context.DivideStatement().Symbol), context);
            else if (context.divideStatementConditional() != null) { }// Node will be created in EnterDivideStatementConditional
            else if (context.MultiplyStatement() != null) Enter(new Multiply((MultiplyStatement)context.MultiplyStatement().Symbol), context);
            else if (context.multiplyStatementConditional() != null) { }// Node will be created in EnterMultiplyStatementConditional
            else if (context.SubtractStatement() != null) Enter(new Subtract((SubtractStatement)context.SubtractStatement().Symbol), context);
            else if (context.subtractStatementConditional() != null) { }// Node will be created in EnterSubtractStatementConditional
                                                                        // -- file --
            else if (context.OpenStatement() != null) Enter(new Open((OpenStatement)context.OpenStatement().Symbol), context);
            else if (context.CloseStatement() != null) Enter(new Close((CloseStatement)context.CloseStatement().Symbol), context);
            else if (context.ReadStatement() != null) Enter(new Read((ReadStatement)context.ReadStatement().Symbol), context);
            else if (context.readStatementConditional() != null) { }// Node will be created in EnterReadStatementConditional
            else if (context.RewriteStatement() != null) Enter(new Rewrite((RewriteStatement)context.RewriteStatement().Symbol), context);
            else if (context.rewriteStatementConditional() != null) { }// Node will be created in EnterRewriteStatementConditional
            else if (context.WriteStatement() != null) Enter(new Write((WriteStatement)context.WriteStatement().Symbol), context);
            else if (context.writeStatementConditional() != null) { }// Node will be created in EnterWriteStatementConditional
                                                                     // -- data movement --
            else if (context.MoveStatement() != null) Enter(new Move((MoveStatement)context.MoveStatement().Symbol), context);
            else if (context.SetStatement() != null) Enter(new Set((SetStatement)context.SetStatement().Symbol), context);
            // -- other --
            else if (context.AcceptStatement() != null) Enter(new Accept((AcceptStatement)context.AcceptStatement().Symbol), context);
            else if (context.AlterStatement() != null) Enter(new Alter((AlterStatement)context.AlterStatement().Symbol), context);
            else if (context.CallStatement() != null) Enter(new Call((CallStatement)context.CallStatement().Symbol), context);
            else if (context.callStatementConditional() != null) { }// Node will be created in EnterCallStatementConditional
            else if (context.ProcedureStyleCall() != null) Enter(new ProcedureStyleCall((ProcedureStyleCallStatement)context.ProcedureStyleCall().Symbol), context);
            else if (context.procedureStyleCallConditional() != null) { }// Node will be created in EnterProcedureStyleCallConditional
            else if (context.CancelStatement() != null) Enter(new Cancel((CancelStatement)context.CancelStatement().Symbol), context);
            else if (context.ContinueStatement() != null) Enter(new Continue((ContinueStatement)context.ContinueStatement().Symbol), context);
            else if (context.DeleteStatement() != null) Enter(new Delete((DeleteStatement)context.DeleteStatement().Symbol), context);
            else if (context.deleteStatementConditional() != null) { }// Node will be created in EnterDeleteStatementConditional
            else if (context.DisplayStatement() != null) Enter(new Display((DisplayStatement)context.DisplayStatement().Symbol), context);
            else if (context.EntryStatement() != null) Enter(new Entry((EntryStatement)context.EntryStatement().Symbol), context);
            else if (context.ExitStatement() != null) Enter(new Exit((ExitStatement)context.ExitStatement().Symbol), context);
            else if (context.ExitMethodStatement() != null) Enter(new ExitMethod((ExitMethodStatement)context.ExitMethodStatement().Symbol), context);
            else if (context.ExitProgramStatement() != null) Enter(new ExitProgram((ExitProgramStatement)context.ExitProgramStatement().Symbol), context);
            else if (context.GobackStatement() != null) Enter(new Goback((GobackStatement)context.GobackStatement().Symbol), context);
            else if (context.GotoStatement() != null) Enter(new Goto((GotoStatement)context.GotoStatement().Symbol), context);
            else if (context.InitializeStatement() != null) Enter(new Initialize((InitializeStatement)context.InitializeStatement().Symbol), context);
            else if (context.InspectStatement() != null) Enter(new Inspect((InspectStatement)context.InspectStatement().Symbol), context);
            else if (context.InvokeStatement() != null) Enter(new Invoke((InvokeStatement)context.InvokeStatement().Symbol), context);
            else if (context.invokeStatementConditional() != null) { }// Node will be created in EnterInvokeStatementConditional
            else if (context.MergeStatement() != null) Enter(new Merge((MergeStatement)context.MergeStatement().Symbol), context);
            else if (context.PerformProcedureStatement() != null) Enter(new PerformProcedure((PerformProcedureStatement)context.PerformProcedureStatement().Symbol), context);
            else if (context.ReleaseStatement() != null) Enter(new Release((ReleaseStatement)context.ReleaseStatement().Symbol), context);
            else if (context.ReturnStatement() != null) Enter(new Return((ReturnStatement)context.ReturnStatement().Symbol), context);
            else if (context.returnStatementConditional() != null) { }// Node will be created in EnterReturnStatementConditional
            else if (context.SortStatement() != null) Enter(new Sort((SortStatement)context.SortStatement().Symbol), context);
            else if (context.StartStatement() != null) Enter(new Start((StartStatement)context.StartStatement().Symbol), context);
            else if (context.startStatementConditional() != null) { }// Node will be created in EnterStartStatementConditional
            else if (context.StopStatement() != null) Enter(new Stop((StopStatement)context.StopStatement().Symbol), context);
            else if (context.StringStatement() != null) Enter(new Nodes.String((StringStatement)context.StringStatement().Symbol), context);
            else if (context.stringStatementConditional() != null) { }// Node will be created in EnterStringStatementConditional
            else if (context.UnstringStatement() != null) Enter(new Unstring((UnstringStatement)context.UnstringStatement().Symbol), context);
            else if (context.unstringStatementConditional() != null) { }// Node will be created in EnterUnstringStatementConditional
            else if (context.XmlGenerateStatement() != null) Enter(new XmlGenerate((XmlGenerateStatement)context.XmlGenerateStatement().Symbol), context);
            else if (context.xmlGenerateStatementConditional() != null) { }// Node will be created in EnterXmlGenerateStatementConditional
            else if (context.XmlParseStatement() != null) Enter(new XmlParse((XmlParseStatement)context.XmlParseStatement().Symbol), context);
            else if (context.xmlParseStatementConditional() != null) { }// Node will be created in EnterXmlParseStatementConditional
            else if (context.GetText().Length < 1) skipEmptyStatement = true;
            else throw new NotImplementedException("Implementation error: \"" + context.GetText() + "\"[" + context.GetType().Name + ']');
        }
        private bool skipEmptyStatement = false;
        public override void ExitStatement(ProgramClassParser.StatementContext context)
        {
            if (skipEmptyStatement) skipEmptyStatement = false;
            else Exit();
        }
        /*TODO#249
                private void FixSubscriptableQualifiedNames(CodeElement statement) {
                    var identifiers = statement as IdentifierUser;
                    if (identifiers == null) return;
                    foreach(var identifier in identifiers.Identifiers) {
                        if (identifier.Name is TypeCobol.Compiler.CodeElements.Expressions.Subscripted) continue;
                        if (identifier is TypeCobol.Compiler.CodeElements.Expressions.Subscriptable) {
                            var found = CurrentProgram.CurrentTable.Get(identifier.Name);
                            if (found.Count != 1) continue;// ambiguity is not our job
                            List<string> errors;
                            var qelement = TypeCobol.Compiler.CodeElements.Expressions.SubscriptedQualifiedName.Create(identifier, found[0], out errors);
                            (identifier as TypeCobol.Compiler.CodeElements.Expressions.Subscriptable).UpdateSubscripting(qelement);
                            foreach(string error in errors) DiagnosticUtils.AddError(statement, error);
                        }
                    }
                }
        */


        public override void EnterIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context)
        {
            var terminal = context.IfStatement();
            var statement = terminal != null ? (IfStatement)terminal.Symbol : null;
            Enter(new If(statement), context);
            Enter(new Then(), context);
        }
        public override void EnterElseClause(ProgramClassParser.ElseClauseContext context)
        {
            Exit();// we want ELSE to be child of IF, not THEN, so exit THEN
            var terminal = context.ElseCondition();
            var condition = terminal != null ? (ElseCondition)terminal.Symbol : null;
            Enter(new Else(condition), context);// ELSE
            if (context.NextSentenceStatement() != null)
            {
                Enter(new NextSentence((NextSentenceStatement)context.NextSentenceStatement().Symbol));
                Exit();
            }
        }
        public override void ExitIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context)
        {
            Exit(); // Exit ELSE (if any) or THEN
            AttachEndIfExists(context.IfStatementEnd());
            // DO NOT Exit() IF node because this will be done in ExitStatement
        }


        public override void EnterEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context)
        {
            var terminal = context.EvaluateStatement();
            var statement = terminal != null ? (EvaluateStatement)terminal.Symbol : null;
            Enter(new Evaluate(statement), context);// enter EVALUATE
        }
        public override void EnterWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context)
        {
            Enter(new WhenGroup(), context);// enter WHEN group
            foreach (var ctxt in context.whenEvaluateCondition())
            {
                WhenCondition condition;
                if (ctxt == null)
                {
                    condition = null;
                }
                else
                if (ctxt.WhenSearchCondition() != null)
                {
                    var whensearch = (WhenSearchCondition)ctxt.WhenSearchCondition().Symbol;
                    condition = new WhenCondition();
                    whensearch.ApplyPropertiesToCE(condition);

                    condition.SelectionObjects = new EvaluateSelectionObject[1];
                    condition.SelectionObjects[0] = new EvaluateSelectionObject();
                    condition.SelectionObjects[0].BooleanComparisonVariable = new BooleanValueOrExpression(whensearch.Condition);
                }
                else
                {
                    condition = (WhenCondition)ctxt.WhenCondition().Symbol;
                }
                Enter(new When(condition), context);
                Exit();
            }
            Exit();// exit WHEN group
            Enter(new Then(), context);// enter THEN
        }
        public override void ExitWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context)
        {
            Exit();// exit THEN
        }
        public override void EnterWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context)
        {
            var terminal = context.WhenOtherCondition();
            var condition = terminal != null ? (WhenOtherCondition)terminal.Symbol : null;
            Enter(new WhenOther(condition), context);// enter WHEN OTHER
        }
        public override void ExitWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context)
        {
            Exit();// exit WHEN OTHER
        }
        public override void ExitEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context)
        {
            AttachEndIfExists(context.EvaluateStatementEnd());// exit EVALUATE
        }


        public override void EnterPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context)
        {
            var terminal = context.PerformStatement();
            var statement = terminal != null ? (PerformStatement)terminal.Symbol : null;
            Enter(new Perform(statement), context);
        }
        public override void ExitPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context)
        {
            AttachEndIfExists(context.PerformStatementEnd());
        }

        public override void EnterSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context)
        {
            var terminal = context.SearchStatement();
            var statement = terminal != null ? (SearchStatement)terminal.Symbol : null;
            Enter(new Search(statement), context);
        }
        public override void EnterWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context)
        {
            var terminal = context.WhenSearchCondition();
            var condition = terminal != null ? (WhenSearchCondition)terminal.Symbol : null;
            Enter(new WhenSearch(condition), context);
            if (context.NextSentenceStatement() != null)
            {
                Enter(new NextSentence((NextSentenceStatement)context.NextSentenceStatement().Symbol));
                Exit();
            }
        }
        public override void ExitWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context)
        {
            Exit(); // WHEN
        }
        public override void ExitSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context)
        {
            AttachEndIfExists(context.SearchStatementEnd());
        }


        public override void EnterAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context)
        {
            var terminal = context.AddStatement();
            var statement = terminal != null ? (AddStatement)terminal.Symbol : null;
            Enter(new Add(statement), context);
        }
        public override void ExitAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context)
        {
            AttachEndIfExists(context.AddStatementEnd());
        }
        public override void EnterComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context)
        {
            var terminal = context.ComputeStatement();
            var statement = terminal != null ? (ComputeStatement)terminal.Symbol : null;
            Enter(new Compute(statement), context);
        }
        public override void ExitComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context)
        {
            AttachEndIfExists(context.ComputeStatementEnd());
        }
        public override void EnterDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context)
        {
            var terminal = context.DivideStatement();
            var statement = terminal != null ? (DivideStatement)terminal.Symbol : null;
            Enter(new Divide(statement), context);
        }
        public override void ExitDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context)
        {
            AttachEndIfExists(context.DivideStatementEnd());
        }
        public override void EnterMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context)
        {
            var terminal = context.MultiplyStatement();
            var statement = terminal != null ? (MultiplyStatement)terminal.Symbol : null;
            Enter(new Multiply(statement), context);
        }
        public override void ExitMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context)
        {
            AttachEndIfExists(context.MultiplyStatementEnd());
        }
        public override void EnterSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context)
        {
            var terminal = context.SubtractStatement();
            var statement = terminal != null ? (SubtractStatement)terminal.Symbol : null;
            Enter(new Subtract(statement), context);
        }
        public override void ExitSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context)
        {
            AttachEndIfExists(context.SubtractStatementEnd());
        }
        public override void EnterDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context)
        {
            var terminal = context.DeleteStatement();
            var statement = terminal != null ? (DeleteStatement)terminal.Symbol : null;
            Enter(new Delete(statement), context);
        }
        public override void ExitDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context)
        {
            AttachEndIfExists(context.DeleteStatementEnd());
        }
        public override void EnterReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context)
        {
            var terminal = context.ReadStatement();
            var statement = terminal != null ? (ReadStatement)terminal.Symbol : null;
            Enter(new Read(statement), context);
        }
        public override void ExitReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context)
        {
            AttachEndIfExists(context.ReadStatementEnd());
        }
        public override void EnterWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context)
        {
            var terminal = context.WriteStatement();
            var statement = terminal != null ? (WriteStatement)terminal.Symbol : null;
            Enter(new Write(statement), context);
        }
        public override void ExitWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context)
        {
            AttachEndIfExists(context.WriteStatementEnd());
        }
        public override void EnterRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context)
        {
            var terminal = context.RewriteStatement();
            var statement = terminal != null ? (RewriteStatement)terminal.Symbol : null;
            Enter(new Rewrite(statement), context);
        }
        public override void ExitRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context)
        {
            AttachEndIfExists(context.RewriteStatementEnd());
        }
        public override void EnterStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context)
        {
            var terminal = context.StartStatement();
            var statement = terminal != null ? (StartStatement)terminal.Symbol : null;
            Enter(new Start(statement), context);
        }
        public override void ExitStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context)
        {
            AttachEndIfExists(context.StartStatementEnd());
        }
        public override void EnterReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context)
        {
            var terminal = context.ReturnStatement();
            var statement = terminal != null ? (ReturnStatement)terminal.Symbol : null;
            Enter(new Return(statement), context);
        }
        public override void ExitReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context)
        {
            AttachEndIfExists(context.ReturnStatementEnd());
        }
        public override void EnterStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context)
        {
            var terminal = context.StringStatement();
            var statement = terminal != null ? (StringStatement)terminal.Symbol : null;
            Enter(new Nodes.String(statement), context);
        }
        public override void ExitStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context)
        {
            AttachEndIfExists(context.StringStatementEnd());
        }
        public override void EnterUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context)
        {
            var terminal = context.UnstringStatement();
            var statement = terminal != null ? (UnstringStatement)terminal.Symbol : null;
            Enter(new Unstring(statement), context);
        }
        public override void ExitUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context)
        {
            AttachEndIfExists(context.UnstringStatementEnd());
        }
        public override void EnterCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context)
        {
            var terminal = context.CallStatement();
            var statement = terminal != null ? (CallStatement)terminal.Symbol : null;
            Enter(new Call(statement), context);
        }
        public override void ExitCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context)
        {
            AttachEndIfExists(context.CallStatementEnd());
        }
        public override void EnterProcedureStyleCallConditional(ProgramClassParser.ProcedureStyleCallConditionalContext context)
        {
            var terminal = context.ProcedureStyleCall();
            var statement = terminal != null ? (ProcedureStyleCallStatement)terminal.Symbol : null;
            Enter(new ProcedureStyleCall(statement), context);
        }
        public override void ExitProcedureStyleCallConditional(ProgramClassParser.ProcedureStyleCallConditionalContext context)
        {
            AttachEndIfExists(context.CallStatementEnd());
        }
        public override void EnterInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context)
        {
            var terminal = context.InvokeStatement();
            var statement = terminal != null ? (InvokeStatement)terminal.Symbol : null;
            Enter(new Invoke(statement), context);
        }
        public override void ExitInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context)
        {
            AttachEndIfExists(context.InvokeStatementEnd());
        }
        public override void EnterXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context)
        {
            var terminal = context.XmlGenerateStatement();
            var statement = terminal != null ? (XmlGenerateStatement)terminal.Symbol : null;
            Enter(new XmlGenerate(statement), context);
        }
        public override void ExitXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context)
        {
            AttachEndIfExists(context.XmlStatementEnd());
        }
        public override void EnterXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context)
        {
            var terminal = context.XmlParseStatement();
            var statement = terminal != null ? (XmlParseStatement)terminal.Symbol : null;
            Enter(new XmlParse(statement), context);
        }
        public override void ExitXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context)
        {
            AttachEndIfExists(context.XmlStatementEnd());
        }

        public override void EnterOnSizeError(ProgramClassParser.OnSizeErrorContext context)
        {
            var terminal = context.OnSizeErrorCondition();
            var condition = terminal != null ? (OnSizeErrorCondition)terminal.Symbol : null;
            Enter(new OnSizeError(condition), context);
        }
        public override void ExitOnSizeError(ProgramClassParser.OnSizeErrorContext context)
        {
            Exit();
        }
        public override void EnterNoSizeError(ProgramClassParser.NoSizeErrorContext context)
        {
            var terminal = context.NotOnSizeErrorCondition();
            var condition = terminal != null ? (NotOnSizeErrorCondition)terminal.Symbol : null;
            Enter(new NoSizeError(condition), context);
        }
        public override void ExitNoSizeError(ProgramClassParser.NoSizeErrorContext context)
        {
            Exit();
        }
        public override void EnterOnAtEnd(ProgramClassParser.OnAtEndContext context)
        {
            var terminal = context.AtEndCondition();
            var condition = terminal != null ? (AtEndCondition)terminal.Symbol : null;
            Enter(new OnAtEnd(condition), context);
        }
        public override void ExitOnAtEnd(ProgramClassParser.OnAtEndContext context)
        {
            Exit();
        }
        public override void EnterNoAtEnd(ProgramClassParser.NoAtEndContext context)
        {
            var terminal = context.NotAtEndCondition();
            var condition = terminal != null ? (NotAtEndCondition)terminal.Symbol : null;
            Enter(new NoAtEnd(condition), context);
        }
        public override void ExitNoAtEnd(ProgramClassParser.NoAtEndContext context)
        {
            Exit();
        }
        public override void EnterOnException(ProgramClassParser.OnExceptionContext context)
        {
            var terminal = context.OnExceptionCondition();
            var condition = terminal != null ? (OnExceptionCondition)terminal.Symbol : null;
            Enter(new OnException(condition), context);
        }
        public override void ExitOnException(ProgramClassParser.OnExceptionContext context)
        {
            Exit();
        }
        public override void EnterNoException(ProgramClassParser.NoExceptionContext context)
        {
            var terminal = context.NotOnExceptionCondition();
            var condition = terminal != null ? (NotOnExceptionCondition)terminal.Symbol : null;
            Enter(new NoException(condition), context);
        }
        public override void ExitNoException(ProgramClassParser.NoExceptionContext context)
        {
            Exit();
        }
        public override void EnterOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context)
        {
            var terminal = context.InvalidKeyCondition();
            var condition = terminal != null ? (InvalidKeyCondition)terminal.Symbol : null;
            Enter(new OnInvalidKey(condition), context);
        }
        public override void ExitOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context)
        {
            Exit();
        }
        public override void EnterNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context)
        {
            var terminal = context.NotInvalidKeyCondition();
            var condition = terminal != null ? (NotInvalidKeyCondition)terminal.Symbol : null;
            Enter(new NoInvalidKey(condition), context);
        }
        public override void ExitNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context)
        {
            Exit();
        }
        public override void EnterOnOverflow(ProgramClassParser.OnOverflowContext context)
        {
            var terminal = context.OnOverflowCondition();
            var condition = terminal != null ? (OnOverflowCondition)terminal.Symbol : null;
            Enter(new OnOverflow(condition), context);
        }
        public override void ExitOnOverflow(ProgramClassParser.OnOverflowContext context)
        {
            Exit();
        }
        public override void EnterNoOverflow(ProgramClassParser.NoOverflowContext context)
        {
            var terminal = context.NotOnOverflowCondition();
            var condition = terminal != null ? (NotOnOverflowCondition)terminal.Symbol : null;
            Enter(new NoOverflow(condition), context);
        }
        public override void ExitNoOverflow(ProgramClassParser.NoOverflowContext context)
        {
            Exit();
        }





        private void AttachEndIfExists(Antlr4.Runtime.Tree.ITerminalNode terminal)
        {
            var end = terminal != null ? terminal.Symbol as CodeElementEnd : null;
            if (end == null) return;
            Enter(new End(end));
            Exit();
        }
    }
}
