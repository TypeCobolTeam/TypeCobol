using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Tools;
using Antlr4.Runtime;

namespace TypeCobol.Compiler.Parser
{
	/// <summary>
	/// Build a Program or Class object while visiting its parse tree
	/// </summary>
	public class CobolNodeBuilder: ProgramClassBaseListener {
		/// <summary>
		/// Program object resulting of the visit the parse tree
		/// </summary>
		public Program Program { get; private set; }

		// Programs can be nested => track current programs being analyzed
		private Stack<Program> programsStack = null;

		private Program CurrentProgram {
			get { return programsStack.Peek(); }
			set { programsStack.Push(value); }
		}

		/// <summary>Class object resulting of the visit the parse tree</summary>
		public Class Class { get; private set; }

		private SymbolTable TableOfIntrisic = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
		private SymbolTable TableOfGlobals;

		public SymbolTable CustomSymbols {
			private get { throw new System.InvalidOperationException(); }
			set {
				if (value != null) {
					foreach(var values in value.DataEntries.Values)
						foreach(var data in values)
							TableOfIntrisic.Add(data);
					foreach(var type in value.CustomTypes)
						TableOfIntrisic.RegisterCustomType(type);
				}
//				RegisterCustomType(TableOfIntrisic, DataType.Date);
//				RegisterCustomType(TableOfIntrisic, DataType.Boolean);
			}
		}
/*
		private void RegisterCustomType(SymbolTable table, DataType type) {
			try { table.GetCustomType(type.Name); }
			catch(ArgumentException ex) { table.RegisterCustomType(new CustomTypeDefinition(type)); }
		}
		private void RegisterCustomType(SymbolTable table, TypeDefinition type) {
			try { table.GetCustomType(type.DataType.Name); }
			catch(ArgumentException ex) { table.RegisterCustomType(type); }
		}
*/


		public NodeDispatcher Dispatcher { get; internal set; }

		private void Enter(Node node, ParserRuleContext context = null, SymbolTable table = null) {
			node.SymbolTable = table ?? CurrentProgram.CurrentTable;
			Program.SyntaxTree.Enter(node, context);
		}
		private void Exit() {
			var node = Program.SyntaxTree.CurrentNode;
			var context = Program.SyntaxTree.CurrentContext;
			Dispatcher.OnNode(node, context, CurrentProgram);
			Program.SyntaxTree.Exit();
		}
		private void Delete() {
			Program.SyntaxTree.Delete();
		}
		private Node CurrentNode { get { return Program.SyntaxTree.CurrentNode; } }

		private void AttachIfExists(Antlr4.Runtime.Tree.ITerminalNode node) {
			var ce = AsCodeElement(node);
			if (ce == null) return;
			Enter(new Node(ce));
			Exit();
		}

		/// <summary>
		/// Initialization code run before parsing each new Program or Class
		/// </summary>
		public override void EnterCobolCompilationUnit(ProgramClassParser.CobolCompilationUnitContext context)
		{
			TableOfGlobals = new SymbolTable(TableOfIntrisic, SymbolTable.Scope.Global);
			Program = null;
			Class = null;
		}

		public override void EnterCobolProgram(ProgramClassParser.CobolProgramContext context) {
			if (Program == null) {
				Program = new SourceProgram(TableOfGlobals);
				programsStack = new Stack<Program>();
				CurrentProgram = Program;
			} else {
				var enclosing = CurrentProgram;
				CurrentProgram = new NestedProgram(enclosing);
				Enter(CurrentProgram.SyntaxTree.Root, context);
			}
			CurrentProgram.Identification = (ProgramIdentification)AsCodeElement(context.ProgramIdentification());
			Enter(new Node(CurrentProgram.Identification), context, CurrentProgram.SymbolTable);
		}

		public override void ExitCobolProgram(ProgramClassParser.CobolProgramContext context) {
			AttachIfExists(context.ProgramEnd());
			Exit();
			programsStack.Pop();
		}

		public override void EnterEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
			Enter(new Node(AsCodeElement(context.EnvironmentDivisionHeader())), context);
		}
		public override void ExitEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
			Exit();
		}

		public override void EnterConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			Enter(new Node(AsCodeElement(context.ConfigurationSectionHeader())), context);
			var paragraphs = new List<CodeElement>();
			foreach(var paragraph in context.configurationParagraph()) {
				if (paragraph.SourceComputerParagraph() != null)
					paragraphs.Add(AsCodeElement(paragraph.SourceComputerParagraph()));
				if (paragraph.ObjectComputerParagraph() != null)
					paragraphs.Add(AsCodeElement(paragraph.ObjectComputerParagraph()));
				if (paragraph.SpecialNamesParagraph() != null)
					paragraphs.Add(AsCodeElement(paragraph.SpecialNamesParagraph()));
				if (paragraph.RepositoryParagraph() != null)
					paragraphs.Add(AsCodeElement(paragraph.RepositoryParagraph()));
			}
			foreach(var p in paragraphs) {
				Enter(new Node(p));
				Exit();
			}
		}
		public override void ExitConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			Exit();
		}

		public override void EnterDataDivision(ProgramClassParser.DataDivisionContext context) {
			Enter(new Node(AsCodeElement(context.DataDivisionHeader())), context);
		}
		public override void ExitDataDivision(ProgramClassParser.DataDivisionContext context) {
			Exit();
		}

		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">FILE SECTION</param>
		public override void EnterFileSection(ProgramClassParser.FileSectionContext context) {
			Enter(new Node(AsCodeElement(context.FileSectionHeader())), context);
			//TODO: FILE & DATA DESCRIPTION ENTRIES
		}
		public override void ExitFileSection(ProgramClassParser.FileSectionContext context) {
			Exit();
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">WORKING-STORAGE SECTION</param>
		public override void EnterWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context) {
			Enter(new WorkingStorageSection(AsCodeElement<WorkingStorageSectionHeader>(context.WorkingStorageSectionHeader())), context);
		}

		public override void ExitWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context) {
			while(Reflection.IsTypeOf(CurrentNode.GetType(), typeof(DataSection)))
				Exit(); // Exit last level-01 data definition entry, as long as its subordinates
			Exit(); // Exit WorkingStorageSection
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">LOCAL-STORAGE SECTION</param>
		public override void EnterLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context) {
			Enter(new LocalStorageSection(AsCodeElement<LocalStorageSectionHeader>(context.LocalStorageSectionHeader())), context);
		}
		public override void ExitLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context) {
			while(Reflection.IsTypeOf(CurrentNode.GetType(), typeof(DataSection)))
				Exit(); // Exit last level-01 data definition entry, as long as its subordinates
			Exit(); // Exit LocalStorageSection
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">LINKAGE SECTION</param>
		public override void EnterLinkageSection(ProgramClassParser.LinkageSectionContext context) {
			Enter(new LinkageSection(AsCodeElement<LinkageSectionHeader>(context.LinkageSectionHeader())), context);
		}
		public override void ExitLinkageSection(ProgramClassParser.LinkageSectionContext context) {
			while(Reflection.IsTypeOf(CurrentNode.GetType(), typeof(DataSection)))
				Exit(); // Exit last level-01 data definition entry, as long as its subordinates
			Exit(); // Exit LinkageSection
		}

		public override void EnterDataDefinitionEntry(ProgramClassParser.DataDefinitionEntryContext context) {
			if (context.DataDescriptionEntry() != null) {
				var data = (DataDescriptionEntry)AsCodeElement(context.DataDescriptionEntry());
				if (data is TypeDefinitionEntry) EnterTypeDefinitionEntry((TypeDefinitionEntry)data);
				else EnterDataDescriptionEntry(data);
			}
			if (context.DataConditionEntry() != null)
				EnterDataConditionEntry((DataConditionEntry)AsCodeElement(context.DataConditionEntry()));
			if (context.DataRedefinesEntry() != null)
				EnterDataRedefinesEntry((DataRedefinesEntry)AsCodeElement(context.DataRedefinesEntry()));
			if (context.DataRenamesEntry() != null)
				EnterDataRenamesEntry((DataRenamesEntry)AsCodeElement(context.DataRenamesEntry()));
		}
		public override void ExitDataDefinitionEntry(ProgramClassParser.DataDefinitionEntryContext context) {
			// DO NOTHING: current node will be Exit()ed either:
			// - by next DataDefinition if current node is not its top-level item
			// - by enclosing DataSection if it is part of the last level-01 item in it
		}

		private void EnterTypeDefinitionEntry(TypeDefinitionEntry typedef) {
			SetCurrentNodeToTopLevelItem(typedef.LevelNumber.Value);
			Enter(new Node(typedef));
		}

		private void EnterDataDescriptionEntry(DataDescriptionEntry data) {
			SetCurrentNodeToTopLevelItem(data.LevelNumber.Value);
			Enter(new Node(data));
		}

		private void EnterDataConditionEntry(DataConditionEntry data) {
			throw new NotImplementedException("TODO#249");
		}

		private void EnterDataRedefinesEntry(DataRedefinesEntry data) {
			throw new NotImplementedException("TODO#249");
		}

		private void EnterDataRenamesEntry(DataRenamesEntry data) {
			throw new NotImplementedException("TODO#249");
		}

		/// <summary>Exit() every Node that is not the top-level item for a data of a given level.</summary>
		/// <param name="level">Level number of the next data definition that will be Enter()ed.</param>
		private void SetCurrentNodeToTopLevelItem(long level) {
			Node parent = GetTopLevelItem(level);
			if (parent != null) while (parent != CurrentNode) Exit();
		}

		private Node GetTopLevelItem(long level) {
			var parent = CurrentNode;
			while(parent != null) {
				var data = parent.CodeElement as DataDefinitionEntry;
				if (data == null) return null;
				if (data.LevelNumber.Value < level) return parent;
				parent = parent.Parent;
			}
			return null;
		}


/*
		private void AddEntries(IEnumerable<DataDescriptionEntry> entries) {
			foreach(var entry in entries) {
				var child = new Node(entry);
				Enter(child);
				AddEntries(entry.Subordinates);
				Exit();
			}
		}

		/// <summary>Update toplevel/subordinate relations of data description entries.</summary>
		/// <param name="nodes">DataDescriptionEntry[] array -typically <section context>.DataDescriptionEntry()</param>
		/// <returns>nodes parameter, but with each element having its TopLevel and Subordinates properties initialized</returns>
		private IList<DataDescriptionEntry> CreateDataDescriptionEntries(Antlr4.Runtime.Tree.ITerminalNode[] nodes) {
			IList<DataDescriptionEntry> result = new List<DataDescriptionEntry>();
			if (nodes == null) return result;
			char[] currencies = GetCurrencies();
			Stack<DataDescriptionEntry> groups = new Stack<DataDescriptionEntry>();

			foreach (var node in nodes) {
				DataDescriptionEntry data = node.Symbol as DataDescriptionEntry;

				if (data.IsTypeDefinition) CurrentProgram.CurrentTable.RegisterCustomType(data);
				bool hasParent = ComputeParent(data, groups);
				if (!hasParent) result.Add(data);
				var customtype = ComputeType(data, currencies);


				//TODO move these rules in a new TypeCobolTypeDefChecker (or Cobol2002TypeDefChecker)
				if (customtype != null && !customtype.DataType.IsNestable && hasParent) {
					DiagnosticUtils.AddError(data, "Type "+customtype.DataType.Name+" should not be subordinate to another item");
					var parent = data.TopLevel;
					while(parent != null) {
						DiagnosticUtils.AddError(parent, "Group items should not contain non nestable type "+customtype.DataType.Name+" items");
						parent = parent.TopLevel;
					}
				}


				//Rules that apply to item under a TYPEDEF, the TYPEDEF item itself is check by others rules
				//TODO move these rules in a new TypeCobolTypeDefChecker (or Cobol2002TypeDefChecker)
				var typeDefinition = data.GetTypeDefinition();
				if (typeDefinition != null && !data.IsTypeDefinition)
				{
					//Redefines is not allowed under a TYPEDEF
					if (data.RedefinesDataName != null)
					{
						DiagnosticUtils.AddError(data, "Typedef can't contains redefined item: " + data);
					}
					if (data.IsRenamesDataNameDescription)
					{
						DiagnosticUtils.AddError(data, "Typedef can't contains renamed item: " + data);
					}
					//If
					if (typeDefinition.DataType.IsStrong && data.InitialValue != null)
					{
						DiagnosticUtils.AddError(data, "Item under a Strong Typedef can't contains value clause: " + data);
					}
				}

				//TODO move these rules in a new CobolTxxxxChecker and TypeCobolTypeDefChecker (or Cobol2002TypeDefChecker)
				if (data.RedefinesDataName != null)
				{
					var redfinedItems = CurrentProgram.CurrentTable.Get(data.RedefinesDataName.Name);
					if (redfinedItems.Count == 0)
					{
						DiagnosticUtils.AddError(data, data + " redefines a variable not referenced " + data.RedefinesDataName);
					}
					else if (redfinedItems.Count > 1)
					{
						DiagnosticUtils.AddError(data, data + " redefines an ambiguous variable  " + data.RedefinesDataName);
					}
					else
					{
						if (redfinedItems[0].IsTypeDefinitionPart)
						{
							DiagnosticUtils.AddError(data, data + " can't redefines a TYPEDEF " + data.RedefinesDataName);
						}
						if (redfinedItems[0].GetFirstStrongDataDescriptionEntry() != null)
						{
							DiagnosticUtils.AddError(data, data + " can't redefines a STRONG TYPE " + data.RedefinesDataName);
						}
					}
				}
				//TODO move these rules in a new CobolTxxxxChecker and TypeCobolTypeDefChecker (or Cobol2002TypeDefChecker)
				if (data.RenamesFromDataName != null)
				{
					CheckRenameClause(data, data.RenamesFromDataName);
				}
				//TODO move these rules in a new CobolTxxxxChecker and TypeCobolTypeDefChecker (or Cobol2002TypeDefChecker)
				if (data.RenamesToDataName != null)
				{
					CheckRenameClause(data, data.RenamesToDataName);
				}

				CurrentProgram.CurrentTable.Add(data);
				if (customtype != null) {
					foreach(var sub in customtype.Subordinates) {
						// add a clone so parent/child relations are not spoiled
						var clone = sub.Clone() as DataDescriptionEntry;
						data.Subordinates.Add(clone);
						clone.TopLevel = data;
						UpdateLevelNumbers(clone, data.LevelNumber);

						CurrentProgram.CurrentTable.Add(clone);
						AddGeneratedSymbols(clone);
					}
				}
			}
			foreach(var data in result) {
				int offset = 0;
				ComputeMemoryProfile(data, ref offset);
				if (data.IsTypeDefinition && data.Subordinates.Count < 1 && data.Picture == null)
					DiagnosticUtils.AddError(data, data.Name+" has no description.");
			}
			return result;
		}

		private void CheckRenameClause(DataDescriptionEntry data, TypeCobol.Compiler.CodeElements.Expressions.QualifiedName renamesFrom)
		{
			var renames = CurrentProgram.CurrentTable.Get(renamesFrom);
			if (renames.Count == 0)
			{
				DiagnosticUtils.AddError(data, data + " rename a variable not referenced " + renamesFrom);
			}
			else if (renames.Count > 1)
			{
				DiagnosticUtils.AddError(data, data + " rename an ambiguous variable  " + renamesFrom);
			}
			else
			{

				if (renames[0].IsTypeDefinitionPart)
				{
					DiagnosticUtils.AddError(data, data + " can't renames a TYPEDEF " + renamesFrom);
				}
				if (renames[0].GetFirstStrongDataDescriptionEntry() != null)
				{
					DiagnosticUtils.AddError(data, data + " can't renames a strongly typed variable" + renamesFrom);
				}

				
			}
		}

		private void ComputeMemoryProfile(DataDescriptionEntry data, ref int offset) {
			if (data.Subordinates.Count < 1) {
				int length = picture2Size(data.Picture) * type2Size(data.DataType);
				data.MemoryArea = CreateMemoryArea(data, offset, length);
				offset += data.MemoryArea.Length; // offset for next sibling or for toplevel's next sibling
			} else {
				int length = 0;
				int os = -1;
				foreach(var child in data.Subordinates) {
					COBOLMemoryArea rmem = null;
					if (child.RedefinesDataName != null) {
						rmem = GetRedefinedMemory(child.RedefinesDataName.Name);
						// rmem can be null if we try to redefine something in a TYPEDEF
						if (rmem != null) offset = rmem.Offset;
					}
					ComputeMemoryProfile(child, ref offset);
					if (os < 0) os = child.MemoryArea.Offset;// parent offset = first child offset
					if (rmem != null) {
						if (child.MemoryArea.Length > rmem.Length) {
System.Console.WriteLine("TODO: "+child.Name+'('+child.MemoryArea.Length+") REDEFINES smaller area "+child.RedefinesDataName.Name+'('+rmem.Length+')');
							length += child.MemoryArea.Length - rmem.Length;//warn: redefines smaller area
						} else {
							offset += rmem.Length - child.MemoryArea.Length;//catch with redefined offset
						}
					} else length += child.MemoryArea.Length;
				}
				data.MemoryArea = CreateMemoryArea(data, os, length);
			}
		}
		private static int picture2Size(string picture) {
			if (picture == null) return 1;
			var betweenparentheses = picture.Split("()".ToCharArray());
			if (betweenparentheses.Length > 1)
			{
				//caught a FormatException during unit tests
				//TODO check what to do in this case
				try
				{
					return int.Parse(betweenparentheses[1]);
				}
				catch (FormatException)
				{
					return 1;
				}
			}
			return 1;
		}
		private static int type2Size(DataType type) {
			return 1; //TODO
		}
		private COBOLMemoryArea GetRedefinedMemory(string redefined) {
			var matches = Program.SymbolTable.Get(redefined);
			if (matches.Count != 1) {
System.Console.WriteLine("TODO: name resolution errors in REDEFINES clause");
				return null;
			}
			return matches[0].MemoryArea;
		}
		private COBOLMemoryArea CreateMemoryArea(DataDescriptionEntry data, int offset, int length) {
			if (data.IsTableOccurence) return new TableInMemory(length, offset, data.Occurences);
			else return new DataInMemory(length, offset);
		}



		private void AddGeneratedSymbols(DataDescriptionEntry data) {
			if (data.DataType == null) return;
			var custom = GetCustomType(data.DataType.Name);
			if (custom == null) return;
			foreach(var sub in new List<DataDescriptionEntry>(custom.Subordinates)) {
				var clone = sub.Clone() as DataDescriptionEntry;
				data.Subordinates.Add(clone);
				clone.TopLevel = data;
				UpdateLevelNumbers(clone, data.LevelNumber);
				CurrentProgram.CurrentTable.Add(clone);
				AddGeneratedSymbols(clone);
			}
		}
		private void UpdateLevelNumbers(DataDescriptionEntry clone, int level) {
			clone.LevelNumber = level+1;
			if (clone.LevelNumber == 66
			 || clone.LevelNumber == 77
			 || clone.LevelNumber == 88)
				clone.LevelNumber++;
			foreach(var sub in clone.Subordinates)
				UpdateLevelNumbers(sub, clone.LevelNumber);
		}

		/// <summary>Update the toplevel data of a given data description.</summary>
		/// <param name="data">Data description to update</param>
		/// <param name="groups">Current "branch" of parent data. If its size is greater than 0, data is a subordinate.</param>
		/// <returns>True if the parental relation has been updated</returns>
		private bool ComputeParent(DataDescriptionEntry data, Stack<DataDescriptionEntry> groups) {
			bool updated = false;
			while(!updated && groups.Count > 0) {
				var toplevel = groups.Peek();
				if (data.LevelNumber <= toplevel.LevelNumber || data.LevelNumber == 66) groups.Pop();
				else {
					toplevel.Subordinates.Add(data);
					data.TopLevel = toplevel;
					updated = true;
				}
			}
			if (updated || data.IsGroup) groups.Push(data);
			return updated;
		}

		/// <summary>Update the toplevel data of a given data description.</summary>
		/// <param name="data">Data description to update</param>
		/// <param name="currencies">Currency characters, used to know if data is numeric or numeric edited</param>
		/// <returns>Representation of the corresponding TYPEDEF if data is of a custom TYPE, or null if data type is unknown of from COBOL standard.</returns>
		private TypeDefinition ComputeType(DataDescriptionEntry data, char[] currencies) {
			if (data.DataType != null) return null;
			if (data.Picture == null) {
				data.DataType = DataType.Unknown;
				return null;
			}
			if (data.Picture.StartsWith("TYPE:")) {
				string typename = data.Picture.Substring(5);
				try {
					var customTypeGroup = CurrentProgram.CurrentTable.GetCustomType(typename);
					data.DataType = customTypeGroup.DataType;
					return customTypeGroup;
				} catch(ArgumentException ex) {
					data.DataType = DataType.Unknown;
					DiagnosticUtils.AddError(data, "Type "+typename+" is not referenced.");
					return null;
				}
			} else {
				data.DataType = DataType.Create(data.Picture, currencies);
				return null;
			}
		}
		private TypeDefinition GetCustomType(string name) {
			try { return CurrentProgram.CurrentTable.GetCustomType(name); }
			catch(ArgumentException ex) { return null; }
		}

		/// <summary>Retrieve currency characters from SPECIAL NAMES paragraph.</summary>
		/// <returns>Currency characters array</returns>
		private char[] GetCurrencies() {
			IDictionary<string, string> currencies = null;
			var specialnode = GetNode(typeof(SpecialNamesParagraph));
			if (specialnode != null) currencies = (specialnode.CodeElement as SpecialNamesParagraph).CurrencySymbols;
			if (currencies == null || currencies.Count < 1) return new char[] { '$' };
			var chars = new List<char>();
			foreach(var key in currencies.Keys)
				if (key.Length == 1) chars.Add(key[0]);
			return chars.ToArray();
		}

		/// <summary>Get first node holding data of a given type.</summary>
		/// <param name="type">Type of data we want</param>
		/// <returns>First node encountered during a breadth-first traversal of the tree.</returns>
		private Node GetNode(Type type) {
			var nodes = new List<Node>();
			nodes.Add(CurrentProgram.SyntaxTree.Root);
			while(nodes.Count > 0) {
				var node = nodes[0];
				if (node.CodeElement != null && node.CodeElement.GetType() == type) return node;
				nodes.Remove(node);
				nodes.AddRange(node.Children); //breadth-first
			}
			return null;
		}
*/



		public override void EnterProcedureDivision(ProgramClassParser.ProcedureDivisionContext context) {
			Enter(new Node(AsCodeElement(context.ProcedureDivisionHeader())), context);
		}
		public override void ExitProcedureDivision(ProgramClassParser.ProcedureDivisionContext context) {
			Exit();
		}



		/// <summary>Parent node: PROCEDURE DIVISION</summary>
		/// <param name="context">DECLARE FUNCTION</param>
		public override void EnterFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context) {
			var header = (FunctionDeclarationHeader)AsCodeElement(context.FunctionDeclarationHeader());
			header.SetLibrary(CurrentProgram.Identification.ProgramName.Name);
			Enter(new Node(header), context, new SymbolTable(CurrentProgram.CurrentTable, SymbolTable.Scope.Function));
		}
		public override void ExitFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context) {
			Enter(new Node(AsCodeElement(context.FunctionDeclarationEnd())), context);
			Exit();
			Exit();// exit DECLARE FUNCTION
		}
		/// <summary>Parent node: DECLARE FUNCTION</summary>
		/// <param name="context">PROCEDURE DIVISION</param>
		public override void EnterFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context) {
			CodeElement profile = AsCodeElement(context.ProcedureDivisionHeader());
			if (profile is ProcedureDivisionHeader) {
				// there are neither INPUT nor OUTPUT defined,
				// and CodeElementBuilder can't guess we were inside a function declaration,
				// so it created a basic ProcedureDivisionHeader, but we need a FunctionDeclarationProfile
				profile = new FunctionDeclarationProfile(profile as ProcedureDivisionHeader);
			}
			Enter(new Node(profile), context);
		}
		public override void ExitFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context) {
			Exit();
		}



		public override void EnterSection(ProgramClassParser.SectionContext context) {
			var terminal = context.SectionHeader();
			if (terminal == null) terminal = context.ParagraphHeader();
			// if we Enter(..) a node here, it will be detached by ExitParagraph
			// if we do not, no need to detach anything in ExitSection
			if (terminal != null) Enter(new Node(AsCodeElement(terminal)), context);
		}

		public override void EnterParagraph(ProgramClassParser.ParagraphContext context) {
			if (Program.SyntaxTree.CurrentNode.CodeElement is ParagraphHeader) Exit();
			Enter(new Node(AsCodeElement(context.ParagraphHeader())), context);
		}
		public override void ExitParagraph(ProgramClassParser.ParagraphContext context) {
			Exit();
		}

		public override void EnterSentence(ProgramClassParser.SentenceContext context) {
			Enter(new Node(null), context);
		}
		public override void ExitSentence(ProgramClassParser.SentenceContext context) {
			AttachIfExists(context.SentenceEnd());
			Exit();
		}

		public override void EnterStatement(ProgramClassParser.StatementContext context) {
			CodeElement statement = AsStatement(context);
//TODO#249			FixSubscriptableQualifiedNames(statement);
			Enter(new Node(statement), context);
		}
		public override void ExitStatement(ProgramClassParser.StatementContext context) {
			Exit();
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


		public override void EnterIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.IfStatement())), context);
			Enter(new Node(null), context);//THEN
		}
		public override void EnterElseClause(ProgramClassParser.ElseClauseContext context) {
			Exit();// we want ELSE to be child of IF, not THEN, so exit THEN
			Enter(new Node(AsCodeElement(context.ElseCondition())), context);// ELSE
			AttachIfExists(context.NextSentenceStatement());
		}
		public override void ExitIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
			Exit(); // Exit ELSE (if any) or THEN
			AttachIfExists(context.IfStatementEnd());
			// DO NOT Exit() IF node because this will be done in ExitStatement
		}


		public override void EnterEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.EvaluateStatement())), context);// enter EVALUATE
		}
		public override void EnterWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			Enter(new Node(null), context);// enter WHEN group
			foreach(var condition in context.WhenCondition()) {
				Enter(new Node(AsCodeElement(condition)), context);
				Exit();
			}
			Exit();// exit WHEN group
			Enter(new Node(null), context);// enter THEN
		}
		public override void ExitWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			Exit();// exit THEN
		}
		public override void EnterWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			Enter(new Node(AsCodeElement(context.WhenOtherCondition())), context);// enter WHEN OTHER
		}
		public override void ExitWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			Exit();// exit WHEN OTHER
		}
		public override void ExitEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			AttachIfExists(context.EvaluateStatementEnd());// exit EVALUATE
		}


		public override void EnterPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.PerformStatement())), context);
		}
		public override void ExitPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
			AttachIfExists(context.PerformStatementEnd());
		}

		public override void EnterSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.SearchStatement())), context);
		}
		public override void EnterWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
			Enter(new Node(AsCodeElement(context.WhenSearchCondition())), context);
			AttachIfExists(context.NextSentenceStatement());
		}
		public override void ExitWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
			Exit(); // WHEN
		}
		public override void ExitSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
			AttachIfExists(context.SearchStatementEnd());
		}


		public override void EnterAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.AddStatement())), context);
		}
		public override void ExitAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
			AttachIfExists(context.AddStatementEnd());
		}
		public override void EnterComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.ComputeStatement())), context);
		}
		public override void ExitComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
			AttachIfExists(context.ComputeStatementEnd());
		}
		public override void EnterDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.DivideStatement())), context);
		}
		public override void ExitDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
			AttachIfExists(context.DivideStatementEnd());
		}
		public override void EnterMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.MultiplyStatement())), context);
		}
		public override void ExitMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
			AttachIfExists(context.MultiplyStatementEnd());
		}
		public override void EnterSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.SubtractStatement())), context);
		}
		public override void ExitSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
			AttachIfExists(context.SubtractStatementEnd());
		}
		public override void EnterDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.DeleteStatement())), context);
		}
		public override void ExitDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
			AttachIfExists(context.DeleteStatementEnd());
		}
		public override void EnterReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.ReadStatement())), context);
		}
		public override void ExitReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
			AttachIfExists(context.ReadStatementEnd());
		}
		public override void EnterWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.WriteStatement())), context);
		}
		public override void ExitWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
			AttachIfExists(context.WriteStatementEnd());
		}
		public override void EnterRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.RewriteStatement())), context);
		}
		public override void ExitRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
			AttachIfExists(context.RewriteStatementEnd());
		}
		public override void EnterStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.StartStatement())), context);
		}
		public override void ExitStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
			AttachIfExists(context.StartStatementEnd());
		}
		public override void EnterReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.ReturnStatement())), context);
		}
		public override void ExitReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
			AttachIfExists(context.ReturnStatementEnd());
		}
		public override void EnterStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.StringStatement())), context);
		}
		public override void ExitStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
			AttachIfExists(context.StringStatementEnd());
		}
		public override void EnterUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.UnstringStatement())), context);
		}
		public override void ExitUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
			AttachIfExists(context.UnstringStatementEnd());
		}
		public override void EnterCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.CallStatement())), context);
		}
		public override void ExitCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
			AttachIfExists(context.CallStatementEnd());
		}
		public override void EnterInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.InvokeStatement())), context);
		}
		public override void ExitInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
			AttachIfExists(context.InvokeStatementEnd());
		}
		public override void EnterXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.XmlGenerateStatement())), context);
		}
		public override void ExitXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
			AttachIfExists(context.XmlStatementEnd());
		}
		public override void EnterXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			Enter(new Node(AsCodeElement(context.XmlParseStatement())), context);
		}
		public override void ExitXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
			AttachIfExists(context.XmlStatementEnd());
		}

		public override void EnterOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
			Enter(new Node(AsCodeElement(context.OnSizeErrorCondition())), context);
		}
		public override void ExitOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
			Exit();
		}
		public override void EnterNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
			Enter(new Node(AsCodeElement(context.NotOnSizeErrorCondition())), context);
		}
		public override void ExitNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
			Exit();
		}
		public override void EnterOnAtEnd(ProgramClassParser.OnAtEndContext context) {
			Enter(new Node(AsCodeElement(context.AtEndCondition())), context);
		}
		public override void ExitOnAtEnd(ProgramClassParser.OnAtEndContext context) {
			Exit();
		}
		public override void EnterNoAtEnd(ProgramClassParser.NoAtEndContext context) {
			Enter(new Node(AsCodeElement(context.NotAtEndCondition())), context);
		}
		public override void ExitNoAtEnd(ProgramClassParser.NoAtEndContext context) {
			Exit();
		}
		public override void EnterOnException(ProgramClassParser.OnExceptionContext context) {
			Enter(new Node(AsCodeElement(context.OnExceptionCondition())), context);
		}
		public override void ExitOnException(ProgramClassParser.OnExceptionContext context) {
			Exit();
		}
		public override void EnterNoException(ProgramClassParser.NoExceptionContext context) {
			Enter(new Node(AsCodeElement(context.NotOnExceptionCondition())), context);
		}
		public override void ExitNoException(ProgramClassParser.NoExceptionContext context) {
			Exit();
		}
		public override void EnterOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
			Enter(new Node(AsCodeElement(context.InvalidKeyCondition())), context);
		}
		public override void ExitOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
			Exit();
		}
		public override void EnterNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
			Enter(new Node(AsCodeElement(context.NotInvalidKeyCondition())), context);
		}
		public override void ExitNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
			Exit();
		}
		public override void EnterOnOverflow(ProgramClassParser.OnOverflowContext context) {
			Enter(new Node(AsCodeElement(context.OnOverflowCondition())), context);
		}
		public override void ExitOnOverflow(ProgramClassParser.OnOverflowContext context) {
			Exit();
		}
		public override void EnterNoOverflow(ProgramClassParser.NoOverflowContext context) {
			Enter(new Node(AsCodeElement(context.NotOnOverflowCondition())), context);
		}
		public override void ExitNoOverflow(ProgramClassParser.NoOverflowContext context) {
			Exit();
		}





		private CodeElement AsCodeElement(Antlr4.Runtime.Tree.ITerminalNode node) {
			return node != null? (CodeElement)node.Symbol : null;
		}
		private T AsCodeElement<T>(Antlr4.Runtime.Tree.ITerminalNode node) where T: CodeElement {
			return node != null? (T)node.Symbol : null;
		}

		private CodeElement AsStatement(ProgramClassParser.StatementContext context)
		{
			return
				(CodeElement)AsCodeElement(context.ContinueStatement()) ??
				/* TODO
					| evaluateStatementExplicitScope
					| ifStatementExplicitScope
					| searchStatementExplicitScope
				 */
				// -- arithmetic --
				(CodeElement)AsCodeElement(context.AddStatement()) ??
				(CodeElement)AsCodeElement(context.ComputeStatement()) ??
				(CodeElement)AsCodeElement(context.DivideStatement()) ??
				(CodeElement)AsCodeElement(context.MultiplyStatement()) ??
				(CodeElement)AsCodeElement(context.SubtractStatement()) ??
				/* TODO
					| addStatementExplicitScope
					| computeStatementExplicitScope
					| divideStatementExplicitScope
					| multiplyStatementExplicitScope
					| subtractStatementExplicitScope
				 */

				// -- data movement --
				(CodeElement)AsCodeElement(context.AcceptStatement()) ?? // (DATE, DAY, DAY-OF-WEEK, TIME)
				(CodeElement)AsCodeElement(context.InitializeStatement()) ??
				(CodeElement)AsCodeElement(context.InspectStatement()) ??
				(CodeElement)AsCodeElement(context.MoveStatement()) ??
				(CodeElement)AsCodeElement(context.SetStatement()) ?? // "table-handling" too
				(CodeElement)AsCodeElement(context.StringStatement()) ??
				(CodeElement)AsCodeElement(context.UnstringStatement()) ??
				(CodeElement)AsCodeElement(context.XmlGenerateStatement()) ??
				(CodeElement)AsCodeElement(context.XmlParseStatement()) ??
				/* TODO
					| stringStatementExplicitScope
					| unstringStatementExplicitScope
					| xmlGenerateStatementExplicitScope
					| xmlParseStatementExplicitScope
				 */
				// -- ending --
				(CodeElement)AsCodeElement(context.StopStatement()) ?? // RUN
				(CodeElement)AsCodeElement(context.ExitMethodStatement()) ??
				(CodeElement)AsCodeElement(context.ExitProgramStatement()) ??
				(CodeElement)AsCodeElement(context.GobackStatement()) ??
				// -- input-output --
				//              (CodeElement)AsCodeElement(context.AcceptStatement()) ?? // identifier
				(CodeElement)AsCodeElement(context.CloseStatement()) ??
				(CodeElement)AsCodeElement(context.DeleteStatement()) ??
				(CodeElement)AsCodeElement(context.DisplayStatement()) ??
				(CodeElement)AsCodeElement(context.OpenStatement()) ??
				(CodeElement)AsCodeElement(context.ReadStatement()) ??
				(CodeElement)AsCodeElement(context.RewriteStatement()) ??
				(CodeElement)AsCodeElement(context.StartStatement()) ??
				//              (CodeElement)AsCodeElement(context.StopStatement()) ?? // literal
				(CodeElement)AsCodeElement(context.WriteStatement()) ??
				/* TODO
					| deleteStatementExplicitScope
					| readStatementExplicitScope
					| rewriteStatementExplicitScope
					| startStatementExplicitScope
					| writeStatementExplicitScope
				 */
				// -- ordering --
				(CodeElement)AsCodeElement(context.MergeStatement()) ??
				(CodeElement)AsCodeElement(context.ReleaseStatement()) ??
				(CodeElement)AsCodeElement(context.ReturnStatement()) ??
				(CodeElement)AsCodeElement(context.SortStatement()) ??
				/* TODO
					| returnStatementExplicitScope
				 */
				// -- procedure-branching --
				(CodeElement)AsCodeElement(context.AlterStatement()) ??
				(CodeElement)AsCodeElement(context.ExitStatement()) ??
				(CodeElement)AsCodeElement(context.GotoStatement()) ??
				(CodeElement)AsCodeElement(context.PerformProcedureStatement()) ??
				/* TODO
					| performStatementWithBody
				 */
				// -- program or method linkage --
				(CodeElement)AsCodeElement(context.CallStatement()) ??
				(CodeElement)AsCodeElement(context.CancelStatement()) ??
				(CodeElement)AsCodeElement(context.InvokeStatement()) ??
				/* TODO
					| callStatementExplicitScope
					| invokeStatementExplicitScope
				 */
				(CodeElement)AsCodeElement(context.ExecStatement()) ??
				null;
		}
	}
}
