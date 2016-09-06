using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;
using Antlr4.Runtime;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.Parser
{
	/// <summary>
	/// Build a Program or Class object while visiting its parse tree
	/// </summary>
	public class CobolNodeBuilder: ProgramClassBaseListener {
		/// <summary>
		/// Program object resulting of the visit the parse tree
		/// </summary>
		public CodeModel.Program Program { get; private set; }

		// Programs can be nested => track current programs being analyzed
		private Stack<CodeModel.Program> programsStack = null;

		private CodeModel.Program CurrentProgram {
			get { return programsStack.Peek(); }
			set { programsStack.Push(value); }
		}

		/// <summary>Class object resulting of the visit the parse tree</summary>
		public CodeModel.Class Class { get; private set; }

		private SymbolTable TableOfIntrisic = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
		private SymbolTable TableOfGlobals;

		public SymbolTable CustomSymbols {
			private get { throw new System.InvalidOperationException(); }
			set {
				if (value != null) {
					foreach(var values in value.DataEntries.Values)
						foreach(var data in values)
							TableOfIntrisic.AddVariable(data);
					foreach(var type in value.CustomTypes)
						TableOfIntrisic.AddType(type);
				}
				// TODO#249: use a COPY for these
				foreach (var type in DataType.BuiltInCustomTypes) {
					var typedef = new TypeDefinitionEntry();
					typedef.LevelNumber = new GeneratedIntegerValue(1);
					typedef.DataName = new SymbolDefinition(new GeneratedAlphanumericValue(type.Name), SymbolType.DataName);
					typedef.DataType = type;
					TableOfIntrisic.AddType(new TypeDefinition(typedef));
				}
			}
		}



		public NodeDispatcher Dispatcher { get; internal set; }


		public Node CurrentNode { get { return Program.SyntaxTree.CurrentNode; } }
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
				programsStack = new Stack<CodeModel.Program>();
				CurrentProgram = Program;
			} else {
				var enclosing = CurrentProgram;
				CurrentProgram = new NestedProgram(enclosing);
				Enter(CurrentProgram.SyntaxTree.Root, context, new SymbolTable(TableOfGlobals));
			}
			var terminal = context.ProgramIdentification();
			CurrentProgram.Identification = terminal != null? (ProgramIdentification)terminal.Symbol : null;
			Enter(new Nodes.Program(CurrentProgram.Identification), context, CurrentProgram.SymbolTable);
		}

		public override void ExitCobolProgram(ProgramClassParser.CobolProgramContext context) {
			AttachEndIfExists(context.ProgramEnd());
			Exit();
			programsStack.Pop();
		}

		public override void EnterEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
			var terminal = context.EnvironmentDivisionHeader();
			var header = terminal != null? (EnvironmentDivisionHeader)terminal.Symbol : null;
			Enter(new EnvironmentDivision(header), context);
		}
		public override void ExitEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
			Exit();
		}

		public override void EnterConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			var terminal = context.ConfigurationSectionHeader();
			var header = terminal != null? (ConfigurationSectionHeader)terminal.Symbol : null;
			Enter(new ConfigurationSection(header), context);
			var paragraphs = new List<CodeElement>();
			foreach(var paragraph in context.configurationParagraph()) {
				if (paragraph.SourceComputerParagraph() != null) {
					Enter(new SourceComputer((SourceComputerParagraph)paragraph.SourceComputerParagraph().Symbol));
					Exit();
				}
				if (paragraph.ObjectComputerParagraph() != null) {
					Enter(new ObjectComputer((ObjectComputerParagraph)paragraph.ObjectComputerParagraph().Symbol));
					Exit();
				}
				if (paragraph.SpecialNamesParagraph() != null) {
					Enter(new SpecialNames((SpecialNamesParagraph)paragraph.SpecialNamesParagraph().Symbol));
					Exit();
				}
				if (paragraph.RepositoryParagraph() != null) {
					Enter(new Repository((RepositoryParagraph)paragraph.RepositoryParagraph().Symbol));
					Exit();
				}
			}
		}
		public override void ExitConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			Exit(); // exit ConfigurationSection node
		}

		public override void EnterDataDivision(ProgramClassParser.DataDivisionContext context) {
			var terminal = context.DataDivisionHeader();
			var header = terminal != null? (DataDivisionHeader)terminal.Symbol : null;
			Enter(new DataDivision(header), context);
		}
		public override void ExitDataDivision(ProgramClassParser.DataDivisionContext context) {
			Exit();
		}

		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">FILE SECTION</param>
		public override void EnterFileSection(ProgramClassParser.FileSectionContext context) {
			var terminal = context.FileSectionHeader();
			var header = terminal != null? (FileSectionHeader)terminal.Symbol : null;
			Enter(new FileSection(header), context);
			//TODO: ( 1 FILE DESCRIPTION ENTRY + N DATA DESCRIPTION ENTRY ) N TIMES
		}
		public override void ExitFileSection(ProgramClassParser.FileSectionContext context) {
			ExitLastLevel1Definition();
			Exit();
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">WORKING-STORAGE SECTION</param>
		public override void EnterWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context) {
			var terminal = context.WorkingStorageSectionHeader();
			var header = terminal != null? (WorkingStorageSectionHeader)terminal.Symbol : null;
			Enter(new WorkingStorageSection(header), context);
		}
		public override void ExitWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context) {
			ExitLastLevel1Definition();
			Exit(); // Exit WorkingStorageSection
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">LOCAL-STORAGE SECTION</param>
		public override void EnterLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context) {
			var terminal = context.LocalStorageSectionHeader();
			var header = terminal != null? (LocalStorageSectionHeader)terminal.Symbol : null;
			Enter(new LocalStorageSection(header), context);
		}
		public override void ExitLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context) {
			ExitLastLevel1Definition();
			Exit(); // Exit LocalStorageSection
		}
		/// <summary>parent: DATA DIVISION</summary>
		/// <param name="context">LINKAGE SECTION</param>
		public override void EnterLinkageSection(ProgramClassParser.LinkageSectionContext context) {
			var terminal = context.LinkageSectionHeader();
			var header = terminal != null? (LinkageSectionHeader)terminal.Symbol : null;
			Enter(new LinkageSection(header), context);
		}
		public override void ExitLinkageSection(ProgramClassParser.LinkageSectionContext context) {
			ExitLastLevel1Definition();
			Exit(); // Exit LinkageSection
		}

		public override void EnterDataDefinitionEntry(ProgramClassParser.DataDefinitionEntryContext context) {
			if (context.DataDescriptionEntry() != null) {
				var data = (DataDescriptionEntry)context.DataDescriptionEntry().Symbol;
				if (data is TypeDefinitionEntry) EnterTypeDefinitionEntry((TypeDefinitionEntry)data);
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
		private void EnterTypeDefinitionEntry(TypeDefinitionEntry typedef) {
			SetCurrentNodeToTopLevelItem(typedef.LevelNumber.Value);
			Enter(new Nodes.TypeDefinition(typedef));
		}
// [/COBOL 2002]

		private void EnterDataDescriptionEntry(DataDescriptionEntry data) {
			SetCurrentNodeToTopLevelItem(data.LevelNumber.Value);
			var node = new DataDescription(data);
			Enter(node);
			node.SymbolTable.AddVariable(node);
		}

		private void EnterDataConditionEntry(DataConditionEntry data) {
			//TODO#249
			Enter(new DataCondition(data));
		}

		private void EnterDataRedefinesEntry(DataRedefinesEntry data) {
			//TODO#249
			Enter(new DataRedefines(data));
		}

		private void EnterDataRenamesEntry(DataRenamesEntry data) {
			//TODO#249
			Enter(new DataRenames(data));
		}

		/// <summary>Exit() every Node that is not the top-level item for a data of a given level.</summary>
		/// <param name="level">Level number of the next data definition that will be Enter()ed.</param>
		private void SetCurrentNodeToTopLevelItem(long level) {
			Node parent = GetTopLevelItem(level);
			if (parent != null) {
				// Exit() previous sibling and all of its last children
				while (parent != CurrentNode) Exit();
			} else {
				ExitLastLevel1Definition();
			}
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

		/// <summary>Exit last level-01 data definition entry, as long as all its subordinates.</summary>
		private void ExitLastLevel1Definition() {
			while (CurrentNode.CodeElement != null && TypeCobol.Tools.Reflection.IsTypeOf(CurrentNode.CodeElement.GetType(), typeof(DataDefinitionEntry))) Exit();
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
		public static int picture2Size(string picture) {
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
			bool hasParent = false;
			while(!hasParent && groups.Count > 0) {
				var toplevel = groups.Peek();
				if (data.LevelNumber <= toplevel.LevelNumber || data.LevelNumber == 66 || data.LevelNumber == 77) groups.Pop();
				else {
					toplevel.Subordinates.Add(data);
					data.TopLevel = toplevel;
					hasParent = true;
				}
			}
			groups.Push(data);
			return hasParent;
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
			var terminal = context.ProcedureDivisionHeader();
			var header = terminal != null? (ProcedureDivisionHeader)terminal.Symbol : null;
			Enter(new ProcedureDivision(header), context);
		}
		public override void ExitProcedureDivision(ProgramClassParser.ProcedureDivisionContext context) {
			Exit();
		}



		/// <summary>Parent node: PROCEDURE DIVISION</summary>
		/// <param name="context">DECLARE FUNCTION</param>
		public override void EnterFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context) {
			var terminal = context.FunctionDeclarationHeader();
			var header = terminal != null? (FunctionDeclarationHeader)terminal.Symbol : null;
			if (header != null) header.SetLibrary(CurrentProgram.Identification.ProgramName.Name);
			var node = new FunctionDeclaration(header);
			node.Library = CurrentProgram.Identification.ProgramName.Name;
			CurrentProgram.CurrentTable.AddFunction(node);
			Enter(node, context, new SymbolTable(CurrentProgram.CurrentTable, SymbolTable.Scope.Function));
		}
		public override void ExitFunctionDeclaration(ProgramClassParser.FunctionDeclarationContext context) {
			var terminal = context.FunctionDeclarationEnd();
			var end = terminal != null? (FunctionDeclarationEnd)terminal.Symbol : null;
			Enter(new FunctionEnd(end), context);
			Exit();
			Exit();// exit DECLARE FUNCTION
		}
		/// <summary>Parent node: DECLARE FUNCTION</summary>
		/// <param name="context">PROCEDURE DIVISION</param>
		public override void EnterFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context) {
			var terminal = context.ProcedureDivisionHeader();
			FunctionDeclarationProfile p = null;
			if (terminal.Symbol is FunctionDeclarationProfile)
				p = (FunctionDeclarationProfile)terminal.Symbol;
			else
			if (terminal.Symbol is ProcedureDivisionHeader)
				p = new FunctionDeclarationProfile((ProcedureDivisionHeader)terminal.Symbol);
			else throw new NotImplementedException("Unknown type: "+terminal.Symbol.GetType().Name);
/*			char[] currencies = GetCurrencies();
			int offset = 0;
			Stack<DataDescriptionEntry> groups;
			groups = new Stack<DataDescriptionEntry>();
			foreach(var p in profile.InputParameters) {
				ComputeParent(p, groups);
				ComputeType(p, currencies);
				ComputeMemoryProfile(p, ref offset);
			}
			groups = new Stack<DataDescriptionEntry>();
			foreach(var p in profile.InoutParameters) {
				ComputeParent(p, groups);
				ComputeType(p, currencies);
				ComputeMemoryProfile(p, ref offset);
			}
			groups = new Stack<DataDescriptionEntry>();
			foreach(var p in profile.OutputParameters) {
				ComputeParent(p, groups);
				ComputeType(p, currencies);
				ComputeMemoryProfile(p, ref offset);
			}
			groups = new Stack<DataDescriptionEntry>();
			if (profile.ReturningParameter != null) {
				ComputeParent(profile.ReturningParameter, groups);
				ComputeType(profile.ReturningParameter, currencies);
				ComputeMemoryProfile(profile.ReturningParameter, ref offset);
			}
*/
			var profile = new FunctionProfile(p);
			Enter(profile, context);

			var node = profile.Parent;
			var header = (FunctionDeclarationHeader)node.CodeElement;
			foreach(var parameter in p.Profile.InputParameters)  node.SymbolTable.AddVariable(parameter);
			foreach(var parameter in p.Profile.OutputParameters) node.SymbolTable.AddVariable(parameter);
			foreach(var parameter in p.Profile.InoutParameters)  node.SymbolTable.AddVariable(parameter);
			if (p.Profile.ReturningParameter != null) node.SymbolTable.AddVariable(p.Profile.ReturningParameter);

//			var function = new Function(header.Name, p.Profile.InputParameters, p.Profile.OutputParameters, p.Profile.InoutParameters, p.Profile.ReturningParameter, header.Visibility);
//			node.SymbolTable.EnclosingScope.Register(function);
		}
		public override void ExitFunctionProcedureDivision(ProgramClassParser.FunctionProcedureDivisionContext context) {
			Exit();
		}



		public override void EnterSection(ProgramClassParser.SectionContext context) {
			// if we Enter(..) a node here, it will be detached by ExitParagraph
			// if we do not, no need to detach anything in ExitSection
			if (context.SectionHeader() != null) {
				SectionHeader header = (SectionHeader)context.SectionHeader().Symbol;
				Enter(new Section(header), context);
			} else
			if (context.ParagraphHeader() != null) {
				ParagraphHeader header = (ParagraphHeader)context.ParagraphHeader().Symbol;
				Enter(new Paragraph(header), context);
			}
		}

		public override void EnterParagraph(ProgramClassParser.ParagraphContext context) {
			if (!(Program.SyntaxTree.CurrentNode.CodeElement is ParagraphHeader)) {
				ParagraphHeader header = (ParagraphHeader)context.ParagraphHeader().Symbol;
				Enter(new Paragraph(header), context);
			}
		}
		public override void ExitParagraph(ProgramClassParser.ParagraphContext context) {
			Exit();
		}

		public override void EnterSentence(ProgramClassParser.SentenceContext context) {
			Enter(new Sentence(), context);
		}
		public override void ExitSentence(ProgramClassParser.SentenceContext context) {
			AttachEndIfExists(context.SentenceEnd());
			Exit();
		}

		public override void EnterStatement(ProgramClassParser.StatementContext context) {
			if (context.ExecStatement() != null) Enter(new Exec((ExecStatement)context.ExecStatement().Symbol), context);
			else if (context.evaluateStatementWithBody() != null) ;// Node will be created in EnterEvaluateStatementWithBody
			else if (context.ifStatementWithBody() != null) ;// Node will be created in EnterIfStatementWithBody
			else if (context.performStatementWithBody() != null) ;// Node will be created in EnterPerformStatementWithBody
			else if (context.PerformProcedureStatement() != null) Enter(new PerformProcedure((PerformProcedureStatement)context.PerformProcedureStatement().Symbol), context);
			else if (context.searchStatementWithBody() != null) ;// Node will be created in EnterSearchStatementWithBody
			// -- arithmetic --
			else if (context.AddStatement() != null) Enter(new Add((AddStatement)context.AddStatement().Symbol), context);
			else if (context.addStatementConditional() != null) ;// Node will be created in EnterAddStatementConditional
			else if (context.ComputeStatement() != null) Enter(new Compute((ComputeStatement)context.ComputeStatement().Symbol), context);
			else if (context.computeStatementConditional() != null) ;// Node will be created in EnterComputeStatementConditional
			else if (context.DivideStatement() != null) Enter(new Divide((DivideStatement)context.DivideStatement().Symbol), context);
			else if (context.divideStatementConditional() != null) ;// Node will be created in EnterDivideStatementConditional
			else if (context.MultiplyStatement() != null) Enter(new Multiply((MultiplyStatement)context.MultiplyStatement().Symbol), context);
			else if (context.multiplyStatementConditional() != null) ;// Node will be created in EnterMultiplyStatementConditional
			else if (context.SubtractStatement() != null) Enter(new Subtract((SubtractStatement)context.SubtractStatement().Symbol), context);
			else if (context.subtractStatementConditional() != null) ;// Node will be created in EnterSubtractStatementConditional
			// -- file --
			else if (context.OpenStatement() != null) Enter(new Open((OpenStatement)context.OpenStatement().Symbol), context);
			else if (context.CloseStatement() != null) Enter(new Close((CloseStatement)context.CloseStatement().Symbol), context);
			else if (context.ReadStatement() != null) Enter(new Read((ReadStatement)context.ReadStatement().Symbol), context);
			else if (context.readStatementConditional() != null) ;// Node will be created in EnterReadStatementConditional
			else if (context.RewriteStatement() != null) Enter(new Rewrite((RewriteStatement)context.RewriteStatement().Symbol), context);
			else if (context.rewriteStatementConditional() != null) ;// Node will be created in EnterRewriteStatementConditional
			else if (context.WriteStatement() != null) Enter(new Write((WriteStatement)context.WriteStatement().Symbol), context);
			else if (context.writeStatementConditional() != null) ;// Node will be created in EnterWriteStatementConditional
			// -- data movement --
			else if (context.MoveStatement() != null) Enter(new Move((MoveStatement)context.MoveStatement().Symbol), context);
			else if (context.SetStatement() != null) Enter(new Set((SetStatement)context.SetStatement().Symbol), context);
			// -- other --
			else if (context.AcceptStatement() != null) Enter(new Accept((AcceptStatement)context.AcceptStatement().Symbol), context);
			else if (context.AlterStatement() != null) Enter(new Alter((AlterStatement)context.AlterStatement().Symbol), context);
			else if (context.CallStatement() != null) Enter(new Call((CallStatement)context.CallStatement().Symbol), context);
			else if (context.callStatementConditional() != null) ;// Node will be created in EnterCallStatementConditional
			else if (context.CancelStatement() != null) Enter(new Cancel((CancelStatement)context.CancelStatement().Symbol), context);
			else if (context.ContinueStatement() != null) Enter(new Continue((ContinueStatement)context.ContinueStatement().Symbol), context);
			else if (context.DeleteStatement() != null) Enter(new Delete((DeleteStatement)context.DeleteStatement().Symbol), context);
			else if (context.deleteStatementConditional() != null) ;// Node will be created in EnterDeleteStatementConditional
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
			else if (context.invokeStatementConditional() != null) ;// Node will be created in EnterInvokeStatementConditional
			else if (context.MergeStatement() != null) Enter(new Merge((MergeStatement)context.MergeStatement().Symbol), context);
			else if (context.PerformProcedureStatement() != null) Enter(new PerformProcedure((PerformProcedureStatement)context.PerformProcedureStatement().Symbol), context);
			else if (context.ReleaseStatement() != null) Enter(new Release((ReleaseStatement)context.ReleaseStatement().Symbol), context);
			else if (context.ReturnStatement() != null) Enter(new Return((ReturnStatement)context.ReturnStatement().Symbol), context);
			else if (context.returnStatementConditional() != null) ;// Node will be created in EnterReturnStatementConditional
			else if (context.SortStatement() != null) Enter(new Sort((SortStatement)context.SortStatement().Symbol), context);
			else if (context.StartStatement() != null) Enter(new Start((StartStatement)context.StartStatement().Symbol), context);
			else if (context.startStatementConditional() != null) ;// Node will be created in EnterStartStatementConditional
			else if (context.StopStatement() != null) Enter(new Stop((StopStatement)context.StopStatement().Symbol), context);
			else if (context.StringStatement() != null) Enter(new Nodes.String((StringStatement)context.StringStatement().Symbol), context);
			else if (context.stringStatementConditional() != null) ;// Node will be created in EnterStringStatementConditional
			else if (context.UnstringStatement() != null) Enter(new Unstring((UnstringStatement)context.UnstringStatement().Symbol), context);
			else if (context.unstringStatementConditional() != null) ;// Node will be created in EnterUnstringStatementConditional
			else if (context.XmlGenerateStatement() != null) Enter(new XmlGenerate((XmlGenerateStatement)context.XmlGenerateStatement().Symbol), context);
			else if (context.xmlGenerateStatementConditional() != null) ;// Node will be created in EnterXmlGenerateStatementConditional
			else if (context.XmlParseStatement() != null) Enter(new XmlParse((XmlParseStatement)context.XmlParseStatement().Symbol), context);
			else if (context.xmlParseStatementConditional() != null) ;// Node will be created in EnterXmlParseStatementConditional
			else if (context.GetText().Length < 1) skipEmptyStatement = true;
			else throw new NotImplementedException("Implementation error: \""+context.GetText()+"\"["+context.GetType().Name+']');
		}
		private bool skipEmptyStatement = false;
		public override void ExitStatement(ProgramClassParser.StatementContext context) {
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


		public override void EnterIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
			var terminal = context.IfStatement();
			var statement = terminal != null? (IfStatement)terminal.Symbol : null;
			Enter(new If(statement), context);
			Enter(new Then(), context);
		}
		public override void EnterElseClause(ProgramClassParser.ElseClauseContext context) {
			Exit();// we want ELSE to be child of IF, not THEN, so exit THEN
			var terminal = context.ElseCondition();
			var condition = terminal != null? (ElseCondition)terminal.Symbol : null;
			Enter(new Else(condition), context);// ELSE
			if (context.NextSentenceStatement() != null) {
				Enter(new NextSentence((NextSentenceStatement)context.NextSentenceStatement().Symbol));
				Exit();
			}
		}
		public override void ExitIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
			Exit(); // Exit ELSE (if any) or THEN
			AttachEndIfExists(context.IfStatementEnd());
			// DO NOT Exit() IF node because this will be done in ExitStatement
		}


		public override void EnterEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			var terminal = context.EvaluateStatement();
			var statement = terminal != null? (EvaluateStatement)terminal.Symbol : null;
			Enter(new Evaluate(statement), context);// enter EVALUATE
		}
		public override void EnterWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			Enter(new WhenGroup(), context);// enter WHEN group
			foreach(var terminal in context.WhenCondition()) {
				var condition = terminal != null? (WhenCondition)terminal.Symbol : null;
				Enter(new When(condition), context);
				Exit();
			}
			Exit();// exit WHEN group
			Enter(new Then(), context);// enter THEN
		}
		public override void ExitWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			Exit();// exit THEN
		}
		public override void EnterWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			var terminal = context.WhenOtherCondition();
			var condition = terminal != null? (WhenOtherCondition)terminal.Symbol : null;
			Enter(new WhenOther(condition), context);// enter WHEN OTHER
		}
		public override void ExitWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			Exit();// exit WHEN OTHER
		}
		public override void ExitEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			AttachEndIfExists(context.EvaluateStatementEnd());// exit EVALUATE
		}


		public override void EnterPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
			var terminal = context.PerformStatement();
			var statement = terminal != null? (PerformStatement)terminal.Symbol : null;
			Enter(new Perform(statement), context);
		}
		public override void ExitPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
			AttachEndIfExists(context.PerformStatementEnd());
		}

		public override void EnterSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
			var terminal = context.SearchStatement();
			var statement = terminal != null? (SearchStatement)terminal.Symbol : null;
			Enter(new Search(statement), context);
		}
		public override void EnterWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
			var terminal = context.WhenSearchCondition();
			var condition = terminal != null? (WhenSearchCondition)terminal.Symbol : null;
			Enter(new WhenSearch(condition), context);
			if (context.NextSentenceStatement() != null) {
				Enter(new NextSentence((NextSentenceStatement)context.NextSentenceStatement().Symbol));
				Exit();
			}
		}
		public override void ExitWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
			Exit(); // WHEN
		}
		public override void ExitSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
			AttachEndIfExists(context.SearchStatementEnd());
		}


		public override void EnterAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.AddStatement();
			var statement = terminal != null? (AddStatement)terminal.Symbol : null;
			Enter(new Add(statement), context);
		}
		public override void ExitAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
			AttachEndIfExists(context.AddStatementEnd());
		}
		public override void EnterComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.ComputeStatement();
			var statement = terminal != null? (ComputeStatement)terminal.Symbol : null;
			Enter(new Compute(statement), context);
		}
		public override void ExitComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
			AttachEndIfExists(context.ComputeStatementEnd());
		}
		public override void EnterDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.DivideStatement();
			var statement = terminal != null? (DivideStatement)terminal.Symbol : null;
			Enter(new Divide(statement), context);
		}
		public override void ExitDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
			AttachEndIfExists(context.DivideStatementEnd());
		}
		public override void EnterMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.MultiplyStatement();
			var statement = terminal != null? (MultiplyStatement)terminal.Symbol : null;
			Enter(new Multiply(statement), context);
		}
		public override void ExitMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
			AttachEndIfExists(context.MultiplyStatementEnd());
		}
		public override void EnterSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.SubtractStatement();
			var statement = terminal != null? (SubtractStatement)terminal.Symbol : null;
			Enter(new Subtract(statement), context);
		}
		public override void ExitSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
			AttachEndIfExists(context.SubtractStatementEnd());
		}
		public override void EnterDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.DeleteStatement();
			var statement = terminal != null? (DeleteStatement)terminal.Symbol : null;
			Enter(new Delete(statement), context);
		}
		public override void ExitDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
			AttachEndIfExists(context.DeleteStatementEnd());
		}
		public override void EnterReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.ReadStatement();
			var statement = terminal != null? (ReadStatement)terminal.Symbol : null;
			Enter(new Read(statement), context);
		}
		public override void ExitReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
			AttachEndIfExists(context.ReadStatementEnd());
		}
		public override void EnterWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.WriteStatement();
			var statement = terminal != null? (WriteStatement)terminal.Symbol : null;
			Enter(new Write(statement), context);
		}
		public override void ExitWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
			AttachEndIfExists(context.WriteStatementEnd());
		}
		public override void EnterRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.RewriteStatement();
			var statement = terminal != null? (RewriteStatement)terminal.Symbol : null;
			Enter(new Rewrite(statement), context);
		}
		public override void ExitRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
			AttachEndIfExists(context.RewriteStatementEnd());
		}
		public override void EnterStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.StartStatement();
			var statement = terminal != null? (StartStatement)terminal.Symbol : null;
			Enter(new Start(statement), context);
		}
		public override void ExitStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
			AttachEndIfExists(context.StartStatementEnd());
		}
		public override void EnterReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.ReturnStatement();
			var statement = terminal != null? (ReturnStatement)terminal.Symbol : null;
			Enter(new Return(statement), context);
		}
		public override void ExitReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
			AttachEndIfExists(context.ReturnStatementEnd());
		}
		public override void EnterStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.StringStatement();
			var statement = terminal != null? (StringStatement)terminal.Symbol : null;
			Enter(new Nodes.String(statement), context);
		}
		public override void ExitStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
			AttachEndIfExists(context.StringStatementEnd());
		}
		public override void EnterUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.UnstringStatement();
			var statement = terminal != null? (UnstringStatement)terminal.Symbol : null;
			Enter(new Unstring(statement), context);
		}
		public override void ExitUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
			AttachEndIfExists(context.UnstringStatementEnd());
		}
		public override void EnterCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.CallStatement();
			var statement = terminal != null? (CallStatement)terminal.Symbol : null;
			Enter(new Call(statement), context);
		}
		public override void ExitCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
			AttachEndIfExists(context.CallStatementEnd());
		}
		public override void EnterInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.InvokeStatement();
			var statement = terminal != null? (InvokeStatement)terminal.Symbol : null;
			Enter(new Invoke(statement), context);
		}
		public override void ExitInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
			AttachEndIfExists(context.InvokeStatementEnd());
		}
		public override void EnterXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.XmlGenerateStatement();
			var statement = terminal != null? (XmlGenerateStatement)terminal.Symbol : null;
			Enter(new XmlGenerate(statement), context);
		}
		public override void ExitXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
			AttachEndIfExists(context.XmlStatementEnd());
		}
		public override void EnterXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
			Delete();// delete the node we attached in EnterStatement
			var terminal = context.XmlParseStatement();
			var statement = terminal != null? (XmlParseStatement)terminal.Symbol : null;
			Enter(new XmlParse(statement), context);
		}
		public override void ExitXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
			AttachEndIfExists(context.XmlStatementEnd());
		}

		public override void EnterOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
			var terminal = context.OnSizeErrorCondition();
			var condition = terminal != null? (OnSizeErrorCondition)terminal.Symbol : null;
			Enter(new OnSizeError(condition), context);
		}
		public override void ExitOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
			Exit();
		}
		public override void EnterNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
			var terminal = context.NotOnSizeErrorCondition();
			var condition = terminal != null? (NotOnSizeErrorCondition)terminal.Symbol : null;
			Enter(new NoSizeError(condition), context);
		}
		public override void ExitNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
			Exit();
		}
		public override void EnterOnAtEnd(ProgramClassParser.OnAtEndContext context) {
			var terminal = context.AtEndCondition();
			var condition = terminal != null? (AtEndCondition)terminal.Symbol : null;
			Enter(new OnAtEnd(condition), context);
		}
		public override void ExitOnAtEnd(ProgramClassParser.OnAtEndContext context) {
			Exit();
		}
		public override void EnterNoAtEnd(ProgramClassParser.NoAtEndContext context) {
			var terminal = context.NotAtEndCondition();
			var condition = terminal != null? (NotAtEndCondition)terminal.Symbol : null;
			Enter(new NoAtEnd(condition), context);
		}
		public override void ExitNoAtEnd(ProgramClassParser.NoAtEndContext context) {
			Exit();
		}
		public override void EnterOnException(ProgramClassParser.OnExceptionContext context) {
			var terminal = context.OnExceptionCondition();
			var condition = terminal != null? (OnExceptionCondition)terminal.Symbol : null;
			Enter(new OnException(condition), context);
		}
		public override void ExitOnException(ProgramClassParser.OnExceptionContext context) {
			Exit();
		}
		public override void EnterNoException(ProgramClassParser.NoExceptionContext context) {
			var terminal = context.NotOnExceptionCondition();
			var condition = terminal != null? (NotOnExceptionCondition)terminal.Symbol : null;
			Enter(new NoException(condition), context);
		}
		public override void ExitNoException(ProgramClassParser.NoExceptionContext context) {
			Exit();
		}
		public override void EnterOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
			var terminal = context.InvalidKeyCondition();
			var condition = terminal != null? (InvalidKeyCondition)terminal.Symbol : null;
			Enter(new OnInvalidKey(condition), context);
		}
		public override void ExitOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
			Exit();
		}
		public override void EnterNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
			var terminal = context.NotInvalidKeyCondition();
			var condition = terminal != null? (NotInvalidKeyCondition)terminal.Symbol : null;
			Enter(new NoInvalidKey(condition), context);
		}
		public override void ExitNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
			Exit();
		}
		public override void EnterOnOverflow(ProgramClassParser.OnOverflowContext context) {
			var terminal = context.OnOverflowCondition();
			var condition = terminal != null? (OnOverflowCondition)terminal.Symbol : null;
			Enter(new OnOverflow(condition), context);
		}
		public override void ExitOnOverflow(ProgramClassParser.OnOverflowContext context) {
			Exit();
		}
		public override void EnterNoOverflow(ProgramClassParser.NoOverflowContext context) {
			var terminal = context.NotOnOverflowCondition();
			var condition = terminal != null? (NotOnOverflowCondition)terminal.Symbol : null;
			Enter(new NoOverflow(condition), context);
		}
		public override void ExitNoOverflow(ProgramClassParser.NoOverflowContext context) {
			Exit();
		}





		private void AttachEndIfExists(Antlr4.Runtime.Tree.ITerminalNode terminal) {
			var end = terminal != null? (CodeElementEnd)terminal.Symbol : null;
			if (end == null) return;
			Enter(new End(end));
			Exit();
		}
	}
}
