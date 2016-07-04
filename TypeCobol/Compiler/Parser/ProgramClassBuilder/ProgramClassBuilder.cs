using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;
using Antlr4.Runtime;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a Program or Class object while visiting its parse tree
    /// </summary>
    public class ProgramClassBuilder : ProgramClassBaseListener
    {
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
				RegisterCustomType(TableOfIntrisic, DataType.Date);
				RegisterCustomType(TableOfIntrisic, DataType.Boolean);
				TableOfIntrisic.Register(CodeElements.Functions.SampleFactory.Create("POW"));
            }
		}

		private void RegisterCustomType(SymbolTable table, DataType type) {
			try { table.GetCustomType(type.Name); }
			catch(ArgumentException ex) { table.RegisterCustomType(new CustomTypeDefinition(type)); }
		}
		private void RegisterCustomType(SymbolTable table, TypeDefinition type) {
			try { table.GetCustomType(type.DataType.Name); }
			catch(ArgumentException ex) { table.RegisterCustomType(type); }
		}

        public NodeDispatcher Dispatcher { get; internal set; }

		private void _add(Node node) {
			node.SymbolTable = CurrentProgram.SymbolTable;
			Program.SyntaxTree.Add(node);
		}
		private void _enter(CodeElement e, ParserRuleContext context) {
			var node = new Node(e);
			_enter(node);
			if (e!=null) Dispatcher.OnNode(node, context, CurrentProgram);
		}
		private void _enter(Node node) {
			node.SymbolTable = CurrentProgram.SymbolTable;
			Program.SyntaxTree.Attach(node);
		}
		private void _exit() { Program.SyntaxTree.Detach(); }
		private void _del() { Program.SyntaxTree.Delete(); }

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
                _enter(CurrentProgram.SyntaxTree.Root);
            }
            CurrentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;
            _enter(new Node(AsCodeElement(context.ProgramIdentification())));
        }

        public override void ExitCobolProgram(ProgramClassParser.CobolProgramContext context) {
            var end = AsCodeElement(context.ProgramEnd());
            if (end != null) _add(new Node(end));
            _exit();
            programsStack.Pop();
        }

        public override void EnterEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
            _enter(new Node(AsCodeElement(context.EnvironmentDivisionHeader())));
        }
        public override void ExitEnvironmentDivision(ProgramClassParser.EnvironmentDivisionContext context) {
            _exit();
        }

		public override void EnterConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			_enter(new Node(AsCodeElement(context.ConfigurationSectionHeader())));
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
			foreach(var p in paragraphs) _add(new Node(p));
		}
		public override void ExitConfigurationSection(ProgramClassParser.ConfigurationSectionContext context) {
			_exit();
		}

        public override void EnterDataDivision(ProgramClassParser.DataDivisionContext context) {
            _enter(new Node(AsCodeElement(context.DataDivisionHeader())));
        }
        public override void ExitDataDivision(ProgramClassParser.DataDivisionContext context) {
            _exit();
        }

        public override void EnterWorkingStorageSection(ProgramClassParser.WorkingStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.WorkingStorageSectionHeader(), entries);
        }

        public override void EnterLocalStorageSection(ProgramClassParser.LocalStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LocalStorageSectionHeader(), entries);
        }

        public override void EnterLinkageSection(ProgramClassParser.LinkageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LinkageSectionHeader(), entries);
        }

		private void AddStorageNode(Antlr4.Runtime.Tree.ITerminalNode terminal, IList<DataDescriptionEntry> entries) {
			var node = new Node(AsCodeElement(terminal));
			_enter(node);
			AddEntries(node, entries);
			_exit();
		}
		private void AddEntries(Node root, IEnumerable<DataDescriptionEntry> entries) {
			foreach(var entry in entries) {
				var child = new Node(entry);
				_enter(child);
				AddEntries(child, entry.Subordinates);
				_exit();
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

				if (data.IsTypeDefinition) CurrentProgram.SymbolTable.RegisterCustomType(data);
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
			        var redfinedItems = CurrentProgram.SymbolTable.Get(data.RedefinesDataName.Name);
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

			    CurrentProgram.SymbolTable.Add(data);
				if (customtype != null) {
					foreach(var sub in customtype.Subordinates) {
						// add a clone so parent/child relations are not spoiled
						var clone = sub.Clone() as DataDescriptionEntry;
						data.Subordinates.Add(clone);
						clone.TopLevel = data;
						UpdateLevelNumbers(clone, data.LevelNumber);

						CurrentProgram.SymbolTable.Add(clone);
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
            var renames = CurrentProgram.SymbolTable.Get(renamesFrom);
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
				CurrentProgram.SymbolTable.Add(clone);

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
					var customTypeGroup = CurrentProgram.SymbolTable.GetCustomType(typename);
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
			try { return CurrentProgram.SymbolTable.GetCustomType(name); }
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

        public override void EnterProcedureDivision(ProgramClassParser.ProcedureDivisionContext context) {
            _enter(new Node(AsCodeElement(context.ProcedureDivisionHeader())));
        }
        public override void ExitProcedureDivision(ProgramClassParser.ProcedureDivisionContext context) {
            _exit();
        }

        public override void EnterSection(ProgramClassParser.SectionContext context) {
            var terminal = context.SectionHeader();
            if (terminal == null) terminal = context.ParagraphHeader();
            // if we _enter(..) a node here, it will be detached by ExitParagraph
            // if we do not, no need to detach anything in ExitSection
            if (terminal != null) _enter(new Node(AsCodeElement(terminal)));
        }

        public override void EnterParagraph(ProgramClassParser.ParagraphContext context) {
            if (Program.SyntaxTree.Head().CodeElement is ParagraphHeader) _exit();
            _enter(new Node(AsCodeElement(context.ParagraphHeader())));
        }
        public override void ExitParagraph(ProgramClassParser.ParagraphContext context) {
            _exit();
        }

        public override void EnterSentence(ProgramClassParser.SentenceContext context) {
            _enter(new Node(null));
        }
        public override void ExitSentence(ProgramClassParser.SentenceContext context) {
            var end = AsCodeElement(context.SentenceEnd());
            if (end != null) _add(new Node(end));
            _exit();
        }

		public override void EnterStatement(ProgramClassParser.StatementContext context) {
			CodeElement statement = AsStatement(context);
			FixSubscriptableQualifiedNames(statement);
			_enter(statement, context);
		}
		public override void ExitStatement(ProgramClassParser.StatementContext context) {
			_exit();
		}

		private void FixSubscriptableQualifiedNames(CodeElement statement) {
			var identifiers = statement as IdentifierUser;
			if (identifiers == null) return;
			foreach(var identifier in identifiers.Identifiers) {
				if (identifier.Name is TypeCobol.Compiler.CodeElements.Expressions.Subscripted) continue;
				if (identifier is TypeCobol.Compiler.CodeElements.Expressions.Subscriptable) {
					var found = CurrentProgram.SymbolTable.Get(identifier.Name);
					if (found.Count != 1) continue;// ambiguity is not our job
					List<string> errors;
					var qelement = TypeCobol.Compiler.CodeElements.Expressions.SubscriptedQualifiedName.Create(identifier, found[0], out errors);
					(identifier as TypeCobol.Compiler.CodeElements.Expressions.Subscriptable).UpdateSubscripting(qelement);
					foreach(string error in errors) DiagnosticUtils.AddError(statement, error);
				}
			}
		}



        public override void EnterIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.IfStatement())));
            _enter(new Node(null));//THEN
        }
        public override void EnterElseClause(ProgramClassParser.ElseClauseContext context) {
            _exit();// we want ELSE to be child of IF, not THEN, so exit THEN
            _enter(new Node(AsCodeElement(context.ElseCondition())));// ELSE
            var next = AsCodeElement(context.NextSentenceStatement());
            if (next != null) _add(new Node(next));
        }
        public override void ExitIfStatementWithBody(ProgramClassParser.IfStatementWithBodyContext context) {
            _exit(); // _exit() ELSE (if any) or THEN
            var end = AsCodeElement(context.IfStatementEnd());
            if (end != null) _add(new Node(end));
            // don't _exit() IF node because this will be done in ExitStatement
        }


		public override void EnterEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			_del();// delete the node we attached in EnterStatement
			_enter(new Node(AsCodeElement(context.EvaluateStatement())));// enter EVALUATE
		}
		public override void EnterWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			_enter(new Node(null));// enter WHEN group
			foreach(var condition in context.WhenCondition()) {
				_enter(new Node(AsCodeElement(condition)));
				_exit();
			}
			_exit();// exit WHEN group
			_enter(new Node(null));// enter THEN
		}
		public override void ExitWhenConditionClause(ProgramClassParser.WhenConditionClauseContext context) {
			_exit();// exit THEN
		}
		public override void EnterWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			_enter(new Node(AsCodeElement(context.WhenOtherCondition())));// enter WHEN OTHER
		}
		public override void ExitWhenOtherClause(ProgramClassParser.WhenOtherClauseContext context) {
			_exit();// exit WHEN OTHER
		}
		public override void ExitEvaluateStatementWithBody(ProgramClassParser.EvaluateStatementWithBodyContext context) {
			ExitConditionalStatement(context.EvaluateStatementEnd());// exit EVALUATE
		}


        public override void EnterPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.PerformStatement())));
        }
        public override void ExitPerformStatementWithBody(ProgramClassParser.PerformStatementWithBodyContext context) {
            ExitConditionalStatement(context.PerformStatementEnd());
        }

        public override void EnterSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.SearchStatement())));
        }
        public override void EnterWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
            _enter(new Node(AsCodeElement(context.WhenCondition())));
            var next = AsCodeElement(context.NextSentenceStatement());
            if (next != null) _add(new Node(next));
        }
        public override void ExitWhenSearchConditionClause(ProgramClassParser.WhenSearchConditionClauseContext context) {
            _exit(); // WHEN
        }
        public override void ExitSearchStatementWithBody(ProgramClassParser.SearchStatementWithBodyContext context) {
            ExitConditionalStatement(context.SearchStatementEnd());
        }


        public override void EnterAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
            EnterConditionalStatement(context.AddStatement());
        }
        public override void ExitAddStatementConditional(ProgramClassParser.AddStatementConditionalContext context) {
            ExitConditionalStatement(context.AddStatementEnd());
        }
        public override void EnterComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
            EnterConditionalStatement(context.ComputeStatement());
        }
        public override void ExitComputeStatementConditional(ProgramClassParser.ComputeStatementConditionalContext context) {
            ExitConditionalStatement(context.ComputeStatementEnd());
        }
        public override void EnterDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
            EnterConditionalStatement(context.DivideStatement());
        }
        public override void ExitDivideStatementConditional(ProgramClassParser.DivideStatementConditionalContext context) {
            ExitConditionalStatement(context.DivideStatementEnd());
        }
        public override void EnterMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
            EnterConditionalStatement(context.MultiplyStatement());
        }
        public override void ExitMultiplyStatementConditional(ProgramClassParser.MultiplyStatementConditionalContext context) {
            ExitConditionalStatement(context.MultiplyStatementEnd());
        }
        public override void EnterSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
            EnterConditionalStatement(context.SubtractStatement());
        }
        public override void ExitSubtractStatementConditional(ProgramClassParser.SubtractStatementConditionalContext context) {
            ExitConditionalStatement(context.SubtractStatementEnd());
        }

        public override void EnterDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
            EnterConditionalStatement(context.DeleteStatement());
        }
        public override void ExitDeleteStatementConditional(ProgramClassParser.DeleteStatementConditionalContext context) {
            ExitConditionalStatement(context.DeleteStatementEnd());
        }
        public override void EnterReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
            EnterConditionalStatement(context.ReadStatement());
        }
        public override void ExitReadStatementConditional(ProgramClassParser.ReadStatementConditionalContext context) {
            ExitConditionalStatement(context.ReadStatementEnd());
        }
        public override void EnterWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
            EnterConditionalStatement(context.WriteStatement());
        }
        public override void ExitWriteStatementConditional(ProgramClassParser.WriteStatementConditionalContext context) {
            ExitConditionalStatement(context.WriteStatementEnd());
        }
        public override void EnterRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
            EnterConditionalStatement(context.RewriteStatement());
        }
        public override void ExitRewriteStatementConditional(ProgramClassParser.RewriteStatementConditionalContext context) {
            ExitConditionalStatement(context.RewriteStatementEnd());
        }
        public override void EnterStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
            EnterConditionalStatement(context.StartStatement());
        }
        public override void ExitStartStatementConditional(ProgramClassParser.StartStatementConditionalContext context) {
            ExitConditionalStatement(context.StartStatementEnd());
        }
        public override void EnterReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
            EnterConditionalStatement(context.ReturnStatement());
        }
        public override void ExitReturnStatementConditional(ProgramClassParser.ReturnStatementConditionalContext context) {
            ExitConditionalStatement(context.ReturnStatementEnd());
        }

        public override void EnterStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
            EnterConditionalStatement(context.StringStatement());
        }
        public override void ExitStringStatementConditional(ProgramClassParser.StringStatementConditionalContext context) {
            ExitConditionalStatement(context.StringStatementEnd());
        }
        public override void EnterUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
            EnterConditionalStatement(context.UnstringStatement());
        }
        public override void ExitUnstringStatementConditional(ProgramClassParser.UnstringStatementConditionalContext context) {
            ExitConditionalStatement(context.UnstringStatementEnd());
        }

        public override void EnterCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
            EnterConditionalStatement(context.CallStatement());
        }
        public override void ExitCallStatementConditional(ProgramClassParser.CallStatementConditionalContext context) {
            ExitConditionalStatement(context.CallStatementEnd());
        }
        public override void EnterInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
            EnterConditionalStatement(context.InvokeStatement());
        }
        public override void ExitInvokeStatementConditional(ProgramClassParser.InvokeStatementConditionalContext context) {
            ExitConditionalStatement(context.InvokeStatementEnd());
        }
        public override void EnterXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
            EnterConditionalStatement(context.XmlGenerateStatement());
        }
        public override void ExitXmlGenerateStatementConditional(ProgramClassParser.XmlGenerateStatementConditionalContext context) {
            ExitConditionalStatement(context.XmlStatementEnd());
        }
        public override void EnterXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
            EnterConditionalStatement(context.XmlParseStatement());
        }
        public override void ExitXmlParseStatementConditional(ProgramClassParser.XmlParseStatementConditionalContext context) {
            ExitConditionalStatement(context.XmlStatementEnd());
        }

        private void EnterConditionalStatement(Antlr4.Runtime.Tree.ITerminalNode terminal) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(terminal)));
        }
        private void ExitConditionalStatement(Antlr4.Runtime.Tree.ITerminalNode terminal) {
            var end = AsCodeElement(terminal);
            if (end != null) _add(new Node(end));
            // don't _exit() because this will be done in ExitStatement
        }

        public override void EnterOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
            _enter(new Node(AsCodeElement(context.OnSizeErrorCondition())));
        }
        public override void ExitOnSizeError(ProgramClassParser.OnSizeErrorContext context) {
            _exit();
        }
        public override void EnterNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
            _enter(new Node(AsCodeElement(context.NotOnSizeErrorCondition())));
        }
        public override void ExitNoSizeError(ProgramClassParser.NoSizeErrorContext context) {
            _exit();
        }

        public override void EnterOnAtEnd(ProgramClassParser.OnAtEndContext context) {
            _enter(new Node(AsCodeElement(context.AtEndCondition())));
        }
        public override void ExitOnAtEnd(ProgramClassParser.OnAtEndContext context) {
            _exit();
        }
        public override void EnterNoAtEnd(ProgramClassParser.NoAtEndContext context) {
            _enter(new Node(AsCodeElement(context.NotAtEndCondition())));
        }
        public override void ExitNoAtEnd(ProgramClassParser.NoAtEndContext context) {
            _exit();
        }

        public override void EnterOnException(ProgramClassParser.OnExceptionContext context) {
            _enter(new Node(AsCodeElement(context.OnExceptionCondition())));
        }
        public override void ExitOnException(ProgramClassParser.OnExceptionContext context) {
            _exit();
        }
        public override void EnterNoException(ProgramClassParser.NoExceptionContext context) {
            _enter(new Node(AsCodeElement(context.NotOnExceptionCondition())));
        }
        public override void ExitNoException(ProgramClassParser.NoExceptionContext context) {
            _exit();
        }

        public override void EnterOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
            _enter(new Node(AsCodeElement(context.InvalidKeyCondition())));
        }
        public override void ExitOnInvalidKey(ProgramClassParser.OnInvalidKeyContext context) {
            _exit();
        }
        public override void EnterNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
            _enter(new Node(AsCodeElement(context.NotInvalidKeyCondition())));
        }
        public override void ExitNoInvalidKey(ProgramClassParser.NoInvalidKeyContext context) {
            _exit();
        }

        public override void EnterOnOverflow(ProgramClassParser.OnOverflowContext context) {
            _enter(new Node(AsCodeElement(context.OnOverflowCondition())));
        }
        public override void ExitOnOverflow(ProgramClassParser.OnOverflowContext context) {
            _exit();
        }
        public override void EnterNoOverflow(ProgramClassParser.NoOverflowContext context) {
            _enter(new Node(AsCodeElement(context.NotOnOverflowCondition())));
        }
        public override void ExitNoOverflow(ProgramClassParser.NoOverflowContext context) {
            _exit();
        }





        private CodeElement AsCodeElement(Antlr4.Runtime.Tree.ITerminalNode node) {
            return node != null? (CodeElement)node.Symbol : null;
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
