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
    public class ProgramClassBuilder : CobolProgramClassBaseListener
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

        private SymbolTable TableOfExternals = new SymbolTable(null, SymbolTable.Scope.External);
        private SymbolTable TableOfGlobals;

		public SymbolTable CustomSymbols {
			private get { throw new System.InvalidOperationException(); }
			set {
				if (value != null)
					foreach(var type in value.CustomTypes.Values)
						TableOfExternals.RegisterCustomType(type);
System.Console.Error.WriteLine("ProgramClassBuilder: "+(TableOfExternals != null ? TableOfExternals.CustomTypes.Count.ToString() : "?")+" types loaded.");
			}
		}

        public ProgramDispatcher Dispatcher { get; internal set; }

		private void _add(Node node) {
			node.SymbolTable = CurrentProgram.SymbolTable;
			Program.SyntaxTree.Add(node);
		}
		private void _enter(CodeElement e, ParserRuleContext context) {
			_enter(new Node(e));
			if (e!=null) Dispatcher.OnCodeElement(e, context, CurrentProgram);
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
        public override void EnterCobolCompilationUnit(CobolProgramClassParser.CobolCompilationUnitContext context)
        {
            TableOfGlobals = new SymbolTable(TableOfExternals, SymbolTable.Scope.Global);
            Program = null;
            Class = null;
        }

        public override void EnterCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
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

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context) {
            var end = AsCodeElement(context.ProgramEnd());
            if (end != null) _add(new Node(end));
            _exit();
            programsStack.Pop();
        }

        public override void EnterEnvironmentDivision(CobolProgramClassParser.EnvironmentDivisionContext context) {
            _enter(new Node(AsCodeElement(context.EnvironmentDivisionHeader())));
        }
        public override void ExitEnvironmentDivision(CobolProgramClassParser.EnvironmentDivisionContext context) {
            _exit();
        }

        public override void EnterConfigurationSection(CobolProgramClassParser.ConfigurationSectionContext context) {
            _enter(new Node(AsCodeElement(context.ConfigurationSectionHeader())));
            IList<CodeElement> paragraphs;
            paragraphs = AsCodeElements(context.SourceComputerParagraph());
            foreach(var p in paragraphs) _add(new Node(p)); // should be 0 or 1
            paragraphs = AsCodeElements(context.ObjectComputerParagraph());
            foreach(var p in paragraphs) _add(new Node(p)); // should be 0 or 1
            paragraphs = AsCodeElements(context.SpecialNamesParagraph());
            foreach(var p in paragraphs) _add(new Node(p)); // should be 0 or 1
            paragraphs = AsCodeElements(context.RepositoryParagraph());
            foreach(var p in paragraphs) _add(new Node(p)); // should be 0 or 1
        }
        public override void ExitConfigurationSection(CobolProgramClassParser.ConfigurationSectionContext context) {
            _exit();
        }

        public override void EnterDataDivision(CobolProgramClassParser.DataDivisionContext context) {
            _enter(new Node(AsCodeElement(context.DataDivisionHeader())));
        }
        public override void ExitDataDivision(CobolProgramClassParser.DataDivisionContext context) {
            _exit();
        }

        public override void EnterWorkingStorageSection(CobolProgramClassParser.WorkingStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.WorkingStorageSectionHeader(), entries);
        }

        public override void EnterLocalStorageSection(CobolProgramClassParser.LocalStorageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LocalStorageSectionHeader(), entries);
        }

        public override void EnterLinkageSection(CobolProgramClassParser.LinkageSectionContext context) {
            var entries = CreateDataDescriptionEntries(context.DataDescriptionEntry());
            AddStorageNode(context.LinkageSectionHeader(), entries);
        }

		private void AddStorageNode(Antlr4.Runtime.Tree.ITerminalNode terminal, IList<DataDescriptionEntry> entries) {
			var node = new Node(AsCodeElement(terminal));
			_enter(node);
			AddEntries(node, entries);
			_exit();
		}
		private void AddEntries(Node root, IList<DataDescriptionEntry> entries) {
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
				var customTypeDescription = ComputeType(data, currencies);
				if (data.IsGroup) groups.Push(data);

				if (!data.IsTypeDefinitionPart) {
					CurrentProgram.SymbolTable.Add(data);
					if (customTypeDescription != null) {
						foreach(var sub in customTypeDescription.Subordinates) {
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
			}
			return result;
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
				if (data.LevelNumber <= toplevel.LevelNumber) groups.Pop();
				else {
					toplevel.Subordinates.Add(data);
					data.TopLevel = toplevel;
					updated = true;
				}
			}
			return updated;
		}

		/// <summary>Update the toplevel data of a given data description.</summary>
		/// <param name="data">Data description to update</param>
		/// <param name="currencies">Currency characters, used to know if data is numeric or numeric edited</param>
		/// <returns>Representation of the corresponding TYPEDEF if data is of a custom TYPE, or null if data type is unknown of from COBOL standard.</returns>
		private DataDescriptionEntry ComputeType(DataDescriptionEntry data, char[] currencies) {
			if (data.DataType != null) return null;
			if (data.Picture == null) {
				data.DataType = DataType.Unknown;
				return null;
			}
			try {
				var customTypeGroup = CurrentProgram.SymbolTable.GetCustomType(data.Picture);
				data.DataType = customTypeGroup.DataType;
				return customTypeGroup;
			} catch(ArgumentException ex) {
				data.DataType = DataType.Create(data.Picture, currencies);
				return null;
			}
		}
		private DataDescriptionEntry GetCustomType(string name) {
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

        public override void EnterProcedureDivision(CobolProgramClassParser.ProcedureDivisionContext context) {
            _enter(new Node(AsCodeElement(context.ProcedureDivisionHeader())));
        }
        public override void ExitProcedureDivision(CobolProgramClassParser.ProcedureDivisionContext context) {
            _exit();
        }

        public override void EnterSection(CobolProgramClassParser.SectionContext context) {
            var terminal = context.SectionHeader();
            if (terminal == null) terminal = context.ParagraphHeader();
            // if we _enter(..) a node here, it will be detached by ExitParagraph
            // if we do not, no need to detach anything in ExitSection
            if (terminal != null) _enter(new Node(AsCodeElement(terminal)));
        }

        public override void EnterParagraph(CobolProgramClassParser.ParagraphContext context) {
            if (Program.SyntaxTree.Head().CodeElement is ParagraphHeader) _exit();
            _enter(new Node(AsCodeElement(context.ParagraphHeader())));
        }
        public override void ExitParagraph(CobolProgramClassParser.ParagraphContext context) {
            _exit();
        }

        public override void EnterSentence(CobolProgramClassParser.SentenceContext context) {
            _enter(new Node(null));
        }
        public override void ExitSentence(CobolProgramClassParser.SentenceContext context) {
            var end = AsCodeElement(context.SentenceEnd());
            if (end != null) _add(new Node(end));
            _exit();
        }

        public override void EnterStatement(CobolProgramClassParser.StatementContext context) {
            CodeElement statement = AsStatement(context);
            _enter(statement, context);
        }
        public override void ExitStatement(CobolProgramClassParser.StatementContext context) {
            _exit();
        }



        public override void EnterIfStatementWithBody(CobolProgramClassParser.IfStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.IfStatement())));
            _enter(new Node(null));//THEN
        }
        public override void EnterElseClause(CobolProgramClassParser.ElseClauseContext context) {
            _exit();// we want ELSE to be child of IF, not THEN, so exit THEN
            _enter(new Node(AsCodeElement(context.ElseCondition())));// ELSE
            var next = AsCodeElement(context.NextSentenceStatement());
            if (next != null) _add(new Node(next));
        }
        public override void ExitIfStatementWithBody(CobolProgramClassParser.IfStatementWithBodyContext context) {
            _exit(); // _exit() ELSE (if any) or THEN
            var end = AsCodeElement(context.IfStatementEnd());
            if (end != null) _add(new Node(end));
            // don't _exit() IF node because this will be done in ExitStatement
        }


        public override void EnterEvaluateStatementWithBody(CobolProgramClassParser.EvaluateStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.EvaluateStatement())));
        }
        public override void EnterWhenConditionClause(CobolProgramClassParser.WhenConditionClauseContext context) {
            _enter(new Node(null)); // WHEN group
            var nodes = new List<Node>();
            foreach(var condition in context.WhenCondition()) {
                var node = new Node(AsCodeElement(condition));
                nodes.Add(node);
                CurrentProgram.SyntaxTree.Add(node);
            }
            CurrentProgram.SyntaxTree.Push(nodes[nodes.Count-1]);
        }
        public override void ExitWhenConditionClause(CobolProgramClassParser.WhenConditionClauseContext context) {
            _exit(); // last WHEN
            _exit(); // WHEN group
        }
        public override void EnterWhenOtherClause(CobolProgramClassParser.WhenOtherClauseContext context) {
            _enter(new Node(AsCodeElement(context.WhenOtherCondition())));
        }
        public override void ExitWhenOtherClause(CobolProgramClassParser.WhenOtherClauseContext context) {
            _exit();
        }
        public override void ExitEvaluateStatementWithBody(CobolProgramClassParser.EvaluateStatementWithBodyContext context) {
            ExitConditionalStatement(context.EvaluateStatementEnd());
        }


        public override void EnterPerformStatementWithBody(CobolProgramClassParser.PerformStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.PerformStatement())));
        }
        public override void ExitPerformStatementWithBody(CobolProgramClassParser.PerformStatementWithBodyContext context) {
            ExitConditionalStatement(context.PerformStatementEnd());
        }

        public override void EnterSearchStatementWithBody(CobolProgramClassParser.SearchStatementWithBodyContext context) {
            _del();// delete the node we attached in EnterStatement
            _enter(new Node(AsCodeElement(context.SearchStatement())));
        }
        public override void EnterWhenSearchConditionClause(CobolProgramClassParser.WhenSearchConditionClauseContext context) {
            _enter(new Node(AsCodeElement(context.WhenCondition())));
            var next = AsCodeElement(context.NextSentenceStatement());
            if (next != null) _add(new Node(next));
        }
        public override void ExitWhenSearchConditionClause(CobolProgramClassParser.WhenSearchConditionClauseContext context) {
            _exit(); // WHEN
        }
        public override void ExitSearchStatementWithBody(CobolProgramClassParser.SearchStatementWithBodyContext context) {
            ExitConditionalStatement(context.SearchStatementEnd());
        }


        public override void EnterAddStatementConditional(CobolProgramClassParser.AddStatementConditionalContext context) {
            EnterConditionalStatement(context.AddStatement());
        }
        public override void ExitAddStatementConditional(CobolProgramClassParser.AddStatementConditionalContext context) {
            ExitConditionalStatement(context.AddStatementEnd());
        }
        public override void EnterComputeStatementConditional(CobolProgramClassParser.ComputeStatementConditionalContext context) {
            EnterConditionalStatement(context.ComputeStatement());
        }
        public override void ExitComputeStatementConditional(CobolProgramClassParser.ComputeStatementConditionalContext context) {
            ExitConditionalStatement(context.ComputeStatementEnd());
        }
        public override void EnterDivideStatementConditional(CobolProgramClassParser.DivideStatementConditionalContext context) {
            EnterConditionalStatement(context.DivideStatement());
        }
        public override void ExitDivideStatementConditional(CobolProgramClassParser.DivideStatementConditionalContext context) {
            ExitConditionalStatement(context.DivideStatementEnd());
        }
        public override void EnterMultiplyStatementConditional(CobolProgramClassParser.MultiplyStatementConditionalContext context) {
            EnterConditionalStatement(context.MultiplyStatement());
        }
        public override void ExitMultiplyStatementConditional(CobolProgramClassParser.MultiplyStatementConditionalContext context) {
            ExitConditionalStatement(context.MultiplyStatementEnd());
        }
        public override void EnterSubtractStatementConditional(CobolProgramClassParser.SubtractStatementConditionalContext context) {
            EnterConditionalStatement(context.SubtractStatement());
        }
        public override void ExitSubtractStatementConditional(CobolProgramClassParser.SubtractStatementConditionalContext context) {
            ExitConditionalStatement(context.SubtractStatementEnd());
        }

        public override void EnterDeleteStatementConditional(CobolProgramClassParser.DeleteStatementConditionalContext context) {
            EnterConditionalStatement(context.DeleteStatement());
        }
        public override void ExitDeleteStatementConditional(CobolProgramClassParser.DeleteStatementConditionalContext context) {
            ExitConditionalStatement(context.DeleteStatementEnd());
        }
        public override void EnterReadStatementConditional(CobolProgramClassParser.ReadStatementConditionalContext context) {
            EnterConditionalStatement(context.ReadStatement());
        }
        public override void ExitReadStatementConditional(CobolProgramClassParser.ReadStatementConditionalContext context) {
            ExitConditionalStatement(context.ReadStatementEnd());
        }
        public override void EnterWriteStatementConditional(CobolProgramClassParser.WriteStatementConditionalContext context) {
            EnterConditionalStatement(context.WriteStatement());
        }
        public override void ExitWriteStatementConditional(CobolProgramClassParser.WriteStatementConditionalContext context) {
            ExitConditionalStatement(context.WriteStatementEnd());
        }
        public override void EnterRewriteStatementConditional(CobolProgramClassParser.RewriteStatementConditionalContext context) {
            EnterConditionalStatement(context.RewriteStatement());
        }
        public override void ExitRewriteStatementConditional(CobolProgramClassParser.RewriteStatementConditionalContext context) {
            ExitConditionalStatement(context.RewriteStatementEnd());
        }
        public override void EnterStartStatementConditional(CobolProgramClassParser.StartStatementConditionalContext context) {
            EnterConditionalStatement(context.StartStatement());
        }
        public override void ExitStartStatementConditional(CobolProgramClassParser.StartStatementConditionalContext context) {
            ExitConditionalStatement(context.StartStatementEnd());
        }
        public override void EnterReturnStatementConditional(CobolProgramClassParser.ReturnStatementConditionalContext context) {
            EnterConditionalStatement(context.ReturnStatement());
        }
        public override void ExitReturnStatementConditional(CobolProgramClassParser.ReturnStatementConditionalContext context) {
            ExitConditionalStatement(context.ReturnStatementEnd());
        }

        public override void EnterStringStatementConditional(CobolProgramClassParser.StringStatementConditionalContext context) {
            EnterConditionalStatement(context.StringStatement());
        }
        public override void ExitStringStatementConditional(CobolProgramClassParser.StringStatementConditionalContext context) {
            ExitConditionalStatement(context.StringStatementEnd());
        }
        public override void EnterUnstringStatementConditional(CobolProgramClassParser.UnstringStatementConditionalContext context) {
            EnterConditionalStatement(context.UnstringStatement());
        }
        public override void ExitUnstringStatementConditional(CobolProgramClassParser.UnstringStatementConditionalContext context) {
            ExitConditionalStatement(context.UnstringStatementEnd());
        }

        public override void EnterCallStatementConditional(CobolProgramClassParser.CallStatementConditionalContext context) {
            EnterConditionalStatement(context.CallStatement());
        }
        public override void ExitCallStatementConditional(CobolProgramClassParser.CallStatementConditionalContext context) {
            ExitConditionalStatement(context.CallStatementEnd());
        }
        public override void EnterInvokeStatementConditional(CobolProgramClassParser.InvokeStatementConditionalContext context) {
            EnterConditionalStatement(context.InvokeStatement());
        }
        public override void ExitInvokeStatementConditional(CobolProgramClassParser.InvokeStatementConditionalContext context) {
            ExitConditionalStatement(context.InvokeStatementEnd());
        }
        public override void EnterXmlGenerateStatementConditional(CobolProgramClassParser.XmlGenerateStatementConditionalContext context) {
            EnterConditionalStatement(context.XmlGenerateStatement());
        }
        public override void ExitXmlGenerateStatementConditional(CobolProgramClassParser.XmlGenerateStatementConditionalContext context) {
            ExitConditionalStatement(context.XmlStatementEnd());
        }
        public override void EnterXmlParseStatementConditional(CobolProgramClassParser.XmlParseStatementConditionalContext context) {
            EnterConditionalStatement(context.XmlParseStatement());
        }
        public override void ExitXmlParseStatementConditional(CobolProgramClassParser.XmlParseStatementConditionalContext context) {
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

        public override void EnterOnSizeError(CobolProgramClassParser.OnSizeErrorContext context) {
            _enter(new Node(AsCodeElement(context.OnSizeErrorCondition())));
        }
        public override void ExitOnSizeError(CobolProgramClassParser.OnSizeErrorContext context) {
            _exit();
        }
        public override void EnterNoSizeError(CobolProgramClassParser.NoSizeErrorContext context) {
            _enter(new Node(AsCodeElement(context.NotOnSizeErrorCondition())));
        }
        public override void ExitNoSizeError(CobolProgramClassParser.NoSizeErrorContext context) {
            _exit();
        }

        public override void EnterOnAtEnd(CobolProgramClassParser.OnAtEndContext context) {
            _enter(new Node(AsCodeElement(context.AtEndCondition())));
        }
        public override void ExitOnAtEnd(CobolProgramClassParser.OnAtEndContext context) {
            _exit();
        }
        public override void EnterNoAtEnd(CobolProgramClassParser.NoAtEndContext context) {
            _enter(new Node(AsCodeElement(context.NotAtEndCondition())));
        }
        public override void ExitNoAtEnd(CobolProgramClassParser.NoAtEndContext context) {
            _exit();
        }

        public override void EnterOnException(CobolProgramClassParser.OnExceptionContext context) {
            _enter(new Node(AsCodeElement(context.OnExceptionCondition())));
        }
        public override void ExitOnException(CobolProgramClassParser.OnExceptionContext context) {
            _exit();
        }
        public override void EnterNoException(CobolProgramClassParser.NoExceptionContext context) {
            _enter(new Node(AsCodeElement(context.NotOnExceptionCondition())));
        }
        public override void ExitNoException(CobolProgramClassParser.NoExceptionContext context) {
            _exit();
        }

        public override void EnterOnInvalidKey(CobolProgramClassParser.OnInvalidKeyContext context) {
            _enter(new Node(AsCodeElement(context.InvalidKeyCondition())));
        }
        public override void ExitOnInvalidKey(CobolProgramClassParser.OnInvalidKeyContext context) {
            _exit();
        }
        public override void EnterNoInvalidKey(CobolProgramClassParser.NoInvalidKeyContext context) {
            _enter(new Node(AsCodeElement(context.NotInvalidKeyCondition())));
        }
        public override void ExitNoInvalidKey(CobolProgramClassParser.NoInvalidKeyContext context) {
            _exit();
        }

        public override void EnterOnOverflow(CobolProgramClassParser.OnOverflowContext context) {
            _enter(new Node(AsCodeElement(context.OnOverflowCondition())));
        }
        public override void ExitOnOverflow(CobolProgramClassParser.OnOverflowContext context) {
            _exit();
        }
        public override void EnterNoOverflow(CobolProgramClassParser.NoOverflowContext context) {
            _enter(new Node(AsCodeElement(context.NotOnOverflowCondition())));
        }
        public override void ExitNoOverflow(CobolProgramClassParser.NoOverflowContext context) {
            _exit();
        }





        private CodeElement AsCodeElement(Antlr4.Runtime.Tree.ITerminalNode node) {
            return node != null? (CodeElement)node.Symbol : null;
        }
        private IList<CodeElement> AsCodeElements(Antlr4.Runtime.Tree.ITerminalNode[] nodes) {
            var list = new List<CodeElement>();
            foreach(var node in nodes) {
                var e = AsCodeElement(node);
                if (e != null)
                    list.Add(e);
            }
            return list;
        }

        private CodeElement AsStatement(CobolProgramClassParser.StatementContext context)
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
