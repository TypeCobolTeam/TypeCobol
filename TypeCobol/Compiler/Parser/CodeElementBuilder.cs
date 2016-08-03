using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.CodeElements.Symbols;
using TypeCobol.Compiler.CodeElements.Functions;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    ///     Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CodeElementBuilder : CodeElementsBaseListener
    {
        private CodeElement _ce;
        private ParserRuleContext Context;

        /// <summary>CodeElement object resulting of the visit the parse tree</summary>
        public CodeElement CodeElement {
            get { return _ce; }
            private set {
                _ce = value;
                if (value != null) Dispatcher.OnCodeElement(value, Context);
            }
        }
        public CodeElementDispatcher Dispatcher { get; internal set; }

		/// <summary>Initialization code run before parsing each new COBOL CodeElement</summary>
		public override void EnterCodeElement(CodeElementsParser.CodeElementContext context) {
			CodeElement = null;
			Context = null;
		}
		/// <summary>Initialization code run before parsing each new TypeCobol CodeElement</summary>
		public override void EnterTcCodeElement(CodeElementsParser.TcCodeElementContext context) {
			CodeElement = null;
			Context = null;
		}

        // Code structure

        // -- Program --

        public override void EnterProgramIdentification(CodeElementsParser.ProgramIdentificationContext context)
        {
            var programIdentification = new ProgramIdentification();

            Token programName = ParseTreeUtils.GetFirstToken(context.programNameDefinition());
            if (programName != null)
            {
                programIdentification.ProgramName = new ProgramName(programName);
            }
			programIdentification.IsCommon = context.COMMON() != null;
			programIdentification.IsInitial = context.INITIAL() != null;
			programIdentification.IsRecursive = context.RECURSIVE() != null;

            Context = context;
            CodeElement = programIdentification;
        }

        public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context)
        {
            var programEnd = new ProgramEnd();

            Token programName = ParseTreeUtils.GetFirstToken(context.programNameReference2());
            if (programName != null)
            {
                programEnd.ProgramName = new ProgramName(programName);
            }

            Context = context;
            CodeElement = programEnd;
        }

        public override void EnterAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context)
        {
            var authoringProperties = new AuthoringProperties();

            if (context.authorParagraph().Length > 0)
            {
                authoringProperties.Author =
                    BuildCommentEntriesProperty(context.authorParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.dateCompiledParagraph().Length > 0)
            {
                authoringProperties.DateCompiled =
                    BuildCommentEntriesProperty(context.dateCompiledParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.dateWrittenParagraph().Length > 0)
            {
                authoringProperties.DateWritten =
                    BuildCommentEntriesProperty(context.dateWrittenParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.installationParagraph().Length > 0)
            {
                authoringProperties.Installation =
                    BuildCommentEntriesProperty(context.installationParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.securityParagraph().Length > 0)
            {
                authoringProperties.Security =
                    BuildCommentEntriesProperty(context.securityParagraph().SelectMany(p => p.CommentEntry()));
            }

            if (CodeElement is ProgramIdentification)
            {
                ((ProgramIdentification) CodeElement).AuthoringProperties = authoringProperties;
            }
            else if (CodeElement is ClassIdentification)
            {
                ((ClassIdentification) CodeElement).AuthoringProperties = authoringProperties;
            }
            else if (CodeElement is MethodIdentification)
            {
                ((MethodIdentification) CodeElement).AuthoringProperties = authoringProperties;
            }
        }

        private SyntaxProperty<string> BuildCommentEntriesProperty(IEnumerable<ITerminalNode> commentEntriesNodes)
        {
            var tokensList = new List<Token>();
            var sbCommentEntries = new StringBuilder();

            bool isFirstLine = true;
            foreach (ITerminalNode commentEntryNode in commentEntriesNodes)
            {
                Token token = ParseTreeUtils.GetTokenFromTerminalNode(commentEntryNode);
                tokensList.Add(token);

                if (isFirstLine)
                {
                    sbCommentEntries.Append(ParseTreeUtils.GetTokenFromTerminalNode(commentEntryNode).Text);
                    isFirstLine = false;
                }
                else
                {
                    sbCommentEntries.AppendLine();
                    sbCommentEntries.Append(ParseTreeUtils.GetTokenFromTerminalNode(commentEntryNode).Text);
                }
            }

            return new SyntaxProperty<string>(sbCommentEntries.ToString(), tokensList);
        }


        // -- Class --

        public override void EnterClassIdentification(CodeElementsParser.ClassIdentificationContext context)
        {
            var classIdentification = new ClassIdentification();
            classIdentification.ClassName = SyntaxElementBuilder.CreateClassName(context.classNameDefinition());
            classIdentification.InheritsFrom = SyntaxElementBuilder.CreateClassName(context.inheritsFromClassName);

            Context = context;
            CodeElement = classIdentification;
        }

        public override void EnterClassEnd(CodeElementsParser.ClassEndContext context)
        {
            var classEnd = new ClassEnd();

            Token className = ParseTreeUtils.GetFirstToken(context.classNameReference());
            if (className != null)
            {
                classEnd.ClassName = new ClassName(className);
            }

            Context = context;
            CodeElement = classEnd;
        }

        public override void EnterFactoryIdentification(CodeElementsParser.FactoryIdentificationContext context)
        {
            Context = context;
            CodeElement = new FactoryIdentification();
        }

        public override void EnterFactoryEnd(CodeElementsParser.FactoryEndContext context)
        {
            Context = context;
            CodeElement = new FactoryEnd();
        }

        public override void EnterObjectIdentification(CodeElementsParser.ObjectIdentificationContext context)
        {
            Context = context;
            CodeElement = new ObjectIdentification();
        }

        public override void EnterObjectEnd(CodeElementsParser.ObjectEndContext context)
        {
            Context = context;
            CodeElement = new ObjectEnd();
        }

        public override void EnterMethodIdentification(CodeElementsParser.MethodIdentificationContext context)
        {
            var methodIdentification = new MethodIdentification();

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodNameDefinition());
            if (methodName != null)
            {
                methodIdentification.MethodName = new MethodName(methodName);
            }

            Context = context;
            CodeElement = methodIdentification;
        }

        public override void EnterMethodEnd(CodeElementsParser.MethodEndContext context)
        {
            var methodEnd = new MethodEnd();

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodNameReference());
            if (methodName != null)
            {
                methodEnd.MethodName = new MethodName(methodName);
            }

            Context = context;
            CodeElement = methodEnd;
        }

        // -- Division --

        public override void EnterEnvironmentDivisionHeader(CodeElementsParser.EnvironmentDivisionHeaderContext context)
        {
            Context = context;
            CodeElement = new EnvironmentDivisionHeader();
        }

        public override void EnterDataDivisionHeader(CodeElementsParser.DataDivisionHeaderContext context)
        {
            Context = context;
            CodeElement = new DataDivisionHeader();
        }

        public override void EnterProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context)
        {
// [TYPECOBOL]
			if (context.inputPhrase() != null || context.outputPhrase() != null) {
				Context = context;
				CodeElement = new FunctionDeclarationProfile();
				return;
			}
// [/TYPECOBOL]

            var procedureDivisionHeader = new ProcedureDivisionHeader();

            if (context.usingPhrase() != null)
            {
                foreach (CodeElementsParser.InputParametersContext inputParametersContext in context.usingPhrase().inputParameters())
                {
                    SyntaxProperty<ReceivingMode> receivingMode = null;
                    if (inputParametersContext.receivingMode() != null)
                    {
                        receivingMode = new SyntaxProperty<ReceivingMode>(
                            inputParametersContext.receivingMode() is CodeElementsParser.ByValueContext
                                ? ReceivingMode.ByValue
                                : ReceivingMode.ByReference,
                            ParseTreeUtils.GetTokensList(inputParametersContext.receivingMode()));
                    }
                    foreach (CodeElementsParser.DataNameReferenceContext dataNameContext in inputParametersContext.dataNameReference())
                    {
                        Token dataName = ParseTreeUtils.GetFirstToken(dataNameContext);
                        var inputParameter = new InputParameter(new DataName(dataName), receivingMode);

                        if (procedureDivisionHeader.UsingParameters == null)
                        {
                            procedureDivisionHeader.UsingParameters = new List<InputParameter>();
                        }
                        procedureDivisionHeader.UsingParameters.Add(inputParameter);
                    }
                }
            }

            Context = context;
            CodeElement = procedureDivisionHeader;
        }

        public override void EnterDeclarativesHeader(CodeElementsParser.DeclarativesHeaderContext context)
        {
            Context = context;
            CodeElement = new DeclarativesHeader();
        }

        public override void EnterDeclarativesEnd(CodeElementsParser.DeclarativesEndContext context)
        {
            Context = context;
            CodeElement = new DeclarativesEnd();
        }

        // -- Section --

        public override void EnterSectionHeader(CodeElementsParser.SectionHeaderContext context)
        {
            var sectionHeader = new SectionHeader();

            Token sectionName = ParseTreeUtils.GetFirstToken(context.sectionNameDefinition());
            if (sectionName != null)
            {
                sectionHeader.SectionName = new SectionName(sectionName);
            }

            Token priorityNumber = ParseTreeUtils.GetFirstToken(context.priorityNumber());
            if (priorityNumber != null)
            {
                sectionHeader.PriorityNumber = new SyntaxNumber(priorityNumber);
            }

            Context = context;
            CodeElement = sectionHeader;
        }

        public override void EnterConfigurationSectionHeader(
            CodeElementsParser.ConfigurationSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new ConfigurationSectionHeader();
        }

        public override void EnterInputOutputSectionHeader(
            CodeElementsParser.InputOutputSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new InputOutputSectionHeader();
        }

        public override void EnterFileSectionHeader(CodeElementsParser.FileSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new FileSectionHeader();
        }

        public override void EnterWorkingStorageSectionHeader(
            CodeElementsParser.WorkingStorageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new WorkingStorageSectionHeader();
        }

        public override void EnterLocalStorageSectionHeader(
            CodeElementsParser.LocalStorageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new LocalStorageSectionHeader();
        }

        public override void EnterLinkageSectionHeader(CodeElementsParser.LinkageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new LinkageSectionHeader();
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            Token paragraphName = ParseTreeUtils.GetFirstToken(context.paragraphNameDefinition());
            if (paragraphName != null)
            {
                paragraphHeader.ParagraphName = new ParagraphName(paragraphName);
            }

            Context = context;
            CodeElement = paragraphHeader;
        }

        public override void EnterFileControlParagraphHeader(
            CodeElementsParser.FileControlParagraphHeaderContext context)
        {
            Context = context;
            CodeElement = new FileControlParagraphHeader();
        }

        public override void EnterIoControlParagraphHeader(
            CodeElementsParser.IoControlParagraphHeaderContext context)
        {
            Context = context;
            CodeElement = new IOControlParagraphHeader();
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CodeElementsParser.SentenceEndContext context)
        {
            Context = context;
            CodeElement = new SentenceEnd();
        }

        // Entries

        // -- Data Division --

		public override void EnterFileDescriptionEntry(CodeElementsParser.FileDescriptionEntryContext context) {
			Context = context;
			CodeElement = new FileDescriptionEntry();
		}

		public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context) {
			if(context.dataRenamesEntry() != null || context.dataConditionEntry() != null) {
				// For levels 66 and 88, the DataDescriptionEntry is created by the following methods
				// - EnterDataRenamesEntry
				// - EnterDataConditionEntry
				return;
			}

			DataDescriptionEntry entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.DataName = SyntaxElementBuilder.CreateDataName(context.dataNameDefinition());
			//entry.IsFiller = (dataname == null || context.FILLER() != null);

			var redefines = context.redefinesClause();
			if (redefines != null)
                entry.RedefinesDataName = SyntaxElementBuilder.CreateDataName(redefines.dataNameReference());

			var picture = DataDescriptionChecker.GetContext(entry, context.pictureClause(), false);
			if (picture != null) entry.Picture = picture.PictureCharacterString().GetText();

			var blank = DataDescriptionChecker.GetContext(entry, context.blankWhenZeroClause(), false);
			entry.IsBlankWhenZero = blank != null && blank.BLANK() != null;
			var external = DataDescriptionChecker.GetContext(entry, context.externalClause(), false);
			entry.IsExternal = external != null && external.EXTERNAL() != null;
			var global = DataDescriptionChecker.GetContext(entry, context.globalClause(), false);
			entry.IsGlobal = global != null && global.GLOBAL() != null;
			var justified = DataDescriptionChecker.GetContext(entry, context.justifiedClause(), false);
			entry.IsJustified = justified != null && (justified.JUSTIFIED() != null || justified.JUST() != null);
			var sync = DataDescriptionChecker.GetContext(entry, context.synchronizedClause(), false);
			entry.IsSynchronized = (sync != null) && (sync.SYNC() != null || sync.SYNCHRONIZED() != null || sync.LEFT() != null || sync.RIGHT() != null);
			var group = DataDescriptionChecker.GetContext(entry, context.groupUsageClause(), false);
			entry.IsGroupUsageNational = group != null && (group.GROUP_USAGE() != null || group.NATIONAL() != null);
			UpdateDataDescriptionEntryWithUsageClause(entry, DataDescriptionChecker.GetContext(entry, context.usageClause(), false));
			UpdateDataDescriptionEntryWithSignClause(entry, DataDescriptionChecker.GetContext(entry, context.signClause(), false));
			UpdateDataDescriptionEntryWithOccursClause(entry, DataDescriptionChecker.GetContext(entry, context.occursClause(), false));
			UpdateDataDescriptionEntryWithValueClause(entry, DataDescriptionChecker.GetContext(entry, context.valueClause(), false));

            // [Cobol 2002]
			entry.IsTypeDefinition = context.cobol2002TypedefClause() != null;
			if (entry.IsTypeDefinition && entry.Name != null) {
				bool strong = context.cobol2002TypedefClause().STRONG() != null;
				entry.DataType = new DataType(entry.Name.Name, strong);
			}

		    var cobol2002TypeClause = DataDescriptionChecker.GetContext(entry, context.cobol2002TypeClause());
			if (cobol2002TypeClause != null) {
				Token token = null;
				if (cobol2002TypeClause.UserDefinedWord() != null)
					token = ParseTreeUtils.GetTokenFromTerminalNode(cobol2002TypeClause.UserDefinedWord());
				if (cobol2002TypeClause.DATE() != null)
					token = ParseTreeUtils.GetTokenFromTerminalNode(cobol2002TypeClause.DATE());
				if (token != null) entry.Picture = "TYPE:"+token.Text.ToUpper();
			}
            // [/Cobol 2002]

            Context = context;
			CodeElement = entry;
		}

		public override void EnterDataRenamesEntry(CodeElementsParser.DataRenamesEntryContext context) {
			DataDescriptionEntry entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.DataName = SyntaxElementBuilder.CreateDataName(context.dataNameDefinition());
            //entry.IsFiller = (dataname == null || context.FILLER() != null);


			var names = SyntaxElementBuilder.CreateQualifiedNames(context.renamesClause().qualifiedDataName());
			if (names.Count > 0) entry.RenamesFromDataName = names[0];
			if (names.Count > 1) entry.RenamesToDataName   = names[1];
			//note: "RENAMES THRU dataname" will yield "from" initialized and "to" uninitialized

			Context = context;
			CodeElement = entry;
		}

		public override void EnterDataConditionEntry(CodeElementsParser.DataConditionEntryContext context) {
			var entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.ConditionName = SyntaxElementBuilder.CreateConditionName(context.conditionNameDefinition());
			entry.IsConditionNameDescription = true;
			UpdateDataDescriptionEntryWithValueClauseForCondition(entry, context.valueClauseForCondition());

			Context = context;
			CodeElement = entry;
		}

        private void UpdateDataDescriptionEntryWithSignClause(DataDescriptionEntry entry, CodeElementsParser.SignClauseContext context)
        {
            if (context == null) return;
            entry.SignPosition = SignPosition.None;
            if (context.TRAILING() != null) entry.SignPosition = SignPosition.Trailing;
            if (context.LEADING()  != null) entry.SignPosition = SignPosition.Leading;
            entry.IsSignSeparate = context.SEPARATE() != null;
        }

        private void UpdateDataDescriptionEntryWithUsageClause(DataDescriptionEntry entry, CodeElementsParser.UsageClauseContext context)
        {
            if (context == null) return;
            entry.Usage = CreateDataUsage(context);
            entry.ObjectReference = SyntaxElementBuilder.CreateClassName(context.classNameReference());
        }

        private void UpdateDataDescriptionEntryWithOccursClause(DataDescriptionEntry entry, CodeElementsParser.OccursClauseContext context)
        {
            if (context == null) return;
            entry.IsTableOccurence = true;

            bool isVariable = (context.occursDependingOn() != null);
            if (isVariable) {
                entry.OccursDependingOn = SyntaxElementBuilder.CreateDataName(context.occursDependingOn().dataNameReference());
            }
            isVariable = isVariable || (context.UNBOUNDED() != null) || (context.TO() != null);

            var integers = context.IntegerLiteral();
            if (integers != null) {
                isVariable = isVariable || (integers.Length == 2);
                if (integers.Length == 0) {
                    if (isVariable) {
                            // 1) OCCURS UNBOUNDED DEPENDING ON...
                        entry.MinOccurencesCount = 1;
                        entry.MaxOccurencesCount = Int32.MaxValue;
                    }
                    // else;   2) OCCURS ... -syntax error (fixed length, exact missing)
                } else
                if (integers.Length == 1) {
                    if (isVariable) {
                        if (context.UNBOUNDED() != null) {
                            // 3) OCCURS min TO UNBOUNDED DEPENDING ON...
                            // 4) OCCURS min UNBOUNDED DEPENDING ON... -syntax error (TO missing)
                            entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                            entry.NoMaxOccurencesCount = true;
                        } else {
                            // 5) OCCURS max DEPENDING ON...
                            // 6) OCCURS min TO DEPENDING ON... -syntax error (max missing)
                            // 7) OCCURS TO max DEPENDING ON... -syntax error (min missing)
                            // WARNING! due to our grammar, we cannot discriminate between 6) and 7)
                            // this shouldn't be a problem as both cases are syntax errors
                            entry.MinOccurencesCount = 1;
                            entry.MaxOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                        }
                    } else {
                            // 8) OCCURS exact ... (fixed length)
                        entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                        entry.MaxOccurencesCount = entry.MinOccurencesCount;
                    }
                } else { // isVariable == true && integers.Length == 2
                            // 9) OCCURS min TO max DEPENDING ON...
                            //10) OCCURS min max DEPENDING ON... -syntax error (TO missing)
                    entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                    entry.MaxOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[1]);
                }
            }

            var keys = context.occursKeys();
            if (keys != null) {
                entry.TableOccurenceKeys = new List<DataName>();
                entry.TableOccurenceKeyDirections = new List<KeyDirection>();
                foreach(var key in keys) {
                    var direction = KeyDirection.None;
                    if (key.ASCENDING()  != null) direction = KeyDirection.Ascending;
                    if (key.DESCENDING() != null) direction = KeyDirection.Descending;
                    foreach(var name in key.dataNameReference()) {
                        var data = SyntaxElementBuilder.CreateDataName(name);
                        if (data == null) continue;
                        entry.TableOccurenceKeys.Add(data);
                        entry.TableOccurenceKeyDirections.Add(direction);
                    }
                }
            }

            var indexes = context.indexNameDefinition();
            if (indexes != null) {
                entry.IndexedBy = new List<IndexName>();
                foreach(var index in indexes) entry.IndexedBy.Add(SyntaxElementBuilder.CreateIndexName(index));
            }
        }

        private DataUsage CreateDataUsage(CodeElementsParser.UsageClauseContext context)
        {
            if (context.BINARY() != null
             || context.COMP()   != null || context.COMPUTATIONAL()   != null
             || context.COMP_4() != null || context.COMPUTATIONAL_4() != null) return DataUsage.Binary;
            if (context.COMP_1() != null || context.COMPUTATIONAL_1() != null) return DataUsage.FloatingPöint;
            if (context.COMP_2() != null || context.COMPUTATIONAL_2() != null) return DataUsage.LongFloatingPöint;
            if (context.PACKED_DECIMAL() != null
             || context.COMP_3() != null || context.COMPUTATIONAL_3() != null) return DataUsage.PackedDecimal;
            if (context.COMP_5() != null || context.COMPUTATIONAL_5() != null) return DataUsage.NativeBinary;
            if (context.DISPLAY_ARG() != null) return DataUsage.Display;
            if (context.DISPLAY_1()   != null) return DataUsage.DBCS;
            if (context.INDEX() != null) return DataUsage.Index;
            if (context.NATIONAL() != null) return DataUsage.National;
            if (context.OBJECT() != null || context.REFERENCE() != null) return DataUsage.ObjectReference;
            if (context.POINTER() != null) return DataUsage.Pointer;
            if (context.FUNCTION_POINTER()  != null) return DataUsage.FunctionPointer;
            if (context.PROCEDURE_POINTER() != null) return DataUsage.ProcedurePointer;
            return DataUsage.None;
        }

        private void UpdateDataDescriptionEntryWithValueClause(DataDescriptionEntry entry, CodeElementsParser.ValueClauseContext context)
        {
            if (context == null) return;
            var value = context.literal();
            if (value != null) entry.InitialValue = SyntaxElementBuilder.CreateLiteral(value); // format 1
            entry.IsInitialValueNull = (context.NULL() != null || context.NULLS() != null); // format 3
        }

        private void UpdateDataDescriptionEntryWithValueClauseForCondition(DataDescriptionEntry entry, CodeElementsParser.ValueClauseForConditionContext context)
        {
            if (context == null) return;
            var values = context.literal();
            if (values.Length > 0) entry.InitialValue = SyntaxElementBuilder.CreateLiteral(values[0]); // format 1 and 2
            if (values.Length > 1) entry.ThroughValue = SyntaxElementBuilder.CreateLiteral(values[1]); // format 2
        }

        // -- InputOutput Section --

        public override void EnterFileControlEntry(CodeElementsParser.FileControlEntryContext context)
        {
            Context = context;
            CodeElement = new FileControlEntry();
        }

        public override void EnterIoControlEntry(CodeElementsParser.IoControlEntryContext context)
        {
            Context = context;
            CodeElement = new IOControlEntry();
        }

        // Paragraphs

        // --Configuration Section --

        public override void EnterSourceComputerParagraph(CodeElementsParser.SourceComputerParagraphContext context)
        {
            Context = context;
            CodeElement = new SourceComputerParagraph();
        }

        public override void EnterObjectComputerParagraph(CodeElementsParser.ObjectComputerParagraphContext context)
        {
            Context = context;
            CodeElement = new ObjectComputerParagraph();
        }

		public override void EnterSpecialNamesParagraph(CodeElementsParser.SpecialNamesParagraphContext context) {
			var paragraph = new SpecialNamesParagraph();
			foreach(var clause in context.currencySignClause())
				CreateCurrencySign(paragraph, clause);

			Context = context;
			CodeElement = paragraph;
		}
		public void CreateCurrencySign(SpecialNamesParagraph paragraph, CodeElementsParser.CurrencySignClauseContext context) {
			var currencies = context.alphanumOrHexadecimalLiteral();
			string currencyStr = null;
			string currencyChar = "$";
			if (currencies.Length > 0)
				currencyStr = SyntaxElementBuilder.CreateString(currencies[0]);
			if (currencies.Length > 1)
				currencyChar = SyntaxElementBuilder.CreateString(currencies[1]);
			paragraph.CurrencySymbols[currencyChar] = currencyStr;
		}

        public override void EnterRepositoryParagraph(CodeElementsParser.RepositoryParagraphContext context)
        {
            Context = context;
            CodeElement = new RepositoryParagraph();
        }

        // Statements

        public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateAcceptStatement(context);
        }



          ///////////////////////////
         // ARITHMETIC STATEMENTS //
        ///////////////////////////

        public override void EnterAddStatementFormat1(CodeElementsParser.AddStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat2(CodeElementsParser.AddStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            if (context.GIVING() != null)
            {
                builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                    context.identifierRounded());
            }

            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat3(CodeElementsParser.AddStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context)
        {
            Context = context;
            CodeElement = new AddStatementEnd();
        }

        public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context)
        {
            Context = context;
            CodeElement = new ComputeStatementBuilder().CreateComputeStatement(context);
        }
        public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context)
        {
            Context = context;
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context)
        {
            Context = context;
            CodeElement = new DivideStatementBuilder().CreateStatement(context);
        }
        public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context)
        {
            Context = context;
            CodeElement = new DivideStatementEnd();
        }



        public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context)
        {
            var statement = new AlterStatement();
            // context.procedureName().Length %2 != 0 can never happen outside of syntax errors
            AlterStatement.Alter alter = null;
            foreach (var p in context.procedureName())
            {
                if (alter == null) {
                    alter = new AlterStatement.Alter();
                    alter.Procedure1 = SyntaxElementBuilder.CreateProcedureName(p);
                } else {
                    alter.Procedure2 = SyntaxElementBuilder.CreateProcedureName(p);
                    statement.Items.Add(alter);
                    alter = null;
                }
            }
            Context = context;
            CodeElement = statement;
        }

        public override void EnterCallStatement(CodeElementsParser.CallStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateCallStatement(context);
        }
        public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context)
        {
            Context = context;
            CodeElement = new CallStatementEnd();
        }

        public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context)
        {
            var statement = new CancelStatement();
            if (context.programNameReference1() != null)
            {
                foreach (var c in context.programNameReference1())
                {
                    if (c.alphanumericLiteral() != null)
                    {
                        var item = SyntaxElementBuilder.CreateLiteral(c.alphanumericLiteral());
                        statement.Items.Add(item);
                    }
                }
            }
            if (context.programNameFromData() != null)
            {
                foreach (var c in context.programNameFromData())
                {
                    if (c.identifier() != null)
                    {
                        var item = SyntaxElementBuilder.CreateIdentifier(c.identifier());
                        statement.Items.Add(item);
                    }
                }
            }
            Context = context;
            CodeElement = statement;
        }

        public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context)
        {
            Context = context;
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context)
        {
            var statement = new DeleteStatement();
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileNameReference());
            Context = context;
            CodeElement = statement;
        }
        public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context)
        {
            Context = context;
            CodeElement = new DeleteStatementEnd();
        }

        /// <summary>
        ///     Create a MnemonicOrEnvironmentName from a token.
        ///     This method first check if the token match an environment name from EnvironmentNameEnum
        ///     If so, it's an EnvironmentName
        ///     otherwise, it's a mnemonic environment name
        /// </summary>
        /// <param name="mnemonicOrEnvironmentName">a token corresponding to environment or a mnemonic environment name</param>
        /// <returns>A MnemonicOrEnvironmentName of the correct CodeElementType: EnvironmentName or MnemonicForEnvironmentName</returns>
        public static MnemonicOrEnvironmentName CreateMnemonicOrEnvironmentName(Token mnemonicOrEnvironmentName)
        {
            EnvironmentNameEnum envNameValue;
            if (Enum.TryParse(mnemonicOrEnvironmentName.Text, true, out envNameValue))
            {
                return new EnvironmentName(mnemonicOrEnvironmentName, envNameValue);
            }
            //if this happens, it means it's a mnemonic environment name
            return new MnemonicForEnvironmentName(mnemonicOrEnvironmentName);
        }

        public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context)
        {
            var statement = new DisplayStatement();

            //Identifiers & literals
            if (context.identifierOrLiteral() != null)
            {
                var expressions = new List<Expression>();
                foreach (CodeElementsParser.IdentifierOrLiteralContext idOrLiteral in context.identifierOrLiteral())
                {
                    Expression identifier = CreateIdentifierOrLiteral(idOrLiteral, statement, "Display");
                    if (identifier != null)
                    {
                        expressions.Add(identifier);
                    }
                }
                statement.IdentifierOrLiteral = expressions;
            }
            //else don't set the displayStement. It will remains null

            //(mnemonic) Environment name
            if (context.uponEnvironmentName() != null)
            {
                Token mnemonicOrEnvironmentName = ParseTreeUtils.GetFirstToken(context.uponEnvironmentName().mnemonicForEnvironmentNameReferenceOrEnvironmentName());
                if (mnemonicOrEnvironmentName != null)
                {
                    statement.UponMnemonicOrEnvironmentName = new MnemonicOrEnvironmentName(mnemonicOrEnvironmentName);
//            EnvironmentNameEnum envNameValue;
//            if (Enum.TryParse(mnemonicOrEnvironmentName.Text, true, out envNameValue))
//            {
//                return new EnvironmentName(mnemonicOrEnvironmentName, envNameValue);
//            }
//            else
//            {
//                //if this happens, it means it's a mnemonic environment name
//                return new MnemonicForEnvironmentName(mnemonicOrEnvironmentName);
//            }
                }
            } //else don't set UponMnemonicOrEnvironmentName. it will remains null

			statement.IsWithNoAdvancing = context.withNoAdvancing() != null;

            Context = context;
            CodeElement = statement;
        }

        /// <summary>
        /// </summary>
        /// <param name="idOrLiteral"></param>
        /// <param name="statement">Only used in case of error to link the error with the current statement</param>
        /// <param name="statementName">Only used in case of error to have the name of the current statement</param>
        /// <returns></returns>
        public Expression CreateIdentifierOrLiteral(CodeElementsParser.IdentifierOrLiteralContext idOrLiteral, CodeElement statement,
            string statementName)
        {
            if (idOrLiteral.identifier() != null)
            {
                return SyntaxElementBuilder.CreateIdentifier(idOrLiteral.identifier());
            }
            if (idOrLiteral.literal() != null)
            {
                return SyntaxElementBuilder.CreateLiteral(idOrLiteral.literal());
            }
                //TODO manage figurativeConstant here or as a literal ?

            DiagnosticUtils.AddError(statement, statementName + ": required <identifier> or <literal>", idOrLiteral);
            return null;
        }

        public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context)
        {
            var statement = new EntryStatement();
            if (context.programEntryDefinition() != null)
            {
                statement.ProgramName = SyntaxElementBuilder.CreateLiteral(context.programEntryDefinition().alphanumericLiteral());
            }
            foreach(var by in context.byReferenceOrByValueIdentifiers()) {
                var u = new EntryStatement.Using<Identifier>();
                var identifiers = SyntaxElementBuilder.CreateIdentifiers(by.identifier());
                foreach (var i in identifiers) u.Add(i);
                u.ByValue = by.VALUE() != null;
                statement.Usings.Add(u);
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context)
        {
            var statement = new ExecStatement();
            Token node = null;
            if (context.execTranslatorName() != null)
            {
                node = ParseTreeUtils.GetTokenFromTerminalNode(context.execTranslatorName().ExecTranslatorName());
            }
            if (node != null) statement.Compiler = node.Text;
            var str = new StringBuilder();
            foreach (var line in context.ExecStatementText())
            {
                node = ParseTreeUtils.GetTokenFromTerminalNode(line);
                if (node != null) str.Append(node.Text);
            }
            statement.Code = str.ToString();

            Context = context;
            CodeElement = statement;
        }

        public override void EnterExitMethodStatement(CodeElementsParser.ExitMethodStatementContext context)
        {
            Context = context;
            CodeElement = new ExitMethodStatement();
        }

        public override void EnterExitProgramStatement(CodeElementsParser.ExitProgramStatementContext context)
        {
            Context = context;
            CodeElement = new ExitProgramStatement();
        }

        public override void EnterExitStatement(CodeElementsParser.ExitStatementContext context)
        {
            Context = context;
            CodeElement = new ExitStatement();
        }

        public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context)
        {
            Context = context;
            CodeElement = new GobackStatement();
        }

        public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateGotoStatement(context);
        }


        public override void EnterIfStatement(CodeElementsParser.IfStatementContext context)
        {
            var statement = new IfStatement();
            if (context.conditionalExpression() != null) {
                statement.condition = new LogicalExpressionBuilder().createCondition(context.conditionalExpression());
            }
            Context = context;
            CodeElement = statement;
        }

        public override void EnterElseCondition(CodeElementsParser.ElseConditionContext context)
        {
            Context = context;
            CodeElement = new ElseCondition();
        }

        public override void EnterIfStatementEnd(CodeElementsParser.IfStatementEndContext context)
        {
            Context = context;
            CodeElement = new IfStatementEnd();
        }


        public override void EnterEvaluateStatement(CodeElementsParser.EvaluateStatementContext context)
        {
            Context = context;
            CodeElement = new EvaluateStatement();
        }

        public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context)
        {
            Context = context;
            CodeElement = new EvaluateStatementEnd();
        }


        public override void EnterInitializeStatement(CodeElementsParser.InitializeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInitializeStatement(context);
        }

        public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInspectStatement(context);
        }

        public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInvokeStatement(context);
        }

        public override void EnterMoveStatement(CodeElementsParser.MoveStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateMoveStatement(context);
        }

        public override void EnterMultiplyStatementFormat1(CodeElementsParser.MultiplyStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatementFormat2(CodeElementsParser.MultiplyStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context)
        {
            Context = context;
            CodeElement = new MultiplyStatementEnd();
        }


        public override void EnterNextSentenceStatement(CodeElementsParser.NextSentenceStatementContext context)
        {
            Context = context;
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateOpenStatement(context);
        }

        public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateCloseStatement(context);
        }

        public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateReadStatement(context);
        }

        public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateWriteStatement(context);
        }

        public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new WriteStatementEnd();
        }

        public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateRewriteStatement(context);
        }

        public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new RewriteStatementEnd();
        }


        public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context)
        {
            Context = context;
            CodeElement = new PerformStatement();
        }

        public override void EnterPerformProcedureStatement(CodeElementsParser.PerformProcedureStatementContext context)
        {
            Context = context;
            CodeElement = new PerformProcedureStatement();
        }

        public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context)
        {
            Context = context;
            CodeElement = new PerformStatementEnd();
        }



        public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context)
        {
            var statement = new ReleaseStatement();
            statement.RecordName = SyntaxElementBuilder.CreateQualifiedName(context.qualifiedDataName());
            statement.From = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            Context = context;
            CodeElement = statement;
        }

        public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateReturnStatement(context);
        }
        public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReturnStatementEnd();
        }

        public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateSearchStatement(context);
        }
        public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context)
        {
            Context = context;
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterSetStatementForAssignation(CodeElementsParser.SetStatementForAssignationContext context)
        {
            var statement = new SetStatementForAssignation();
            if (context.identifier() != null)
            {
                statement.Receiving = new List<Expression>();
                foreach ( var identifierContext in context.identifier()) {
                    Expression receiving;
                    if (identifierContext != null)
                    {
                        receiving = SyntaxElementBuilder.CreateIdentifier(identifierContext);
                    }
                    else break;
                    statement.Receiving.Add(receiving);
                }
            }

            if (context.setStatementForAssignationSending() != null)
            {
               if (context.setStatementForAssignationSending().identifier() != null)
                {
                    statement.Sending = SyntaxElementBuilder.CreateIdentifier(context.setStatementForAssignationSending().identifier());
                }
                else if (context.setStatementForAssignationSending().IntegerLiteral() != null)
                {
                    statement.Sending = new Number(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().IntegerLiteral())));
                }
                else if (context.setStatementForAssignationSending().TRUE() != null)
                {
                    statement.Sending = new Literal(new SyntaxBoolean(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().TRUE())));
                }
                else if (context.setStatementForAssignationSending().FALSE() != null)
                {
                    statement.Sending = new Literal(new SyntaxBoolean(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().FALSE())));
                }
                else if (context.setStatementForAssignationSending().NULL() != null)
                {
                    statement.Sending = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULL()));
                }
                else if (context.setStatementForAssignationSending().NULLS() != null)
                {
                    statement.Sending = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULLS()));
                }
                else if (context.setStatementForAssignationSending().SELF() != null)
                {
                    statement.Sending =
                        new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().SELF()));
                }
            }

            Context = context;
            CodeElement = statement;
        }


        public override void EnterSetStatementForIndexes(CodeElementsParser.SetStatementForIndexesContext context)
        {
            var statement = new SetStatementForIndex();

            if (context.indexNameReference() != null)
            {
                var indexs = new List<Index>();
                foreach (var indexNameContext in context.indexNameReference())
                {
                    indexs.Add(SyntaxElementBuilder.CreateIndex(indexNameContext));
                }
                statement.ReceivingIndexs = indexs;
            }
			statement.UpBy   = (context.UP() != null);
			statement.DownBy = (context.DOWN() != null);

            if (context.identifier() != null)
            {
                statement.SendingField = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            } 
            else if (context.IntegerLiteral() != null)
            {
                statement.SendingField = new Number(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral())));
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterSetStatementForSwitches(CodeElementsParser.SetStatementForSwitchesContext context)
        {
            var statement = new SetStatementForSwitches();

            if (context.setStatementForSwitchesWhat() != null)
            {
                var setExternalSwitchs = new List<SetExternalSwitch>();
                foreach (var switchesWhatContext in context.setStatementForSwitchesWhat())
                {
                    var setExternalSwitch = new SetExternalSwitch();
                    
                    if (switchesWhatContext.mnemonicForUPSISwitchNameReference() != null)
                    {
                        var mnemonics = new List<MnemonicForEnvironmentName>();
                        foreach (var mnemonicContext in switchesWhatContext.mnemonicForUPSISwitchNameReference())
                        {
                           mnemonics.Add(new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(mnemonicContext)));
                        }
                        setExternalSwitch.MnemonicForEnvironmentNames = mnemonics;
                    }
					setExternalSwitch.ToOn = (switchesWhatContext.ON() != null);
					setExternalSwitch.ToOff= (switchesWhatContext.OFF() != null);
					setExternalSwitchs.Add(setExternalSwitch);
                }
                statement.SetExternalSwitches = setExternalSwitchs;
            }

            Context = context;
            CodeElement = statement;
        }



        public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateMergeStatement(context);
        }

        public override void EnterSortStatement(CodeElementsParser.SortStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateSortStatement(context);
        }

        public override void EnterStartStatement(CodeElementsParser.StartStatementContext context)
        {
            var statement = new StartStatement();
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileNameReference());
            statement.DataName = SyntaxElementBuilder.CreateQualifiedName(context.qualifiedDataName());
            if (context.relationalOperator() != null)
                statement.Operator = new LogicalExpressionBuilder().CreateOperator(context.relationalOperator());
            
            Context = context;
            CodeElement = statement;
        }
        public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context)
        {
            Context = context;
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStopStatement(CodeElementsParser.StopStatementContext context)
        {
            var statement = new StopStatement();
            if (context.literal() != null)
                statement.Literal = SyntaxElementBuilder.CreateLiteral(context.literal());
            statement.IsStopRun = context.RUN() != null;

            Context = context;
            CodeElement = statement;
        }

        public override void EnterStringStatement(CodeElementsParser.StringStatementContext context)
        {
            var statement = new StringStatement();

            if (context.stringStatementWhat() != null)
            {
                var statementWhatList = new List<StringStatementWhat>();
                foreach (CodeElementsParser.StringStatementWhatContext stringStatementWhatContext in context.stringStatementWhat())
                {
                    var stringStatementWhat = new StringStatementWhat();

                    if (stringStatementWhatContext.identifierToConcat != null)
                    {
                        var identifierToConcat = new List<Expression>();
                        foreach (
                            CodeElementsParser.IdentifierOrLiteralContext idOrLiteral in
                                stringStatementWhatContext.identifierOrLiteral())
                        {
                            identifierToConcat.Add(CreateIdentifierOrLiteral(idOrLiteral, statement, "String"));
                        }
                        stringStatementWhat.IdentifierToConcat = identifierToConcat;
                    }
                    //else don't set IdentifierToConcat. It will remains null


					if (stringStatementWhatContext.stringStatementDelimiter() != null) {
						if (stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral() != null) {
							stringStatementWhat.DelimiterIdentifier =
								CreateIdentifierOrLiteral(stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral(), statement, "String");
						} else {
							stringStatementWhat.DelimitedBySize = (stringStatementWhatContext.stringStatementDelimiter().SIZE() != null);
						}
					}
                    statementWhatList.Add(stringStatementWhat);
                }

                statement.StringStatementWhat = statementWhatList;
            }
            //else don't set statement.StringStatementWhat


            if (context.identifierInto != null)
            {
                statement.IntoIdentifier = SyntaxElementBuilder.CreateIdentifier(context.identifierInto);
            } //else don't set statement.IntoIdentifier


            if (context.stringStatementWith() != null)
            {
                statement.PointerIdentifier = SyntaxElementBuilder.CreateIdentifier(context.stringStatementWith().identifier());
            } //else don't set statement.PointerIdentifier

            Context = context;
            CodeElement = statement;
        }

        public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context)
        {
            Context = context;
            CodeElement = new StringStatementEnd();
        }

        public override void EnterSubtractStatementFormat1(CodeElementsParser.SubtractStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat2(CodeElementsParser.SubtractStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat3(CodeElementsParser.SubtractStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context)
        {
            Context = context;
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context)
        {
            var statement = new UnstringStatement();

            if (context.unstringIdentifier != null)
            {
                statement.UnstringIdentifier = SyntaxElementBuilder.CreateIdentifier(context.unstringIdentifier);
            }

            if (context.unstringDelimited() != null)
            {
                if (context.unstringDelimited().delimitedBy != null)
                {
                    statement.DelimitedBy = CreateIdentifierOrLiteral(context.unstringDelimited().delimitedBy, statement, "unstring");
                }

                if (context.unstringDelimited().ustringOthersDelimiters() != null)
                {
                    var otherDelimiters = new List<Expression>();
                    foreach (
                        CodeElementsParser.UstringOthersDelimitersContext ustringOthersDelimitersContext in
                            context.unstringDelimited().ustringOthersDelimiters())
                    {
                        otherDelimiters.Add(CreateIdentifierOrLiteral(ustringOthersDelimitersContext.identifierOrLiteral(), statement,
                            "Unstring"));
                    }
                    statement.OtherDelimiters = otherDelimiters;
                }
            }

            if (context.unstringReceiver() != null)
            {
                var unstringReceiverList = new List<UnstringReceiver>();
                foreach (CodeElementsParser.UnstringReceiverContext unstringReceiverContext in context.unstringReceiver())
                {
                    var unstringReceiver = new UnstringReceiver();
                    if (unstringReceiverContext.intoIdentifier != null)
                    {
                        unstringReceiver.IntoIdentifier = SyntaxElementBuilder.CreateIdentifier(unstringReceiverContext.intoIdentifier);
                    }
                    if (unstringReceiverContext.unstringDelimiter() != null &&
                        unstringReceiverContext.unstringDelimiter().identifier() != null)
                    {
                        unstringReceiver.DelimiterIdentifier =
                            SyntaxElementBuilder.CreateIdentifier(unstringReceiverContext.unstringDelimiter().identifier());
                    }
                    if (unstringReceiverContext.unstringCount() != null && unstringReceiverContext.unstringCount().identifier() != null)
                    {
                        unstringReceiver.CountIdentifier =
                            SyntaxElementBuilder.CreateIdentifier(unstringReceiverContext.unstringCount().identifier());
                    }
                    unstringReceiverList.Add(unstringReceiver);
                }
                statement.UnstringReceivers = unstringReceiverList;
            }

            if (context.unstringPointer() != null && context.unstringPointer().identifier() != null)
            {
                statement.WithPointer = SyntaxElementBuilder.CreateIdentifier(context.unstringPointer().identifier());
            }

            if (context.unstringTallying() != null && context.unstringTallying().identifier() != null)
            {
                statement.Tallying = SyntaxElementBuilder.CreateIdentifier(context.unstringTallying().identifier());
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context)
        {
            Context = context;
            CodeElement = new UnstringStatementEnd();
        }

        public override void EnterUseStatement(CodeElementsParser.UseStatementContext context)
        {
            Context = context;
            var builder = new StatementsBuilder();
            if (context.useStatementForExceptionDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForExceptionDeclarative());
            else
            if (context.useStatementForDebuggingDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForDebuggingDeclarative());
            else
                Console.WriteLine("?TODO: USE?");
        }

        public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateXmlGenerateStatement(context);
        }

        public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateXmlParseStatement(context);
        }

        public override void EnterXmlStatementEnd(CodeElementsParser.XmlStatementEndContext context)
        {
            Context = context;
            CodeElement = new XmlStatementEnd();
        }

        // Statement conditions

        public override void EnterWhenCondition(CodeElementsParser.WhenConditionContext context)
        {
            Context = context;
            CodeElement = new WhenConditionalExpression();
        }

        public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context)
        {
            Context = context;
            CodeElement = new WhenOtherCondition();
        }

        public override void EnterAtEndCondition(CodeElementsParser.AtEndConditionContext context)
        {
            Context = context;
            CodeElement = new AtEndCondition();
        }

        public override void EnterNotAtEndCondition(CodeElementsParser.NotAtEndConditionContext context)
        {
            Context = context;
            CodeElement = new NotAtEndCondition();
        }

        public override void EnterAtEndOfPageCondition(CodeElementsParser.AtEndOfPageConditionContext context)
        {
            Context = context;
            CodeElement = new AtEndOfPageCondition();
        }

        public override void EnterNotAtEndOfPageCondition(CodeElementsParser.NotAtEndOfPageConditionContext context)
        {
            Context = context;
            CodeElement = new NotAtEndOfPageCondition();
        }

        public override void EnterOnExceptionCondition(CodeElementsParser.OnExceptionConditionContext context)
        {
            Context = context;
            CodeElement = new OnExceptionCondition();
        }

        public override void EnterNotOnExceptionCondition(CodeElementsParser.NotOnExceptionConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnExceptionCondition();
        }

        public override void EnterOnOverflowCondition(CodeElementsParser.OnOverflowConditionContext context)
        {
            Context = context;
            CodeElement = new OnOverflowCondition();
        }

        public override void EnterNotOnOverflowCondition(CodeElementsParser.NotOnOverflowConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnOverflowCondition();
        }

        public override void EnterInvalidKeyCondition(CodeElementsParser.InvalidKeyConditionContext context)
        {
            Context = context;
            CodeElement = new InvalidKeyCondition();
        }

        public override void EnterNotInvalidKeyCondition(CodeElementsParser.NotInvalidKeyConditionContext context)
        {
            Context = context;
            CodeElement = new NotInvalidKeyCondition();
        }

        public override void EnterOnSizeErrorCondition(CodeElementsParser.OnSizeErrorConditionContext context)
        {
            Context = context;
            CodeElement = new OnSizeErrorCondition();
        }

        public override void EnterNotOnSizeErrorCondition(CodeElementsParser.NotOnSizeErrorConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnSizeErrorCondition();
        }

        // -- Symbols --

        // ** Program names and Program entries **

        public override void EnterProgramNameDefinition(CodeElementsParser.ProgramNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramEntryDefinition(CodeElementsParser.ProgramEntryDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramEntry);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference1(CodeElementsParser.ProgramNameReference1Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference2(CodeElementsParser.ProgramNameReference2Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReferenceOrProgramEntryReference(CodeElementsParser.ProgramNameReferenceOrProgramEntryReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ProgramName, SymbolType.ProgramEntry };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references
        // => programNameFromDataOrProgramEntryFromData
        // => programNameFromDataOrProgramEntryFromDataOrProcedurePointerOrFunctionPointer

        // ** Section names and Paragraph names **

        public override void EnterSectionNameDefinition(CodeElementsParser.SectionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameDefinition(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSectionNameReference(CodeElementsParser.SectionNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameOrSectionNameReference(CodeElementsParser.ParagraphNameOrSectionNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.SectionName, SymbolType.ParagraphName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void ExitQualifiedParagraphNameReference(CodeElementsParser.QualifiedParagraphNameReferenceContext context)
        {
            if (context.sectionNameReference() != null)
            {
                Token qualifiedToken = ParseTreeUtils.GetFirstToken(context.paragraphNameReference());
                Token[] qualifierTokens = new Token[] { ParseTreeUtils.GetFirstToken(context.sectionNameReference()) };
                UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
            }
        }

        // ** Class names and Method names **

        public override void EnterClassNameDefinition(CodeElementsParser.ClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameDefinition(CodeElementsParser.MethodNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameReference(CodeElementsParser.ClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameDefOrRef(CodeElementsParser.ClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.ClassName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
        
        public override void EnterExternalClassNameDefOrRef(CodeElementsParser.ExternalClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ExternalClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameReference(CodeElementsParser.MethodNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references 
        // => methodNameFromData

        // ** Environment names, UPSI switch names and associated Mnemonics, Condition  **

        public override void EnterEnvironmentName(CodeElementsParser.EnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.EnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterUpsiSwitchName(CodeElementsParser.UpsiSwitchNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.UPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameDefinition(CodeElementsParser.MnemonicForEnvironmentNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameDefinition(CodeElementsParser.MnemonicForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionForUPSISwitchNameDefinition(CodeElementsParser.ConditionForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReference(CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReferenceOrEnvironmentName(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.MnemonicForEnvironmentName, SymbolType.EnvironmentName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameReference(CodeElementsParser.MnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName, SymbolType.MnemonicForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Character sets **

        public override void EnterSymbolicCharacterDefinition(CodeElementsParser.SymbolicCharacterDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSymbolicCharacterReference(CodeElementsParser.SymbolicCharacterReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameDefinition(CodeElementsParser.AlphabetNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameReference(CodeElementsParser.AlphabetNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameDefinition(CodeElementsParser.CharacterClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameReference(CodeElementsParser.CharacterClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Data item, Data conditions and Indexes **

        public override void EnterDataNameDefinition(CodeElementsParser.DataNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReference(CodeElementsParser.DataNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSpecialRegister(CodeElementsParser.SpecialRegisterContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        
        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
        public override void EnterConditionNameDefinition(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Condition name reference already handled above :
        // => public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)

        // Condition name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
		public override void ExitQualifiedDataName(CodeElementsParser.QualifiedDataNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrIndexName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrIndexNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedConditionName(CodeElementsParser.QualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrIndexName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrFileName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrFileNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrClassName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrClassNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}

		private void GetTokensForSymbolInformation(out Token qualifiedToken, out Token[] qualifierTokens, IParseTree node, IParseTree[] nodes) {
			if (node != null)  qualifiedToken  = ParseTreeUtils.GetFirstToken(node);
			else qualifiedToken = null;
			if (nodes != null) qualifierTokens = nodes.Select(ctx => ParseTreeUtils.GetFirstToken(ctx)).ToArray();
			else qualifierTokens = null;
		}

		private void UpdateSymbolInformationForQualifiedNames(Token qualifiedToken, Token[] qualifierTokens) {
			if (qualifiedToken != null) {
				SymbolInformation qualifiedSymbolInfo = CodeElement.SymbolInformationForTokens[qualifiedToken];
				qualifiedSymbolInfo.QualifedBy = qualifierTokens;
			}
			if (qualifierTokens != null)
			foreach (var qualifierToken in qualifierTokens) {
				SymbolInformation qualifierSymbolInfo = CodeElement.SymbolInformationForTokens[qualifierToken];
				qualifierSymbolInfo.QualifierFor = qualifiedToken;
			}
		}

        public override void EnterIndexNameDefinition(CodeElementsParser.IndexNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterIndexNameReference(CodeElementsParser.IndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
               
        // ** Files **

        public override void EnterFileNameDefinition(CodeElementsParser.FileNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterFileNameReference(CodeElementsParser.FileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameDefinition(CodeElementsParser.XmlSchemaNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameReference(CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentName(CodeElementsParser.AssignmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.AssignmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentNameOrFileNameReference(CodeElementsParser.AssignmentNameOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.AssignmentName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Runtime functions **

		public override void EnterIntrinsicFunctionName(CodeElementsParser.IntrinsicFunctionNameContext context) {
			ITerminalNode node = null;
			if (context.UserDefinedWord() != null) node = context.UserDefinedWord();
			else
			if (context.LENGTH() != null) node = context.LENGTH();
			else
			if (context.RANDOM() != null) node = context.RANDOM();
			else
			if (context.WHEN_COMPILED() != null) node = context.WHEN_COMPILED();
			Token symbolToken = ParseTreeUtils.GetFirstToken(node);
			SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.FunctionName);
			CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }


// [TYPECOBOL]
		public override void EnterFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context) {
			// TCRFUN_NO_DEFAULT_ACCESS_MODIFIER: our grammar guarantees that writting no access modifier is a syntax error.
			//		In this case (or if PRIVATE was written), we set the modifier to Private so visibility is not left empty.
			var visibility = context.PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;
			QualifiedName name = null;
			if (context.UserDefinedWord() != null) {
				var token = ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord());
				name = new URI(token.Text);
			}
			Context = context;
			CodeElement = new FunctionDeclarationHeader(name, visibility);
		}
		public override void EnterInputPhrase(CodeElementsParser.InputPhraseContext context) {
			var ce = CodeElement as FunctionDeclarationProfile;
			foreach (var parameter in context.inputParameters())
				ce.InputParameters = CreateParameters(parameter);
		}
		private IList<InputParameter> CreateParameters(CodeElementsParser.InputParametersContext context) {
			var parameters = new List<InputParameter>();
			var by = ReceivingMode.ByReference;
			IList<Token> tokens = null;
			if (context.receivingMode() != null) {
				if (context.receivingMode() is CodeElementsParser.ByValueContext) by = ReceivingMode.ByValue;
				tokens = ParseTreeUtils.GetTokensList(context.receivingMode());
			}
			var mode = new SyntaxProperty<ReceivingMode>(by, tokens);
			foreach(var dataname in context.dataNameReference()) {
				parameters.Add(new InputParameter(SyntaxElementBuilder.CreateDataName(dataname), mode));
			}
			return parameters;
		}
		public override void EnterInoutPhrase(CodeElementsParser.InoutPhraseContext context) {
			var ce = CodeElement as FunctionDeclarationProfile;
			foreach(var dataname in context.dataNameReference())
				ce.InoutParameters.Add(SyntaxElementBuilder.CreateDataName(dataname));
		}
		public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
			var ce = CodeElement as FunctionDeclarationProfile;
			foreach(var dataname in context.dataNameReference())
				ce.OutputParameters.Add(SyntaxElementBuilder.CreateDataName(dataname));
		}
		public override void EnterReturningPhrase(CodeElementsParser.ReturningPhraseContext context) {
			var dataname = SyntaxElementBuilder.CreateDataName(context.dataNameReference());
			//TODO? dataname should be a QualifiedName,
			//      because LINKAGE data items can be complex,
			//      with group items and name collision and crap
			if (CodeElement is ProcedureDivisionHeader)
				((ProcedureDivisionHeader)CodeElement).ReturningParameter = dataname;
			else//if (CodeElement is FunctionDeclarationProfile)
				// let's say if we break here it's an implementation error!
				((FunctionDeclarationProfile)CodeElement).ReturningParameter = dataname;
		}
		public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
			Context = context;
			CodeElement = new FunctionDeclarationEnd();
		}
// [/TYPECOBOL]


        public override void EnterExecTranslatorName(CodeElementsParser.ExecTranslatorNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.ExecTranslatorName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
    }
}
