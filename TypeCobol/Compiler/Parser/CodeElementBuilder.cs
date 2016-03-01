﻿using System;
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

        /// <summary>
        ///     Initialization code run before parsing each new CodeElement
        /// </summary>
        public override void EnterCodeElement(CodeElementsParser.CodeElementContext context)
        {
            CodeElement = null;
            Context = null;
        }

        // Code structure

        // -- Program --

        public override void EnterProgramIdentification(CodeElementsParser.ProgramIdentificationContext context)
        {
            var programIdentification = new ProgramIdentification();

            Token programName = ParseTreeUtils.GetFirstToken(context.programName());
            if (programName != null)
            {
                programIdentification.ProgramName = new ProgramName(programName);
            }
            Token commonFlag = ParseTreeUtils.GetFirstToken(context.COMMON());
            if (commonFlag != null)
            {
                programIdentification.IsCommon = new SyntaxBoolean(commonFlag);
            }
            Token initialFlag = ParseTreeUtils.GetFirstToken(context.INITIAL());
            if (initialFlag != null)
            {
                programIdentification.IsInitial = new SyntaxBoolean(initialFlag);
            }
            Token recursiveFlag = ParseTreeUtils.GetFirstToken(context.RECURSIVE());
            if (recursiveFlag != null)
            {
                programIdentification.IsRecursive = new SyntaxBoolean(recursiveFlag);
            }

            Context = context;
            CodeElement = programIdentification;
        }

        public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context)
        {
            var programEnd = new ProgramEnd();

            Token programName = ParseTreeUtils.GetFirstToken(context.programName());
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
            classIdentification.ClassName = SyntaxElementBuilder.CreateClassName(context.classId);
            classIdentification.InheritsFrom = SyntaxElementBuilder.CreateClassName(context.inheritsFromClassName);

            Context = context;
            CodeElement = classIdentification;
        }

        public override void EnterClassEnd(CodeElementsParser.ClassEndContext context)
        {
            var classEnd = new ClassEnd();

            Token className = ParseTreeUtils.GetFirstToken(context.className());
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

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodName());
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

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodName());
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
                    foreach (CodeElementsParser.DataNameContext dataNameContext in inputParametersContext.dataName())
                    {
                        Token dataName = ParseTreeUtils.GetFirstToken(dataNameContext);
                        var inputParameter = new InputParameter {ReceivingMode = receivingMode, DataName = new DataName(dataName)};

                        if (procedureDivisionHeader.UsingParameters == null)
                        {
                            procedureDivisionHeader.UsingParameters = new List<InputParameter>();
                        }
                        procedureDivisionHeader.UsingParameters.Add(inputParameter);
                    }
                }
            }

            if (context.returningPhrase() != null)
            {
                Token dataName = ParseTreeUtils.GetFirstToken(context.returningPhrase().dataName());
                if (dataName != null)
                {
                    procedureDivisionHeader.ReturningDataName = new DataName(dataName);
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

            Token sectionName = ParseTreeUtils.GetFirstToken(context.sectionName());
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

            Token paragraphName = ParseTreeUtils.GetFirstToken(context.paragraphName());
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

        public override void EnterFileDescriptionEntry(CodeElementsParser.FileDescriptionEntryContext context)
        {
            Context = context;
            CodeElement = new FileDescriptionEntry();
        }

        public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context)
        {
            int level = 0;
            if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
                level = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
            }

            DataDescriptionEntry entry = new DataDescriptionEntry();
            entry.LevelNumber = level;
            entry.DataName = SyntaxElementBuilder.CreateDataName(context.dataName());
            //entry.IsFiller = (dataname == null || context.FILLER() != null);
            if (entry.LevelNumber == 88) entry.IsConditionNameDescription = true;

            UpdateDataDescriptionEntryWithRenamesClause(entry, context.renamesClause());

            var redefines = context.redefinesClause();
            if (redefines != null) entry.RedefinesDataName = SyntaxElementBuilder.CreateDataName(redefines.dataName());

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
// [TYPECOBOL]
			entry.IsTypeDefinition = (context.tcExtTypedefClause().Length > 0);
// [/TYPECOBOL]

            Context = context;
            CodeElement = entry;
        }

        private void UpdateDataDescriptionEntryWithRenamesClause(DataDescriptionEntry entry, CodeElementsParser.RenamesClauseContext context)
        {
            if (context == null) return;
            var names = SyntaxElementBuilder.CreateDataNames(context.dataName());
            if (names.Count > 0) entry.RenamesFromDataName = names[0];
            if (names.Count > 1) entry.RenamesToDataName   = names[1];
            //note: "RENAMES THRU dataname" will yield "from" initialized and "to" uninitialized
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
            entry.ObjectReference = SyntaxElementBuilder.CreateClassName(context.className());
        }

        private void UpdateDataDescriptionEntryWithOccursClause(DataDescriptionEntry entry, CodeElementsParser.OccursClauseContext context)
        {
            if (context == null) return;
            entry.IsTableOccurence = true;

            bool isVariable = (context.occursDependingOn() != null);
            if (isVariable) {
                entry.OccursDependingOn = SyntaxElementBuilder.CreateDataName(context.occursDependingOn().dataName());
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
                            entry.MaxOccurencesCount = Int32.MaxValue;
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
                    foreach(var name in key.dataName()) {
                        var data = SyntaxElementBuilder.CreateDataName(name);
                        if (data == null) continue;
                        entry.TableOccurenceKeys.Add(data);
                        entry.TableOccurenceKeyDirections.Add(direction);
                    }
                }
            }

            var indexes = context.indexName();
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
            var values = context.literal();
            if (values.Length > 0) entry.InitialValue = SyntaxElementBuilder.CreateLiteral(values[0]); // format 1 and 2
            if (values.Length > 1) entry.ThroughValue = SyntaxElementBuilder.CreateLiteral(values[1]); // format 2
            entry.IsInitialValueNull = (context.NULL() != null || context.NULLS() != null); // format 3
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
            foreach (var c in context.identifierOrLiteral())
            {
                var item = SyntaxElementBuilder.CreateIdentifierOrLiteral(c);
                if (item != null) statement.Items.Add(item);
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
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileName());
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
                Token mnemonicOrEnvironmentName = ParseTreeUtils.GetFirstToken(context.uponEnvironmentName().mnemonicOrEnvironmentName());
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


            //With no advancing
            Token withNoAdvancing = ParseTreeUtils.GetFirstToken(context.withNoAdvancing());
            statement.IsWithNoAdvancing = new SyntaxBoolean(withNoAdvancing);

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
            statement.ProgramName = SyntaxElementBuilder.CreateLiteral(context.literal());
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
            var node = ParseTreeUtils.GetTokenFromTerminalNode(context.ExecTranslatorName());
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
                statement.ReceivingFields = new List<Expression>();
                foreach ( var identifierContext in context.identifier()) {
                    Expression receiving;
                    if (identifierContext != null)
                    {
                        receiving = SyntaxElementBuilder.CreateIdentifier(identifierContext);
                    }
                    else break;
                    statement.ReceivingFields.Add(receiving);
                }
            }

            if (context.setStatementForAssignationSending() != null)
            {
               if (context.setStatementForAssignationSending().identifier() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreateIdentifier(context.setStatementForAssignationSending().identifier());
                }
                else if (context.setStatementForAssignationSending().IntegerLiteral() != null)
                {
                    statement.SendingField = new Number(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().IntegerLiteral())));
                }
                else if (context.setStatementForAssignationSending().TRUE() != null)
                {
                    statement.SendingField = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().TRUE()));
                }
                else if (context.setStatementForAssignationSending().NULL() != null)
                {
                    statement.SendingField = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULL()));
                }
                else if (context.setStatementForAssignationSending().NULLS() != null)
                {
                    statement.SendingField = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULLS()));
                }
                else if (context.setStatementForAssignationSending().SELF() != null)
                {
                    statement.SendingField =
                        new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().SELF()));
                }
            }

            Context = context;
            CodeElement = statement;
        }


        public override void EnterSetStatementForIndexes(CodeElementsParser.SetStatementForIndexesContext context)
        {
            var statement = new SetStatementForIndex();

            if (context.indexName() != null)
            {
                var indexs = new List<Index>();
                foreach (var indexNameContext in context.indexName())
                {
                    indexs.Add(SyntaxElementBuilder.CreateIndex(indexNameContext));
                }
                statement.ReceivingIndexs = indexs;
            }
            if (context.UP() != null)
            {
                statement.UpBy = new SyntaxBoolean(ParseTreeUtils.GetFirstToken(context.UP()));
            }
            if (context.DOWN() != null)
            {
                statement.DownBy = new SyntaxBoolean(ParseTreeUtils.GetFirstToken(context.DOWN()));
            }

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
                    
                    if (switchesWhatContext.mnemonicForUPSISwitchName() != null)
                    {
                        var mnemonics = new List<MnemonicForEnvironmentName>();
                        foreach (var mnemonicContext in switchesWhatContext.mnemonicForUPSISwitchName())
                        {
                           mnemonics.Add(new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(mnemonicContext)));
                        }
                        setExternalSwitch.MnemonicForEnvironmentNames = mnemonics;
                    }
                    if (switchesWhatContext.ON() != null)
                    {
                        setExternalSwitch.ToOn = new SyntaxBoolean(ParseTreeUtils.GetFirstToken(switchesWhatContext.ON()));
                    }
                    if (switchesWhatContext.OFF() != null)
                    {
                        setExternalSwitch.ToOff = new SyntaxBoolean(ParseTreeUtils.GetFirstToken(switchesWhatContext.OFF()));
                    }
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
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileName());
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


                    if (stringStatementWhatContext.stringStatementDelimiter() != null)
                    {
                        if (stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral() != null)
                        {
                            stringStatementWhat.DelimiterIdentifier =
                                CreateIdentifierOrLiteral(stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral(),
                                    statement, "String");
                        }
                        else if (stringStatementWhatContext.stringStatementDelimiter().SIZE() != null)
                        {
                            Token sizeToken = ParseTreeUtils.GetFirstToken(stringStatementWhatContext.stringStatementDelimiter().SIZE());
                            stringStatementWhat.Size = new SyntaxBoolean(sizeToken);
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
    }
}
