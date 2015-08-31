using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Statement;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    ///     Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CodeElementBuilder : CobolCodeElementsBaseListener
    {
        /// <summary>
        ///     CodeElement object resulting of the visit the parse tree
        /// </summary>
        public CodeElement CodeElement = null;

        /// <summary>
        ///     Initialization code run before parsing each new CodeElement
        /// </summary>
        public override void EnterCodeElement(CobolCodeElementsParser.CodeElementContext context)
        {
            CodeElement = null;
        }

        // Code structure

        // -- Program --

        public override void EnterProgramIdentification(CobolCodeElementsParser.ProgramIdentificationContext context)
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

            CodeElement = programIdentification;
        }

        public override void EnterProgramEnd(CobolCodeElementsParser.ProgramEndContext context)
        {
            var programEnd = new ProgramEnd();

            Token programName = ParseTreeUtils.GetFirstToken(context.programName());
            if (programName != null)
            {
                programEnd.ProgramName = new ProgramName(programName);
            }

            CodeElement = programEnd;
        }

        public override void EnterAuthoringProperties(CobolCodeElementsParser.AuthoringPropertiesContext context)
        {
            var authoringProperties = new AuthoringProperties();

            if (context.authorParagraph().Count > 0)
            {
                authoringProperties.Author =
                    BuildCommentEntriesProperty(context.authorParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.dateCompiledParagraph().Count > 0)
            {
                authoringProperties.DateCompiled =
                    BuildCommentEntriesProperty(context.dateCompiledParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.dateWrittenParagraph().Count > 0)
            {
                authoringProperties.DateWritten =
                    BuildCommentEntriesProperty(context.dateWrittenParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.installationParagraph().Count > 0)
            {
                authoringProperties.Installation =
                    BuildCommentEntriesProperty(context.installationParagraph().SelectMany(p => p.CommentEntry()));
            }
            if (context.securityParagraph().Count > 0)
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

        public override void EnterClassIdentification(CobolCodeElementsParser.ClassIdentificationContext context)
        {
            var classIdentification = new ClassIdentification();

            Token className = ParseTreeUtils.GetFirstToken(context.classId);
            if (className != null)
            {
                classIdentification.ClassName = new ClassName(className);
            }
            Token inheritsFromClassName = ParseTreeUtils.GetFirstToken(context.inheritsFromClassName);
            if (inheritsFromClassName != null)
            {
                classIdentification.InheritsFromClassName = new SymbolReference<ClassName>(new ClassName(inheritsFromClassName));
            }

            CodeElement = classIdentification;
        }

        public override void EnterClassEnd(CobolCodeElementsParser.ClassEndContext context)
        {
            var classEnd = new ClassEnd();

            Token className = ParseTreeUtils.GetFirstToken(context.className());
            if (className != null)
            {
                classEnd.ClassName = new ClassName(className);
            }

            CodeElement = classEnd;
        }

        public override void EnterFactoryIdentification(CobolCodeElementsParser.FactoryIdentificationContext context)
        {
            CodeElement = new FactoryIdentification();
        }

        public override void EnterFactoryEnd(CobolCodeElementsParser.FactoryEndContext context)
        {
            CodeElement = new FactoryEnd();
        }

        public override void EnterObjectIdentification(CobolCodeElementsParser.ObjectIdentificationContext context)
        {
            CodeElement = new ObjectIdentification();
        }

        public override void EnterObjectEnd(CobolCodeElementsParser.ObjectEndContext context)
        {
            CodeElement = new ObjectEnd();
        }

        public override void EnterMethodIdentification(CobolCodeElementsParser.MethodIdentificationContext context)
        {
            var methodIdentification = new MethodIdentification();

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodName());
            if (methodName != null)
            {
                methodIdentification.MethodName = new MethodName(methodName);
            }

            CodeElement = methodIdentification;
        }

        public override void EnterMethodEnd(CobolCodeElementsParser.MethodEndContext context)
        {
            var methodEnd = new MethodEnd();

            Token methodName = ParseTreeUtils.GetFirstToken(context.methodName());
            if (methodName != null)
            {
                methodEnd.MethodName = new MethodName(methodName);
            }

            CodeElement = methodEnd;
        }

        // -- Division --

        public override void EnterEnvironmentDivisionHeader(CobolCodeElementsParser.EnvironmentDivisionHeaderContext context)
        {
            CodeElement = new EnvironmentDivisionHeader();
        }

        public override void EnterDataDivisionHeader(CobolCodeElementsParser.DataDivisionHeaderContext context)
        {
            CodeElement = new DataDivisionHeader();
        }

        public override void EnterProcedureDivisionHeader(CobolCodeElementsParser.ProcedureDivisionHeaderContext context)
        {
            var procedureDivisionHeader = new ProcedureDivisionHeader();

            if (context.usingPhrase() != null)
            {
                foreach (CobolCodeElementsParser.InputParametersContext inputParametersContext in context.usingPhrase().inputParameters())
                {
                    SyntaxProperty<ReceivingMode> receivingMode = null;
                    if (inputParametersContext.receivingMode() != null)
                    {
                        receivingMode = new SyntaxProperty<ReceivingMode>(
                            inputParametersContext.receivingMode() is CobolCodeElementsParser.ByValueContext
                                ? ReceivingMode.ByValue
                                : ReceivingMode.ByReference,
                            ParseTreeUtils.GetTokensList(inputParametersContext.receivingMode()));
                    }
                    foreach (CobolCodeElementsParser.DataNameContext dataNameContext in inputParametersContext.dataName())
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

            CodeElement = procedureDivisionHeader;
        }

        public override void EnterDeclarativesHeader(CobolCodeElementsParser.DeclarativesHeaderContext context)
        {
            CodeElement = new DeclarativesHeader();
        }

        public override void EnterDeclarativesEnd(CobolCodeElementsParser.DeclarativesEndContext context)
        {
            CodeElement = new DeclarativesEnd();
        }

        // -- Section --

        public override void EnterSectionHeader(CobolCodeElementsParser.SectionHeaderContext context)
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

            CodeElement = sectionHeader;
        }

        public override void EnterConfigurationSectionHeader(
            CobolCodeElementsParser.ConfigurationSectionHeaderContext context)
        {
            CodeElement = new ConfigurationSectionHeader();
        }

        public override void EnterInputOutputSectionHeader(
            CobolCodeElementsParser.InputOutputSectionHeaderContext context)
        {
            CodeElement = new InputOutputSectionHeader();
        }

        public override void EnterFileSectionHeader(CobolCodeElementsParser.FileSectionHeaderContext context)
        {
            CodeElement = new FileSectionHeader();
        }

        public override void EnterWorkingStorageSectionHeader(
            CobolCodeElementsParser.WorkingStorageSectionHeaderContext context)
        {
            CodeElement = new WorkingStorageSectionHeader();
        }

        public override void EnterLocalStorageSectionHeader(
            CobolCodeElementsParser.LocalStorageSectionHeaderContext context)
        {
            CodeElement = new LocalStorageSectionHeader();
        }

        public override void EnterLinkageSectionHeader(CobolCodeElementsParser.LinkageSectionHeaderContext context)
        {
            CodeElement = new LinkageSectionHeader();
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CobolCodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            Token paragraphName = ParseTreeUtils.GetFirstToken(context.paragraphName());
            if (paragraphName != null)
            {
                paragraphHeader.ParagraphName = new ParagraphName(paragraphName);
            }

            CodeElement = paragraphHeader;
        }

        public override void EnterFileControlParagraphHeader(
            CobolCodeElementsParser.FileControlParagraphHeaderContext context)
        {
            CodeElement = new FileControlParagraphHeader();
        }

        public override void EnterIoControlParagraphHeader(
            CobolCodeElementsParser.IoControlParagraphHeaderContext context)
        {
            CodeElement = new IOControlParagraphHeader();
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CobolCodeElementsParser.SentenceEndContext context)
        {
            CodeElement = new SentenceEnd();
        }

        // Entries

        // -- Data Division --

        public override void EnterFileDescriptionEntry(CobolCodeElementsParser.FileDescriptionEntryContext context)
        {
            CodeElement = new FileDescriptionEntry();
        }

        public override void EnterDataDescriptionEntry(CobolCodeElementsParser.DataDescriptionEntryContext context)
        {
            CodeElement = new DataDescriptionEntry();
        }

        // -- InputOutput Section --

        public override void EnterFileControlEntry(CobolCodeElementsParser.FileControlEntryContext context)
        {
            CodeElement = new FileControlEntry();
        }

        public override void EnterIoControlEntry(CobolCodeElementsParser.IoControlEntryContext context)
        {
            CodeElement = new IOControlEntry();
        }

        // Paragraphs

        // --Configuration Section --

        public override void EnterSourceComputerParagraph(CobolCodeElementsParser.SourceComputerParagraphContext context)
        {
            CodeElement = new SourceComputerParagraph();
        }

        public override void EnterObjectComputerParagraph(CobolCodeElementsParser.ObjectComputerParagraphContext context)
        {
            CodeElement = new ObjectComputerParagraph();
        }

        public override void EnterSpecialNamesParagraph(CobolCodeElementsParser.SpecialNamesParagraphContext context)
        {
            CodeElement = new SpecialNamesParagraph();
        }

        public override void EnterRepositoryParagraph(CobolCodeElementsParser.RepositoryParagraphContext context)
        {
            CodeElement = new RepositoryParagraph();
        }

        // Statements

        public override void EnterAcceptStatement(CobolCodeElementsParser.AcceptStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateAcceptStatement(context);
        }



          ///////////////////////////
         // ARITHMETIC STATEMENTS //
        ///////////////////////////

        public override void EnterAddStatementFormat1(CobolCodeElementsParser.AddStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat2(CobolCodeElementsParser.AddStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            if (context.GIVING() != null)
            {
                builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                    context.identifierRounded());
            }
            else
            {
                DiagnosticUtils.AddError(builder.statement, "Required: <identifier> after TO", context.identifierOrNumericLiteralTmp());
            }
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat3(CobolCodeElementsParser.AddStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterAddStatement(CobolCodeElementsParser.AddStatementContext context)
        {
            //CodeElement = new AddStatement();
        }

        public override void EnterAddStatementEnd(CobolCodeElementsParser.AddStatementEndContext context)
        {
            CodeElement = new AddStatementEnd();
        }

        public override void EnterComputeStatement(CobolCodeElementsParser.ComputeStatementContext context)
        {
            CodeElement = new ComputeStatementBuilder().CreateComputeStatement(context);
        }
        public override void EnterComputeStatementEnd(CobolCodeElementsParser.ComputeStatementEndContext context)
        {
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterDivideStatement(CobolCodeElementsParser.DivideStatementContext context)
        {
            CodeElement = new DivideStatementBuilder().CreateStatement(context);
        }
        public override void EnterDivideStatementEnd(CobolCodeElementsParser.DivideStatementEndContext context)
        {
            CodeElement = new DivideStatementEnd();
        }



        public override void EnterAlterStatement(CobolCodeElementsParser.AlterStatementContext context)
        {
            CodeElement = new AlterStatement();
        }

        public override void EnterCallStatement(CobolCodeElementsParser.CallStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateCallStatement(context);
        }
        public override void EnterCallStatementEnd(CobolCodeElementsParser.CallStatementEndContext context)
        {
            CodeElement = new CallStatementEnd();
        }

        public override void EnterCancelStatement(CobolCodeElementsParser.CancelStatementContext context)
        {
            var statement = new CancelStatement();
            foreach (var c in context.identifierOrLiteral())
            {
                var item = SyntaxElementBuilder.CreateIdentifierOrLiteral(c);
                var literal = item as Literal;
                if (literal != null && (literal.Value is double || literal.Value is long))
                    DiagnosticUtils.AddError(statement, "CANCEL: <literal> must be alphanumeric", c);
                if (item != null) statement.Items.Add(item);
            }
            CodeElement = statement;
        }

        public override void EnterContinueStatement(CobolCodeElementsParser.ContinueStatementContext context)
        {
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CobolCodeElementsParser.DeleteStatementContext context)
        {
            CodeElement = new DeleteStatement();
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

        public override void EnterDisplayStatement(CobolCodeElementsParser.DisplayStatementContext context)
        {
            var statement = new DisplayStatement();

            //Identifiers & literals
            if (context.identifierOrLiteral() != null)
            {
                var expressions = new List<Expression>();
                foreach (CobolCodeElementsParser.IdentifierOrLiteralContext idOrLiteral in context.identifierOrLiteral())
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


            CodeElement = statement;
        }

        /// <summary>
        /// </summary>
        /// <param name="idOrLiteral"></param>
        /// <param name="statement">Only used in case of error to link the error with the current statement</param>
        /// <param name="statementName">Only used in case of error to have the name of the current statement</param>
        /// <returns></returns>
        public Expression CreateIdentifierOrLiteral(CobolCodeElementsParser.IdentifierOrLiteralContext idOrLiteral, CodeElement statement,
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

        public override void EnterEntryStatement(CobolCodeElementsParser.EntryStatementContext context)
        {
            CodeElement = new EntryStatement();
        }

        public override void EnterExecStatement(CobolCodeElementsParser.ExecStatementContext context)
        {
            CodeElement = new ExecStatement();
        }

        public override void EnterExitMethodStatement(CobolCodeElementsParser.ExitMethodStatementContext context)
        {
            CodeElement = new ExitMethodStatement();
        }

        public override void EnterExitProgramStatement(CobolCodeElementsParser.ExitProgramStatementContext context)
        {
            CodeElement = new ExitProgramStatement();
        }

        public override void EnterExitStatement(CobolCodeElementsParser.ExitStatementContext context)
        {
            CodeElement = new ExitStatement();
        }

        public override void EnterGobackStatement(CobolCodeElementsParser.GobackStatementContext context)
        {
            CodeElement = new GobackStatement();
        }

        public override void EnterGotoStatement(CobolCodeElementsParser.GotoStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateGotoStatement(context);
        }


        public override void EnterIfStatement(CobolCodeElementsParser.IfStatementContext context)
        {
            var statement = new IfStatement();
            if (context.conditionalExpression() != null)
            {
                statement.condition = new LogicalExpressionBuilder().createCondition(context.conditionalExpression());
            }
            CodeElement = statement;
        }

        public override void EnterElseCondition(CobolCodeElementsParser.ElseConditionContext context)
        {
            CodeElement = new ElseCondition();
        }

        public override void EnterIfStatementEnd(CobolCodeElementsParser.IfStatementEndContext context)
        {
            CodeElement = new IfStatementEnd();
        }


        public override void EnterEvaluateStatement(CobolCodeElementsParser.EvaluateStatementContext context)
        {
            CodeElement = new EvaluateStatement();
        }

        public override void EnterWhenEvaluateCondition(CobolCodeElementsParser.WhenEvaluateConditionContext context)
        {
            CodeElement = new WhenEvaluateCondition();
        }

        public override void EnterWhenOtherCondition(CobolCodeElementsParser.WhenOtherConditionContext context)
        {
            CodeElement = new WhenOtherCondition();
        }

        public override void EnterEvaluateStatementEnd(CobolCodeElementsParser.EvaluateStatementEndContext context)
        {
            CodeElement = new EvaluateStatementEnd();
        }


        public override void EnterInitializeStatement(CobolCodeElementsParser.InitializeStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateInitializeStatement(context);
        }

        public override void EnterInspectStatement(CobolCodeElementsParser.InspectStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateInspectStatement(context);
        }

        public override void EnterInvokeStatement(CobolCodeElementsParser.InvokeStatementContext context)
        {
            CodeElement = new InvokeStatement();
        }

        public override void EnterMergeStatement(CobolCodeElementsParser.MergeStatementContext context)
        {
            CodeElement = new MergeStatement();
        }

        public override void EnterMoveStatement(CobolCodeElementsParser.MoveStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateMoveStatement(context);
        }

        public override void EnterMultiplyStatementFormat1(CobolCodeElementsParser.MultiplyStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatementFormat2(CobolCodeElementsParser.MultiplyStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatement(CobolCodeElementsParser.MultiplyStatementContext context)
        {
            //CodeElement = new MultiplyStatement();
        }

        public override void EnterMultiplyStatementEnd(CobolCodeElementsParser.MultiplyStatementEndContext context)
        {
            CodeElement = new MultiplyStatementEnd();
        }


        public override void EnterNextSentenceStatement(CobolCodeElementsParser.NextSentenceStatementContext context)
        {
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterOpenStatement(CobolCodeElementsParser.OpenStatementContext context)
        {
            CodeElement = new FileOperationBuilder().CreateOpenStatement(context);
        }

        public override void EnterCloseStatement(CobolCodeElementsParser.CloseStatementContext context)
        {
            CodeElement = new FileOperationBuilder().CreateCloseStatement(context);
        }

        public override void EnterReadStatement(CobolCodeElementsParser.ReadStatementContext context)
        {
            CodeElement = new FileOperationBuilder().CreateReadStatement(context);
        }

        public override void EnterReadStatementEnd(CobolCodeElementsParser.ReadStatementEndContext context)
        {
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterWriteStatement(CobolCodeElementsParser.WriteStatementContext context)
        {
            CodeElement = new FileOperationBuilder().CreateWriteStatement(context);
        }

        public override void EnterWriteStatementEnd(CobolCodeElementsParser.WriteStatementEndContext context)
        {
            CodeElement = new WriteStatementEnd();
        }

        public override void EnterRewriteStatement(CobolCodeElementsParser.RewriteStatementContext context)
        {
            CodeElement = new FileOperationBuilder().CreateRewriteStatement(context);
        }


        public override void EnterPerformStatement(CobolCodeElementsParser.PerformStatementContext context)
        {
            CodeElement = new PerformStatement();
        }

        public override void EnterPerformProcedureStatement(CobolCodeElementsParser.PerformProcedureStatementContext context)
        {
            CodeElement = new PerformStatement();
        }

        public override void EnterPerformStatementEnd(CobolCodeElementsParser.PerformStatementEndContext context)
        {
            CodeElement = new PerformStatementEnd();
        }



        public override void EnterReleaseStatement(CobolCodeElementsParser.ReleaseStatementContext context)
        {
            CodeElement = new ReleaseStatement();
        }

        public override void EnterReturnStatement(CobolCodeElementsParser.ReturnStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateReturnStatement(context);
        }
        public override void EnterReturnStatementEnd(CobolCodeElementsParser.ReturnStatementEndContext context)
        {
            CodeElement = new ReturnStatementEnd();
        }

        public override void EnterSearchStatement(CobolCodeElementsParser.SearchStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateSearchStatement(context);
        }
        public override void EnterSearchStatementEnd(CobolCodeElementsParser.SearchStatementEndContext context)
        {
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterSetStatementForAssignation(CobolCodeElementsParser.SetStatementForAssignationContext context)
        {
            var statement = new SetStatementForAssignation();
            if (context.setStatementForAssignationReceiving() != null)
            {
                var receivginList = new List<Expression>();
                foreach (
                    CobolCodeElementsParser.SetStatementForAssignationReceivingContext receivingContext in
                        context.setStatementForAssignationReceiving())
                {
                    Expression receiving;
                    if (receivingContext.indexName() != null)
                    {
                        receiving = SyntaxElementBuilder.CreateIndex(receivingContext.indexName());
                    }
                    else if (receivingContext.identifier() != null)
                    {
                        receiving = SyntaxElementBuilder.CreateIdentifier(receivingContext.identifier());
                    }
                    else if (receivingContext.procedurePointer() != null)
                    {
                        receiving = SyntaxElementBuilder.CreateProcedurePointer(receivingContext.procedurePointer());
                    }
                    else if (receivingContext.functionPointer() != null)
                    {
                        receiving = SyntaxElementBuilder.CreateFunctionPointer(receivingContext.functionPointer());
                    }
                    else if (receivingContext.objectReferenceId() != null)
                    {
                        receiving = SyntaxElementBuilder.CreateObjectReferenceId(receivingContext.objectReferenceId());
                    }
                    else
                    {
                        DiagnosticUtils.AddError(statement, "Set: Receiving fields missing or type unknown before TO", receivingContext);
                        break;
                    }
                    receivginList.Add(receiving);
                }
                statement.ReceivingFields = receivginList;
            }

            if (context.setStatementForAssignationSending() != null)
            {
                if (context.setStatementForAssignationSending().indexName() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreateIndex(context.setStatementForAssignationSending().indexName());
                }
                else if (context.setStatementForAssignationSending().identifier() != null)
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
                else if (context.setStatementForAssignationSending().procedurePointer() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreateProcedurePointer(context.setStatementForAssignationSending().procedurePointer());
                }
                else if (context.setStatementForAssignationSending().functionPointer() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreateFunctionPointer(context.setStatementForAssignationSending().functionPointer());
                }
                else if (context.setStatementForAssignationSending().pointerDataItem() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreatePointerDataItem(context.setStatementForAssignationSending().pointerDataItem());
                }
                else if (context.setStatementForAssignationSending().objectReferenceId() != null)
                {
                    statement.SendingField = SyntaxElementBuilder.CreateObjectReferenceId(context.setStatementForAssignationSending().objectReferenceId());
                } 
                else if (context.setStatementForAssignationSending().SELF() != null)
                {
                    statement.SendingField =
                        new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().SELF()));
                }
                else
                {
                    DiagnosticUtils.AddError(statement, "Set: Sending field missing or type unknown after TO", context.setStatementForAssignationSending());
                }
            }

            CodeElement = statement;
        }


        public override void EnterSetStatementForIndexes(CobolCodeElementsParser.SetStatementForIndexesContext context)
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
            else
            {
                DiagnosticUtils.AddError(statement, "Set xxx up/down by xxx: Sending field missing or type unknown", context);
            }

            CodeElement = statement;
        }

        public override void EnterSetStatementForSwitches(CobolCodeElementsParser.SetStatementForSwitchesContext context)
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

            CodeElement = statement;
        }


        public override void EnterSortStatement(CobolCodeElementsParser.SortStatementContext context)
        {
            CodeElement = new SortStatement();
        }

        public override void EnterStartStatement(CobolCodeElementsParser.StartStatementContext context)
        {
            CodeElement = new StartStatement();
        }

        public override void EnterStopStatement(CobolCodeElementsParser.StopStatementContext context)
        {
            var statement = new StopStatement();
            if (context.literal() != null)
            {
                statement.Literal = SyntaxElementBuilder.CreateLiteral(context.literal());
                if (statement.Literal != null && statement.Literal.All)
                    DiagnosticUtils.AddError(statement, "STOP: Illegal ALL", context.literal());
            }
            statement.IsStopRun = context.RUN() != null;
            CodeElement = statement;
        }

        public override void EnterStringStatement(CobolCodeElementsParser.StringStatementContext context)
        {
            var statement = new StringStatement();

            if (context.stringStatementWhat() != null)
            {
                var statementWhatList = new List<StringStatementWhat>();
                foreach (CobolCodeElementsParser.StringStatementWhatContext stringStatementWhatContext in context.stringStatementWhat())
                {
                    var stringStatementWhat = new StringStatementWhat();

                    if (stringStatementWhatContext.identifierToConcat != null)
                    {
                        var identifierToConcat = new List<Expression>();
                        foreach (
                            CobolCodeElementsParser.IdentifierOrLiteralContext idOrLiteral in
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

            CodeElement = statement;
        }

        public override void EnterSubtractStatementFormat1(CobolCodeElementsParser.SubtractStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat2(CobolCodeElementsParser.SubtractStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat3(CobolCodeElementsParser.SubtractStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatement(CobolCodeElementsParser.SubtractStatementContext context)
        {
            //CodeElement = new SubtractStatement();
        }

        public override void EnterSubtractStatementEnd(CobolCodeElementsParser.SubtractStatementEndContext context)
        {
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatement(CobolCodeElementsParser.UnstringStatementContext context)
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
                        CobolCodeElementsParser.UstringOthersDelimitersContext ustringOthersDelimitersContext in
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
                foreach (CobolCodeElementsParser.UnstringReceiverContext unstringReceiverContext in context.unstringReceiver())
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

            CodeElement = statement;
        }

        public override void EnterUseStatement(CobolCodeElementsParser.UseStatementContext context)
        {
            var builder = new StatementsBuilder();
            if (context.useStatementForExceptionDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForExceptionDeclarative());
            else
            if (context.useStatementForDebuggingDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForDebuggingDeclarative());
            else
                Console.WriteLine("?TODO: USE?");
        }

        public override void EnterXmlGenerateStatement(CobolCodeElementsParser.XmlGenerateStatementContext context)
        {
            CodeElement = new XmlGenerateStatement();
        }

        public override void EnterXmlParseStatement(CobolCodeElementsParser.XmlParseStatementContext context)
        {
            CodeElement = new StatementsBuilder().CreateXmlParseStatement(context);
        }

        public override void EnterXmlStatementEnd(CobolCodeElementsParser.XmlStatementEndContext context)
        {
            CodeElement = new XmlStatementEnd();
        }

        // Statement conditions

        public override void EnterWhenConditionalExpression(CobolCodeElementsParser.WhenConditionalExpressionContext context)
        {
            CodeElement = new WhenConditionalExpression();
        }

        public override void EnterAtEndCondition(CobolCodeElementsParser.AtEndConditionContext context)
        {
            CodeElement = new AtEndCondition();
        }

        public override void EnterNotAtEndCondition(CobolCodeElementsParser.NotAtEndConditionContext context)
        {
            CodeElement = new NotAtEndCondition();
        }

        public override void EnterAtEndOfPageCondition(CobolCodeElementsParser.AtEndOfPageConditionContext context)
        {
            CodeElement = new AtEndOfPageCondition();
        }

        public override void EnterNotAtEndOfPageCondition(CobolCodeElementsParser.NotAtEndOfPageConditionContext context)
        {
            CodeElement = new NotAtEndOfPageCondition();
        }

        public override void EnterOnExceptionCondition(CobolCodeElementsParser.OnExceptionConditionContext context)
        {
            CodeElement = new OnExceptionCondition();
        }

        public override void EnterNotOnExceptionCondition(CobolCodeElementsParser.NotOnExceptionConditionContext context)
        {
            CodeElement = new NotOnExceptionCondition();
        }

        public override void EnterOnOverflowCondition(CobolCodeElementsParser.OnOverflowConditionContext context)
        {
            CodeElement = new OnOverflowCondition();
        }

        public override void EnterNotOnOverflowCondition(CobolCodeElementsParser.NotOnOverflowConditionContext context)
        {
            CodeElement = new NotOnOverflowCondition();
        }

        public override void EnterInvalidKeyCondition(CobolCodeElementsParser.InvalidKeyConditionContext context)
        {
            CodeElement = new InvalidKeyCondition();
        }

        public override void EnterNotInvalidKeyCondition(CobolCodeElementsParser.NotInvalidKeyConditionContext context)
        {
            CodeElement = new NotInvalidKeyCondition();
        }

        public override void EnterOnSizeErrorCondition(CobolCodeElementsParser.OnSizeErrorConditionContext context)
        {
            CodeElement = new OnSizeErrorCondition();
        }

        public override void EnterNotOnSizeErrorCondition(CobolCodeElementsParser.NotOnSizeErrorConditionContext context)
        {
            CodeElement = new NotOnSizeErrorCondition();
        }

        // Statement ends

        public override void EnterRewriteStatementEnd(CobolCodeElementsParser.RewriteStatementEndContext context)
        {
            CodeElement = new RewriteStatementEnd();
        }

        public override void EnterStartStatementEnd(CobolCodeElementsParser.StartStatementEndContext context)
        {
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStringStatementEnd(CobolCodeElementsParser.StringStatementEndContext context)
        {
            CodeElement = new StringStatementEnd();
        }

        public override void EnterUnstringStatementEnd(CobolCodeElementsParser.UnstringStatementEndContext context)
        {
            CodeElement = new UnstringStatementEnd();
        }
    }
}
