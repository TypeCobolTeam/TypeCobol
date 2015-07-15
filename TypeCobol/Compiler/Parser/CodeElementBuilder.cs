using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Diagnostics;
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
        public CodeElement CodeElement { get; private set; }

        /// <summary>
        ///     List of syntax diagnostics gathered while transversing the parse tree
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }

        /// <summary>
        ///     Initialization code run before parsing each new CodeElement
        /// </summary>
        public override void EnterCodeElement(CobolCodeElementsParser.CodeElementContext context)
        {
            CodeElement = null;
            Diagnostics = new List<Diagnostic>();
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
            ClassIdentification classIdentification = new ClassIdentification();
            
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
            ClassEnd classEnd = new ClassEnd();

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
            ProcedureDivisionHeader procedureDivisionHeader = new ProcedureDivisionHeader();

            if(context.usingPhrase() != null)
            {
                foreach(var inputParametersContext in context.usingPhrase().inputParameters())
                {
                    SyntaxProperty<ReceivingMode> receivingMode = null;
                    if (inputParametersContext.receivingMode() != null)
                    {
                        receivingMode = new SyntaxProperty<ReceivingMode>(
                            inputParametersContext.receivingMode() is CobolCodeElementsParser.ByValueContext ? ReceivingMode.ByValue : ReceivingMode.ByReference, 
                            ParseTreeUtils.GetTokensList(inputParametersContext.receivingMode()));
                    }
                    foreach(var dataNameContext in inputParametersContext.dataName())
                    {
                        Token dataName = ParseTreeUtils.GetFirstToken(dataNameContext);
                        InputParameter inputParameter = new InputParameter() { ReceivingMode = receivingMode, DataName = new DataName(dataName) };

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
            ParagraphHeader paragraphHeader = new ParagraphHeader();

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
            CodeElement = new AcceptStatement();
        }

        private static Identifier CreateIdentifier(IParseTree node) {
            //TODO: effective identifier parsing, DON'T take only first Token
            // (identifier can be like "ADDRESS OF myvar(10+3.14:20-101.42) OF mygroup")
            return new Identifier(ParseTreeUtils.GetFirstToken(node));
        }

        private SyntaxNumber CreateNumberLiteral(CobolCodeElementsParser.NumericLiteralContext context)
        {
            if (context.IntegerLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.DecimalLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.DecimalLiteral()));
            }
            if (context.FloatingPointLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.FloatingPointLiteral()));
            }
            if (context.ZERO() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZERO()));
            }
            if (context.ZEROS() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROS()));
            }
            if (context.ZEROES() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROES()));
            }
            throw new System.Exception("This is not a number!");
        }

        private Expression createLeftOperand(IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> operands)
        {
            Expression left = null;
            foreach (var operand in operands) {
                Expression tail = null;
                if (operand.identifier() != null)
                {
                    tail = CreateIdentifier(operand.identifier());
                }
                else
                if (operand.numericLiteral() != null)
                {
                    tail = new Number(CreateNumberLiteral(operand.numericLiteral()));
                }
                if (tail == null) continue;
                if (left == null)
                {
                    // first element of the list that is the "left" operand
                    left = tail;
                }
                else
                {
                    // add this element to the others, to get the sum that is the "left" operand
                    left = new Addition(left, tail);
                }
            }
            return left;
        }

        public override void EnterAddStatementFormat1(CobolCodeElementsParser.AddStatementFormat1Context context)
        {
            AddStatement statement = new AddStatement();

            Expression left = null;
            if (context.identifierOrNumericLiteral() != null)
            {
                // create the "left" operand of this addition
                left = createLeftOperand(context.identifierOrNumericLiteral());
            }
            if (left != null && context.identifierRounded() != null)
            {
                // note: "ADD a b TO c d." gives c = a+b+c and d = a+b+d
                // so add the "left" operand to all the elements of the "right" operand
                foreach (var operand in context.identifierRounded())
                {
                    Identifier right = CreateIdentifier(operand.identifier());
                    right.rounded = operand.ROUNDED() != null;
                    Expression operation = new Addition(left, right);
                    Token token = ParseTreeUtils.GetFirstToken(operand.identifier());
                    statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);
                }
            }
            CodeElement = statement;
        }

        public override void EnterAddStatementFormat2(CobolCodeElementsParser.AddStatementFormat2Context context)
        {
            AddStatement statement = new AddStatement();

            Expression operation = null;
            if (context.identifierOrNumericLiteral() != null)
            {
                // here we add all abc..yz in "ADD ab..y TO z" without distinction between
                // what is after the ADD and before the TO, and what is after the TO
                operation = createLeftOperand(context.identifierOrNumericLiteral());
            }
            if (operation != null && context.identifierRounded() != null)
            {
                foreach (var operand in context.identifierRounded())
                {
                    Identifier right = CreateIdentifier(operand.identifier());
                    right.rounded = operand.ROUNDED() != null;
                    Token token = ParseTreeUtils.GetFirstToken(operand.identifier());
                    statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);
                }
            }

            CodeElement = statement;
        }

        public override void EnterAddStatementFormat3(CobolCodeElementsParser.AddStatementFormat3Context context)
        {
            AddStatement statement = new AddStatement();

            Expression left = new Identifier(ParseTreeUtils.GetFirstToken(context.identifier()));
            Token token = ParseTreeUtils.GetFirstToken(context.identifierRounded());
            Expression right = new Identifier(token, context.identifierRounded().ROUNDED() != null);
            Expression operation = new Addition(left, right);
            statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);

            CodeElement = statement;
        }



        private void AddDataNameToCodeElement(CobolCodeElementsParser.InOrOfDataNameContext context)
        {
            Token token = ParseTreeUtils.GetFirstToken(context);
            SymbolReference<DataName> dataname = new SymbolReference<DataName>(new DataName(token));
            bool isIN = context.IN() != null;
            bool isOF = context.OF() != null;
            //TODO: I has dataname. What I do wif it?
        }
        private void AddFileNameToCodeElement(CobolCodeElementsParser.InOrOfFileNameContext context)
        {
            Token token = ParseTreeUtils.GetFirstToken(context);
            SymbolReference<FileName> filename = new SymbolReference<FileName>(new FileName(token));
            bool isIN = context.IN() != null;
            bool isOF = context.OF() != null;
            //TODO: I has filename. What I do wif it?
        }

        public override void EnterIdentifierFormat1(CobolCodeElementsParser.IdentifierFormat1Context context)
        {
            if (context.dataName() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(context.dataName());
                SymbolReference<DataName> data = new SymbolReference<DataName>(new DataName(token));
                //TODO and ... now ?
            }
            if (context.inOrOfDataName() != null)
            {
                foreach (var inof in context.inOrOfDataName())
                {
                    AddDataNameToCodeElement(inof);
                }
            }
            if (context.inOrOfFileName() != null)
            {
                AddFileNameToCodeElement(context.inOrOfFileName());
            }
            //TODO: subscripts
            //TODO: reference modifiers
        }

        public override void EnterIdentifierFormat2(CobolCodeElementsParser.IdentifierFormat2Context context)
        {
            var condition = context.conditionName(); //TODO
            if (context.inOrOfDataName() != null)
            {
                foreach (var inof in context.inOrOfDataName())
                {
                    AddDataNameToCodeElement(inof);
                }
            }
            if (context.inOrOfFileName() != null)
            {
                AddFileNameToCodeElement(context.inOrOfFileName());
            }
        }

        public override void EnterIdentifierFormat3(CobolCodeElementsParser.IdentifierFormat3Context context)
        {
            context.LINAGE_COUNTER();
            if (context.inOrOfFileName() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(context.inOrOfFileName());
                SymbolReference<FileName> filename = new SymbolReference<FileName>(new FileName(token));
                bool isIN = context.inOrOfFileName().IN() != null;
                bool isOF = context.inOrOfFileName().OF() != null;
                //TODO: I has filename. What I do wif it?
            }
        }



        public override void EnterAlterStatement(CobolCodeElementsParser.AlterStatementContext context)
        {
            CodeElement = new AlterStatement();
        }

        public override void EnterCallStatement(CobolCodeElementsParser.CallStatementContext context)
        {
            CodeElement = new CallStatement();
        }

        public override void EnterCancelStatement(CobolCodeElementsParser.CancelStatementContext context)
        {
            CodeElement = new CancelStatement();
        }

        public override void EnterCloseStatement(CobolCodeElementsParser.CloseStatementContext context)
        {
            CodeElement = new CloseStatement();
        }

        public override void EnterComputeStatement(CobolCodeElementsParser.ComputeStatementContext context)
        {
            CodeElement = new ComputeStatement();
        }

        public override void EnterContinueStatement(CobolCodeElementsParser.ContinueStatementContext context)
        {
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CobolCodeElementsParser.DeleteStatementContext context)
        {
            CodeElement = new DeleteStatement();
        }

        public static LiteralForDisplay CreateLiteral(IParseTree node)
        {
            //TODO
            //Dumb code just to avoid to return null
            return new LiteralForDisplay(ParseTreeUtils.GetFirstToken(node));
        }

        /// <summary>
        /// Create a MnemonicOrEnvironmentName from a token.
        /// This method first check if the token match an environment name from EnvironmentNameEnum
        /// If so, it's an EnvironmentName
        /// otherwise, it's a mnemonic environment name
        /// 
        /// 
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
            else
            {
                //if this happens, it means it's a mnemonic environment name
                return new MnemonicForEnvironmentName(mnemonicOrEnvironmentName);
            }
        }

        public override void EnterDisplayStatement(CobolCodeElementsParser.DisplayStatementContext context)
        {
            var displayStement = new DisplayStatement();

            //Identifiers & literals
            if (context.identifierOrLiteral() != null)
            {
                var expressions = new List<Expression>();
                foreach (CobolCodeElementsParser.IdentifierOrLiteralContext idOrLiteral in context.identifierOrLiteral())
                {
                    if (idOrLiteral.identifier() != null)
                    {
                        expressions.Add(CreateIdentifier(idOrLiteral));
                    }
                    else if (idOrLiteral.literal() != null)
                    {
                        expressions.Add(CreateLiteral(idOrLiteral));
                    }
                    else
                    {
                        //TODO
                        // Register a new diagnostic
                        var diagnostic = new ParserDiagnostic("Unknow symbol", ParseTreeUtils.GetFirstToken(idOrLiteral),
                            "identifierOrLiteral: contains something other than an identifier or a literal");
                        Diagnostics.Add(diagnostic);
                    }
                }
                displayStement.VarsToDisplay = expressions;
            }
            //else don't set the displayStement. It will remains null


            //(mnemonic) Environment name
            if (context.uponEnvironmentName() != null)
            {
                Token mnemonicOrEnvironmentName = ParseTreeUtils.GetFirstToken(context.uponEnvironmentName().mnemonicOrEnvironmentName());
                if (mnemonicOrEnvironmentName != null)
                {
                    displayStement.UponMnemonicOrEnvironmentName = CreateMnemonicOrEnvironmentName(mnemonicOrEnvironmentName);
                }
            } //else don't set UponMnemonicOrEnvironmentName. it will remains null


            //With no advancing
            Token withNoAdvancing = ParseTreeUtils.GetFirstToken(context.withNoAdvancing());
            displayStement.IsWithNoAdvancing = new SyntaxBoolean(withNoAdvancing);


            CodeElement = displayStement;
        }

        public override void EnterDivideStatement(CobolCodeElementsParser.DivideStatementContext context)
        {
            CodeElement = new DivideStatement();
        }

        public override void EnterEntryStatement(CobolCodeElementsParser.EntryStatementContext context)
        {
            CodeElement = new EntryStatement();
        }

        public override void EnterEvaluateStatement(CobolCodeElementsParser.EvaluateStatementContext context)
        {
            CodeElement = new EvaluateStatement();
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
            CodeElement = new GotoStatement();
        }

        public override void EnterIfStatement(CobolCodeElementsParser.IfStatementContext context)
        {
            CodeElement = new IfStatement();
        }

        public override void EnterInitializeStatement(CobolCodeElementsParser.InitializeStatementContext context)
        {
            CodeElement = new InitializeStatement();
        }

        public override void EnterInspectStatement(CobolCodeElementsParser.InspectStatementContext context)
        {
            CodeElement = new InspectStatement();
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
            CodeElement = new MoveStatement();
        }

        public override void EnterMultiplyStatement(CobolCodeElementsParser.MultiplyStatementContext context)
        {
            CodeElement = new MultiplyStatement();
        }

        public override void EnterNextSentenceStatement(CobolCodeElementsParser.NextSentenceStatementContext context)
        {
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterOpenStatement(CobolCodeElementsParser.OpenStatementContext context)
        {
            CodeElement = new OpenStatement();
        }

        public override void EnterPerformProcedureStatement(
            CobolCodeElementsParser.PerformProcedureStatementContext context)
        {
            CodeElement = new PerformProcedureStatement();
        }

        public override void EnterPerformStatement(CobolCodeElementsParser.PerformStatementContext context)
        {
            CodeElement = new PerformStatement();
        }

        public override void EnterReadStatement(CobolCodeElementsParser.ReadStatementContext context)
        {
            CodeElement = new ReadStatement();
        }

        public override void EnterReleaseStatement(CobolCodeElementsParser.ReleaseStatementContext context)
        {
            CodeElement = new ReleaseStatement();
        }

        public override void EnterReturnStatement(CobolCodeElementsParser.ReturnStatementContext context)
        {
            CodeElement = new ReturnStatement();
        }

        public override void EnterRewriteStatement(CobolCodeElementsParser.RewriteStatementContext context)
        {
            CodeElement = new RewriteStatement();
        }

        public override void EnterSearchStatement(CobolCodeElementsParser.SearchStatementContext context)
        {
            CodeElement = new SearchStatement();
        }

        public override void EnterSetStatement(CobolCodeElementsParser.SetStatementContext context)
        {
            CodeElement = new SetStatement();
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
            CodeElement = new StopStatement();
        }

        public override void EnterStringStatement(CobolCodeElementsParser.StringStatementContext context)
        {
            CodeElement = new StringStatement();
        }

        public override void EnterSubtractStatement(CobolCodeElementsParser.SubtractStatementContext context)
        {
            CodeElement = new SubtractStatement();
        }

        public override void EnterUnstringStatement(CobolCodeElementsParser.UnstringStatementContext context)
        {
            CodeElement = new UnstringStatement();
        }

        public override void EnterUseStatement(CobolCodeElementsParser.UseStatementContext context)
        {
            CodeElement = new UseStatement();
        }

        public override void EnterWriteStatement(CobolCodeElementsParser.WriteStatementContext context)
        {
            CodeElement = new WriteStatement();
        }

        public override void EnterXmlGenerateStatement(CobolCodeElementsParser.XmlGenerateStatementContext context)
        {
            CodeElement = new XmlGenerateStatement();
        }

        public override void EnterXmlParseStatement(CobolCodeElementsParser.XmlParseStatementContext context)
        {
            CodeElement = new XmlParseStatement();
        }
        
        // Statement conditions

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

        public override void EnterElseCondition(CobolCodeElementsParser.ElseConditionContext context)
        {
            CodeElement = new ElseCondition();
        }

        public override void EnterWhenEvaluateCondition(CobolCodeElementsParser.WhenEvaluateConditionContext context)
        {
            CodeElement = new WhenEvaluateCondition();
        }

        public override void EnterWhenOtherCondition(CobolCodeElementsParser.WhenOtherConditionContext context)
        {
            CodeElement = new WhenOtherCondition();
        }

        public override void EnterWhenConditionalExpression(
            CobolCodeElementsParser.WhenConditionalExpressionContext context)
        {
            CodeElement = new WhenConditionalExpression();
        }
        
        // Statement ends

        public override void EnterAddStatementEnd(CobolCodeElementsParser.AddStatementEndContext context)
        {
            CodeElement = new AddStatementEnd();
        }

        public override void EnterCallStatementEnd(CobolCodeElementsParser.CallStatementEndContext context)
        {
            CodeElement = new CallStatementEnd();
        }

        public override void EnterComputeStatementEnd(CobolCodeElementsParser.ComputeStatementEndContext context)
        {
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterDeleteStatementEnd(CobolCodeElementsParser.DeleteStatementEndContext context)
        {
            CodeElement = new DeleteStatementEnd();
        }

        public override void EnterDivideStatementEnd(CobolCodeElementsParser.DivideStatementEndContext context)
        {
            CodeElement = new DivideStatementEnd();
        }

        public override void EnterEvaluateStatementEnd(CobolCodeElementsParser.EvaluateStatementEndContext context)
        {
            CodeElement = new EvaluateStatementEnd();
        }

        public override void EnterIfStatementEnd(CobolCodeElementsParser.IfStatementEndContext context)
        {
            CodeElement = new IfStatementEnd();
        }

        public override void EnterInvokeStatementEnd(CobolCodeElementsParser.InvokeStatementEndContext context)
        {
            CodeElement = new InvokeStatementEnd();
        }

        public override void EnterMultiplyStatementEnd(CobolCodeElementsParser.MultiplyStatementEndContext context)
        {
            CodeElement = new MultiplyStatementEnd();
        }

        public override void EnterPerformStatementEnd(CobolCodeElementsParser.PerformStatementEndContext context)
        {
            CodeElement = new PerformStatementEnd();
        }

        public override void EnterReadStatementEnd(CobolCodeElementsParser.ReadStatementEndContext context)
        {
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterReturnStatementEnd(CobolCodeElementsParser.ReturnStatementEndContext context)
        {
            CodeElement = new ReturnStatementEnd();
        }

        public override void EnterRewriteStatementEnd(CobolCodeElementsParser.RewriteStatementEndContext context)
        {
            CodeElement = new RewriteStatementEnd();
        }

        public override void EnterSearchStatementEnd(CobolCodeElementsParser.SearchStatementEndContext context)
        {
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterStartStatementEnd(CobolCodeElementsParser.StartStatementEndContext context)
        {
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStringStatementEnd(CobolCodeElementsParser.StringStatementEndContext context)
        {
            CodeElement = new StringStatementEnd();
        }

        public override void EnterSubtractStatementEnd(CobolCodeElementsParser.SubtractStatementEndContext context)
        {
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatementEnd(CobolCodeElementsParser.UnstringStatementEndContext context)
        {
            CodeElement = new UnstringStatementEnd();
        }

        public override void EnterWriteStatementEnd(CobolCodeElementsParser.WriteStatementEndContext context)
        {
            CodeElement = new WriteStatementEnd();
        }

        public override void EnterXmlStatementEnd(CobolCodeElementsParser.XmlStatementEndContext context)
        {
            CodeElement = new XmlStatementEnd();
        }
    }
}
