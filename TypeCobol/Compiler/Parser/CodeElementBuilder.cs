using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CodeElementBuilder : CobolBaseListener
    {
        /// <summary>
        /// CodeElement object resulting of the visit the parse tree
        /// </summary>
        public CodeElement CodeElement { get; private set; }

        /// <summary>
        /// List of syntax diagnostics gathered while transversing the parse tree
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }

        public override void EnterCodeElement(CobolParser.CodeElementContext context)
        {
            CodeElement = null;
            Diagnostics = new List<Diagnostic>();
        }
        
        // Code structure

        // -- Program --

        public override void EnterProgramIdentification(CobolParser.ProgramIdentificationContext context)
        {
            CodeElement = new ProgramIdentification();
        }

        public override void EnterProgramEnd(CobolParser.ProgramEndContext context)
        {
            CodeElement = new ProgramEnd();
        }

        // -- Class --

        public override void EnterClassIdentification(CobolParser.ClassIdentificationContext context)
        {
            CodeElement = new ClassIdentification();
        }

        public override void EnterClassEnd(CobolParser.ClassEndContext context)
        {
            CodeElement = new ClassEnd();
        }

        public override void EnterFactoryIdentification(CobolParser.FactoryIdentificationContext context)
        {
            CodeElement = new FactoryIdentification();
        }

        public override void EnterFactoryEnd(CobolParser.FactoryEndContext context)
        {
            CodeElement = new FactoryEnd();
        }

        public override void EnterObjectIdentification(CobolParser.ObjectIdentificationContext context)
        {
            CodeElement = new ObjectIdentification();
        }

        public override void EnterObjectEnd(CobolParser.ObjectEndContext context)
        {
            CodeElement = new ObjectEnd();
        }

        public override void EnterMethodIdentification(CobolParser.MethodIdentificationContext context)
        {
            CodeElement = new MethodIdentification();
        }

        public override void EnterMethodEnd(CobolParser.MethodEndContext context)
        {
            CodeElement = new MethodEnd();
        }

        // -- Division --

        public override void EnterEnvironmentDivisionHeader(CobolParser.EnvironmentDivisionHeaderContext context)
        {
            CodeElement = new EnvironmentDivisionHeader();
        }

        public override void EnterDataDivisionHeader(CobolParser.DataDivisionHeaderContext context)
        {
            CodeElement = new DataDivisionHeader();
        }

        public override void EnterProcedureDivisionHeader(CobolParser.ProcedureDivisionHeaderContext context)
        {
            CodeElement = new ProcedureDivisionHeader();
        }

        public override void EnterDeclarativesHeader(CobolParser.DeclarativesHeaderContext context)
        {
            CodeElement = new DeclarativesHeader();
        }

        public override void EnterDeclarativesEnd(CobolParser.DeclarativesEndContext context)
        {
            CodeElement = new DeclarativesEnd();
        }

        // -- Section --

        public override void EnterSectionHeader(CobolParser.SectionHeaderContext context)
        {
            CodeElement = new SectionHeader();
        }

        public override void EnterConfigurationSectionHeader(CobolParser.ConfigurationSectionHeaderContext context)
        {
            CodeElement = new ConfigurationSectionHeader();
        }

        public override void EnterInputOutputSectionHeader(CobolParser.InputOutputSectionHeaderContext context)
        {
            CodeElement = new InputOutputSectionHeader();
        }

        public override void EnterFileSectionHeader(CobolParser.FileSectionHeaderContext context)
        {
            CodeElement = new FileSectionHeader();
        }

        public override void EnterWorkingStorageSectionHeader(CobolParser.WorkingStorageSectionHeaderContext context)
        {
            CodeElement = new WorkingStorageSectionHeader();
        }

        public override void EnterLocalStorageSectionHeader(CobolParser.LocalStorageSectionHeaderContext context)
        {
            CodeElement = new LocalStorageSectionHeader();
        }

        public override void EnterLinkageSectionHeader(CobolParser.LinkageSectionHeaderContext context)
        {
            CodeElement = new LinkageSectionHeader();
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CobolParser.ParagraphHeaderContext context)
        {
            CodeElement = new ParagraphHeader();
        }

        public override void EnterFileControlParagraphHeader(CobolParser.FileControlParagraphHeaderContext context)
        {
            CodeElement = new FileControlParagraphHeader();
        }

        public override void EnterIoControlParagraphHeader(CobolParser.IoControlParagraphHeaderContext context)
        {
            CodeElement = new IOControlParagraphHeader();
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CobolParser.SentenceEndContext context)
        {
            CodeElement = new SentenceEnd();
        }
        
        // Entries
        
        // -- Data Division --

        public override void EnterFileDescriptionEntry(CobolParser.FileDescriptionEntryContext context)
        {
            CodeElement = new FileDescriptionEntry();
        }

        public override void EnterDataDescriptionEntry(CobolParser.DataDescriptionEntryContext context)
        {
            CodeElement = new DataDescriptionEntry();
        }

        // -- InputOutput Section --

        public override void EnterFileControlEntry(CobolParser.FileControlEntryContext context)
        {
            CodeElement = new FileControlEntry();
        }

        public override void EnterIoControlEntry(CobolParser.IoControlEntryContext context)
        {
            CodeElement = new IOControlEntry();
        }

        // Paragraphs

        // --Configuration Section --

        public override void EnterSourceComputerParagraph(CobolParser.SourceComputerParagraphContext context)
        {
            CodeElement = new SourceComputerParagraph();
        }

        public override void EnterObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext context)
        {
            CodeElement = new ObjectComputerParagraph();
        }

        public override void EnterSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext context)
        {
            CodeElement = new SpecialNamesParagraph();
        }

        public override void EnterRepositoryParagraph(CobolParser.RepositoryParagraphContext context)
        {
            CodeElement = new RepositoryParagraph();
        }
        
        // Statements

        public override void EnterAcceptStatement(CobolParser.AcceptStatementContext context)
        {
            CodeElement = new AcceptStatement();
        }

        public override void EnterAddStatement(CobolParser.AddStatementContext context)
        {
            CodeElement = new AddStatement();
        }

        public override void EnterAlterStatement(CobolParser.AlterStatementContext context)
        {
            CodeElement = new AlterStatement();
        }

        public override void EnterCallStatement(CobolParser.CallStatementContext context)
        {
            CodeElement = new CallStatement();
        }

        public override void EnterCancelStatement(CobolParser.CancelStatementContext context)
        {
            CodeElement = new CancelStatement();
        }

        public override void EnterCloseStatement(CobolParser.CloseStatementContext context)
        {
            CodeElement = new CloseStatement();
        }

        public override void EnterComputeStatement(CobolParser.ComputeStatementContext context)
        {
            CodeElement = new ComputeStatement();
        }

        public override void EnterContinueStatement(CobolParser.ContinueStatementContext context)
        {
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CobolParser.DeleteStatementContext context)
        {
            CodeElement = new DeleteStatement();
        }

        public override void EnterDisplayStatement(CobolParser.DisplayStatementContext context)
        {
            CodeElement = new DisplayStatement();
        }

        public override void EnterDivideStatement(CobolParser.DivideStatementContext context)
        {
            CodeElement = new DivideStatement();
        }

        public override void EnterEntryStatement(CobolParser.EntryStatementContext context)
        {
            CodeElement = new EntryStatement();
        }

        public override void EnterEvaluateStatement(CobolParser.EvaluateStatementContext context)
        {
            CodeElement = new EvaluateStatement();
        }

        public override void EnterExecStatement(CobolParser.ExecStatementContext context)
        {
            CodeElement = new ExecStatement();
        }

        public override void EnterExitMethodStatement(CobolParser.ExitMethodStatementContext context)
        {
            CodeElement = new ExitMethodStatement();
        }

        public override void EnterExitProgramStatement(CobolParser.ExitProgramStatementContext context)
        {
            CodeElement = new ExitProgramStatement();
        }

        public override void EnterExitStatement(CobolParser.ExitStatementContext context)
        {
            CodeElement = new ExitStatement();
        }

        public override void EnterGobackStatement(CobolParser.GobackStatementContext context)
        {
            CodeElement = new GobackStatement();
        }

        public override void EnterGotoStatement(CobolParser.GotoStatementContext context)
        {
            CodeElement = new GotoStatement();
        }

        public override void EnterIfStatement(CobolParser.IfStatementContext context)
        {
            CodeElement = new IfStatement();
        }

        public override void EnterInitializeStatement(CobolParser.InitializeStatementContext context)
        {
            CodeElement = new InitializeStatement();
        }

        public override void EnterInspectStatement(CobolParser.InspectStatementContext context)
        {
            CodeElement = new InspectStatement();
        }

        public override void EnterInvokeStatement(CobolParser.InvokeStatementContext context)
        {
            CodeElement = new InvokeStatement();
        }

        public override void EnterMergeStatement(CobolParser.MergeStatementContext context)
        {
            CodeElement = new MergeStatement();
        }

        public override void EnterMoveStatement(CobolParser.MoveStatementContext context)
        {
            CodeElement = new MoveStatement();
        }

        public override void EnterMultiplyStatement(CobolParser.MultiplyStatementContext context)
        {
            CodeElement = new MultiplyStatement();
        }

        public override void EnterNextSentenceStatement(CobolParser.NextSentenceStatementContext context)
        {
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterOpenStatement(CobolParser.OpenStatementContext context)
        {
            CodeElement = new OpenStatement();
        }

        public override void EnterPerformProcedureStatement(CobolParser.PerformProcedureStatementContext context)
        {
            CodeElement = new PerformProcedureStatement();
        }

        public override void EnterPerformStatement(CobolParser.PerformStatementContext context)
        {
            CodeElement = new PerformStatement();
        }

        public override void EnterReadStatement(CobolParser.ReadStatementContext context)
        {
            CodeElement = new ReadStatement();
        }

        public override void EnterReleaseStatement(CobolParser.ReleaseStatementContext context)
        {
            CodeElement = new ReleaseStatement();
        }

        public override void EnterReturnStatement(CobolParser.ReturnStatementContext context)
        {
            CodeElement = new ReturnStatement();
        }

        public override void EnterRewriteStatement(CobolParser.RewriteStatementContext context)
        {
            CodeElement = new RewriteStatement();
        }

        public override void EnterSearchStatement(CobolParser.SearchStatementContext context)
        {
            CodeElement = new SearchStatement();
        }

        public override void EnterSetStatement(CobolParser.SetStatementContext context)
        {
            CodeElement = new SetStatement();
        }

        public override void EnterSortStatement(CobolParser.SortStatementContext context)
        {
            CodeElement = new SortStatement();
        }

        public override void EnterStartStatement(CobolParser.StartStatementContext context)
        {
            CodeElement = new StartStatement();
        }

        public override void EnterStopStatement(CobolParser.StopStatementContext context)
        {
            CodeElement = new StopStatement();
        }

        public override void EnterStringStatement(CobolParser.StringStatementContext context)
        {
            CodeElement = new StringStatement();
        }

        public override void EnterSubtractStatement(CobolParser.SubtractStatementContext context)
        {
            CodeElement = new SubtractStatement();
        }

        public override void EnterUnstringStatement(CobolParser.UnstringStatementContext context)
        {
            CodeElement = new UnstringStatement();
        }

        public override void EnterUseStatement(CobolParser.UseStatementContext context)
        {
            CodeElement = new UseStatement();
        }

        public override void EnterWriteStatement(CobolParser.WriteStatementContext context)
        {
            CodeElement = new WriteStatement();
        }

        public override void EnterXmlGenerateStatement(CobolParser.XmlGenerateStatementContext context)
        {
            CodeElement = new XmlGenerateStatement();
        }

        public override void EnterXmlParseStatement(CobolParser.XmlParseStatementContext context)
        {
            CodeElement = new XmlParseStatement();
        }
        
        // Statement conditions

        public override void EnterAtEndCondition(CobolParser.AtEndConditionContext context)
        {
            CodeElement = new AtEndCondition();
        }

        public override void EnterNotAtEndCondition(CobolParser.NotAtEndConditionContext context)
        {
            CodeElement = new NotAtEndCondition();
        }

        public override void EnterAtEndOfPageCondition(CobolParser.AtEndOfPageConditionContext context)
        {
            CodeElement = new AtEndOfPageCondition();
        }

        public override void EnterNotAtEndOfPageCondition(CobolParser.NotAtEndOfPageConditionContext context)
        {
            CodeElement = new NotAtEndOfPageCondition();
        }

        public override void EnterOnExceptionCondition(CobolParser.OnExceptionConditionContext context)
        {
            CodeElement = new OnExceptionCondition();
        }

        public override void EnterNotOnExceptionCondition(CobolParser.NotOnExceptionConditionContext context)
        {
            CodeElement = new NotOnExceptionCondition();
        }

        public override void EnterOnOverflowCondition(CobolParser.OnOverflowConditionContext context)
        {
            CodeElement = new OnOverflowCondition();
        }

        public override void EnterNotOnOverflowCondition(CobolParser.NotOnOverflowConditionContext context)
        {
            CodeElement = new NotOnOverflowCondition();
        }

        public override void EnterInvalidKeyCondition(CobolParser.InvalidKeyConditionContext context)
        {
            CodeElement = new InvalidKeyCondition();
        }

        public override void EnterNotInvalidKeyCondition(CobolParser.NotInvalidKeyConditionContext context)
        {
            CodeElement = new NotInvalidKeyCondition();
        }

        public override void EnterOnSizeErrorCondition(CobolParser.OnSizeErrorConditionContext context)
        {
            CodeElement = new OnSizeErrorCondition();
        }

        public override void EnterNotOnSizeErrorCondition(CobolParser.NotOnSizeErrorConditionContext context)
        {
            CodeElement = new NotOnSizeErrorCondition();
        }

        public override void EnterElseCondition(CobolParser.ElseConditionContext context)
        {
            CodeElement = new ElseCondition();
        }

        public override void EnterWhenEvaluateCondition(CobolParser.WhenEvaluateConditionContext context)
        {
            CodeElement = new WhenEvaluateCondition();
        }

        public override void EnterWhenOtherCondition(CobolParser.WhenOtherConditionContext context)
        {
            CodeElement = new WhenOtherCondition();
        }

        public override void EnterWhenConditionalExpression(CobolParser.WhenConditionalExpressionContext context)
        {
            CodeElement = new WhenConditionalExpression();
        }
        
        // Statement ends

        public override void EnterAddStatementEnd(CobolParser.AddStatementEndContext context)
        {
            CodeElement = new AddStatementEnd();
        }

        public override void EnterCallStatementEnd(CobolParser.CallStatementEndContext context)
        {
            CodeElement = new CallStatementEnd();
        }

        public override void EnterComputeStatementEnd(CobolParser.ComputeStatementEndContext context)
        {
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterDeleteStatementEnd(CobolParser.DeleteStatementEndContext context)
        {
            CodeElement = new DeleteStatementEnd();
        }

        public override void EnterDivideStatementEnd(CobolParser.DivideStatementEndContext context)
        {
            CodeElement = new DivideStatementEnd();
        }

        public override void EnterEvaluateStatementEnd(CobolParser.EvaluateStatementEndContext context)
        {
            CodeElement = new EvaluateStatementEnd();
        }

        public override void EnterIfStatementEnd(CobolParser.IfStatementEndContext context)
        {
            CodeElement = new IfStatementEnd();
        }

        public override void EnterInvokeStatementEnd(CobolParser.InvokeStatementEndContext context)
        {
            CodeElement = new InvokeStatementEnd();
        }

        public override void EnterMultiplyStatementEnd(CobolParser.MultiplyStatementEndContext context)
        {
            CodeElement = new MultiplyStatementEnd();
        }

        public override void EnterPerformStatementEnd(CobolParser.PerformStatementEndContext context)
        {
            CodeElement = new PerformStatementEnd();
        }

        public override void EnterReadStatementEnd(CobolParser.ReadStatementEndContext context)
        {
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterReturnStatementEnd(CobolParser.ReturnStatementEndContext context)
        {
            CodeElement = new ReturnStatementEnd();
        }

        public override void EnterRewriteStatementEnd(CobolParser.RewriteStatementEndContext context)
        {
            CodeElement = new RewriteStatementEnd();
        }

        public override void EnterSearchStatementEnd(CobolParser.SearchStatementEndContext context)
        {
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterStartStatementEnd(CobolParser.StartStatementEndContext context)
        {
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStringStatementEnd(CobolParser.StringStatementEndContext context)
        {
            CodeElement = new StringStatementEnd();
        }

        public override void EnterSubtractStatementEnd(CobolParser.SubtractStatementEndContext context)
        {
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatementEnd(CobolParser.UnstringStatementEndContext context)
        {
            CodeElement = new UnstringStatementEnd();
        }

        public override void EnterWriteStatementEnd(CobolParser.WriteStatementEndContext context)
        {
            CodeElement = new WriteStatementEnd();
        }

        public override void EnterXmlStatementEnd(CobolParser.XmlStatementEndContext context)
        {
            CodeElement = new XmlStatementEnd();
        } 
    }
}
