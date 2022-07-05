using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// A Node Listener that can also dispatch Program Class Builder actions
    /// </summary>
    public class ProgramClassBuilderNodeListener : IProgramClassBuilderNodeListener
    {
        public virtual void OnNode(Node node, Program program)
        {

        }

        public virtual void OnLevel1Definition(DataDefinition level1Node)
        {

        }

        public virtual void Enter(Node node)
        {

        }

        public virtual void Exit(Node node)
        {

        }

        public virtual void AddNextSentenceStatement(NextSentenceStatement stmt)
        {

        }

        public virtual void CheckStartSentenceLastStatement()
        {

        }

        public virtual void EndAddStatementConditional(AddStatementEnd end = null)
        {

        }

        public virtual void EndCallStatementConditional(CallStatementEnd end = null)
        {

        }

        public virtual void EndCobolProgram(ProgramEnd end)
        {

        }

        public virtual void EndComputeStatementConditional(ComputeStatementEnd end = null)
        {

        }

        public virtual void EndConfigurationSection()
        {

        }

        public virtual void EndDataDivision()
        {

        }

        public virtual void EndDeclaratives(DeclarativesEnd end)
        {

        }

        public virtual void EndDeleteStatementConditional(DeleteStatementEnd end = null)
        {

        }

        public virtual void EndDivideStatementConditional(DivideStatementEnd end = null)
        {

        }

        public virtual void EndEnvironmentDivision()
        {

        }

        public virtual void EndEvaluateStatementWithBody(EvaluateStatementEnd end = null)
        {

        }

        public virtual void EndExecStatement()
        {

        }

        public virtual void OnExecStatementText(ExecStatementText text)
        {

        }

        public virtual void EndExecStatement(ExecStatementEnd end)
        {
            
        }

        public virtual void EndFileControlEntry()
        {

        }

        public virtual void EndFileControlParagraph()
        {

        }

        public virtual void EndFileDescriptionEntry()
        {

        }

        public virtual void EndFileDescriptionEntryIfAny()
        {

        }

        public virtual void EndFileSection()
        {

        }

        public virtual void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {

        }

        public virtual void EndFunctionProcedureDivision()
        {

        }

        public virtual void EndGlobalStorageSection()
        {

        }

        public virtual void EndIfStatementWithBody(IfStatementEnd end = null)
        {

        }

        public virtual void EndInputOutputSection()
        {

        }

        public virtual void EndInvokeStatementConditional(InvokeStatementEnd end = null)
        {

        }

        public virtual void EndJsonGenerateStatementConditional(JsonStatementEnd end = null)
        {

        }

        public virtual void EndJsonParseStatementConditional(JsonStatementEnd end = null)
        {

        }

        public virtual void EndLinkageSection()
        {

        }

        public virtual void EndLocalStorageSection()
        {

        }

        public virtual void EndMultiplyStatementConditional(MultiplyStatementEnd end = null)
        {

        }

        public virtual void EndNoAtEnd()
        {

        }

        public virtual void EndNoException()
        {

        }

        public virtual void EndNoInvalidKey()
        {

        }

        public virtual void EndNoOverflow()
        {

        }

        public virtual void EndNoSizeError()
        {

        }

        public virtual void EndObjectComputerParagraph()
        {

        }

        public virtual void EndOnAtEnd()
        {

        }

        public virtual void EndOnException()
        {

        }

        public virtual void EndOnInvalidKey()
        {

        }

        public virtual void EndOnOverflow()
        {

        }

        public virtual void EndOnSizeError()
        {

        }

        public virtual void EndParagraph()
        {

        }

        public virtual void EndPerformStatementWithBody(PerformStatementEnd end = null)
        {

        }

        public virtual void EndProcedureDivision()
        {

        }

        public virtual void EndReadStatementConditional(ReadStatementEnd end = null)
        {

        }

        public virtual void EndRepositoryParagraph()
        {

        }

        public virtual void EndReturnStatementConditional(ReturnStatementEnd end = null)
        {

        }

        public virtual void EndRewriteStatementConditional(RewriteStatementEnd end = null)
        {

        }

        public virtual void EndSearchStatementWithBody(SearchStatementEnd end = null)
        {

        }

        public virtual void EndSection()
        {

        }

        public virtual void EndSentence(SentenceEnd end, bool bCheck = false)
        {

        }

        public virtual void EndSourceComputerParagraph()
        {

        }

        public virtual void EndSpecialNamesParagraph()
        {

        }

        public virtual void EndStartStatementConditional(StartStatementEnd end = null)
        {

        }

        public virtual void EndStringStatementConditional(StringStatementEnd end = null)
        {

        }

        public virtual void EndSubtractStatementConditional(SubtractStatementEnd end = null)
        {

        }

        public virtual void EndUnstringStatementConditional(UnstringStatementEnd end = null)
        {

        }

        public virtual void EndWhenConditionClause()
        {

        }

        public virtual void EndWhenOtherClause()
        {

        }

        public virtual void EndWhenSearchConditionClause()
        {

        }

        public virtual void EndWorkingStorageSection()
        {

        }

        public virtual void EndWriteStatementConditional(WriteStatementEnd end = null)
        {

        }

        public virtual void EndXmlGenerateStatementConditional(XmlStatementEnd end = null)
        {

        }

        public virtual void EndXmlParseStatementConditional(XmlStatementEnd end = null)
        {

        }

        public virtual void EnterElseClause(ElseCondition clause)
        {

        }

        public virtual void EnterReadStatementConditional(ReadStatement stmt)
        {

        }

        public virtual void EnterReturnStatementConditional(ReturnStatement stmt)
        {

        }

        public virtual void EnterUseStatement(UseStatement useStatement)
        {

        }

        public virtual void OnAcceptStatement(AcceptStatement stmt)
        {

        }

        public virtual void OnAlterStatement(AlterStatement stmt)
        {

        }

        public virtual void OnCancelStatement(CancelStatement stmt)
        {

        }

        public virtual void OnCloseStatement(CloseStatement stmt)
        {

        }

        public virtual void OnContinueStatement(ContinueStatement stmt)
        {

        }

        public virtual void OnDisplayStatement(DisplayStatement stmt)
        {

        }

        public virtual void OnEntryStatement(EntryStatement stmt)
        {

        }

        public virtual void OnExecStatement(ExecStatement stmt)
        {

        }

        public virtual void OnExitMethodStatement(ExitMethodStatement stmt)
        {

        }

        public virtual void OnExitProgramStatement(ExitProgramStatement stmt)
        {

        }

        public virtual void OnExitStatement(ExitStatement stmt)
        {

        }

        public virtual void OnAllocateStatement(AllocateStatement stmt)
        {

        }

        public virtual void OnFreeStatement(FreeStatement stmt)
        {

        }

        public virtual void OnGobackStatement(GobackStatement stmt)
        {

        }

        public virtual void OnGotoStatement(GotoStatement stmt)
        {

        }

        public virtual void OnInitializeStatement(InitializeStatement stmt)
        {

        }

        public virtual void OnInspectStatement(InspectStatement stmt)
        {

        }

        public virtual void OnMergeStatement(MergeStatement stmt)
        {

        }

        public virtual void OnMoveStatement(MoveStatement stmt)
        {

        }

        public virtual void OnOpenStatement(OpenStatement stmt)
        {

        }

        public virtual void OnPerformProcedureStatement(PerformProcedureStatement stmt)
        {

        }

        public virtual void OnProcedureStyleCall(ProcedureStyleCallStatement stmt, CallStatementEnd end = null)
        {

        }

        public virtual void OnReleaseStatement(ReleaseStatement stmt)
        {

        }

        public virtual void OnSetStatement(SetStatement stmt)
        {

        }

        public virtual void OnSortStatement(SortStatement stmt)
        {

        }

        public virtual void OnStopStatement(StopStatement stmt)
        {

        }

        public virtual void StartAddStatementConditional(AddStatement stmt)
        {

        }

        public virtual void StartCallStatementConditional(CallStatement stmt)
        {

        }

        public virtual void StartCobolCompilationUnit()
        {

        }

        public virtual void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {

        }

        public virtual void StartComputeStatementConditional(ComputeStatement stmt)
        {

        }

        public virtual void StartConfigurationSection(ConfigurationSectionHeader header)
        {

        }

        public virtual void StartDataConditionEntry(DataConditionEntry entry)
        {

        }

        public virtual void StartDataDescriptionEntry(DataDescriptionEntry entry)
        {

        }

        public virtual void StartDataDivision(DataDivisionHeader header)
        {

        }

        public virtual void StartDataRedefinesEntry(DataRedefinesEntry entry)
        {

        }

        public virtual void StartDataRenamesEntry(DataRenamesEntry entry)
        {

        }

        public virtual void StartDeclaratives(DeclarativesHeader header)
        {

        }

        public virtual void StartDeleteStatementConditional(DeleteStatement stmt)
        {

        }

        public virtual void StartDivideStatementConditional(DivideStatement stmt)
        {

        }

        public virtual void StartEnvironmentDivision(EnvironmentDivisionHeader header)
        {

        }

        public virtual void StartEvaluateStatementWithBody(EvaluateStatement stmt)
        {

        }

        public virtual void StartExecStatement(ExecStatement execStmt)
        {

        }

        public virtual void StartFileControlEntry(FileControlEntry entry)
        {

        }

        public virtual void StartFileControlParagraph(FileControlParagraphHeader header)
        {

        }

        public virtual void StartFileDescriptionEntry(FileDescriptionEntry entry)
        {

        }

        public virtual void StartFileSection(FileSectionHeader header)
        {

        }

        public virtual void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {

        }

        public virtual void StartFunctionProcedureDivision(ProcedureDivisionHeader header)
        {

        }

        public virtual void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {

        }

        public virtual void StartIfStatementWithBody(IfStatement stmt)
        {

        }

        public virtual void StartInputOutputSection(InputOutputSectionHeader header)
        {

        }

        public virtual void StartInvokeStatementConditional(InvokeStatement stmt)
        {

        }

        public virtual void StartJsonGenerateStatementConditional(JsonGenerateStatement stmt)
        {

        }

        public virtual void StartJsonParseStatementConditional(JsonParseStatement stmt)
        {

        }

        public virtual void StartLinkageSection(LinkageSectionHeader header)
        {

        }

        public virtual void StartLocalStorageSection(LocalStorageSectionHeader header)
        {

        }

        public virtual void StartMultiplyStatementConditional(MultiplyStatement stmt)
        {

        }

        public virtual void StartNoAtEnd(NotAtEndCondition cond)
        {

        }

        public virtual void StartNoException(NotOnExceptionCondition cond)
        {

        }

        public virtual void StartNoInvalidKey(NotInvalidKeyCondition cond)
        {

        }

        public virtual void StartNoOverflow(NotOnOverflowCondition cond)
        {

        }

        public virtual void StartNoSizeError(NotOnSizeErrorCondition cond)
        {

        }

        public virtual void StartObjectComputerParagraph(ObjectComputerParagraph paragraph)
        {

        }

        public virtual void StartOnAtEnd(AtEndCondition cond)
        {

        }

        public virtual void StartOnException(OnExceptionCondition cond)
        {

        }

        public virtual void StartOnInvalidKey(InvalidKeyCondition cond)
        {

        }

        public virtual void StartOnOverflow(OnOverflowCondition cond)
        {

        }

        public virtual void StartOnSizeError(OnSizeErrorCondition cond)
        {

        }

        public virtual void StartParagraph(ParagraphHeader header)
        {

        }

        public virtual void StartPerformStatementWithBody(PerformStatement stmt)
        {

        }

        public virtual void StartProcedureDivision(ProcedureDivisionHeader header)
        {

        }

        public virtual void StartRepositoryParagraph(RepositoryParagraph paragraph)
        {

        }

        public virtual void StartRewriteStatementConditional(RewriteStatement stmt)
        {

        }

        public virtual void StartSearchStatementWithBody(SearchStatement stmt)
        {

        }

        public virtual void StartSection(SectionHeader header)
        {

        }

        public virtual void StartSentence()
        {

        }

        public virtual void StartSourceComputerParagraph(SourceComputerParagraph paragraph)
        {

        }

        public virtual void StartSpecialNamesParagraph(SpecialNamesParagraph paragraph)
        {

        }

        public virtual void StartStartStatementConditional(StartStatement stmt)
        {

        }

        public virtual void StartStringStatementConditional(StringStatement stmt)
        {

        }

        public virtual void StartSubtractStatementConditional(SubtractStatement stmt)
        {

        }

        public virtual void StartTypeDefinitionEntry(DataTypeDescriptionEntry typedef)
        {

        }

        public virtual void StartUnstringStatementConditional(UnstringStatement stmt)
        {

        }

        public virtual void StartWhenConditionClause(List<CodeElement> conditions)
        {

        }

        public virtual void StartWhenOtherClause(WhenOtherCondition cond)
        {

        }

        public virtual void StartWhenSearchConditionClause(WhenSearchCondition condition)
        {

        }

        public virtual void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {

        }

        public virtual void StartWriteStatementConditional(WriteStatement stmt)
        {

        }

        public virtual void StartXmlGenerateStatementConditional(XmlGenerateStatement stmt)
        {

        }

        public virtual void StartXmlParseStatementConditional(XmlParseStatement stmt)
        {

        }

        // FOR SQL
        public void OnCommitStatement([NotNull] CommitStatement commit)
        {

        }
        public void OnSelectStatement([NotNull] SelectStatement select)
        {

        }
        public void OnRollbackStatement([NotNull] RollbackStatement rollback)
        {

        }
        public void OnTruncateStatement([NotNull] TruncateStatement truncate)
        {

        }
        public void OnWhenEverStatement([NotNull] WhenEverStatement whenEver)
        {

        }
        public void OnLockTableStatement([NotNull] LockTableStatement lockTable)
        {

        }
        public void OnSetAssignmentStatement([NotNull] SetAssignmentStatement setAssignment)
        {

        }
    }
}
