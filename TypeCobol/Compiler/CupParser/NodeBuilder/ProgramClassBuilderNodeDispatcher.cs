using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// Implements IProgramClassBuilderNodeListener so as to dispatch calls to a list of listeners.
    /// </summary>
    public class ProgramClassBuilderNodeDispatcher : IProgramClassBuilderNodeListener
    {
        private readonly IList<IProgramClassBuilderNodeListener> _listeners;

        /// <summary>
        /// Creates a new ProgramClassBuilderNodeDispatcher without any listener attached.
        /// </summary>
        public ProgramClassBuilderNodeDispatcher()
        {
            _listeners = new List<IProgramClassBuilderNodeListener>();
        }

        /// <summary>
        /// Add a listener to this dispatcher.
        /// </summary>
        /// <param name="listener">A non-null instance of IProgramClassBuilderNodeListener.</param>
        public void AddListener([NotNull] IProgramClassBuilderNodeListener listener) => _listeners.Add(listener);

        public virtual void OnNode(Node node, Program program)
        {
            foreach (var listener in _listeners) listener.OnNode(node, program);
        }

        public virtual void StartCobolCompilationUnit()
        {
            foreach (var listener in _listeners) listener.StartCobolCompilationUnit();
        }

        public virtual void StartCobolProgram(ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
            foreach (var listener in _listeners) listener.StartCobolProgram(programIdentification, libraryCopy);
        }

        public virtual void EndCobolProgram(ProgramEnd end)
        {
            foreach (var listener in _listeners) listener.EndCobolProgram(end);
        }

        public virtual void StartEnvironmentDivision(EnvironmentDivisionHeader header)
        {
            foreach (var listener in _listeners) listener.StartEnvironmentDivision(header);
        }

        public virtual void EndEnvironmentDivision()
        {
            foreach (var listener in _listeners) listener.EndEnvironmentDivision();
        }

        public virtual void StartConfigurationSection(ConfigurationSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartConfigurationSection(header);
        }

        public virtual void EndConfigurationSection()
        {
            foreach (var listener in _listeners) listener.EndConfigurationSection();
        }

        public virtual void StartSourceComputerParagraph(SourceComputerParagraph paragraph)
        {
            foreach (var listener in _listeners) listener.StartSourceComputerParagraph(paragraph);
        }

        public virtual void EndSourceComputerParagraph()
        {
            foreach (var listener in _listeners) listener.EndSourceComputerParagraph();
        }

        public virtual void StartObjectComputerParagraph(ObjectComputerParagraph paragraph)
        {
            foreach (var listener in _listeners) listener.StartObjectComputerParagraph(paragraph);
        }

        public virtual void EndObjectComputerParagraph()
        {
            foreach (var listener in _listeners) listener.EndObjectComputerParagraph();
        }

        public virtual void StartSpecialNamesParagraph(SpecialNamesParagraph paragraph)
        {
            foreach (var listener in _listeners) listener.StartSpecialNamesParagraph(paragraph);
        }

        public virtual void EndSpecialNamesParagraph()
        {
            foreach (var listener in _listeners) listener.EndSpecialNamesParagraph();
        }

        public virtual void StartRepositoryParagraph(RepositoryParagraph paragraph)
        {
            foreach (var listener in _listeners) listener.StartRepositoryParagraph(paragraph);
        }

        public virtual void EndRepositoryParagraph()
        {
            foreach (var listener in _listeners) listener.EndRepositoryParagraph();
        }

        public virtual void StartInputOutputSection(InputOutputSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartInputOutputSection(header);
        }

        public virtual void EndInputOutputSection()
        {
            foreach (var listener in _listeners) listener.EndInputOutputSection();
        }

        public virtual void StartFileControlParagraph(FileControlParagraphHeader header)
        {
            foreach (var listener in _listeners) listener.StartFileControlParagraph(header);
        }

        public virtual void EndFileControlParagraph()
        {
            foreach (var listener in _listeners) listener.EndFileControlParagraph();
        }

        public virtual void StartFileControlEntry(FileControlEntry entry)
        {
            foreach (var listener in _listeners) listener.StartFileControlEntry(entry);
        }

        public virtual void EndFileControlEntry()
        {
            foreach (var listener in _listeners) listener.EndFileControlEntry();
        }

        public virtual void StartDataDivision(DataDivisionHeader header)
        {
            foreach (var listener in _listeners) listener.StartDataDivision(header);
        }

        public virtual void EndDataDivision()
        {
            foreach (var listener in _listeners) listener.EndDataDivision();
        }

        public virtual void StartFileSection(FileSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartFileSection(header);
        }

        public virtual void EndFileSection()
        {
            foreach (var listener in _listeners) listener.EndFileSection();
        }

        public virtual void StartFileDescriptionEntry(FileDescriptionEntry entry)
        {
            foreach (var listener in _listeners) listener.StartFileDescriptionEntry(entry);
        }

        public virtual void EndFileDescriptionEntry()
        {
            foreach (var listener in _listeners) listener.EndFileDescriptionEntry();
        }

        public virtual void EndFileDescriptionEntryIfAny()
        {
            foreach (var listener in _listeners) listener.EndFileDescriptionEntryIfAny();
        }

        public virtual void StartDataDescriptionEntry(DataDescriptionEntry entry)
        {
            foreach (var listener in _listeners) listener.StartDataDescriptionEntry(entry);
        }

        public virtual void StartDataRedefinesEntry(DataRedefinesEntry entry)
        {
            foreach (var listener in _listeners) listener.StartDataRedefinesEntry(entry);
        }

        public virtual void StartDataRenamesEntry(DataRenamesEntry entry)
        {
            foreach (var listener in _listeners) listener.StartDataRenamesEntry(entry);
        }

        public virtual void StartDataConditionEntry(DataConditionEntry entry)
        {
            foreach (var listener in _listeners) listener.StartDataConditionEntry(entry);
        }

        public virtual void StartTypeDefinitionEntry(DataTypeDescriptionEntry typedef)
        {
            foreach (var listener in _listeners) listener.StartTypeDefinitionEntry(typedef);
        }

        public virtual void StartWorkingStorageSection(WorkingStorageSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartWorkingStorageSection(header);
        }

        public virtual void EndWorkingStorageSection()
        {
            foreach (var listener in _listeners) listener.EndWorkingStorageSection();
        }

        public virtual void StartGlobalStorageSection(GlobalStorageSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartGlobalStorageSection(header);
        }

        public virtual void EndGlobalStorageSection()
        {
            foreach (var listener in _listeners) listener.EndGlobalStorageSection();
        }

        public virtual void StartLocalStorageSection(LocalStorageSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartLocalStorageSection(header);
        }

        public virtual void EndLocalStorageSection()
        {
            foreach (var listener in _listeners) listener.EndLocalStorageSection();
        }

        public virtual void StartLinkageSection(LinkageSectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartLinkageSection(header);
        }

        public virtual void EndLinkageSection()
        {
            foreach (var listener in _listeners) listener.EndLinkageSection();
        }

        public virtual void StartProcedureDivision(ProcedureDivisionHeader header)
        {
            foreach (var listener in _listeners) listener.StartProcedureDivision(header);
        }

        public virtual void EndProcedureDivision()
        {
            foreach (var listener in _listeners) listener.EndProcedureDivision();
        }

        public virtual void StartDeclaratives(DeclarativesHeader header)
        {
            foreach (var listener in _listeners) listener.StartDeclaratives(header);
        }

        public virtual void EndDeclaratives(DeclarativesEnd end)
        {
            foreach (var listener in _listeners) listener.EndDeclaratives(end);
        }

        public virtual void StartFunctionDeclaration(FunctionDeclarationHeader header)
        {
            foreach (var listener in _listeners) listener.StartFunctionDeclaration(header);
        }

        public virtual void EndFunctionDeclaration(FunctionDeclarationEnd end)
        {
            foreach (var listener in _listeners) listener.EndFunctionDeclaration(end);
        }

        public virtual void StartFunctionProcedureDivision(ProcedureDivisionHeader header)
        {
            foreach (var listener in _listeners) listener.StartFunctionProcedureDivision(header);
        }

        public virtual void EndFunctionProcedureDivision()
        {
            foreach (var listener in _listeners) listener.EndFunctionProcedureDivision();
        }

        public virtual void StartSection(SectionHeader header)
        {
            foreach (var listener in _listeners) listener.StartSection(header);
        }

        public virtual void EndSection()
        {
            foreach (var listener in _listeners) listener.EndSection();
        }

        public virtual void StartParagraph(ParagraphHeader header)
        {
            foreach (var listener in _listeners) listener.StartParagraph(header);
        }

        public virtual void EndParagraph()
        {
            foreach (var listener in _listeners) listener.EndParagraph();
        }

        public virtual void StartSentence()
        {
            foreach (var listener in _listeners) listener.StartSentence();
        }

        public virtual void EndSentence(SentenceEnd end, bool bCheck = false)
        {
            foreach (var listener in _listeners) listener.EndSentence(end, bCheck);
        }

        public virtual void CheckStartSentenceLastStatement()
        {
            foreach (var listener in _listeners) listener.CheckStartSentenceLastStatement();
        }

        public virtual void StartExecStatement(ExecStatement execStmt)
        {
            foreach (var listener in _listeners) listener.StartExecStatement(execStmt);
        }

        public virtual void OnExecStatementText(ExecStatementText execText)
        {
            foreach (var listener in _listeners) listener.OnExecStatementText(execText);
        }

        public virtual void EndExecStatement(ExecStatementEnd end)
        {
            foreach (var listener in _listeners) listener.EndExecStatement(end);
        }

        public virtual void EnterUseStatement(UseStatement useStatement)
        {
            foreach (var listener in _listeners) listener.EnterUseStatement(useStatement);
        }

        public virtual void OnContinueStatement(ContinueStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnContinueStatement(stmt);
        }

        public virtual void OnEntryStatement(EntryStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnEntryStatement(stmt);
        }

        public virtual void OnAcceptStatement(AcceptStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnAcceptStatement(stmt);
        }

        public virtual void OnInitializeStatement(InitializeStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnInitializeStatement(stmt);
        }

        public virtual void OnInspectStatement(InspectStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnInspectStatement(stmt);
        }

        public virtual void OnMoveStatement(MoveStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnMoveStatement(stmt);
        }

        public virtual void OnSetStatement(SetStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnSetStatement(stmt);
        }

        public virtual void OnStopStatement(StopStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnStopStatement(stmt);
        }

        public virtual void OnExitMethodStatement(ExitMethodStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnExitMethodStatement(stmt);
        }

        public virtual void OnExitProgramStatement(ExitProgramStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnExitProgramStatement(stmt);
        }

        public virtual void OnAllocateStatement(AllocateStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnAllocateStatement(stmt);
        }

        public virtual void OnFreeStatement(FreeStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnFreeStatement(stmt);
        }

        public virtual void OnGobackStatement(GobackStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnGobackStatement(stmt);
        }

        public virtual void OnCloseStatement(CloseStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnCloseStatement(stmt);
        }

        public virtual void OnDisplayStatement(DisplayStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnDisplayStatement(stmt);
        }

        public virtual void OnOpenStatement(OpenStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnOpenStatement(stmt);
        }

        public virtual void OnMergeStatement(MergeStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnMergeStatement(stmt);
        }

        public virtual void OnReleaseStatement(ReleaseStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnReleaseStatement(stmt);
        }

        public virtual void OnSortStatement(SortStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnSortStatement(stmt);
        }

        public virtual void OnAlterStatement(AlterStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnAlterStatement(stmt);
        }

        public virtual void OnExitStatement(ExitStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnExitStatement(stmt);
        }

        public virtual void OnGotoStatement(GotoStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnGotoStatement(stmt);
        }

        public virtual void OnPerformProcedureStatement(PerformProcedureStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnPerformProcedureStatement(stmt);
        }

        public virtual void OnCancelStatement(CancelStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnCancelStatement(stmt);
        }

        public virtual void OnProcedureStyleCall(ProcedureStyleCallStatement stmt, CallStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.OnProcedureStyleCall(stmt, end);
        }

        public virtual void OnExecStatement(ExecStatement stmt)
        {
            foreach (var listener in _listeners) listener.OnExecStatement(stmt);
        }

        public virtual void StartAddStatementConditional(AddStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartAddStatementConditional(stmt);
        }

        public virtual void EndAddStatementConditional(AddStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndAddStatementConditional(end);
        }

        public virtual void StartCallStatementConditional(CallStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartCallStatementConditional(stmt);
        }

        public virtual void EndCallStatementConditional(CallStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndCallStatementConditional(end);
        }

        public virtual void StartComputeStatementConditional(ComputeStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartComputeStatementConditional(stmt);
        }

        public virtual void EndComputeStatementConditional(ComputeStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndComputeStatementConditional(end);
        }

        public virtual void StartDeleteStatementConditional(DeleteStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartDeleteStatementConditional(stmt);
        }

        public virtual void EndDeleteStatementConditional(DeleteStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndDeleteStatementConditional(end);
        }

        public virtual void StartDivideStatementConditional(DivideStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartDivideStatementConditional(stmt);
        }

        public virtual void EndDivideStatementConditional(DivideStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndDivideStatementConditional(end);
        }

        public virtual void StartEvaluateStatementWithBody(EvaluateStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartEvaluateStatementWithBody(stmt);
        }

        public virtual void EndEvaluateStatementWithBody(EvaluateStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndEvaluateStatementWithBody(end);
        }

        public virtual void StartWhenConditionClause(List<CodeElement> conditions)
        {
            foreach (var listener in _listeners) listener.StartWhenConditionClause(conditions);
        }

        public virtual void EndWhenConditionClause()
        {
            foreach (var listener in _listeners) listener.EndWhenConditionClause();
        }

        public virtual void StartWhenOtherClause(WhenOtherCondition cond)
        {
            foreach (var listener in _listeners) listener.StartWhenOtherClause(cond);
        }

        public virtual void EndWhenOtherClause()
        {
            foreach (var listener in _listeners) listener.EndWhenOtherClause();
        }

        public virtual void StartIfStatementWithBody(IfStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartIfStatementWithBody(stmt);
        }

        public virtual void EnterElseClause(ElseCondition clause)
        {
            foreach (var listener in _listeners) listener.EnterElseClause(clause);
        }

        public virtual void EndIfStatementWithBody(IfStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndIfStatementWithBody(end);
        }

        public virtual void AddNextSentenceStatement(NextSentenceStatement stmt)
        {
            foreach (var listener in _listeners) listener.AddNextSentenceStatement(stmt);
        }

        public virtual void StartInvokeStatementConditional(InvokeStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartInvokeStatementConditional(stmt);
        }

        public virtual void EndInvokeStatementConditional(InvokeStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndInvokeStatementConditional(end);
        }

        public virtual void StartJsonGenerateStatementConditional(JsonGenerateStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartJsonGenerateStatementConditional(stmt);
        }

        public virtual void EndJsonGenerateStatementConditional(JsonStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndJsonGenerateStatementConditional(end);
        }

        public virtual void StartJsonParseStatementConditional(JsonParseStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartJsonParseStatementConditional(stmt);
        }

        public virtual void EndJsonParseStatementConditional(JsonStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndJsonParseStatementConditional(end);
        }

        public virtual void StartMultiplyStatementConditional(MultiplyStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartMultiplyStatementConditional(stmt);
        }

        public virtual void EndMultiplyStatementConditional(MultiplyStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndMultiplyStatementConditional(end);
        }

        public virtual void StartPerformStatementWithBody(PerformStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartPerformStatementWithBody(stmt);
        }

        public virtual void EndPerformStatementWithBody(PerformStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndPerformStatementWithBody(end);
        }

        public virtual void StartSearchStatementWithBody(SearchStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartSearchStatementWithBody(stmt);
        }

        public virtual void EndSearchStatementWithBody(SearchStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndSearchStatementWithBody(end);
        }

        public virtual void StartWhenSearchConditionClause(WhenSearchCondition condition)
        {
            foreach (var listener in _listeners) listener.StartWhenSearchConditionClause(condition);
        }

        public virtual void EndWhenSearchConditionClause()
        {
            foreach (var listener in _listeners) listener.EndWhenSearchConditionClause();
        }

        public virtual void EnterReadStatementConditional(ReadStatement stmt)
        {
            foreach (var listener in _listeners) listener.EnterReadStatementConditional(stmt);
        }

        public virtual void EndReadStatementConditional(ReadStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndReadStatementConditional(end);
        }

        public virtual void EnterReturnStatementConditional(ReturnStatement stmt)
        {
            foreach (var listener in _listeners) listener.EnterReturnStatementConditional(stmt);
        }

        public virtual void EndReturnStatementConditional(ReturnStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndReturnStatementConditional(end);
        }

        public virtual void StartRewriteStatementConditional(RewriteStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartRewriteStatementConditional(stmt);
        }

        public virtual void EndRewriteStatementConditional(RewriteStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndRewriteStatementConditional(end);
        }

        public virtual void StartStartStatementConditional(StartStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartStartStatementConditional(stmt);
        }

        public virtual void EndStartStatementConditional(StartStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndStartStatementConditional(end);
        }

        public virtual void StartStringStatementConditional(StringStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartStringStatementConditional(stmt);
        }

        public virtual void EndStringStatementConditional(StringStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndStringStatementConditional(end);
        }

        public virtual void StartSubtractStatementConditional(SubtractStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartSubtractStatementConditional(stmt);
        }

        public virtual void EndSubtractStatementConditional(SubtractStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndSubtractStatementConditional(end);
        }

        public virtual void StartUnstringStatementConditional(UnstringStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartUnstringStatementConditional(stmt);
        }

        public virtual void EndUnstringStatementConditional(UnstringStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndUnstringStatementConditional(end);
        }

        public virtual void StartWriteStatementConditional(WriteStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartWriteStatementConditional(stmt);
        }

        public virtual void EndWriteStatementConditional(WriteStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndWriteStatementConditional(end);
        }

        public virtual void StartXmlGenerateStatementConditional(XmlGenerateStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartXmlGenerateStatementConditional(stmt);
        }

        public virtual void EndXmlGenerateStatementConditional(XmlStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndXmlGenerateStatementConditional(end);
        }

        public virtual void StartXmlParseStatementConditional(XmlParseStatement stmt)
        {
            foreach (var listener in _listeners) listener.StartXmlParseStatementConditional(stmt);
        }

        public virtual void EndXmlParseStatementConditional(XmlStatementEnd end = null)
        {
            foreach (var listener in _listeners) listener.EndXmlParseStatementConditional(end);
        }

        public virtual void StartOnSizeError(OnSizeErrorCondition cond)
        {
            foreach (var listener in _listeners) listener.StartOnSizeError(cond);
        }

        public virtual void EndOnSizeError()
        {
            foreach (var listener in _listeners) listener.EndOnSizeError();
        }

        public virtual void StartNoSizeError(NotOnSizeErrorCondition cond)
        {
            foreach (var listener in _listeners) listener.StartNoSizeError(cond);
        }

        public virtual void EndNoSizeError()
        {
            foreach (var listener in _listeners) listener.EndNoSizeError();
        }

        public virtual void StartOnException(OnExceptionCondition cond)
        {
            foreach (var listener in _listeners) listener.StartOnException(cond);
        }

        public virtual void EndOnException()
        {
            foreach (var listener in _listeners) listener.EndOnException();
        }

        public virtual void StartNoException(NotOnExceptionCondition cond)
        {
            foreach (var listener in _listeners) listener.StartNoException(cond);
        }

        public virtual void EndNoException()
        {
            foreach (var listener in _listeners) listener.EndNoException();
        }

        public virtual void StartOnOverflow(OnOverflowCondition cond)
        {
            foreach (var listener in _listeners) listener.StartOnOverflow(cond);
        }

        public virtual void EndOnOverflow()
        {
            foreach (var listener in _listeners) listener.EndOnOverflow();
        }

        public virtual void StartNoOverflow(NotOnOverflowCondition cond)
        {
            foreach (var listener in _listeners) listener.StartNoOverflow(cond);
        }

        public virtual void EndNoOverflow()
        {
            foreach (var listener in _listeners) listener.EndNoOverflow();
        }

        public virtual void StartOnInvalidKey(InvalidKeyCondition cond)
        {
            foreach (var listener in _listeners) listener.StartOnInvalidKey(cond);
        }

        public virtual void EndOnInvalidKey()
        {
            foreach (var listener in _listeners) listener.EndOnInvalidKey();
        }

        public virtual void StartNoInvalidKey(NotInvalidKeyCondition cond)
        {
            foreach (var listener in _listeners) listener.StartNoInvalidKey(cond);
        }

        public virtual void EndNoInvalidKey()
        {
            foreach (var listener in _listeners) listener.EndNoInvalidKey();
        }

        public virtual void StartOnAtEnd(AtEndCondition cond)
        {
            foreach (var listener in _listeners) listener.StartOnAtEnd(cond);
        }

        public virtual void EndOnAtEnd()
        {
            foreach (var listener in _listeners) listener.EndOnAtEnd();
        }

        public virtual void StartNoAtEnd(NotAtEndCondition cond)
        {
            foreach (var listener in _listeners) listener.StartNoAtEnd(cond);
        }

        public virtual void EndNoAtEnd()
        {
            foreach (var listener in _listeners) listener.EndNoAtEnd();
        }

        public virtual void Enter(Node node)
        {
            foreach (var listener in _listeners) listener.Enter(node);
        }

        public virtual void Exit(Node node)
        {
            foreach (var listener in _listeners) listener.Exit(node);
        }

        public virtual void OnLevel1Definition(DataDefinition level1Node)
        {
            foreach (var listener in _listeners) listener.OnLevel1Definition(level1Node);
        }

        // FOR SQL
        public void OnCommitStatement([NotNull] CommitStatement commit)
        {
            foreach (var listener in _listeners) listener.OnCommitStatement(commit);
        }
        public void OnSelectStatement([NotNull] SelectStatement select)
        {
            foreach (var listener in _listeners) listener.OnSelectStatement(select);
        }
        public void OnRollbackStatement([NotNull] RollbackStatement rollback)
        {
            foreach (var listener in _listeners) listener.OnRollbackStatement(rollback);
        }
        public void OnTruncateStatement([NotNull] TruncateStatement truncate)
        {
            foreach (var listener in _listeners) listener.OnTruncateStatement(truncate);
        }
        public void OnSavepointStatement([NotNull] SavepointStatement savepoint)
        {
            foreach (var listener in _listeners) listener.OnSavepointStatement(savepoint);
        }

        public void OnWhenEverStatement(WhenEverStatement whenEver)
        {
            foreach (var listener in _listeners) listener.OnWhenEverStatement(whenEver);
        }
        public void OnLockTableStatement([NotNull] LockTableStatement lockTable)
        {
            foreach (var listener in _listeners) listener.OnLockTableStatement(lockTable);
        }
        public void OnReleaseSavepointStatement([NotNull] ReleaseSavepointStatement releaseSavepoint)
        {
            foreach (var listener in _listeners) listener.OnReleaseSavepointStatement(releaseSavepoint);
        }
    }
}
