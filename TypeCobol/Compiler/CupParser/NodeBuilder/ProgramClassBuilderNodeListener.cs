using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// A Node Listener that can also dispatch Program Class Builder actions
    /// </summary>
    public class ProgramClassBuilderNodeListener : NodeListener, IProgramClassBuilder
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

        public virtual void AddNextSentenceStatement([NotNull] NextSentenceStatement stmt)
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

        public virtual void EndCobolCompilationUnit()
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

        public virtual void EndDeclaratives([NotNull] DeclarativesEnd end)
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

        public virtual void EndFunctionDeclaration([NotNull] FunctionDeclarationEnd end)
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

        public virtual void EnterReadStatementConditional([NotNull] ReadStatement stmt)
        {

        }

        public virtual void EnterReturnStatementConditional([NotNull] ReturnStatement stmt)
        {

        }

        public virtual void EnterUseStatement(UseStatement useStatement)
        {

        }

        public virtual void OnAcceptStatement([NotNull] AcceptStatement stmt)
        {

        }

        public virtual void OnAlterStatement([NotNull] AlterStatement stmt)
        {

        }

        public virtual void OnCancelStatement([NotNull] CancelStatement stmt)
        {

        }

        public virtual void OnCloseStatement([NotNull] CloseStatement stmt)
        {

        }

        public virtual void OnContinueStatement([NotNull] ContinueStatement stmt)
        {

        }

        public virtual void OnDisplayStatement([NotNull] DisplayStatement stmt)
        {

        }

        public virtual void OnEntryStatement([NotNull] EntryStatement stmt)
        {

        }

        public virtual void OnExecStatement([NotNull] ExecStatement stmt)
        {

        }

        public virtual void OnExitMethodStatement([NotNull] ExitMethodStatement stmt)
        {

        }

        public virtual void OnExitProgramStatement([NotNull] ExitProgramStatement stmt)
        {

        }

        public virtual void OnExitStatement([NotNull] ExitStatement stmt)
        {

        }

        public virtual void OnAllocateStatement([NotNull] AllocateStatement stmt)
        {

        }

        public virtual void OnFreeStatement([NotNull] FreeStatement stmt)
        {

        }

        public virtual void OnGobackStatement([NotNull] GobackStatement stmt)
        {

        }

        public virtual void OnGotoStatement([NotNull] GotoStatement stmt)
        {

        }

        public virtual void OnInitializeStatement([NotNull] InitializeStatement stmt)
        {

        }

        public virtual void OnInspectStatement([NotNull] InspectStatement stmt)
        {

        }

        public virtual void OnMergeStatement([NotNull] MergeStatement stmt)
        {

        }

        public virtual void OnMoveStatement([NotNull] MoveStatement stmt)
        {

        }

        public virtual void OnOpenStatement([NotNull] OpenStatement stmt)
        {

        }

        public virtual void OnPerformProcedureStatement([NotNull] PerformProcedureStatement stmt)
        {

        }

        public virtual void OnProcedureStyleCall([NotNull] ProcedureStyleCallStatement stmt, CallStatementEnd end = null)
        {

        }

        public virtual void OnReleaseStatement([NotNull] ReleaseStatement stmt)
        {

        }

        public virtual void OnSetStatement([NotNull] SetStatement stmt)
        {

        }

        public virtual void OnSortStatement([NotNull] SortStatement stmt)
        {

        }

        public virtual void OnStopStatement([NotNull] StopStatement stmt)
        {

        }

        public virtual void StartAddStatementConditional([NotNull] AddStatement stmt)
        {

        }

        public virtual void StartCallStatementConditional([NotNull] CallStatement stmt)
        {

        }

        public virtual void StartCobolCompilationUnit()
        {

        }

        public virtual void StartCobolProgram([NotNull] ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {

        }

        public virtual void StartComputeStatementConditional([NotNull] ComputeStatement stmt)
        {

        }

        public virtual void StartConfigurationSection([NotNull] ConfigurationSectionHeader header)
        {

        }

        public virtual void StartDataConditionEntry([NotNull] DataConditionEntry entry)
        {

        }

        public virtual void StartDataDescriptionEntry([NotNull] DataDescriptionEntry entry)
        {

        }

        public virtual void StartDataDivision([NotNull] DataDivisionHeader header)
        {

        }

        public virtual void StartDataRedefinesEntry([NotNull] DataRedefinesEntry entry)
        {

        }

        public virtual void StartDataRenamesEntry([NotNull] DataRenamesEntry entry)
        {

        }

        public virtual void StartDeclaratives([NotNull] DeclarativesHeader header)
        {

        }

        public virtual void StartDeleteStatementConditional([NotNull] DeleteStatement stmt)
        {

        }

        public virtual void StartDivideStatementConditional([NotNull] DivideStatement stmt)
        {

        }

        public virtual void StartEnvironmentDivision([NotNull] EnvironmentDivisionHeader header)
        {

        }

        public virtual void StartEvaluateStatementWithBody([NotNull] EvaluateStatement stmt)
        {

        }

        public virtual void StartExecStatement([NotNull] ExecStatement execStmt)
        {

        }

        public virtual void StartFileControlEntry([NotNull] FileControlEntry entry)
        {

        }

        public virtual void StartFileControlParagraph([NotNull] FileControlParagraphHeader header)
        {

        }

        public virtual void StartFileDescriptionEntry([NotNull] FileDescriptionEntry entry)
        {

        }

        public virtual void StartFileSection([NotNull] FileSectionHeader header)
        {

        }

        public virtual void StartFunctionDeclaration([NotNull] FunctionDeclarationHeader header)
        {

        }

        public virtual void StartFunctionProcedureDivision([NotNull] ProcedureDivisionHeader header)
        {

        }

        public virtual void StartGlobalStorageSection([NotNull] GlobalStorageSectionHeader header)
        {

        }

        public virtual void StartIfStatementWithBody([NotNull] IfStatement stmt)
        {

        }

        public virtual void StartInputOutputSection([NotNull] InputOutputSectionHeader header)
        {

        }

        public virtual void StartInvokeStatementConditional([NotNull] InvokeStatement stmt)
        {

        }

        public virtual void StartJsonGenerateStatementConditional([NotNull] JsonGenerateStatement stmt)
        {

        }

        public virtual void StartJsonParseStatementConditional([NotNull] JsonParseStatement stmt)
        {

        }

        public virtual void StartLinkageSection([NotNull] LinkageSectionHeader header)
        {

        }

        public virtual void StartLocalStorageSection([NotNull] LocalStorageSectionHeader header)
        {

        }

        public virtual void StartMultiplyStatementConditional([NotNull] MultiplyStatement stmt)
        {

        }

        public virtual void StartNoAtEnd([NotNull] NotAtEndCondition cond)
        {

        }

        public virtual void StartNoException([NotNull] NotOnExceptionCondition cond)
        {

        }

        public virtual void StartNoInvalidKey([NotNull] NotInvalidKeyCondition cond)
        {

        }

        public virtual void StartNoOverflow([NotNull] NotOnOverflowCondition cond)
        {

        }

        public virtual void StartNoSizeError([NotNull] NotOnSizeErrorCondition cond)
        {

        }

        public virtual void StartObjectComputerParagraph([NotNull] ObjectComputerParagraph paragraph)
        {

        }

        public virtual void StartOnAtEnd([NotNull] AtEndCondition cond)
        {

        }

        public virtual void StartOnException([NotNull] OnExceptionCondition cond)
        {

        }

        public virtual void StartOnInvalidKey([NotNull] InvalidKeyCondition cond)
        {

        }

        public virtual void StartOnOverflow([NotNull] OnOverflowCondition cond)
        {

        }

        public virtual void StartOnSizeError([NotNull] OnSizeErrorCondition cond)
        {

        }

        public virtual void StartParagraph([NotNull] ParagraphHeader header)
        {

        }

        public virtual void StartPerformStatementWithBody([NotNull] PerformStatement stmt)
        {

        }

        public virtual void StartProcedureDivision([NotNull] ProcedureDivisionHeader header)
        {

        }

        public virtual void StartRepositoryParagraph([NotNull] RepositoryParagraph paragraph)
        {

        }

        public virtual void StartRewriteStatementConditional([NotNull] RewriteStatement stmt)
        {

        }

        public virtual void StartSearchStatementWithBody([NotNull] SearchStatement stmt)
        {

        }

        public virtual void StartSection([NotNull] SectionHeader header)
        {

        }

        public virtual void StartSentence()
        {

        }

        public virtual void StartSourceComputerParagraph([NotNull] SourceComputerParagraph paragraph)
        {

        }

        public virtual void StartSpecialNamesParagraph([NotNull] SpecialNamesParagraph paragraph)
        {

        }

        public virtual void StartStartStatementConditional([NotNull] StartStatement stmt)
        {

        }

        public virtual void StartStringStatementConditional([NotNull] StringStatement stmt)
        {

        }

        public virtual void StartSubtractStatementConditional([NotNull] SubtractStatement stmt)
        {

        }

        public virtual void StartTypeDefinitionEntry([NotNull] DataTypeDescriptionEntry typedef)
        {

        }

        public virtual void StartUnstringStatementConditional([NotNull] UnstringStatement stmt)
        {

        }

        public virtual void StartWhenConditionClause([NotNull] List<CodeElement> conditions)
        {

        }

        public virtual void StartWhenOtherClause([NotNull] WhenOtherCondition cond)
        {

        }

        public virtual void StartWhenSearchConditionClause([NotNull] WhenSearchCondition condition)
        {

        }

        public virtual void StartWorkingStorageSection([NotNull] WorkingStorageSectionHeader header)
        {

        }

        public virtual void StartWriteStatementConditional([NotNull] WriteStatement stmt)
        {

        }

        public virtual void StartXmlGenerateStatementConditional([NotNull] XmlGenerateStatement stmt)
        {

        }

        public virtual void StartXmlParseStatementConditional([NotNull] XmlParseStatement stmt)
        {

        }
    }
}
