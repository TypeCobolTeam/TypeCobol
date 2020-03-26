using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// A Node Dispatcher on a ProgramClassBuilder that is able to also dispatch building actions.
    /// </summary>
    public class ProgramClassBuilderNodeDispatcher : NodeDispatcher, IProgramClassBuilder
    {
        private IList<ProgramClassBuilderNodeListener> _builderListeners = null;

        public ProgramClassBuilderNodeDispatcher()
        {
        }

        /// <summary>
        /// Overriden AddListener to filter those that are ProgramClassBuilderNodeListener instance.
        /// So that we can build a specific list, and avoid dynamic type checking at dispatch time.
        /// </summary>
        /// <param name="listener">The listener to be added</param>
        protected override void AddListener(NodeListener listener)
        {
            base.AddListener(listener);
            if (listener is ProgramClassBuilderNodeListener nodeListener)
            {
                if (_builderListeners == null)
                {
                    _builderListeners = new List<ProgramClassBuilderNodeListener>();
                }
                _builderListeners.Add(nodeListener);
            }
        }

        public virtual void OnLevel1Definition(DataDefinition level1Node)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnLevel1Definition(level1Node);
                }
        }

        public virtual void Enter(Node node)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.Enter(node);
                }
        }

        public virtual void Exit(Node node)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.Exit(node);
                }
        }

        public void AddNextSentenceStatement([NotNull] NextSentenceStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.AddNextSentenceStatement(stmt);
                }
        }

        public void CheckStartSentenceLastStatement()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.CheckStartSentenceLastStatement();
                }
        }

        public void EndAddStatementConditional(AddStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndAddStatementConditional(end);
                }
        }

        public void EndCallStatementConditional(CallStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndCallStatementConditional(end);
                }
        }

        public void EndCobolCompilationUnit()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndCobolCompilationUnit();
                }
        }

        public void EndCobolProgram(ProgramEnd end)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndCobolProgram(end);
                }
        }

        public void EndComputeStatementConditional(ComputeStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndComputeStatementConditional(end);
                }
        }

        public void EndConfigurationSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndConfigurationSection();
                }
        }

        public void EndDataDivision()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndDataDivision();
                }
        }

        public void EndDeclaratives([NotNull] DeclarativesEnd end)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndDeclaratives(end);
                }
        }

        public void EndDeleteStatementConditional(DeleteStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndDeleteStatementConditional(end);
                }
        }

        public void EndDivideStatementConditional(DivideStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndDivideStatementConditional(end);
                }
        }

        public void EndEnvironmentDivision()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndEnvironmentDivision();
                }
        }

        public void EndEvaluateStatementWithBody(EvaluateStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndEvaluateStatementWithBody(end);
                }
        }

        public void EndExecStatement()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndExecStatement();
                }
        }

        public void EndFileControlEntry()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFileControlEntry();
                }
        }

        public void EndFileControlParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFileControlParagraph();
                }
        }

        public void EndFileDescriptionEntry()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFileDescriptionEntry();
                }
        }

        public void EndFileDescriptionEntryIfAny()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFileDescriptionEntryIfAny();
                }
        }

        public void EndFileSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFileSection();
                }
        }

        public void EndFunctionDeclaration([NotNull] FunctionDeclarationEnd end)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFunctionDeclaration(end);
                }
        }

        public void EndFunctionProcedureDivision()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndFunctionProcedureDivision();
                }
        }

        public void EndGlobalStorageSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndGlobalStorageSection();
                }
        }

        public void EndIfStatementWithBody(IfStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndIfStatementWithBody();
                }
        }

        public void EndInputOutputSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndInputOutputSection();
                }
        }

        public void EndInvokeStatementConditional(InvokeStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndInvokeStatementConditional(end);
                }
        }

        public void EndJsonGenerateStatementConditional(JsonStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndJsonGenerateStatementConditional(end);
                }
        }

        public void EndJsonParseStatementConditional(JsonStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
            {
                l.EndJsonParseStatementConditional(end);
            }
        }

        public void EndLinkageSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndLinkageSection();
                }
        }

        public void EndLocalStorageSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndLocalStorageSection();
                }
        }

        public void EndMultiplyStatementConditional(MultiplyStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndMultiplyStatementConditional(end);
                }
        }

        public void EndNoAtEnd()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndNoAtEnd();
                }
        }

        public void EndNoException()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndNoException();
                }
        }

        public void EndNoInvalidKey()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndNoInvalidKey();
                }
        }

        public void EndNoOverflow()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndNoOverflow();
                }
        }

        public void EndNoSizeError()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndNoSizeError();
                }
        }

        public void EndObjectComputerParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndObjectComputerParagraph();
                }
        }

        public void EndOnAtEnd()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndOnAtEnd();
                }
        }

        public void EndOnException()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndOnException();
                }
        }

        public void EndOnInvalidKey()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndOnInvalidKey();
                }
        }

        public void EndOnOverflow()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndOnOverflow();
                }
        }

        public void EndOnSizeError()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndOnSizeError();
                }
        }

        public void EndParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndParagraph();
                }
        }

        public void EndPerformStatementWithBody(PerformStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndPerformStatementWithBody(end);
                }
        }

        public void EndProcedureDivision()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndProcedureDivision();
                }
        }

        public void EndReadStatementConditional(ReadStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndReadStatementConditional(end);
                }
        }

        public void EndRepositoryParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndRepositoryParagraph();
                }
        }

        public void EndReturnStatementConditional(ReturnStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndReturnStatementConditional(end);
                }
        }

        public void EndRewriteStatementConditional(RewriteStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndRewriteStatementConditional(end);
                }
        }

        public void EndSearchStatementWithBody(SearchStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSearchStatementWithBody(end);
                }
        }

        public void EndSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSection();
                }
        }

        public void EndSentence(SentenceEnd end, bool bCheck = false)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSentence(end, bCheck);
                }
        }

        public void EndSourceComputerParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSourceComputerParagraph();
                }
        }

        public void EndSpecialNamesParagraph()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSpecialNamesParagraph();
                }
        }

        public void EndStartStatementConditional(StartStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndStartStatementConditional(end);
                }
        }

        public void EndStringStatementConditional(StringStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndStringStatementConditional(end);
                }
        }

        public void EndSubtractStatementConditional(SubtractStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndSubtractStatementConditional(end);
                }
        }

        public void EndUnstringStatementConditional(UnstringStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndUnstringStatementConditional(end);
                }
        }

        public void EndWhenConditionClause()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndWhenConditionClause();
                }
        }

        public void EndWhenOtherClause()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndWhenOtherClause();
                }
        }

        public void EndWhenSearchConditionClause()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndWhenSearchConditionClause();
                }
        }

        public void EndWorkingStorageSection()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndWorkingStorageSection();
                }
        }

        public void EndWriteStatementConditional(WriteStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndWriteStatementConditional(end);
                }
        }

        public void EndXmlGenerateStatementConditional(XmlStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndXmlGenerateStatementConditional(end);
                }
        }

        public void EndXmlParseStatementConditional(XmlStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EndXmlParseStatementConditional(end);
                }
        }

        public void EnterElseClause(ElseCondition clause)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EnterElseClause(clause);
                }
        }

        public void EnterReadStatementConditional([NotNull] ReadStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EnterReadStatementConditional(stmt);
                }
        }

        public void EnterReturnStatementConditional([NotNull] ReturnStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EnterReturnStatementConditional(stmt);
                }
        }

        public void EnterUseStatement(UseStatement useStatement)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.EnterUseStatement(useStatement);
                }
        }

        public void OnAcceptStatement([NotNull] AcceptStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnAcceptStatement(stmt);
                }
        }

        public void OnAlterStatement([NotNull] AlterStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnAlterStatement(stmt);
                }
        }

        public void OnCancelStatement([NotNull] CancelStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnCancelStatement(stmt);
                }
        }

        public void OnCloseStatement([NotNull] CloseStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnCloseStatement(stmt);
                }
        }

        public void OnContinueStatement([NotNull] ContinueStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnContinueStatement(stmt);
                }
        }

        public void OnDisplayStatement([NotNull] DisplayStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnDisplayStatement(stmt);
                }
        }

        public void OnEntryStatement([NotNull] EntryStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnEntryStatement(stmt);
                }
        }

        public void OnExecStatement([NotNull] ExecStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnExecStatement(stmt);
                }
        }

        public void OnExitMethodStatement([NotNull] ExitMethodStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnExitMethodStatement(stmt);
                }
        }

        public void OnExitProgramStatement([NotNull] ExitProgramStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnExitProgramStatement(stmt);
                }
        }

        public void OnExitStatement([NotNull] ExitStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnExitStatement(stmt);
                }
        }

        public void OnAllocateStatement([NotNull] AllocateStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnAllocateStatement(stmt);
                }
        }

        public void OnFreeStatement([NotNull] FreeStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnFreeStatement(stmt);
                }
        }

        public void OnGobackStatement([NotNull] GobackStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnGobackStatement(stmt);
                }
        }

        public void OnGotoStatement([NotNull] GotoStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnGotoStatement(stmt);
                }
        }

        public void OnInitializeStatement([NotNull] InitializeStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnInitializeStatement(stmt);
                }
        }

        public void OnInspectStatement([NotNull] InspectStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnInspectStatement(stmt);
                }
        }

        public void OnMergeStatement([NotNull] MergeStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnMergeStatement(stmt);
                }
        }

        public void OnMoveStatement([NotNull] MoveStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnMoveStatement(stmt);
                }
        }

        public void OnOpenStatement([NotNull] OpenStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnOpenStatement(stmt);
                }
        }

        public void OnPerformProcedureStatement([NotNull] PerformProcedureStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnPerformProcedureStatement(stmt);
                }
        }

        public void OnProcedureStyleCall([NotNull] ProcedureStyleCallStatement stmt, CallStatementEnd end = null)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnProcedureStyleCall(stmt, end);
                }
        }

        public void OnReleaseStatement([NotNull] ReleaseStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnReleaseStatement(stmt);
                }
        }

        public void OnSetStatement([NotNull] SetStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnSetStatement(stmt);
                }
        }

        public void OnSortStatement([NotNull] SortStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnSortStatement(stmt);
                }
        }

        public void OnStopStatement([NotNull] StopStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.OnStopStatement(stmt);
                }
        }

        public void StartAddStatementConditional([NotNull] AddStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartAddStatementConditional(stmt);
                }
        }

        public void StartCallStatementConditional([NotNull] CallStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartCallStatementConditional(stmt);
                }
        }

        public void StartCobolCompilationUnit()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartCobolCompilationUnit();
                }
        }

        public void StartCobolProgram([NotNull] ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartCobolProgram(programIdentification, libraryCopy);
                }
        }

        public void StartComputeStatementConditional([NotNull] ComputeStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartComputeStatementConditional(stmt);
                }
        }

        public void StartConfigurationSection([NotNull] ConfigurationSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartConfigurationSection(header);
                }
        }

        public void StartDataConditionEntry([NotNull] DataConditionEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDataConditionEntry(entry);
                }
        }

        public void StartDataDescriptionEntry([NotNull] DataDescriptionEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDataDescriptionEntry(entry);
                }
        }

        public void StartDataDivision([NotNull] DataDivisionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDataDivision(header);
                }
        }

        public void StartDataRedefinesEntry([NotNull] DataRedefinesEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDataRedefinesEntry(entry);
                }
        }

        public void StartDataRenamesEntry([NotNull] DataRenamesEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDataRenamesEntry(entry);
                }
        }

        public void StartDeclaratives([NotNull] DeclarativesHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDeclaratives(header);
                }
        }

        public void StartDeleteStatementConditional([NotNull] DeleteStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDeleteStatementConditional(stmt);
                }
        }

        public void StartDivideStatementConditional([NotNull] DivideStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartDivideStatementConditional(stmt);
                }
        }

        public void StartEnvironmentDivision([NotNull] EnvironmentDivisionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartEnvironmentDivision(header);
                }
        }

        public void StartEvaluateStatementWithBody([NotNull] EvaluateStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartEvaluateStatementWithBody(stmt);
                }
        }

        public void StartExecStatement([NotNull] ExecStatement execStmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartExecStatement(execStmt);
                }
        }

        public void StartFileControlEntry([NotNull] FileControlEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFileControlEntry(entry);
                }
        }

        public void StartFileControlParagraph([NotNull] FileControlParagraphHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFileControlParagraph(header);
                }
        }

        public void StartFileDescriptionEntry([NotNull] FileDescriptionEntry entry)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFileDescriptionEntry(entry);
                }
        }

        public void StartFileSection([NotNull] FileSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFileSection(header);
                }
        }

        public void StartFunctionDeclaration([NotNull] FunctionDeclarationHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFunctionDeclaration(header);
                }
        }

        public void StartFunctionProcedureDivision([NotNull] ProcedureDivisionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartFunctionProcedureDivision(header);
                }
        }

        public void StartGlobalStorageSection([NotNull] GlobalStorageSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartGlobalStorageSection(header);
                }
        }

        public void StartIfStatementWithBody([NotNull] IfStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartIfStatementWithBody(stmt);
                }
        }

        public void StartInputOutputSection([NotNull] InputOutputSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartInputOutputSection(header);
                }
        }

        public void StartInvokeStatementConditional([NotNull] InvokeStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartInvokeStatementConditional(stmt);
                }
        }

        public void StartJsonGenerateStatementConditional([NotNull] JsonGenerateStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartJsonGenerateStatementConditional(stmt);
                }
        }

        public void StartJsonParseStatementConditional([NotNull] JsonParseStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
            {
                l.StartJsonParseStatementConditional(stmt);
            }
        }

        public void StartLinkageSection([NotNull] LinkageSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartLinkageSection(header);
                }
        }

        public void StartLocalStorageSection([NotNull] LocalStorageSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartLocalStorageSection(header);
                }
        }

        public void StartMultiplyStatementConditional([NotNull] MultiplyStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartMultiplyStatementConditional(stmt);
                }
        }

        public void StartNoAtEnd([NotNull] NotAtEndCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartNoAtEnd(cond);
                }
        }

        public void StartNoException([NotNull] NotOnExceptionCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartNoException(cond);
                }
        }

        public void StartNoInvalidKey([NotNull] NotInvalidKeyCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartNoInvalidKey(cond);
                }
        }

        public void StartNoOverflow([NotNull] NotOnOverflowCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartNoOverflow(cond);
                }
        }

        public void StartNoSizeError([NotNull] NotOnSizeErrorCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartNoSizeError(cond);
                }
        }

        public void StartObjectComputerParagraph([NotNull] ObjectComputerParagraph paragraph)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartObjectComputerParagraph(paragraph);
                }
        }

        public void StartOnAtEnd([NotNull] AtEndCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartOnAtEnd(cond);
                }
        }

        public void StartOnException([NotNull] OnExceptionCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartOnException(cond);
                }
        }

        public void StartOnInvalidKey([NotNull] InvalidKeyCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartOnInvalidKey(cond);
                }
        }

        public void StartOnOverflow([NotNull] OnOverflowCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartOnOverflow(cond);
                }
        }

        public void StartOnSizeError([NotNull] OnSizeErrorCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartOnSizeError(cond);
                }
        }

        public void StartParagraph([NotNull] ParagraphHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartParagraph(header);
                }
        }

        public void StartPerformStatementWithBody([NotNull] PerformStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartPerformStatementWithBody(stmt);
                }
        }

        public void StartProcedureDivision([NotNull] ProcedureDivisionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartProcedureDivision(header);
                }
        }

        public void StartRepositoryParagraph([NotNull] RepositoryParagraph paragraph)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartRepositoryParagraph(paragraph);
                }
        }

        public void StartRewriteStatementConditional([NotNull] RewriteStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartRewriteStatementConditional(stmt);
                }
        }

        public void StartSearchStatementWithBody([NotNull] SearchStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSearchStatementWithBody(stmt);
                }
        }

        public void StartSection([NotNull] SectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSection(header);
                }
        }

        public void StartSentence()
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSentence();
                }
        }

        public void StartSourceComputerParagraph([NotNull] SourceComputerParagraph paragraph)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSourceComputerParagraph(paragraph);
                }
        }

        public void StartSpecialNamesParagraph([NotNull] SpecialNamesParagraph paragraph)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSpecialNamesParagraph(paragraph);
                }
        }

        public void StartStartStatementConditional([NotNull] StartStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartStartStatementConditional(stmt);
                }
        }

        public void StartStringStatementConditional([NotNull] StringStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartStringStatementConditional(stmt);
                }
        }

        public void StartSubtractStatementConditional([NotNull] SubtractStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartSubtractStatementConditional(stmt);
                }
        }

        public void StartTypeDefinitionEntry([NotNull] DataTypeDescriptionEntry typedef)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartTypeDefinitionEntry(typedef);
                }
        }

        public void StartUnstringStatementConditional([NotNull] UnstringStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartUnstringStatementConditional(stmt);
                }
        }

        public void StartWhenConditionClause([NotNull] List<CodeElement> conditions)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartWhenConditionClause(conditions);
                }
        }

        public void StartWhenOtherClause([NotNull] WhenOtherCondition cond)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartWhenOtherClause(cond);
                }
        }

        public void StartWhenSearchConditionClause([NotNull] WhenSearchCondition condition)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartWhenSearchConditionClause(condition);
                }
        }

        public void StartWorkingStorageSection([NotNull] WorkingStorageSectionHeader header)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartWorkingStorageSection(header);
                }
        }

        public void StartWriteStatementConditional([NotNull] WriteStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartWriteStatementConditional(stmt);
                }
        }

        public void StartXmlGenerateStatementConditional([NotNull] XmlGenerateStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartXmlGenerateStatementConditional(stmt);
                }
        }

        public void StartXmlParseStatementConditional([NotNull] XmlParseStatement stmt)
        {
            if (_builderListeners != null) foreach (var l in _builderListeners)
                {
                    l.StartXmlParseStatementConditional(stmt);
                }
        }
    }
}
