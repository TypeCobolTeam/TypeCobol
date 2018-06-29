using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// A Type List that contains Code Element items.
    /// </summary>
    public class CodeElementList : List<TypeCobol.Compiler.CodeElements.CodeElement>
    {
        /// <summary>
        /// Empty constructor.
        /// </summary>
        public CodeElementList()
        {
            
        }
    };

    /// <summary>
    /// Interface of a Program class builder based
    /// </summary>
    public interface IProgramClassBuilder
    {
        /// <summary>
        /// Starts a compilation unit.
        /// </summary>
        void StartCobolCompilationUnit();

        /// <summary>
        /// Start a Cobol Program
        /// </summary>
        /// <param name="programIdentification">The Program Identification Code Element</param>
        /// <param name="libraryCopy">The Library Copy Code Element</param>
        void StartCobolProgram([NotNull]ProgramIdentification programIdentification, LibraryCopyCodeElement libraryCopy);

        /// <summary>
        /// End The current Cobol Program
        /// <param name="end">Optional ProgramEnd code elemnt</param>
        /// </summary>
        void EndCobolProgram(ProgramEnd end);

        /// <summary>
        /// Start an Environment Division 
        /// </summary>
        /// <param name="header">EnvironmentDivision Headercode element</param>
        void StartEnvironmentDivision([NotNull]EnvironmentDivisionHeader header);

        /// <summary>
        /// End an Environment Division 
        /// </summary>
        void EndEnvironmentDivision();

        /// <summary>
        /// Start a Configuration Section
        /// </summary>
        /// <param name="header">Configuration Section heser code element</param>
        void StartConfigurationSection([NotNull]ConfigurationSectionHeader header);

        /// <summary>
        /// End a configuration section header.
        /// </summary>
        void EndConfigurationSection();

        /// <summary>
        /// Start a Source Computer Paragraph
        /// </summary>
        /// <param name="paragraph">The Source Computer paragraph code element</param>
        void StartSourceComputerParagraph([NotNull]SourceComputerParagraph paragraph);

        /// <summary>
        /// End a Source Computer Paragraph
        /// </summary>
        void EndSourceComputerParagraph();

        /// <summary>
        /// Start a Object Computer Paragraph
        /// </summary>
        /// <param name="paragraph">The Object Computer paragraph code element</param>
        void StartObjectComputerParagraph([NotNull]ObjectComputerParagraph paragraph);

        /// <summary>
        /// End a Object Computer Paragraph
        /// </summary>
        void EndObjectComputerParagraph();

        /// <summary>
        /// Start a Special Names Paragraph
        /// </summary>
        /// <param name="paragraph">The Special Names paragraph code element</param>
        void StartSpecialNamesParagraph([NotNull]SpecialNamesParagraph paragraph);

        /// <summary>
        /// End a Special Names Paragraph
        /// </summary>
        void EndSpecialNamesParagraph();

        /// <summary>
        /// Start a Repository Paragraph
        /// </summary>
        /// <param name="paragraph">The Repository paragraph code element</param>
        void StartRepositoryParagraph([NotNull]RepositoryParagraph paragraph);

        /// <summary>
        /// End a Repository Paragraph
        /// </summary>
        void EndRepositoryParagraph();

        /// <summary>
        /// Start an Input Output Section
        /// </summary>
        /// <param name="header">The Input Output Section header code element</param>
        void StartInputOutputSection([NotNull] InputOutputSectionHeader header);

        /// <summary>
        /// End an Input Output Section
        /// </summary>
        void EndInputOutputSection();

        /// <summary>
        /// Start a File Control Paragraph
        /// </summary>
        /// <param name="header">The File Control Paragraph header code element</param>
        void StartFileControlParagraph([NotNull] FileControlParagraphHeader header);

        /// <summary>
        /// End a File Control Paragraph
        /// </summary>
        void EndFileControlParagraph();

        /// <summary>
        /// Start a File Control Entry
        /// </summary>
        /// <param name="entry">The File Control Entry code element</param>
        void StartFileControlEntry([NotNull]FileControlEntry entry);

        /// <summary>
        /// End a File Control Entry
        /// </summary>
        void EndFileControlEntry();

        /// <summary>
        /// Start a data division
        /// </summary>
        /// <param name="header">Data Division header Code Element</param>
        void StartDataDivision([NotNull]DataDivisionHeader header);

        /// <summary>
        /// End a Data division
        /// </summary>
        void EndDataDivision();

        /// <summary>
        /// parent: DATA DIVISION
        /// Start a File Section
        /// </summary>
        /// <param name="header">File Section code element</param>
        void StartFileSection([NotNull]FileSectionHeader header);

        /// <summary>
        /// End a File Section
        /// </summary>
        void EndFileSection();

        /// <summary>
        /// Start a File Description Entry
        /// </summary>
        /// <param name="entry">The File Description Entry code element</param>
        void StartFileDescriptionEntry([NotNull] FileDescriptionEntry entry);

        /// <summary>
        /// End a File Description Entry
        /// </summary>
        void EndFileDescriptionEntry();

        /// <summary>
        /// End a File Description Entry if one is pending
        /// </summary>
        void EndFileDescriptionEntryIfAny();

        /// <summary>
        /// Start DataDescriptionEntry
        /// </summary>
        /// <param name="entry">The DataDescriptionEntry code element</param>
        void StartDataDescriptionEntry([NotNull] DataDescriptionEntry entry);
        /// <summary>
        /// Start DataRedefinesEntry
        /// </summary>
        /// <param name="entry">The DataRedefinesEntry code element</param>
        void StartDataRedefinesEntry([NotNull] DataRedefinesEntry entry);
        /// <summary>
        /// Start DataRenamesEntry
        /// </summary>
        /// <param name="entry">The DataRenamesEntry code element</param>
        void StartDataRenamesEntry([NotNull] DataRenamesEntry entry);
        /// <summary>
        /// Start DataConditionEntry
        /// </summary>
        /// <param name="entry">The DataConditionEntry code element</param>
        void StartDataConditionEntry([NotNull] DataConditionEntry entry);

        /// <summary>
        /// Start a TypeCobol Type Definition entry
        /// </summary>
        /// <param name="typedef">The Type Definition Entry code element</param>
        void StartTypeDefinitionEntry([NotNull] DataTypeDescriptionEntry typedef);

        /// <summary>
        /// Start a WorkingStorageSection
        /// </summary>
        /// <param name="header">WorkingStorageSection Header code element</param>
        void StartWorkingStorageSection([NotNull] WorkingStorageSectionHeader header);

        /// <summary>
        /// End a WorkingStorageSection
        /// </summary>
        void EndWorkingStorageSection();

        /// <summary>
        /// Start a LocalStorageSection
        /// </summary>
        /// <param name="header">LocalStorageSection Header code element</param>
        void StartLocalStorageSection([NotNull] LocalStorageSectionHeader header);

        /// <summary>
        /// End a WorkingStorageSection
        /// </summary>
        void EndLocalStorageSection();

        /// <summary>
        /// Start a LinkageSection
        /// </summary>
        /// <param name="header">LinkageSection Header code element</param>
        void StartLinkageSection([NotNull] LinkageSectionHeader header);

        /// <summary>
        /// End a LinkageSection
        /// </summary>
        void EndLinkageSection();

        /// <summary>
        /// Start a Procedure Division
        /// </summary>
        /// <param name="header">The Procedure Division header code element</param>
        void StartProcedureDivision([NotNull] ProcedureDivisionHeader header);

        /// <summary>
        /// End a Procedure Division
        /// </summary>
        void EndProcedureDivision();

        /// <summary>
        /// Start Declaratives
        /// </summary>
        /// <param name="header">DeclarativesHeader code element</param>
        void StartDeclaratives([NotNull] DeclarativesHeader header);

        /// <summary>
        /// End Declaratives
        /// <param name="end">The DeclarativesEnd code element</param>
        /// </summary>
        void EndDeclaratives([NotNull] DeclarativesEnd end);

        /// <summary>
        /// Parent node: PROCEDURE DIVISION
        /// Start a DECLARE FUNCTION
        /// </summary>
        /// <param name="header">The Function Declaration Header cod element</param>
        void StartFunctionDeclaration([NotNull] FunctionDeclarationHeader header);

        /// <summary>
        /// Parent node: PROCEDURE DIVISION
        /// End a FUNCTION DECLARATION
        /// </summary>
        /// <param name="end">The FUNCTION DECLARE end code element</param>
        void EndFunctionDeclaration([NotNull] FunctionDeclarationEnd end);

        /// <summary>
        /// Parent node: DECLARE FUNCTION
        /// Start a Function PROCEDURE DIVISION
        /// </summary>
        /// <param name="header">The Function PROCEDURE DIVISION header code element</param>
        void StartFunctionProcedureDivision([NotNull] ProcedureDivisionHeader header);

        /// <summary>
        /// End a Function PROCEDURE DIVISION
        /// </summary>
        void EndFunctionProcedureDivision();

        /// <summary>
        /// Start a Section
        /// </summary>
        /// <param name="header">The section header code element</param>
        void StartSection([NotNull] SectionHeader header);

        /// <summary>
        /// End a Section
        /// </summary>
        void EndSection();

        /// <summary>
        /// Start a Paragraph
        /// </summary>
        /// <param name="header">The Paragraph header code element</param>
        void StartParagraph([NotNull] ParagraphHeader header);

        /// <summary>
        /// End a paragraph
        /// </summary>
        void EndParagraph();

        /// <summary>
        /// Start a sentence
        /// </summary>
        void StartSentence();

        /// <summary>
        /// End a sentence
        /// </summary>
        void EndSentence(SentenceEnd end, bool bCheck = false);

        /// <summary>
        /// Checks if the last statement being entered must leads to the start of a sentence.
        /// </summary>
        void CheckStartSentenceLastStatement();

        /// <summary>
        /// Start an Exec Statement
        /// </summary>
        /// <param name="execStmt">The Exec Statement code element</param>
        void StartExecStatement([NotNull] ExecStatement execStmt);

        /// <summary>
        /// End an Exec Statement
        /// </summary>
        void EndExecStatement();

        /// <summary>
        /// Continue Statement seen
        /// </summary>
        /// <param name="stmt">Continue Statement code element</param>
        void OnContinueStatement([NotNull] ContinueStatement stmt);

        /// <summary>
        /// Entry Statement seen
        /// </summary>
        /// <param name="stmt">Entry Statement code element</param>
        void OnEntryStatement([NotNull] EntryStatement stmt);

        /// <summary>
        /// Accept Statement seen
        /// </summary>
        /// <param name="stmt">Accept Statement code element</param>
        void OnAcceptStatement([NotNull] AcceptStatement stmt);

        /// <summary>
        /// Intialize Statement seen
        /// </summary>
        /// <param name="stmt">Initialize Statement code element</param>
        void OnInitializeStatement([NotNull] InitializeStatement stmt);

        /// <summary>
        /// Inspect Statement seen
        /// </summary>
        /// <param name="stmt">Inspect Statement code element</param>
        void OnInspectStatement([NotNull] InspectStatement stmt);

        /// <summary>
        /// Move Statement seen
        /// </summary>
        /// <param name="stmt">Move Statement code element</param>
        void OnMoveStatement([NotNull] MoveStatement stmt);

        /// <summary>
        /// Set Statement seen
        /// </summary>
        /// <param name="stmt">Set Statement code element</param>
        void OnSetStatement([NotNull] SetStatement stmt);

        /// <summary>
        /// Stop Statement seen
        /// </summary>
        /// <param name="stmt">Stop Statement code element</param>
        void OnStopStatement([NotNull] StopStatement stmt);

        /// <summary>
        /// Exit Method Statement seen
        /// </summary>
        /// <param name="stmt">Exit Method Statement code element</param>
        void OnExitMethodStatement([NotNull] ExitMethodStatement stmt);

        /// <summary>
        /// Exit Program Statement seen
        /// </summary>
        /// <param name="stmt">Exit Program Statement code element</param>
        void OnExitProgramStatement([NotNull] ExitProgramStatement stmt);

        /// <summary>
        /// GoBack Statement seen
        /// </summary>
        /// <param name="stmt">GoBack Statement code element</param>
        void OnGobackStatement([NotNull] GobackStatement stmt);

        /// <summary>
        /// Close Statement seen
        /// </summary>
        /// <param name="stmt">Close Statement code element</param>
        void OnCloseStatement([NotNull] CloseStatement stmt);

        /// <summary>
        /// Display Statement seen
        /// </summary>
        /// <param name="stmt">Display Statement code element</param>
        void OnDisplayStatement([NotNull] DisplayStatement stmt);

        /// <summary>
        /// Open Statement seen
        /// </summary>
        /// <param name="stmt">Open Statement code element</param>
        void OnOpenStatement([NotNull] OpenStatement stmt);

        /// <summary>
        /// Merge Statement seen
        /// </summary>
        /// <param name="stmt">Merge Statement code element</param>
        void OnMergeStatement([NotNull] MergeStatement stmt);

        /// <summary>
        /// Release Statement seen
        /// </summary>
        /// <param name="stmt">Release Statement code element</param>
        void OnReleaseStatement([NotNull] ReleaseStatement stmt);

        /// <summary>
        /// Sort Statement seen
        /// </summary>
        /// <param name="stmt">Sort Statement code element</param>
        void OnSortStatement([NotNull] SortStatement stmt);

        /// <summary>
        /// Alter Statement seen
        /// </summary>
        /// <param name="stmt">Alter Statement code element</param>
        void OnAlterStatement([NotNull] AlterStatement stmt);

        /// <summary>
        /// Exit Statement seen
        /// </summary>
        /// <param name="stmt">Exit Statement code element</param>
        void OnExitStatement([NotNull] ExitStatement stmt);

        /// <summary>
        /// Goto Statement seen
        /// </summary>
        /// <param name="stmt">Goto Statement code element</param>
        void OnGotoStatement([NotNull] GotoStatement stmt);

        /// <summary>
        /// Perform Statement seen
        /// </summary>
        /// <param name="stmt">Perform Statement code element</param>
        void OnPerformProcedureStatement([NotNull] PerformProcedureStatement stmt);

        /// <summary>
        /// Cancel Statement seen
        /// </summary>
        /// <param name="stmt">Cancel Statement code element</param>
        void OnCancelStatement([NotNull] CancelStatement stmt);

        /// <summary>
        /// Procedure Style Call Statement seen
        /// </summary>
        /// <param name="stmt">Procedure Style Call Statement code element</param>
        /// <param name="end">Optional END-CALL code element</param>
        void OnProcedureStyleCall([NotNull] ProcedureStyleCallStatement stmt, CallStatementEnd end = null);

        /// <summary>
        /// Exec Statement seen
        /// </summary>
        /// <param name="stmt">Exec Statement code element</param>
        void OnExecStatement([NotNull] ExecStatement stmt);

        #region CompoundStatements
        /// <summary>
        /// Start a Conditional ADD Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional ADD Statement code element</param>
        void StartAddStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.AddStatement stmt);
        /// <summary>
        /// End a Conditional ADD Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-ADD Statement code element</param>
        void EndAddStatementConditional(TypeCobol.Compiler.CodeElements.AddStatementEnd end = null);

        /// <summary>
        /// Start a Conditional CALL Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional CALL Statement code element</param>
        void StartCallStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.CallStatement stmt);
        /// <summary>
        /// End a Conditional CALL Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-CALL Statement code element</param>
        void EndCallStatementConditional(TypeCobol.Compiler.CodeElements.CallStatementEnd end = null);
        /// <summary>
        /// Start a Conditional COMPUTE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional COMPUTE Statement code element</param>
        void StartComputeStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.ComputeStatement stmt);
        /// <summary>
        /// End a Conditional COMPUTE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-COMPUTE Statement code element</param>
        void EndComputeStatementConditional(TypeCobol.Compiler.CodeElements.ComputeStatementEnd end = null);
        /// <summary>
        /// Start a Conditional DELETE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional DELETE Statement code element</param>
        void StartDeleteStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.DeleteStatement stmt);
        /// <summary>
        /// End a Conditional DELETE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-DELETE Statement code element</param>
        void EndDeleteStatementConditional(TypeCobol.Compiler.CodeElements.DeleteStatementEnd end = null);
        /// <summary>
        /// Start a Conditional DIVIDE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional DIVIDE Statement code element</param>
        void StartDivideStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.DivideStatement stmt);
        /// <summary>
        /// End a Conditional DIVIDE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-DIVIDE Statement code element</param>
        void EndDivideStatementConditional(TypeCobol.Compiler.CodeElements.DivideStatementEnd end = null);
        /// <summary>
        /// Start a Conditional EVALUATE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional EVALUATE Statement code element</param>
        void StartEvaluateStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.EvaluateStatement stmt);
        /// <summary>
        /// End a Conditional EVALUATE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-EVALUATE Statement code element</param>
        void EndEvaluateStatementWithBody(TypeCobol.Compiler.CodeElements.EvaluateStatementEnd end = null);
        /// <summary>
        /// Start a WHEN condition clause
        /// </summary>
        /// <param name="conditions">The WHEN condition clause code element</param>
        void StartWhenConditionClause([NotNull] List<TypeCobol.Compiler.CodeElements.CodeElement> conditions);
        /// <summary>
        /// End a WHEN condition clause. 
        /// </summary>
        void EndWhenConditionClause();
        /// <summary>
        /// Start a WHEN other condition clause
        /// </summary>
        /// <param name="cond">The WHEN other condition clause code element</param>
        void StartWhenOtherClause([NotNull] TypeCobol.Compiler.CodeElements.WhenOtherCondition cond);
        /// <summary>
        /// End a WHEN other condition clause
        /// </summary>
        void EndWhenOtherClause();
        /// <summary>
        /// Start a IF Statemenent 
        /// </summary>
        /// <param name="stmt">The IF Statement code element</param>
        void StartIfStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.IfStatement stmt);
        /// <summary>
        /// Enter an ELSE clause
        /// </summary>
        /// <param name="clause">The ELSE clause code element</param>
        void EnterElseClause(TypeCobol.Compiler.CodeElements.ElseCondition clause);
        /// <summary>
        /// End an IF Statemenent 
        /// </summary>
        /// <param name="end">The Optional IF-END Statement code element</param>
        void EndIfStatementWithBody(TypeCobol.Compiler.CodeElements.IfStatementEnd end = null);
        /// <summary>
        /// Add an NEXT sentence statement
        /// </summary>
        /// <param name="stmt">The NEXT sentence code elemnt</param>
        void AddNextSentenceStatement([NotNull] TypeCobol.Compiler.CodeElements.NextSentenceStatement stmt);
        /// <summary>
        /// Start a Conditional INVOKE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional INVOKE Statement code element</param>
        void StartInvokeStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.InvokeStatement stmt);
        /// <summary>
        /// End a Conditional INVOKE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-INVOKE Statement code element</param>
        void EndInvokeStatementConditional(TypeCobol.Compiler.CodeElements.InvokeStatementEnd end = null);
        /// <summary>
        /// Start a Conditional MULTIPLY Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional MULTIPLY Statement code element</param>
        void StartMultiplyStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.MultiplyStatement stmt);
        /// <summary>
        /// End a Conditional MULTIPLY Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-MULTIPLY Statement code element</param>
        void EndMultiplyStatementConditional(TypeCobol.Compiler.CodeElements.MultiplyStatementEnd end = null);
        /// <summary>
        /// Start a Conditional PERFORM Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional PERFORM Statement code element</param>
        void StartPerformStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.PerformStatement stmt);
        /// <summary>
        /// End a Conditional PERFORM Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-PERFORM Statement code element</param>
        void EndPerformStatementWithBody(TypeCobol.Compiler.CodeElements.PerformStatementEnd end = null);
        /// <summary>
        /// Start a Conditional SEARCH Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional SEARCH Statement code element</param>
        void StartSearchStatementWithBody([NotNull] TypeCobol.Compiler.CodeElements.SearchStatement stmt);
        /// <summary>
        /// End a Conditional SEARCH Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-SEARCH Statement code element</param>
        void EndSearchStatementWithBody(TypeCobol.Compiler.CodeElements.SearchStatementEnd end = null);
        /// <summary>
        /// Start a WHEN SEARCH condition
        /// </summary>
        /// <param name="condition">The WHEN Search condition code element</param>
        void StartWhenSearchConditionClause([NotNull] TypeCobol.Compiler.CodeElements.WhenSearchCondition condition);
        /// <summary>
        /// End the When search condition.
        /// </summary>
        void EndWhenSearchConditionClause();
        /// <summary>
        /// Start a Conditional READ Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional SEARCH Statement code element</param>
        void EnterReadStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.ReadStatement stmt);
        /// <summary>
        /// End a Conditional READ Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-READ Statement code element</param>
        void EndReadStatementConditional(TypeCobol.Compiler.CodeElements.ReadStatementEnd end = null);
        /// <summary>
        /// Start a Conditional RETURN Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional RETURN Statement code element</param>
        void EnterReturnStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.ReturnStatement stmt);
        /// <summary>
        /// End a Conditional RETURN Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-RETURN Statement code element</param>
        void EndReturnStatementConditional(TypeCobol.Compiler.CodeElements.ReturnStatementEnd end = null);
        /// <summary>
        /// Start a Conditional REWRITE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional REWRITE Statement code element</param>
        void StartRewriteStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.RewriteStatement stmt);
        /// <summary>
        /// End a Conditional REWRITE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-REWRITE Statement code element</param>
        void EndRewriteStatementConditional(TypeCobol.Compiler.CodeElements.RewriteStatementEnd end = null);
        /// <summary>
        /// Start a Conditional START Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional START Statement code element</param>
        void StartStartStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.StartStatement stmt);
        /// <summary>
        /// End a Conditional START Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-START Statement code element</param>
        void EndStartStatementConditional(TypeCobol.Compiler.CodeElements.StartStatementEnd end = null);
        /// <summary>
        /// Start a Conditional STRING Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional STRING Statement code element</param>
        void StartStringStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.StringStatement stmt);
        /// <summary>
        /// End a Conditional STRING Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-STRING Statement code element</param>
        void EndStringStatementConditional(TypeCobol.Compiler.CodeElements.StringStatementEnd end = null);
        /// <summary>
        /// Start a Conditional SUBTRACT Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional SUBTRACT Statement code element</param>
        void StartSubtractStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.SubtractStatement stmt);
        /// <summary>
        /// End a Conditional SUBTRACT Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-SUBTRACT Statement code element</param>
        void EndSubtractStatementConditional(TypeCobol.Compiler.CodeElements.SubtractStatementEnd end = null);
        /// <summary>
        /// Start a Conditional UNSTRING Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional UNSTRING Statement code element</param>
        void StartUnstringStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.UnstringStatement stmt);
        /// <summary>
        /// End a Conditional UNSTRING Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-UNSTRING Statement code element</param>
        void EndUnstringStatementConditional(TypeCobol.Compiler.CodeElements.UnstringStatementEnd end = null);
        /// <summary>
        /// Start a Conditional WRITE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional UNSTRING Statement code element</param>
        void StartWriteStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.WriteStatement stmt);
        /// <summary>
        /// End a Conditional WRITE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-WRITE Statement code element</param>
        void EndWriteStatementConditional(TypeCobol.Compiler.CodeElements.WriteStatementEnd end = null);
        /// <summary>
        /// Start a Conditional XML GENERATE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional XML GENERATE Statement code element</param>
        void StartXmlGenerateStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlGenerateStatement stmt);
        /// <summary>
        /// End a Conditional XML GENERATE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-XML Statement code element</param>
        void EndXmlGenerateStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end = null);
        /// <summary>
        /// Start a Conditional XML PARSE Statemenent 
        /// </summary>
        /// <param name="stmt">The Conditional XML PARSE Statement code element</param>
        void StartXmlParseStatementConditional([NotNull] TypeCobol.Compiler.CodeElements.XmlParseStatement stmt);
        /// <summary>
        /// End a Conditional XML PARSE Statemenent 
        /// </summary>
        /// <param name="end">The Optional END-XML Statement code element</param>
        void EndXmlParseStatementConditional(TypeCobol.Compiler.CodeElements.XmlStatementEnd end = null);
        #endregion

        #region conditions
        void StartOnSizeError([NotNull] TypeCobol.Compiler.CodeElements.OnSizeErrorCondition cond);
        void EndOnSizeError();
        void StartNoSizeError([NotNull] TypeCobol.Compiler.CodeElements.NotOnSizeErrorCondition cond);
        void EndNoSizeError();

        void StartOnException([NotNull] TypeCobol.Compiler.CodeElements.OnExceptionCondition cond);
        void EndOnException();
        void StartNoException([NotNull] TypeCobol.Compiler.CodeElements.NotOnExceptionCondition cond);
        void EndNoException();

        void StartOnOverflow([NotNull] TypeCobol.Compiler.CodeElements.OnOverflowCondition cond);
        void EndOnOverflow();
        void StartNoOverflow([NotNull] TypeCobol.Compiler.CodeElements.NotOnOverflowCondition cond);
        void EndNoOverflow();

        void StartOnInvalidKey([NotNull] TypeCobol.Compiler.CodeElements.InvalidKeyCondition cond);
        void EndOnInvalidKey();
        void StartNoInvalidKey([NotNull] TypeCobol.Compiler.CodeElements.NotInvalidKeyCondition cond);
        void EndNoInvalidKey();

        void StartOnAtEnd([NotNull] TypeCobol.Compiler.CodeElements.AtEndCondition cond);
        void EndOnAtEnd();
        void StartNoAtEnd([NotNull] TypeCobol.Compiler.CodeElements.NotAtEndCondition cond);
        void EndNoAtEnd();
        #endregion
    }
}
