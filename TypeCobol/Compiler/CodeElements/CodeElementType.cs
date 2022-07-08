using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The Cobol syntax can be decomposed in 117 elementary code elements
    /// </summary>
    public enum CodeElementType
    {
        // Code structure

        // -- Program --
        ProgramIdentification = 1, // <- value 1 is mandatory to synchronize this list with the Antlr parser
        ProgramEnd,
        // -- Class --
        ClassIdentification,
        ClassEnd,
        FactoryIdentification,
        FactoryEnd,
        ObjectIdentification,
        ObjectEnd,
        MethodIdentification,
        MethodEnd,
        // -- Division --
        EnvironmentDivisionHeader,
        DataDivisionHeader,
        ProcedureDivisionHeader,
        DeclarativesHeader,
        DeclarativesEnd,
        // -- Section --
        SectionHeader,
        ConfigurationSectionHeader,
        InputOutputSectionHeader,
        FileSectionHeader,
        WorkingStorageSectionHeader,
        LocalStorageSectionHeader,
        LinkageSectionHeader,
        // -- Paragraph --
        ParagraphHeader,
        FileControlParagraphHeader,
        IOControlParagraphHeader,
        // -- Sentence --
        SentenceEnd,

        // Entries

        // -- Data Division --
        FileDescriptionEntry,
        DataDescriptionEntry,
        DataRedefinesEntry,
        DataRenamesEntry,
        DataConditionEntry,
        // -- InputOutput Section --
        FileControlEntry,
        IOControlEntry,

        // Paragraphs

        // --Configuration Section --
        SourceComputerParagraph,
        ObjectComputerParagraph,
        SpecialNamesParagraph,
        RepositoryParagraph,          

        // Statements

        AcceptStatement,
        AddStatement,
        AllocateStatement,
        AlterStatement,
        CallStatement,
        CancelStatement,
        CloseStatement,
        ComputeStatement,
        ContinueStatement,
        DeleteStatement,
        DisplayStatement,
        DivideStatement,
        EntryStatement,
        EvaluateStatement,
        ExecStatement,
        ExecStatementText,
        ExitMethodStatement,
        ExitProgramStatement,
        ExitStatement,
        FreeStatement,
        GobackStatement,
        GotoStatement,
        IfStatement,
        InitializeStatement,
        InspectStatement,
        InvokeStatement,
        JsonGenerateStatement,
        JsonParseStatement,
        MergeStatement,
        MoveStatement,
        MultiplyStatement,
        NextSentenceStatement,
        OpenStatement,
        PerformProcedureStatement,
        PerformStatement,
        ReadStatement,
        ReleaseStatement,
        ReturnStatement,
        RewriteStatement,
        SearchStatement,
        SetStatement,
        SortStatement,
        StartStatement,
        StopStatement,
        StringStatement,
        SubtractStatement,
        UnstringStatement,
        UseStatement,
        WriteStatement,
        XmlGenerateStatement,
        XmlParseStatement,

        // Statement conditions

        AtEndCondition,
        NotAtEndCondition,
        AtEndOfPageCondition,
        NotAtEndOfPageCondition,
        OnExceptionCondition,
        NotOnExceptionCondition,
        OnOverflowCondition,
        NotOnOverflowCondition,
        InvalidKeyCondition,
        NotInvalidKeyCondition,
        OnSizeErrorCondition,
        NotOnSizeErrorCondition,
        ElseCondition,
        WhenCondition,
        WhenOtherCondition,
        WhenSearchCondition,

        // Statement ends

        AddStatementEnd,
        CallStatementEnd,
        ComputeStatementEnd,
        DeleteStatementEnd,
        DivideStatementEnd,
        EvaluateStatementEnd,
        ExecStatementEnd,
        IfStatementEnd,
        InvokeStatementEnd,
        JsonStatementEnd,
        MultiplyStatementEnd,
        PerformStatementEnd,
        ReadStatementEnd,
        ReturnStatementEnd,
        RewriteStatementEnd,
        SearchStatementEnd,
        StartStatementEnd,
        StringStatementEnd,
        SubtractStatementEnd,
        UnstringStatementEnd,
        WriteStatementEnd,
        XmlStatementEnd,

        // FOR SQL
        //Sql Statements,
        CommitStatement,
        SelectStatement,
        RollbackStatement,
        TruncateStatement,
        SavepointStatement,
        WhenEverStatement,
        LockTableStatement,
        ReleaseSavepointStatement,
        DropTableStatement,

        // [TYPECOBOL]
        LibraryCopy,
		FunctionDeclarationHeader,
		FunctionDeclarationEnd,
		ProcedureStyleCall,
        GlobalStorageSectionHeader,
        // [/TYPECOBOL]

        Program,
        ParametersProfile,
    }
}
