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
        ProgramIdentification,
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
        ProcedureDivisionHeaderWithParameters,
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
        ExitMethodStatement,
        ExitProgramStatement,
        ExitStatement,
        GobackStatement,
        GotoStatement,
        IfStatement,
        InitializeStatement,
        InspectStatement,
        InvokeStatement,
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
        AtEndOfPage,
        NotAtEndOfPage,
        OnExceptionCondition,
        NotOnExceptionCondition,
        OnOverflowCondition,
        NotOnOverflowCondition,
        InvalidKeyCondition,
        NotInvalidKeyCondition,
        OnSizeErrorCondition,
        NotOnSizeErrorCondition,
        ElseCondition,
        WhenEvaluateCondition,
        WhenOtherCondition,
        WhenSearchCondition,

        // Statement ends

        AddStatementEnd,
        CallStatementEnd,
        ComputeStatementEnd,
        DeleteStatementEnd,
        DivideStatementEnd,
        EvaluateStatementEnd,
        IfStatementEnd,
        InvokeStatementEnd,
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
        XmlStatementEnd
    }
}