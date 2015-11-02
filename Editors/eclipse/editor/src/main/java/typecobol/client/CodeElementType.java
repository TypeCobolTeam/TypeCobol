package typecobol.client;

public enum CodeElementType {

        // -- Program --
        ProgramIdentification("ProgramIdentification"),
        ProgramEnd("ProgramEnd"),
        // -- Class --
        ClassIdentification("ClassIdentification"),
        ClassEnd("ClassEnd"),
        FactoryIdentification("FactoryIdentification"),
        FactoryEnd("FactoryEnd"),
        ObjectIdentification("ObjectIdentification"),
        ObjectEnd("ObjectEnd"),
        MethodIdentification("MethodIdentification"),
        MethodEnd("MethodEnd"),
        // -- Division --
        EnvironmentDivisionHeader("EnvironmentDivisionHeader"),
        DataDivisionHeader("DataDivisionHeader"),
        ProcedureDivisionHeader("ProcedureDivisionHeader"),
        DeclarativesHeader("DeclarativesHeader"),
        DeclarativesEnd("DeclarativesEnd"),
        // -- Section --
        SectionHeader("SectionHeader"),
        ConfigurationSectionHeader("ConfigurationSectionHeader"),
        InputOutputSectionHeader("InputOutputSectionHeader"),
        FileSectionHeader("FileSectionHeader"),
        WorkingStorageSectionHeader("WorkingStorageSectionHeader"),
        LocalStorageSectionHeader("LocalStorageSectionHeader"),
        LinkageSectionHeader("LinkageSectionHeader"),
        // -- Paragraph --
        ParagraphHeader("ParagraphHeader"),
        FileControlParagraphHeader("FileControlParagraphHeader"),
        IOControlParagraphHeader("IOControlParagraphHeader"),
        // -- Sentence --
        SentenceEnd("SentenceEnd"),

        // Entries

        // -- Data Division --
        FileDescriptionEntry("FileDescriptionEntry"),
        DataDescriptionEntry("DataDescriptionEntry"),
        // -- InputOutput Section --
        FileControlEntry("FileControlEntry"),
        IOControlEntry("IOControlEntry"),

        // Paragraphs

        // --Configuration Section --
        SourceComputerParagraph("SourceComputerParagraph"),
        ObjectComputerParagraph("ObjectComputerParagraph"),
        SpecialNamesParagraph("SpecialNamesParagraph"),
        RepositoryParagraph("RepositoryParagraph"),

        // Statements

        AcceptStatement("AcceptStatement"),
        AddStatement("AddStatement"),
        AlterStatement("AlterStatement"),
        CallStatement("CallStatement"),
        CancelStatement("CancelStatement"),
        CloseStatement("CloseStatement"),
        ComputeStatement("ComputeStatement"),
        ContinueStatement("ContinueStatement"),
        DeleteStatement("DeleteStatement"),
        DisplayStatement("DisplayStatement"),
        DivideStatement("DivideStatement"),
        EntryStatement("EntryStatement"),
        EvaluateStatement("EvaluateStatement"),
        ExecStatement("ExecStatement"),
        ExitMethodStatement("ExitMethodStatement"),
        ExitProgramStatement("ExitProgramStatement"),
        ExitStatement("ExitStatement"),
        GobackStatement("GobackStatement"),
        GotoStatement("GotoStatement"),
        IfStatement("IfStatement"),
        InitializeStatement("InitializeStatement"),
        InspectStatement("InspectStatement"),
        InvokeStatement("InvokeStatement"),
        MergeStatement("MergeStatement"),
        MoveStatement("MoveStatement"),
        MultiplyStatement("MultiplyStatement"),
        NextSentenceStatement("NextSentenceStatement"),
        OpenStatement("OpenStatement"),
        PerformProcedureStatement("PerformProcedureStatement"),
        PerformStatement("PerformStatement"),
        ReadStatement("ReadStatement"),
        ReleaseStatement("ReleaseStatement"),
        ReturnStatement("ReturnStatement"),
        RewriteStatement("RewriteStatement"),
        SearchStatement("SearchStatement"),
        SetStatement("SetStatement"),
        SortStatement("SortStatement"),
        StartStatement("StartStatement"),
        StopStatement("StopStatement"),
        StringStatement("StringStatement"),
        SubtractStatement("SubtractStatement"),
        UnstringStatement("UnstringStatement"),
        UseStatement("UseStatement"),
        WriteStatement("WriteStatement"),
        XmlGenerateStatement("XmlGenerateStatement"),
        XmlParseStatement("XmlParseStatement"),

        // Statement conditions

        AtEndCondition("AtEndCondition"),
        NotAtEndCondition("NotAtEndCondition"),
        AtEndOfPageCondition("AtEndOfPageCondition"),
        NotAtEndOfPageCondition("NotAtEndOfPageCondition"),
        OnExceptionCondition("OnExceptionCondition"),
        NotOnExceptionCondition("NotOnExceptionCondition"),
        OnOverflowCondition("OnOverflowCondition"),
        NotOnOverflowCondition("NotOnOverflowCondition"),
        InvalidKeyCondition("InvalidKeyCondition"),
        NotInvalidKeyCondition("NotInvalidKeyCondition"),
        OnSizeErrorCondition("OnSizeErrorCondition"),
        NotOnSizeErrorCondition("NotOnSizeErrorCondition"),
        ElseCondition("ElseCondition"),
        WhenEvaluateCondition("WhenEvaluateCondition"),
        WhenOtherCondition("WhenOtherCondition"),
        WhenConditionalExpression("WhenConditionalExpression"),

        // Statement ends

        AddStatementEnd("AddStatementEnd"),
        CallStatementEnd("CallStatementEnd"),
        ComputeStatementEnd("ComputeStatementEnd"),
        DeleteStatementEnd("DeleteStatementEnd"),
        DivideStatementEnd("DivideStatementEnd"),
        EvaluateStatementEnd("EvaluateStatementEnd"),
        IfStatementEnd("IfStatementEnd"),
        InvokeStatementEnd("InvokeStatementEnd"),
        MultiplyStatementEnd("MultiplyStatementEnd"),
        PerformStatementEnd("PerformStatementEnd"),
        ReadStatementEnd("ReadStatementEnd"),
        ReturnStatementEnd("ReturnStatementEnd"),
        RewriteStatementEnd("RewriteStatementEnd"),
        SearchStatementEnd("SearchStatementEnd"),
        StartStatementEnd("StartStatementEnd"),
        StringStatementEnd("StringStatementEnd"),
        SubtractStatementEnd("SubtractStatementEnd"),
        UnstringStatementEnd("UnstringStatementEnd"),
        WriteStatementEnd("WriteStatementEnd"),
        XmlStatementEnd("XmlStatementEnd");

	private final String name;
	private CodeElementType(final String name) { this.name = name; }
	@Override
	public String toString() { return name; }

	public static CodeElementType asEnum(final String name) {
		for(CodeElementType type: CodeElementType.values()) {
			if (type.name.equals(name)) return type;
		}
		throw new IllegalArgumentException("Invalid name for enum: \""+name+"\"");
	}
}

