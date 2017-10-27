using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Base class for all statement CodeElements
    /// </summary>
    public abstract class StatementElement : CodeElement
    {
        protected StatementElement(CodeElementType codeElementType, StatementType statementType) : base(codeElementType)
        {
            StatementType = statementType;
        }

        /// <summary>
        /// Type of executable statement
        /// </summary>
        public StatementType StatementType { get; private set; }

        /// <summary>
        /// List of all storage areas read by this statement
        /// </summary>
        public StorageArea[] DataReadAccess { get; set; }

        /// <summary>
        /// List of all storage areas written to by this statement
        /// </summary>
        public ReceivingStorageArea[] DataWriteAccess { get; set; }

        /// <summary>
        /// List of all expressions we need to compute for this statement
        /// </summary>
        public Expression[] ExpressionsToCompute { get; set; }

        /// <summary>
        /// List of all instrinsic function calls we need to execute for this statement
        /// </summary>
        public FunctionCallResult[] FunctionCalls { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, DataReadAccess,
                                                            DataWriteAccess, 
                                                            ExpressionsToCompute, 
                                                            FunctionCalls);
        }
    }

    /// <summary>
    /// List all types of Cobol executable statements
    /// </summary>
    public enum StatementType
    {
        // Entry points

        ProcedureDivisionHeader,
        EntryStatement,

        // Statements

        AcceptFromInputDeviceStatement,
        AcceptFromSystemDateStatement,
        AddSimpleStatement,
        AddGivingStatement,
        AddCorrespondingStatement,
        AlterStatement,
        CallStatement,
        CancelStatement,
        CloseStatement,
        ComputeStatement,
        ContinueStatement,
        DeleteStatement,
        DisplayStatement,
        DivideSimpleStatement,
        DivideGivingStatement,
        DivideRemainderStatement,
        EvaluateStatement,
        WhenCondition,
        WhenOtherCondition,
        ExecStatement,
        ExitMethodStatement,
        ExitProgramStatement,
        ExitStatement,
        GobackStatement,
        GotoSimpleStatement,
        GotoConditionalStatement,
        IfStatement,
        ElseCondition,
        InitializeStatement,
        InspectConvertingStatement,
        InspectReplacingStatement,
        InspectTallyingStatement,
        InvokeStatement,
        MergeStatement,
        MoveSimpleStatement,
        MoveCorrespondingStatement,
        MultiplySimpleStatement,
        MultiplyGivingStatement,
        NextSentenceStatement,
        OpenStatement,
        PerformStatement,
        PerformProcedureStatement,
        ReadStatement,
        ReleaseStatement,
        ReturnStatement,
        RewriteStatement,
        SearchSerialStatement,
        SearchBinaryStatement,
        WhenSearchCondition,
        SetStatementForAssignment,
        SetStatementForIndexes,
        SetStatementForSwitches,
        SetStatementForConditions,
        SetStatementPartial,
        SortStatement,
        StartStatement,
        StopStatement,
        StringStatement,
        SubtractSimpleStatement,
        SubtractGivingStatement,
        SubtractCorrespondingStatement,
        UnstringStatement,
        UseStatement,
        WriteStatement,
        XmlGenerateStatement,
        XmlParseStatement
    }
}
