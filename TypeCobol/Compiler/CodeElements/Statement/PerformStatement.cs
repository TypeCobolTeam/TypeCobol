using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p384: PERFORM statement
    /// imperative-statement-1
    /// The statements to be executed for an in-line PERFORM
    /// An in-line PERFORM must be delimited by the END-PERFORM phrase.
    /// </summary>
    public class PerformStatement : StatementElement
    {
        public PerformStatement() : base(CodeElementType.PerformStatement, StatementType.PerformStatement)
        { }

        protected PerformStatement(CodeElementType codeElementType, StatementType statementType) 
            : base(codeElementType, statementType)
        { }

        /// <summary>
        /// PERFORM with TIMES phrase
        /// PERFORM with UNTIL phrase
        /// PERFORM with VARYING phrase
        /// </summary>
        public SyntaxProperty<PerformIterationType> IterationType { get; set; }

        /// <summary>
        /// * PERFORM with TIMES phrase
        /// The procedures referred to in the TIMES phrase of the PERFORM statement are
        /// executed the number of times specified by the value in identifier-1 or integer-1, up
        /// to a maximum of 999,999,999 times. Control then passes to the next executable
        /// statement following the PERFORM statement.
        /// </summary>
        public NumericVariable TimesIterationCount { get; set; }

        /// <summary>
        /// If the TEST BEFORE phrase is specified or assumed, the condition is tested before
        /// any statements are executed (corresponds to DO WHILE).
        /// If the TEST AFTER phrase is specified, the statements to be performed are
        /// executed at least once before the condition is tested (corresponds to DO UNTIL).
        /// </summary>
        public SyntaxProperty<TerminationConditionTestTime> TerminationConditionTestTime { get; set; }

        /// <summary>
        /// * PERFORM with UNTIL phrase
        /// In the UNTIL phrase format, the procedures referred to are performed until the
        /// condition specified by the UNTIL phrase is true. Control is then passed to the next
        /// executable statement following the PERFORM statement.
        /// </summary>
        public ConditionalExpression UntilTerminationCondition { get; set; }

        /// <summary>
        /// * PERFORM with VARYING phrase
        // The VARYING phrase increases or decreases the value of one or more identifiers or
        // index-names, according to certain rules.
        /// </summary>
        public PerformLoopDescription[] VaryingLoopDescriptions { get; set; }
    }

    /// <summary>
    /// PERFORM with TIMES phrase
    /// PERFORM with UNTIL phrase
    /// PERFORM with VARYING phrase
    /// </summary>
    public enum PerformIterationType
    {
        None,
        Times,
        Until,
        Varying
    }

    /// <summary>
    /// If the TEST BEFORE phrase is specified or assumed, the condition is tested before
    /// any statements are executed (corresponds to DO WHILE).
    /// If the TEST AFTER phrase is specified, the statements to be performed are
    /// executed at least once before the condition is tested (corresponds to DO UNTIL).
    /// </summary>
    public enum TerminationConditionTestTime
    {
        BeforeIteration,
        AfterIteration
    }

    /// <summary>
    /// identifier-2 through identifier-7
    /// Must name a numeric elementary item.
    /// literal-1 through literal-4
    /// Must represent a numeric literal.
    /// condition-1, condition-2
    /// Can be any condition described under “Conditional expressions” on page
    /// 256. If the condition is true at the time the PERFORM statement is
    /// initiated, the specified procedures are not executed.
    /// After the conditions specified in the UNTIL phrase are satisfied, control is
    /// passed to the next executable statement following the PERFORM
    /// statement.
    /// </summary>
    public class PerformLoopDescription
    {
        public ReceivingStorageArea LoopVariable { get; set; }

        public NumericVariable InitialValue { get; set; }

        public NumericVariable Increment { get; set; }

        public ConditionalExpression TerminationCondition { get; set; }
    }
}
