using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Base class for a generated statement.
    /// Generated statement, like actual statements, are organized in a tree structure.
    /// Some statements are considered inactive unless they get at least one active children
    /// statement. For example PERFORM statement is useless until some statements are added
    /// into its loop body.
    /// </summary>
    internal abstract class GeneratedStatement
    {
        protected GeneratedStatement(bool isActive)
        {
            IsActive = isActive;
            Parent = null;
            Children = new List<GeneratedStatement>();
        }

        /// <summary>
        /// Indicate whether the statement is active or not, an inactive statement should not
        /// generate any code whereas an active statement is meant to be translated to Cobol code.
        /// </summary>
        protected bool IsActive { get; private set; }

        public GeneratedStatement Parent { get; private set; }

        public List<GeneratedStatement> Children { get; }

        private void Activate()
        {
            // Activate this statement and propagate to parent.
            IsActive = true;
            Parent?.Activate();
        }

        public void AddChild(GeneratedStatement childStatement)
        {
            childStatement.Parent = this;
            Children.Add(childStatement);
            if (childStatement.IsActive)
            {
                Activate();
            }
        }

        protected internal void WriteCobolCode(int statementLevel, CobolStringBuilder builder)
        {
            if (!IsActive) return; // Nothing to generate

            /*
             * Generic Cobol code generation algorithm: parameter statementLevel is used to indent statement code
             * according to its nesting level and then passed recursively to children statement.
             * Every statement has an opening line. For composite statements, the WriteStatementEnd is called to
             * get the closing line.
             */

            WriteIndent();
            WriteStatementOpening(builder);
            builder.AppendLine();

            if (Children.Count > 0)
            {
                foreach (var child in Children)
                {
                    child.WriteCobolCode(statementLevel + 1, builder);
                }

                WriteIndent();
                WriteStatementEnd(builder);
                builder.AppendLine();
            }

            void WriteIndent()
            {
                Debug.Assert(statementLevel >= 0);

                int length = CobolFormatAreas.End_A - CobolFormatAreas.Begin_A + 1; // 4 spaces to start in AreaB
                length += 2 * statementLevel; // 2 additional spaces for each level of nested statements
                builder.AppendIndent(length);
            }
        }

        /// <summary>
        /// Derived classes must implement this method to write their own keywords
        /// and expressions to the supplied builder.
        /// </summary>
        /// <param name="builder">Target builder.</param>
        protected abstract void WriteStatementOpening(CobolStringBuilder builder);

        /// <summary>
        /// For composite statements only, derived classes must implement this method
        /// to write their end element.
        /// </summary>
        /// <param name="builder">Target builder.</param>
        protected abstract void WriteStatementEnd(CobolStringBuilder builder);
    }

    /// <summary>
    /// Root for a tree of generated statements. This does not correspond to any code,
    /// however the code for nested statements can be retrieved only using this class.
    /// </summary>
    internal class GeneratedRoot : GeneratedStatement, ICobolCodeProvider
    {
        public GeneratedRoot()
            : base(false) // Initially inactive as it serves no purpose until an active nested statement is added
        {

        }

        public bool HasContent => IsActive;

        public void WriteCobolCode(CobolStringBuilder builder)
        {
            // Write all children
            foreach (var child in Children)
            {
                child.WriteCobolCode(0, builder);
            }
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("Root statement has no beginning code element.");
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("Root statement has no ending code element.");
        }
    }

    /// <summary>
    /// Class to write a PERFORM statement.
    /// Form is:
    /// PERFORM VARYING [Index] FROM 1 BY 1 UNTIL [Index] > [Max]
    ///   [Children]
    /// END-PERFORM
    /// </summary>
    internal class GeneratedPerform : GeneratedStatement
    {
        public string Index { get; }

        public string Max { get; }

        public GeneratedPerform(string index, string max)
            : base(false) // Initially inactive as it serves no purpose until an active nested statement is added
        {
            Index = index;
            Max = max;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            builder.AppendWord("PERFORM");
            builder.AppendWord("VARYING");
            builder.AppendWord(Index);
            builder.AppendWord("FROM");
            builder.AppendWord("1");
            builder.AppendWord("BY");
            builder.AppendWord("1");
            builder.AppendWord("UNTIL");
            builder.AppendWord(Index);
            builder.AppendWord(">");
            builder.AppendWord(Max);
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            builder.AppendWord("END-PERFORM");
        }
    }

    /// <summary>
    /// Class to write a DISPLAY statement for a variable.
    /// Form varies according to the presence or absence of value, indices, logical level
    /// of the variable, etc.
    /// </summary>
    internal class GeneratedDisplayVariable : GeneratedStatement
    {
        /// <summary>
        /// Define indentation level of the variable name, to replicate its nesting
        /// in the output.
        /// </summary>
        public int LogicalLevel { get; }

        /// <summary>
        /// Data definition to display
        /// </summary>
        public DataDefinition Target { get; }

        /// <summary>
        /// Accessor for the targeted data
        /// </summary>
        public DataDefinitionHelper.DataAccessor Accessor { get; }

        /// <summary>
        /// Table of indices to use to access the targeted data, when it is defined
        /// under one or more OCCURS.
        /// </summary>
        public string[] Indices { get; }

        /// <summary>
        /// True to display the value of the variable, False to display its name only.
        /// </summary>
        public bool WithValue { get; }

        public GeneratedDisplayVariable(int logicalLevel, DataDefinition target, DataDefinitionHelper.DataAccessor accessor, string[] indices, bool withValue)
            : base(true) // Always active
        {
            LogicalLevel = logicalLevel;
            Target = target;
            Accessor = accessor;
            Indices = indices;
            WithValue = withValue;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            builder.AppendWord("DISPLAY");

            var wordBuilder = new StringBuilder();
            bool withIndices = Indices.Length > 0;

            /*
             * There are four main forms for the DISPLAY statement:
             * - No indices, no value: DISPLAY '    var1'
             * - No indices but a value: DISPLAY '    var1 <' var1 '>'
             * - Indices but no value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ')'
             * - Indices and value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ') <' var1 (Idx-1 Idx-2) '>'
             *
             * Those four forms are altered when a reference modifier is needed.
             *
             * The AppendDisplayName method is responsible for creating the part describing the data
             * including the opening value delimiter (char <) when required.
             * The AppendValue method is responsible for creating the value part including the closing
             * value delimiter (char >)
             */

            AppendDisplayName();
            if (WithValue)
            {
                AppendValue();
            }

            void AppendDisplayName()
            {
                /*
                 * The whole display name is made of several literals (and references) when it includes indices.
                 * The first literal may be long, for example when the variable is deeply nested and
                 * has a long name. To avoid generating invalid cobol lines, we use the AppendLiteralForDisplay
                 * method for the first literal, as it is able to split it on several lines. The other literals
                 * are short and won't need splitting, so we add them as regular words.
                 *
                 * This boolean manages this distinction.
                 */
                bool isFirstLiteral = true;

                // Indentation of the name
                string indent = new string(' ', 2 * LogicalLevel);
                wordBuilder.Append(indent);

                // Data name
                string name = string.IsNullOrEmpty(Target.Name) ? "FILLER" : Target.Name;
                wordBuilder.Append(name);

                if (withIndices)
                {
                    // Add opening parenthesis and flush the first literal
                    wordBuilder.Append(" (");
                    builder.AppendLiteralForDisplay(wordBuilder.ToString());
                    wordBuilder.Clear();
                    isFirstLiteral = false;

                    // Indices for display are separated by a space literal
                    for (int i = 0; i < Indices.Length; i++)
                    {
                        builder.AppendWord(Indices[i]);

                        bool isLast = i == Indices.Length - 1;
                        if (!isLast)
                        {
                            builder.AppendWord("' '");
                        }
                    }

                    // Start a new literal, as it will ultimately be appended as word, we have to add the opening delimiter ourselves
                    wordBuilder.Append("')");
                }

                // Do not attempt to represent reference modifier when it is not a direct access (i.e. not accessing using the immediate parent)
                // or too complex (using an expression when accessing an anonymous data located inside an OCCURS)
                if (Accessor.Data == Target.Parent && Accessor.ReferenceModifier is { Count: 1 })
                {
                    wordBuilder.Append(' ');
                    wordBuilder.Append(Accessor.ReferenceModifier[0]);
                }

                // Add opening value delimiter
                if (WithValue)
                {
                    wordBuilder.Append(" <");
                }

                // Flush last literal
                if (isFirstLiteral)
                {
                    builder.AppendLiteralForDisplay(wordBuilder.ToString());
                }
                else
                {
                    // We have to add closing delimiter ourselves
                    wordBuilder.Append('\'');
                    builder.AppendWord(wordBuilder.ToString());
                }
                
                wordBuilder.Clear();
            }

            void AppendValue()
            {
                if (!string.IsNullOrEmpty(Target.Name) && Target.IsNationalOrNationalEdited())
                {
                    Debug.Assert(Target == Accessor.Data); // Target is named

                    // Display of national items require the use of DISPLAY-OF built-in function
                    // Note that display of national fillers is not supported by Cobol as the DISPLAY-OF
                    // function does not accept modified references
                    builder.AppendWord("FUNCTION");
                    builder.AppendWord("DISPLAY-OF");

                    // Append name
                    wordBuilder.Append('(');
                    wordBuilder.Append(Target.Name);
                    if (Indices.Length == 0)
                    {
                        wordBuilder.Append(')');
                    }
                    builder.AppendWord(wordBuilder.ToString());
                    wordBuilder.Clear();

                    // Append indices (if any) and the closing parenthesis of the DISPLAY-OF call
                    AppendIndicesForAccess(true);
                }
                else
                {
                    // Not a National, use accessor info
                    builder.AppendWord(Accessor.Data.Name);
                    AppendIndicesForAccess(false);
                    if (Accessor.ReferenceModifier != null)
                    {
                        foreach (var referenceModifierWord in Accessor.ReferenceModifier)
                        {
                            builder.AppendWord(referenceModifierWord);
                        }
                    }
                }

                builder.AppendWord("'>'");

                void AppendIndicesForAccess(bool addClosingParenthesis)
                {
                    // Append (Idx1 Idx2 [...] Idxn) and an additional closing parenthesis when requested

                    for (int i = 0; i < Accessor.IndicesCount; i++)
                    {
                        bool isFirst = i == 0;
                        if (isFirst)
                        {
                            wordBuilder.Append('(');
                        }

                        wordBuilder.Append(Indices[i]);

                        bool isLast = i == Accessor.IndicesCount - 1;
                        if (isLast)
                        {
                            wordBuilder.Append(')');
                            if (addClosingParenthesis)
                            {
                                wordBuilder.Append(')');
                            }
                        }

                        builder.AppendWord(wordBuilder.ToString());
                        wordBuilder.Clear();
                    }
                }
            }
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("DISPLAY statement is not composite.");
        }
    }

    /// <summary>
    /// Class to write an IF statement.
    /// Form is:
    /// IF [Variable] IS NUMERIC AND [Variable] >=1 AND &lt;= [MaxOccurs]
    ///   [Children]
    /// ELSE
    ///   DISPLAY [ErrorMessage]
    /// END-IF
    ///
    /// or
    ///
    /// IF [Variable] >=1 AND &lt;= [MaxOccurs]
    ///   [Children]
    /// ELSE
    ///   DISPLAY [ErrorMessage]
    /// END-IF
    /// (when the variable cannot be class tested)
    /// </summary>
    internal class GeneratedIfIsNumericAndInRangeElseDisplayMessage : GeneratedStatement
    {
        public string Variable { get; }

        public bool CheckNumeric { get; }

        public string MaxOccurs { get; }

        public string ErrorMessage { get; }

        public GeneratedIfIsNumericAndInRangeElseDisplayMessage(string variable, bool checkNumeric, string maxOccurs, string errorMessage)
            : base(false) // Initially inactive as it serves no purpose until an active nested statement is added
        {
            Variable = variable;
            CheckNumeric = checkNumeric;
            MaxOccurs = maxOccurs;
            ErrorMessage = errorMessage;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            builder.AppendWord("IF");

            if (CheckNumeric)
            {
                builder.AppendWord(Variable);
                builder.AppendWord("IS");
                builder.AppendWord("NUMERIC");
                builder.AppendWord("AND");
            }

            builder.AppendWord(Variable);
            builder.AppendWord(">=");
            builder.AppendWord("1");
            builder.AppendWord("AND");
            builder.AppendWord("<=");
            builder.AppendWord(MaxOccurs);
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            /*
             * HACK ! To model properly an IF statement, we should have something like
             * IF
             *  THEN
             *    [Children of THEN when the condition is true]
             *  ELSE
             *    [Children of ELSE when the condition is false]
             * END-IF
             *
             * With THEN a pseudo statement as it should not produce any code and not increase statement level
             *
             * Instead we hack the WriteStatementEnd to write directly the ELSE and DISPLAY [ErrorMessage] part
             * without using the real GeneratedDisplay class !!!
             */
            builder.AppendWord("ELSE");
            int indent = builder.AppendLine();
            builder.AppendIndent(indent + 2);
            builder.AppendWord("DISPLAY");
            builder.AppendLiteralForDisplay(ErrorMessage);
            builder.AppendLine();
            builder.AppendIndent(indent);
            builder.AppendWord("END-IF");
        }
    }

    /// <summary>
    /// Class to write a simple DISPLAY statement for a separator literal.
    /// The separator is made of repeated '-' characters and starts with
    /// indentation defined by given logical level.
    /// </summary>
    internal class GeneratedDisplaySeparator : GeneratedStatement
    {
        private const int SYSOUT_LINE_LENGTH = 120;

        private readonly int _logicalLevel;

        public GeneratedDisplaySeparator(int logicalLevel)
            : base(true)
        {
            _logicalLevel = logicalLevel;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            string separator = new string(' ', 2 * _logicalLevel).PadRight(SYSOUT_LINE_LENGTH, '-');
            builder.AppendWord("DISPLAY");
            builder.AppendLiteralForDisplay(separator);
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("DISPLAY statement is not composite.");
        }
    }
}
