using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Types;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal abstract class GeneratedStatement
    {
        private bool _active;

        protected GeneratedStatement(bool active)
        {
            _active = active;
            Parent = null;
            Children = new List<GeneratedStatement>();
        }

        public GeneratedStatement Parent { get; private set; }

        public List<GeneratedStatement> Children { get; }

        private void Activate()
        {
            _active = true;
            Parent?.Activate();
        }

        public void AddChild(GeneratedStatement childStatement)
        {
            childStatement.Parent = this;
            Children.Add(childStatement);
            if (childStatement._active)
            {
                Activate();
            }
        }

        protected internal void WriteCobolCode(int statementLevel, CobolStringBuilder builder)
        {
            if (!_active) return;

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

        protected abstract void WriteStatementOpening(CobolStringBuilder builder);

        protected abstract void WriteStatementEnd(CobolStringBuilder builder);
    }

    internal class GeneratedRoot : GeneratedStatement
    {
        public GeneratedRoot()
            : base(false)
        {

        }

        public void WriteCobolCode(CobolStringBuilder builder)
        {
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

    internal class GeneratedPerform : GeneratedStatement
    {
        public string Index { get; }

        public string Max { get; }

        public GeneratedPerform(string index, string max)
            : base(false)
        {
            Index = index;
            Max = max;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            // PERFORM VARYING Idx-hash-n FROM 1 BY 1 UNTIL Idx-hash-n > Max
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

    internal class GeneratedDisplayVariable : GeneratedStatement
    {
        public int LogicalLevel { get; }

        public DataDefinition Target { get; }

        public DataDefinitionHelper.DataAccessor Accessor { get; }

        public string[] Indices { get; }

        public bool WithValue { get; }

        public GeneratedDisplayVariable(int logicalLevel, DataDefinition target, DataDefinitionHelper.DataAccessor accessor, string[] indices, bool withValue)
            : base(true)
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
            switch (withIndices, WithValue)
            {
                case (false, false):
                    // No indices, no value: DISPLAY '    var1'
                    AppendDisplayName(null);
                    break;
                case (false, true):
                    // No indices but a value: DISPLAY '    var1 <' var1 '>'
                    AppendDisplayName('<');
                    AppendValue();
                    AppendClosingValueDelimiter();
                    break;
                case (true, false):
                    // Indices but no value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ')'
                    AppendDisplayName('(');
                    AppendIndicesForDisplay();
                    AppendClosingIndicesDelimiter();
                    break;
                case (true, true):
                    // Indices and value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ') <' var1 (Idx-1 Idx-2) '>'
                    AppendDisplayName('(');
                    AppendIndicesForDisplay();
                    AppendClosingIndicesDelimiter();
                    AppendValue();
                    AppendClosingValueDelimiter();
                    break;
            }

            void AppendDisplayName(char? openingChar)
            {
                // DisplayName is a non-breakable literal: ' Indent Name (referenceModifierPrecededBySpace)? (openingCharPrecededBySpace)? '
                wordBuilder.Append('\'');

                string indent = new string(' ', 2 * LogicalLevel);
                wordBuilder.Append(indent);

                string name = string.IsNullOrEmpty(Target.Name) ? "FILLER" : Target.Name;
                wordBuilder.Append(name);

                if (Indices.Length == 0 && Accessor.ReferenceModifier != null && Accessor.Data == Target.Parent)
                {
                    wordBuilder.Append(' ');
                    wordBuilder.Append(Accessor.ReferenceModifier);
                }

                if (openingChar.HasValue)
                {
                    wordBuilder.Append(' ');
                    wordBuilder.Append(openingChar.Value);
                }

                wordBuilder.Append('\'');
                builder.AppendWord(wordBuilder.ToString());
                wordBuilder.Clear();
            }

            void AppendValue()
            {
                if (!string.IsNullOrEmpty(Target.Name) && IsNationalOrNationalEdited(Target))
                {
                    builder.AppendWord("FUNCTION");
                    builder.AppendWord("DISPLAY-OF");
                    wordBuilder.Append('(');
                    wordBuilder.Append(Target.Name);
                    if (Indices.Length == 0)
                    {
                        wordBuilder.Append(')');
                    }
                    builder.AppendWord(wordBuilder.ToString());
                    wordBuilder.Clear();
                    AppendIndicesForAccess(true);
                }
                else
                {
                    builder.AppendWord(Accessor.Data.Name);
                    AppendIndicesForAccess(false);
                    if (Accessor.ReferenceModifier != null)
                    {
                        builder.AppendWord(Accessor.ReferenceModifier);
                    }
                }

                void AppendIndicesForAccess(bool addClosingParenthesis)
                {
                    for (int i = 0; i < Indices.Length; i++)
                    {
                        bool isFirst = i == 0;
                        bool isLast = i == Indices.Length - 1;

                        if (isFirst)
                        {
                            wordBuilder.Append('(');
                        }

                        wordBuilder.Append(Indices[i]);

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

            static bool IsNationalOrNationalEdited(DataDefinition dataDefinition)
            {
                bool hasPicture = dataDefinition.SemanticData?.Type?.Tag == Compiler.Types.Type.Tags.Picture;
                if (hasPicture)
                {
                    var picture = (PictureType)dataDefinition.SemanticData.Type;
                    return picture.Category == PictureCategory.National || picture.Category == PictureCategory.NationalEdited;
                }

                return false;
            }

            void AppendClosingValueDelimiter()
            {
                builder.AppendWord("'>'");
            }

            void AppendIndicesForDisplay()
            {
                for (int i = 0; i < Indices.Length; i++)
                {
                    builder.AppendWord(Indices[i]);

                    bool isLast = i == Indices.Length - 1;
                    if (!isLast)
                    {
                        builder.AppendWord("' '");
                    }
                }
            }

            void AppendClosingIndicesDelimiter()
            {
                wordBuilder.Append("')");
                
                Debug.Assert(Indices.Length > 0);
                if (Accessor.ReferenceModifier != null && Accessor.Data == Target.Parent)
                {
                    wordBuilder.Append(' ');
                    wordBuilder.Append(Accessor.ReferenceModifier);
                }

                if (WithValue)
                {
                    wordBuilder.Append(" <");
                }

                wordBuilder.Append('\'');
                builder.AppendWord(wordBuilder.ToString());
                wordBuilder.Clear();
            }
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("DISPLAY statement is not composite.");
        }
    }

    internal class GeneratedIfNumeric : GeneratedStatement
    {
        public string Variable { get; }

        public bool Negate { get; }

        public GeneratedIfNumeric(string variable, bool negate)
            : base(false)
        {
            Variable = variable;
            Negate = negate;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            builder.AppendWord("IF");
            builder.AppendWord(Variable);
            builder.AppendWord("IS");
            if (Negate)
            {
                builder.AppendWord("NOT");
            }
            builder.AppendWord("NUMERIC");
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            builder.AppendWord("END-IF");
        }
    }

    internal class GeneratedDisplayMessage : GeneratedStatement
    {
        public string Message { get; }

        public GeneratedDisplayMessage(string message)
            : base(true)
        {
            Message = message;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            Debug.Assert(!Message.Contains('\''));
            Debug.Assert(Message.Split(' ').All(ContainsNoQuotesOrTwoQuotes));

            builder.AppendWord("DISPLAY");
            string displayArg = '\'' + Message + '\'';
            builder.AppendWord(displayArg);

            static bool ContainsNoQuotesOrTwoQuotes(string word)
            {
                int quoteCount = word.Count(c => c == '"');
                return quoteCount == 0 || quoteCount == 2;
            }
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            throw new InvalidOperationException("DISPLAY statement is not composite.");
        }
    }
}
