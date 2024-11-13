using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Text;

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

    internal class GeneratedDisplay : GeneratedStatement
    {
        public int LogicalLevel { get; }

        public string DisplayName { get; }

        public string AccessName { get; }

        public string[] Indices { get; }

        public string ReferenceModifier { get; }

        public bool WithValue { get; }

        public GeneratedDisplay(int logicalLevel, string name, string[] indices, bool withValue)
            : this(logicalLevel, name, name, indices, null, withValue)
        {

        }

        public GeneratedDisplay(int logicalLevel, string accessName, string[] indices, string referenceModifier)
            : this(logicalLevel, "FILLER", accessName, indices, referenceModifier, true)
        {

        }

        private GeneratedDisplay(int logicalLevel, string displayName, string accessName, string[] indices, string referenceModifier, bool withValue)
            : base(true)
        {
            LogicalLevel = logicalLevel;
            DisplayName = displayName;
            AccessName = accessName;
            Indices = indices;
            ReferenceModifier = referenceModifier;
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
                    builder.AppendWord(AccessName);
                    if (ReferenceModifier != null)
                    {
                        builder.AppendWord(ReferenceModifier);
                    }
                    builder.AppendWord("'>'");
                    break;
                case (true, false):
                    // Indices but no value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ')'
                    AppendDisplayName('(');
                    AppendIndicesForLabel();
                    builder.AppendWord("')'");
                    break;
                case (true, true):
                    // Indices and value: DISPLAY '    var1 (' Idx-1 ' ' Idx-2 ') <' var1 (Idx-1 Idx-2) '>'
                    AppendDisplayName('(');
                    AppendIndicesForLabel();
                    builder.AppendWord("') <'");
                    builder.AppendWord(AccessName);
                    AppendIndicesForCode();
                    if (ReferenceModifier != null)
                    {
                        builder.AppendWord(ReferenceModifier);
                    }
                    builder.AppendWord("'>'");
                    break;
            }

            void AppendDisplayName(char? terminator)
            {
                wordBuilder.Append('\'');
                wordBuilder.Append(' ', LogicalLevel);
                wordBuilder.Append(DisplayName);
                if (terminator.HasValue)
                {
                    wordBuilder.Append(' ');
                    wordBuilder.Append(terminator.Value);
                }
                wordBuilder.Append('\'');
                builder.AppendWord(wordBuilder.ToString());
                wordBuilder.Clear();
            }

            void AppendIndicesForLabel()
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

            void AppendIndicesForCode()
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
                    }

                    builder.AppendWord(wordBuilder.ToString());
                    wordBuilder.Clear();
                }
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

        public GeneratedIfNumeric(string variable)
            : base(false)
        {
            Variable = variable;
        }

        protected override void WriteStatementOpening(CobolStringBuilder builder)
        {
            builder.AppendWord("IF");
            builder.AppendWord(Variable);
            builder.AppendWord("IS");
            builder.AppendWord("NUMERIC");
        }

        protected override void WriteStatementEnd(CobolStringBuilder builder)
        {
            builder.AppendWord("END-IF");
        }
    }
}
