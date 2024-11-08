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

        protected void WriteIndent(int level, TextWriter writer)
        {
            string indent = new string(' ', 2 * level);
            writer.Write(indent);
        }

        protected void Write(int level, TextWriter writer)
        {
            WriteStatementLine(level, writer);

            if (Children.Count > 0)
            {
                foreach (var child in Children)
                {
                    child.Write(level + 1, writer);
                }

                WriteEndLine(level, writer);
            }
        }

        protected abstract void WriteStatementLine(int level, TextWriter writer);

        protected abstract void WriteEndLine(int level, TextWriter writer);
    }

    internal class GeneratedRoot : GeneratedStatement
    {
        public GeneratedRoot()
            : base(false)
        {

        }

        public void Write(TextWriter writer) => Write(-1, writer);

        protected override void WriteStatementLine(int level, TextWriter writer)
        {

        }

        protected override void WriteEndLine(int level, TextWriter writer)
        {

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

        protected override void WriteStatementLine(int level, TextWriter writer)
        {
            WriteIndent(level, writer);
            writer.WriteLine($"PERFORM VARYING {Index} FROM 1 BY 1 UNTIL {Index} > {Max}");
        }

        protected override void WriteEndLine(int level, TextWriter writer)
        {
            WriteIndent(level, writer);
            writer.WriteLine("END-PERFORM");
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

        protected override void WriteStatementLine(int level, TextWriter writer)
        {
            WriteIndent(level, writer);
            writer.Write("DISPLAY '");
            WriteIndent(LogicalLevel, writer);
            writer.Write(DisplayName);

            if (Indices.Length > 0)
            {
                writer.Write(" (' ");
                string subscripts = string.Join(" ' ' ", Indices);
                writer.Write(subscripts);
                writer.Write(" ')");
            }

            if (WithValue)
            {
                writer.Write(" <' ");
                writer.Write(AccessName);

                if (Indices.Length > 0)
                {
                    writer.Write(" (");
                    string subscripts = string.Join(' ', Indices);
                    writer.Write(subscripts);
                    writer.Write(')');
                }

                if (ReferenceModifier != null)
                {
                    writer.Write(' ');
                    writer.Write(ReferenceModifier);
                }

                writer.Write(" '>'");
            }
            else
            {
                writer.Write('\'');
            }

            writer.WriteLine();
        }

        protected override void WriteEndLine(int level, TextWriter writer)
        {
            throw new InvalidOperationException("DISPLAY statement is not composite.");
        }
    }

    internal class GeneratedIf : GeneratedStatement
    {
        public string Condition { get; }

        public GeneratedIf(string condition)
            : base(false)
        {
            Condition = condition;
        }

        protected override void WriteStatementLine(int level, TextWriter writer)
        {
            WriteIndent(level, writer);
            writer.WriteLine($"IF ({Condition})");
        }

        protected override void WriteEndLine(int level, TextWriter writer)
        {
            WriteIndent(level, writer);
            writer.WriteLine("END-IF");
        }
    }
}
