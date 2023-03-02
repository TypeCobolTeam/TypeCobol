namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Data structure equivalent to TextDocumentContentChangeEvent from LSP
    /// but for the parser.
    /// </summary>
    public readonly struct RangeUpdate
    {
        public int LineStart { get; }

        public int ColumnStart { get; }

        public int LineEnd { get; }

        public int ColumnEnd { get; }

        public string Text { get; }

        public RangeUpdate(int lineStart, int columnStart, int lineEnd, int columnEnd, string text)
        {
            LineStart = lineStart;
            ColumnStart = columnStart;
            LineEnd = lineEnd;
            ColumnEnd = columnEnd;
            Text = text;
        }

        public override string ToString()
        {
            return $"RangeUpdate ({LineStart}, {ColumnStart}) -> ({LineEnd}, {ColumnEnd}): {Text}";
        }
    }
}
