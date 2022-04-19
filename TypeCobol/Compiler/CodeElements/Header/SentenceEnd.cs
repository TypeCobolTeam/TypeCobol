using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Sentence
    /// One or more statements terminated by a separator period.
    /// </summary>
    public class SentenceEnd : CodeElementEnd
    {
        public SentenceEnd() : base(CodeElementType.SentenceEnd)
        { }
        public override TextAreaType StartingArea => TextAreaType.Source;
    }
}
