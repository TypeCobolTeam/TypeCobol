namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// A validation context.
        /// </summary>
        public class Context
        {
            private readonly Result _result;

            internal Context(Character[] sequence, Result result)
            {
                Sequence = sequence;
                _result = result;
            }

            public int Scale => _result.Scale;
            public int RealDigits => _result.RealDigits;
            public bool IsSigned => _result.IsSigned;
            public bool IsExternalFloatSequence() => Category == PictureCategory.ExternalFloat;

            public int Digits => _result.Digits;
            public PictureCategory Category => _result.Category;
            internal Character[] Sequence { get; }
            public int Size => _result.Size;
            public bool IsDbcsSequence() => Category == PictureCategory.Dbcs;
        }
    }
}
