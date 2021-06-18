namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// A Picture Character String validation result.
        /// </summary>
        public class Result
        {
            /// <summary>
            /// Is the string valid or not ?
            /// </summary>
            public bool IsValid { get; }

            /// <summary>
            /// Parsed character sequence.
            /// </summary>
            internal Character[] Sequence { get; }

            /// <summary>
            /// Computed picture category.
            /// </summary>
            public PictureCategory Category { get; }

            /// <summary>
            /// Computed number of digits.
            /// </summary>
            public int Digits { get; }

            /// <summary>
            /// Computed total number of digits.
            /// </summary>
            public int RealDigits { get; }

            /// <summary>
            /// Does this sequence have a sign ?
            /// </summary>
            public bool IsSigned { get; }

            /// <summary>
            /// 1^10 scale.
            /// </summary>
            public int Scale { get; }

            /// <summary>
            /// Size in bytes.
            /// </summary>
            public int Size { get; }

            //Invalid result, no parsed sequence
            internal Result()
                : this(null)
            {

            }

            //Invalid result, but we have a parsed sequence
            internal Result(Character[] sequence)
            {
                IsValid = false;
                Sequence = sequence;
                //Default values for the rest of properties
            }

            //Valid result
            internal Result(Character[] sequence, PictureCategory category, int digits, int realDigits, bool isSigned, int scale, int size)
            {
                IsValid = true;
                Sequence = sequence;
                Category = category;
                Digits = digits;
                RealDigits = realDigits;
                IsSigned = isSigned;
                Scale = scale;
                Size = size;
            }
        }
    }
}
