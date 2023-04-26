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
            /// Currency descriptor found in Picture, if any.
            /// </summary>
            public CurrencyDescriptor? CurrencyDescriptor { get; }

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
                : this(null, null)
            {

            }

            //Invalid result, but we have a parsed sequence and potentially a currency descriptor
            internal Result(Character[] sequence, CurrencyDescriptor? currencyDescriptor)
            {
                IsValid = false;
                Sequence = sequence;
                CurrencyDescriptor = currencyDescriptor;
                //Default values for the rest of properties
            }

            //Valid result
            internal Result(Character[] sequence, CurrencyDescriptor? currencyDescriptor, PictureCategory category, int digits, int realDigits, bool isSigned, int scale, int size)
            {
                IsValid = true;
                Sequence = sequence;
                CurrencyDescriptor = currencyDescriptor;
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
