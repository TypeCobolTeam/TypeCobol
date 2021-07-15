using System.IO;
using System.Linq;
using System.Text;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// A Type that Comes from a COBOL PICTURE clause.
    /// </summary>
    public class PictureType : Type
    {
        /// <summary>
        /// Indicates whether the usage is compatible with a PictureType.
        /// </summary>
        public static bool IsUsageCompatible(UsageFormat usage)
        {
            switch (usage)
            {
                case UsageFormat.Comp1:
                case UsageFormat.Comp2:
                case UsageFormat.ObjectReference:
                case UsageFormat.Pointer:
                case UsageFormat.FunctionPointer:
                case UsageFormat.ProcedurePointer:
                    return false;
                default:
                    return true;
            }
        }

        /// <summary>
        /// Validator constructor.
        /// </summary>
        public PictureType(PictureValidator.Result validationResult, bool isSeparateSign)
            : base(Tags.Picture)
        {
            System.Diagnostics.Debug.Assert(validationResult != null);
            if (validationResult.IsValid)
            {
                IsSigned = validationResult.IsSigned;
                Scale = validationResult.Scale;
                Digits = validationResult.Digits;
                RealDigits = validationResult.RealDigits;
                Category = validationResult.Category;
                Sequence = validationResult.Sequence;
                Size = validationResult.Size;
                IsSeparateSign = isSeparateSign;
            }
            else
            {
                Category = PictureCategory.Error;
            }
        }

        /// <summary>
        /// The Category of picture type.
        /// </summary>
        public PictureCategory Category { get; }

        /// <summary>
        /// Indicating whether the sign is separate character
        /// </summary>
        public bool IsSeparateSign { get; }

        /// <summary>
        /// a Normalized Textual String representation of the Picture clause.
        /// </summary>
        public string Picture
        {
            get
            {
                if (Sequence != null)
                {
                    StringBuilder sb = new StringBuilder();
                    foreach (var c in Sequence)
                    {
                        sb.Append(c);
                    }

                    return sb.ToString();
                }

                return "???";
            }
        }

        /// <summary>
        /// The Size of this Picture Type, the size in bytes.
        /// </summary>
        public int Size { get; }

        /// <summary>
        /// The count of digits places in a Numeric or NumericEdited category of PICTURE
        /// </summary>
        public int Digits { get; }

        /// <summary>
        /// The read count of digits, that are represented by the '9' symbol.
        /// </summary>
        public int RealDigits { get; }

        /// <summary>
        /// If this type is decimal, this the number of digits after the decimal
        /// separator character.
        /// </summary>
        public int Scale { get; }

        /// <summary>
        /// Is this type marked this S, that is to say signed.
        /// </summary>
        public bool IsSigned { get; }

        /// <summary>
        /// The sequence of characters that was calculated by the PictureValidator
        /// to validate this PICTURE string.
        /// </summary>
        internal PictureValidator.Character[] Sequence { get; }

        /// <summary>
        /// Get this picture Type Length;
        /// </summary>
        public override int Length
        {
            get
            {
                if (Category == PictureCategory.Error)
                    return 0;
                if (Usage == UsageFormat.None)
                {
                    int add = 0;
                    if (Category == PictureCategory.Dbcs)
                    {
                        add = Sequence.Sum(c => c.SpecialChar == PictureValidator.SC.B ? c.Count : 0);
                    }
                    return Size + add;
                }
                if (Category == PictureCategory.ExternalFloat)
                {
                    return Size;
                }
                switch (Usage)
                {
                    case UsageFormat.Comp:
                    case UsageFormat.Comp5:
                        // The Picture must be a numeric Picture.
                        System.Diagnostics.Contracts.Contract.Requires(Category == PictureCategory.Numeric);
                        System.Diagnostics.Debug.Assert(Category == PictureCategory.Numeric);
                        if (this.RealDigits <= 4)
                        {
                            return 2;//2 bytes half-word
                        }
                        if (this.RealDigits <= 9)
                        {
                            return 4;//4 bytes full-word
                        }
                        else
                        {
                            return 8;//8 bytes double word.
                        }
                    case UsageFormat.Comp3:
                        {
                            //S9(4) COMP - 3 would occupy 2 bytes.
                            //S9(6) COMP - 3 would occupy 3 bytes.
                            //S9(7) COMP - 3 would occupy 4 bytes.
                            //S9(n) COMP - 3 would occupy (n + 1) / 2 bytes

                            bool odd = (this.RealDigits % 2) != 0;
                            int len = (this.RealDigits / 2) + (this.RealDigits % 2);
                            len += odd ? 0 : 1;//for sign
                            return len;
                        }
                    case UsageFormat.Display1:
                        {
                            int len = Size;
                            foreach (PictureValidator.Character c in Sequence)
                            {
                                switch (c.SpecialChar)
                                {
                                    case PictureValidator.SC.A:
                                    case PictureValidator.SC.E:
                                    case PictureValidator.SC.X:
                                    case PictureValidator.SC.G:
                                    case PictureValidator.SC.N:
                                    case PictureValidator.SC.CR:
                                    case PictureValidator.SC.DB:
                                    case PictureValidator.SC.P:
                                    case PictureValidator.SC.V:
                                    case PictureValidator.SC.S:
                                        break;
                                    case PictureValidator.SC.B:
                                        if (Category == PictureCategory.Dbcs)
                                        {
                                            len += c.Count;//double the size of B.
                                        }
                                        break;
                                    default:
                                        len += c.Count;//double the size
                                        break;
                                }
                            }
                            return len;
                        }
                    case UsageFormat.National:
                        {
                            int len = Size;
                            foreach (PictureValidator.Character c in Sequence)
                            {
                                switch (c.SpecialChar)
                                {
                                    case PictureValidator.SC.S:
                                        if (IsSeparateSign)
                                        {
                                            len += c.Count;//double the size of S.
                                        }
                                        break;
                                    case PictureValidator.SC.A:
                                    case PictureValidator.SC.B:
                                    case PictureValidator.SC.Z:
                                    case PictureValidator.SC.NINE:
                                    case PictureValidator.SC.DOT:
                                    case PictureValidator.SC.COMMA:
                                        len += c.Count;//double the size.
                                        break;
                                }
                            }
                            return len;
                        }
                }
                return Size;
            }
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"Picture: {Picture}");
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitPictureType(this, arg);
        }
    }
}
