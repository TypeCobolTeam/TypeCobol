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
        /// Empty Constructor
        /// </summary>
        public PictureType()
            : base(Type.Tags.Picture)
        {
            Category = PictureCategory.Error;
        }

        /// <summary>
        /// AlphaNumericValue Constructor
        /// <param name="separateSign">a boolean value indicating whether the sign is separate character</param>
        /// </summary>
        /// <param name="tokens"></param>
        public PictureType(Compiler.CodeElements.AlphanumericValue value, bool separateSign) : this(value.Token, separateSign)
        {
        }

        /// <summary>
        /// Token constructor
        /// </summary>
        /// <param name="token">The consumed token corresponding to this Picture</param>
        /// <param name="separateSign">a boolean value indicating whether the sign is separate character</param>
        public PictureType(TypeCobol.Compiler.Scanner.Token token, bool separateSign) : this()
        {
            ConsumedToken = token;
            IsSeparateSign = separateSign;
        }

        /// <summary>
        /// String constructor
        /// </summary>
        /// <param name="value">Picture string value</param>
        /// <param name="separateSign">a boolean value indicating whether the sign is separate character</param>
        public PictureType(string value, bool separateSign) : this(new PictureValidator(value, separateSign))
        {            
        }

        /// <summary>
        /// Validator constructor.
        /// </summary>
        /// <param name="validator"></param>
        public PictureType(PictureValidator validator)
            : base(Type.Tags.Picture)
        {
            AssignFromValidator(validator);
        }

        /// <summary>
        /// The Category of picture type.
        /// </summary>
        public PictureCategory Category
        {
            get;
            private set;
        }

        /// <summary>
        /// Indicating whether the sign is separate character
        /// </summary>
        public bool IsSeparateSign
        {
            get;
            private set;
        }

        /// <summary>
        /// The Consumed Token of this Picture String
        /// </summary>
        TypeCobol.Compiler.Scanner.Token m_ConsumedToken;

        /// <summary>
        /// The Consumed Tokens that represents this Picture Type.
        /// </summary>
        public TypeCobol.Compiler.Scanner.Token ConsumedToken
        {
            get
            {
                return m_ConsumedToken;
            }
            private set
            {
                System.Diagnostics.Debug.Assert(value != null);
                m_ConsumedToken = value;
                //Use the Automata Picture String Validator
                PictureValidator validator = new PictureValidator(value.Text, IsSeparateSign);
                AssignFromValidator(validator);
            }
        }

        /// <summary>
        /// Assign this Picture Type from a Validator
        /// </summary>
        private void AssignFromValidator(PictureValidator validator)
        {
            System.Diagnostics.Debug.Assert(validator != null);
            if (validator.IsValid())
            {
                this.IsSigned = validator.ValidationContext.IsSigned;
                this.Scale = validator.ValidationContext.Scale;
                this.Digits = validator.ValidationContext.Digits;
                this.RealDigits = validator.ValidationContext.RealDigits;
                this.Category = validator.ValidationContext.Category;
                this.IsExternalFloat = validator.ValidationContext.IsExternalFloatSequence();
                this.Sequence = validator.ValidationContext.Sequence.ToArray();
                this.Size = validator.ValidationContext.Size;
                this.IsSeparateSign = validator.IsSeparateSign;
                if (validator.ValidationContext.IsDbcsSequence())
                    Category = PictureCategory.Dbcs;
                if (this.IsExternalFloat)
                    Category = PictureCategory.ExternalFloat;
            }
            else
            {
                Category = PictureCategory.Error;
            }
        }
        /// <summary>
        /// a Normalized Textual String representation of the Picture clause.
        /// </summary>
        public string Picture
        {
            get
            {
                if (ConsumedToken != null)
                {
                    return ConsumedToken.Text;
                }
                else if (Sequence != null)
                {
                    StringBuilder sb = new StringBuilder();
                    foreach (var c in Sequence)
                    {
                        sb.Append(c);
                    }

                    return sb.ToString();
                }
                else return "???";
            }
        }

        /// <summary>
        /// The decimal character used if this type is a Decimal,
        /// otherwise the Decimal character is '\0'
        /// </summary>
        public char DecimalSeparator
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Size of this Picture Type, the sizein byte.
        /// </summary>
        public int Size
        {
            get;
            internal set;
        }

        /// <summary>
        /// The count of digits places in a Numeric or NumericEdited category of PICTURE
        /// </summary>
        public int Digits
        {
            get;
            internal set;
        }

        /// <summary>
        /// The read count of digits, that are represented by the '9' symbol.
        /// </summary>
        public int RealDigits
        {
            get;
            internal set;
        }

        /// <summary>
        /// If this type is decimal, this the number of digits after the decimal
        /// separator character.
        /// </summary>
        public int Scale
        {
            get;
            internal set;
        }

        /// <summary>
        /// Is this type marked this S, that is to say signed.
        /// </summary>
        public bool IsSigned
        {
            get;
            private set;
        }

        /// <summary>
        /// Is this picture an external Floating point ?
        /// </summary>
        public bool IsExternalFloat
        {
            get;
            private set;
        }

        /// <summary>
        /// The sequence of characters that was calculated by the PictureValidator
        /// to validate this PICTURE string.
        /// </summary>
        internal PictureValidator.Character[] Sequence
        {
            get;
            private set;
        }

        /// <summary>
        /// Indicates whether the usage is compatible with this PictureType.
        /// </summary>
        public bool IsUsageValid =>
            !(Usage == UsageFormat.Comp1 ||
              Usage == UsageFormat.Comp2 ||
              Usage == UsageFormat.ObjectReference ||
              Usage == UsageFormat.Pointer ||
              Usage == UsageFormat.FunctionPointer ||
              Usage == UsageFormat.ProcedurePointer);

        /// <summary>
        /// Get this picture Type Length;
        /// </summary>
        public override int Length
        {
            get
            {
                if (Category == PictureCategory.Error)
                    return 0;
                if (Usage == Type.UsageFormat.None)
                {
                    int add = 0;
                    if (Category == PictureCategory.Dbcs)
                    {
                        add = Sequence.Sum(c => c.SpecialChar == PictureValidator.SC.B ? c.Count : 0);
                    }
                    return Size + add;
                }
                if (IsExternalFloat)
                {
                    int add = 0;
                    if (Category == PictureCategory.Dbcs)
                    {
                        add = Sequence.Sum(c => c.SpecialChar == PictureValidator.SC.B ? c.Count : 0);
                    }
                    return Size + add;
                }
                switch (Usage)
                {
                    case Type.UsageFormat.Comp:
                    case Type.UsageFormat.Comp5:
                        // The Picture must be a numeric Picture.
                        System.Diagnostics.Contracts.Contract.Requires(Category == PictureCategory.Numeric);
                        System.Diagnostics.Debug.Assert(Category == PictureCategory.Numeric);
                        if (this.RealDigits <= 4)
                        {
                            return 2;//2 bytes halfword
                        }
                        if (this.RealDigits <= 9)
                        {
                            return 4;//4 bytes fullword
                        }
                        else
                        {
                            return 8;//8 bytes double word.
                        }
                    case Type.UsageFormat.Comp3:
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
                    case Type.UsageFormat.Display1:
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
                    case Type.UsageFormat.National:
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
