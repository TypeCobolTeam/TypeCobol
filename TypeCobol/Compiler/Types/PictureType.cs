using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
            : base(Tags.Picture)
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
        public PictureType(String value, bool separateSign) : this(new PictureValidator(value, separateSign))
        {
        }

        /// <summary>
        /// Validator constructor.
        /// </summary>
        /// <param name="validator"></param>
        public PictureType(PictureValidator validator)
            : base(Tags.Picture)
        {
            AssignFromValidator(validator);
        }

        /// <summary>
        /// The Category  of picure type.
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
                this.IsSigned = validator.ValidationContext.HaveSign;
                this.Scale = validator.ValidationContext.Scale;
                this.Digits = validator.ValidationContext.Digits;
                this.RealDigits = validator.ValidationContext.RealDigits;
                this.IsSigned = validator.ValidationContext.HaveSign;
                this.Category = validator.ValidationContext.Category;
                this.IsExternalFloat = validator.ValidationContext.IsExternalFloatSequence();
                this.Sequence = validator.ValidationContext.Sequence.ToArray();
                this.Size = validator.ValidationContext.Size;
                this.IsSeparateSign = validator.ValidationContext.IsSeparateSign;
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
        public String Picture
        {
            get
            {
                return ConsumedToken.Text;
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
        /// Is this picture an external Floting point ?
        /// </summary>
        public bool IsExternalFloat
        {
            get;
            private set;
        }

        /// <summary>
        /// The sequence of charcaters that was calculated by the PictureValidator
        /// to validate this PICTURE string.
        /// </summary>
        internal PictureValidator.Character[] Sequence
        {
            get;
            private set;
        }

        /// <summary>
        /// Sets the usage associated to this PICTURE type.
        /// </summary>
        public override UsageFormat Usage
        {
            get
            {
                return base.Usage;
            }
            set
            {
                bool bNotValid = (value == UsageFormat.Comp1 ||
                        value == UsageFormat.Comp2 ||
                        value == UsageFormat.ObjectReference ||
                        value == UsageFormat.Pointer ||
                        value == UsageFormat.FunctionPointer ||
                        value == UsageFormat.ProcedurePointer
                    );
                System.Diagnostics.Contracts.Contract.Requires(!bNotValid);
                System.Diagnostics.Debug.Assert(!bNotValid);
                if (bNotValid)
                {
                    throw new ArgumentException("Invalid PICTURE Usage : " + value.ToString());
                }
                base.Usage = value;
            }
        }

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
                        add = Sequence.Sum(c => c.ch == PictureValidator.SC.B ? c.count : 0);
                    }
                    return Size + add;
                }
                if (IsExternalFloat)
                {
                    int add = 0;
                    if (Category == PictureCategory.Dbcs)
                    {
                        add = Sequence.Sum(c => c.ch == PictureValidator.SC.B ? c.count : 0);
                    }
                    return Size + add;
                }
                switch (Usage)
                {
                    case UsageFormat.Binary:
                    case UsageFormat.Comp:
                    case UsageFormat.Comp4:
                    case UsageFormat.Comp5:
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
                    case UsageFormat.Comp3:
                    case UsageFormat.PackedDecimal:
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
                                switch (c.ch)
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
                                            len += c.count;//double the size of B.
                                        }
                                        break;
                                    default:
                                        len += c.count;//double the size
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
                                switch (c.ch)
                                {
                                    case PictureValidator.SC.S:
                                        if (IsSeparateSign)
                                        {
                                            len += c.count;//double the size of S.
                                        }
                                        break;
                                    case PictureValidator.SC.A:
                                    case PictureValidator.SC.B:
                                    case PictureValidator.SC.Z:
                                    case PictureValidator.SC.NINE:
                                    case PictureValidator.SC.DOT:
                                    case PictureValidator.SC.COMMA:
                                        len += c.count;//double the size.
                                        break;
                                }
                            }
                            return len;
                        }
                }
                return Size;
            }
        }
    }
}
