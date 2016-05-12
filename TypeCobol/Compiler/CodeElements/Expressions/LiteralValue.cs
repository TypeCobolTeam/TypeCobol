using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Value defined by a single token in the Cobol syntax
    /// </summary>
    public abstract class LiteralValue<T>
    {
        public LiteralValue(Token t) { Token = t; }

        /// <summary>
        /// Token defining the value
        /// </summary>
        public Token Token { get; private set; }

        /// <summary>
        /// Strongly typed value defined by the token
        /// </summary>
        public abstract T Value { get; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value.ToString();
        }
    }

    /// <summary>
    /// Value for tokens : TRUE | FALSE
    /// </summary>
    public class BooleanValue : LiteralValue<bool>
    {
        public BooleanValue(Token t) : base(t) { }

        /// <summary>
        /// Returns true for token TRUE, false for token FALSE.
        /// </summary>
        public override bool Value
        {
            get
            {
                switch(Token.TokenType)
                {
                    case TokenType.TRUE:
                        return true;
                    case TokenType.FALSE:
                        return false;
                    default:
                        throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }
    }

    /// <summary>
    /// Value for tokens : IntegerLiteral
    /// </summary>
    public class IntegerValue : LiteralValue<long>
    {
        public IntegerValue(Token t) : base(t) { }

        public override long Value
        {
            get
            {
                IntegerLiteralTokenValue integerLiteralValue = Token.LiteralValue as IntegerLiteralTokenValue;
                if (integerLiteralValue != null)
                {
                    return integerLiteralValue.Number;
                }
                else
                {
                    throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }
    }

    /// <summary>
    /// Value for tokens : IntegerLiteral | DecimalLiteral | FloatingPointLiteral | ZERO | ZEROS | ZEROES
    /// </summary>
    public class NumericValue : LiteralValue<double>
    {
        public NumericValue(Token t) : base(t) { }

        public override double Value
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.IntegerLiteral:
                        return ((IntegerLiteralTokenValue)Token.LiteralValue).Number;
                    case TokenType.DecimalLiteral:
                        return ((DecimalLiteralTokenValue)Token.LiteralValue).Number;
                    case TokenType.FloatingPointLiteral:
                        return ((FloatingPointLiteralTokenValue)Token.LiteralValue).Number;
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        return 0;
                    default:
                        throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }

        /// <summary>
        /// True if the literal represents an integer value
        /// </summary>
        public bool IsInteger
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.IntegerLiteral:
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        return true;
                    default:
                        return false;
                }
            }
        }

        /// <summary>
        /// If IsInteger is true, returns the integer value for this literal
        /// </summary>
        public long IntegerValue
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.IntegerLiteral:
                        return ((IntegerLiteralTokenValue)Token.LiteralValue).Number;
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        return 0;
                    default:
                        throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }
    }

    /// <summary>
    /// Value for tokens : 
    /// AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral |
    /// DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral |
    /// HIGH_VALUE | HIGH_VALUES | LOW_VALUE  | LOW_VALUES | QUOTE | QUOTES | SPACE | SPACES | ZERO  | ZEROS  | ZEROES |
    /// </summary>
    public class CharacterValue : LiteralValue<char>
    {
        public CharacterValue(Token t) : base(t) { }

        public override char Value
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.AlphanumericLiteral:
                    case TokenType.HexadecimalAlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                    case TokenType.DBCSLiteral:
                    case TokenType.NationalLiteral:
                    case TokenType.HexadecimalNationalLiteral:
                        string strValue = ((AlphanumericLiteralTokenValue)Token.LiteralValue).Text;
                        if (!String.IsNullOrEmpty(strValue))
                        {
                            return strValue[0];
                        }
                        else
                        {
                            throw new InvalidOperationException("Unexpected literal token type");
                        }
                    case TokenType.HIGH_VALUE:
                    case TokenType.HIGH_VALUES:
                    case TokenType.LOW_VALUE:
                    case TokenType.LOW_VALUES:
                    case TokenType.QUOTE:
                    case TokenType.QUOTES:
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                    default:
                        throw new NotImplementedException();
                }
            }
        }

        public CharacterEncodingType CharacterEncodingType
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.AlphanumericLiteral:
                    case TokenType.HexadecimalAlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                        return CharacterEncodingType.Alphanumeric;
                    case TokenType.DBCSLiteral:
                        return CharacterEncodingType.DBCS;
                    case TokenType.NationalLiteral:
                    case TokenType.HexadecimalNationalLiteral:
                        return CharacterEncodingType.National;
                    case TokenType.HIGH_VALUE:
                    case TokenType.HIGH_VALUES:
                    case TokenType.LOW_VALUE:
                    case TokenType.LOW_VALUES:
                        // treated as an alphanumeric literal in a context that requires an alphanumeric character
                        // treated as a national literal when used in a context that requires a national literal
                        return CharacterEncodingType.AlphanumericOrNational;
                    case TokenType.QUOTE:
                    case TokenType.QUOTES:
                        // represents an alphanumeric character when used in a context that requires an alphanumeric character
                        // represents a national character when used in a context that requires a national character
                        return CharacterEncodingType.AlphanumericOrNational;
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                        // treated as an alphanumeric literal when used in a context that requires an alphanumeric character
                        // as a DBCS literal when used in a context that requires a DBCS character
                        // as a national literal when used in a context that requires a national character
                        return CharacterEncodingType.AlphanumericOrDBCSOrNational;
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        // used in a context that requires an alphanumeric character, an alphanumeric character zero is used
                        // context requires a national character zero, a national character zero is used
                        return CharacterEncodingType.AlphanumericOrNational;
                    default:
                        throw new InvalidOperationException("Unexpected literal value");
                }
            }
        }

        public bool ValueNeedsContext
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.AlphanumericLiteral:
                    case TokenType.HexadecimalAlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                    case TokenType.DBCSLiteral:
                    case TokenType.NationalLiteral:
                    case TokenType.HexadecimalNationalLiteral:
                        return false; ;
                    case TokenType.HIGH_VALUE:
                    case TokenType.HIGH_VALUES:
                    case TokenType.LOW_VALUE:
                    case TokenType.LOW_VALUES:
                        // treated as an alphanumeric literal in a context that requires an alphanumeric character
                        // treated as a national literal when used in a context that requires a national literal 
                        return true;
                    case TokenType.QUOTE:
                    case TokenType.QUOTES:
                        return true;
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:

                    default:
                        throw new InvalidOperationException("Unexpected literal value");
                }
            }
        }
    }

    /// <summary>
    /// Value for tokens : 
    /// symbolicCharacterReference => SymbolReference
    /// </summary>
    public class SymbolicCharacterValue : CharacterValue
    {
        public SymbolicCharacterValue(SymbolReference symbolicCharacter) : 
            base(symbolicCharacter.NameLiteral.Token)
        {

        }
    }

    public enum CharacterEncodingType
    {
        Alphanumeric,
        DBCS,
        National,
        AlphanumericOrNational,
        AlphanumericOrDBCSOrNational
    }

    /// <summary>
    /// Value for tokens : 
    /// AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral |
    /// DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral |
    /// HIGH_VALUE | HIGH_VALUES | LOW_VALUE  | LOW_VALUES | QUOTE | QUOTES | SPACE | SPACES | ZERO  | ZEROS  | ZEROES |
    /// symbolicCharacterReference => SymbolReference
    /// UserDefinedWord | CommentEntry | PictureCharacterString | ExecStatementText | SymbolicCharacter
    /// DEBUG_CONTENTS | DEBUG_ITEM | DEBUG_LINE | ... special registers ... | XML_NTEXT | XML_TEXT
    /// STANDARD_1 | STANDARD_2 | NATIVE | EBCDIC
    /// </summary>
    public class AlphanumericValue : LiteralValue<string>
    {
        public AlphanumericValue(Token t) : base(t) { }

        public override string Value
        {
            get
            {
                if (Token.TokenFamily == TokenFamily.Symbol ||
                    Token.TokenFamily == TokenFamily.SyntaxKeyword)
                {
                    return Token.Text;
                }
                else if (Token.TokenFamily == TokenFamily.AlphanumericLiteral)
                {
                    return Token.Text;
                    //                    return ((AlphanumericLiteralValue)Token.LiteralValue).Text;
                }
                else if (Token.TokenFamily == TokenFamily.FigurativeConstantKeyword)
                {
                    return Token.Text;
                    //                    return ((AlphanumericLiteralValue)Token.LiteralValue).Text;
                }
                else
                {
                    throw new InvalidOperationException("A string value can not be defined by a token of type : " + Token.TokenType);
                }
            }
        }

        /// <summary>
        /// Value for tokens : 
        /// symbolicCharacterReference => SymbolReference
        /// </summary>
        public class SymbolicCharacterAlphanumericValue : AlphanumericValue
        {
            public SymbolicCharacterAlphanumericValue(SymbolReference symbolicCharacter) :
                base(symbolicCharacter.NameLiteral.Token)
            {

            }
        }

        /// <summary>
        /// Value for tokens :
        /// UserDefinedWord
        /// FunctionName | LENGTH | RANDOM | WHEN_COMPILED
        /// ExecTranslatorName
        /// </summary>
        public class EnumeratedValue : AlphanumericValue
        {
            public EnumeratedValue(Token t, Type enumType) : base(t)
            {
                EnumType = enumType;
            }

            public override string Value
            {
                get
                {
                    switch (Token.TokenType)
                    {
                        case TokenType.UserDefinedWord:
                        case TokenType.FunctionName:
                        case TokenType.LENGTH:
                        case TokenType.RANDOM:
                        case TokenType.WHEN_COMPILED:
                        case TokenType.ExecTranslatorName:
                            return Token.Text;
                        default:
                            throw new InvalidOperationException("Unexpected literal token type");
                    }
                }
            }

            /// <summary>
            /// C# enum type representing all the possible values
            /// </summary>
            public Type EnumType { get; private set; }

            /// <summary>
            /// C# enum value equivalent of this alphanumeric literal
            /// </summary>
            public object EnumValue
            {
                get { return Enum.Parse(EnumType, Value); }
            }
        }
     }

    /// <summary>
    /// Value for tokens :
    /// HIGH_VALUE | HIGH_VALUES | LOW_VALUE  | LOW_VALUES | QUOTE | QUOTES | SPACE | SPACES | ZERO  | ZEROS  | ZEROES |
    /// symbolicCharacterReference => SymbolReference
    /// ALL figurativeConstant | ALL notNullTerminatedAlphanumericOrNationalLiteralToken
    /// </summary>
    public class RepeatedAlphanumericValue : AlphanumericValue
    {
        public RepeatedAlphanumericValue(Token optionalALL, Token t) : base(t)
        {
            OptionalALLToken = optionalALL;
        }

        public Token OptionalALLToken { get; private set; }

        public override string Value
        {
            get
            {
               throw new InvalidOperationException("Can't compute literal value without context information");
            }
        }
    }

    /// <summary>
    /// Value for tokens : NULL
    /// </summary>
    public class NullPointerValue : LiteralValue<bool>
    {
        public NullPointerValue(Token t) : base(t) { }

        /// <summary>
        /// Returns true for token NULL
        /// </summary>
        public override bool Value
        {
            get
            {
                switch (Token.TokenType)
                {
                    case TokenType.NULL:
                        return true;
                    default:
                        throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }
    }

    /// <summary>
    /// Union class used to store any type of LiteralValue
    /// </summary>
    public class Value
    {
        public Value(NumericValue numericValue)
        {
            LiteralType = ValueLiteralType.Numeric;
            NumericValue = numericValue;
        }

        public Value(AlphanumericValue alphanumericValue)
        {
            LiteralType = ValueLiteralType.Alphanumeric;
            AlphanumericValue = alphanumericValue;
        }

        public Value(RepeatedAlphanumericValue repeatedAlphanumericValue)
        {
            LiteralType = ValueLiteralType.RepeatedAlphanumeric;
            RepeatedAlphanumericValue = repeatedAlphanumericValue;
        }

        public Value(NullPointerValue nullPointerValue)
        {
            LiteralType = ValueLiteralType.NullPointer;
            NullPointerValue = nullPointerValue;
        }

        public enum ValueLiteralType
        {
            Numeric,
            Alphanumeric,
            RepeatedAlphanumeric,
            NullPointer
        }

        public ValueLiteralType LiteralType { get; private set; }

        public NumericValue NumericValue { get; private set; }

        public AlphanumericValue AlphanumericValue { get; private set; }

        public RepeatedAlphanumericValue RepeatedAlphanumericValue { get; private set; }

        public NullPointerValue NullPointerValue { get; private set; }        
    }
}
