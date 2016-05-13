using System;
using System.Collections.Generic;
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
    /// 
    /// + not allowed for derived class CharacterValue
    /// UserDefinedWord | CommentEntry | PictureCharacterString | ExecStatementText | SymbolicCharacter
    /// DEBUG_CONTENTS | DEBUG_ITEM | DEBUG_LINE | ... special registers ... | XML_NTEXT | XML_TEXT
    /// STANDARD_1 | STANDARD_2 | NATIVE | EBCDIC
    /// 
    /// + only allowed for derived class EnumValue
    /// UserDefinedWord
    /// FunctionName | LENGTH | RANDOM | WHEN_COMPILED
    /// ExecTranslatorName
    /// </summary>
    public class AlphanumericValue : LiteralValue<string>
    {
        public AlphanumericValue(Token t) : base(t)
        { }

        public AlphanumericValue(SymbolReference symbolicCharacterReference) : base(symbolicCharacterReference.NameLiteral.Token)
        {
            IsSymbolicCharacterReference = true;
        }

        public bool IsSymbolicCharacterReference { get; private set; }

        public bool ValueNeedsCompilationContext
        {
            get
            {
                if(IsSymbolicCharacterReference)
                {
                    return false;
                }                
                if(Token.TokenType >= TokenType.ADDRESS && Token.TokenType <= TokenType.XML_TEXT)
                {
                    // Special registers
                    return false;
                }
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
                        // Represents one or more occurrences of the character that has 
                        // the highest/lowest ordinal position in the collating sequence used. 
                        return true;
                    case TokenType.QUOTE:
                    case TokenType.QUOTES:
                        // The quotation mark character ("), if the QUOTE compiler option is in effect 
                        // The apostrophe character (’), if the APOST compiler option is in effect 
                        return true;
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        return false;
                    case TokenType.PictureCharacterString:
                    case TokenType.CommentEntry:
                    case TokenType.ExecStatementText:
                    case TokenType.FunctionName:
                    case TokenType.ExecTranslatorName:
                    case TokenType.UserDefinedWord:
                    case TokenType.SymbolicCharacter:
                        return false;
                    case TokenType.STANDARD_1:
                    case TokenType.STANDARD_2:
                    case TokenType.NATIVE:
                    case TokenType.EBCDIC:
                        return false;
                    default:
                        throw new InvalidOperationException("Unexpected literal value");
                }
            }
        }

        public bool ValueNeedsSymbolicCharactersMap
        {
            get
            {
                return IsSymbolicCharacterReference;
            }
        }

        public virtual bool ValueNeedsCharactersCountContext
        {
            get
            {
                return false;
            }
        }

        public CharacterEncodingType CharacterEncodingType
        {
            get
            {
                if(IsSymbolicCharacterReference)
                {
                    // symbolic - character always represents an alphanumeric character
                    return CharacterEncodingType.Alphanumeric;
                }
                if (Token.TokenType >= TokenType.ADDRESS && Token.TokenType <= TokenType.XML_TEXT)
                {
                    // Special registers
                    return CharacterEncodingType.Alphanumeric;
                }
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
                    case TokenType.PictureCharacterString:
                    case TokenType.CommentEntry:
                    case TokenType.ExecStatementText:
                    case TokenType.FunctionName:
                    case TokenType.ExecTranslatorName:
                    case TokenType.UserDefinedWord:
                    case TokenType.SymbolicCharacter:
                        return CharacterEncodingType.Alphanumeric;
                    case TokenType.STANDARD_1:
                    case TokenType.STANDARD_2:
                    case TokenType.NATIVE:
                    case TokenType.EBCDIC:
                        return CharacterEncodingType.Alphanumeric;
                    default:
                        throw new InvalidOperationException("Unexpected literal value");
                }
            }
        }

        public override string Value
        {
            get
            {
                if(ValueNeedsCompilationContext || ValueNeedsSymbolicCharactersMap || ValueNeedsCharactersCountContext)
                {
                    throw new InvalidOperationException("Impossible to evaluate literal value without context information");
                }

                if (Token.TokenType >= TokenType.ADDRESS && Token.TokenType <= TokenType.XML_TEXT)
                {
                    // Special registers
                    return Token.Text;
                }
                switch (Token.TokenType)
                {
                    case TokenType.AlphanumericLiteral:
                    case TokenType.HexadecimalAlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                    case TokenType.DBCSLiteral:
                    case TokenType.NationalLiteral:
                    case TokenType.HexadecimalNationalLiteral:
                        return ((AlphanumericLiteralTokenValue)Token.LiteralValue).Text;
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                        return " ";
                    case TokenType.ZERO:
                    case TokenType.ZEROS:
                    case TokenType.ZEROES:
                        return "0";
                    case TokenType.PictureCharacterString:
                    case TokenType.CommentEntry:
                    case TokenType.ExecStatementText:
                    case TokenType.FunctionName:
                    case TokenType.ExecTranslatorName:
                    case TokenType.UserDefinedWord:
                    case TokenType.SymbolicCharacter:
                        return Token.Text;
                    case TokenType.STANDARD_1:
                    case TokenType.STANDARD_2:
                    case TokenType.NATIVE:
                    case TokenType.EBCDIC:
                        return Token.Text;
                    default:
                        throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }
        
        public virtual string GetValueInContext(
            CollatingSequence usedCollatingSequence, bool apostCompilerOption,
            IDictionary<string, string> symbolicCharactersMap,
            int charactersCountContext)
        {
            if (IsSymbolicCharacterReference)
            {
                string symbolicCharacterValue = null;
                if (symbolicCharactersMap.TryGetValue(Token.Text, out symbolicCharacterValue))
                {
                    return symbolicCharacterValue;
                }
                else
                {
                    throw new InvalidOperationException("Undefined symbolic character reference");
                }
            }
            switch (Token.TokenType)
            {
                case TokenType.HIGH_VALUE:
                case TokenType.HIGH_VALUES:
                    // Represents one or more occurrences of the character that has 
                    // the highest/lowest ordinal position in the collating sequence used. 
                    return usedCollatingSequence.GetHighValueChar().ToString();
                case TokenType.LOW_VALUE:
                case TokenType.LOW_VALUES:
                    // Represents one or more occurrences of the character that has 
                    // the highest/lowest ordinal position in the collating sequence used. 
                    return usedCollatingSequence.GetLowValueChar().ToString();
                case TokenType.QUOTE:
                case TokenType.QUOTES:
                    // The quotation mark character ("), if the QUOTE compiler option is in effect 
                    // The apostrophe character (’), if the APOST compiler option is in effect 
                    if (apostCompilerOption)
                    {
                        return "'";
                    }
                    else
                    {
                        return "\"";
                    }
                default:
                    return Value;
            }
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

            // List of accepted token types is more restrictive than for the base class
            switch (Token.TokenType)
            {
                case TokenType.UserDefinedWord:
                case TokenType.FunctionName:
                case TokenType.LENGTH:
                case TokenType.RANDOM:
                case TokenType.WHEN_COMPILED:
                case TokenType.ExecTranslatorName:
                    break;
                default:
                    throw new InvalidOperationException("Unexpected literal token type");
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

    /// <summary>
    /// Value for tokens : 
    /// AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral |
    /// DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral |
    /// HIGH_VALUE | HIGH_VALUES | LOW_VALUE  | LOW_VALUES | QUOTE | QUOTES | SPACE | SPACES | ZERO  | ZEROS  | ZEROES
    /// </summary>
    public class CharacterValue : AlphanumericValue
    {
        public CharacterValue(Token t) : base(t)
        {
            // List of accepted token types is more restrictive than for the base class
            switch (Token.TokenType)
            {
                case TokenType.AlphanumericLiteral:
                case TokenType.HexadecimalAlphanumericLiteral:
                case TokenType.NullTerminatedAlphanumericLiteral:
                case TokenType.DBCSLiteral:
                case TokenType.NationalLiteral:
                case TokenType.HexadecimalNationalLiteral:
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
                    break;
                default:
                    throw new InvalidOperationException("Unexpected literal token type");
            }
        }

        public CharacterValue(SymbolReference symbolicCharacterReference) : base(symbolicCharacterReference)
        { }

        public char CharValue
        {
            get
            {
                string strValue = base.Value;
                if (!String.IsNullOrEmpty(strValue))
                {
                    return strValue[0];
                }
                else
                {
                    throw new InvalidOperationException("Unexpected literal token type");
                }
            }
        }

        public char GetCharValueInContext(
             CollatingSequence usedCollatingSequence, bool apostCompilerOption,
             IDictionary<string, string> symbolicCharactersMap)
        {
            string strValue = base.GetValueInContext(usedCollatingSequence, apostCompilerOption, symbolicCharactersMap, 1);
            if (!String.IsNullOrEmpty(strValue))
            {
                return strValue[0];
            }
            else
            {
                throw new InvalidOperationException("Unexpected literal token type");
            }
        }
    }

    /// <summary>
    /// Value for tokens :
    /// HIGH_VALUE | HIGH_VALUES | LOW_VALUE  | LOW_VALUES | QUOTE | QUOTES | SPACE | SPACES | ZERO  | ZEROS  | ZEROES |
    /// symbolicCharacterReference => SymbolReference
    /// ALL figurativeConstant | ALL notNullTerminatedAlphanumericOrNationalLiteralToken
    /// </summary>
    public class RepeatedCharacterValue : CharacterValue
    {
        public RepeatedCharacterValue(Token optionalALLToken, Token t) : base(t)
        {
            ALLToken = optionalALLToken;
        }

        public RepeatedCharacterValue(Token optionalALLToken, SymbolReference symbolicCharacterReference) : base(symbolicCharacterReference)
        {
            ALLToken = optionalALLToken;
        }

        public Token ALLToken { get; private set; }

        public override bool ValueNeedsCharactersCountContext
        {
            get
            {
                return true;
            }
        }

        public override string Value
        {
            get
            {
               throw new InvalidOperationException("Can't compute literal value without characters count context");
            }
        }

        public override string GetValueInContext(
            CollatingSequence usedCollatingSequence, bool apostCompilerOption,
            IDictionary<string, string> symbolicCharactersMap,
            int charactersCountContext)
        {
            char repeatedChar = base.GetCharValueInContext(usedCollatingSequence, apostCompilerOption, symbolicCharactersMap);
            return new string(repeatedChar, charactersCountContext);
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

        public Value(RepeatedCharacterValue repeatedAlphanumericValue)
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

        public RepeatedCharacterValue RepeatedAlphanumericValue { get; private set; }

        public NullPointerValue NullPointerValue { get; private set; }        
    }
}
