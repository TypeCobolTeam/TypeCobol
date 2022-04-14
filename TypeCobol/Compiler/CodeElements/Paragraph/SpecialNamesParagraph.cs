using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The SPECIAL-NAMES paragraph:
    /// - Relates IBM-specified environment-names to user-defined mnemonic-names
    /// - Relates alphabet-names to character sets or collating sequences
    /// - Specifies symbolic characters
    /// - Relates class names to sets of characters
    /// - Specifies one or more currency sign values and defines a picture symbol to
    ///   represent each currency sign value in PICTURE clauses
    /// - Specifies that the functions of the comma and decimal point are to be
    ///   interchanged in PICTURE clauses and numeric literals
    /// - Relates xml-schema-names to ddnames or environment variable names
    ///   identifying files containing XML schemas
    /// </summary>
    public class SpecialNamesParagraph : CodeElement
    {
		public SpecialNamesParagraph() : base(CodeElementType.SpecialNamesParagraph)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// Relates IBM-specified environment-names to user-defined mnemonic-names
        /// </summary>
        public IDictionary<SymbolDefinition, ExternalName> MnemonicsForEnvironmentNames { get; set; }

        /// <summary>
        /// Relates IBM-specified user-programmable status indicator switches to user-defined mnemonic-names
        /// </summary>
        public IDictionary<SymbolDefinition, ExternalName> MnemonicsForUPSISwitchNames { get; set; }

        /// <summary>
        /// A condition-name can be associated with the on status or off status of each UPSI switch specified.
        /// </summary>
        public IDictionary<SymbolDefinition, Tuple<ExternalName, UPSISwitchStatus>> ConditionNamesForUPSISwitchStatus { get; set; }

        /// <summary>
        /// The ALPHABET clause provides a means of relating an alphabet-name to a
        /// specified character code set or collating sequence.
        /// The related character code set or collating sequence can be used for alphanumeric
        /// data, but not for DBCS or national data.
        /// </summary>
        public IDictionary<SymbolDefinition, CollatingSequence> AlphabetNames { get; set; }

        /// <summary>
        /// The SYMBOLIC CHARACTERS clause provides a means of specifying one or more symbolic characters.
        /// Each character represented is an alphanumeric character (applicable only to single-byte character).
        /// </summary>
        public IDictionary<SymbolDefinition, Tuple<IntegerValue, SymbolReference>> SymbolicCharacters { get; set; }

        /// <summary>
        /// The CLASS clause provides a means for relating a name to the specified set of characters listed in that clause.
        /// </summary>
        public IDictionary<SymbolDefinition, UserDefinedCollatingSequence> CharsetClassNames { get; set; }

        /// <summary>
        /// The CURRENCY SIGN clause affects numeric-edited data items whose PICTURE
        /// character-strings contain a currency symbol.
        /// A currency symbol represents a currency sign value that is:
        /// - Inserted in such data items when they are used as receiving items
        /// - Removed from such data items when they are used as sending items for a
        /// numeric or numeric-edited receiver
        /// Typically, currency sign values identify the monetary units stored in a data item.
        /// For example: '$', 'EUR', 'CHF', 'JPY', 'HK$', 'HKD', or X'9F' (hexadecimal code point
        /// in some EBCDIC code pages for €, the Euro currency sign). 
        /// The CURRENCY SIGN clause specifies a currency sign value and the currency
        /// symbol used to represent that currency sign value in a PICTURE clause.
        /// </summary>
        public IDictionary<AlphanumericValue, CharacterValue> CurrencySymbols { get; set; }

        /// <summary>
        /// The DECIMAL-POINT IS COMMA clause exchanges the functions of the period
        /// and the comma in PICTURE character-strings and in numeric literals.
        /// </summary>
        public SyntaxProperty<bool> DecimalPointIsComma { get; set; }

        public bool DecimalPointIsComma2 {
            get { return DecimalPointIsComma != null && DecimalPointIsComma.Value; }
        }
        /// <summary>
        /// The XML-SCHEMA clause provides the means of relating xml-schema-name-1 to an
        /// external file identifier: a ddname or environment variable that identifies the actual
        /// external file that contains the optimized XML schema.
        /// </summary>
        public IDictionary<SymbolDefinition, ExternalName> XmlSchemaNames { get; set; }
    }
    
    /// <summary>
    /// User-programmable status indicator
    /// </summary>
    public enum UPSISwitchStatus
    {
        On, Off
    }

    public abstract class CollatingSequence
    {
        public char GetLowValueChar() { return Char.MinValue; }

        public char GetHighValueChar() { return Char.MaxValue; }
    }

    public class InstrinsicCollatingSequence : CollatingSequence
    {
        public SymbolReference IntrinsicAlphabetName { get; set; }
    }

    /// <summary>
    /// Specifies that the collating sequence for alphanumeric data is
    /// determined by the program, according to the following rules:
    /// - The order in which literals appear specifies the ordinal number,
    /// in ascending sequence, of the characters in this collating
    /// sequence.
    /// - Each numeric literal specified must be an unsigned integer.
    /// - Each numeric literal must have a value that corresponds to a
    /// valid ordinal position within the collating sequence in effect.
    /// - Each character in an alphanumeric literal represents that actual
    /// character in the character set. (If the alphanumeric literal
    /// contains more than one character, each character, starting with
    /// the leftmost, is assigned a successively ascending position within
    /// this collating sequence.)
    /// - Any characters that are not explicitly specified assume positions
    /// in this collating sequence higher than any of the explicitly
    /// specified characters. The relative order within the collating
    /// sequence of these unspecified characters is their relative order in
    /// the collating sequence indicated by the COLLSEQ compiler
    /// option.
    /// - Within one alphabet-name clause, a given character must not be
    /// specified more than once.
    /// - Each alphanumeric literal associated with a THROUGH or ALSO
    /// phrase must be one character in length.
    /// - When the THROUGH phrase is specified, the contiguous
    /// characters in the native character set beginning with the
    /// character specified by literal-1 and ending with the character
    /// specified by literal-2 are assigned successively ascending
    /// positions in this collating sequence.
    /// This sequence can be either ascending or descending within the
    /// original native character set. That is, if "Z" THROUGH "A" is
    /// specified, the ascending values, left-to-right, for the uppercase
    /// letters are:
    /// ZYXWVUTSRQPONMLKJIHGFEDCBA
    /// - When the ALSO phrase is specified, the characters specified as
    /// literal-1, literal-3, ... are assigned to the same position in this
    /// collating sequence. For example, if you specify:
    /// "D" ALSO "N" ALSO "%"
    /// the characters D, N, and % are all considered to be in the same
    /// position in the collating sequence.
    /// - When the ALSO phrase is specified and alphabet-name-1 is
    /// referenced in a SYMBOLIC CHARACTERS clause, only literal-1
    /// is used to represent the character in the character set.
    /// - The character that has the highest ordinal position in this
    /// collating sequence is associated with the figurative constant
    /// HIGH-VALUE. If more than one character has the highest
    /// position because of specification of the ALSO phrase, the last
    /// character specified (or defaulted to when any characters are not
    /// explicitly specified) is considered to be the HIGH-VALUE
    /// character for procedural statements such as DISPLAY and as the
    /// sending field in a MOVE statement. (If the ALSO phrase
    /// example given above were specified as the high-order characters
    /// of this collating sequence, the HIGH-VALUE character would be
    /// %.)
    /// - The character that has the lowest ordinal position in this
    /// collating sequence is associated with the figurative constant
    /// LOW-VALUE. If more than one character has the lowest position
    /// because of specification of the ALSO phrase, the first character
    /// specified is the LOW-VALUE character. (If the ALSO phrase
    /// example given above were specified as the low-order characters
    /// of the collating sequence, the LOW-VALUE character would be
    /// D.)
    /// </summary>
    public class UserDefinedCollatingSequence : CollatingSequence
    {
        public CharacterSetInCollatingSequence[] CharacterSets { get; set; }
    }

    public abstract  class CharacterSetInCollatingSequence { }

    // In the rule below, if characterInCollatingSequence is an alphanumeric literal, 
    // it may contain SEVERAL characters

    public class SingleCharacterInCollatingSequence : CharacterSetInCollatingSequence
    {
        public CharacterInCollatingSequence Character { get; set; }
    }

    // In the two rules below, if characterInCollatingSequence is an alphanumeric literal, 
    // it can contain ONLY ONE characters

    public class CharactersRangeInCollatingSequence : CharacterSetInCollatingSequence
    {
        public CharacterInCollatingSequence StartCharacter { get; set; }

        public CharacterInCollatingSequence EndCharacter { get; set; }
    }

    public class CharactersEqualSetInCollatingSequence : CharacterSetInCollatingSequence
    {
        public CharacterInCollatingSequence[] EqualCharacters { get; set; }
    }
    
    public class CharacterInCollatingSequence : CharacterSetInCollatingSequence
    {
        public CharacterValue CharacterValue { get; set; }

        public IntegerValue OrdinalPositionInCollatingSequence { get; set; }
    }
}
