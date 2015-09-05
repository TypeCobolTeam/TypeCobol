using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{

    /// <summary>
    /// p346:
    /// The INSPECT statement examines characters or groups of characters in a data item.
    ///
    /// The INSPECT statement does the following tasks:
    /// * Counts the occurrences of a specific character (alphanumeric, DBCS, or national)
    /// in a data item (formats 1 and 3).
    /// * Counts the occurrences of specific characters and fills all or portions of a data
    /// item with specified characters, such as spaces or zeros (formats 2 and 3).
    /// * Converts all occurrences of specific characters in a data item to user-supplied
    /// replacement characters (format 4).
    /// </summary>
    public class InspectStatement : CodeElement
    {
        /// <summary>
        /// p349:
        /// identifier-1
        /// Is the inspected item and can be any of the following items:
        /// * An alphanumeric group item or a national group item
        /// * An elementary data item described explicitly or implicitly with usage
        /// DISPLAY, DISPLAY-1, or NATIONAL. The item can have any category
        /// that is valid for the selected usage.
        /// </summary>
        public Identifier Item;

        public IList<Tallying> TallyingList = new List<Tallying>();
        public IList<Subject> ReplacingCharacters  = new List<Subject>();
        public IList<ALF> ReplacingIdentifiers = new List<ALF>();

        public InspectStatement() : base(CodeElementType.InspectStatement) { }



        /// <summary>
        /// p349:
        /// TALLYING phrase (formats 1 and 3)
        /// This phrase counts the occurrences of a specific character or special character in a
        /// data item.
        ///
        /// When identifier-1 is a DBCS data item, DBCS characters are counted; when
        /// identifier-1 is a data item of usage national, national characters (encoding units) are
        /// counted; otherwise, alphanumeric characters (bytes) are counted.
        /// </summary>
        public class Tallying
        {
            /// <summary>
            /// p350:
            /// identifier-2
            /// Is the count field, and must be an elementary integer item defined without
            /// the symbol P in its PICTURE character-string.
            /// identifier-2 cannot be of category external floating-point.
            /// You must initialize identifier-2 before execution of the INSPECT statement
            /// begins.
            ///
            /// Usage note: The count field can be an integer data item defined with usage
            /// NATIONAL.
            /// </summary>
            public Identifier Count;

            public Subject CharactersPhrase;
            public ALF IdentifiersPhrase;
        }

        public class ALF
        {
            /// <summary>
            /// p351:
            /// When ALL is specified and neither the BEFORE nor AFTER phrase is
            /// specified, the substitution field replaces each nonoverlapping occurrence of
            /// the subject field in the inspected item (identifier-1), beginning at the
            /// leftmost character position and continuing to the rightmost.
            /// </summary>
            public bool All = false;
            /// <summary>
            /// p351:
            /// When LEADING is specified and neither the BEFORE nor AFTER phrase is
            /// specified, the substitution field replaces each contiguous nonoverlapping
            /// occurrence of the subject field in the inspected item (identifier-1), provided
            /// that the leftmost such occurrence is at the point where comparison began
            /// in the first comparison cycle for which this substitution field is eligible to
            /// participate.
            /// </summary>
            public bool Leading = false;
            /// <summary>
            /// p351:
            /// When FIRST is specified and neither the BEFORE nor AFTER phrase is
            /// specified, the substitution field replaces the leftmost occurrence of the
            /// subject field in the inspected item (identifier-1).
            /// </summary>
            public bool First = false;

            public IList<Subject> Subjects = new List<Subject>();
        }

        /// <summary>
        /// p350:
        /// CHARACTERS
        /// When CHARACTERS is specified and neither the BEFORE nor AFTER
        /// phrase is specified, the count field (identifier-2) is increased by 1 for each
        /// character (including the space character) in the inspected item (identifier-1).
        /// Thus, execution of an INSPECT statement with the TALLYING phrase
        /// increases the value in the count field by the number of character positions
        /// in the inspected item.
        ///
        /// pp350-351:
        /// CHARACTERS BY
        /// When the CHARACTERS BY phrase is used, the substitution field must be
        /// one character position in length.
        ///
        /// When CHARACTERS BY is specified and neither the BEFORE nor AFTER
        /// phrase is specified, the substitution field replaces each character in the
        /// inspected item (identifier-1), beginning at the leftmost character position
        /// and continuing to the rightmost.
        /// </summary>
        public class Subject
        {
            /// <summary>
            /// p350:
            /// identifier-3 or literal-1
            /// Is the tallying field (the item whose occurrences will be tallied).
            /// Is the subject field, which identifies the characters to be replaced.
            /// </summary>
            public Expression Item;

            /// <summary>
            /// p350:
            /// identifier-5 or literal-3
            /// Is the substitution field (the item that replaces the subject field).
            /// The subject field and the substitution field must be the same length.
            /// </summary>
            public Expression SubstitutionField;

            /// <summary>
            /// p351:
            /// BEFORE and AFTER phrases (all formats)
            /// This phrase narrows the set of items being tallied or replaced.
            ///
            /// No more than one BEFORE phrase and one AFTER phrase can be specified for any
            /// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
            /// </summary>
            public IList<Delimiter> Delimiters;
        }
    }

    /// <summary>
    /// p352:
    /// CONVERTING phrase (format 4)
    /// This phrase converts all occurrences of a specific character or string of characters in
    /// a data item (identifier-1) to user-supplied replacement characters.
    ///
    /// A format-4 INSPECT statement is interpreted and executed as if a format-2
    /// INSPECT statement had been written with a series of ALL phrases (one for each
    /// character of literal-4), specifying the same identifier-1. The effect is as if each single
    /// character of literal-4 were referenced as literal-1, and the corresponding single
    /// character of literal-5 referenced as literal-3. Correspondence between the characters
    /// of literal-4 and the characters of literal-5 is by ordinal position within the data item.
    ///
    /// If identifier-4, identifier-6, or identifier-7 occupies the same storage area as identifier-1,
    /// the result of the execution of this statement is undefined, even if they are defined
    /// by the same data description entry.
    /// </summary>
    public class InspectConvertingStatement : CodeElement
    {
        /// <summary>
        /// p352:
        /// identifier-7 or literal-5
        /// Specifies the replacing character string.
        /// The replacing character string (identifier-7 or literal-5) must be the same size
        /// as the replaced character string (identifier-6 or literal-4).
        /// </summary>
        public Expression Replacing;
        /// <summary>
        /// p352:
        /// identifier-6 or literal-4
        /// Specifies the character string to be replaced.
        /// The same character must not appear more than once in either literal-4 or
        /// identifier-6.
        /// </summary>
        public Expression Replaced;

        /// <summary>
        /// p351:
        /// BEFORE and AFTER phrases (all formats)
        /// This phrase narrows the set of items being tallied or replaced.
        ///
        /// No more than one BEFORE phrase and one AFTER phrase can be specified for any
        /// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
        /// </summary>
        public IList<Delimiter> Delimiters;

        public InspectConvertingStatement() : base(CodeElementType.InspectStatement) { }
    }



    /// <summary>
    /// p351:
    /// BEFORE and AFTER phrases (all formats)
    /// This phrase narrows the set of items being tallied or replaced.
    ///
    /// No more than one BEFORE phrase and one AFTER phrase can be specified for any
    /// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
    /// </summary>
    public class Delimiter
    {
        /// <summary>
        /// p351:
        /// INITIAL
        /// The first occurrence of a specified item.
        /// </summary>
        public bool Initial = false;
        /// <summary>
        /// The BEFORE and AFTER phrases change how counting and replacing are done:
        /// p352:
        /// * When BEFORE is specified, counting or replacing of the inspected item
        /// (identifier-1) begins at the leftmost character position and continues until the first
        /// occurrence of the delimiter is encountered. If no delimiter is present in the
        /// inspected item, counting or replacing continues toward the rightmost character
        /// position.
        /// </summary>
        public bool Before = false;
        /// <summary>
        /// The BEFORE and AFTER phrases change how counting and replacing are done:
        /// p352:
        /// * When AFTER is specified, counting or replacing of the inspected item
        /// (identifier-1) begins with the first character position to the right of the delimiter
        /// and continues toward the rightmost character position in the inspected item. If
        /// no delimiter is present in the inspected item, no counting or replacement takes
        /// place.
        /// </summary>
        public bool After = false;
        /// <summary>
        /// identifier-4 or literal-2
        /// Is the delimiter.
        /// Delimiters are not counted or replaced.
        /// </summary>
        public Expression Item;
    }
}
