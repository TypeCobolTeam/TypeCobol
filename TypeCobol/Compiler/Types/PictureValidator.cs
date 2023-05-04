using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The goal of this class is to implement a PICTURE string format validator.
    /// Using precedence rules as described in the IBM Cobol Reference.
    /// 
    /// My strategy is to implement a NFA (A non-deterministic Finite Automata) over precedence
    /// character rules. Determinism is obtained, by taking in account insertion floating position and whether or not
    /// the position is before or after a decimal floating point, cause for these cases specific states handle
    /// these situations.
    /// </summary>
    public partial class PictureValidator
    {
        private const string EMPTY = "Empty PICTURE string";
        private const string UNKNOWN_SYMBOL = "Invalid PICTURE string '{0}': character '{1}' at position '{2}' was not expected";
        private const string PARENTHESES_DO_NOT_MATCH = "Missing '(' or ')' in PICTURE string";
        private const string SYMBOL_COUNT_CANNOT_BE_ZERO = "Symbol count cannot be zero";
        private const string MULTIPLE_CURRENCIES_IN_SAME_PICTURE = "Cannot mix currency symbols in a PICTURE string: '{0}' symbol was not expected";
        private const string INVALID_PICTURE_STRING = "Combination of symbols '{0}' was not recognized as a valid PICTURE string";
        private const string INVALID_SYMBOL_POSITION = "Invalid position in PICTURE string of the symbol: {0}";
        private const string SYMBOL_S_MUST_BE_THE_FIRST = "S must be at the beginning of a PICTURE string";
        private const string MULTIPLE_V = "V must appears only once in a PICTURE string";
        private const string WRONG_P_POSITION = "P must appears at the head or tail position of a PICTURE string";
        private const string Z_STAR_MUTUALLY_EXCLUSIVE = "Z and * symbols are mutually exclusive in a PICTURE string";
        private const string SYMBOL_CAN_APPEAR_ONLY_ONCE = "Only one occurrence of '{0}' symbol can appear in a PICTURE string";
        private const string AT_LEAST_ONE_OR_TWO_OF_SYMBOLS_MUST_BE_PRESENT = "At least one of symbols A, G, N, X, Z, 9, or *, or at least two of symbols +, -, or CS must be present";

        /// <summary>
        /// Picture string constructor.
        /// </summary>
        /// <param name="picture">The picture string to validate, this string does not contains the PICTURE|PIC keyword.
        /// And it does not contains any spaces
        /// </param>
        /// <param name="separateSign">a boolean value indicating whether the sign is separate character</param>
        /// <param name="decimalPointIsComma">a boolean to swap NumericSeparator and DecimalPoint characters, default is false</param>
        /// <param name="currencyDescriptors">All custom currency descriptors indexed by their symbol. If null, the default currency descriptor is used.</param>
        public PictureValidator(string picture, bool separateSign = false, bool decimalPointIsComma = false, [CanBeNull] IDictionary<char, CurrencyDescriptor> currencyDescriptors = null)
        {
            System.Diagnostics.Debug.Assert(picture != null);
            System.Diagnostics.Debug.Assert(!picture.ToUpper().Contains("PIC"));
            System.Diagnostics.Debug.Assert(!picture.Contains(" "));

            if (currencyDescriptors == null || currencyDescriptors.Count == 0)
            {
                //Use default currency descriptor ($)
                var defaultCurrencyDescriptor = CurrencyDescriptor.Default;
                _potentialCurrencyDescriptors = new Dictionary<char, CurrencyDescriptor>()
                                                {
                                                    { defaultCurrencyDescriptor.Symbol, defaultCurrencyDescriptor }
                                                };
            }
            else
            {
                _potentialCurrencyDescriptors = currencyDescriptors;
            }
            _currencyDescriptor = null;

            OriginalPicture = picture;
            _decimalPointIsComma = decimalPointIsComma;
            if (_decimalPointIsComma)
            {
                //Swap '.' and ',' in original picture
                var pictureBuilder = new StringBuilder();
                foreach (char c in picture)
                {
                    switch (c)
                    {
                        case '.':
                            pictureBuilder.Append(',');
                            break;
                        case ',':
                            pictureBuilder.Append('.');
                            break;
                        default:
                            pictureBuilder.Append(c);
                            break;
                    }
                }

                Picture = pictureBuilder.ToString();
            }
            else
            {
                Picture = picture;
            }

            IsSeparateSign = separateSign;
        }

        /// <summary>
        /// The unaltered Picture string
        /// </summary>
        public string OriginalPicture { get; }

        /// <summary>
        /// The Picture string to validate, with comma and dot swapped if DECIMAL POINT IS COMMA is active.
        /// </summary>
        public string Picture { get; }

        /// <summary>
        /// Indicating whether the sign is separate character
        /// </summary>
        public bool IsSeparateSign { get; }

        private readonly IDictionary<char, CurrencyDescriptor> _potentialCurrencyDescriptors;
        /// <summary>
        /// The Currency Symbol found in the picture.
        /// </summary>
        [CanBeNull]
        private CurrencyDescriptor _currencyDescriptor;

        /// <summary>
        /// Comma and Dot swapped ?
        /// </summary>
        private readonly bool _decimalPointIsComma;

        /// <summary>
        /// Determines whether the picture string of this instance is valid or not.
        /// </summary>
        /// <param name="validationMessages">List of encountered validation errors.</param>
        /// <returns>A validation result instance.</returns>
        public Result Validate(out List<string> validationMessages)
        {
            //Accumulate validation error messages
            validationMessages = new List<string>();

            //1. First Validate against the PICTURE string regular expression.
            List<Tuple<string, int>> matches = PictureStringSplitter(validationMessages);
            if (matches == null || matches.Count == 0) return new Result();

            //Build Character sequence
            Character[] sequence = CollectPictureSequence(matches, validationMessages, out var symbolCounts);
            if (validationMessages.Count > 0) return new Result(sequence, _currencyDescriptor);

            //Determine data category
            var category = DeterminePictureCategory(sequence, symbolCounts);
            if (category == PictureCategory.Invalid)
            {
                validationMessages.Add(string.Format(INVALID_PICTURE_STRING, OriginalPicture));
            }
            else
            {
                //Validate the sequence using automata
                Automata automata = new Automata(this);
                if (automata.Run(sequence, category, validationMessages))
                {
                    //OK
                    return new Result(sequence, _currencyDescriptor, category, automata.Digits, automata.RealDigits, automata.IsSigned, automata.Scale, automata.Size);
                }
            }

            //KO
            return new Result(sequence, _currencyDescriptor);
        }

        /// <summary>
        /// Check the count in a picture string part ([0-9]+)
        /// </summary>
        /// <param name="pic">The picture string</param>
        /// <param name="startOffset">start offset inside the picture string where should begin the count</param>
        /// <param name="validationMessages">List of error messages</param>
        /// <param name="count">[out] the count calculated if no error, -1 otherwise.</param>
        /// <returns>The new starting offset after the count part.</returns>
        private static int CheckItemCount(string pic, int startOffset, List<string> validationMessages, out int count)
        {
            if (startOffset < pic.Length && pic[startOffset] == '(')
            {
                count = 0;
                bool bGood = false;
                startOffset++;
                while (startOffset < pic.Length)
                {
                    if (pic[startOffset] == ')')
                    {
                        bGood = true;
                        startOffset++;
                        break;
                    }
                    if (pic[startOffset] < '0' || pic[startOffset] > '9')
                    {
                        break;
                    }
                    int n = pic[startOffset] - '0';
                    count = count * 10 + n;
                    startOffset++;
                }
                if (!bGood)
                {
                    //Error on parenthesis
                    validationMessages.Add(PARENTHESES_DO_NOT_MATCH);
                    count = -1;
                }
            }
            else
            {
                count = 1;
            }
            return startOffset;
        }

        //static part of the splitter alphabet
        private static readonly string[] _Alphabet =
        {
            "A", "a", "B", "b", "E", "e", "G", "g", "N", "n", "P", "p", "S", "s", "V", "v", "X", "x", "Z", "z",
            "9", "0", "/", ",", ".", "+", "-", "CR", "cR", "Cr", "cr", "DB", "dB", "Db", "db", "*"
        };

        /// <summary>
        /// Split the Picture String into its parts. A List of tuple(Symbol, count)
        /// Example: 9(2)X(4)9
        /// {("9",2),("X",4),("9",1)}
        /// </summary>
        /// <returns>The list of parts if this is a well formed picture string, null otherwise.</returns>
        private List<Tuple<string, int>> PictureStringSplitter(List<string> validationMessages)
        {
            if (string.IsNullOrWhiteSpace(Picture))
            {
                validationMessages.Add(EMPTY);
                return null;
            }

            List<Tuple<string, int>> items = new List<Tuple<string, int>>();
            //The whole alphabet is made of static picture string symbols and custom (or only default) currency symbol(s).
            string[] alphabet = _Alphabet
                .Concat(_potentialCurrencyDescriptors.Keys.Select(currencySymbol => currencySymbol.ToString()))
                .ToArray();

            for (int l = 0; l < Picture.Length;)
            {
                bool match = false;
                for (int i = 0; i < alphabet.Length && !match; i++)
                {
                    switch (alphabet[i].Length)
                    {
                        case 1:
                            match = Picture[l] == alphabet[i][0];
                            break;
                        case 2:
                            if ((l + 1) < Picture.Length)
                            {
                                match = Picture[l] == alphabet[i][0] && Picture[l + 1] == alphabet[i][1];
                            }
                            break;
                        default:
                            if ((l + alphabet[i].Length - 1) < Picture.Length)
                            {
                                match = true;
                                for (int j = 0, n = l; j < alphabet[i].Length && n < Picture.Length; j++, n++)
                                {
                                    if (Picture[n] != alphabet[i][j])
                                    {
                                        match = false;
                                        break;
                                    }
                                }
                            }
                            break;
                    }

                    if (match)
                    {
                        l = CheckItemCount(Picture, l + alphabet[i].Length, validationMessages, out int count);
                        if (count == -1)
                            return null;
                        items.Add(new Tuple<string, int>(alphabet[i], count));
                    }
                }

                if (!match)
                {
                    validationMessages.Add(string.Format(UNKNOWN_SYMBOL, OriginalPicture, Picture[l], l + 1));
                    return null;
                }
            }
            return items;
        }

        /// <summary>
        /// Collect the Picture sequence of characters from the list of parts. And perform some pre-validation
        /// checks.
        /// </summary>
        /// <param name="matches">All Picture items matched</param>
        /// <param name="validationMessages">List of error messages</param>
        /// <param name="symbolCounts">[out] Symbol count dictionary</param>
        /// <returns>The list of picture item sequence</returns>
        private Character[] CollectPictureSequence(List<Tuple<string, int>> matches, List<string> validationMessages, out IDictionary<SC, int> symbolCounts)
        {
            symbolCounts = Enum.GetValues(typeof(SC)).Cast<SC>().ToDictionary(sc => sc, sc => 0);
            List<Character> sequence = new List<Character>();
            Character prevChar = null; //Previous char so that we can accumulate consecutive same characters.
            foreach (var m in matches)
            {
                string ch = m.Item1;
                int count = m.Item2;
                if (count == 0)
                {
                    //Count cannot be 0.
                    validationMessages.Add(SYMBOL_COUNT_CANNOT_BE_ZERO);
                }

                SC sc;
                if (ch.Length == 1)
                {
                    char c = ch[0];
                    if (_potentialCurrencyDescriptors.TryGetValue(c, out var currencyDescriptor))
                    {
                        if (_currencyDescriptor == null)
                        {
                            //This is the first currency symbol encountered
                            _currencyDescriptor = currencyDescriptor;
                            sc = SC.CS;
                        }
                        else if (_currencyDescriptor == currencyDescriptor)
                        {
                            //New occurrence of the previously found currency symbol
                            sc = SC.CS;
                        }
                        else
                        {
                            //Error, cannot mix different currency symbols
                            validationMessages.Add(string.Format(MULTIPLE_CURRENCIES_IN_SAME_PICTURE, c));
                            continue;
                        }
                    }
                    else
                    {
                        sc = Char2SC(c);
                    }
                }
                else if (ch.Equals("CR", StringComparison.OrdinalIgnoreCase))
                    sc = SC.CR;
                else if (ch.Equals("DB", StringComparison.OrdinalIgnoreCase))
                    sc = SC.DB;
                else
                    throw new InvalidOperationException();//Should never arrive

                //Accumulate symbol with previous or create new Character
                symbolCounts[sc] += count;
                if (prevChar != null && prevChar.SpecialChar == sc)
                {
                    prevChar.Count += count;
                }
                else
                {
                    prevChar = new Character(sc, count);
                    sequence.Add(prevChar);
                }
            }

            //Validate symbol counts
            SC[] onlyOnce = { SC.E, SC.S, SC.DOT, SC.CR, SC.DB};
            foreach (var sc in onlyOnce)
            {
                if (symbolCounts[sc] > 1)
                {
                    validationMessages.Add(string.Format(SYMBOL_CAN_APPEAR_ONLY_ONCE, SC2String(sc)));
                }
            }
            bool atLeastOneAGNXZNineStar = symbolCounts[SC.A] + symbolCounts[SC.G] + symbolCounts[SC.N] +
                                           symbolCounts[SC.X] + symbolCounts[SC.Z] + symbolCounts[SC.NINE] +
                                           symbolCounts[SC.STAR] >= 1;
            bool atLeastTwoPlusMinusCs = symbolCounts[SC.PLUS] >= 2 || symbolCounts[SC.MINUS] >= 2 || symbolCounts[SC.CS] >= 2;
            if (!(atLeastOneAGNXZNineStar || atLeastTwoPlusMinusCs))
            {
                validationMessages.Add(AT_LEAST_ONE_OR_TWO_OF_SYMBOLS_MUST_BE_PRESENT);
            }

            return sequence.ToArray();
        }

        /// <summary>
        /// Check symbol counts to determine the data category of the picture.
        /// </summary>
        /// <param name="sequence">Sequence of symbols</param>
        /// <param name="symbolCounts">Dictionary of all symbol counts</param>
        /// <returns>PictureCategory of the sequence, maybe Invalid</returns>
        private PictureCategory DeterminePictureCategory(Character[] sequence, IDictionary<SC, int> symbolCounts)
        {
            int totalCount = symbolCounts.Values.Sum();
            if (IsNumericEdited())         return PictureCategory.NumericEdited;
            if (IsNumeric())               return PictureCategory.Numeric;
            if (IsAlphanumericEdited())    return PictureCategory.AlphanumericEdited;
            if (IsAlphanumeric())          return PictureCategory.Alphanumeric;
            if (IsExternalFloatingPoint()) return PictureCategory.ExternalFloatingPoint;
            if (IsDBCS())                  return PictureCategory.DBCS;
            if (IsNationalEdited())        return PictureCategory.NationalEdited;
            if (IsNational())              return PictureCategory.National;
            if (IsAlphabetic())            return PictureCategory.Alphabetic;
            return PictureCategory.Invalid;

            bool IsAlphabetic()
            {
                //Only A
                return Count(SC.A) == totalCount;
            }

            bool IsAlphanumeric()
            {
                //Only A X 9 but not allowed to be made only of A or 9
                int x = Count(SC.X);
                if (x == totalCount) return true;
                int a = Count(SC.A);
                int nine = Count(SC.NINE);
                return a + x + nine == totalCount && a != totalCount && nine != totalCount;
            }

            bool IsAlphanumericEdited()
            {
                //Only A X 9 B 0 /
                bool onlyAXNineBZeroSlash = Count(SC.A, SC.X, SC.NINE, SC.B, SC.ZERO, SC.SLASH) == totalCount;
                if (!onlyAXNineBZeroSlash) return false;

                //At least one A or one X
                bool atLeastOneAX = Count(SC.A, SC.X) >= 1;
                if (!atLeastOneAX) return false;

                //At least one B or 0 or /
                bool atLeastOneBZeroSlash = Count(SC.B, SC.ZERO, SC.SLASH) >= 1;
                return atLeastOneBZeroSlash;
            }

            bool IsDBCS()
            {
                //Only G B
                bool onlyGB = Count(SC.G, SC.B) == totalCount;
                if (!onlyGB) return false;

                //At least one G
                bool atLeastOneG = Count(SC.G) >= 1;
                return atLeastOneG;
            }

            bool IsNational()
            {
                //Only N
                return Count(SC.N) == totalCount;
            }

            bool IsNationalEdited()
            {
                //Only N B 0 /
                bool onlyNBZeroSlash = Count(SC.N, SC.B, SC.ZERO, SC.SLASH) == totalCount;
                if (!onlyNBZeroSlash) return false;

                //At least one N
                bool atLeastOneN = Count(SC.N) >= 1;
                if (!atLeastOneN) return false;

                //At least one B or 0 or /
                bool atLeastOneBZeroSlash = Count(SC.B, SC.ZERO, SC.SLASH) >= 1;
                return atLeastOneBZeroSlash;
            }

            bool IsExternalFloatingPoint()
            {
                //Format is: (+|-) (9|.|V) E (+|-) 99
                int i = 0;
                int length = sequence.Length;

                //First symbol is + or -
                if (!OnePlusOrMinus()) return false;
                i++;

                //Consume mantissa
                bool seenDecimalPoint = false;
                while (i < length && sequence[i].SpecialChar != SC.E)
                {
                    switch (sequence[i].SpecialChar)
                    {
                        case SC.DOT:
                        case SC.V:
                            if (seenDecimalPoint) return false;
                            seenDecimalPoint = true;
                            goto case SC.NINE;
                        case SC.NINE:
                            i++;
                            break;
                        default:
                            return false;
                    }
                }

                //Found a decimal point ?
                if (!seenDecimalPoint) return false;

                //Reached the E ?
                if (i == length) return false;
                i++;

                //Next is + or -
                if (!OnePlusOrMinus()) return false;
                i++;

                //Last symbol is exponent, always represented as two 9
                return i == length - 1 && sequence[i].SpecialChar == SC.NINE && sequence[i].Count == 2;

                bool OnePlusOrMinus() => sequence[i].Count == 1 && (sequence[i].SpecialChar == SC.PLUS || sequence[i].SpecialChar == SC.MINUS);
            }

            bool IsNumeric()
            {
                //Only 9 P S V
                bool onlyNinePSV = Count(SC.NINE, SC.P, SC.S, SC.V) == totalCount;
                if (!onlyNinePSV) return false;

                //S must be first -> validated by automata

                //Only one V
                bool onlyOneV = Count(SC.V) <= 1;
                return onlyOneV;
            }

            bool IsNumericEdited()
            {
                //Only B P V Z 9 0 / , . + - CR DB * cs
                bool onlyBPVZNineZeroSlashCommaDotPlusMinusCRDBStarCS = Count(SC.E, SC.A, SC.X, SC.S, SC.G, SC.N) == 0;
                if (!onlyBPVZNineZeroSlashCommaDotPlusMinusCRDBStarCS) return false;

                //Truly edited otherwise it is Numeric
                bool edited = Count(SC.NINE, SC.P, SC.V) != totalCount;
                if (!edited) return false;

                //+ - CR DB mutually exclusive
                int plus = Count(SC.PLUS);
                int minus = Count(SC.MINUS);
                int cr = Count(SC.CR);
                int db = Count(SC.DB);
                int plusMinusCrDb = plus + minus + cr + db;
                bool plusMinusCrDbExclusive = plus == plusMinusCrDb || minus == plusMinusCrDb || cr == plusMinusCrDb || db == plusMinusCrDb;
                return plusMinusCrDbExclusive;
            }

            int Count(params SC[] symbols) => symbols.Sum(symbol => symbolCounts[symbol]);
        }
    }
}
