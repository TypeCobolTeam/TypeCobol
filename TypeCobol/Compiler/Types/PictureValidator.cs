using System;
using System.Collections.Generic;

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
    public class PictureValidator
    {
        /// <summary>
        /// Picture string constructor.
        /// </summary>
        /// <param name="picture">The picture string to validate, this string does not contains the PICTURE|PIC keyword.
        /// And it does not contains any spaces
        /// </param>
        /// <param name="separateSign">a boolean value indicating whether the sign is separate character</param>
        /// <param name="currencySymbol">a string to identify the Currency symbol in a picture character string, default is '$'</param>
        /// <param name="decimalPointIsComma">a boolean to swap NumericSeparator and DecimalPoint characters, default is false</param>
        public PictureValidator(string picture, bool separateSign = false, string currencySymbol = "$", bool decimalPointIsComma = false)
        {
            System.Diagnostics.Debug.Assert(picture != null);
            System.Diagnostics.Debug.Assert(!picture.ToUpper().Contains("PIC"));
            System.Diagnostics.Debug.Assert(!picture.ToUpper().Contains(" "));

            Picture = picture.ToUpper();
            IsSeparateSign = separateSign;
            CurrencySymbol = currencySymbol;
            if (decimalPointIsComma)
            {
                DecimalPoint = ',';
                NumericSeparator = '.';
            }
            else
            {
                DecimalPoint = '.';
                NumericSeparator = ',';
            }
            ValidationMessages = new List<string>();
        }

        /// <summary>
        /// The Picture string.
        /// </summary>
        public string Picture { get; }

        /// <summary>
        /// Indicating whether the sign is separate character
        /// </summary>
        public bool IsSeparateSign { get; }

        /// <summary>
        /// The Currency Symbol to be used.
        /// </summary>
        public string CurrencySymbol { get; }

        /// <summary>
        /// The decimal point character
        /// </summary>
        public char DecimalPoint { get; }

        /// <summary>
        /// The Numeric Separator
        /// </summary>
        public char NumericSeparator { get; }

        /// <summary>
        /// All validation messages if any.
        /// </summary>
        public List<string> ValidationMessages { get; }

        /// <summary>
        /// Check the count in a picture string part ([0-9]+)
        /// </summary>
        /// <param name="pic">The picture string</param>
        /// <param name="startOffset">start offset inside the picture string where should begin the count</param>
        /// <param name="count">[out] the count calculated if no error, -1 otherwise.</param>
        /// <returns>The new starting offset after the count part.</returns>
        private static int CheckItemCount(string pic, int startOffset, out int count)
        {
            count = -1;
            if (startOffset >= pic.Length)
            {
                count = 1;
            }
            else if (pic[startOffset] == '(')
            {
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
                    {//Error
                        count = -1;
                        break;
                    }
                    int n = pic[startOffset] - '0';
                    count = count == -1 ? n : count * 10 + n;
                    startOffset++;
                }
                if (!bGood)
                {
                    count = -1;
                }
            }
            else
            {
                count = 1;
            }
            return startOffset;
        }

        /// <summary>
        /// Split the Picture String into its parts. A List of tuple(Symbol, count)
        /// Example: 9(2)X(4)9
        /// {("9",2),("X",4),("9",1)}
        /// </summary>
        /// <returns>The list of parts if this is a well formed picture string, null otherwise.</returns>
        public static List<Tuple<string, int>> PictureStringSplitter(string picture, string currencySymbol)
        {
            List<Tuple<string, int>> items = new List<Tuple<string, int>>();
            string[] alphabet = { "A", "B", "E", "G", "N", "P", "S", "V", "X", "Z", "9", "0", "/", ",", ".", "+", "-", "CR", "DB", "*", currencySymbol };
            for (int l = 0; l < picture.Length;)
            {
                bool match = false;
                for (int i = 0; i < alphabet.Length && !match; i++)
                {
                    switch (alphabet[i].Length)
                    {
                        case 1:
                            match = picture[l] == alphabet[i][0];
                            break;
                        case 2:
                            if ((l + 1) < picture.Length)
                            {
                                match = picture[l] == alphabet[i][0] && picture[l + 1] == alphabet[i][1];
                            }
                            break;
                        default:
                            if ((l + alphabet[i].Length - 1) < picture.Length)
                            {
                                match = true;
                                for (int j = 0, n = l; j < alphabet[i].Length && n < picture.Length; j++, n++)
                                {
                                    if (picture[n] != alphabet[i][j])
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
                        l = CheckItemCount(picture, l + alphabet[i].Length, out int count);
                        if (count == -1)
                            return null;
                        items.Add(new Tuple<string, int>(alphabet[i], count));
                    }
                }
                if (!match)
                    return null;
            }
            return items;
        }

        /// <summary>
        /// Special chracters.
        /// </summary>
        internal enum SC
        {
            B,
            ZERO, // 0
            SLASH, // /
            COMMA, // ,
            DOT, // .
            PLUS, // +
            MINUS, // -
            CR,
            DB,
            CS,
            E,
            Z,
            STAR, // *
            NINE, // 9                        
            A,
            X,
            S,
            V,
            P,
            G,
            N,

            //.. The count of special char
            SpecialCharCount
        }

        private static SC Char2SC(char c)
        {
            switch (c)
            {
                case 'A':
                    return SC.A;
                case 'B':
                    return SC.B;
                case 'E':
                    return SC.E;
                case 'G':
                    return SC.G;
                case 'N':
                    return SC.N;
                case 'P':
                    return SC.P;
                case 'S':
                    return SC.S;
                case 'V':
                    return SC.V;
                case 'X':
                    return SC.X;
                case 'Z':
                    return SC.Z;
                case '9':
                    return SC.NINE;
                case '0':
                    return SC.ZERO;
                case '/':
                    return SC.SLASH;
                case ',':
                    return SC.COMMA;
                case '.':
                    return SC.DOT;
                case '+':
                    return SC.PLUS;
                case '-':
                    return SC.MINUS;
                case '*':
                    return SC.STAR;
                case '$':
                    return SC.CS;
                default:
                    throw new ArgumentException();
            }
        }

        private string SC2String(SC c)
        {
            switch (c)
            {
                case SC.A:
                    return "A";
                case SC.B:
                    return "B";
                case SC.E:
                    return "E";
                case SC.G:
                    return "G";
                case SC.N:
                    return "N";
                case SC.P:
                    return "P";
                case SC.S:
                    return "S";
                case SC.V:
                    return "V";
                case SC.X:
                    return "X";
                case SC.Z:
                    return "Z";
                case SC.NINE:
                    return "9";
                case SC.ZERO:
                    return "0";
                case SC.SLASH:
                    return "/";
                case SC.COMMA:
                    return ",";
                case SC.DOT:
                    return ".";
                case SC.PLUS:
                    return "+";
                case SC.MINUS:
                    return "-";
                case SC.STAR:
                    return "*";
                case SC.CS:
                    return CurrencySymbol;
                case SC.CR:
                    return "CR";
                case SC.DB:
                    return "DB";
                default:
                    throw new ArgumentException($"Unknown '{c}' special character.", nameof(c));
            }
        }

        /// <summary>
        /// Determine if the given character is a simple insertion character.
        /// </summary>
        /// <param name="c">Return true if yes, false otherwise</param>
        /// <returns>true if yes, false otherwise</returns>
        private bool IsSimpleInsertionCharacter(SC c)
        {
            return c == SC.B || c == SC.ZERO || c == SC.SLASH || c == Char2SC(NumericSeparator);
        }

        /// <summary>
        /// Collect the Picture sequence of characters from the list of parts. And perform some pre-validation
        /// checks.
        /// </summary>
        /// <param name="matches">All Picture items matched</param>
        /// <returns>The list of picture item sequence</returns>
        private List<Character> CollectPictureSequence(List<Tuple<string, int>> matches)
        {
            List<Character> sequence = new List<Character>();
            Character prevChar = null;//Previous char so that we can accumulate consecutive same characters.
            int cr_count = 0;
            int db_count = 0;
            int e_count = 0;
            int s_count = 0;
            int v_count = 0;
            int dot_count = 0;

            bool foundCR = false;
            bool foundDB = false;
            bool foundPlus = false;
            bool foundMinus = false;

            foreach (var m in matches)
            {
                string ch = m.Item1;
                int count = m.Item2;
                if (count == 0)
                {//Count cannot be 0.
                    ValidationMessages.Add(Context.SymbolCountCannotBeZeroMsg);
                }

                SC sc;
                if (ch.Length == 1)
                {
                    try
                    {
                        sc = Char2SC(ch[0]);
                    }
                    catch (ArgumentException ae)
                    {
                        if (ch.Equals(CurrencySymbol))
                        {
                            sc = SC.CS;
                        }
                        else
                        {
                            throw ae;
                        }
                    }
                }
                else if (ch == CurrencySymbol)
                    sc = SC.CS;
                else if (ch == "CR")
                    sc = SC.CR;
                else if (ch == "DB")
                    sc = SC.DB;
                else
                    throw new InvalidOperationException();//Should never arrive
                if (prevChar != null && prevChar.ch == sc)
                {
                    prevChar.count += count;
                }
                else
                {
                    prevChar = new Character(sc, count);
                    sequence.Add(prevChar);
                }
                //Validate those symbols that can appear only once in a PICTURE string.
                switch (sc)
                {
                    case SC.PLUS:
                        foundPlus = true;
                        break;
                    case SC.MINUS:
                        foundMinus = true;
                        break;
                    case SC.CR:
                        foundCR = true;
                        cr_count += count;
                        if (cr_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_CR_CharacterMsg);
                        }
                        break;
                    case SC.DB:
                        foundDB = true;
                        db_count += count;
                        if (db_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_DB_CharacterMsg);
                        }
                        break;
                    case SC.S:
                        s_count += count;
                        if (s_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_S_CharacterMsg);
                        }
                        break;
                    case SC.V:
                        v_count += count;
                        if (v_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_V_CharacterMsg);
                        }
                        break;
                    case SC.E:
                        e_count += count;
                        if (e_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_E_CharacterMsg);
                        }
                        break;
                    case SC.DOT:
                        dot_count += count;
                        if (dot_count > 1)
                        {
                            ValidationMessages.Add(Context.MoreThanOne_Dot_CharacterMsg);
                        }
                        break;
                }
            }
            int cntFound = (foundCR || foundDB ? 1 : 0);
            cntFound += (foundPlus || foundMinus ? 1 : 0);
            if (cntFound > 1)
            { // 0 is valid
                ValidationMessages.Add(Context.MutuallyExclusiveSymbolMsg);
            }
            return sequence;
        }

        /// <summary>
        /// The validation context that has been used.
        /// </summary>
        public Context ValidationContext
        {
            get;
            private set;
        }

        /// <summary>
        /// Determines whether the picture string of this instance is valid or not.
        /// </summary>
        /// <returns>true if the pic is valid, false otherwise</returns>
        public bool IsValid()
        {
            //0. Picture String must contains at least
            //1. First Validate against the PICTURE string regular expression.
            List<Tuple<string, int>> matches = PictureStringSplitter(Picture, CurrencySymbol);
            if (matches == null)
                return false;
            if (matches.Count <= 0)
                return false;
            //Construct Character sequence
            ValidationMessages.Clear();
            List<Character> sequence = CollectPictureSequence(matches);
            if (ValidationMessages.Count > 0)
                return false;
            //No Validate the sequence
            return ValidatePictureSequence(sequence);
        }

        /// <summary>
        /// Validate a picture sequence
        /// </summary>
        /// <param name="sequence">The sequence to validate</param>
        /// <returns>true if the sequence represents a valid Picture, false otherwise.</returns>
        private bool ValidatePictureSequence(List<Character> sequence)
        {
            Context ctx = ComputeInitialContext(sequence);
            ValidationContext = ctx;
            return this.RunAutomata(ctx);
        }

        /// <summary>
        /// Compute indexes of the Floating Insertion String. That is to the left most and the right most of the characters
        /// CS, + or -.
        /// </summary>
        /// <param name="sequence">The on which to perform the computation</param>
        /// <param name="firstIndex">[out] First Floating Index</param>
        /// <param name="lastIndex">[out] Last Floating Index</param>
        private void ComputeFloatingStringIndexes(List<Character> sequence, out int firstIndex, out int lastIndex)
        {
            firstIndex = lastIndex = -1;
            int lastNonSimpleIndex = -1;
            SC floatChar = (SC)(-1); //The float character that corresponds to the first index either CS, + or -.
            int i;
            for (i = 0; i < sequence.Count; i++)
            {
                Character c = sequence[i];
                if (firstIndex == -1 && (c.ch == SC.PLUS || c.ch == SC.MINUS || c.ch == SC.CS))
                {
                    if (lastNonSimpleIndex >= 0 && sequence[lastNonSimpleIndex].ch == c.ch)
                    {
                        firstIndex = lastNonSimpleIndex;
                        floatChar = c.ch;
                        continue;
                    }
                    else if (c.count > 1)
                    {
                        firstIndex = i;
                        floatChar = c.ch;
                        continue;
                    }
                }
                if (firstIndex == -1 && !IsSimpleInsertionCharacter(c.ch))
                {
                    lastNonSimpleIndex = i;
                }
                else if (firstIndex >= 0 && !(IsSimpleInsertionCharacter(c.ch) || c.ch == floatChar))
                {
                    lastIndex = i - 1;
                    break;
                }
            }

            if (i >= sequence.Count && firstIndex >= 0)
            { //We have reach the end of the sequence with a first index and no lastIndex ==>
              //Set the last index to the last character of the sequence
                lastIndex = sequence.Count - 1;
                return;
            }
            else if (!(i < sequence.Count && (SC2String(sequence[i].ch)[0] == DecimalPoint || sequence[i].ch == SC.V)))
            {//The last index does not precede the DecimalPoint separator position
                return;
            }
            //If the last index precede the DecimalPoint position so all characters including the decimal point
            //that are not simple characters or the floating character must be part o the right most index

            for (++i; i < sequence.Count; i++)
            {
                Character c = sequence[i];
                if (!(IsSimpleInsertionCharacter(c.ch) || c.ch == floatChar))
                    return;
            }
            lastIndex = i - 1;
        }

        /// <summary>
        /// Compute the initial validation context.
        /// </summary>
        /// <param name="sequence">The sequence of characters.</param>
        /// <returns></returns>
        private Context ComputeInitialContext(List<Character> sequence)
        {
            ComputeFloatingStringIndexes(sequence, out int firstIndex, out int lastIndex);
            return new Context(sequence, this.ValidationMessages)
                   {
                       FirstFloatingIndex = firstIndex,
                       LastFloatingIndex = lastIndex,
                       CurrencySymbol = this.CurrencySymbol,
                       DecimalPoint = this.DecimalPoint,
                       NumericSeparator = this.NumericSeparator,
                       IsSeparateSign = this.IsSeparateSign,
                   };
        }

        /// <summary>
        /// The representation of a character of the DFA alphabet.
        /// </summary>
        internal class Character
        {
            /// <summary>
            /// The character
            /// </summary>
            public SC ch;
            /// <summary>
            /// The repetition count of the character.
            /// </summary>
            public int count;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="c">The character</param>
            /// <param name="n">The repetition count</param>
            public Character(SC c, int n)
            {
                ch = c;
                count = n;
            }

            public override string ToString()
            {
                string s;
                switch (ch)
                {
                    case SC.A:
                        s = "A";
                        break;
                    case SC.B:
                        s = "B"; break;
                    case SC.E:
                        s = "E"; break;
                    case SC.G:
                        s = "G"; break;
                    case SC.N:
                        s = "N"; break;
                    case SC.P:
                        s = "P"; break;
                    case SC.S:
                        s = "S"; break;
                    case SC.V:
                        s = "V"; break;
                    case SC.X:
                        s = "X"; break;
                    case SC.Z:
                        s = "Z"; break;
                    case SC.NINE:
                        s = "9"; break;
                    case SC.ZERO:
                        s = "0"; break;
                    case SC.SLASH:
                        s = "/"; break;
                    case SC.COMMA:
                        s = ","; break;
                    case SC.DOT:
                        s = "."; break;
                    case SC.PLUS:
                        s = "+"; break;
                    case SC.MINUS:
                        s = "-"; break;
                    case SC.STAR:
                        s = "*"; break;
                    case SC.CS:
                        s = "."; break;
                    case SC.CR:
                        s = "CR"; break;
                    case SC.DB:
                        s = "DB"; break;
                    default:
                        throw new ArgumentException();
                }
                return count > 0 ? (s + '(' + count + ')') : s;
            }
        }

        /// <summary>
        /// A validation context.
        /// </summary>
        public class Context
        {
            internal static readonly string SymbolCountCannotBeZeroMsg = "Symbol count cannot be zero";
            internal static readonly string InvalidSymbolPosMsg = "Invalid position in PICTURE string of the symbol : {0}";
            internal static readonly string SymbolSMustOccurOnlyOnceMsg = "Character S must be repeated only once in PICTURE string";
            internal static readonly string SymbolSMustBeTheFirstMsg = "S must be at the beginning of a PICTURE string";
            internal static readonly string MultipleVMsg = "V must appears only once in a PICTURE string";
            internal static readonly string WrongPPositionMsg = "P must appears at the head or tail position of a PICTURE string";
            internal static readonly string ZStarMutuallyExclusiveMsg = "Z and * symbols are mutually exclusive in a PICTURE string.";
            internal static readonly string MoreThanOne_E_CharacterMsg = "Only one occurrence of E symbol can appear in a PICTURE string";
            internal static readonly string MoreThanOne_CR_CharacterMsg = "Only one occurrence of CR symbol can appear in a PICTURE string";
            internal static readonly string MoreThanOne_DB_CharacterMsg = "Only one occurrence of DB symbol can appear in a PICTURE string";
            internal static readonly string MoreThanOne_S_CharacterMsg = "Only one occurrence of S symbol can appear in a PICTURE string";
            internal static readonly string MoreThanOne_V_CharacterMsg = "Only one occurrence of V symbol can appear in a PICTURE string";
            internal static readonly string MoreThanOne_Dot_CharacterMsg = "Only one occurrence of '.' symbol can appear in a PICTURE string";
            internal static readonly string MutuallyExclusiveSymbolMsg = "+/-/CR/DB are mutually exclusive";

            /// <summary>
            /// Empty constructor.
            /// </summary>
            internal Context(List<Character> sequence, List<string> validationMessages)
            {
                ValidationMessages = validationMessages;
                Sequence = sequence;
                IsBeforeDecimalPoint = true;
            }

            /// <summary>
            /// Indicating whether the sign is separate character
            /// </summary>
            internal bool IsSeparateSign
            {
                get;
                set;
            }

            /// <summary>
            /// The Currency Symbol to be used.
            /// </summary>
            internal string CurrencySymbol
            {
                get;
                set;
            }

            /// <summary>
            /// The decimal point character
            /// </summary>
            internal char DecimalPoint
            {
                get;
                set;
            }

            /// <summary>
            /// The Numeric Separator
            /// </summary>
            internal char NumericSeparator
            {
                get;
                set;
            }

            /// <summary>
            /// Current Index in the sequence
            /// </summary>
            internal int SequenceIndex
            {
                get;
                set;
            }

            /// <summary>
            /// The sequence
            /// </summary>
            internal List<Character> Sequence
            {
                get;
                set;
            }

            /// <summary>
            /// Current State
            /// </summary>
            internal int StateIndex
            {
                get;
                set;
            }

            /// <summary>
            /// The Current Picture category during validation.
            /// </summary>
            public PictureCategory Category
            {
                get;
                private set;
            }

            /// <summary>
            /// Size in byte
            /// </summary>
            public int Size
            {
                get;
                private set;
            }

            /// <summary>
            /// Number of digit places
            /// </summary>
            public int Digits
            {
                get;
                private set;
            }

            /// <summary>
            /// 1/10^Scale
            /// </summary>
            public int Scale
            {
                get;
                private set;
            }

            /// <summary>
            /// Have 'S'
            /// </summary>
            public bool HaveSign
            {
                get;
                private set;
            }

            /// <summary>
            /// Real number of digits
            /// </summary>
            public int RealDigits
            {
                get;
                private set;
            }

            /// <summary>
            /// The Index of the first character of a floating insertion string in the sequence.
            /// </summary>
            internal int FirstFloatingIndex
            {
                get;
                set;
            }

            /// <summary>
            /// The Index of the last character of a floating insertion string in the sequence.
            /// </summary>
            internal int LastFloatingIndex
            {
                get;
                set;
            }

            /// <summary>
            /// True if we are before a decimal point character, false otherwise.
            /// </summary>
            internal bool IsBeforeDecimalPoint
            {
                get;
                set;
            }

            /// <summary>
            /// true if we have encountered Z,*,+,-, CS or 9 before P
            /// </summary>
            internal bool DigitsSeenBeforeP
            {
                get;
                set;
            }

            /// <summary>
            /// Current count of alphanumeric characters.
            /// </summary>
            private int AX_digits = 0;
            /// <summary>
            /// The count of V symbol
            /// </summary>
            private int V_count = 0;
            /// <summary>
            /// The count of either CR, DB, + or - symbols
            /// </summary>
            private int S_count = 0;
            /// <summary>
            /// Have we seen Z ?
            /// </summary>
            private bool Z_seen = false;
            /// <summary>
            /// Have we seen '*' ?
            /// </summary>
            private bool Star_seen = false;

            /// <summary>
            /// Called when changing transition on the given character, so specific validation actions can be performed here.
            /// </summary>
            /// <param name="c">Character on the transition</param>
            /// <param name="state">The current state of the character</param>
            /// <param name="gotoState">The state to goto, the method cans decide go to to another state that's why this value is passed by reference.</param>
            /// <returns>return true to continue validation, false otherwise.</returns>
            internal bool OnGoto(Character c, int state, ref int gotoState)
            {
                switch (c.ch)
                {
                    case SC.B:      //'B'
                    case SC.ZERO:   //'0'
                    case SC.SLASH:  //'/'
                        this.Category |= PictureCategory.Edited;
                        break;
                    case SC.PLUS:   //'+'
                    case SC.MINUS:  //'-'
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.count - 1;
                        S_count++;
                        break;
                    case SC.Z:
                    case SC.STAR:
                        if (c.ch == SC.Z)
                            Z_seen = true;
                        else
                            Star_seen = true;
                        if (Z_seen && Star_seen)
                        {
                            ValidationMessages.Add(ZStarMutuallyExclusiveMsg);
                            return false;
                        }
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.count;
                        if (V_count > 0)
                        {
                            Scale += c.count;
                        }
                        break;
                    case SC.CR:
                    case SC.DB:
                        this.Category |= PictureCategory.NumericEdited;
                        S_count++;
                        break;
                    case SC.CS:
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.count - 1;
                        break;
                    case SC.E:
                        break;
                    case SC.A:
                        this.Category |= PictureCategory.Alphabetic;
                        AX_digits += c.count;
                        break;
                    case SC.X:
                        this.Category |= PictureCategory.AlphaNumeric;
                        AX_digits += c.count;
                        break;
                    case SC.NINE://9
                        this.Category |= PictureCategory.Numeric;
                        Digits += c.count;
                        RealDigits += c.count;
                        if (V_count > 0)
                        {//We have seen the decimal point --> digitq are un the decimal part
                            Scale += c.count;
                        }
                        break;
                    case SC.G:
                        this.Category |= PictureCategory.Dbcs;
                        AX_digits += c.count;
                        break;
                    case SC.N:
                        this.Category |= PictureCategory.National;
                        AX_digits += c.count;
                        break;
                    case SC.S:
                        this.Category |= PictureCategory.Numeric;
                        if (c.count > 1)
                        {
                            ValidationMessages.Add(SymbolSMustOccurOnlyOnceMsg);
                            return false;
                        }
                        if (state != 0 || this.SequenceIndex != 0)
                        {
                            ValidationMessages.Add(SymbolSMustBeTheFirstMsg);
                            return false;
                        }
                        S_count += c.count;
                        break;
                    case SC.DOT:
                    case SC.COMMA:
                        this.Category |= PictureCategory.NumericEdited;
                        if (c.ch != Char2SC(this.DecimalPoint))
                            break;
                        goto case SC.V;
                    case SC.V:
                        this.Category |= PictureCategory.Numeric;
                        V_count += c.count;
                        if (V_count > 1)
                        {
                            ValidationMessages.Add(MultipleVMsg);
                            return false;
                        }
                        this.IsBeforeDecimalPoint = false;
                        break;
                    case SC.P:
                        this.Category |= PictureCategory.Numeric;
                        if (!ValidateP())
                            return false;
                        Digits += c.count;
                        Scale += (V_count > 0 ? c.count : (-c.count));
                        break;
                }
                if (S_count > 0)
                    HaveSign = true;
                //Update total size
                switch (c.ch)
                {
                    case SC.S:
                        Size += this.IsSeparateSign ? 1 : 0;
                        break;
                    case SC.V:
                    case SC.P:
                        break;
                    case SC.CR:
                    case SC.CS:
                        Size += c.count * 2;
                        break;
                    case SC.N:
                    case SC.G:
                        Size += c.count * 2;
                        break;
                    default:
                        Size += c.count;
                        break;
                }
                return true;
            }

            /// <summary>
            /// Validate the presence of the P character. 
            /// The Symbol P specified a scaling position and implies an assumed decimal point (to the left of the Ps if the Ps are leftmost PICTURE characters;
            /// to right of the Ps if the Ps are rightmost PICTURE characters).
            /// If we say that the character ^ means the beginning of the PICTURE sequence
            /// and $ means the end of the PICTURE sequence sequence, only the following situations are valid for P.
            /// ^P | ^VP | ^SP | ^SVP | P$ | PV$
            /// </summary>
            /// <returns>true if OK, false otherwise.</returns>
            private bool ValidateP()
            {
                if (this.SequenceIndex == 0 || this.SequenceIndex == (this.Sequence.Count - 1))
                {
                    V_count += (this.SequenceIndex == 0) ? 1 : 0; //Assume decimal point symbol V at the beginning
                    return true;//^P | P$;
                }
                if (this.SequenceIndex == 1 && (this.Sequence[0].ch == SC.S || this.Sequence[0].ch == SC.V))
                {
                    V_count += 1;//Assume decimal point symbol V at the beginning
                    return true;//^SP | ^VP
                }
                if (this.SequenceIndex == 2 && (this.Sequence[0].ch == SC.S && this.Sequence[1].ch == SC.V))
                {
                    V_count += 1;//Assume decimal point symbol V at the beginning
                    return true;//^SVP
                }
                if (this.SequenceIndex == (this.Sequence.Count - 2) && this.Sequence[this.Sequence.Count - 1].ch == SC.V)
                    return true;//$PV
                this.ValidationMessages.Add(WrongPPositionMsg);
                return false;
            }

            /// <summary>
            /// Determines if the current Sequence Index is inside the Floating Insertion Symbols range.
            /// </summary>
            private bool IsCurrentIndexInsideFloatingInsertion
                => this.FirstFloatingIndex >= 0
                   && this.LastFloatingIndex >= 0
                   && this.FirstFloatingIndex <= this.SequenceIndex
                   && this.SequenceIndex <= this.LastFloatingIndex;

            /// <summary>
            /// Return true if the current sequence index is the first symbol of the sequence.
            /// </summary>
            private bool IsFirstSymbol => SequenceIndex == 0;

            /// <summary>
            /// Return true if the current sequence index is the last symbol of the sequence.
            /// </summary>
            private bool IsLastSymbol => SequenceIndex == Sequence.Count - 1;

            /// <summary>
            /// Get the state that is used to handle the given character in the Automata
            /// </summary>
            /// <param name="c">The character to get the handling state</param>
            /// <returns>The state number if one exist, -1 otherwise</returns>
            internal int GetState(Character c)
            {
                switch (c.ch)
                {
                    case SC.B:
                        return 1;
                    case SC.ZERO:
                    case SC.SLASH:
                        return 2;
                    case SC.COMMA:
                        return 3;
                    case SC.DOT:
                        return 4;
                    case SC.PLUS:
                    case SC.MINUS:
                        {
                            if (!IsCurrentIndexInsideFloatingInsertion)
                            {
                                if (IsFirstSymbol)
                                    return 5;
                                else if (IsLastSymbol)
                                    return 6;
                                else
                                    return 5;
                            }
                            else
                            {
                                DigitsSeenBeforeP = true;
                                return IsBeforeDecimalPoint ? 12 : 13;
                            }
                        }
                    case SC.CR:
                    case SC.DB:
                        return 7;
                    case SC.CS:
                        {
                            if (!IsCurrentIndexInsideFloatingInsertion)
                            {
                                return 8;
                            }
                            else
                            {
                                DigitsSeenBeforeP = true;
                                return IsBeforeDecimalPoint ? 14 : 15;
                            }
                        }
                    case SC.E:
                        return 9;
                    case SC.Z:
                    case SC.STAR:
                        DigitsSeenBeforeP = true;
                        return IsBeforeDecimalPoint ? 10 : 11;
                    case SC.NINE:
                        DigitsSeenBeforeP = true;
                        return 16;
                    case SC.A:
                    case SC.X:
                        return 17;
                    case SC.S:
                        return 18;
                    case SC.V:
                        return 19;
                    case SC.P:
                        return DigitsSeenBeforeP && IsBeforeDecimalPoint ? 20 : 21;
                    case SC.G:
                        return 22;
                    case SC.N:
                        return 23;
                    default:
                        return -1;//Should never arrive.
                }
            }

            /// <summary>
            /// Determines if the current sequence after a call to IsValid method, is in fact an ExternalFloat picture string
            /// category.
            /// </summary>
            /// <returns></returns>
            public bool IsExternalFloatSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count <= 4)
                    return false;// should contained with at leas (+|-)*2,(.|V),E
                if (this.Category != PictureCategory.NumericEdited)
                    return false;//By Default is a NumericEdited category.
                int i = 0;
                if (Sequence[i].ch != SC.PLUS && Sequence[i].ch != SC.MINUS)
                    return false;
                int len = Sequence.Count;
                i++;
                if (Sequence[i].ch == SC.DOT || Sequence[i].ch == SC.V)
                {
                    if (Sequence[i + 1].ch != SC.NINE)
                        return false;
                }
                else if (Sequence[i].ch == SC.NINE)
                {
                    i++;
                    if (Sequence[i].ch != SC.DOT & Sequence[i].ch != SC.V)
                        return false;
                    i++;
                }
                else
                    return false;
                if (i >= len || Sequence[i].ch == SC.NINE)
                    i++;
                if (i >= len || Sequence[i].ch != SC.E)
                    return false;
                if (++i >= len || (Sequence[i].ch != SC.PLUS && Sequence[i].ch != SC.MINUS))
                    return false;
                if (++i >= len || !(Sequence[i].ch == SC.NINE && Sequence[i].count == 2))
                    return false;
                return i == (len - 1);
            }

            /// <summary>
            /// Determine whether or not the sequence is alphabetic.
            /// </summary>
            /// <returns>true if the sequence is alphabetic, false otherwise</returns>
            public bool IsAlphabeticSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.A);
            }

            /// <summary>
            /// Determines if the current sequence is a DBCS sequence
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsDbcsSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.G || c.ch == SC.B);
            }

            /// <summary>
            /// Determines if the current sequence is a numeric sequence
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsNumericSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.NINE || c.ch == SC.S || c.ch == SC.V || c.ch == SC.P);
            }

            /// <summary>
            /// Determine whether or not the sequence is alphanumeric.
            /// </summary>
            /// <returns>true if the sequence is alphanumeric, false otherwise</returns>
            public bool IsAlphanumericSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.NINE || c.ch == SC.X || c.ch == SC.A);
            }

            /// <summary>
            /// Determine whether or not the sequence is a numeric-edited item
            /// </summary>
            /// <returns>true if the sequence is numeric-edited, false otherwise</returns>
            public bool IsNumericEditedSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c =>
                    c.ch == SC.B ||
                    c.ch == SC.P ||
                    c.ch == SC.V ||
                    c.ch == SC.Z ||
                    c.ch == SC.NINE ||
                    c.ch == SC.ZERO ||
                    c.ch == SC.SLASH ||
                    c.ch == SC.COMMA ||
                    c.ch == SC.DOT ||
                    c.ch == SC.PLUS ||
                    c.ch == SC.MINUS ||
                    c.ch == SC.STAR ||
                    c.ch == SC.CS
                );
            }

            /// <summary>
            /// Determine whether or not the sequence is a alphanumeric-edited item
            /// </summary>
            /// <returns>true if the sequence is alphanumeric-edited, false otherwise</returns>
            public bool IsAlphaNumericEditedSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c =>
                    c.ch == SC.A ||
                    c.ch == SC.X ||
                    c.ch == SC.NINE ||
                    c.ch == SC.B ||
                    c.ch == SC.ZERO ||
                    c.ch == SC.SLASH
                );
            }

            /// <summary>
            /// Is a sequence only formed with national characters that is to say only N.
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsNationalSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.N);
            }

            /// <summary>
            /// Is a sequence only formed with national edited characters, that is to say with
            /// symbols N, B, 0 or /, with at least one N and one other symbol in the picture character string.
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsNationalEditedSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                bool hasN = false;
                bool bAll = this.Sequence.TrueForAll(c => (hasN |= c.ch == SC.N) ||
                                c.ch == SC.B ||
                                c.ch == SC.ZERO ||
                                c.ch == SC.SLASH);
                return hasN && bAll;
            }

            /// <summary>
            /// All validation messages if any.
            /// </summary>
            public List<string> ValidationMessages { get; }
        }

        /// <summary>
        /// An Automate State a state is a list of Transition.
        /// </summary>
        private class State
        {
            /// <summary>
            /// The State Number
            /// </summary>
            public int Number { get; }

            /// <summary>
            /// Transitions on characters boolean vector.
            /// </summary>
            public bool[] Trans { get; }

            /// <summary>
            /// Transition table constructor
            /// </summary>
            /// <param name="number">The state number</param>
            /// <param name="vtrans">Transition table for this state</param>
            public State(int number, params SC[] vtrans)
            {
                System.Diagnostics.Debug.Assert(vtrans != null);
                bool[] atrans = new bool[(int)SC.SpecialCharCount];
                foreach (var sc in vtrans)
                {
                    System.Diagnostics.Debug.Assert(atrans[(int)sc] == false);
                    atrans[(int)sc] = true;
                }
                Number = number;
                Trans = atrans;
            }

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[Character c] => this[c.ch];

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[SC c] => Trans[(int)c];
        }

        private static SC T(char c)
        {
            return Char2SC(c);
        }

        /// <summary>
        /// Static representation of a (NFA) Non Deterministic Finite Automata over PICTURE symbols precedence rules.
        /// </summary>
        private static readonly State[] _Automata =
        {
            //State 0: Start Symbols
            new State(0,T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),T('Z'),T('*'),SC.CS,T('9'),T('A'),T('X'),T('S'),T('V'),T('P'),T('G'),T('N') ),

            //------------------------------------------------------
            // NON FLOATING INSERTION SYMBOLS
            //------------------------------------------------------
            //State 1: --B-->(1)
            new State(1, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),SC.CS,T('9'),T('A'),T('X'),T('V'),T('P'),T('G'),T('N') ),
            //State 2: --[0|/]-->(2)
            new State(2, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),SC.CS,T('9'),T('A'),T('X'),T('V'),T('P'),T('N') ),
            //State 3: --,-->(3)
            new State(3,  T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),SC.CS,T('E'),T('9'),T('V'),T('P') ),
            //State 4: --.-->(4)
            new State(4, T('B'),T('0'),T('/'),T(','),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),SC.CS,T('E'),T('9') ),
            //State 5: --[+|-]-->(5)
            new State(5, T('B'),T('0'),T('/'),T(','),T('.'),T('Z'),T('*'),SC.CS,T('9'),T('V'),T('P') ),
            //State 6: --[+|-]-->(6)
            new State(6, new SC[0]),
            //State : --[CR|DB]-->()
            new State(7, new SC[0]),
            //State : --CS-->(8)
            new State(8, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),T('9'),T('V'),T('P') ),
            //State : --E-->(9)
            new State(9, T('+'),T('-') ),

            //------------------------------------------------------
            //FLOATING INSERTION SYMBOLS
            //------------------------------------------------------
            //State 10: --[Z|*]-->(10)
            new State(10, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),T('9'),T('V'),T('P') ),
            //State 11: --[Z|*]-->(11)
            new State(11, T('B'),T('0'),T('/'),T(','),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*') ),
            //State 12: --[+|-]-->(12)
            new State(12, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),T('9'),T('V'),T('P') ),
            //State 13: --[+|-]-->(13)
            new State(13, T('B'),T('0'),T('/'),T(','),T('+'),T('-') ),
            //State 14: --CS-->(14)
            new State(14, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,SC.CS,T('9'),T('V'),T('P') ),
            //State 15: --CS-->(15)
            new State(15, T('B'),T('0'),T('/'),T(','),T('+'),T('-'),SC.CR,SC.DB,SC.CS ),

            //------------------------------------------------------
            //OTHER SYMBOLS
            //------------------------------------------------------
            //State 16: --9-->(16)
            new State(16, T('B'),T('0'),T('/'),T(','),T('.'),T('+'),T('-'),SC.CR,SC.DB,T('E'),T('9'),T('A'),T('X'),T('V'),T('P')),
            //State 17: --[A|X]-->(17)
            new State(17, T('B'),T('0'),T('/'),T('9'),T('A'),T('X')),
            //State 18: --S-->(18)
            new State(18, T('9'),T('V'),T('P')),
            //State 19: --V-->(19)
            new State(19, T('B'),T('0'),T('/'),T(','),T('+'),T('-'),SC.CR,SC.DB,T('E'),T('Z'),T('*'),SC.CS,T('9'),T('P') ),
            //State 20: --P-->(20)
            new State(20, T('+'),T('-'),SC.CR,SC.DB,T('V'),T('P')),
            //State 21: --P-->(21)
            new State(21, T('B'),T('0'),T('/'),T(','),T('+'),T('-'),SC.CR,SC.DB,T('Z'),T('*'),T('9'),T('P')),
            //State 22: --G-->(22)
            new State(22, T('B'),T('G') ),
            //State 23: --N-->(23)
            new State(23, T('B'),T('0'),T('/'),T('N') ),
        };

        /// <summary>
        /// Run the automata on the given context along with its PICTURE sequence.
        /// </summary>
        /// <param name="ctx">The context</param>
        /// <returns>true if we reach the final character in a valid state, false otherwise.</returns>
        private bool RunAutomata(Context ctx)
        {
            int stateIndex = 0;
            for (int i = 0; i < ctx.Sequence.Count; i++)
            {
                Character c = ctx.Sequence[i];
                if (!_Automata[stateIndex][c])
                {//No transition
                    ctx.ValidationMessages.Add(string.Format(Context.InvalidSymbolPosMsg, SC2String(c.ch)));
                    return false;
                }
                ctx.StateIndex = stateIndex;
                ctx.SequenceIndex = i;
                int gotoState = ctx.GetState(c);
                if (!ctx.OnGoto(c, stateIndex, ref gotoState))
                    return false;
                stateIndex = gotoState;
            }
            return true;
        }
    }
}
