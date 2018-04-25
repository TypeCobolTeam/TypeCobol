﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The goal of this class is to implement a PICTURE string format validator.
    /// Using precedence rules as described in the IBM Cobol Reference.
    /// 
    /// My strategy is to implement a NFA (A non-deterministic Finite Automata) over predecence
    /// character rules. Deterministic is obtained, by taking in acount insertion floating position and whether or nor
    /// the position is before or after a decimal floating point, cause for these situations specific states handle
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
        public PictureValidator(string picture, bool separateSign = false)
        {
            System.Diagnostics.Contracts.Contract.Requires(picture != null);
            System.Diagnostics.Contracts.Contract.Requires(picture.ToUpper().IndexOf("PIC") == -1);
            System.Diagnostics.Contracts.Contract.Requires(picture.ToUpper().IndexOf(' ') == -1);

            Picture = picture.ToUpper();
            IsSeparateSign = separateSign;
            CurrencySymbol = "$";
            DecimalPoint = '.';
            NumericSeparator = ',';
            ValidationMesssage = new List<string>();
        }

        /// <summary>
        /// The Picture string.
        /// </summary>
        public string Picture
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
        /// The Currency Symbol to be used.
        /// </summary>
        public String CurrencySymbol
        {
            get;
            set;
        }

        /// <summary>
        /// The decimal point character
        /// </summary>
        public char DecimalPoint
        {
            get;
            set;
        }

        /// <summary>
        /// The Numeric Separator
        /// </summary>
        public char NumericSeparator
        {
            get;
            set;
        }

        /// <summary>
        /// All validation messages if any.
        /// </summary>
        public List<String> ValidationMesssage
        {
            get;
            internal set;
        }

        /// <summary>
        /// Special RegEx characters
        /// </summary>
        private static readonly string SpecialReChars = "$^()[]{}|.,:;+*#<>\\";

        /// <summary>
        /// Normalize a String for a regular expression
        /// </summary>
        /// <param name="s">The string to normalize</param>
        /// <returns></returns>
        private static string NormalizeStringForRegEx(string s)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < s.Length; i += 2)
            {
                int index = SpecialReChars.IndexOf(s[i]);
                if (index >= 0)
                {
                    sb.Append('\\');
                }
                sb.Append(s[i]);
            }
            return sb.ToString();
        }

        /// <summary>
        /// Get the Picture regular expression
        /// </summary>
        /// <returns>A couple of regular expression the first item is for validation and the second for getting all matches collection.</returns>
        private Tuple<string, string> GetPictureRegEx()
        {
            string cs = NormalizeStringForRegEx(CurrencySymbol);
            String[] alphabet = { "A", "B", "E", "G", "N", "P", "S", "V", "X", "Z", "9", "0", "/", "\\,", "\\.", "\\+", "\\-", "CR", "DB", "\\*", cs };
            StringBuilder sb_valid = new StringBuilder();
            StringBuilder sb_matches = new StringBuilder();
            string alt = "";
            sb_valid.Append("(");
            foreach (string a in alphabet)
            {
                sb_matches.Append(alt);
                sb_matches.Append("(?<ch>").Append(a).Append(')').Append("(\\((?<count>[0-9]+)\\))?");
                alt = "|";
            }
            sb_valid.Append(sb_matches.ToString());
            sb_valid.Append(")+");
            return new Tuple<string, string>(sb_valid.ToString(), sb_matches.ToString());
        }

        /// <summary>
        /// Get all matches of the picture string against the picture regular expression.
        /// </summary>
        /// <returns>The matches collection</returns>
        private System.Text.RegularExpressions.MatchCollection GetPictureRegExMatches()
        {
            Tuple<string, string> pic_res = GetPictureRegEx();
            string pic_valid_re = pic_res.Item1;
            string pic_matches_re = pic_res.Item2;
            if (!System.Text.RegularExpressions.Regex.IsMatch(Picture, pic_valid_re))
                return null;
            System.Text.RegularExpressions.MatchCollection collection = System.Text.RegularExpressions.Regex.Matches(Picture, pic_matches_re);
            return collection;
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

        private String SC2String(SC c)
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
                    throw new ArgumentException();
            }
        }


        /// <summary>
        /// Dertermine if the given character is a simple insertion character.
        /// </summary>
        /// <param name="c">Return true if yes, false otherwise</param>
        /// <returns>true if yes, false otherwise</returns>
        public bool IsSimpleInsertionCharacter(char c)
        {
            return c == 'B' || c == '0' || c == '/' || c == NumericSeparator;
        }

        /// <summary>
        /// Dertermine if the given character is a simple insertion character.
        /// </summary>
        /// <param name="c">Return true if yes, false otherwise</param>
        /// <returns>true if yes, false otherwise</returns>
        internal bool IsSimpleInsertionCharacter(SC c)
        {
            return c == SC.B || c == SC.ZERO || c == SC.SLASH || c == Char2SC(NumericSeparator);
        }

        /// <summary>
        /// Collect the Picture sequence of characters from the RegEx matching. And perform some prevalidation
        /// checks.
        /// </summary>
        /// <param name="matches">All Picture items matched</param>
        /// <returns>The list of picture item sequence</returns>
        private List<Character> CollectPictureSequence(System.Text.RegularExpressions.MatchCollection matches)
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
                System.Text.RegularExpressions.Match match = (System.Text.RegularExpressions.Match)m;
                string ch = match.Groups["ch"].Value;
                string scount = match.Groups["count"].Value;
                int count = scount.Length == 0 ? 1 : System.Int32.Parse(scount);
                if (count == 0)
                {//Count cannot be 0.
                    ValidationMesssage.Add(Context.SymbolCountCannotBeZeroMsg);
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
                            ValidationMesssage.Add(Context.MoreThanOne_CR_CharacterMsg);
                        }
                        break;
                    case SC.DB:
                        foundDB = true;
                        db_count += count;
                        if (db_count > 1)
                        {
                            ValidationMesssage.Add(Context.MoreThanOne_DB_CharacterMsg);
                        }
                        break;
                    case SC.S:
                        s_count += count;
                        if (s_count > 1)
                        {
                            ValidationMesssage.Add(Context.MoreThanOne_S_CharacterMsg);
                        }
                        break;
                    case SC.V:
                        v_count += count;
                        if (v_count > 1)
                        {
                            ValidationMesssage.Add(Context.MoreThanOne_V_CharacterMsg);
                        }
                        break;
                    case SC.E:
                        e_count += count;
                        if (e_count > 1)
                        {
                            ValidationMesssage.Add(Context.MoreThanOne_E_CharacterMsg);
                        }
                        break;
                    case SC.DOT:
                        dot_count += count;
                        if (dot_count > 1)
                        {
                            ValidationMesssage.Add(Context.MoreThanOne_Dot_CharacterMsg);
                        }
                        break;
                }
            }
            int cntFound = (foundCR || foundDB ? 1 : 0);
            cntFound += (foundPlus || foundMinus ? 1 : 0);
            if (cntFound > 1)
            { // 0 is valid
                ValidationMesssage.Add(Context.MultuallyExclusiveSymbolMsg);
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
            //0. Picure String must contains at least
            //1. First Validate against the PICTURE string regular expression.
            System.Text.RegularExpressions.MatchCollection matches = GetPictureRegExMatches();
            if (matches == null)
                return false;
            if (matches.Count <= 0)
                return false;
            //Construct Charcater sequence
            ValidationMesssage.Clear();
            List<Character> sequence = CollectPictureSequence(matches);
            if (ValidationMesssage.Count > 0)
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
            SC floatChar = (SC)(-1); ; //The float character that corresponds to the first index either CS, + or -.
            int i = 0;
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
            { //We have reach thn end of the sequence with a first index and no lastIndex ==>
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
                Character c = sequence[i]; ;
                if (!(IsSimpleInsertionCharacter(c.ch) || c.ch == floatChar))
                    return;
            }
            lastIndex = i - 1;
        }

        /// <summary>
        /// Compute the initial validation context.
        /// </summary>
        /// <param name="sequence">The sequenc of characters.</param>
        /// <returns></returns>
        private Context ComputeInitialContext(List<Character> sequence)
        {
            int firstIndex;
            int lastIndex;
            ComputeFloatingStringIndexes(sequence, out firstIndex, out lastIndex);
            Context ctx = new Context(sequence);
            ctx.FirstFloatingIndex = firstIndex;
            ctx.LastFloatingIndex = lastIndex;
            ctx.CurrencySymbol = this.CurrencySymbol;
            ctx.DecimalPoint = this.DecimalPoint;
            ctx.NumericSeparator = this.NumericSeparator;
            ctx.IsSeparateSign = IsSeparateSign;
            ctx.ValidationMesssage = ValidationMesssage;

            return ctx;
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
            /// The repetion count of the character.
            /// </summary>
            public int count;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="c">The character</param>
            /// <param name="n">The repetion count</param>
            public Character(SC c, int n)
            {
                ch = c;
                count = n;
            }
        }

        /// <summary>
        /// A validation context.
        /// </summary>
        public class Context
        {
            internal static readonly string SymbolCountCannotBeZeroMsg = "Symbol count cannot be zero";
            internal static readonly string InvalidSymbolPosMsg = "Invalid position in PICTURE string of the symbol : {0}";
            internal static readonly string SymbolSMustOccurOnlyOnceMsg = "Character S must be repeated ont once in PICTURE string";
            internal static readonly string SymbolSMustBeTheFirstMsg = "S must be at the beginning of a PICTURE string";
            internal static readonly string MultipleVMsg = "V must appears only once in a PICTURE string";
            internal static readonly string WrongPPoitionMsg = "P must appears at the head or tail position of a PICTURE string";
            internal static readonly string ZStarMutuallyExclusiveMsg = "Z and * symbols are mutually exclusise ina PICTURE string.";
            internal static readonly string MoreThanOne_E_CharacterMsg = "Only one occurence of E symbol can appers in a PICTURE string";
            internal static readonly string MoreThanOne_CR_CharacterMsg = "Only one occurence of CR symbol can appers in a PICTURE string";
            internal static readonly string MoreThanOne_DB_CharacterMsg = "Only one occurence of DB symbol can appers in a PICTURE string";
            internal static readonly string MoreThanOne_S_CharacterMsg = "Only one occurence of S symbol can appers in a PICTURE string";
            internal static readonly string MoreThanOne_V_CharacterMsg = "Only one occurence of V symbol can appers in a PICTURE string";
            internal static readonly string MoreThanOne_Dot_CharacterMsg = "Only one occurence of '.' symbol can appers in a PICTURE string";
            internal static readonly string MultuallyExclusiveSymbolMsg = "+/-/CR/DB are mutually exclusive";

            /// <summary>
            /// Empty constructor.
            /// </summary>
            internal Context(List<Character> sequence)
            {
                ValidationMesssage = new List<String>();
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
            internal String CurrencySymbol
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
            /// Are we in Floating Insertion Symboils Mode ?
            /// </summary>
            internal bool IsFloatingInsertionMode
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
            internal int State
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
            /// Real number of digite
            /// </summary>
            public int RealDigits
            {
                get;
                private set;
            }

            /// <summary>
            /// The Index of the first character of a floating inssertion string in the sequence.
            /// </summary>
            internal int FirstFloatingIndex
            {
                get;
                set;
            }

            /// <summary>
            /// The Index of the last character of a floating inssertion string in the sequence.
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
            /// Called when changing transition on the given character, so specific validaton actions can be performed here.
            /// </summary>
            /// <param name="c">Character on the transition</param>
            /// <param name="t">Transition representing the next state</param>
            /// <param name="state">The current state of the character</param>
            /// <param name="gotoState">The state to goto, the method cans decide go to to another state that's why thie value is passed by reference.</param>
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
                            ValidationMesssage.Add(ZStarMutuallyExclusiveMsg);
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
                            ValidationMesssage.Add(SymbolSMustOccurOnlyOnceMsg);
                            return false;
                        }
                        if (state != 0 || this.SequenceIndex != 0)
                        {
                            ValidationMesssage.Add(SymbolSMustBeTheFirstMsg);
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
                            ValidationMesssage.Add(MultipleVMsg);
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
                    default:
                        break;
                }
                if (S_count > 0)
                    HaveSign = true;
                //Update total sise
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
            /// to right of the Ps if the Ps are rightmost PICTURE charcaters).
            /// If we say that the charcater ^ means the beginning of the PICTURE sequence
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
                this.ValidationMesssage.Add(WrongPPoitionMsg);
                return false;
            }

            /// <summary>
            /// Determines if the current Sequence Index is inside the Floating Insertion Symbols range.
            /// </summary>
            private bool IsCurrentIndexInsidefloatingInsertion
            {
                get
                {
                    return this.FirstFloatingIndex >= 0 && this.LastFloatingIndex >= 0 &&
                        this.FirstFloatingIndex <= this.SequenceIndex && this.SequenceIndex <= this.LastFloatingIndex;
                }
            }

            /// <summary>
            /// Return true if the current sequence index is the first symbol of the sequence.
            /// </summary>
            private bool IsFirstSymbol
            {
                get
                {
                    return SequenceIndex == 0;
                }
            }

            /// <summary>
            /// Return true if the current sequence index is the last symbol of the sequence.
            /// </summary>
            private bool IsLastymbol
            {
                get
                {
                    return SequenceIndex == Sequence.Count - 1;
                }

            }

            /// <summary>
            /// Get the state that is used to handle the given charcater in the Automata
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
                            if (!IsCurrentIndexInsidefloatingInsertion)
                            {
                                if (IsFirstSymbol)
                                    return 5;
                                else if (IsLastymbol)
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
                            if (!IsCurrentIndexInsidefloatingInsertion)
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
            /// Determines if the current sequence after a call to Isvalid method, is in fact an ExternalFloat picture string
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
                    return false;//By Defualt is a NumericEdited category.
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
            public bool IsAplphabeticSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.A);
            }

            /// <summary>
            /// Determines if the current sequence is a Dbcs seqeunce
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsDbcsSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Count == 0)
                    return false;
                return this.Sequence.TrueForAll(c => c.ch == SC.G || c.ch == SC.N || c.ch == SC.B);
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
            /// All validation messages if any.
            /// </summary>
            public List<String> ValidationMesssage
            {
                get;
                internal set;
            }

        }

        /// <summary>
        /// An Automate State a state is a list of Transition.
        /// </summary>
        private class State
        {
            /// <summary>
            /// The State Number
            /// </summary>
            public int Number
            {
                get;
                private set;
            }
            /// <summary>
            /// Transitions on characters boolean vector.
            /// </summary>
            public bool[] Trans
            {
                get;
                private set;
            }

            /// <summary>
            /// Transition table constructor
            /// </summary>
            /// <param name="number">The state number</param>
            /// <param name="gotos"></param>
            public State(int number, params SC[] vtrans)
            {
                System.Diagnostics.Contracts.Contract.Requires(vtrans != null);
                bool[] atrans = new bool[(int)SC.SpecialCharCount];
                for (int i = 0; i < vtrans.Length; i++)
                {
                    System.Diagnostics.Contracts.Contract.Assume(atrans[(int)vtrans[i]] == false);
                    atrans[(int)vtrans[i]] = true;
                }
                Number = number;
                Trans = atrans;
            }

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[Character c]
            {
                get
                {
                    return this[c.ch];
                }
            }

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[SC c]
            {
                get
                {
                    return Trans[(int)c];
                }
            }
        }

        private static SC T(char c)
        {
            return Char2SC(c);
        }
        /// <summary>
        /// Static representation of a (NFA) Non Deterministic Finite Automata over PICTURE symbols precedence rules.
        /// </summary>
        private static State[] Automata =
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
            int state = 0;
            for (int i = 0; i < ctx.Sequence.Count; i++)
            {
                Character c = ctx.Sequence[i];
                if (!Automata[state][c])
                {//No transition
                    ctx.ValidationMesssage.Add(string.Format(Context.InvalidSymbolPosMsg, SC2String(c.ch)));
                    return false;
                }
                ctx.State = state;
                ctx.SequenceIndex = i;
                int gotoState = ctx.GetState(c); ;
                if (!ctx.OnGoto(c, state, ref gotoState))
                    return false;
                state = gotoState;
            }
            return true;
        }
    }
}
