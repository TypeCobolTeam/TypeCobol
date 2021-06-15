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
    public partial class PictureValidator
    {
        private const string SYMBOL_COUNT_CANNOT_BE_ZERO = "Symbol count cannot be zero";
        private const string INVALID_SYMBOL_POSITION = "Invalid position in PICTURE string of the symbol : {0}";
        private const string SYMBOL_S_MUST_OCCUR_ONLY_ONCE = "Character S must be repeated only once in PICTURE string";
        private const string SYMBOL_S_MUST_BE_THE_FIRST = "S must be at the beginning of a PICTURE string";
        private const string MULTIPLE_V = "V must appears only once in a PICTURE string";
        private const string WRONG_P_POSITION = "P must appears at the head or tail position of a PICTURE string";
        private const string Z_STAR_MUTUALLY_EXCLUSIVE = "Z and * symbols are mutually exclusive in a PICTURE string.";
        private const string MORE_THAN_ONE_E_CHARACTER = "Only one occurrence of E symbol can appear in a PICTURE string";
        private const string MORE_THAN_ONE_CR_CHARACTER = "Only one occurrence of CR symbol can appear in a PICTURE string";
        private const string MORE_THAN_ONE_DB_CHARACTER = "Only one occurrence of DB symbol can appear in a PICTURE string";
        private const string MORE_THAN_ONE_S_CHARACTER = "Only one occurrence of S symbol can appear in a PICTURE string";
        private const string MORE_THAN_ONE_V_CHARACTER = "Only one occurrence of V symbol can appear in a PICTURE string";
        private const string MORE_THAN_ONE_DOT_CHARACTER = "Only one occurrence of '.' symbol can appear in a PICTURE string";
        private const string MUTUALLY_EXCLUSIVE_SYMBOLS = "+/-/CR/DB are mutually exclusive";

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
        private Character[] CollectPictureSequence(List<Tuple<string, int>> matches)
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
                    ValidationMessages.Add(SYMBOL_COUNT_CANNOT_BE_ZERO);
                }

                SC sc;
                if (ch.Length == 1)
                {
                    sc = ch.Equals(CurrencySymbol) ? SC.CS : Char2SC(ch[0]);
                }
                else if (ch == CurrencySymbol)
                    sc = SC.CS;
                else if (ch == "CR")
                    sc = SC.CR;
                else if (ch == "DB")
                    sc = SC.DB;
                else
                    throw new InvalidOperationException();//Should never arrive
                if (prevChar != null && prevChar.SpecialChar == sc)
                {
                    prevChar.Count += count;
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
                            ValidationMessages.Add(MORE_THAN_ONE_CR_CHARACTER);
                        }
                        break;
                    case SC.DB:
                        foundDB = true;
                        db_count += count;
                        if (db_count > 1)
                        {
                            ValidationMessages.Add(MORE_THAN_ONE_DB_CHARACTER);
                        }
                        break;
                    case SC.S:
                        s_count += count;
                        if (s_count > 1)
                        {
                            ValidationMessages.Add(MORE_THAN_ONE_S_CHARACTER);
                        }
                        break;
                    case SC.V:
                        v_count += count;
                        if (v_count > 1)
                        {
                            ValidationMessages.Add(MORE_THAN_ONE_V_CHARACTER);
                        }
                        break;
                    case SC.E:
                        e_count += count;
                        if (e_count > 1)
                        {
                            ValidationMessages.Add(MORE_THAN_ONE_E_CHARACTER);
                        }
                        break;
                    case SC.DOT:
                        dot_count += count;
                        if (dot_count > 1)
                        {
                            ValidationMessages.Add(MORE_THAN_ONE_DOT_CHARACTER);
                        }
                        break;
                }
            }
            int cntFound = (foundCR || foundDB ? 1 : 0);
            cntFound += (foundPlus || foundMinus ? 1 : 0);
            if (cntFound > 1)
            { // 0 is valid
                ValidationMessages.Add(MUTUALLY_EXCLUSIVE_SYMBOLS);
            }

            return sequence.ToArray();
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
            Character[] sequence = CollectPictureSequence(matches);
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
        private bool ValidatePictureSequence(Character[] sequence)
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
        private void ComputeFloatingStringIndexes(Character[] sequence, out int firstIndex, out int lastIndex)
        {
            firstIndex = lastIndex = -1;
            int lastNonSimpleIndex = -1;
            SC floatChar = (SC)(-1); //The float character that corresponds to the first index either CS, + or -.
            int i;
            for (i = 0; i < sequence.Length; i++)
            {
                Character c = sequence[i];
                if (firstIndex == -1 && (c.SpecialChar == SC.PLUS || c.SpecialChar == SC.MINUS || c.SpecialChar == SC.CS))
                {
                    if (lastNonSimpleIndex >= 0 && sequence[lastNonSimpleIndex].SpecialChar == c.SpecialChar)
                    {
                        firstIndex = lastNonSimpleIndex;
                        floatChar = c.SpecialChar;
                        continue;
                    }
                    else if (c.Count > 1)
                    {
                        firstIndex = i;
                        floatChar = c.SpecialChar;
                        continue;
                    }
                }
                if (firstIndex == -1 && !IsSimpleInsertionCharacter(c.SpecialChar))
                {
                    lastNonSimpleIndex = i;
                }
                else if (firstIndex >= 0 && !(IsSimpleInsertionCharacter(c.SpecialChar) || c.SpecialChar == floatChar))
                {
                    lastIndex = i - 1;
                    break;
                }
            }

            if (i >= sequence.Length && firstIndex >= 0)
            { //We have reach the end of the sequence with a first index and no lastIndex ==>
              //Set the last index to the last character of the sequence
                lastIndex = sequence.Length - 1;
                return;
            }
            else if (!(i < sequence.Length && (SC2String(sequence[i].SpecialChar)[0] == DecimalPoint || sequence[i].SpecialChar == SC.V)))
            {//The last index does not precede the DecimalPoint separator position
                return;
            }
            //If the last index precede the DecimalPoint position so all characters including the decimal point
            //that are not simple characters or the floating character must be part o the right most index

            for (++i; i < sequence.Length; i++)
            {
                Character c = sequence[i];
                if (!(IsSimpleInsertionCharacter(c.SpecialChar) || c.SpecialChar == floatChar))
                    return;
            }
            lastIndex = i - 1;
        }

        /// <summary>
        /// Compute the initial validation context.
        /// </summary>
        /// <param name="sequence">The sequence of characters.</param>
        /// <returns></returns>
        private Context ComputeInitialContext(Character[] sequence)
        {
            ComputeFloatingStringIndexes(sequence, out int firstIndex, out int lastIndex);
            return new Context(sequence, this.ValidationMessages, this.IsSeparateSign)
                   {
                       FirstFloatingIndex = firstIndex,
                       LastFloatingIndex = lastIndex,
                       CurrencySymbol = this.CurrencySymbol,
                       DecimalPoint = this.DecimalPoint,
                       NumericSeparator = this.NumericSeparator
                   };
        }

        /// <summary>
        /// Run the automata on the given context along with its PICTURE sequence.
        /// </summary>
        /// <param name="ctx">The context</param>
        /// <returns>true if we reach the final character in a valid state, false otherwise.</returns>
        private bool RunAutomata(Context ctx)
        {
            int state = 0;
            for (int i = 0; i < ctx.Sequence.Length; i++)
            {
                Character c = ctx.Sequence[i];
                if (!Automata._States[state][(int) c.SpecialChar])
                {//No transition
                    ctx.ValidationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, SC2String(c.SpecialChar)));
                    return false;
                }
                
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
