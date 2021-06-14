using System.Collections.Generic;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// A validation context.
        /// </summary>
        public class Context
        {
            /// <summary>
            /// Empty constructor.
            /// </summary>
            internal Context(List<Character> sequence, List<string> validationMessages, bool isSeparateSign)
            {
                Sequence = sequence;
                ValidationMessages = validationMessages;
                _isSeparateSign = isSeparateSign;
                IsBeforeDecimalPoint = true;
            }

            /// <summary>
            /// Indicating whether the sign is separate character
            /// </summary>
            private readonly bool _isSeparateSign;

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
            public bool IsSigned
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
                switch (c.SpecialChar)
                {
                    case SC.B:      //'B'
                    case SC.ZERO:   //'0'
                    case SC.SLASH:  //'/'
                        this.Category |= PictureCategory.Edited;
                        break;
                    case SC.PLUS:   //'+'
                    case SC.MINUS:  //'-'
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.Count - 1;
                        S_count++;
                        break;
                    case SC.Z:
                    case SC.STAR:
                        if (c.SpecialChar == SC.Z)
                            Z_seen = true;
                        else
                            Star_seen = true;
                        if (Z_seen && Star_seen)
                        {
                            ValidationMessages.Add(Z_STAR_MUTUALLY_EXCLUSIVE);
                            return false;
                        }
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.Count;
                        if (V_count > 0)
                        {
                            Scale += c.Count;
                        }
                        break;
                    case SC.CR:
                    case SC.DB:
                        this.Category |= PictureCategory.NumericEdited;
                        S_count++;
                        break;
                    case SC.CS:
                        this.Category |= PictureCategory.NumericEdited;
                        Digits += c.Count - 1;
                        break;
                    case SC.E:
                        break;
                    case SC.A:
                        this.Category |= PictureCategory.Alphabetic;
                        AX_digits += c.Count;
                        break;
                    case SC.X:
                        this.Category |= PictureCategory.AlphaNumeric;
                        AX_digits += c.Count;
                        break;
                    case SC.NINE://9
                        this.Category |= PictureCategory.Numeric;
                        Digits += c.Count;
                        RealDigits += c.Count;
                        if (V_count > 0)
                        {//We have seen the decimal point --> digitq are un the decimal part
                            Scale += c.Count;
                        }
                        break;
                    case SC.G:
                        this.Category |= PictureCategory.Dbcs;
                        AX_digits += c.Count;
                        break;
                    case SC.N:
                        this.Category |= PictureCategory.National;
                        AX_digits += c.Count;
                        break;
                    case SC.S:
                        this.Category |= PictureCategory.Numeric;
                        if (c.Count > 1)
                        {
                            ValidationMessages.Add(SYMBOL_S_MUST_OCCUR_ONLY_ONCE);
                            return false;
                        }
                        if (state != 0 || this.SequenceIndex != 0)
                        {
                            ValidationMessages.Add(SYMBOL_S_MUST_BE_THE_FIRST);
                            return false;
                        }
                        S_count += c.Count;
                        break;
                    case SC.DOT:
                    case SC.COMMA:
                        this.Category |= PictureCategory.NumericEdited;
                        if (c.SpecialChar != Char2SC(this.DecimalPoint))
                            break;
                        goto case SC.V;
                    case SC.V:
                        this.Category |= PictureCategory.Numeric;
                        V_count += c.Count;
                        if (V_count > 1)
                        {
                            ValidationMessages.Add(MULTIPLE_V);
                            return false;
                        }
                        this.IsBeforeDecimalPoint = false;
                        break;
                    case SC.P:
                        this.Category |= PictureCategory.Numeric;
                        if (!ValidateP())
                            return false;
                        Digits += c.Count;
                        Scale += (V_count > 0 ? c.Count : (-c.Count));
                        break;
                }
                if (S_count > 0)
                    IsSigned = true;
                //Update total size
                switch (c.SpecialChar)
                {
                    case SC.S:
                        Size += this._isSeparateSign ? 1 : 0;
                        break;
                    case SC.V:
                    case SC.P:
                        break;
                    case SC.CR:
                    case SC.CS:
                        Size += c.Count * 2;
                        break;
                    case SC.N:
                    case SC.G:
                        Size += c.Count * 2;
                        break;
                    default:
                        Size += c.Count;
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
                if (this.SequenceIndex == 1 && (this.Sequence[0].SpecialChar == SC.S || this.Sequence[0].SpecialChar == SC.V))
                {
                    V_count += 1;//Assume decimal point symbol V at the beginning
                    return true;//^SP | ^VP
                }
                if (this.SequenceIndex == 2 && (this.Sequence[0].SpecialChar == SC.S && this.Sequence[1].SpecialChar == SC.V))
                {
                    V_count += 1;//Assume decimal point symbol V at the beginning
                    return true;//^SVP
                }
                if (this.SequenceIndex == (this.Sequence.Count - 2) && this.Sequence[this.Sequence.Count - 1].SpecialChar == SC.V)
                    return true;//$PV
                this.ValidationMessages.Add(WRONG_P_POSITION);
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
                switch (c.SpecialChar)
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
                if (Sequence[i].SpecialChar != SC.PLUS && Sequence[i].SpecialChar != SC.MINUS)
                    return false;
                int len = Sequence.Count;
                i++;
                if (Sequence[i].SpecialChar == SC.DOT || Sequence[i].SpecialChar == SC.V)
                {
                    if (Sequence[i + 1].SpecialChar != SC.NINE)
                        return false;
                }
                else if (Sequence[i].SpecialChar == SC.NINE)
                {
                    i++;
                    if (Sequence[i].SpecialChar != SC.DOT & Sequence[i].SpecialChar != SC.V)
                        return false;
                    i++;
                }
                else
                    return false;
                if (i >= len || Sequence[i].SpecialChar == SC.NINE)
                    i++;
                if (i >= len || Sequence[i].SpecialChar != SC.E)
                    return false;
                if (++i >= len || (Sequence[i].SpecialChar != SC.PLUS && Sequence[i].SpecialChar != SC.MINUS))
                    return false;
                if (++i >= len || !(Sequence[i].SpecialChar == SC.NINE && Sequence[i].Count == 2))
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
                return this.Sequence.TrueForAll(c => c.SpecialChar == SC.A);
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
                return this.Sequence.TrueForAll(c => c.SpecialChar == SC.G || c.SpecialChar == SC.B);
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
                return this.Sequence.TrueForAll(c => c.SpecialChar == SC.NINE || c.SpecialChar == SC.S || c.SpecialChar == SC.V || c.SpecialChar == SC.P);
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
                return this.Sequence.TrueForAll(c => c.SpecialChar == SC.NINE || c.SpecialChar == SC.X || c.SpecialChar == SC.A);
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
                    c.SpecialChar == SC.B ||
                    c.SpecialChar == SC.P ||
                    c.SpecialChar == SC.V ||
                    c.SpecialChar == SC.Z ||
                    c.SpecialChar == SC.NINE ||
                    c.SpecialChar == SC.ZERO ||
                    c.SpecialChar == SC.SLASH ||
                    c.SpecialChar == SC.COMMA ||
                    c.SpecialChar == SC.DOT ||
                    c.SpecialChar == SC.PLUS ||
                    c.SpecialChar == SC.MINUS ||
                    c.SpecialChar == SC.STAR ||
                    c.SpecialChar == SC.CS
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
                    c.SpecialChar == SC.A ||
                    c.SpecialChar == SC.X ||
                    c.SpecialChar == SC.NINE ||
                    c.SpecialChar == SC.B ||
                    c.SpecialChar == SC.ZERO ||
                    c.SpecialChar == SC.SLASH
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
                return this.Sequence.TrueForAll(c => c.SpecialChar == SC.N);
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
                bool bAll = this.Sequence.TrueForAll(c => (hasN |= c.SpecialChar == SC.N) ||
                                c.SpecialChar == SC.B ||
                                c.SpecialChar == SC.ZERO ||
                                c.SpecialChar == SC.SLASH);
                return hasN && bAll;
            }

            /// <summary>
            /// All validation messages if any.
            /// </summary>
            public List<string> ValidationMessages { get; }
        }
    }
}
