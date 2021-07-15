using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        private class Automata
        {
            /// <summary>
            /// Total count of special characters. Size of the automata alphabet.
            /// </summary>
            private static readonly int _SpecialCharactersCount = Enum.GetValues(typeof(SC)).Length;

            /// <summary>
            /// States of the automata. Each state lists all allowed transitions from one SC to another.
            /// </summary>
            private static readonly bool[][] _States =
            {
                //State 0: Start Symbols
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.Z, SC.STAR, SC.CS, SC.NINE, SC.A, SC.X, SC.S, SC.V, SC.P, SC.G, SC.N),

                //------------------------------------------------------
                // NON FLOATING INSERTION SYMBOLS
                //------------------------------------------------------
                //State 1: --B-->(1)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.CS, SC.NINE, SC.A, SC.X, SC.V, SC.P, SC.G, SC.N),
                //State 2: --[0|/]-->(2)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.CS, SC.NINE, SC.A, SC.X, SC.V, SC.P, SC.N),
                //State 3: --,-->(3)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.CS, SC.E, SC.NINE, SC.V, SC.P),
                //State 4: --.-->(4)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.CS, SC.E, SC.NINE),
                //State 5: --[+|-]-->(5)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.Z, SC.STAR, SC.CS, SC.NINE, SC.V, SC.P),
                //State 6: --[+|-]-->(6)
                State(),
                //State : --[CR|DB]-->()
                State(),
                //State : --CS-->(8)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.NINE, SC.V, SC.P),
                //State : --E-->(9)
                State(SC.PLUS, SC.MINUS),

                //------------------------------------------------------
                //FLOATING INSERTION SYMBOLS
                //------------------------------------------------------
                //State 10: --[Z|*]-->(10)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.NINE, SC.V, SC.P),
                //State 11: --[Z|*]-->(11)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR),
                //State 12: --[+|-]-->(12)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.NINE, SC.V, SC.P),
                //State 13: --[+|-]-->(13)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS),
                //State 14: --CS-->(14)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.CS, SC.NINE, SC.V, SC.P),
                //State 15: --CS-->(15)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.CS),

                //------------------------------------------------------
                //OTHER SYMBOLS
                //------------------------------------------------------
                //State 16: --9-->(16)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.DOT, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.E, SC.NINE, SC.A, SC.X, SC.V, SC.P),
                //State 17: --[A|X]-->(17)
                State(SC.B, SC.ZERO, SC.SLASH, SC.NINE, SC.A, SC.X),
                //State 18: --S-->(18)
                State(SC.NINE, SC.V, SC.P),
                //State 19: --V-->(19)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.E, SC.Z, SC.STAR, SC.CS, SC.NINE, SC.P),
                //State 20: --P-->(20)
                State(SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.V, SC.P),
                //State 21: --P-->(21)
                State(SC.B, SC.ZERO, SC.SLASH, SC.COMMA, SC.PLUS, SC.MINUS, SC.CR, SC.DB, SC.Z, SC.STAR, SC.NINE, SC.P),
                //State 22: --G-->(22)
                State(SC.B, SC.G),
                //State 23: --N-->(23)
                State(SC.B, SC.ZERO, SC.SLASH, SC.N)
            };

            /// <summary>
            /// Create a state
            /// </summary>
            /// <param name="characterTransitions">Transition table for this state</param>
            private static bool[] State(params SC[] characterTransitions)
            {
                System.Diagnostics.Debug.Assert(characterTransitions != null);
                var result = new bool[_SpecialCharactersCount];
                foreach (var sc in characterTransitions)
                {
                    System.Diagnostics.Debug.Assert(result[(int) sc] == false); //Avoid transition redefinition
                    result[(int) sc] = true;
                }

                return result;
            }

            private readonly PictureValidator _validator;

            //run private data
            private Character[] _sequence;
            private int _firstFloatingIndex;
            private int _lastFloatingIndex;
            private int _sequenceIndex;
            private bool _digitsSeenBeforeP;
            private bool _isBeforeDecimalPoint;

            //run output
            public PictureCategory Category { get; private set; }
            public int Digits { get; private set; }
            public int RealDigits { get; private set; }
            public bool IsSigned { get; private set; }
            public int Scale { get; private set; }
            public int Size { get; private set; }

            public Automata(PictureValidator validator)
            {
                _validator = validator;
            }

            /// <summary>
            /// Determines if the current Sequence Index is inside the Floating Insertion Symbols range.
            /// </summary>
            private bool IsCurrentIndexInsideFloatingInsertion
                => _firstFloatingIndex >= 0
                   && _lastFloatingIndex >= 0
                   && _firstFloatingIndex <= _sequenceIndex
                   && _sequenceIndex <= _lastFloatingIndex;

            /// <summary>
            /// Return true if the current sequence index is the first symbol of the sequence.
            /// </summary>
            private bool IsFirstSymbol => _sequenceIndex == 0;

            /// <summary>
            /// Return true if the current sequence index is the last symbol of the sequence.
            /// </summary>
            private bool IsLastSymbol => _sequenceIndex == _sequence.Length - 1;

            public bool Run(Character[] sequence, List<string> validationMessages)
            {
                //Initialize run variables
                Initialize(sequence);

                //Some character counts/flags used during validation
                int S_count = 0;               //Count of either CR, DB, + or - symbols
                int V_count = 0;               //Count of V symbol
                bool Z_seen = false;           //Have we seen Z ?
                bool Star_seen = false;        //Have we seen '*' ?
                bool CS_signSizeAdded = false; //Flag to detect the first time we encounter a CS symbol during size computation

                //Run automata
                int state = 0;
                while (_sequenceIndex < _sequence.Length)
                {
                    Character c = _sequence[_sequenceIndex];

                    if (!_States[state][(int) c.SpecialChar])
                    {
                        //No transition allowed
                        validationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, _validator.SC2String(c.SpecialChar)));
                        return false;
                    }

                    int nextState = GetState(c);
                    if (!ValidateState(c))
                    {
                        return false;
                    }

                    state = nextState;
                    _sequenceIndex++;
                }

                //Successful validation, adjust category then return
                AdjustCategory();
                return true;

                //Called when changing transition on the given character, so specific validation actions can be performed here.
                bool ValidateState(Character c)
                {
                    switch (c.SpecialChar)
                    {
                        case SC.B:      //'B'
                        case SC.ZERO:   //'0'
                        case SC.SLASH:  //'/'
                            Category |= PictureCategory.Edited;
                            break;
                        case SC.PLUS:   //'+'
                        case SC.MINUS:  //'-'
                            Category |= PictureCategory.NumericEdited;
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
                                validationMessages.Add(Z_STAR_MUTUALLY_EXCLUSIVE);
                                return false;
                            }
                            Category |= PictureCategory.NumericEdited;
                            Digits += c.Count;
                            if (V_count > 0)
                            {
                                Scale += c.Count;
                            }
                            break;
                        case SC.CR:
                        case SC.DB:
                            Category |= PictureCategory.NumericEdited;
                            S_count++;
                            break;
                        case SC.CS:
                            Category |= PictureCategory.NumericEdited;
                            Digits += c.Count - 1;
                            break;
                        case SC.E:
                            break;
                        case SC.A:
                            Category |= PictureCategory.Alphabetic;
                            break;
                        case SC.X:
                            Category |= PictureCategory.AlphaNumeric;
                            break;
                        case SC.NINE://9
                            Category |= PictureCategory.Numeric;
                            Digits += c.Count;
                            RealDigits += c.Count;
                            if (V_count > 0)
                            {
                                //We have seen the decimal point --> digits are in the decimal part
                                Scale += c.Count;
                            }
                            break;
                        case SC.G:
                            Category |= PictureCategory.Dbcs;
                            break;
                        case SC.N:
                            Category |= PictureCategory.National;
                            break;
                        case SC.S:
                            Category |= PictureCategory.Numeric;
                            if (c.Count > 1)
                            {
                                validationMessages.Add(SYMBOL_S_MUST_OCCUR_ONLY_ONCE);
                                return false;
                            }
                            if (state != 0 || _sequenceIndex != 0)
                            {
                                validationMessages.Add(SYMBOL_S_MUST_BE_THE_FIRST);
                                return false;
                            }
                            S_count += c.Count;
                            break;
                        case SC.DOT:
                        case SC.COMMA:
                            Category |= PictureCategory.NumericEdited;
                            if (!_validator.IsDecimalPoint(c.SpecialChar))
                                break;
                            goto case SC.V;
                        case SC.V:
                            Category |= PictureCategory.Numeric;
                            V_count += c.Count;
                            if (V_count > 1)
                            {
                                validationMessages.Add(MULTIPLE_V);
                                return false;
                            }
                            _isBeforeDecimalPoint = false;
                            break;
                        case SC.P:
                            Category |= PictureCategory.Numeric;
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
                            Size += _validator.IsSeparateSign ? 1 : 0;
                            break;
                        case SC.V:
                        case SC.P:
                            break;
                        case SC.DB:
                        case SC.CR:
                            Size += c.Count * 2;
                            break;
                        case SC.CS:
                            System.Diagnostics.Debug.Assert(_validator._currencyDescriptor != null);
                            if (!CS_signSizeAdded)
                            {
                                //First CS adds the sign length
                                Size += _validator._currencyDescriptor.Sign.Length;
                                CS_signSizeAdded = true;
                                Size += c.Count - 1; //Each subsequent CS counts for 1 character
                            }
                            else
                            {
                                Size += c.Count; //Each subsequent CS counts for 1 character
                            }
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

                /*
                 * Validate the position of the P character. 
                   The Symbol P specified a scaling position and implies an assumed decimal point (to the left of the Ps if the Ps are leftmost PICTURE characters,
                   to right of the Ps if the Ps are rightmost PICTURE characters).
                   If we say that the character ^ means the beginning of the PICTURE sequence
                   and $ means the end of the PICTURE sequence sequence, only the following situations are valid for P.
                   ^P | ^VP | ^SP | ^SVP | P$ | PV$
                 */
                bool ValidateP()
                {
                    if (IsFirstSymbol || IsLastSymbol)
                    {
                        V_count += _sequenceIndex == 0 ? 1 : 0; //Assume decimal point symbol V at the beginning
                        return true;//^P | P$;
                    }
                    if (_sequenceIndex == 1 && (_sequence[0].SpecialChar == SC.S || _sequence[0].SpecialChar == SC.V))
                    {
                        V_count += 1;//Assume decimal point symbol V at the beginning
                        return true;//^SP | ^VP
                    }
                    if (_sequenceIndex == 2 && _sequence[0].SpecialChar == SC.S && _sequence[1].SpecialChar == SC.V)
                    {
                        V_count += 1;//Assume decimal point symbol V at the beginning
                        return true;//^SVP
                    }
                    if (_sequenceIndex == _sequence.Length - 2 && _sequence[_sequence.Length - 1].SpecialChar == SC.V)
                    {
                        return true;//$PV
                    }

                    //validation failed
                    validationMessages.Add(WRONG_P_POSITION);
                    return false;
                }
            }

            private void Initialize(Character[] sequence)
            {
                _sequence = sequence;
                _validator.ComputeFloatingStringIndexes(_sequence, out _firstFloatingIndex, out _lastFloatingIndex);
                _sequenceIndex = 0;
                _digitsSeenBeforeP = false;
                _isBeforeDecimalPoint = true;
            }

            /// <summary>
            /// Get the state that is used to handle the given character in the Automata
            /// </summary>
            /// <param name="c">The character to get the handling state</param>
            /// <returns>The state number if one exist, -1 otherwise</returns>
            private int GetState(Character c)
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
                                if (IsLastSymbol)
                                    return 6;
                                return 5;
                            }

                            _digitsSeenBeforeP = true;
                            return _isBeforeDecimalPoint ? 12 : 13;
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

                            _digitsSeenBeforeP = true;
                            return _isBeforeDecimalPoint ? 14 : 15;
                        }
                    case SC.E:
                        return 9;
                    case SC.Z:
                    case SC.STAR:
                        _digitsSeenBeforeP = true;
                        return _isBeforeDecimalPoint ? 10 : 11;
                    case SC.NINE:
                        _digitsSeenBeforeP = true;
                        return 16;
                    case SC.A:
                    case SC.X:
                        return 17;
                    case SC.S:
                        return 18;
                    case SC.V:
                        return 19;
                    case SC.P:
                        return _digitsSeenBeforeP && _isBeforeDecimalPoint ? 20 : 21;
                    case SC.G:
                        return 22;
                    case SC.N:
                        return 23;
                    default:
                        return -1;//Should never arrive.
                }
            }

            /// <summary>
            /// Perform post-validation adjustment of the computed Category
            /// for special categories ExternalFloat and Dbcs.
            /// </summary>
            private void AdjustCategory()
            {
                if (IsExternalFloatSequence())
                {
                    Category = PictureCategory.ExternalFloat;
                }
                else if (IsDbcsSequence())
                {
                    Category = PictureCategory.Dbcs;
                }

                bool IsExternalFloatSequence()
                {
                    if (_sequence.Length <= 4)
                        return false; //Should contain at least (+|-)*2,(.|V),E
                    if (Category != PictureCategory.NumericEdited)
                        return false; //By Default we should end on a NumericEdited category.
                    int i = 0;
                    if (_sequence[i].SpecialChar != SC.PLUS && _sequence[i].SpecialChar != SC.MINUS)
                        return false;
                    int len = _sequence.Length;
                    i++;
                    if (_sequence[i].SpecialChar == SC.DOT || _sequence[i].SpecialChar == SC.V)
                    {
                        if (_sequence[i + 1].SpecialChar != SC.NINE)
                            return false;
                    }
                    else if (_sequence[i].SpecialChar == SC.NINE)
                    {
                        i++;
                        if (_sequence[i].SpecialChar != SC.DOT & _sequence[i].SpecialChar != SC.V)
                            return false;
                        i++;
                    }
                    else
                        return false;
                    if (i >= len || _sequence[i].SpecialChar == SC.NINE)
                        i++;
                    if (i >= len || _sequence[i].SpecialChar != SC.E)
                        return false;
                    if (++i >= len || (_sequence[i].SpecialChar != SC.PLUS && _sequence[i].SpecialChar != SC.MINUS))
                        return false;
                    if (++i >= len || !(_sequence[i].SpecialChar == SC.NINE && _sequence[i].Count == 2))
                        return false;
                    return i == (len - 1);
                }

                bool IsDbcsSequence() => _sequence.Length > 0 && Array.TrueForAll(_sequence, c => c.SpecialChar == SC.G || c.SpecialChar == SC.B);
            }
        }
    }
}
