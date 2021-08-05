using System;
using System.Collections;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        private partial class Automata
        {
            /// <summary>
            /// Custom sequence enumerator that also tracks digits seen before P symbol and decimal point position
            /// </summary>
            private class SequenceEnumerator : IEnumerator<Character>
            {
                private readonly Character[] _sequence;
                private readonly int _firstFloatingIndex;
                private readonly int _lastFloatingIndex;

                public SequenceEnumerator(Character[] sequence, int firstFloatingIndex, int lastFloatingIndex)
                {
                    _sequence = sequence;
                    _firstFloatingIndex = firstFloatingIndex;
                    _lastFloatingIndex = lastFloatingIndex;
                    Reset();
                }

                public int Index { get; private set; }

                public bool IsBeforeDecimalPoint { get; private set; }

                public bool DigitsSeenBeforeP { get; private set; }

                object IEnumerator.Current => Current;

                public Character Current => _sequence[Index];

                public Character Previous => _sequence[Index - 1];

                public bool IsFirst => Index == 0;

                public bool IsLast => Index == _sequence.Length - 1;

                public bool IsInsideFloatingInsertionRange => _firstFloatingIndex >= 0 && _lastFloatingIndex >= 0 && _firstFloatingIndex <= Index && Index <= _lastFloatingIndex;

                public bool MoveNext()
                {
                    //Can't move next
                    if (IsLast) return false;

                    //Move to next character
                    Index++;

                    //Update status
                    System.Diagnostics.Debug.Assert(Current != null);
                    switch (Current.SpecialChar)
                    {
                        case SC.DOT:
                        case SC.V:
                            IsBeforeDecimalPoint = false;
                            break;
                        case SC.ZERO:
                        case SC.Z:
                        case SC.STAR:
                        case SC.NINE:
                            DigitsSeenBeforeP = true;
                            break;
                    }

                    return true;
                }

                public void Reset()
                {
                    Index = -1;
                    IsBeforeDecimalPoint = true;
                    DigitsSeenBeforeP = false;
                }

                public void Dispose()
                {
                    
                }
            }

            /// <summary>
            /// Implementation of table of symbol precedence in a PICTURE string
            /// </summary>
            private class State
            {
                private const int TRANSITION_TABLE_SIZE = 24;

                //Special initial state, allows all transitions
                public static readonly State Initial = new State(-1);

                //Each state is a column of the precedence table, each index given describes an allowed transition to the next state identified by this index
                private static readonly State _OnB = new State(0, 0, 1, 2, 3, 4, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 22, 23);
                private static readonly State _OnZero = new State(1, 0, 1, 2, 3, 4, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 23);
                private static readonly State _OnSlash = new State(2, 0, 1, 2, 3, 4, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 23);
                private static readonly State _OnComma = new State(3, 0, 1, 2, 3, 4, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 19, 20);
                private static readonly State _OnDot = new State(4, 0, 1, 2, 3, 6, 7, 9, 11, 13, 15, 16);
                private static readonly State _OnNonFloatingPlusOrMinusBeforeDecimalPoint = new State(5, 0, 1, 2, 3, 4, 8, 10, 11, 14, 15, 16, 19, 20, 21);
                private static readonly State _OnNonFloatingPlusOrMinusAfterDecimalPoint = new State(6);
                private static readonly State _OnCROrDB = new State(7);
                private static readonly State _OnNonFloatingCS = new State(8, 0, 1, 2, 3, 4, 6, 7, 10, 11, 12, 13, 16, 19, 20, 21);
                private static readonly State _OnE = new State(9, 6);
                private static readonly State _OnZOrStarBeforeDecimalPoint = new State(10, 0, 1, 2, 3, 4, 6, 7, 10, 11, 16, 19, 20);
                private static readonly State _OnZOrStarAfterDecimalPoint = new State(11, 0, 1, 2, 3, 6, 7, 11);
                private static readonly State _OnFloatingPlusOrMinusBeforeDecimalPoint = new State(12, 0, 1, 2, 3, 4, 12, 13, 16, 19, 20);
                private static readonly State _OnFloatingPlusOrMinusAfterDecimalPoint = new State(13, 0, 1, 2, 3, 13);
                private static readonly State _OnFloatingCSBeforeDecimalPoint = new State(14, 0, 1, 2, 3, 4, 6, 7, 14, 15, 16, 19, 20);
                private static readonly State _OnFloatingCSAfterDecimalPoint = new State(15, 0, 1, 2, 3, 6, 7, 15);
                private static readonly State _OnNine = new State(16, 0, 1, 2, 3, 4, 6, 7, 9, 16, 17, 19, 20);
                private static readonly State _OnAOrX = new State(17, 0, 1, 2, 16, 17);
                private static readonly State _OnS = new State(18, 16, 19, 20, 21);
                private static readonly State _OnV = new State(19, 0, 1, 2, 3, 6, 7, 9, 11, 13, 15, 16, 21);
                private static readonly State _OnPBeforeDecimalPoint = new State(20, 6, 7, 19, 20);
                private static readonly State _OnPAfterDecimalPoint = new State(21, 0, 1, 2, 3, 6, 7, 11, 16, 21);
                private static readonly State _OnG = new State(22, 0, 22);
                private static readonly State _OnN = new State(23, 0, 1, 2, 23);

                private readonly int _index;
                private readonly bool[] _allowed;

                private State(int index, params int[] allowedTransitions)
                {
                    _index = index;
                    _allowed = new bool[TRANSITION_TABLE_SIZE];
                    if (_index == -1)
                    {
                        //Initial state allows every symbol
                        for (int i = 0; i < TRANSITION_TABLE_SIZE; i++)
                        {
                            _allowed[i] = true;
                        }
                    }
                    else
                    {
                        foreach (int allowedTransition in allowedTransitions)
                        {
                            _allowed[allowedTransition] = true;
                        }
                    }
                }

                /// <summary>
                /// Check that the current state allows to transition on a the given symbol.
                /// Returns the new reached state as an out parameter.
                /// </summary>
                /// <param name="sc">New symbol encountered</param>
                /// <param name="sequenceEnumerator">Current sequence enumerator positioned on the new symbol encountered</param>
                /// <param name="nextState">[out] When allowed, the next state reached</param>
                /// <returns>True if the transition is allowed, False otherwise</returns>
                public bool CheckTransition(SC sc, SequenceEnumerator sequenceEnumerator, out State nextState)
                {
                    State candidate;
                    switch (sc)
                    {
                        case SC.B:
                            candidate = _OnB;
                            break;
                        case SC.ZERO:
                            candidate = _OnZero;
                            break;
                        case SC.SLASH:
                            candidate = _OnSlash;
                            break;
                        case SC.COMMA:
                            candidate = _OnComma;
                            break;
                        case SC.DOT:
                            candidate = _OnDot;
                            break;
                        case SC.PLUS:
                        case SC.MINUS:
                            if (sequenceEnumerator.IsInsideFloatingInsertionRange)
                            {
                                candidate = sequenceEnumerator.IsBeforeDecimalPoint ? _OnFloatingPlusOrMinusBeforeDecimalPoint : _OnFloatingPlusOrMinusAfterDecimalPoint;
                            }
                            else
                            {
                                if (!sequenceEnumerator.IsBeforeDecimalPoint || sequenceEnumerator.IsLast)
                                {
                                    candidate = _OnNonFloatingPlusOrMinusAfterDecimalPoint;
                                }
                                else
                                {
                                    candidate = _OnNonFloatingPlusOrMinusBeforeDecimalPoint;
                                }
                            }
                            break;
                        case SC.CR:
                        case SC.DB:
                            candidate = _OnCROrDB;
                            break;
                        case SC.CS:
                            if (sequenceEnumerator.IsInsideFloatingInsertionRange)
                            {
                                candidate = sequenceEnumerator.IsBeforeDecimalPoint ? _OnFloatingCSBeforeDecimalPoint : _OnFloatingCSAfterDecimalPoint;
                            }
                            else
                            {
                                candidate = _OnNonFloatingCS;
                            }
                            break;
                        case SC.E:
                            candidate = _OnE;
                            break;
                        case SC.Z:
                        case SC.STAR:
                            candidate = sequenceEnumerator.IsBeforeDecimalPoint ? _OnZOrStarBeforeDecimalPoint : _OnZOrStarAfterDecimalPoint;
                            break;
                        case SC.NINE:
                            candidate = _OnNine;
                            break;
                        case SC.A:
                        case SC.X:
                            candidate = _OnAOrX;
                            break;
                        case SC.S:
                            candidate = _OnS;
                            break;
                        case SC.V:
                            candidate = _OnV;
                            break;
                        case SC.P:
                            candidate = sequenceEnumerator.DigitsSeenBeforeP && sequenceEnumerator.IsBeforeDecimalPoint ? _OnPBeforeDecimalPoint : _OnPAfterDecimalPoint;
                            break;
                        case SC.G:
                            candidate = _OnG;
                            break;
                        case SC.N:
                            candidate = _OnN;
                            break;
                        default:
                            throw new ArgumentException($"Special character '{sc}' is not valid.", nameof(sc));
                    }

                    if (_allowed[candidate._index])
                    {
                        nextState = candidate;

                        /*
                         * Special case for ExternalFloatingPoint exponents.
                         * The table does not allow transition from {+|-} located after decimal point to 9, but it is valid in exponent of an external floating point.
                         * To bypass this, we replace the next state with the {+|-} before decimal point which allows this transition.
                         */
                        if (nextState == _OnNonFloatingPlusOrMinusAfterDecimalPoint && sequenceEnumerator.Index > 0 && sequenceEnumerator.Previous.SpecialChar == SC.E)
                        {
                            nextState = _OnNonFloatingPlusOrMinusBeforeDecimalPoint;
                        }

                        return true;
                    }

                    nextState = null;
                    return false;
                }

                /// <summary>
                /// Does this state allows repetition of the symbol ?
                /// </summary>
                /// <returns>True if reflexive transition is allowed, False otherwise</returns>
                public bool CheckReflexiveTransition() => _allowed[_index];
            }
        }
    }
}
