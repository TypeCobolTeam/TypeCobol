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
            internal static readonly bool[][] _States = //make private !
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

                //Run automata
                int stateIndex = 0;
                while (_sequenceIndex < _sequence.Length)
                {
                    Character c = _sequence[_sequenceIndex];

                    if (!_States[stateIndex][(int) c.SpecialChar])
                    {
                        //No transition
                        validationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, _validator.SC2String(c.SpecialChar)));
                        return false;
                    }

                    //TODO GetState / OnGoto

                    _sequenceIndex++;
                }

                return true;
            }

            private void Initialize(Character[] sequence)
            {
                _sequence = sequence;
                _validator.ComputeFloatingStringIndexes(_sequence, out _firstFloatingIndex, out _lastFloatingIndex);
                _sequenceIndex = 0;
                _digitsSeenBeforeP = false;
                _isBeforeDecimalPoint = true;
            }
        }
    }
}
