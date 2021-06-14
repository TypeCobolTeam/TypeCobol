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
            private readonly Character[] _sequence;
            private readonly int _firstFloatingIndex;
            private readonly int _lastFloatingIndex;

            public Automata(PictureValidator validator, Character[] sequence, int firstFloatingIndex, int lastFloatingIndex)
            {
                _validator = validator;
                _sequence = sequence;
                _firstFloatingIndex = firstFloatingIndex;
                _lastFloatingIndex = lastFloatingIndex;
            }

            public bool Run(List<string> validationMessages, Context context) //TODO remove context arg when done
            {
                int stateIndex = 0;
                for (int sequenceIndex = 0; sequenceIndex < _sequence.Length; sequenceIndex++)
                {
                    Character c = _sequence[sequenceIndex];
                    if (!_States[stateIndex][(int)c.SpecialChar])
                    {
                        //No transition
                        validationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, _validator.SC2String(c.SpecialChar)));
                        return false;
                    }

                    context.SequenceIndex = sequenceIndex;
                    int gotoState = context.GetState(c);
                    if (!context.OnGoto(c, stateIndex, ref gotoState))
                        return false;

                    stateIndex = gotoState;
                }

                return true;
            }
        }
    }
}
