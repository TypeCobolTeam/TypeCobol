using System;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// An Automate State a state is a list of Transition.
        /// </summary>
        private class State
        {
            /// <summary>
            /// Total count of special characters.
            /// </summary>
            private static readonly int _SpecialCharactersCount = Enum.GetValues(typeof(SC)).Length;

            /// <summary>
            /// Transitions on characters boolean vector.
            /// </summary>
            private readonly bool[] _trans;

            /// <summary>
            /// Transition table constructor
            /// </summary>
            /// <param name="characterTransitions">Transition table for this state</param>
            public State(params SC[] characterTransitions)
            {
                System.Diagnostics.Debug.Assert(characterTransitions != null);
                _trans = new bool[_SpecialCharactersCount];
                foreach (var sc in characterTransitions)
                {
                    System.Diagnostics.Debug.Assert(_trans[(int) sc] == false);
                    _trans[(int) sc] = true;
                }
            }

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[Character c] => _trans[(int) c.SpecialChar];
        }
    }
}
