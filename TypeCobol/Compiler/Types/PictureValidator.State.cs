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
            public bool this[Character c] => this[c.SpecialChar];

            /// <summary>
            /// Determines if this state has a transition on the given character.
            /// </summary>
            /// <param name="c">The character to determine if it is a character of transition</param>
            /// <returns>true if a transition is possible, false otherwise</returns>
            public bool this[SC c] => Trans[(int)c];
        }
    }
}
