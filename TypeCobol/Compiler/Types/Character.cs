namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The representation of a character of the NFA alphabet.
    /// </summary>
    public class Character
    {
        /// <summary>
        /// The character
        /// </summary>
        public SC SpecialChar { get; }

        /// <summary>
        /// The repetition count of the character.
        /// </summary>
        public int Count
        {
            get;
            internal set; //Used only at build time to increase count when two identical consecutive symbols are found.
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="sc">The character</param>
        /// <param name="n">The repetition count</param>
        internal Character(SC sc, int n)
        {
            SpecialChar = sc;
            Count = n;
        }

        public override string ToString()
        {
            string s;
            switch (SpecialChar)
            {
                case SC.A:
                    s = "A"; break;
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
                case SC.U:
                    s = "U"; break;
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
                    s = "¤"; break;
                case SC.CR:
                    s = "CR"; break;
                case SC.DB:
                    s = "DB"; break;
                default:
                    throw new NotSupportedException($"Unknown '{SpecialChar}' character.");
            }

            return Count > 0 ? s + '(' + Count + ')' : s;
        }
    }
}
