using System.Linq;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// A validation context.
        /// </summary>
        public class Context
        {
            private readonly Automata _automata;

            internal Context(Character[] sequence, Automata automata)
            {
                Sequence = sequence;
                _automata = automata;
            }

            public int Scale => _automata.Scale;
            public int RealDigits => _automata.RealDigits;
            public bool IsSigned => _automata.IsSigned;

            /// <summary>
            /// Determines if the current sequence after a call to IsValid method, is in fact an ExternalFloat picture string
            /// category.
            /// </summary>
            /// <returns></returns>
            public bool IsExternalFloatSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Length <= 4)
                    return false;// should contained with at leas (+|-)*2,(.|V),E
                if (this.Category != PictureCategory.NumericEdited)
                    return false;//By Default is a NumericEdited category.
                int i = 0;
                if (Sequence[i].SpecialChar != SC.PLUS && Sequence[i].SpecialChar != SC.MINUS)
                    return false;
                int len = Sequence.Length;
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

            public int Digits => _automata.Digits;
            public PictureCategory Category => _automata.Category;
            internal Character[] Sequence { get; }
            public int Size => _automata.Size;

            /// <summary>
            /// Determines if the current sequence is a DBCS sequence
            /// </summary>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsDbcsSequence()
            {
                if (this.Sequence == null)
                    return false;
                if (this.Sequence.Length == 0)
                    return false;
                return this.Sequence.All(c => c.SpecialChar == SC.G || c.SpecialChar == SC.B);
            }
        }
    }
}
