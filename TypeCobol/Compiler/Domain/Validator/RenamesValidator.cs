using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;
using static TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Domain.Validator
{
    /// <summary>
    /// Class use to validate a RENAMES construction.
    /// It Check that:
    ///     -it contains no variable with an ArrayType.
    ///     - from appears before to
    ///     - both from and to have been encountered
    ///     - It build the resulting RenamesType.
    /// </summary>
    public class RenamesValidator : Symbol.AbstractSymbolVisitor<bool, object>
    {
        private readonly RenamesSymbol _renamesSymbol;
        private readonly VariableSymbol _from;
        private readonly VariableSymbol _to;
        private readonly GroupType _containerType;
        private int _index;

        public int FromIndex { get; private set; }
        public int ToIndex { get; private set; }
        public bool ContainsOccur { get; private set; }
        public Symbol OccurSymbol { get; private set; }

        /// <summary>
        /// The computed RENAMES type.
        /// </summary>
        public RenamesType Type { get; private set; }

        public bool IsValid => !ContainsOccur && FromSeen && ToSeen && FromIndex <= ToIndex;

        public bool FromSeen => FromIndex >= 0;

        public bool ToSeen => ToIndex >= 0;

        public RenamesValidator(RenamesSymbol renamesSymbol, GroupType containerType, VariableSymbol from, VariableSymbol to = null)
        {
            System.Diagnostics.Debug.Assert(renamesSymbol != null);
            System.Diagnostics.Debug.Assert(from != null);

            this._renamesSymbol = renamesSymbol;
            this._containerType = containerType;
            this._from = from;
            this._to = to ?? from;

            FromIndex = ToIndex = -1;
            _index = 0;
        }

        public override bool VisitSymbol(Symbol s, object arg)
        {
            if (FromSeen && ToSeen)
                return true;//Stop every thing
            if (s == _from)
            {
                FromIndex = _index++;
            }

            if (s == _to)
            {
                ToIndex = _index++;
            }

            bool result = true;
            if (s.Type != null)
            {
                switch (s.Type.Tag)
                {
                    case Tags.Array:
                        ContainsOccur = true; //It contains Array Type
                        OccurSymbol = s;
                        result = false;
                        break;
                    case Tags.Group:
                    {//Validate each Symbol in the Group
                        GroupType recType = (GroupType) s.Type;
                        foreach (var field in recType.Fields)
                        {
                            if (!field.Accept(this, null))
                                result = false;
                        }
                    }
                        break;
                }
            }
            
            //We use a flat representation so Group Symbols are flattened.
            if (FromSeen && s.Type != null && s.Type.Tag != Tags.Group)
            {//We can start building the renames type
                if (this.Type == null)
                {
                    this.Type = new RenamesType(_renamesSymbol, _containerType, _from, _to);
                }

                this.Type.Fields.Enter((VariableSymbol)s);
            }
            return result;
        }
    }
}
