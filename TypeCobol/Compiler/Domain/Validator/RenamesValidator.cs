using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;
using static TypeCobol.Compiler.Types.Type;
using Type = TypeCobol.Compiler.Types.Type;

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
    public class RenamesValidator : Symbol.AbstractSymbolVisitor<bool, ProgramSymbol>
    {
        private readonly VariableSymbol _from;
        private readonly VariableSymbol _to;
        private readonly RecordType _containerType;

        public int FromIndex { get; private set; }
        public int ToIndex { get; private set; }
        private int _index;
        public bool ContainsOccur { get; private set; }
        public Symbol OccurSymbol { get; private set; }

        /// <summary>
        /// The computed RENAMES type.
        /// </summary>
        public RenamesType Type { get; private set; }

        public bool IsValid => !ContainsOccur && FromSeen && ToSeen && FromIndex <= ToIndex;

        public bool FromSeen => FromIndex >= 0;
        public bool ToSeen => ToIndex >= 0;

        public RenamesValidator(RecordType containerType, VariableSymbol from, VariableSymbol to = null)
        {
            System.Diagnostics.Debug.Assert(from != null);
            this._containerType = containerType;
            this._from = from;
            this._to = to??from;
            FromIndex = ToIndex = -1;
            _index = 0;
        }

        public override bool VisitSymbol(Symbol s, ProgramSymbol arg)
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
                    case Tags.Record:
                    {//Validate each Symbol in the Record
                        RecordType recType = (RecordType) s.Type;
                        foreach (var field in recType.Scope)
                        {
                            if (!field.Accept(this, arg))
                                result = false;
                        }
                    }
                        break;
                }
            }
            
            //We use a flat representation so RECORD Symbols are flattened.
            if (FromSeen && s.Type != null && s.Type.Tag != Tags.Record)
            {//We can start building the renames type
                if (this.Type == null)
                {
                    this.Type = new RenamesType(null, _containerType, _from, _to);
                }

                this.Type.Scope.Enter((VariableSymbol)s);
            }
            return result;
        }
    }
}
