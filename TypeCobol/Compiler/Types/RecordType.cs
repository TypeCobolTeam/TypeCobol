using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;
using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Class that represents a record type
    /// </summary>
    public class RecordType : Type
    {
        /// <summary>
        /// The Scope of variables in this record.
        /// </summary>
        private Scope<VariableSymbol> _scope;

        public RecordType() : base(Tags.Record)
        {
            _scope = new Scope<VariableSymbol>();
        }
        /// <summary>
        /// Fields in this Records.
        /// </summary>
        public IList<VariableSymbol> Fields => Scope.Symbols;

        /// <summary>
        /// Get the scope of this reecord.
        /// </summary>
        public Scope<VariableSymbol> Scope => _scope;

        public override Type Expand(Symbol owner, bool bClone, Func<uint> varSymIndexer)
        {
            if (bClone)
            {
                RecordType recType = new RecordType();
                foreach (var varSym in Scope.Symbols)
                {
                    VariableSymbol sym = (VariableSymbol)varSym.Clone();
                    if (varSymIndexer != null)
                    {//Fresh variable with a new Global Index
                        varSym.GlobalIndex = varSymIndexer();
                    }
                    recType.Scope.Enter(sym);
                    //Check this symbol too for it to be expanded if necessary
                    sym.Check(varSymIndexer);
                    sym.Owner = owner;
                    //Propagate DataDivision location
                    sym.SetFlag(Symbol.Flags.FILE_SECTION, owner.HasFlag(Symbol.Flags.FILE_SECTION));
                    sym.SetFlag(Symbol.Flags.GLOBAL_STORAGE, owner.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
                    sym.SetFlag(Symbol.Flags.WORKING_STORAGE, owner.HasFlag(Symbol.Flags.WORKING_STORAGE));
                    sym.SetFlag(Symbol.Flags.LOCAL_STORAGE, owner.HasFlag(Symbol.Flags.LOCAL_STORAGE));
                    sym.SetFlag(Symbol.Flags.LINKAGE, owner.HasFlag(Symbol.Flags.LINKAGE));
                }
                return recType;
            }
            else
            {
                foreach (var varSym in Scope.Symbols)
                {
                    varSym.Check(varSymIndexer);
                }
                return this;
            }
        }

        public override void SetFlag(Flags flag, bool value)
        {
            foreach (var varSym in Scope.Symbols)
            {
                varSym.SetFlag(flag, value, true);
            }
        }

    }
}
