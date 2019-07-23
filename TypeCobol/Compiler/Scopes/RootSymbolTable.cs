using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// The Root Symbol Table is a special Namespace
    /// </summary>
    public class RootSymbolTable : NamespaceSymbol
    {
        /// <summary>
        /// This is the first variable of the universe that can be assimilated to the 0 or null variable.
        /// </summary>
        public VariableSymbol BottomVariable { get; private set;}

        /// <summary>
        /// The count of all variable created in this RootSymbolTable
        /// </summary>
        private uint _variableSymbolCounter = 0;
        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public RootSymbolTable() : base("<<Root>>")
        {
            base.Kind = Kinds.Global;
            Universe = new List<VariableSymbol>();
            BottomVariable = new VariableSymbol("<<BottomVariable>>");
            AddToUniverse(BottomVariable);
        }

        /// <summary>
        /// Full qualified name of this Symbol à la TypeCobol using "::"
        /// </summary>
        public override String FullName => "";

        /// <summary>
        /// Full qualified name of this Symbol à la COBOL85 using OF
        /// </summary>
        public override String FullOfName => "";

        /// <summary>
        /// Full dotted qualified name
        /// </summary>
        public override String FullDotName => "";

        /// <summary>
        /// Full typed dotted qualified name
        /// </summary>
        public override String FullTypedDotName => "";

        public override String TypedName => "";


        /// <summary>
        /// The Count of Variable Symbol created
        /// </summary>
        public uint VariableSymbolCount => _variableSymbolCounter;

        /// <summary>
        /// Get the Next VariableSymbol Context.
        /// </summary>
        /// <returns></returns>
        private uint NextVariableSymbolIndex()
        {
            return _variableSymbolCounter++;
        }

        /// <summary>
        /// Add the given VariableSymbol instance in this Root Symbol Table universe
        /// </summary>
        /// <param name="varSym">The Variable Symbol to be added</param>
        /// <returns>The given VariableSymbol instance.</returns>
        public VariableSymbol AddToUniverse(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex == 0);
            lock (Universe)
            {
                varSym.GlobalIndex = NextVariableSymbolIndex();
                Universe.Add(varSym);
            }
            return varSym;
        }

        /// <summary>
        /// All Ordered Symbol that can be reached from this Root Symbol Table.
        /// This is in fact the entire domain of variable within this Flobal Symbol Table.
        /// </summary>
        public IList<VariableSymbol> Universe
        {
            get;
            internal set;
        }
    }
}
