using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Given a Variable symbol this method renumber all level except levels
    /// 66, 88 and 77.
    /// Parameters is the start level.
    /// </summary>
    public class LevelRenumber : Symbol.AbstractSymbolVisitor<int, int>
    {
        /// <summary>
        /// The Type Level Renumber to use.
        /// </summary>
        private readonly Types.TypeLevelRenumber _typeLevelRenumber;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="typLevelRenumber">The TypeLevelRenumber instance to use if any</param>
        public LevelRenumber(Types.TypeLevelRenumber typLevelRenumber = null)
        {
            _typeLevelRenumber = typLevelRenumber??new Types.TypeLevelRenumber(this);
        }

        public override int VisitSymbol(Symbol s, int curLevel)
        {
            return curLevel;
        }

        public override int VisitVariableSymbol(VariableSymbol s, int curLevel)
        {
            System.Diagnostics.Debug.Assert(curLevel > 0);
            if (curLevel <= 49)
            {
                if (s.Level != 66 && s.Level != 77 && s.Level != 88)
                {
                    s.Level = curLevel;
                }
                return s.Type?.Accept(_typeLevelRenumber, curLevel) ?? curLevel;
            }
            else
            {
                throw new Symbol.LevelExceed(s);
            }
        }

        public override int VisitVariableTypeSymbol(VariableTypeSymbol s, int curLevel)
        {
            return VisitVariableSymbol(s, curLevel);
        }
    }
}
