using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Given a Type this method renumber all level except levels
    /// 66, 88 and 77.
    /// Parameters are the start level and the max ending level.
    /// </summary>
    public class TypeLevelRenumber : Type.AbstractTypeVisitor<int, int>
    {
        private readonly Symbols.LevelRenumber _symbolLevelRenumber;

        public TypeLevelRenumber() : this(null)
        {            
        }

        public TypeLevelRenumber(Symbols.LevelRenumber symbolLevelRenumber = null)
        {
            _symbolLevelRenumber = symbolLevelRenumber??new LevelRenumber(this);
        }
        public override int VisitType(Type t, int curLevel)
        {
            return curLevel;
        }

        public override int VisitArrayType(ArrayType t, int currentLevel)
        {
            return t.ElementType?.Accept(this, currentLevel) ?? currentLevel;
        }

        public override int VisitPointerType(PointerType t, int currentLevel)
        {
            return t.ElementType?.Accept(this, currentLevel) ?? currentLevel;
        }

        public override int VisitGroupType(GroupType t, int currentLevel)
        {
            int maxLevel = currentLevel;
            foreach (var field in t.Scope)
            {
                maxLevel = Math.Max(maxLevel, field.Accept(_symbolLevelRenumber, currentLevel + 1));
            }
            return maxLevel;
        }

        public override int VisitTypedefType(TypedefType t, int currentLevel)
        {
            return t.TypeComponent?.Accept(this, currentLevel) ?? currentLevel;
        }
    }
}
