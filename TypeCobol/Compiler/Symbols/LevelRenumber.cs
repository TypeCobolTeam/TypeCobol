using System;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Given a Variable symbol or a Type this class renumber all level except levels
    /// 66, 88 and 77.
    /// Parameters is the start level.
    /// </summary>
    public class LevelRenumber : AbstractSymbolAndTypeVisitor<int, int>
    {
        #region Symbols renumber

        public override int VisitSymbol(Symbol symbol, int currentLevel)
        {
            return currentLevel;
        }

        public override int VisitVariableSymbol(VariableSymbol variableSymbol, int currentLevel)
        {
            System.Diagnostics.Debug.Assert(currentLevel > 0);
            if (currentLevel <= 49)
            {
                if (variableSymbol.Level != 66 && variableSymbol.Level != 77 && variableSymbol.Level != 88)
                {
                    variableSymbol.Level = currentLevel;
                }
                return variableSymbol.Type?.Accept(this, currentLevel) ?? currentLevel;
            }

            throw new Symbol.LevelExceed(variableSymbol);
        }

        public override int VisitVariableTypeSymbol(VariableTypeSymbol variableTypeSymbol, int currentLevel)
        {
            return VisitVariableSymbol(variableTypeSymbol, currentLevel);
        }
        
        #endregion

        #region Types renumber

        public override int VisitType(Types.Type type, int currentLevel)
        {
            return currentLevel;
        }

        public override int VisitArrayType(ArrayType arrayType, int currentLevel)
        {
            return arrayType.ElementType?.Accept(this, currentLevel) ?? currentLevel;
        }

        public override int VisitPointerType(PointerType pointerType, int currentLevel)
        {
            return pointerType.ElementType?.Accept(this, currentLevel) ?? currentLevel;
        }

        public override int VisitGroupType(GroupType groupType, int currentLevel)
        {
            int maxLevel = currentLevel;
            foreach (var field in groupType.Scope)
            {
                maxLevel = Math.Max(maxLevel, field.Accept(this, currentLevel + 1));
            }
            return maxLevel;
        }

        public override int VisitTypedefType(TypedefType typedefType, int currentLevel)
        {
            return typedefType.TypeComponent?.Accept(this, currentLevel) ?? currentLevel;
        }

        #endregion
    }
}
