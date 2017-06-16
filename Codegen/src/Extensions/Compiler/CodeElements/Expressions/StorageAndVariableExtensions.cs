using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Codegen.Extensions.Compiler.CodeElements.Expressions
{
    static class StorageAreaExtension
    {
        /// <summary>
        /// Produce Cobol85 code which correspond of this StorageArea
        /// </summary>
        /// <param name="sa"></param>
        /// <returns></returns>
        public static string ToCobol85(this StorageArea sa) {
            StorageAreaPropertySpecialRegister specialRegister = sa as StorageAreaPropertySpecialRegister;
            if (specialRegister != null) {
                return specialRegister.ToCobol85();
            }
            return sa.ToString();
        }

        public static string ToCobol85(this DataOrConditionStorageArea sa) {
            var str = new StringBuilder();
            str.Append(sa.SymbolReference);
            if (sa.Subscripts != null)
                foreach (var subscript in sa.Subscripts)
                    str.Append('(').Append(subscript).Append(')');
            return str.ToString();
        }


        public static string ToCobol85(this StorageAreaPropertySpecialRegister sa)
        {
            var str = new StringBuilder();
            if (sa.SpecialRegisterName != null)
            {
                if (sa.SpecialRegisterName.TokenType == TokenType.ADDRESS)
                {
                    str.Append("address of ");
                }
                else if (sa.SpecialRegisterName.TokenType == TokenType.LENGTH)
                {
                    str.Append("length of ");
                }
                else
                {
                    str.Append(sa.SpecialRegisterName.TokenType).Append(' ');
                }
            }
            if (sa.OtherStorageAreaReference != null) str.Append(sa.OtherStorageAreaReference);

            if (str.Length > 0) return str.ToString();
            return sa.ToString();
        }
        
    }

    static class VariableExtension {
        public static string ToCobol85(this Variable variable)
        {
            if (variable.NumericValue != null) return variable.NumericValue.Value.ToString();
            try
            {
                if (variable.SymbolReference != null)
                {
                    return variable.SymbolReference.ToString();
                }
                if (variable.StorageArea != null)
                {
                    return variable.StorageArea.ToCobol85();
                }
                //these should be: return XXXValue.GetValueInContext(???);
                if (variable.AlphanumericValue != null) return variable.AlphanumericValue.Token.SourceText;
                if (variable.RepeatedCharacterValue != null) return variable.RepeatedCharacterValue.Token.SourceText;
            }
            catch (System.InvalidOperationException) { }
            return variable.ToString();
        }

    }


}
