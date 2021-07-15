namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// Regroup sign and symbol for a custom CURRENCY SIGN clause
        /// </summary>
        public class CurrencyDescriptor
        {
            //Chars forbidden for a currency symbol
            private const string FORBIDDEN_CHARS = "0123456789ABCDEGNPRSVXZabcdegnprsvxz +-,.*/;()\"=’";

            /// <summary>
            /// Default Currency descriptor using the '$' symbol.
            /// NOTE: on z/OS, The default currency symbol is the character assigned the value X'5B' in the code page in effect at compile time.
            /// </summary>
            public static readonly CurrencyDescriptor Default = new CurrencyDescriptor('$', "$");

            /// <summary>
            /// Checks a string to see whether it is suitable as a currency symbol or not
            /// </summary>
            /// <param name="symbolText">Text to validate</param>
            /// <param name="error">out, error message when validation fails</param>
            /// <returns>True if text validation succeeds, False otherwise</returns>
            public static bool ValidateSymbol(string symbolText, out string error)
            {
                if (string.IsNullOrEmpty(symbolText))
                {
                    error = "Empty string cannot be used as currency symbol.";
                    return false;
                }

                if (symbolText.Length > 1)
                {
                    error = "Currency symbol must be one single character.";
                    return false;
                }

                char symbol = symbolText[0];
                foreach (var forbiddenChar in FORBIDDEN_CHARS)
                {
                    if (symbol == forbiddenChar)
                    {
                        error = $"Character '{forbiddenChar}' is not allowed as currency symbol.";
                        return false;
                    }
                }

                error = null;
                return true;
            }

            public CurrencyDescriptor(char symbol, string sign)
            {
                Symbol = symbol;
                Sign = sign;
            }

            public char Symbol { get; }

            public string Sign { get; }
        }
    }
}
