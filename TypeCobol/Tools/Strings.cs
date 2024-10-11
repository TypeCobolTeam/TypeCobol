using System.Linq;

namespace TypeCobol.Tools
{
    public static class Strings
    {
        /// <summary>
        /// Indicates whether a specified string is empty, or consists only of numbers.
        /// </summary>
        /// <param name="text">The string to check</param>
        /// <returns>True if text is only made of numbers or empty, False otherwise</returns>
        public static bool IsNumeric(this string text)
        {
            return text.All(char.IsNumber);
        }

        /// <summary>
        /// Returns the indent part of a given line.
        /// </summary>
        /// <param name="text">The string to examine</param>
        /// <returns>Leading whitespace of the give string</returns>
        public static string GetIndent(this string text)
        {
            return new string(text.TakeWhile(char.IsWhiteSpace).ToArray());
        }

        /// <summary>
        /// Escape the string.Format placeholders (i.e. braces) in the given string.
        /// </summary>
        /// <param name="text">The string to be escaped</param>
        /// <returns>The given string with escaped string.Format placeholders</returns>
        public static string EscapeStringFormatPlaceHolders(this string text)
        {
            return text?.Replace("{", "{{").Replace("}", "}}");
        }
    }
}
