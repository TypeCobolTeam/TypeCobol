#nullable enable

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Intrinsic function names defined by the Cobol language.
    /// </summary>
    public enum FunctionNameEnum
    {
        ABS, ACOS, ANNUITY, ASIN, ATAN,
        BIT_OF, BIT_TO_CHAR, BYTE_LENGTH,
        CHAR, COMBINED_DATETIME, COS, CURRENT_DATE,
        DATE_OF_INTEGER, DATE_TO_YYYYMMDD, DAY_OF_INTEGER, DAY_TO_YYYYDDD, DISPLAY_OF,
        E, EXP, EXP10,
        FACTORIAL, FORMATTED_CURRENT_DATE, FORMATTED_DATE, FORMATTED_DATETIME, FORMATTED_TIME,
        HEX_OF, HEX_TO_CHAR,
        INTEGER, INTEGER_OF_DATE, INTEGER_OF_DAY, INTEGER_OF_FORMATTED_DATE, INTEGER_PART,
        LENGTH, LOG, LOG10, LOWER_CASE,
        MAX, MEAN, MEDIAN, MIDRANGE, MIN, MOD,
        NATIONAL_OF, NUMVAL, NUMVAL_C, NUMVAL_F,
        ORD, ORD_MAX, ORD_MIN,
        PI, PRESENT_VALUE,
        RANDOM, RANGE, REM, REVERSE,
        SECONDS_FROM_FORMATTED_TIME, SECONDS_PAST_MIDNIGHT, SIGN, SIN, SQRT, STANDARD_DEVIATION, SUM,
        TAN, TEST_DATE_YYYYMMDD, TEST_DAY_YYYYDDD, TEST_FORMATTED_DATETIME, TEST_NUMVAL, TEST_NUMVAL_C, TEST_NUMVAL_F, TRIM,
        ULENGTH, UPOS, UPPER_CASE, USUBSTR, USUPPLEMENTARY, UUID4, UVALID, UWIDTH,
        VARIANCE,
        WHEN_COMPILED,
        YEAR_TO_YYYY
    }

    /// <summary>
    /// Scanner utility for intrinsic function names.
    /// </summary>
    internal static class CobolIntrinsicFunctions
    {
        private static readonly HashSet<string> _Names = Enum.GetNames<FunctionNameEnum>()
            .Select(name => name.Replace("_", "-"))
            .ToHashSet(StringComparer.OrdinalIgnoreCase);

        // Intrinsic functions which are not allowed in the REPOSITORY paragraph
        private static readonly HashSet<string> _NotAllowedIntrinsicFunctions = new(StringComparer.OrdinalIgnoreCase) { "WHEN-COMPILED" };

        public static IEnumerable<string> FunctionNames => _Names;

        public static bool IsFunctionName(string name) => _Names.Contains(name);

        public static bool IsAllowedInRepositoryParagraph(string functionName) => !_NotAllowedIntrinsicFunctions.Contains(functionName);
    }
}
