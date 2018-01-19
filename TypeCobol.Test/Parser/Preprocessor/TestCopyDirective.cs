namespace TypeCobol.Test.Parser.Preprocessor
{
    static class TestCopyDirective
    {
        public static void CheckCopy()
        {
            string testName = "PgmCopy";
            string result = PreprocessorUtils.ProcessCopyDirectives(testName);
            PreprocessorUtils.CheckWithCopyResultFile(result, testName);
        }

        public static void CheckCopyReplacing()
        {
            string testName = "PgmCopyReplacing";
            string result = PreprocessorUtils.ProcessCopyDirectives(testName);
            PreprocessorUtils.CheckWithCopyResultFile(result, testName);
        }
    }
}
