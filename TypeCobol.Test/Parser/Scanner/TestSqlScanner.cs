using System.IO;

namespace TypeCobol.Test.Parser.Scanner
{
    static class TestSqlScanner
    {
        private static void CheckWithResultFile(string actual, string testName) =>
            ScannerUtils.CheckWithResultFile(actual, Path.Combine("SQL", testName));

        public static void Demo_CheckSql()
        {
            string testName = "Demo_CheckSql";
            string[] lines = { "SELECT GX'0041'", "SELECT UX'004100420043'", "SELECT -NaN", "SELECT infinity", "SELECT -10", "SELECT Test", "SELECT BX'0000'" };
            var actual = ScannerUtils.ScanSqlLines(lines);
            CheckWithResultFile(actual, testName);
        }
    }
}
