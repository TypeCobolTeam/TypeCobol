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
            string[] lines = { "COMMIT" };
            var actual = ScannerUtils.ScanSqlLines(lines);
            CheckWithResultFile(actual, testName);
        }
    }
}
