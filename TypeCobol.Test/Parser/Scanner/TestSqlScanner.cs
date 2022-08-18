using System.IO;

namespace TypeCobol.Test.Parser.Scanner
{
    static class TestSqlScanner
    {
        private static void CheckWithResultFile(string actual, string testName) =>
            ScannerUtils.CheckWithResultFile(actual, Path.Combine("SQL", testName));

        public static void CheckSqlConstants()
        {
            string testName = "CheckSqlConstants";
            string[] lines = {"-1","NULL", "025.50", "1228512541.34578E0222", "5.5E2", "SELECT 200","SELECT -10", "SELECT Test", "SELECT -NaN", "SELECT infinity", "SELECT GX'041'", "SELECT UX'004100420043'", "SELECT BX'1'" ,"DATE '10/08/1998' "};
            var actual = ScannerUtils.ScanSqlLines(lines, false);
            CheckWithResultFile(actual, testName);
        }

        public static void CheckSqlConstants2()
        {
            string testName = "CheckSqlConstants2";
            string[] lines = { "025,50", "1228512541,34578E0222", "5,5E2", "240.1" };
            var actual = ScannerUtils.ScanSqlLines(lines, true);
            CheckWithResultFile(actual, testName);
        }
    }
}
