using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace TypeCobol.Test
{
    public class TestUtils
    {
        /// <summary>
        /// Compare result and expectedResult line by line.
        /// If there is at least one difference, throw an exception for the test named by the parameter testName
        /// </summary>
        /// <param name="testName">Name of the test</param>
        /// <param name="result"></param>
        /// <param name="expectedResult"></param>
        /// <returns></returns>
        public static void compareLines(string testName, string result, string expectedResult)
        {
            StringBuilder errors = new StringBuilder();

            result = Regex.Replace(result, "(?<!\r)\n", "\r\n");
            expectedResult = Regex.Replace(expectedResult, "(?<!\r)\n", "\r\n");

            String[] expectedResultLines = expectedResult.Split('\r', '\n' );
            String[] resultLines = result.Split('\r', '\n');

            var linefaults = new List<int>();
            for (int c = 0; c < resultLines.Length && c < expectedResultLines.Length; c++) {
                if (expectedResultLines[c] != resultLines[c]) linefaults.Add(c/2+1);
            }

            if (result != expectedResult)
            {
                errors.Append("result != expectedResult  In test:" + testName)
                      .AppendLine(" at line"+(linefaults.Count>1?"s":"")+": "+string.Join(",", linefaults));
                errors.Append("=== RESULT ==========\n" + result + "====================");
                throw new Exception(errors.ToString());
            }
        }
    }
}
