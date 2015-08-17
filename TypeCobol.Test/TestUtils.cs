using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace TypeCobol.Test
{
    class TestUtils
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

            for (int c = 0; c < resultLines.Length && c < expectedResultLines.Length; c++)
            {
                if (expectedResultLines[c] != resultLines[c])
                {
                    errors.AppendLine("Line " + c + ": result=\"" + resultLines[c] + "\" vs expected=\"" + expectedResultLines[c] + "\"");
                }
            }

            if (expectedResultLines.Length != resultLines.Length)
            {
                errors.AppendLine("result lines=" + resultLines.Length + "; lines expected=" + expectedResultLines.Length);
            }

            if (errors.Length > 0)
            {
                errors.Insert(0, "In test:" + testName + "\n");
                errors.Append("=== RESULT ==========\n" + result + "====================");
                throw new Exception(errors.ToString());
            }
        }
    }
}
