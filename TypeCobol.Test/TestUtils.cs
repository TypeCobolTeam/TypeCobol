using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using DiffPlex;
using DiffPlex.DiffBuilder;
using DiffPlex.DiffBuilder.Model;

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
            var linefaults = new List<int?>();

            var d = new Differ();
            var inlineBuilder = new InlineDiffBuilder(d);
            var result1 = inlineBuilder.BuildDiffModel(result, expectedResult);

            foreach (var line in result1.Lines)
            {
                if (line.Type == ChangeType.Deleted)
                    errors.Append(line.Text + "\n");
                else if (line.Type == ChangeType.Inserted)
                    linefaults.Add(line.Position);
            }

            if (errors.Length > 0)
            {
                StringBuilder errorMessage = new StringBuilder();
                errorMessage.Append("result != expectedResult  In test:" + testName)
                          .AppendLine(" at line" + (linefaults.Count > 1 ? "s" : "") + ": " + string.Join(",", linefaults));

                errorMessage.Append(errors);
                throw new Exception(errorMessage.ToString());
            }
        }
    }
}
