using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Test.Parser.Preprocessor
{
    static class TestSingleLineCompilerDirectiveBuilder
    {
        public static void CheckCALLINTERFACE()
        {
            string testName = "CallInterface";
            string result = PreprocessorUtils.ProcessSingleLineCompilerDirectives(testName);
            PreprocessorUtils.CheckWithSingleLineDirectiveResultFile(result, testName);
        }

    }
}
