using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Test.Compiler.Preprocessor
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
