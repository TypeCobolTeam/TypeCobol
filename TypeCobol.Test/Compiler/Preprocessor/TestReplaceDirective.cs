using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Test.Compiler.Preprocessor
{
    static class TestReplaceDirective
    {
        public static void CheckReplaceSingle()
        {
            string testName = "PgmReplaceSingle";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplacePartial()
        {
            string testName = "PgmReplacePartial";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceSingleToMultiple()
        {
            string testName = "PgmReplaceSingleToMultiple";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceMultiple()
        {
            string testName = "PgmReplaceMultiple";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceNested()
        {
            string testName = "PgmReplaceNested";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceFunction()
        {
            string testName = "PgmReplaceFunction";
            string result = PreprocessorUtils.ProcessReplaceDirectives(testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }
    }
}
