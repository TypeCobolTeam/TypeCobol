using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Scanner;

namespace TypeCobol.Test.Compiler.Preprocessor
{
    static class TestCompilerDirectiveBuilder
    {
        public static void CheckBASIS()
        {
            string testName = "Basis";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckCBL_PROCESS()
        {
            string testName = "CblProcess";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckASTERISK_CONTROL_CBL()
        {
            string testName = "ControlCbl";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckCOPY()
        {
            string testName = "Copy";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckDELETE()
        {
            string testName = "Delete";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckEJECT()
        {
            string testName = "Eject";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckENTER()
        {
            string testName = "Enter";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckEXEC_SQL_INCLUDE()
        {
            string testName = "ExecSqlInclude";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckINSERT()
        {
            string testName = "Insert";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckREADY_RESET_TRACE()
        {
            string testName = "ReadyOrResetTrace";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckREPLACE()
        {
            string testName = "Replace";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckSERVICE_LABEL()
        {
            string testName = "ServiceLabel";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckSERVICE_RELOAD()
        {
            string testName = "ServiceReload";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckSKIP1_2_3()
        {
            string testName = "Skip";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckTITLE()
        {
            string testName = "Title";
            string result = PreprocessorUtils.ProcessCompilerDirectives(testName);
            PreprocessorUtils.CheckWithDirectiveResultFile(result, testName);
        }

        public static void CheckRealFiles()
        {
        }
    }
}
