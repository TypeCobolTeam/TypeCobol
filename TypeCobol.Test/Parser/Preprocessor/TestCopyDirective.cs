using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Test.Parser.Preprocessor
{
    static class TestCopyDirective
    {
        private static readonly CompilationProject _CopyProject = new CompilationProject(
            "copy",
            PlatformUtils.GetPathForProjectFile(PreprocessorUtils.Root + Path.DirectorySeparatorChar + "CopyTestFiles"),
            new [] { ".cbl", ".cpy" },
            PreprocessorUtils.Format,
            new TypeCobolOptions(),
            null);

        public static void CheckCopy()
        {
            string testName = "PgmCopy";
            string result = PreprocessorUtils.ProcessCopyDirectives(_CopyProject, testName);
            PreprocessorUtils.CheckWithCopyResultFile(result, testName);
        }

        public static void CheckCopyReplacing()
        {
            string testName = "PgmCopyReplacing";
            string result = PreprocessorUtils.ProcessCopyDirectives(_CopyProject, testName);
            PreprocessorUtils.CheckWithCopyResultFile(result, testName);
        }
    }
}
