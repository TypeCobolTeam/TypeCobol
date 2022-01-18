using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Test.Parser.Preprocessor
{
    static class TestReplaceDirective
    {
        private static readonly CompilationProject _ReplaceProject = new CompilationProject(
            "replace",
            PlatformUtils.GetPathForProjectFile(PreprocessorUtils.Root + Path.DirectorySeparatorChar + "ReplaceTestFiles"),
            new [] { ".cbl", ".cpy" },
            PreprocessorUtils.Format,
            new TypeCobolOptions(),
            null);

        public static void CheckReplaceSingle()
        {
            string testName = "PgmReplaceSingle";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplacePartial(bool cobol)
        {
            string testName = "PgmReplacePartial";
            _ReplaceProject.CompilationOptions.IsCobolLanguage = cobol;
            _ReplaceProject.ClearImportedCompilationDocumentsCache();
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            _ReplaceProject.CompilationOptions.IsCobolLanguage = false;
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName + (cobol ? string.Empty : "TC"));
        }

        public static void CheckReplaceSingleToMultiple()
        {
            string testName = "PgmReplaceSingleToMultiple";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceMultiple()
        {
            string testName = "PgmReplaceMultiple";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceNested()
        {
            string testName = "PgmReplaceNested";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckReplaceFunction()
        {
            string testName = "PgmReplaceFunction";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckEmptyPartialWordReplace()
        {
            string testName = "PgmEmptyPartialWordReplace";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }

        public static void CheckEmptyPartialWordReplace2()
        {
            string testName = "PgmEmptyPartialWordReplace2";
            string result = PreprocessorUtils.ProcessReplaceDirectives(_ReplaceProject, testName);
            PreprocessorUtils.CheckWithReplaceResultFile(result, testName);
        }
    }
}
