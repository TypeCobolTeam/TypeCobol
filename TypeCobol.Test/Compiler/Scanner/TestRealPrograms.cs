using System;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Scanner
{
    static class TestRealPrograms
    {
        public static void CheckAllFilesForExceptions()
        {
            CompilationProject project = new CompilationProject("test", 
                PlatformUtils.GetPathForProjectFile(@"Compiler\Scanner\Samples"), new string[] { ".txt" },
                IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat, new TypeCobolOptions());

            //int filesCount = 0;
            //int linesCount = 0;
            //Stopwatch chrono = new Stopwatch();
            foreach (string fileName in PlatformUtils.ListFilesInSubdirectory(@"Compiler\Scanner\Samples"))
            {
                string textName = Path.GetFileNameWithoutExtension(fileName);

                // Initialize a CompilationDocument
                FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, new TypeCobolOptions(), null, true, project);

                // Start compilation
                try
                {
                    //chrono.Start();
                    compiler.CompileOnce();
                    //chrono.Stop();
                }
                catch(Exception e)
                {
                    throw new Exception("Error while scanning file " + fileName, e);
                }

                // Stats
                //filesCount++;
                //linesCount += compiler.CompilationResultsForCopy.TokensDocumentSnapshot.Lines.Count;
                //string result = compiler.CompilationResultsForCopy.TokensDocumentSnapshot.GetDebugString();
            }
            // throw new Exception("Test OK for " + filesCount + " files and " + linesCount + " lines : " + chrono.ElapsedMilliseconds + " ms");
        }
    }
}
