using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Scanner
{
    static class TestRealPrograms
    {
        public static void CheckAllFilesForExceptions()
        {
            CompilationProject project = new CompilationProject("test", 
                PlatformUtils.GetPathForProjectFile(@"Compiler\Scanner\Samples"), new string[] { "*.txt" },
                IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat, new TypeCobolOptions());

            //int filesCount = 0;
            //int linesCount = 0;
            //Stopwatch chrono = new Stopwatch();
            foreach (string fileName in PlatformUtils.ListFilesInSubdirectory(@"Compiler\Scanner\Samples"))
            {
                string textName = Path.GetFileNameWithoutExtension(fileName);

                // Initialize a CompilationDocument
                CompilationDocument compilationDocument = new CompilationDocument(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, new TypeCobolOptions());
                // Setup a synchronous compilation pipeline
                compilationDocument.SetupDocumentProcessingPipeline(null, 0);

                // Start compilation
                //chrono.Start();
               compilationDocument.StartDocumentProcessing();
                //chrono.Stop();

                // Stats and errors
                //filesCount++;
                //linesCount += compilationDocument.TokensDocument.TokensLines.Count;
                if (compilationDocument.TokensDocument.LastException != null)
                {
                    throw new Exception("Error while scanning file " + fileName, compilationDocument.TokensDocument.LastException);
                }
                //string result = compilationDocument.TokensDocument.GetDebugString();
            }
            // throw new Exception("Test OK for " + filesCount + " files and " + linesCount + " lines : " + chrono.ElapsedMilliseconds + " ms");
        }
    }
}
