using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Parser.Scanner
{
    static class TestRealPrograms
    {
        private static readonly string ParserScannerSamples = @"Parser\Scanner\Samples";


        public static void CheckAllFilesForExceptions()
        {
            var documentFormat = new DocumentFormat(IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat);
            CompilationProject project = new CompilationProject("test", 
                PlatformUtils.GetPathForProjectFile(ParserScannerSamples), new string[] { ".txt" },
                documentFormat, new TypeCobolOptions(), null);

            int filesCount = 0;
            //int linesCount = 0;
            //Stopwatch chrono = new Stopwatch();
            foreach (string fileName in PlatformUtils.ListFilesInSubdirectory(ParserScannerSamples))
            {
                string textName = Path.GetFileNameWithoutExtension(fileName);
#if SQL_PARSING
                if (textName == "RealPGM2")
                {
                    continue;
                }
#endif
                // Initialize a CompilationDocument
                FileCompiler compiler = new FileCompiler(null, textName, ColumnsLayout.CobolReferenceFormat, false, project.SourceFileProvider, project, new TypeCobolOptions(), null, project);

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
                filesCount++;
                //linesCount += compiler.CompilationResultsForCopy.TokensDocumentSnapshot.Lines.Count;
                //string result = compiler.CompilationResultsForCopy.TokensDocumentSnapshot.GetDebugString();
            }
            Console.WriteLine("Nb of tests " + filesCount);
            Assert.IsTrue(filesCount > 0, "No tests found in " + ParserScannerSamples);
            // throw new Exception("Test OK for " + filesCount + " files and " + linesCount + " lines : " + chrono.ElapsedMilliseconds + " ms");
        }
    }
}
