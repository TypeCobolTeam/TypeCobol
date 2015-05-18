using System;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Tools.CommandLine
{
    /// <summary>
    /// TypeCobol compiler command line tool
    /// </summary>
    public static class TypeCobolCompiler
    {
        public static void Main(string[] args)
        {
            // TO DO : read compiler options on the command line
            // Start with the default compiler options
            TypeCobolOptions compilerOptions = new TypeCobolOptions();

            // Simple test version
            // - all referenced files should be located under the current directory

            CompilationProject project = new CompilationProject("project", ".", new string[] { "*.cbl", "*.cpy" },
                IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat, compilerOptions);

            // - gets one file name as argument and compiles it
            if (args.Length == 1)
            {
                string textName = args[0];

                CompilationUnit compilationUnit = new CompilationUnit(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone());
                compilationUnit.SetupTextGenerationPipeline(null, 0, null);
                compilationUnit.StartDocumentProcessing();
            }
            // - gets an optional "-continuous" flag as the first argument to activate source file monitoring and automatic recompilation
            else if (args.Length == 2)
            {
                if (String.Equals(args[0], "-continuous", StringComparison.InvariantCultureIgnoreCase))
                {
                    string textName = args[1];

                    CompilationUnit compilationUnit = new CompilationUnit(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone());
                    compilationUnit.SetupTextGenerationPipeline(null, 0, null);

                    Console.WriteLine("Processing, press enter to stop ...");
                    compilationUnit.StartContinuousFileProcessing();

                    Console.ReadLine();
                }
                else
                {
                    Console.WriteLine("ERROR : Invalid option");
                }
            }
            else
            {
                Console.WriteLine("ERROR : Invalid number of arguments");
            }
        }
    }
}
