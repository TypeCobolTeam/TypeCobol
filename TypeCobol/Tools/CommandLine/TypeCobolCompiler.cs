using System;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.Tools.APIHelpers;

namespace TypeCobol.Tools.CommandLine
{
    /// <summary>
    /// TypeCobol compiler command line tool
    /// </summary>
    public static class TypeCobolCompiler
    {
        public static void Main(string[] args)
        {
            // Basic test program, useful to debug : compiles all sample programs located under TypeCobol.Test\Samples\EI Cobol samples\EI-Production"

            string currentDirectory = Directory.GetCurrentDirectory();
            string projectRootPath = currentDirectory.Substring(0, currentDirectory.IndexOf(@"\TypeCobol\") + 11);

            string sourcePath = projectRootPath + @"TypeCobol.Test\Samples\EI Cobol samples\EI-Production";
            string[] programExtensions = { ".PGM" };

            DocumentFormat docFormat = new DocumentFormat(Encoding.GetEncoding("iso8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);

            TypeCobolOptions compilerOptions = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("samples", sourcePath, programExtensions.Concat(Helpers.DEFAULT_COPY_EXTENSIONS).ToArray(),
                docFormat, compilerOptions, null);
            
            // Iterate over all programs in the source directory
            foreach (string programExtension in programExtensions)
            {
                foreach (string filePath in Directory.EnumerateFiles(sourcePath, "*" + programExtension))
                {
                    // Compile program
                    string textName = Path.GetFileNameWithoutExtension(filePath);
                    Console.Write(textName + " ... ");
                    try
                    {
                        FileCompiler fileCompiler = new FileCompiler(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone(), null, false, project);
                        fileCompiler.CompileOnce();
                        Console.WriteLine(" OK");
                    }
                    catch(Exception e)
                    {
                        Console.WriteLine("error :");
                        Console.WriteLine(e.Message);
                    }
                }                
            }

            /*
            // TO DO : read compiler options on the command line
            // Start with the default compiler options
            TypeCobolOptions compilerOptions = new TypeCobolOptions();

            // Simple test version
            // - all referenced files should be located under the current directory
            
            CompilationProject project = new CompilationProject("project", ".", new string[] { ".cbl", ".cpy" },
                IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat, compilerOptions);

            // - gets one file name as argument and compiles it
            if (args.Length == 1)
            {
                string textName = args[0];

                FileCompiler fileCompiler = new FileCompiler(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone(), false);
                fileCompiler.CompileOnce();
            }
            // - gets an optional "-continuous" flag as the first argument to activate source file monitoring and automatic recompilation
            else if (args.Length == 2)
            {
                if (String.Equals(args[0], "-continuous", StringComparison.InvariantCultureIgnoreCase))
                {
                    string textName = args[1];

                    FileCompiler fileCompiler = new FileCompiler(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone(), false);
                    fileCompiler.StartContinuousBackgroundCompilation(400, 400, 900, 2000);

                    Console.WriteLine("Processing, press enter to stop ...");
                    fileCompiler.StartContinuousFileProcessing();

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
            */
        }
    }
}
