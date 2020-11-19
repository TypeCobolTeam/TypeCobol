using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServices.CodeAnalysis.Statistics;

namespace TypeCobol.Tools.CommandLine
{
    /// <summary>
    /// Compute stats for all Cobol syntax constructs over a set of files in a directory
    /// </summary>
    public static class TypeCobolStats
    {
//        public static void Main(string[] args)
        public static void main(string[] args)
        {
            // Directory where the source programs and COPY files are stored
            string sourcePath = @"D:\Users\Laurent\OneDrive\Dev\Visual Studio 2012\Projects\TypeCobol\TypeCobol.Test\Samples\EI Cobol samples\EI-Production";
            string[] programExtensions = { ".PGM" };
            string[] copyExtensions = { ".CPY" };

            // List of all sample programs used to compute the statistics
            IList<string> textNames = new List<string>();
            foreach (string programExtension in programExtensions)
            {
                foreach (string filePath in Directory.EnumerateFiles(sourcePath, "*" + programExtension))
                {
                    string textName = Path.GetFileNameWithoutExtension(filePath);
                    textNames.Add(textName);
                }
            }

            // Source file format for the samples
            DocumentFormat docFormat = new DocumentFormat(Encoding.GetEncoding("iso8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);

            // Initialize a compilation project
            TypeCobolOptions compilerOptions = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("samples", sourcePath, programExtensions.Concat(copyExtensions).ToArray(),
                docFormat, compilerOptions, null);

            // Output files used to store the results
            string resultFilePath = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
            string countersFile = Path.Combine(resultFilePath, "SyntaxCounters.txt");
            string languageModelForProgramFile = Path.Combine(resultFilePath, "LanguageModel.Program.txt");
            string languageModelForCopyFile = Path.Combine(resultFilePath, "LanguageModel.Copy.txt");

            // Compute statistics
            Stopwatch chrono = new Stopwatch();
            chrono.Start();
            StatsGenerator.GenerateStatisticsForPrograms(project, textNames, Console.Out, countersFile, languageModelForProgramFile, languageModelForCopyFile);
            chrono.Stop();
            Console.WriteLine("");
            Console.WriteLine("Programs analyzed in " + Math.Round(chrono.ElapsedMilliseconds / (double)1000, 3) + " sec");
        }
    }
}
