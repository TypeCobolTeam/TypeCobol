using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Codegen.Generators;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// This class uses files from TypeCobol.Test\ThirdParty to test performance of Codegen.
    /// </summary>
    [TestClass]
    public class Performance
    {
        private const int ITERATION_COUNT = 20;

        private static string GetThirdParty()
        {
            return Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), @"..\..\TypeCobol.Test\ThirdParty"));
        }

        private static string GetCNAFBatch()
        {
            return Path.GetFullPath(Path.Combine(GetThirdParty(), @"CNAF\Batch"));
        }

        private static string GetCNAFTypeCobol()
        {
            return Path.GetFullPath(Path.Combine(GetThirdParty(), @"CNAF_TypeCobol"));
        }

        // Paths to sample files.
        private static readonly string _Batch1 = Path.Combine(GetCNAFBatch(), "CGM110.cbl");
        private static readonly string _Batch2 = Path.Combine(GetCNAFBatch(), "CGM129.COB");
        private static readonly string _Batch3 = Path.Combine(GetCNAFBatch(), "CGMV01.COB");
        private static readonly string _Batch4 = Path.Combine(GetCNAFBatch(), "CGMX02.COB");
        private static readonly string _Batch5 = Path.Combine(GetCNAFBatch(), "CGS100.COB");
        private static readonly string _Cobol85_NoRedefines = Path.Combine(GetCNAFTypeCobol(), "CGMV01-Cobol85-NoRedefines.cbl");
        private static readonly string _BigTypes_NoProcedure = Path.Combine(GetCNAFTypeCobol(), "CGMV01-BigTypes.tcbl");
        private static readonly string _BigTypes_1Procedure = Path.Combine(GetCNAFTypeCobol(), "CGMV01-BigTypes-1Procedure.tcbl");
        private static readonly string _GlobalStorage = Path.Combine(GetCNAFTypeCobol(), "CGMV01-GlobalStoragey.tcbl");
        private static readonly string _UseALotOfTypes_1Times_Reference = Path.Combine(GetCNAFTypeCobol(), "CGMV01-UseAlotOfTypes-1Times.tcbl");
        private static readonly string _UseALotOfTypes_100Times = Path.Combine(GetCNAFTypeCobol(), "CGMV01-UseAlotOfTypes-100Times.tcbl");
        private static readonly string _UseALotOfTypes_WithProc_100Times = Path.Combine(GetCNAFTypeCobol(), "CGMV01-UseAlotOfTypes-WithProc-100Times.tcbl");
        private static readonly string _DeepVariables = Path.Combine(GetCNAFTypeCobol(), "CGMV01-DeepVariables.cbl");
        private static readonly string _DeepTypes = Path.Combine(GetCNAFTypeCobol(), "CGMV01-DeepTypes.tcbl");

        private static void Test(string path, bool forceFullParsing = false)
        {
            string fileName = Path.GetFileName(path);

            CompilationProject compilationProject = new CompilationProject(
                "codegen-performance-test",
                Path.GetDirectoryName(path),
                new[] { ".tcbl", ".cbl", ".cpy" },
                DocumentFormat.RDZReferenceFormat,
                new TypeCobolOptions(),
                null);
            FileCompiler fileCompiler = new FileCompiler(compilationProject, fileName, false);
            fileCompiler.CompileOnce();

            // Generator.
            StringBuilder destination = new StringBuilder();
            var columnsLayout = fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
            IGenerator generator = new DefaultGenerator(fileCompiler.CompilationResultsForProgram, destination, null);

            // Codegen iterations.
            Console.WriteLine("FILE;ITERATION;STEP;TIME_TAKEN");
            Dictionary<string, TimeSpan> total = new Dictionary<string, TimeSpan>();
            for (int i = 0; i < ITERATION_COUNT; i++)
            {
                // Force recompile (either full or incremental).
                if (forceFullParsing)
                {
                    // Full Compile.
                    fileCompiler = new FileCompiler(compilationProject, fileName, false);
                    fileCompiler.CompileOnce();
                }
                else
                {
                    // Incremental compile.
                    ITextLine newLine = new TextLineSnapshot(fileCompiler.CompilationResultsForProgram.CobolTextLines.Count, string.Empty, null);
                    TextChangedEvent textChangedEvent = new TextChangedEvent();
                    textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, newLine.LineIndex, newLine));
                    fileCompiler.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
                    fileCompiler.CompileOnce();
                }

                // Generate and write stats.
                generator.Generate(fileCompiler.CompilationResultsForProgram, columnsLayout);
                foreach (var pair in generator.PerformanceReport)
                {
                    Console.WriteLine($"{fileName};{i+1};{pair.Key};{pair.Value.TotalMilliseconds}");
                    if (!total.ContainsKey(pair.Key))
                    {
                        total.Add(pair.Key, TimeSpan.Zero);
                    }
                    total[pair.Key] += pair.Value;
                }
            }

            // Aggregated stats.
            Console.WriteLine();
            Console.WriteLine("Time per step :");
            var totalTime = total.Values.Sum(t => t.TotalMilliseconds);
            foreach (var stepData in total.Select((p, i) => new
                                                            {
                                                                Index = i + 1,
                                                                Step = p.Key,
                                                                Duration = p.Value.TotalMilliseconds,
                                                                Percentage = (100 * p.Value.TotalMilliseconds) / totalTime
                                                            }))
            {
                Console.WriteLine($"{stepData.Index}. {stepData.Step,-30} : Total = {stepData.Duration,15:0.00} ms, Avg = {stepData.Duration / ITERATION_COUNT,15:0.00} ms | {stepData.Percentage,3:0}%");
            }

            // Total time.
            Console.WriteLine();
            Console.WriteLine($"Global Codegen time : Total = {totalTime:0.00} ms, Avg = {totalTime / ITERATION_COUNT:0.00} ms");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Batch1()
        {
            Test(_Batch1);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Batch2()
        {
            Test(_Batch2);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Batch3()
        {
            Test(_Batch3);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Batch4()
        {
            Test(_Batch4);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Batch5()
        {
            Test(_Batch5);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_Cobol85_NoRedefines()
        {
            Test(_Cobol85_NoRedefines);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_BigTypes_NoProcedure()
        {
            Test(_BigTypes_NoProcedure);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_BigTypes_1Procedure()
        {
            Test(_BigTypes_1Procedure);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_GlobalStorage()
        {
            Test(_GlobalStorage);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_UseALotOfTypes_1Times_Reference()
        {
            Test(_UseALotOfTypes_1Times_Reference);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_UseALotOfTypes_100Times()
        {
            Test(_UseALotOfTypes_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_UseALotOfTypes_WithProc_100Times()
        {
            Test(_UseALotOfTypes_WithProc_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_DeepVariables()
        {
            Test(_DeepVariables);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [Ignore]
        public void Test_DeepTypes()
        {
            Test(_DeepTypes);
        }
    }
}
