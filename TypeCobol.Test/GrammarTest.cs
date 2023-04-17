using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Diagnostics;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Test.Utils;
using System.Text;

namespace TypeCobol.Test {

	[TestClass]
	public class GrammarTest {
	    [TestMethod]
	    [TestCategory("Parsing")]
	    [TestProperty("Time", "long")]
	    [Ignore] // Ignored, as everybody does not have a Samples folder. Remove this if you do have one.
	    public void CheckGrammarCorrectness() {
	        string regex = "*.PGM";
	        string samples = @"Samples";
	        string root = PlatformUtils.GetPathForProjectFile(samples);
	        CheckTests(root, @"GrammarTest", @"CheckGrammarResults.txt", regex);
        }

        private static void AppendTextToFiles(string tetxToAppend, params string[] files)
	    {
	        foreach (var file in files)
	        {
	            if (!string.IsNullOrEmpty(file))
	            {
	                File.AppendAllText(file, tetxToAppend);
	            }
	        }
	            
	    }

        public static void CheckTests(string rootFolder, string resultFolder, string timedResultFile,
            string regex = "*.cbl",
            string[] include = null,
            string[] exclude = null,
            string[] copiesFolder = null,
            bool codegen = true,
            int stopAfterAsManyErrors = 10000,
            TypeCobolOptions options = null,
            string expectedResultFile = null,
            bool ignoreWarningDiag = false)
        { 
            //Normalize optional arguments
            include = include ?? new string[0];
            exclude = exclude ?? new string[0];
            options = options ?? new TypeCobolOptions();
            options.ExecToStep = ExecutionStep.SemanticCrossCheck;
            options.IsCobolLanguage = true; //Designed for Cobol85 only because we expect generated code to be identical to input code

            string[] files = Directory.GetFiles(rootFolder, regex, SearchOption.AllDirectories);
            var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
	        string resultFile = "GeneratedResultFile.txt";

            //Initialize both files
            File.WriteAllText(timedResultFile, "");
	        if (expectedResultFile != null)
	            File.WriteAllText(resultFile, "");

            int tested = 0, nbFilesInError = 0, ignores = 0;
			TimeSpan parsingSumDuration = new TimeSpan(0);
			TimeSpan codeGenSumDuration = new TimeSpan(0);
			int parseErrors = 0;
			int codegenErrors = 0;
			int codegenDiff = 0;
			foreach (var file in files) {

				string filename = Path.GetFileName(file);
			    AppendTextToFiles((filename + ':'), timedResultFile, resultFile);

                bool ignore = include.Length > 0 && !include.Contains(filename);
				if (!ignore) ignore = exclude.Contains(filename);
				if (ignore) {
					ignores++;
					File.AppendAllText(timedResultFile, " ignored.\n");
                    continue;
				}
				string path = Path.Combine(rootFolder, filename);
				Stopwatch watch = new Stopwatch();
				watch.Start();
                var document = new TypeCobol.Parser();

                //We assume here that all sources are programs (not copies)
                document.Init(path, false, options, format, copiesFolder);
                document.Parse(path);
                

                watch.Stop();
				//TestJSONSerializer.DumpAsJSON(unit.CodeElementsDocumentSnapshot.CodeElements, filename);
				TimeSpan elapsed = watch.Elapsed;
				parsingSumDuration += elapsed;
				string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
			    AppendTextToFiles(" parsed in " + formatted + "\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForText.FirstCompilationTime + " ms : text update\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForScanner.FirstCompilationTime + " ms : scanner\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForPreprocessor.FirstCompilationTime + " ms : preprocessor\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForCodeElementsParser.FirstCompilationTime + " ms : code elements parser\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForTemporarySemantic.FirstCompilationTime + " ms : temporary semantic class parser\n", timedResultFile);
                AppendTextToFiles("- " + document.Results.PerfStatsForProgramCrossCheck.FirstCompilationTime + " ms : cross check class parser\n", timedResultFile);
                

                tested++;
                Console.WriteLine(filename);
                bool okay = true;

                var diagnostics = document.Results.AllDiagnostics();

                if (ignoreWarningDiag)
                {
                    diagnostics = diagnostics.Where(d => d.Info.Severity != Severity.Warning).ToList();
                }
			     
			    if (diagnostics.Any()) {
			        okay = false;
			        parseErrors += diagnostics.Count();
			    }
			    displayAndWriteErrorsToGrammarResult(diagnostics, timedResultFile, resultFile);

                
				if (!okay) {
                    AppendTextToFiles("\n", timedResultFile, resultFile);
                    nbFilesInError++;
					if (nbFilesInError >= stopAfterAsManyErrors) break;
				}

			    if (codegen && okay) {
                    watch.Reset();
			        watch.Start();

                    var writer = new StringWriter();
                    var generatedCobolStringBuilder = new StringBuilder();
			        var generator = new TypeCobol.Codegen.Generators.DefaultGenerator(document.Results, generatedCobolStringBuilder, null);
			        var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
			        generator.Generate(document.Results, columns);
                    writer.Write(generatedCobolStringBuilder);
                    writer.Close();

                    //Write duration to GrammarResultFile
                    watch.Stop();
                    elapsed = watch.Elapsed;
                    codeGenSumDuration += elapsed;
                    formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
                    AppendTextToFiles(" generated in " + formatted + "\n", timedResultFile);

                    var parsingDiags = generator.Diagnostics;

                    if (ignoreWarningDiag && parsingDiags != null)
                    {
                        parsingDiags = parsingDiags.Where(d => d.Info.Severity != Severity.Warning).ToList();
                    }
                    
                    //Error during generation, no need to check the content of generated Cobol
			        if (parsingDiags != null && parsingDiags.Any()) {

			            codegenErrors += parsingDiags.Count();
                        displayAndWriteErrorsToGrammarResult(parsingDiags, timedResultFile, resultFile);
                        nbFilesInError++;
                        if (nbFilesInError >= stopAfterAsManyErrors) break;

                    } else {
                        //Compare generated Cobol with expected
                        var expected = AsLines(File.ReadAllText(path, format.Encoding));
                        var actual = AsLines(writer.ToString());

                        Directory.CreateDirectory(resultFolder);


                        var linesKO = new List<int>();
                        for (int i = 0; i < Math.Min(expected.Count, actual.Count); i++)
                        {
                            if (!expected[i].Equals(actual[i]))
                                linesKO.Add(i);
                        }
                        var errors = new System.Text.StringBuilder();
                        string fmt = Lines2FormatString(Math.Max(expected.Count, actual.Count));
                        if (linesKO.Count > 0 || expected.Count != actual.Count)
                        {
                            errors.AppendLine("--- Lines mismatch ---");
                            File.WriteAllLines(
                            resultFolder + Path.DirectorySeparatorChar + Path.GetFileNameWithoutExtension(file) + "-Expected" +
                            Path.GetExtension(file), expected);
                            File.WriteAllLines(
                                resultFolder + Path.DirectorySeparatorChar + Path.GetFileNameWithoutExtension(file) + "-Actual" +
                                Path.GetExtension(file), actual);
                        }
                        int start = -1;
                        for (int i = 0; i < linesKO.Count; i++)
                        {
                            int currentline = linesKO[i];
                            bool follows = i > 0 && linesKO[i - 1] == currentline - 1;
                            if (!follows)
                            {
                                start = currentline;
                                before(errors, expected, currentline, 3, fmt);
                            }
                            bool preceeds = i + 1 < linesKO.Count && linesKO[i + 1] == currentline + 1;
                            if (!preceeds)
                            {
                                diff(errors, expected, actual, start, currentline, fmt);
                                after(errors, expected, currentline, 3, fmt);
                                start = -1;
                            }
                        }
                        for (int i = actual.Count; i < expected.Count; i++)
                            errors.AppendLine(String.Format("-{0:" + fmt + "} {1}", i, expected[i]));
                        for (int i = expected.Count; i < actual.Count; i++)
                            errors.AppendLine(String.Format("+{0:" + fmt + "} {1}", i, actual[i]));
                        if (errors.Length > 0)
                        {
                            codegenDiff += linesKO.Count + Math.Abs(actual.Count - expected.Count);
                            AppendTextToFiles(errors.ToString(), timedResultFile, resultFile);
                            if (okay) nbFilesInError++;
                        }
                    }
                }
			}
            TimeSpan totalTestDuration = parsingSumDuration + codeGenSumDuration;
            string parsingTotalDurationFormatted = String.Format("{0:00}m{1:00}s{2:000}ms", parsingSumDuration.Minutes, parsingSumDuration.Seconds, parsingSumDuration.Milliseconds);
            
			string message = "Files tested=" + tested + "/" + files.Length + ", files in error=" + nbFilesInError + ", ignored=" + ignores + "\n";
            AppendTextToFiles(message, resultFile);
            if (parseErrors > 0)   message += "Parsing errors: "+ parseErrors   + '\n';
			if (codegenErrors > 0) message += "Codegen errors: "+ codegenErrors + '\n';
			if (codegenDiff > 0) message += "Codegen Diff: "+ codegenDiff + '\n';
            message += "Parsing time: " + parsingTotalDurationFormatted;
            if (codegen) {
                string codeGenTotalDurationFormatted = string.Format("{0:00}m{1:00}s{2:000}ms", codeGenSumDuration.Minutes, codeGenSumDuration.Seconds, codeGenSumDuration.Milliseconds);
                string totalDurationFormatted = String.Format("{0:00}m{1:00}s{2:000}ms", totalTestDuration.Minutes, totalTestDuration.Seconds, totalTestDuration.Milliseconds);
                message += " + Codegen time: " + codeGenTotalDurationFormatted + "  => Total time: " + totalDurationFormatted;
            }
            AppendTextToFiles(message, timedResultFile);
			if (nbFilesInError > 0 && expectedResultFile == null) Assert.Fail('\n'+message); //If no expectedFile to compare throw assert if error
            else if (expectedResultFile != null) //If expectedFileResult exists compare the DefaultGeneratedFile with expectedFile
			{
                StreamReader expectedResultReader = new StreamReader(new FileStream(expectedResultFile, FileMode.Open));
                StreamReader actualResultReader = new StreamReader(new FileStream(resultFile, FileMode.Open));
                TestUtils.compareLines("GrammarTestCompareFiles", expectedResultReader.ReadToEnd(), actualResultReader.ReadToEnd(), expectedResultFile); //The test will fail if result files are different

                expectedResultReader.Close();
                actualResultReader.Close();
            }

		}

        private static void displayAndWriteErrorsToGrammarResult(IEnumerable<Diagnostic> diagnostics, params string[] files) {
			string result = ParserUtils.DiagnosticsToString(diagnostics, false);
			Console.WriteLine(result);
			AppendTextToFiles((result + "\n"), files);
		}

		private static List<string> AsLines(string text) {
			return text.Replace("\r\n","\n").Replace("\r","\n").Split('\n').ToList();
		}
		private static string Lines2FormatString(int lines) {
			string res = "0";
			for (int i=1; i<lines.ToString().Length; i++) res += "0";
			return res;
		}
		/// <param name="output"></param>
		/// <param name="input">Input text</param>
		/// <param name="index">Line index</param>
		/// <param name="before">Number of line before line index</param>
		/// <param name="fmt"></param>
		private static void before(System.Text.StringBuilder output, List<string> input, int index, int before, string fmt="0") {
			for(int line=index-before; line<index; line++)
				if (line > 0) output.AppendLine(String.Format(" {0:"+fmt+"} {1}", line+1, input[line]));
		}
		private static void diff(System.Text.StringBuilder output, List<string> expected, List<string> actual, int start, int end, string fmt="0") {
			for (int i=start; i<=end; i++)
				output.AppendLine(String.Format("-{0:"+fmt+"} {1}", i+1, expected[i]));
			for (int i=start; i<=end; i++)
				output.AppendLine(String.Format("+{0:"+fmt+"} {1}", i+1, actual[i]));
		}
		/// <param name="output"></param>
		/// <param name="input">Input text</param>
		/// <param name="index">Line index</param>
		/// <param name="after">Number of line after line index</param>
		/// <param name="fmt"></param>
		private static void after(System.Text.StringBuilder output, List<string> input, int index, int after, string fmt="0") {
			for(int line=index+1; line<=index+after; line++)
				if (line < input.Count) output.AppendLine(String.Format(" {0:"+fmt+"} {1}", line+1, input[line]));
		}
	}
}
