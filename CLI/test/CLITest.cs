using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Server;
using TypeCobol.Tools.Options_Config;

namespace CLI.Test
{
    [TestClass]
    public class CLITest {

        /// <summary>
        /// Try to perform successfully a parsing witout generation
        /// </summary>
        [TestMethod]
        public void TestParse_1() {
            CLITestHelper.Test("parse_1", ReturnCode.Success);   
        }

        /// <summary>
        /// Try to perform only a Scan of the input file
        /// </summary>
        [TestMethod]
        public void TestExecToStep_1() {
            CLITestHelper.Test("execToStep_1", ReturnCode.Success);
        }

        /// <summary>
        /// Perform a simple generation test with only needed options
        /// </summary>
        [TestMethod]
        public void TestGenerate_1() {
            CLITestHelper.Test("generate_1", ReturnCode.Success);
        }

        /// <summary>
        /// Perform a generation using transform to mix Cobol / TypeCobol
        /// </summary>
        [TestMethod]
        public void TestGenerate_MixedFiles() {
            CLITestHelper.Test("generate_mixedFiles", ReturnCode.Success);
        }

        /// <summary>
        /// Perform a generation with procedure generated as nested programs
        /// </summary>
        [TestMethod]
        public void TestGenerate_NestedProcedure()
        {
            CLITestHelper.Test("generate_nestedProc", ReturnCode.Success);
        }

        /// <summary>
        /// Test various case of usage of dependencies such as good usage, bad file, bad path.
        /// </summary>
        [TestMethod]
        public void TestDependencies() {
            CLITestHelper.Test("dependencies_1", ReturnCode.Success);
            CLITestHelper.Test("dependencies_2", ReturnCode.Success);
            CLITestHelper.Test("dependencies_3", ReturnCode.ParsingDiagnostics);
            CLITestHelper.ReadConsole("dependencies_4", ReturnCode.DependenciesError);            //No dependencies found
            CLITestHelper.Test("dependencies_5", ReturnCode.Success);
            CLITestHelper.Test("dependencies_6", ReturnCode.Success);
            CLITestHelper.Test("dependency_with_copy_loading", ReturnCode.Success);
#if EUROINFO_RULES
            CLITestHelper.Test("ei_dependencies_1", ReturnCode.ParsingDiagnostics);
#endif
        }

        /// <summary>
        /// Test various case of usage of dependencies that all ends with errors.
        /// The purpose is also to not have a too big TestDependencies method/
        /// </summary>
        [TestMethod]
        public void TestDependenciesWithErrors() {
            CLITestHelper.Test("dependencies_7_bad_call_proc", ReturnCode.ParsingDiagnostics);
        }

        /// <summary>
        /// Tests with a dependency that depends on a dependency which is loaded after itself.
        /// </summary>
        [TestMethod]
        public void TestDpendenciesNotLoadedInCorrectOrder() {
            CLITestHelper.Test("dependenciesNotLoadedInCorrectOrder", ReturnCode.ParsingDiagnostics);
            CLITestHelper.Test("dependenciesNotLoadedInCorrectOrder_2", ReturnCode.Success);
        }




        

            [TestMethod]
        public void TestCircularTypedef_1()
        {
            CLITestHelper.Test("CircularTypedef_1", ReturnCode.ParsingDiagnostics);
        }


        [TestMethod]
        public void TestEmptyDependency()
        {
            var testFolderName = "empty_dependency_folder";
            Directory.CreateDirectory("ressources" + Path.DirectorySeparatorChar + testFolderName + Path.DirectorySeparatorChar +  "emptyFolder");

            CLITestHelper.ReadConsoleWarnings(testFolderName, ReturnCode.Success);
        }



        /// <summary>
        /// Test that even with a execToStep>Preprocessor, then the parsing will halt on preprocessor phase because copy are missing
        /// </summary>
        [TestMethod]
        public void TestHaltOnMissingCopy_1()
        {
#if EUROINFO_RULES
            CLITestHelper.Test("haltOnMissingCopy_1_EI", ReturnCode.MissingCopy);
#else
             CLITestHelper.Test("haltOnMissingCopy_1", ReturnCode.MissingCopy);
#endif
        }

        /// <summary>
        /// Avoid loading intrinsic and dependencies when execToStep <= Preprocessor
        /// </summary>
        [TestMethod]
        public void AvoidLoadingIntrinsicAndDependencies()
        {
            CLITestHelper.Test("avoidLoadingIntrinsicAndDependencies", ReturnCode.Success);
        }

        /// <summary>
        /// This test should return MissingCopy.
        /// It test if in case of missing copy and with the proper arguments extracted copies file and missing copie file ar present and well formed.
        /// </summary>
        [TestMethod]
        public void TestExtractCopies()
        {
#if EUROINFO_RULES
            CLITestHelper.Test("extractUsedCopies_EI", ReturnCode.MissingCopy);
#else
            CLITestHelper.Test("extractUsedCopies", ReturnCode.MissingCopy);
#endif

        }

        /// <summary>
        /// Test if Euro Information suffixing rule is deactivated
        /// </summary>
        [TestMethod]
        public void TestReplacingSyntaxOption()
        {
#if EUROINFO_RULES
            CLITestHelper.Test("replacingSyntaxOption", ReturnCode.ParsingDiagnostics);
#endif

        }

        /// <summary>
        /// Test Various Return Code that can be returned by the CLI
        /// (with CLI arguments well formed, for bad argument test see TestArgumentsErrors())
        /// </summary>
        [TestMethod]
        public void TestReturnCode() {
            CLITestHelper.Test("return_code_0", ReturnCode.Success);//0
            CLITestHelper.Test("return_code_2", ReturnCode.OutputFileError); // 1001
            CLITestHelper.Test("return_code_3", ReturnCode.ParsingDiagnostics);// 1000
            CLITestHelper.Test("return_code_4", ReturnCode.Warning);//1
        }

        /// <summary>
        /// Try parsing with PublicSignature as output format.
        /// Should return success.
        /// </summary>
        [TestMethod]
        public void TestOutputFormat()
        {
            CLITestHelper.Test("outputSignature_1", ReturnCode.Warning);
        }


        /// <summary>
        /// Try parsing with Documentation Generation as output format.
        /// Should return success.
        /// </summary>
        [TestMethod]
        public void TestDocGen()
        {
            CLITestHelper.Test("documentation", ReturnCode.Warning);
        }

        /// <summary>
        /// Test all CLI arguments errors as follow:
        /// 
        /// arguments_errors_1:
        /// -i   Bad Paths + Folders, multiple
        /// -o   Count, multiple
        /// -d   Bad Path
        /// -s   Missing
        /// -hc  Bad Path
        /// -e   Unexpected token, multiple
        /// -y   Bad Paths + Folders, multiple
        /// -c   Bad Paths + Folders, multiple
        /// -dp  Bad Paths + Folders, multiple
        /// -md  non-Integer
        /// -f   Unexpected token
        /// -ec  Bad Path
        /// -exc Bad Path
        /// 
        /// arguments_errors_2:
        /// -i   Missing
        /// -o   Missing mais ets != 5
        /// -s   Missing mais ets != 5
        /// -ets Unexpected token, multiple
        /// -e   multiple, wrong ("rdz" is taken")
        /// -md  multiple
        /// -f   multiple
        /// 
        /// arguments_errors_3:
        /// -o   Bad Path
        /// -s   Bad Path, Multiple given
        /// 
        /// </summary>
        [TestMethod]
        public void TestArgumentsErrors()
        {
            CLITestHelper.ReadConsole("arguments_errors_1", ReturnCode.MultipleErrors);
            CLITestHelper.ReadConsole("arguments_errors_2", ReturnCode.MultipleErrors);
            CLITestHelper.ReadConsole("arguments_errors_3", ReturnCode.MultipleErrors);
        }

        /// <summary>
        /// The following test concernes cross compilaton of multiple source file.
        /// These test is linked to issue #1020. Compose of 2 main programs depending on each other to be generated. 
        /// </summary>
        [TestMethod]
        public void CrossCompilationSources()
        {
            CLITestHelper.Test("cross_compilation_sources", ReturnCode.Success);
        }

    }

    public class CLITestHelper {

        /// <summary>
        /// Use the folder ressources\[testFolderName]
        /// Run command TypeCobol.CLI with the content of CLIArguments.txt as arguments
        /// Check that the output of the CLI match the content of the expected console output stored is ExpectedConsole.txt.
        /// </summary>
        internal static void ReadConsole(string testFolderName, ReturnCode expectedReturnCode)
        {
            var workingDirectory = "ressources" + Path.DirectorySeparatorChar + testFolderName;
            string arguments = File.ReadAllText(workingDirectory + Path.DirectorySeparatorChar + "CLIArguments.txt");
            string standardOutput = Test(workingDirectory, arguments, expectedReturnCode).Trim().Replace("\r", "");
            string expectedoutput = File.ReadAllText(workingDirectory + Path.DirectorySeparatorChar + "ExpectedConsole.txt").Trim().Replace("\r", "");
            if (!string.Equals(standardOutput, expectedoutput, StringComparison.CurrentCultureIgnoreCase))
                throw new Exception(string.Format("console outputs not equals.{0}" +
                                                  "Console: {4}{0}{1}{0}" +
                                                  "Expected: {5}{0}{2}{0}" +
                                                  "{3}",
                    Environment.NewLine, standardOutput, expectedoutput, Environment.NewLine, standardOutput.Length, expectedoutput.Length));
        }

        internal static void ReadConsoleWarnings(string testFolderName, ReturnCode expectedReturnCode)
        {
            var workingDirectory = "ressources" + Path.DirectorySeparatorChar + testFolderName;
            string arguments = File.ReadAllText(workingDirectory + Path.DirectorySeparatorChar + "CLIArguments.txt");
            string standardOutput = Test(workingDirectory, arguments, expectedReturnCode).Trim();
            string warnings = string.Empty;

            foreach (string line in standardOutput.Split(new string[] { "\r\n" }, StringSplitOptions.None))
            {
                if (line.StartsWith("Line"))
                    warnings += line + "\r\n";
            }

            warnings = warnings.Trim();
            string expectedoutput = File.ReadAllText(workingDirectory + Path.DirectorySeparatorChar + "ExpectedConsole.txt").Trim();
            if (!string.Equals(warnings, expectedoutput, StringComparison.CurrentCultureIgnoreCase))
                throw new Exception(string.Format("console outputs not equals.{0}" +
                                                  "Console: {4}{0}{1}{0}" +
                                                  "Expected: {5}{0}{2}{0}" +
                                                  "{3}",
                    Environment.NewLine, warnings, expectedoutput, Environment.NewLine, warnings.Length, expectedoutput.Length));
        }

        /// <summary>
        /// Use the folder ressources\[testFolderName]
        /// Run command TypeCobol.CLI with the content of CLIArguments.txt as arguments
        /// Check that the actual "output" folder (the one created by the CLI) match the content of the expected "output" folder.
        /// The number of files and the content of the files must be identical
        /// </summary>
        internal static string Test(string testFolderName, ReturnCode expectedReturnCode)
        {
            var workingDirectory = "ressources" + Path.DirectorySeparatorChar + testFolderName;
            string arguments = File.ReadAllText(workingDirectory + Path.DirectorySeparatorChar + "CLIArguments.txt");
            return Test(workingDirectory, arguments, expectedReturnCode);
        }

        internal static string Test(string workingDirectory, string arguments, ReturnCode expectedReturnCode)
        {
            //
            //Create output folder because CLI will not create it
            DirectoryInfo outputDir = new DirectoryInfo(workingDirectory + Path.DirectorySeparatorChar + "output");
            if (outputDir.Exists)
            {
                outputDir.Delete(true);
                outputDir.Refresh();
                while (outputDir.Exists)
                    outputDir.Refresh();
            }
            outputDir.Create();
            outputDir.Refresh();
            while (!outputDir.Exists)
                outputDir.Refresh();


            System.Diagnostics.Process process = new System.Diagnostics.Process();
            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
            startInfo.FileName = "cmd.exe";
            startInfo.WorkingDirectory = workingDirectory;
            startInfo.Arguments = @"/c " + ".." + Path.DirectorySeparatorChar + ".." + Path.DirectorySeparatorChar +
                                  "TypeCobol.CLI.exe " + arguments;

            process.StartInfo = startInfo;
            process.StartInfo.RedirectStandardOutput = true;
            process.StartInfo.UseShellExecute = false;
            process.Start();
            while (!process.HasExited)
                continue;

            Console.WriteLine("workingDirectory="+ workingDirectory);
            Console.WriteLine("Return Code=" + process.ExitCode);
            //Compare outputDir with expectedOutputDir
            DirectoryInfo expectedOutputDir = new DirectoryInfo(workingDirectory + Path.DirectorySeparatorChar + "output_expected");
            bool dirIdentical = UnitTestHelper.CompareDirectory(expectedOutputDir, outputDir, "CLI\\test");

            string consoleOutput = (process.StandardOutput.ReadToEnd());

            if (!dirIdentical) {
                throw new Exception("directory not equals");
            }

            var returnCode = (ReturnCode)process.ExitCode;
            if (expectedReturnCode != returnCode)
                throw new Exception(string.Format("Wrong return code detected: {0} instead of {1}", returnCode, expectedReturnCode));

            return consoleOutput;
        }

        
    }

    // This implementation defines a very simple comparison  
    // between two FileInfo objects. It only compares the name  
    // of the files being compared and their length in bytes.  
    public class FileCompare : System.Collections.Generic.IEqualityComparer<System.IO.FileInfo>
    {
        public FileCompare() { }

        public bool Equals(System.IO.FileInfo f1, System.IO.FileInfo f2) {
            return (f1?.Name == f2?.Name);
        }

        // Return a hash that reflects the comparison criteria. According to the   
        // rules for IEqualityComparer<T>, if Equals is true, then the hash codes must  
        // also be equal. Because equality as defined here is a simple value equality, not  
        // reference identity, it is possible that two or more objects will produce the same  
        // hash code.  
        public int GetHashCode(System.IO.FileInfo fi)
        {
            return fi.Name.GetHashCode();
        }
    }

    public static class UnitTestHelper
    {
        //Looks for the begining of the message array in the input file, 
        //symbolized by a succession of white space from the beginning of the line and ending with a "{" e.g. "    {"
        private static readonly Regex RxStartUseActual = new Regex(@"^\s+{$");

        //Looks for the ending of the message array in the expected input file,
        //symbolized by a succession of white space from the beginning of the line and ending with a "]" e.g. "  ],"
        private static readonly Regex RxStopUseExpected = new Regex(@"^\s+],$");

        //Looks for the ending of the result_message array,
        //symbolized by a succession of white space from the beginning of the line and ending with a "]" e.g. "  ]"
        private static readonly Regex RxStopUseActual = new Regex(@"^\s+]$");

        /// <summary>
        /// Compare results of LSR tests
        /// Or replace automatically the input content of the test if autoReplace is true
        /// </summary>
        /// <param name="targetDir"></param>
        /// <param name="actualDir"></param>
        /// <param name="testPlaylistDirectory"></param>
        /// <returns></returns>
        public static bool CompareDirectory(DirectoryInfo targetDir, DirectoryInfo actualDir, string testPlaylistDirectory = null)
        {
            bool autoReplace = false;

            if (targetDir == null && actualDir == null && testPlaylistDirectory.Contains("LSRTests"))
                return autoReplace;

            if (!targetDir.Exists)
            {
                Console.WriteLine("No Output folders comparison");
                return true; //If the output_expected does not exist it means that the test doesn't have any expected output. 
            }

            // Take a snapshot of the file system.  
            var targetFiles = targetDir.GetFiles("*.*", System.IO.SearchOption.AllDirectories).ToList();
            var actualFiles = actualDir.GetFiles("*.*", System.IO.SearchOption.AllDirectories).ToList();
            targetFiles.Sort((f1, f2) => string.Compare(f1.Name, f2.Name, StringComparison.Ordinal));
            actualFiles.Sort((f1, f2) => string.Compare(f1.Name, f2.Name, StringComparison.Ordinal));



            FileCompare myFileCompare = new FileCompare();

            /*
            // This query determines whether the two folders contain  
            // identical file lists, based on the custom file comparer  
            // that is defined in the FileCompare class.  
            // The query executes immediately because it returns a bool.  
            bool areIdentical = targetFiles.SequenceEqual(actualFiles, myFileCompare);

            if (areIdentical == true) {
                Console.WriteLine("the two folders are the same");
            } else {
                Console.WriteLine("The two folders are not the same");
            }
            */

            // Find the common files. It produces a sequence and doesn't   
            // execute until the foreach statement.  
            var commonTargetFiles = targetFiles.Intersect(actualFiles, myFileCompare).ToList();
            var commonActualFiles = actualFiles.Intersect(targetFiles, myFileCompare).ToList();
            if (commonTargetFiles.Count != commonActualFiles.Count)
            {
                throw new InvalidOperationException();
            }

            bool dirIdentical = true;
            for (int i = 0; i < commonTargetFiles.Count; i++)
            {
                var targetFileContent = File.ReadAllLines(commonTargetFiles[i].FullName);
                var actualFileContent = File.ReadAllLines(commonActualFiles[i].FullName);
                if (!targetFileContent.SequenceEqual(actualFileContent))
                {
                    if (autoReplace && testPlaylistDirectory != null)
                    {
                        string path = string.Empty;

                        //Convert path from generated test files to orignal
                        string projectPath = commonTargetFiles[i].FullName.Split(new[] {"bin"}, StringSplitOptions.None)[0];
                        string testRelativePath = commonTargetFiles[i].FullName.Split(new[] { "Debug" }, StringSplitOptions.None)[1];

                        path = projectPath + testPlaylistDirectory + testRelativePath;

                        if (testPlaylistDirectory.Contains("LanguageServer") && path != String.Empty)
                            ReplaceLSRTestFile(path, actualFileContent);

                        else if (testPlaylistDirectory.Contains("CLI") && path != String.Empty)
                            ReplaceCliTestFile(path, actualFileContent);

                        Console.WriteLine("File not equals: " + commonTargetFiles[i]);
                        Console.WriteLine("Input file has been modified\n");
                        Console.WriteLine("Please rerun unit test\n");
                        dirIdentical = false;
                    }
                    else
                    {
                        Console.WriteLine("File not equals: " + commonTargetFiles[i]);
                        Console.WriteLine("See \"CLITest.cs\" CompareDirectory method to autoreplace input file");
                        Console.WriteLine("___Actual file content___:\n");
                        foreach (var actual in actualFileContent)
                        {
                            Console.WriteLine(actual);
                        }
                        Console.WriteLine("\n________________\n");
                        Console.WriteLine("___Expected file content___:\n");
                        foreach (var expected in targetFileContent)
                        {
                            Console.WriteLine(expected);
                        }
                        Console.WriteLine("________________");

                        dirIdentical = false;
                    }
                }
            }

            // Find the set difference between the two folders.  
            // Files only in targetFiles
            var queryTargetFilesOnly = (from file in targetFiles select file).Except(actualFiles, myFileCompare);
            if (queryTargetFilesOnly.Any())
            {
                Console.WriteLine("Only present in expected folder");
                dirIdentical = false;
                foreach (var v in queryTargetFilesOnly)
                {
                    Console.WriteLine(v.FullName);
                }
            }

            var queryActualFilesOnly = (from file in actualFiles select file).Except(targetFiles, myFileCompare);

            if (queryActualFilesOnly.Any())
            {
                Console.WriteLine("Only present in actual folder");
                dirIdentical = false;
                foreach (var v in queryActualFilesOnly)
                {
                    Console.WriteLine(v.FullName);
                }
            }

            return dirIdentical;
        }
        /// <summary>
        /// Replacement logic for cli tests
        /// </summary>
        /// <param name="path"></param>
        /// <param name="actualFileContent"></param>
        private static void ReplaceCliTestFile(string path, string[] actualFileContent)
        {
            using (StreamWriter writer = new StreamWriter(new FileStream(path, FileMode.Truncate)))
            {
                foreach (var line in actualFileContent)
                {
                    writer.WriteLine(line);
                }
            }
        }

        /// <summary>
        /// Replacement logic for LSR Tests
        /// </summary>
        /// <param name="path"></param>
        /// <param name="actualFileContent"></param>
        private static void ReplaceLSRTestFile(string path, string[] actualFileContent)
        {
            path = path.Replace("output_expected", "input");
            path = path.Replace(".rlsp", ".tlsp");

            var inputFileContent = File.ReadAllLines(path);


            using (StreamWriter writer = new StreamWriter(new FileStream(path, FileMode.Truncate)))
            {
                for (var index = 0; index < inputFileContent.Length; index++)
                {
                    //if regex string is true and line is followed by the begining of a message. (a message will always start with a "category" item)
                    if (RxStartUseActual.IsMatch(inputFileContent[index]) && inputFileContent[index + 1].Contains("\"category\""))
                    {
                        ReplaceResultLines(actualFileContent, writer);
                        while (!RxStopUseExpected.IsMatch(inputFileContent[index]))
                        {
                            index++;
                        }
                    }

                    //Writes any lines that isn't a message
                    writer.WriteLine(inputFileContent[index]);
                }
            }
            
        }

        /// <summary>
        /// Replaces messages in LSR test inputs by ActualResult messages
        /// </summary>
        /// <param name="replacingText"></param>
        /// <param name="writer"></param>
        /// <returns></returns>
        private static int ReplaceResultLines(string[] replacingText, StreamWriter writer)
        {
            int writenLines = 0;

            bool write = false;

            for (int i = 0; i < replacingText.Length; i++)
            {
                //if regex string is true and line is followed by the begining of a message. (a message will always start with a "category" item)
                if (RxStartUseActual.IsMatch(replacingText[i]) && replacingText[i + 1].Contains("\"category\""))
                {
                    write = true;
                }

                if (RxStopUseActual.IsMatch(replacingText[i]))
                {
                    write = false;
                }

                if (write)
                {
                    writer.WriteLine(replacingText[i]);
                    writenLines++;
                }
                
            }
            return writenLines;
        }
    }
}
