using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Server;
using TypeCobol.Tools.Options_Config;

namespace CLI.Test
{
    [TestClass]
    public class CLITest {
        [TestMethod]
        public void TestParse_1() {
            //From folder ressources\parse_1
            //Run command TypeCobol.CLI with the content of CLIArguments.txt as arguments
            //Check that the actual "output" folder (the one created by the CLI) match the content of the expected "output" folder
            //the one on Git.
            //The number of files and the content of the files must be identical
            CLITestHelper.Test("parse_1", ReturnCode.OutputFileMissing);   
        }

        [TestMethod]
        public void TestExecToStep_1() {
            CLITestHelper.Test("execToStep_1", ReturnCode.Success);
        }

        [TestMethod]
        public void TestGenerate_1() {
            CLITestHelper.Test("generate_1", ReturnCode.Success);
        }

        [TestMethod]
        public void TestDependencies() {
            CLITestHelper.Test("dependencies_1", ReturnCode.Success);
            CLITestHelper.Test("dependencies_2", ReturnCode.Success);
            CLITestHelper.Test("dependencies_3", ReturnCode.ParsingDiagnostics);
            CLITestHelper.Test("dependencies_4", ReturnCode.DependenciesError);            //No dependencies found
            CLITestHelper.Test("dependencies_5", ReturnCode.Success);
#if EUROINFO_RULES
            CLITestHelper.Test("ei_dependencies_1", ReturnCode.ParsingDiagnostics);
#endif
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

        [TestMethod]
        public void TestExtractCopies()
        {
#if EUROINFO_RULES
            CLITestHelper.Test("extractUsedCopies_EI", ReturnCode.MissingCopy);
#else
            CLITestHelper.Test("extractUsedCopies", ReturnCode.MissingCopy);
#endif

        }

        [TestMethod]
        public void TestReturnCode() {
            CLITestHelper.Test("return_code_0", ReturnCode.Success);//0
            //CLITestHelper.Test("return_code_1", ReturnCode.FatalError);
            CLITestHelper.Test("return_code_2", ReturnCode.OutputFileError); // 1001
            CLITestHelper.Test("return_code_3", ReturnCode.ParsingDiagnostics);// 1000
            CLITestHelper.Test("return_code_4", ReturnCode.Warning);//1
        }


        [TestMethod]
        public void TestOutputFormat()
        {
            CLITestHelper.Test("outputSignature_1", ReturnCode.Success);
        }

        [TestMethod]
        public void TestArgumentsErrors()
        {
            CLITestHelper.ReadConsole("arguments_errors_1", ReturnCode.MultipleErrors);
            CLITestHelper.ReadConsole("arguments_errors_2", ReturnCode.MultipleErrors);
            CLITestHelper.ReadConsole("arguments_errors_3", ReturnCode.SkeletonFileError);
        }

    }

    public class CLITestHelper {
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
            bool dirIdentical = UnitTestHelper.CompareDirectory(expectedOutputDir, outputDir);

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
        public static bool CompareDirectory(DirectoryInfo targetDir, DirectoryInfo actualDir)
        {
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
                    Console.WriteLine("File not equals: " + commonTargetFiles[i]);
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
    }
}
