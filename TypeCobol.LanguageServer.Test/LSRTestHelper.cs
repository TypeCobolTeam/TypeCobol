using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CLI.Test;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.LanguageServer.Utilities;

namespace TypeCobol.LanguageServer.Test
{
    public class LSRTestHelper
    {
        /// <summary>
        /// Contains the default argument to launch TypeCobol Language Server executable. 
        /// -r activate the robot mode
        /// -e make LSR stops at first error detected
        /// -lsr contains the path to LanguageServerRobot executable
        /// -script contains the path to the script to test
        /// -config contains the path to the config file necessary for TypeCobolLSR initialization 
        /// -init Give the initialize file path
        /// {4} is filled with -td option if activateTdOption is true. This option will allow to avoid TypeCobolServer to do Node Refresh
        /// {8} is filled with -sc if useSyntaxColoring is set to true.
        /// </summary>
        private static readonly string defaultTypeCobolLSArgs = "-r -lsr={0} -ro=\"-lf={6} -l=3 -init={1} -config={2}\" -script={3} {4} {5} -lf={7} -l=3 {8} {9} {10}";
        /// <summary>
        /// LSR Test Timeout in milli secondes.
        /// </summary>
        public const int LSR_TEST_TIMEOUT = 1000 * 30;

        public static void Test(string testFolderName, LsrTestingOptions lsrTestingOption, bool activateTdOption = false, bool useSyntaxColoring = false, bool useOutline = false, string copyFolder = null, string customIntrinsicFile = null, string customDependenciesFolder = null, bool useCfg = false, bool pureCobol = false)
        {
            var workingDirectory = "LSRTests";
            var testWorkingDirectory = workingDirectory + Path.DirectorySeparatorChar + testFolderName;
            var scriptPath = Directory.GetFiles(testWorkingDirectory + Path.DirectorySeparatorChar + "input", "*.tlsp").FirstOrDefault();
            var initializeFileInfo = new FileInfo(workingDirectory + Path.DirectorySeparatorChar + "initialize.json");
            var configFileInfo = new FileInfo(workingDirectory + Path.DirectorySeparatorChar + "config.json");
            var intrinsicFileInfo = new FileInfo(workingDirectory + Path.DirectorySeparatorChar + "DefaultIntrinsic.txt");

            if (string.IsNullOrEmpty(scriptPath))
                Assert.Fail("Script path is null or empty");

            //Update Init FileInfo 
            var initFileContent = File.ReadAllText(initializeFileInfo.FullName);
            initFileContent = initFileContent.Replace("{rootPath}", Directory.GetCurrentDirectory().Replace(@"\", @"\\"));
            initFileContent = initFileContent.Replace("{rootUri}", new Uri(Directory.GetCurrentDirectory()).ToString());
            var initGeneratedFileInfo = new FileInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "generatedInitialize.json");

            //Write the initialize file content into generatedInitialize.json file
            File.WriteAllText(initGeneratedFileInfo.FullName, initFileContent);

            //Update config file
            var configFileContent = File.ReadAllText(configFileInfo.FullName);
            configFileContent = configFileContent.Replace("{CopyFolder}",
                new DirectoryInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "input" +
                                  Path.DirectorySeparatorChar + copyFolder).FullName.Replace(@"\", @"\\"));
            String testOptions = "";
            testOptions += useOutline ? "" : ",\"-dol\"";
            testOptions += pureCobol ? ",\"-cob\"" : "";
            configFileContent = configFileContent.Replace("{TestOptions}", testOptions);

            configFileContent = configFileContent.Replace("{IntrinsicFile}",
                customIntrinsicFile == null
                    ? intrinsicFileInfo.FullName.Replace(@"\", @"\\")
                    : new FileInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "input" +
                                   Path.DirectorySeparatorChar + customIntrinsicFile).FullName.Replace(@"\", @"\\"));

            configFileContent = configFileContent.Replace("{DependenciesFolder}",
                new DirectoryInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "input" +
                                  Path.DirectorySeparatorChar + customDependenciesFolder).FullName.Replace(@"\", @"\\"));

            var configGeneratedFileInfo = new FileInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "generatedConfig.json");

            //Write the config file content into generatedConfig.json file
            File.WriteAllText(configGeneratedFileInfo.FullName, configFileContent);

            //Pre-Create the Result directory.
            var workingDir =
                new DirectoryInfo(testWorkingDirectory);
            workingDir.CreateSubdirectory(Path.Combine("input", "Results"));

            //Specify log file for LSR
            var logFile = Path.Combine(workingDir.FullName, "LSRLog.txt");

            //Specify log file for TC LSP
            var tcLogFile = Path.Combine(workingDir.FullName, "TCLSPLog.txt");

            var scriptFileInfo = new FileInfo(scriptPath);
            //Setup the arguments
            //The path for LanguageServerRobot depends on the NuGetPackage. If the NuGet is not downloaded, it won't works
            var arguments = string.Format(defaultTypeCobolLSArgs,
                @"..\..\TypeCobol.LanguageServerRobot.exe",
                initGeneratedFileInfo.FullName, 
                configGeneratedFileInfo.FullName, 
                scriptFileInfo.FullName, 
                activateTdOption ? "-td" : "", 
                lsrTestingOption.ToLanguageServerOption(),
                logFile,
                tcLogFile,
                useSyntaxColoring ? "-sc" : "",
                useOutline ? "-ol" : "",
                useCfg ? "-cfg=AsContent" : "");

            //Build full path to default Cpy Copy names file for LSR tests
            string cpyCopiesFile = Path.GetFullPath(Path.Combine(testWorkingDirectory, "input", "CpyCopies.lst"));
            arguments += " -ycpl=\"" + cpyCopiesFile + "\"";

            System.Diagnostics.Process process = new System.Diagnostics.Process();
            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
            startInfo.FileName = Directory.GetCurrentDirectory() + Path.DirectorySeparatorChar +"TypeCobol.LanguageServer.exe";
            startInfo.WorkingDirectory = testWorkingDirectory;
            startInfo.Arguments =  arguments;
            process.StartInfo = startInfo;
            process.Start();
            process.WaitForExit(LSR_TEST_TIMEOUT);
            if (!process.HasExited)
            {
                process.Kill(true); // Also kill associated LSR process
                string logs = Environment.NewLine;
                logs += "Robot log: " + Read(logFile);
                logs += Environment.NewLine;
                logs += "Server log: " + Read(tcLogFile);
                throw new Exception("!!!! TC-LSP PROCESS KILLED !!!" + logs);

                string Read(string filePath)
                {
                    if (File.Exists(filePath))
                    {
                        return File.ReadAllText(filePath);
                    }

                    return $"'{filePath}' does not exist !";
                }
            }
            else
            {
                if (process.ExitCode != 0 && process.ExitCode != 1)
                {
                    throw new Exception("!!!! TC-LSP PROCESS EXIT CODE" + process.ExitCode);
                }
            }

            DirectoryInfo expectedOutputDir = new DirectoryInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "output_expected");
            DirectoryInfo resultOutputDir = new DirectoryInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "input" + Path.DirectorySeparatorChar + "Results");
            bool dirIdentical = UnitTestHelper.CompareDirectory(expectedOutputDir, resultOutputDir, "TypeCobol.LanguageServer.Test");
            if (!dirIdentical)
            {
                throw new Exception("directory not equals");
            }
        }

    }
}
