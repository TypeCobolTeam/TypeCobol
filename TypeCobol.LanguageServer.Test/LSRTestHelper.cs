using CLI.Test;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.LanguageServer.Utilities;

namespace TypeCobol.LanguageServer.Test
{
    public class LSRTestHelper
    {
        /// <summary>
        /// LSR Test Timeout in milliseconds.
        /// </summary>
        private const int LSR_TEST_TIMEOUT = 1000 * 30;

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
                                  (copyFolder != null ? Path.DirectorySeparatorChar + copyFolder : "")).FullName.Replace(@"\", @"\\"));
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
                                  (customDependenciesFolder != null ? Path.DirectorySeparatorChar + customDependenciesFolder : "")).FullName.Replace(@"\", @"\\"));

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

            //Get the path to LanguageServerRobot executable
            string lsrPath = Environment.CurrentDirectory;
            string configuration = Path.GetFileName(lsrPath);
            lsrPath = Path.Combine(lsrPath, "..", "..", "..");
            lsrPath = Path.GetFullPath(lsrPath);
            lsrPath = Path.Combine(lsrPath, "TypeCobol.LanguageServer.Test.LanguageServerRobot.Installer", "bin", configuration, "TypeCobol.LanguageServerRobot.dll");

            // LSR arguments
            var lsrOptions = new List<string>()
            {
                "-lf=" + logFile, // LSR log file path
                "-l=2", // Robot logging level:use the Protocol level
                "-init=" + initGeneratedFileInfo.FullName, // Path to the JSON initialize request
                "-config=" + configGeneratedFileInfo.FullName // Path to the JSON didChangeConfiguration notification
            };
            var lsrOptionsGeneratedFileInfo = new FileInfo(testWorkingDirectory + Path.DirectorySeparatorChar + "generatedLsrOptions.txt");
            File.WriteAllLines(lsrOptionsGeneratedFileInfo.FullName, lsrOptions);

            // LS arguments
            var lsOptions = new List<string>()
            {
                "-r", // Use Robot
                "-lsr=" + lsrPath, // Path to the robot executable file or library
                "-ro=" + lsrOptionsGeneratedFileInfo.FullName, // Path to the robot options file
                "-script=" + scriptFileInfo.FullName // Path to the input script
            };
            if (activateTdOption) lsOptions.Add("-td"); // if true, disable auto rebuild of AST
            lsOptions.Add(lsrTestingOption.ToLanguageServerOption()); // Testing mode for LSR
            lsOptions.Add("-lf=" + tcLogFile); // LS log file path
            lsOptions.Add("-l=2"); // Server logging level: use the Protocol level
            if (useSyntaxColoring) lsOptions.Add("-sc"); // if true, activate syntax coloring
            if (useOutline) lsOptions.Add("-ol"); // if true, activate outline
            if (useCfg) lsOptions.Add("-cfg=AsContent"); // if true, activate cfg as content directly included in messages

            //Build full path to default Cpy Copy names file for LSR tests
            string cpyCopiesFile = Path.GetFullPath(Path.Combine(testWorkingDirectory, "input", "CpyCopies.lst"));
            lsOptions.Add("-ycpl=" + cpyCopiesFile);

            System.Diagnostics.Process process = new System.Diagnostics.Process();
            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
            startInfo.FileName = "dotnet";
            startInfo.WorkingDirectory = testWorkingDirectory;
            var lsPath = Path.Combine(Directory.GetCurrentDirectory(), "TypeCobol.LanguageServer.dll");
            startInfo.ArgumentList.Add(lsPath);
            foreach (var lsOption in lsOptions)
            {
                startInfo.ArgumentList.Add(lsOption);
            }
            process.StartInfo = startInfo;
            process.Start();
            process.WaitForExit(LSR_TEST_TIMEOUT);

            if (!process.HasExited)
            {
                process.Kill(true); // Also kill associated LSR process
                throw new Exception("!!!! TC-LSP PROCESS KILLED !!!");
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
