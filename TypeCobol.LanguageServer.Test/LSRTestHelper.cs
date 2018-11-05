﻿using System;
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
        /// </summary>
        private static readonly string defaultTypeCobolLSArgs = "-r -lsr={0} -ro=\"  -init={1} -config={2}\" -script={3} {4} {5}";
        /// <summary>
        /// LSR Test Timeout in milli secondes.
        /// </summary>
        public const int LSR_TEST_TIMEOUT = 1000 * 30;

        public static void Test(string testFolderName, LsrTestingOptions lsrTestingOption, bool activateTdOption = false, string copyFolder = null, string customIntrinsicFile = null, string customDependenciesFolder = null)
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


            var scriptFileInfo = new FileInfo(scriptPath);
            //Setup the arguments
            //The path for LanguageServerRobot depends on the NuGetPackage. If the NuGet is not downloaded, it won't works
            var arguments = string.Format(defaultTypeCobolLSArgs, @"TypeCobol.LanguageServerRobot.exe", initGeneratedFileInfo.FullName, configGeneratedFileInfo.FullName, scriptFileInfo.FullName, activateTdOption ? "-td" : "", lsrTestingOption.ToLanguageServerOption());

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
                process.Kill();
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
