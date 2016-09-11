using System;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Test
{
    public static class PlatformUtils
    {
        /// <summary>
        /// Relative path of the test project in VS solution
        /// </summary>
        public const string RELATIVE_TEST_PROJECT_PATH = "TypeCobol.Test";
        
	static PlatformUtils() {
		// When tests are executed in Visual Studio test explorer
		// the working directory is ....\TEST_PROJECT_PATH\bin\Debug\...
		// => we can access all project files by extracting the start of
		// the project path on the local disk from the working directory
		string pwd = Directory.GetCurrentDirectory();
		SOLUTION_DIR = Directory.GetParent(pwd).Parent.FullName;
		ABSOLUTE_TEST_PROJECT_PATH = pwd.Substring(0, pwd.IndexOf(RELATIVE_TEST_PROJECT_PATH) + RELATIVE_TEST_PROJECT_PATH.Length);
		}

        /// <summary>Absolute path of the test project on the local disk</summary>
        private static readonly string ABSOLUTE_TEST_PROJECT_PATH;
        private static readonly string SOLUTION_DIR;

	/// <summary>
	/// If file "foo.txt" is stored in project subdirectory "bar",
	/// relativeFilePath input parameter should be "bar/foo.txt"
	/// </summary>
	public static string GetPathForProjectFile(string relativeFilePath) {
System.Console.WriteLine("relativeFilePath: \""+relativeFilePath+"\"");
            string pwd = Directory.GetCurrentDirectory();
System.Console.WriteLine("GetCurrentDirectory(): \""+pwd+"\"");
System.Console.WriteLine("SOLUTION: \""+SOLUTION_DIR+"\"");
System.Console.WriteLine("ABSOLUTE_TEST_PROJECT_PATH: \""+ABSOLUTE_TEST_PROJECT_PATH+"\"");
		return ABSOLUTE_TEST_PROJECT_PATH + Path.DirectorySeparatorChar + relativeFilePath;
	}

        /// <summary>
        /// If file "foo.txt" is stored in project subdirectory "bar",
        /// relativeFilePath input parameter should be "bar/foo.txt"
        /// </summary>
        public static Stream GetStreamForProjectFile(string relativeFilePath)
        {           
            return new FileStream(GetPathForProjectFile(relativeFilePath), FileMode.Open);
        }

        /// <summary>
        /// If directory "foo" is a subdirectory of project directory "bar",
        /// relativeDirPath input parameter should be "bar/foo"
        /// </summary>
        public static IEnumerable<string> ListFilesInSubdirectory(string relativeDirPath)
        {
            return Directory.EnumerateFiles(GetPathForProjectFile(relativeDirPath));
        }
    }
}
