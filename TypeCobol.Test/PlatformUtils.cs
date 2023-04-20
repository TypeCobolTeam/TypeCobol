namespace TypeCobol.Test
{
    public static class PlatformUtils
    {
        static PlatformUtils()
        {
            string currentDirectory = Directory.GetCurrentDirectory();
            while (!File.Exists(Path.Combine(currentDirectory, "TypeCobol.sln")))
            {
                currentDirectory = Path.GetDirectoryName(currentDirectory);
            }
            
            if (currentDirectory == null)
            {
                throw new InvalidOperationException("Could not locate root solution file !");
            }

            SOLUTION_PATH = currentDirectory;
        }

        /// <summary>Absolute path of the solution project on the local disk</summary>
        private static readonly string SOLUTION_PATH;

        /// <summary>
        /// If file "foo.txt" is stored in project subdirectory "bar",
        /// relativeFilePath input parameter should be "bar/foo.txt"
        /// sourceRelativeProjectPath should be filled if the method is used outside TypeCobol.Test
        /// </summary>
        public static string GetPathForProjectFile(string relativeFilePath, string sourceRelativeProjectPath = null)
        {
            string RELATIVE_PROJECT_PATH = sourceRelativeProjectPath == null ? "TypeCobol.Test" : sourceRelativeProjectPath;
            return Path.Combine(SOLUTION_PATH, RELATIVE_PROJECT_PATH, relativeFilePath);
        }

        /// <summary>
        /// If file "foo.txt" is stored in project subdirectory "bar",
        /// relativeFilePath input parameter should be "bar/foo.txt"
        /// sourceRelativeProjectPath should be filled if the method is used outside TypeCobol.Test
        /// </summary>
        public static Stream GetStreamForProjectFile(string relativeFilePath, string sourceRelativeProjectPath = null)
        {
            return new FileStream(GetPathForProjectFile(relativeFilePath, sourceRelativeProjectPath), FileMode.Open);
        }

        /// <summary>
        /// If directory "foo" is a subdirectory of project directory "bar",
        /// relativeDirPath input parameter should be "bar/foo"
        /// sourceRelativeProjectPath should be filled if the method is used outside TypeCobol.Test
        /// </summary>
        public static IEnumerable<string> ListFilesInSubdirectory(string relativeDirPath, string sourceRelativeProjectPath = null)
        {
            return Directory.EnumerateFiles(GetPathForProjectFile(relativeDirPath, sourceRelativeProjectPath));
        }
    }
}
