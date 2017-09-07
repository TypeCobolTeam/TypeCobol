using System.Linq;

namespace TypeCobol.Tools
{

    using System.Collections.Generic;
    using System.IO;

    /// <summary>
    /// For this utility method to be more easily found, I would have created an extension method
    /// for System.IO.Path class instead, but C# doesn't allow static method extension -passing a class
    /// instead of an instance to the extension method was too difficult, I guess ?
    /// The alternative was to create a pointless wrapper class ...
    /// How could a language sell with such half-baked syntactic sugar ?
    /// </summary>
    public static class FileSystem
    {
        public static List<string> GetFiles(string path, string[] patterns = null, bool recursive = true)
        {
            path = CleanFileName(path);
            var results = new List<string>();
            if (File.Exists(path))
            {
                results.Add(path);
            }
            else if (Directory.Exists(path))
            {
                var option = recursive ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
                if (patterns == null)
                {
                    results.AddRange(Directory.EnumerateFiles(path, "*", option));
                }
                else
                {
                    foreach (var pattern in patterns)
                    {
                        results.AddRange(Directory.EnumerateFiles(path, pattern[0] != '*' ? "*" + pattern : pattern, option));
                    }
                }
            }
            else if (path.Contains("*"))
            {
                string[] pattern = new string[1];
                pattern[0] = Path.GetFileName(path); //Get the wildcard at the end of the path
                path = string.Concat(Path.GetDirectoryName(path), Path.DirectorySeparatorChar);

                return GetFiles(path, pattern, recursive);
            }

            return results;
        }

        private static string CleanFileName(string fileName)
        {
            var charToIgnore = new char[] {'\"'};
            return charToIgnore.Aggregate(fileName, (current, c) => current.Replace(c.ToString(), string.Empty));
        }

    }
}
