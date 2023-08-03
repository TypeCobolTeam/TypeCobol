using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Test.ProjectMetrics
{
    /// <summary>
    /// Utility program which can be used to estimate the total number
    /// of lines in all the files found under the root project directory
    /// </summary>
    public static class CountLinesOfCode
    {
        public class FileStats
        {
            public FileStats(string extension)
            {
                Extension = extension;
            }

            public string Extension { get; private set; }

            public int Count;
            public int Lines;
        }

        public static string[] DIR_FILTERS = { "$", "Antlr4", "ICSharpCode", "bin", "packages", "TestResults", "obj", "old", "Properties", "Documentation", "Samples", "Images" };

        private static IDictionary<string,FileStats> AnalyzeDirectory(int level, string dir)
        {
            IDictionary<string,FileStats> dirStats = new Dictionary<string,FileStats>();
            foreach (string filepath in Directory.EnumerateFiles(dir))
            {
                FileInfo file = new FileInfo(filepath);
                string extension = file.Extension;
                int lines = 0;
                using(StreamReader reader = new StreamReader(filepath))
                {
                    while(reader.ReadLine() != null)
                    {
                        lines++;
                    }
                }
                if(dirStats.ContainsKey(extension))
                {
                    dirStats[extension].Count++;
                    dirStats[extension].Lines += lines;
                }
                else
                {
                    FileStats fs = new FileStats(extension);
                    fs.Count = 1;
                    fs.Lines = lines;
                    dirStats.Add(extension, fs);
                }
            }
            WriteStats(true, level, dir, dirStats);

            bool hassubDir = false;
            foreach (string subdir in Directory.EnumerateDirectories(dir))
            {
                string localDir = subdir.Substring(subdir.LastIndexOf('\\') + 1);
                bool skipdir = false;
                foreach(string dirFilter in DIR_FILTERS)
                {
                    if(localDir.StartsWith(dirFilter))
                    {
                        skipdir = true;
                        break; ;
                    }
                }
                if (skipdir) continue;

                IDictionary<string,FileStats> subdirStats = AnalyzeDirectory(level+1, subdir);
                AddStats(dirStats, subdirStats);
                hassubDir = true;
            }
            if (hassubDir)
            {
                WriteStats(false, level, dir, dirStats);
            }

            return dirStats;
        }

        private static void AddStats(IDictionary<string,FileStats> totalStats,IDictionary<string,FileStats> subdirStats)
        {
             foreach(string extension in subdirStats.Keys)
            {
                if(totalStats.ContainsKey(extension))
                {
                    totalStats[extension].Count += subdirStats[extension].Count;
                    totalStats[extension].Lines += subdirStats[extension].Lines;
                }
                else
                {
                    totalStats.Add(extension, subdirStats[extension]);
                }
            }
        }

        private static string STATS_FILE = @"C:\Users\Laurent\Desktop\TypeCobolStats.csv";
        private static StreamWriter statsFileWriter;

        private static void WriteStats(bool isLocal, int level, string path, IDictionary<string,FileStats> stats)
        {
            foreach (string extension in stats.Keys)
            {
                statsFileWriter.Write(isLocal ? "-" : "+");
                statsFileWriter.Write(';');
                statsFileWriter.Write(level);
                statsFileWriter.Write(';');
                statsFileWriter.Write(path);
                statsFileWriter.Write(';');
                statsFileWriter.Write(extension);
                statsFileWriter.Write(';');
                statsFileWriter.Write(stats[extension].Count);
                statsFileWriter.Write(';');
                statsFileWriter.WriteLine(stats[extension].Lines);
            }
        }

        public static void Main(string[] args)
        {
            string rootDirectory = @"D:\Users\Laurent\OneDrive\Dev\Visual Studio 2012\Projects\TypeCobol";
            using(statsFileWriter = new StreamWriter(STATS_FILE))
            {            
                IDictionary<string,FileStats> totalStats = AnalyzeDirectory(0, rootDirectory);
            }
        }
    }
}
