using Mono.Options;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Controller;

namespace TypeCobol.TemplateTranspiler
{
    public class Program
    {
        /// <summary>
        /// Program name from Assembly name
        /// </summary>
        public static string ProgName
        {
            get
            {
                return Assembly.GetExecutingAssembly().GetName().Name;
            }
        }
        /// <summary>
        /// Assembly version
        /// </summary>
        public static string Version
        {
            get
            {
                return Assembly.GetExecutingAssembly().GetName().Version.ToString();
            }
        }

        /// <summary>
        /// The Input Skeleton File
        /// </summary>
        public static string SkeletonXmlFile
        {
            get;
            set;
        }

        /// <summary>
        /// The Output Skeleton File
        /// </summary>
        public static string SkeletonCSharpFile
        {
            get;
            set;
        }

        public static int Main(string[] args)
        {
            bool help = false;
            bool version = false;

            var p = new OptionSet()
            {
                "USAGE",
                "  "+ProgName+" [OPTIONS]",
                "",
                "VERSION:",
                "  "+Version,
                "",
                "DESCRIPTION:",
                "  Run the TypeCobol Template Transpiler.",
                { "v|version","Show version", _ => version = true },
                { "h|help","Show help", _ => help = true },
                { "i|input=","{PATH} The Skeleton xml input file", (string v) => SkeletonXmlFile = v },
                { "o|output=","{PATH} The optional C# file generated", (string v) => SkeletonCSharpFile = v },
            };

            System.Collections.Generic.List<string> arguments;
            try { arguments = p.Parse(args); }
            catch (OptionException ex) { return exit(1, ex.Message); }

            if (help)
            {
                p.WriteOptionDescriptions(System.Console.Out);
                return 0;
            }
            if (version)
            {
                System.Console.WriteLine(string.Format(AppResource.VersionMessage, ProgName, Version));
                return 0;
            }
            if (SkeletonXmlFile == null)
            {//No file especifier 
                return exit(-1, AppResource.MissingSkeletonInputFile);
            }
            if (SkeletonCSharpFile == null)
            {
                SkeletonCSharpFile = "Skeleton.cs";
            }
            //Determines the ouput class name
            string classname = "";
            System.IO.FileInfo fi = new System.IO.FileInfo(SkeletonCSharpFile);
            int indexExt = fi.Name.LastIndexOf('.');
            if (indexExt > 0)
            {
                classname = fi.Name.Substring(0, indexExt).Trim();
            }
            if (classname.Length == 0)
            {
                classname = "Skeleton";
            }
            //Now Generate the file.
            return SkeletonsController.Transpile(SkeletonXmlFile, SkeletonCSharpFile, classname) ? 0 : -1;
        }

        /// <summary>
        /// Command Line Option Set
        /// </summary>
        public static OptionSet Options
        {
            get;
            internal set;
        }

        static int exit(int code, string message)
        {
            string errmsg = ProgName + ": " + message + "\n";
            errmsg += string.Format(AppResource.TryHelpMessage, ProgName);
            System.Console.WriteLine(errmsg);
            return code;
        }
    }
}
