using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Transform
{
    /// <summary>
    /// The Main class.
    /// 
    /// </summary>
    public class Transform
    {
        /// <summary>
        /// Help Option String.
        /// </summary>
        static readonly string HELP_OPTION = "-help";
        static readonly string CONCATENATE_OPTION = "-tcmc";
        static readonly string DECODE_OPTION = "-c2tc";

        private static readonly string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;

        /// <summary>
        /// Main
        /// </summary>
        /// <param name="args">Command line arguments</param>
        /// <returns>0, if ok, 1 otherwise</returns>
        static int Main(string[] args)
        {
            if (args.Length == 1 && args[0].Equals(HELP_OPTION))
            {
                ShowUsage();
                return 0;
            }
            if (args.Length != 3 && args.Length != 4)
            {
                System.Console.WriteLine(String.Format("{0} : {1}", PROGNAME, Resource.Invalid_count_args));
                return 1;
            }
            if (args[0].Equals(HELP_OPTION))
            {
                System.Console.WriteLine(String.Format("{0} : {1}", PROGNAME, Resource.Invalid_count_args));
                return 0;
            }
            else if (args[0].Equals(CONCATENATE_OPTION))
            {
                if (args.Length != 4 && args.Length != 3)
                {
                    System.Console.WriteLine(String.Format("{0} : {1}", PROGNAME, Resource.Invalid_count_args));
                    return 1;
                }
                return args.Length == 4
                    ? Decoder.concatenateFiles(args[1], args[2], args[3]) ? 0 : 1
                    : Decoder.concatenateFiles(args[1], null, args[2]) ? 0 : 1;
            }
            else if (args[0].Equals(DECODE_OPTION))
            {
                if (args.Length != 3)
                {
                    System.Console.WriteLine(String.Format("{0} : {1}", PROGNAME, Resource.Invalid_count_args));
                    return 1;
                }
                return Decoder.decode(args[1], args[2]); // 0 for success, 1 for error
            }
            else
            {
                System.Console.WriteLine(String.Format("{0} : {1}", PROGNAME, Resource.Invalid_options));
                return 1;
            }
        }

        /// <summary>
        /// Show the Usage
        /// </summary>
        static void ShowUsage()
        {
            System.Console.WriteLine(String.Format("Usage: TypeCobol.Transform {0}", HELP_OPTION));
            System.Console.WriteLine("\tor");
            System.Console.WriteLine(String.Format("Usage: TypeCobol.Transform {0} tcbl_input_path cbl_input_path cbl_output_path", CONCATENATE_OPTION));
            System.Console.WriteLine("\tor");
            System.Console.WriteLine(String.Format("Usage: TypeCobol.Transform {0} tcbl_input_path cbl_output_path", CONCATENATE_OPTION));
            System.Console.WriteLine("\tor");
            System.Console.WriteLine(String.Format("Usage: TypeCobol.Transform {0} tcl_cbl_input_path tcbl_output_path", DECODE_OPTION));
            System.Console.WriteLine("Options:");
            System.Console.WriteLine("\thelp : Shows this usage");
            System.Console.WriteLine(String.Format("\t{0} : The options to mix TypeCobol source code the the Generated Cobol in one file.", CONCATENATE_OPTION));
            System.Console.WriteLine(String.Format("\t{0} : The options to extract TypeCobol source form a mix of TypeCobol source code and the Generated cobol.", DECODE_OPTION));
            System.Console.WriteLine("Arguments:");
            System.Console.WriteLine("\tcbl_input_path : The complete full path to the Cobol 85 file if specified, an empty file is assumed otherwise.");
            System.Console.WriteLine("\ttcbl_input_path : The complete full path to the TypeCobol file.");
            System.Console.WriteLine("\tcbl_output_path : The complete full path to the Cobol 85 output file.");
            System.Console.WriteLine("\ttcl_cbl_input_path : The complete full path to the mixed file Typecobol + Cobol 85 source code.");
            System.Console.WriteLine("\ttcbl_output_path : The complete full path to the TypeCobol output file.");
        }
    }
}
