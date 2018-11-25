using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Tools.APIHelpers
{
   
    /// <summary>
    /// Helper for custom legacy behavior at Euro-Information
    /// </summary>
    public class LTNVHelper {

        /// <summary>
        /// For each program inside the source file, return all their LTNV Copy and the level 01 variable associated
        /// </summary>
        /// <param name="sourceFile"></param>
        /// <returns>Key  = program inside source file, 
        ///          Value= Dictionnary with Key = COPY Name, Value = parameter name (level 01 variable)</returns>
        public static IDictionary<Program, IDictionary<string, string>> GetLTNVCopy([NotNull] SourceFile sourceFile) {
            var result = new Dictionary<Program, IDictionary<string, string>>();

            foreach (var program in sourceFile.Programs) {
                result.AddRangeWithoutCheckingDuplicate(GetLTNVCopy(program));
            }
            return result;
        }


        /// <summary>
        /// For each program and nested programs, return all their LTNV Copy and the level 01 variable associated
        /// 
        /// </summary>
        /// <param name="program"></param>
        /// <returns></returns>
        protected static IDictionary<Program, IDictionary<string, string>> GetLTNVCopy([NotNull] Program program) {
            var result = new Dictionary<Program, IDictionary<string, string>>();

            result.Add(program, GetLTNVCopyForOneProgram(program));

            if (program.NestedPrograms != null) {
                foreach (var nestedProgram in program.NestedPrograms) {
                    result.AddRangeWithoutCheckingDuplicate(GetLTNVCopy(nestedProgram));
                }
            }

            return result;
        }

        /// <summary>
        /// Ex:
        /// For copy YDVBE1L with a level 01 DVBE01,  it returns {YDVEB1L, DVBE01}
        /// For copy YDVBFAW with a level 01 YDVBFAL, it returns {YDVBFAW, YDVBFAL}
        /// </summary>
        /// <param name="program"></param>
        /// <returns>Dictionnary with Key = COPY Name, Value = parameter name (level 01 variable)</returns>
        public static IDictionary<string, string> GetLTNVCopyForOneProgram([NotNull] Program program) {
            var result = new Dictionary<string, string>();


            //Structure of LTNV copy
            //01 YxxxFAL.
            //   02 YxxxFAL-DD.
            //    05 FILX0000
            //   02 YxxxFAL-LGMAX-LTNV
            var variablesLTNV = program.SymbolTable.GetVariablesExplicit(new URI("FILX0000"));
            foreach (var ltnvVar in variablesLTNV) {
                if(!ltnvVar.IsInsideCopy() || (ltnvVar.CodeElement)?.LevelNumber?.Value != 05) 
                    continue; //A developer who use "FILX0000" as a variable name? Weird but ignore it
                var parent02_DD = ltnvVar.Parent as DataDefinition;
                if ((parent02_DD?.CodeElement)?.LevelNumber?.Value != 02)
                    continue; //Not a valid LTNV structure


                var parent01 = parent02_DD.Parent as DataDefinition;

                var parent01CE = parent01?.CodeElement as DataDefinitionEntry;
                if (parent01CE?.LevelNumber?.Value != 01) {
                    continue; //Not a valid LTNV structure
                }


                //Remove "-DD" suffix of the name
                var parameterName = parent02_DD.Name.Substring(0, parent02_DD.Name.Length - 3);

                result.Add(parent02_DD.CodeElement.FirstCopyDirective.TextName, parameterName);
            }

            return result;
        }
    }
}
