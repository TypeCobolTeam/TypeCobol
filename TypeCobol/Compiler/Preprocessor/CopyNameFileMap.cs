using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Compiler.Preprocessor
{
    public class CopyNameFileMap
    {
        private string[] _CpyCopyNames;
        private static CopyNameFileMap _Singleton;
        private const string DEFAULT_COPY_FILE_NAME = "COPIES_CPY.txt";

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="filepath">The Copy list File Path to map</param>
        /// <param name="bSort">true to sort the array, false otherwise.</param>
        public CopyNameFileMap(String filepath = DEFAULT_COPY_FILE_NAME, bool bSort = false)
        {            
            try
            {
                _CpyCopyNames = System.IO.File.ReadAllLines(filepath);
                if (_CpyCopyNames != null && bSort)
                    Array.Sort(_CpyCopyNames);                
            }            
            catch (Exception e)
            {//Log the exception
                System.IO.File.AppendAllText(TypeCobolConfiguration.LogFileName, e.ToString());
            }
        }

        /// <summary>
        /// Get the singleton instance. Create one if it does not exists
        /// </summary>
        /// <returns>The singleton instance</returns>
        public static CopyNameFileMap getInstance()
        {
            return _Singleton ?? (_Singleton = new CopyNameFileMap());
        }

        /// <summary>
        /// Clear the singleton instance.
        /// </summary>
        public static void cleartInstance()
        {
            _Singleton = null;
        }

        /// <summary>
        /// Set the singleton instance to be used.
        /// </summary>
        /// <param name="instance">The singleton instance</param>
        public static void setInstance(CopyNameFileMap instance)
        {
            _Singleton = instance;
        }

        /// <summary>
        /// Determine if this instance is valid.
        /// </summary>
        public bool IsValid => _CpyCopyNames != null;

        /// <summary>
        /// Check if the Given name corresponds to a CPY copy name.
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool IsCpyCopy(string name)
        {
            return _CpyCopyNames != null ? System.Array.BinarySearch(_CpyCopyNames, name) >= 0 : false;
        }

        /// <summary>
        /// Check a Copy Directive
        /// </summary>
        /// <param name="copyDirective"></param>
        /// <param name="bReplacing">true if it is a REPLACING false otherwise</param>
        /// <returns>true if ok or if there is no COPY file map, false if a diagnostic has been emitted.</returns>
        public static bool CheckCopyDirective(CopyDirective copyDirective, TypeCobol.Compiler.CupCommon.QualifiedTextName qualifiedTextName, bool bReplacing)
        {
            CopyNameFileMap instance = getInstance();
            if (instance.IsValid)
            {
                string msg = null;
                if (!instance.IsCpyCopy(copyDirective.TextName))
                {
                    if (!bReplacing)
                    {
                        msg = "Copy " + copyDirective.TextName + " of type CPX(or CPM) should be declared using REPLACING clause.";
                    }
                } else if (bReplacing)
                {
                    msg = "Copy " + copyDirective.TextName + " of type CPY cannot be declared using REPLACING clause.";
                }
                if (msg != null)
                {
                    Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, qualifiedTextName.TextName.Column,
                    qualifiedTextName.TextName.EndColumn, qualifiedTextName.TextName.Line, msg);
                    copyDirective.AddDiagnostic(error);
                    return false;
                }
            }
            return true;
        }
    }
}
