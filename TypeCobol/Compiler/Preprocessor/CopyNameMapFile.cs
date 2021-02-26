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
    /// <summary>
    /// Class that implements, mapping of CPY copy names.
    /// The Lookup is performed using  BinarySearch, thus it necessary for the underlying array of names to be sorted.
    /// ALL COPY NAMES MUST BE IN UPPERCASE.
    /// </summary>
    public class CopyNameMapFile
    {
        private string[] _CpyCopyNames;        

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="filepath">The Copy list File Path to map</param>
        /// <param name="bSort">true to sort the resulting array of Copy names, false otherwise.</param>
        public CopyNameMapFile(String filepath, bool bSort = false)
        {
            _CpyCopyNames = System.IO.File.ReadAllLines(filepath);
            if (_CpyCopyNames != null && bSort)
                Array.Sort(_CpyCopyNames);
        }

        /// <summary>
        /// Array constructor
        /// </summary>
        /// <param name="cpyCopyNames">The Array of Cpy Names</param>
        /// <param name="bSort">true to sort the array</param>
        public CopyNameMapFile(string[] cpyCopyNames, bool bSort = false)
        {
            _CpyCopyNames = cpyCopyNames;
            if (_CpyCopyNames != null && bSort)
                Array.Sort(_CpyCopyNames);
        }

        /// <summary>
        /// Get the singleton instance. Create one if it does not exists
        /// </summary>
        /// <returns>The singleton instance</returns>
        public static CopyNameMapFile Singleton { get; set; }
        
        /// <summary>
        /// Check if the Given name corresponds to a CPY copy name. 
        /// A Binary search is performed, thus the underlying array must have been sorted.
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool IsCpyCopy(string name)
        {
            return _CpyCopyNames != null ? System.Array.BinarySearch(_CpyCopyNames, name.ToUpper()) >= 0 : false;
        }

        /// <summary>
        /// Check if using the current Instance, the Given name corresponds to a CPY copy name. 
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public static bool HasCpyCopy(string name)
        {
            return Singleton != null ? Singleton.IsCpyCopy(name) : false;
        }
    }
}
