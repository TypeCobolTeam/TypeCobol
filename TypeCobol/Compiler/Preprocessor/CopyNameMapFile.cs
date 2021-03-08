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
    /// The Lookup is performed using  BinarySearch.
    /// </summary>
    public class CopyNameMapFile
    {
        private string[] _CpyCopyNames;        

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="filepath">The Copy list File Path to map</param>
        public CopyNameMapFile(String filepath) : this(System.IO.File.ReadAllLines(filepath))
        {         
        }

        /// <summary>
        /// Array constructor
        /// </summary>
        /// <param name="cpyCopyNames">The Array of Cpy Names</param>
        public CopyNameMapFile(string[] cpyCopyNames)
        {
            _CpyCopyNames = cpyCopyNames;
            for (int i = 0; i < _CpyCopyNames.Length; i++)
            {
                _CpyCopyNames[i] = _CpyCopyNames[i].ToUpper();
            }
            Array.Sort(_CpyCopyNames);
        }
        
        /// <summary>
        /// Check if the Given name corresponds to a CPY copy name. 
        /// A Binary search is performed, thus the underlying array must have been sorted.
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool HasCpyCopy(string name)
        {
            return _CpyCopyNames != null ? System.Array.BinarySearch(_CpyCopyNames, name.ToUpper()) >= 0 : false;
        }
    }
}
