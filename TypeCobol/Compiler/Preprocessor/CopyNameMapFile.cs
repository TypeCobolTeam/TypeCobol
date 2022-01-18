#if EUROINFO_RULES
using System;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Class that implements, mapping of CPY copy names.
    /// The Lookup is performed using  BinarySearch.
    /// </summary>
    public class CopyNameMapFile
    {
        private readonly string[] _cpyCopyNames;        

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="filepath">The Copy list File Path to map</param>
        public CopyNameMapFile(string filepath)
        {
            _cpyCopyNames = System.IO.File.ReadAllLines(filepath);
            for (int i = 0; i < _cpyCopyNames.Length; i++)
            {
                _cpyCopyNames[i] = _cpyCopyNames[i].ToUpper();
            }
            Array.Sort(_cpyCopyNames);
        }

        /// <summary>
        /// Check if the Given name corresponds to a CPY copy name. 
        /// A Binary search is performed, thus the underlying array must have been sorted.
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool Contains(string name)
        {
            return Array.BinarySearch(_cpyCopyNames, name.ToUpper()) >= 0;
        }
    }
}
#endif
