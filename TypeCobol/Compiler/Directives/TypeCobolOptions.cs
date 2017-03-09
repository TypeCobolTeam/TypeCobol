﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Directives
{
    /// <summary>
    /// TypeCobol compiler options (superset of the IBM Enterprise Cobol compiler options)
    /// </summary>
    public class TypeCobolOptions : IBMCompilerOptions
    {
        // insert options specific to TypeCobol here ...
#if EUROINFO_LEGACY_REPLACING_SYNTAX
        public bool AutoRemarksEnable { get; set; }
#endif
        public bool HaltOnMissingCopy { get; set; }

        /// <summary>
        /// Clone the compiler options to enable specific parameters for each file
        /// </summary>
        public TypeCobolOptions Clone()
        {
            TypeCobolOptions newOptions = new TypeCobolOptions();
            this.CopyIBMOptionsTo(newOptions);
            return newOptions;
        }
    }
}
