using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// Interface of an entity that can be transpilated to source code.
    /// </summary>
    public interface ITranspilable
    {        
        /// <summary>
        /// Get the Transpiled code.
        /// </summary>
        String TranspiledCode
        {
            get;
        }
    }
}
