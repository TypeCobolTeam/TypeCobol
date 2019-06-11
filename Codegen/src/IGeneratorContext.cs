using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// Interface of any instance that can have is Generated context to be set.
    /// </summary>
    public interface IGeneratorContext
    {
        IGenerator Generator { set; }
    }
}
