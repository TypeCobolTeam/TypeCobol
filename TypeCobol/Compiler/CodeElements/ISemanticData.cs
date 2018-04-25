using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Enumeration on the kind of semantic data holded by the SemantidData property if nont null.
    /// </summary>
    public enum SemanticKinds
    {
        Type,
        Symbol
    }

    /// <summary>
    /// Interface of any semantic data
    /// </summary>
    public interface ISemanticData
    {
        /// <summary>
        /// Get The Kind of Semantid data
        /// </summary>
        SemanticKinds SemanticKind { get; }
    }
}

