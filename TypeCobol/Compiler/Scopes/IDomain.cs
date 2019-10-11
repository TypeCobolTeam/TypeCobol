using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// Interface of a Domain. A Domain is in fact a Set of Symbol Set.
    /// </summary>
    interface IDomain<T> where T : Symbol
    {
        /// <summary>
        /// Add an element to the Domain
        /// </summary>
        /// <param name="element">The element to be added to the domain</param>
        /// <returns>The added element</returns>
        T Add(T element);
        /// <summary>
        /// Get the Scope of symbol associated to the given symbol name.
        /// </summary>
        /// <param name="path">The Symbol's path to get the Scope, the path is in reverse order à la COBOL. The path must be in lower case</param>
        /// <returns>The Multi Symbol set of all symbol corresponding to the given path.</returns>
        Scope<T>.MultiSymbols Get(string[] path);
    }
}