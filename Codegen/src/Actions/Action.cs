using System;
using System.Collections.Generic;

namespace TypeCobol.Codegen.Actions
{
    public interface Action
    {
        string Group { get; }
        /// <summary>Modifies AST.</summary>
        IList<Action> Execute();
    }
}
