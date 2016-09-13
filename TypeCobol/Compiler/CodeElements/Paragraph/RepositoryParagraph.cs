using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The REPOSITORY paragraph is used in a program or class definition to identify all
    /// the object-oriented classes that are intended to be referenced in that program or
    /// class definition.
    /// </summary>
    public class RepositoryParagraph : CodeElement
    {
        public RepositoryParagraph() : base(CodeElementType.RepositoryParagraph)
        { }

        /// <summary>
        /// Optionally, the REPOSITORY paragraph defines associations between class-names and external class-names.
        /// </summary>
        public IDictionary<ClassName, string> ClassNames { get; set; }
    }
}
