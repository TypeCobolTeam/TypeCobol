using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The REPOSITORY paragraph is used in a program or class definition to identify all
    /// the object-oriented classes that are intended to be referenced in that program or
    /// class definition. It also allows declaration of intrinsic function names that may be used without
    /// specifying the keyword FUNCTION.
    /// </summary>
    public class RepositoryParagraph : CodeElement
    {
        // Intrinsic functions which are not allowed in the REPOSITORY paragraph
        internal static readonly HashSet<string> NotAllowedIntrinsicFunctions = new(StringComparer.OrdinalIgnoreCase) { "WHEN-COMPILED" };

        public RepositoryParagraph() : base(CodeElementType.RepositoryParagraph)
        { }

        /// <summary>
        /// Optionally, the REPOSITORY paragraph defines associations between class-names and external class-names.
        /// </summary>
        public IDictionary<SymbolDefinitionOrReference, SymbolDefinitionOrReference> ClassNames { get; set; }

        /// <summary>
        /// Optionally, the REPOSITORY paragraph allows to declare all intrinsic functions
        /// </summary>
        public SyntaxProperty<bool> IsAllIntrinsicFunctions { get; set; }

        /// <summary>
        /// Optionally, the REPOSITORY paragraph allows to declare one or several intrinsic functions
        /// </summary>
        public List<SymbolDefinitionOrReference> IntrinsicFunctions { get; set; }
    }
}
