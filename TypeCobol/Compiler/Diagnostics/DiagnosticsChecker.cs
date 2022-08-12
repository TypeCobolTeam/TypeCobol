using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Diagnostics
{
    public class DiagnosticsChecker : AbstractAstVisitor
    {
        private readonly List<Diagnostic> _diagnostics;

        public DiagnosticsChecker(List<Diagnostic> diagnostics)
        {
            if (diagnostics == null)
                throw new ArgumentNullException(nameof(diagnostics), "Diagnostics list has not been initialized");

            _diagnostics = diagnostics;
        }

        public override bool BeginNode(Node node)
        {
            //Check node for any diagnostics
            if (node.Diagnostics != null)
                _diagnostics.AddRange(node.Diagnostics);

            return true;
        }

        public override bool BeginCodeElement(CodeElement codeElement)
        {
            //This checker is only for Node after the full AST has been created
            return false;
        }
    }
}
