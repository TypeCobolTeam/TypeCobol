using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Diagnostics
{
    public class DiagnosticsChecker : AbstractAstVisitor
    {
        private List<Diagnostic> _Diagnostic;
        public DiagnosticsChecker(List<Diagnostic> diagnostics)
        {
            _Diagnostic = diagnostics;
        }

        public override bool BeginNode(Node node)
        {
            //Check node for any diagnostics
            if (node.Diagnostics != null)
                AddDiagnostics(node.Diagnostics);

            return true;
        }

        public override bool BeginCodeElement(CodeElement codeElement)
        {
            //This checker is only for Node after the full AST has been created
            return false;
        }

        /// <summary>
        /// Add diagnostic to the list of diag, also check if diagnostic does not exists in the list
        /// </summary>
        /// <param name="diagnostic"></param>
        private void AddDiagnostic(Diagnostic diagnostic)
        {
            if (_Diagnostic == null)
                throw new Exception("Diagnostics list has not been intialized");

            if (!_Diagnostic.Contains(diagnostic))
                _Diagnostic.Add(diagnostic);
        }

        /// <summary>
        /// Add a range of diagnostic, also check if a diagnostic is already present in the list
        /// </summary>
        /// <param name="diagnostics"></param>
        private void AddDiagnostics(List<Diagnostic> diagnostics)
        {
            foreach (var diagnostic in diagnostics)
            {
                AddDiagnostic(diagnostic);
            }
        }
    }
}
