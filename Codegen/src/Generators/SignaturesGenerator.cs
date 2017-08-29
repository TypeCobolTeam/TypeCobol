using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Diagnostics;
using DataDivision = TypeCobol.Compiler.Nodes.DataDivision;
using FunctionDeclaration = TypeCobol.Compiler.Nodes.FunctionDeclaration;
using LocalStorageSection = TypeCobol.Compiler.Nodes.LocalStorageSection;
using ProcedureDivision = TypeCobol.Compiler.Nodes.ProcedureDivision;
using WorkingStorageSection = TypeCobol.Compiler.Nodes.WorkingStorageSection;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class SignaturesGenerator : IGenerator
    {
        private TextWriter Destination { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public SignaturesGenerator(TextWriter destination) {
            this.Destination = destination;
        }
   


        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
            Destination.Write("");

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            sourceFile.AcceptASTVisitor(new ExportToDependency());
            var lines = sourceFile.SelfAndChildrenLines;
            foreach (var textLine in lines)
            {
                Destination.WriteLine(textLine.Text);
            }
            Destination.Flush();
            Destination.Close();


        }

        public List<Diagnostic> Diagnostics { get; }
    }



    public class ExportToDependency : AbstractAstVisitor
    {

        private bool _isInsideProcedureDivision = false;

        public override bool IsStopVisitingChildren
        {
            get { return false; }
        }


        public override bool BeginNode(Node node)
        {
            if (_isInsideProcedureDivision && !(node is FunctionDeclaration) && !(node is End))
            {
                node.Remove();
                return false;
            }
            return true;
        }


        public override bool Visit(FunctionDeclaration node)
        {
            if (node.CodeElement().Visibility != AccessModifier.Public) {
                node.Remove();
                return false;
            }
            //Note : There is only 1 FunctionEnd
            var functionEnds = node.GetChildren<FunctionEnd>();
            node.RemoveAllChildren();
            foreach (var child in functionEnds)
            {
                node.Add(child);
            }

            return false;
        }


        public override bool Visit(DataDivision dataDivision)
        {
            if (_isInsideProcedureDivision) {
                dataDivision.Remove();
                return false;
            }
               
            
            return true;
        }

        public override bool Visit(LocalStorageSection localStorageSection) {
            RemoveNonPublicDataDeclaration(localStorageSection);
            return false;
        }
        public override bool Visit(WorkingStorageSection workingStorageSection) {
            RemoveNonPublicDataDeclaration(workingStorageSection);
            return false;
        }

        private void RemoveNonPublicDataDeclaration(Node parent) {
            //keep all public type
            var children = parent.GetChildren<Node>();
            foreach (var child in children)
            {
                var typeDefinition = child as TypeDefinition;
                if (typeDefinition == null || typeDefinition.CodeElement().Visibility != AccessModifier.Public)
                {
                    child.Remove();
                }
            }
        }

        public override bool Visit(ProcedureDivision procedureDivision)
        {
            _isInsideProcedureDivision = true;
            return true;
        }


    }
}
