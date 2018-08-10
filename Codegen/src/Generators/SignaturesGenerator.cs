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
using System.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class SignaturesGenerator : IGenerator
    {
        private StringBuilder Destination { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        /// <param name="typeCobolVersion">Version of the TypeCobol Parser/Generator</param>
        public SignaturesGenerator(StringBuilder destination, string typeCobolVersion) {
            this.Destination = destination;
            TypeCobolVersion = typeCobolVersion;
        }
   


        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
            Destination.Append("");
            //Add version to output file
            if (!string.IsNullOrEmpty(TypeCobolVersion))
                Destination.AppendLine("      *TypeCobol_Version:" + TypeCobolVersion);

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            sourceFile.AcceptASTVisitor(new ExportToDependency());
            var lines = sourceFile.SelfAndChildrenLines;
            foreach (var textLine in lines)
            {
                Destination.AppendLine(textLine.Text);
            }
        }

        public List<Diagnostic> Diagnostics { get; }
        public string TypeCobolVersion { get; set; }
    }



    public class ExportToDependency : AbstractAstVisitor
    {

        private bool _isInsideProcedureDivision = false;

        public override bool IsStopVisitingChildren
        {
            get { return false; }
        }

        /// <summary>
        /// CodeGen visitor can modify Node's children
        /// </summary>
        public override bool CanModifyChildrenNode => true;

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

        /// <summary>
        /// Check the Linkage section for private variables
        /// </summary>
        /// <param name="linkageSection"></param>
        /// <returns></returns>
        public override bool Visit(LinkageSection linkageSection)
        {
            RemoveNonPublicDataDeclaration(linkageSection);
            //remove linkage section if it doesn't contain any children
            if (linkageSection.Children.Count == 0)
            {
                linkageSection.Remove();
            }
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
