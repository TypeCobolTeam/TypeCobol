using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// Generator than create the documentation of all applicable Nodes
    /// </summary>
    class DocumentationGenerator : IGenerator
    {
        private StringBuilder Destination { get; set; }
        public DocumentationGenerator(StringBuilder destination, string typeCobolVersion)
        {
            this.Destination = destination;
            TypeCobolVersion = typeCobolVersion;
        }
        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            Destination.Append("");
            //Add version to output file
            if (!string.IsNullOrEmpty(TypeCobolVersion))
                Destination.AppendLine("      *TypeCobol_Version:" + TypeCobolVersion);

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            var docBuilder = new DocumentationBuilder();
            sourceFile.AcceptASTVisitor(docBuilder);

            // For now serialize as JSON
            foreach (var doc in docBuilder.DTOs)
            {
                Destination.Append(doc.SerializeToJSON());
            }
        }

        public List<Diagnostic> Diagnostics { get; set; }
        public string TypeCobolVersion { get; set; }
    }

    class DocumentationBuilder : AbstractAstVisitor
    {
        public List<Documentation> DTOs;

        public DocumentationBuilder()
        {
            DTOs = new List<Documentation>();
        }
        public override bool Visit(TypeDefinition node)
        {
            DataTypeDescriptionEntry ce = node.CodeElement as DataTypeDescriptionEntry;
            if (ce != null && ce.Visibility == AccessModifier.Public)
                DTOs.Add(new DocumentationForType(node));
            return true;
        }
        public override bool Visit(FunctionDeclaration node)
        {
            FunctionDeclarationHeader ce = node.CodeElement as FunctionDeclarationHeader;
            if (ce != null && ce.Visibility == AccessModifier.Public)
                DTOs.Add(new DocumentationForFunction(node));
            return true;
        }
        public override bool Visit(Program node)
        {
            DTOs.Add(new DocumentationForProgram(node));
            return true;
        }

    }
}
