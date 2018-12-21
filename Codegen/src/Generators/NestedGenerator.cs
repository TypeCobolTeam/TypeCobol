using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    public class NestedGenerator : DefaultGenerator
    {
        public NestedGenerator(CompilationDocument document, StringBuilder destination, List<Skeleton> skeletons, string typeCobolVersion) : base(document, destination, skeletons, typeCobolVersion)
        {
        }

        public override void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            sourceFile.AcceptASTVisitor(new TransformToNested());

            base.Generate(compilationUnit, columns);
        }
    }

    public class TransformToNested : AbstractAstVisitor
    {
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
            return true;
        }


        public override bool Visit(FunctionDeclaration node)
        {
            //Specify that all FunctionDeclaration should be generated as nested programs
            node.SetFlag(Node.Flag.GenerateAsNested, true);

            return false;
        }
    }
}
