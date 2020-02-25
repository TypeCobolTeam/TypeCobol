using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    public class NestedGenerator : DefaultGenerator
    {
        public NestedGenerator(CompilationDocument document, StringBuilder destination, string typeCobolVersion)
            : base(document, destination, typeCobolVersion)
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
            //Temporary fix, see PR #1215. Should be set on node. 
            node.Root.MainProgram.SetFlag(Node.Flag.GenerateAsNested, true);

            return false;
        }
    }
}
