using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    public class MixedTransformGenerator : DefaultGenerator
    {
        public MixedTransformGenerator(CompilationDocument document, StringBuilder destination, List<Skeleton> skeletons, string typeCobolVersion) 
            : base(document, destination, skeletons, typeCobolVersion)
        {
           
            
        }

        protected override bool Process(Node node)
        {
            throw new NotImplementedException();
        }

        public override void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            base.Generate(compilationUnit, columns);

            //After generation process based on Default CObol85 generator do the transform
            var mixedContent = new StringBuilder();
            Transform.Decoder.Encode(compilationUnit.CobolTextLines.Select(l => l.Text).ToArray(), Destination.ToString().Split(new string[] {Environment.NewLine}, StringSplitOptions.None), mixedContent);
            this.Destination.Clear();
            this.Destination.Append(mixedContent);
        }
    }
}
