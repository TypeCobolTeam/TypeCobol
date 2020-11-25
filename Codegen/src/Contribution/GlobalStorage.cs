using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Generators;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Contribution
{
    /// <summary>
    /// The contribution to add the Global Storage Data to the programs that need them
    /// </summary>
    public class GlobalStorage : IContribute
    {
        internal class GlobalStorageNode : GeneratedNode, IGeneratorContext
        {
            public IGenerator Generator
            {get; set; }

            public SourceText SourceTextBuffer
            { get;  set; }

            public bool IsGlobalSourceTextBuffer
            { get; set; }

            public GlobalStorageNode() : base(null, true)
            {
            }            

            public override IEnumerable<ITextLine> Lines
            {
                get
                {
                    if (Text == null && _cache == null)
                    {
                        if (Generator is DefaultGeneratorWithLineMap defGenLM && defGenLM.CurrentLineMappinCtx != null)
                        {
                            int add = this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine) ? 1 : 0;
                            if (defGenLM.CurrentLineMappinCtx.funData != null)
                            {   //Function have their own buffer
                                int count = defGenLM.CurrentLineMappinCtx.funData.FunctionDeclBuffer.LineCount(0, defGenLM.CurrentLineMappinCtx.funData.FunctionDeclBuffer.Size);
                                defGenLM.CurrentLineMappinCtx.funData.GlobalStorageLineDelta = count + add;
                            }
                            else
                            {
                                int count = IsGlobalSourceTextBuffer ? 0 : defGenLM.CurrentLineMappinCtx.startLineMapCounter;
                                if (this.SourceTextBuffer != null)
                                {
                                    count += this.SourceTextBuffer.LineCount(0, this.SourceTextBuffer.Size) + 1;
                                }
                                defGenLM.CurrentLineMappinCtx.AddGlobalStorageLineDelta(count + add);
                            }
                        }
                        if (Generator is DefaultGenerator defGen)
                        {
                            Text = defGen.GlobalStorageData ?? "";

                            _cache = new List<ITextLine>();
                            if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                                _cache.Add(new TextLineSnapshot(-1, "", null));
                            foreach (string line in Text.Split(new string[] { Environment.NewLine, "\n", "\r" }, StringSplitOptions.None))
                            {
                                _cache.Add(new CobolTextLine(new TextLineSnapshot(-1, line, null), ColumnsLayout.FreeTextFormat));
                            }
                        }
                    }
                    return _cache;
                }
            }           
        }
        public Node Contribute(Node parent, string pattern, string code, string @group, int? position, bool newline)
        {
            return new GlobalStorageNode();
        }
    }
}
