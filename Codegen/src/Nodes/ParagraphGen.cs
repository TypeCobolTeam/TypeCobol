using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes
{

    public class ParagraphGen : Paragraph, Generated
    {
        public string ParagraphName { get; set; }

        public ParagraphGen(string paragraphName) : base(null) {
            ParagraphName = paragraphName;
        }
        private IList<ITextLine> _cache = null;
        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    _cache = new List<ITextLine>();
                    _cache.Add(new TextLineSnapshot(-1, ParagraphName + ".", null));
                }
                return _cache;
            }
        }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        public bool IsLeaf { get { return false; } }
    }
    
}
