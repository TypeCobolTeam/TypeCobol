using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    class Node : CodeElement {
        private IList<CodeElement> children_ = new List<CodeElement>();
        public IList<CodeElement> Children {
            get { return new System.Collections.ObjectModel.ReadOnlyCollection<CodeElement>(children_); }
            private set { throw new System.InvalidOperationException(); }
        }

        public Node(CodeElementType type) : base(type) { }

        internal void Add(CodeElement child) {
            children_.Add(child);
            child.Parent = this;
        }
    }

    class Sentence : Node {
        public Sentence() : base(CodeElementType.ContinueStatement) { } //TODO CodeElementType.Sentence
    }

    class Paragraph : Node {
        public Paragraph() : base(CodeElementType.ContinueStatement) { } //TODO CodeElementType.Paragraph
    }

    class Section : Node {
        public Section() : base(CodeElementType.ContinueStatement) { } //TODO CodeElementType.Section
    }
}
