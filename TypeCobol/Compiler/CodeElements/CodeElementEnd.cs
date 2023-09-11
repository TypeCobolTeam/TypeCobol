using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements {

    public abstract class CodeElementEnd: CodeElement {
        protected CodeElementEnd(CodeElementType type) : base(type) { }

        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.Unspecified;
    }

}
