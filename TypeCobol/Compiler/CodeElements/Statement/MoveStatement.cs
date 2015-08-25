using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class MoveStatement : CodeElement
    {
        private Expression Sending;
        private IList<Identifier> Receiving;
        private bool IsCorresponding;

        public MoveStatement(Expression sending, IList<Identifier> receiving, bool corresponding)
            : base(CodeElementType.MoveStatement) {
            this.Sending   = sending;
            this.Receiving = receiving;
            this.IsCorresponding = corresponding;
        }
    }
}
