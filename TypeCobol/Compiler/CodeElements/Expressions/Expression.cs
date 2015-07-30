using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{

    public abstract class Expression { }


    //TODO this class is only use for display literals for now.
    //osmedile: I'm not sure if literal must inherits from Expression (so Literal and identifier can be seen as the same object) 
    //if this class can inherits from Expression, rename it to "Literal"
    public class LiteralForDisplay : Expression
    {
        public Token token { get; set; }
        public LiteralForDisplay(Token token)
        {
            this.token = token;
        }
        public override string ToString() { return token.Text; }
    }
}
