using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    public class AddStatement : CodeElement
    {
        public AddStatement() : base(CodeElementType.AddStatement)
        { 
            First = new List<Operand>();
            Second = new List<Operand>();
            Corresponding = false;
        }

        public bool Corresponding { get; set; }

        public IList<Operand> First { get; set; }
        public IList<Operand> Second { get; set; }
        public IList<Operand> Third { get; set; }
    }

    public class Operand
    {
        public Operand() { }

        public Token Value { get; set; }
        public bool Rounded { get; set; }
    }
}
