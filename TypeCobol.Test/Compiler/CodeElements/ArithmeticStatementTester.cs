using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Test.Compiler.CodeElements
{
    class ArithmeticStatementTester
    {
        public string Dump(AddStatement statement)
        {
            string s = "";
            foreach (var pair in statement.affectations)
            {
                s += pair.Key.Text + " = " + statement.operations[pair.Value].ToString() + ", ";
            }
            if (statement.affectations.Count > 0) s = s.Remove(s.Length - 2);
            return s;
        }

        public void CompareWithRPN(TypeCobol.Compiler.Parser.SyntaxDocument tree, string[] rpn)
        {
            int c = 0;
            string errors = "";
            foreach (TypeCobol.Compiler.CodeElements.CodeElement e in tree.CodeElements)
            {
                if (e.GetType() != typeof(AddStatement)) continue;
                AddStatement add = (TypeCobol.Compiler.CodeElements.AddStatement)e;
                string dump = Dump(add);
                if (c >= rpn.Length) errors += ("No "+c+"th rpn provided.");
                else if (dump != rpn[c]) errors += (c + ": \""+dump+"\", expected \""+rpn[c]+"\"\n");
                c++;
            }
            if (errors.Length > 0) throw new Exception(errors);
            if (c != rpn.Length) throw new Exception("Number of checks expected "+rpn.Length+" vs "+c);
        }
    }
}
