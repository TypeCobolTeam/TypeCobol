using System;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Test.Compiler.CodeElements
{
    class ArithmeticStatementTester
    {
        public string Dump(AddStatement statement)
        {
            StringBuilder s = new StringBuilder();
            foreach (var pair in statement.affectations)
            {
                s.AppendFormat("{0} = {1}, ", pair.Key.Text, pair.Value.ToString());
            }
            if (statement.affectations.Count > 0) s.Length -= 2;
            return s.ToString();
        }

        public void CompareWithRPN(TypeCobol.Compiler.Parser.SyntaxDocument tree, string[] rpn)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            foreach (TypeCobol.Compiler.CodeElements.CodeElement e in tree.CodeElements)
            {
                if (e.GetType() != typeof(AddStatement)) continue;
                AddStatement add = (TypeCobol.Compiler.CodeElements.AddStatement)e;
                string dump = Dump(add);
                if (c >= rpn.Length) errors.AppendFormat("RPN number {} not provided.", c);
                else if (dump != rpn[c]) errors.AppendFormat("{0}: \"{1}\", expected \"{2}\"\n", c, dump, rpn[c]);
                c++;
            }
            if (errors.Length > 0) throw new Exception(errors.ToString());
            if (c != rpn.Length) throw new Exception("Number of checks expected "+rpn.Length+" vs "+c);
        }
    }
}
