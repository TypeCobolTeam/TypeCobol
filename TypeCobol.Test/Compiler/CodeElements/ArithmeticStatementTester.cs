using System;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Test.Compiler.CodeElements
{
    class ArithmeticStatementTester
    {
        public void Dump(StringBuilder output, AddStatement statement)
        {
            foreach (var pair in statement.affectations)
            {
                output.AppendFormat("{0} = {1}, ", pair.Key.Text, pair.Value.ToString());
            }
            if (statement.affectations.Count > 0) output.Length -= 2;
        }

        public void CompareWithRPN(TypeCobol.Compiler.Parser.SyntaxDocument tree, string[] rpn)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            foreach (TypeCobol.Compiler.CodeElements.CodeElement e in tree.CodeElements)
            {
                if (e.GetType() != typeof(AddStatement)) continue;
                AddStatement add = (TypeCobol.Compiler.CodeElements.AddStatement)e;
                StringBuilder s = new StringBuilder();
                Dump(s, add);
                string dump = s.ToString();
                if (c >= rpn.Length) errors.AppendFormat("RPN number {} not provided.", c);
                else if (dump != rpn[c]) errors.AppendFormat("{0}: \"{1}\", expected \"{2}\"\n", c, dump, rpn[c]);
                c++;
            }
            if (errors.Length > 0) throw new Exception(errors.ToString());
            if (c != rpn.Length) throw new Exception("Number of checks expected "+rpn.Length+" vs "+c);
        }
    }
}
