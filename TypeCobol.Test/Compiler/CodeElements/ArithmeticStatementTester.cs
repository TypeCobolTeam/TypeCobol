using System;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Test.Compiler.CodeElements
{
    class ArithmeticStatementTester
    {
        public void Dump(StringBuilder output, ArithmeticOperationStatement statement)
        {
            foreach (var pair in statement.affectations)
            {
                output.AppendFormat("{0} = {1}, ", pair.Key.Symbol.NameToken.Text, pair.Value.ToString());
            }
            if (statement.affectations.Count > 0) output.Length -= 2;
        }

        public void CompareWithRPN(TypeCobol.Compiler.Parser.SyntaxDocument tree, string[] rpn)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            foreach (TypeCobol.Compiler.CodeElements.CodeElement e in tree.CodeElements)
            {
                if (e.GetType() != typeof(AddStatement) && e.GetType() != typeof(SubtractStatement)) continue;
                ArithmeticOperationStatement statement = (TypeCobol.Compiler.CodeElements.ArithmeticOperationStatement)e;
                StringBuilder s = new StringBuilder();
                Dump(s, statement);
                string dump = s.ToString();
                if (c >= rpn.Length) errors.AppendFormat("RPN number {0} not provided.", c);
                else if (dump != rpn[c]) errors.AppendFormat("{0}: \"{1}\", expected \"{2}\"\n", c, dump, rpn[c]);
                c++;
            }
            if (errors.Length > 0) throw new Exception(errors.ToString());
            if (c != rpn.Length) throw new Exception("Number of tests="+c+" vs expected="+rpn.Length);
        }

        public void CompareWithRPNFile(TypeCobol.Compiler.Parser.SyntaxDocument tree, string path)
        {
            using (System.IO.StreamReader reader = new System.IO.StreamReader(PlatformUtils.GetStreamForProjectFile(path)))
            {
                System.Collections.Generic.List<string> lines = new System.Collections.Generic.List<string>();
                string line = reader.ReadLine();
                while (line != null)
                {
                    lines.Add(line);
                    line = reader.ReadLine();
                }
                ArithmeticStatementTester tester = new ArithmeticStatementTester();
                tester.CompareWithRPN(tree, lines.ToArray());
            }
        }
    }
}
