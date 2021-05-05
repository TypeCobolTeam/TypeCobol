using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class TestExpressions
    {
        /// <summary>
        /// Issue #1939, check the Expression trees
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckExpressionTrees()
        {
            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var fileName = "Expressions";
            var compilationUnit = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.CrossCheck);
            var root = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root;

            // TODO: Correct the error on switch conditions and set the number of diagnostics to 0
            var diagnostics = compilationUnit.AllDiagnostics();
            Assert.AreEqual(1, diagnostics.Count);
            Assert.AreEqual("Line 169[15,25] <30, Error, Semantics> - Semantic error: Symbol MYSWITCH-ON is not referenced", diagnostics[0].ToString());

            // Get all expressions
            var expCollector = new ExpressionCollector();
            root.AcceptASTVisitor(expCollector);

            // Create the expression trees in a string
            var strToString = new StringBuilder();
            foreach (var expression in expCollector.Expressions)
            {
                strToString.AppendLine(ExpressionToTree(expression));
                strToString.AppendLine("________________________________________");
                // Ensure an expression is equivalent to itself
                Assert.IsTrue(expression.IsEquivalent(expression));
            }

            var expectedPath = Path.ChangeExtension(Path.Combine("Misc", "Expressions-expected"), "txt");
            var expected = File.ReadAllText(expectedPath);
            // ensure the string and the file are the same 
            TestUtils.compareLines("CheckExpressions", strToString.ToString(), expected, PlatformUtils.GetPathForProjectFile(expectedPath));
        }

        /// <summary>
        /// Return the tree of the expression in form of a string
        /// </summary>
        private static string ExpressionToTree(Expression expression, int depth = 0)
        {
            if (expression == null) return string.Empty;
            var (expA, expB) = expression.GetOperands();
            var paddedExpression = $"{string.Empty.PadLeft(depth * 4)}{expression.NodeType}: {expression}";

            if (expA == null && expB == null)
            {
                return paddedExpression;
            }

            if (expA != null && expB != null)
            {
                return $"{paddedExpression} →\n{ExpressionToTree(expA, depth + 1)}\n{ExpressionToTree(expB, depth + 1)}";
            }

            return $"{paddedExpression} →\n{(expA == null ? ExpressionToTree(expB, depth + 1) : ExpressionToTree(expA, depth + 1))}";
        }

        /// <summary>
        /// Visit the nodes and return all top-most expressions found
        /// </summary>
        private class ExpressionCollector : AbstractAstVisitor
        {
            /// <summary>
            /// List of all top-most expressions found
            /// </summary>
            public readonly List<Expression> Expressions = new List<Expression>();

            public override bool Visit(Expression expression)
            {
                Expressions.Add(expression);
                // False to avoid having children expressions
                return false;
            }
        }
    }
}