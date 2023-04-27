using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
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
        public void CheckExpressionEquivalence()
        {
            var folder = Path.Combine("Misc");
            var fileName = "ExpressionsEquivalence";
            var compilationUnit = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.SemanticCrossCheck);
            var root = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root;

            // Get all expressions
            var expCollector = new ExpressionCollector();
            root.AcceptASTVisitor(expCollector);
            var expressionList = expCollector.Expressions;

            // Relational
            var firstRelational = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.RelationCondition);
            AssertEquivalence(0, 1, firstRelational);
            AssertNotEquivalent(1, 2, firstRelational);
            AssertEquivalence(2, 3, firstRelational);
            AssertNotEquivalent(3, 4, firstRelational);
            AssertNotEquivalent(4, 5, firstRelational);
            AssertNotEquivalent(4, 6, firstRelational);
            AssertEquivalence(5, 6, firstRelational);

            // Logical
            var firstLogical = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.LogicalOperation);
            AssertEquivalence(0, 1, firstLogical);
            AssertNotEquivalent(1, 2, firstLogical);
            AssertNotEquivalent(2, 3, firstLogical);

            // Arithmetic
            var firstArithmetic = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.ArithmeticOperation);
            AssertEquivalence(0, 1, firstArithmetic);
            AssertNotEquivalent(1, 2, firstArithmetic);
            AssertNotEquivalent(2, 3, firstArithmetic);

            // Sign
            var firstSign = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.SignCondition);
            AssertEquivalence(0, 1, firstSign);
            AssertNotEquivalent(1, 2, firstSign);
            AssertEquivalence(2, 3, firstSign);
            AssertNotEquivalent(3, 4, firstSign);
            AssertNotEquivalent(4, 5, firstSign);

            // Class
            var firstClass = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.ClassCondition);
            AssertEquivalence(0, 1, firstClass);
            AssertNotEquivalent(1, 2, firstClass);
            AssertNotEquivalent(2, 3, firstClass);
            AssertNotEquivalent(3, 4, firstClass);
            AssertNotEquivalent(4, 5, firstClass);
            AssertNotEquivalent(5, 6, firstClass);

            // ConditionNameConditionOrSwitchStatusCondition
            var firstConditionName = expressionList.FindIndex(e => e.NodeType == ExpressionNodeType.ConditionNameConditionOrSwitchStatusCondition);
            AssertEquivalence(0, 1, firstConditionName);
            AssertNotEquivalent(1, 2, firstConditionName);
            AssertNotEquivalent(2, 3, firstConditionName);
            AssertEquivalence(3, 4, firstConditionName);

            // ConditionOperand
            var (operand1, operand2) = expressionList[firstRelational].GetOperands();
            var conditionOperand1 = operand1 as ConditionOperand;
            var conditionOperand2 = operand2 as ConditionOperand;
            Assert.IsNotNull(conditionOperand1);
            Assert.IsNotNull(conditionOperand2);
            Assert.IsTrue(conditionOperand1.IsEquivalent(conditionOperand2));
            var (op1, op2) = expressionList[firstRelational + 7].GetOperands();
            var conditionOperandPtr1 = op1 as ConditionOperand;
            var conditionOperandNULL = op2 as ConditionOperand;
            Assert.IsNotNull(conditionOperandPtr1);
            Assert.IsNotNull(conditionOperandNULL);
            var (op3, op4) = expressionList[firstRelational + 8].GetOperands();
            var conditionOperandPtr2 = op3 as ConditionOperand;
            var conditionOperandNULLS = op4 as ConditionOperand;
            Assert.IsNotNull(conditionOperandPtr2);
            Assert.IsNotNull(conditionOperandNULLS);
            Assert.IsTrue(conditionOperandNULL.IsEquivalent(conditionOperandNULLS));

            // NumericVariable
            operand1 = conditionOperand1.ArithmeticExpression;
            operand2 = conditionOperand2.ArithmeticExpression;
            var numericVariableOp1 = operand1 as NumericVariableOperand;
            var numericVariableOp2 = operand2 as NumericVariableOperand;
            Assert.IsNotNull(numericVariableOp1);
            Assert.IsNotNull(numericVariableOp2);
            Assert.IsFalse(numericVariableOp1.IsEquivalent(numericVariableOp2));

            var numericVariableOp3 = expressionList[firstRelational].GetOperands().Item1.GetOperands().Item1 as NumericVariableOperand;
            Assert.IsNotNull(numericVariableOp3);
            Assert.IsTrue(numericVariableOp3.IsEquivalent(numericVariableOp1));

            // Other
            var firstOther = expressionList.FindLastIndex(e => e.NodeType == ExpressionNodeType.ConditionNameConditionOrSwitchStatusCondition) + 1;
            AssertEquivalence(0, 1, firstOther);

            // Cross expression type
            var allFirst = new []
            {
                firstLogical,
                firstRelational,
                firstArithmetic,
                firstClass,
                firstConditionName,
                firstSign
            };

            foreach (var exp1 in allFirst)
            {
                foreach (var exp2 in allFirst)
                {
                    if (exp1 != exp2)
                    {
                        AssertNotEquivalent(exp1, exp2, 0);
                    }
                }
            }


            void AssertEquivalence(int indexA, int indexB, int indexShift)
            {
                var expA = expressionList[indexShift + indexA];
                var expB = expressionList[indexShift + indexB];
                Assert.IsTrue(expA.IsEquivalent(expB), $"Expressions were not equivalent: '{expA}', '{expB}'");
            }

            void AssertNotEquivalent(int indexA, int indexB, int indexShift)
            {
                var expA = expressionList[indexShift + indexA];
                var expB = expressionList[indexShift + indexB];
                Assert.IsFalse(expA.IsEquivalent(expB), $"Expressions were equivalent: '{expA}', '{expB}'");
            }
        }

        /// <summary>
        /// Load and parse Parser/Programs/Cobol85/Expressions.rdz.cbl
        /// and return all expressions found.
        /// </summary>
        /// <returns>List of Expression instances.</returns>
        private static List<Expression> ParseExpressionsReferenceFile()
        {
            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var fileName = "Expressions";
            var compilationUnit = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.SemanticCrossCheck);
            var root = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root;

            // TODO: Correct the error on switch conditions and set the number of diagnostics to 0
            var diagnostics = compilationUnit.AllDiagnostics();
            Assert.AreEqual(1, diagnostics.Count);
            Assert.AreEqual("Line 169[15,25] <30, Error, Semantics> - Semantic error: Symbol MYSWITCH-ON is not referenced", diagnostics[0].ToString());

            // Get all expressions
            var expCollector = new ExpressionCollector();
            root.AcceptASTVisitor(expCollector);
            return expCollector.Expressions;
        }

        /// <summary>
        /// Issue #1939, check the Expression trees
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckExpressionTrees()
        {
            // Create the expression trees in a string
            var expressions = ParseExpressionsReferenceFile();
            var strToString = new StringBuilder();
            foreach (var expression in expressions)
            {
                strToString.AppendLine(ExpressionToTree(expression));
                strToString.AppendLine("________________________________________");
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
            var paddedExpression = $"{new string(' ', depth * 4)}{expression.NodeType}: {expression}";

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

        /// <summary>
        /// Build expressions, collect tokens from them using visitor pattern and check result
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckExpressionTokens()
        {
            // Collect all expression tokens and dump in a string
            var expressions = ParseExpressionsReferenceFile();
            var strToString = new StringBuilder();
            foreach (var expression in expressions)
            {
                var tokenCollector = new TokenCollector();
                expression.AcceptASTVisitor(tokenCollector);
                var tokens = tokenCollector.Tokens;

                Assert.IsTrue(tokens.Count > 0);
                int line = tokens[0].Line;
                string tokensText = $"Line {line}: {string.Join(" ", tokens.Select(t => t.Text))}";
                strToString.AppendLine(tokensText);
            }

            var expectedPath = Path.ChangeExtension(Path.Combine("Misc", "ExpressionsTokens-expected"), "txt");
            var expected = File.ReadAllText(expectedPath);
            // ensure the string and the file are the same 
            TestUtils.compareLines("CheckExpressionTokens", strToString.ToString(), expected, PlatformUtils.GetPathForProjectFile(expectedPath));
        }

        private class TokenCollector : AbstractAstVisitor
        {
            public readonly List<Token> Tokens = new List<Token>();

            public override bool Visit(Token token)
            {
                Tokens.Add(token);
                return true;
            }
        }
    }
}
