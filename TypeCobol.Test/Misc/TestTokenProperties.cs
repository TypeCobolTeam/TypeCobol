using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    /// <summary>
    /// Supplementary tests on Token properties after they have been consumed by ANTLR.
    /// </summary>
    [TestClass]
    public class TestTokenProperties
    {
        private static void TestTokenProperty(string propertyName, Func<Token, string> extractor, [CallerMemberName] string testName = null)
        {
            // Parse dedicated source file
            var folder = Path.GetFullPath("Misc");
            var fileName = "TokenProperties";
            var compilationUnit = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.SemanticCrossCheck);

            // Collect consumed tokens and read target property
            var consumedTokens = compilationUnit.CodeElementsDocumentSnapshot.CodeElements.SelectMany(ce => ce.ConsumedTokens);
            var result = new StringBuilder();
            foreach (var consumedToken in consumedTokens)
            {
                result.AppendLine($"Class={consumedToken.GetType().Name}, TokenType={consumedToken.TokenType}");
                result.AppendLine($"-{propertyName}={extractor(consumedToken)}");
            }

            // Compare to expected
            var expectedPath = Path.Combine(folder, fileName) + $"-{propertyName}.txt";
            var expected = new TestUtils.FileInfo(expectedPath);
            TestUtils.CompareContent(testName, result.ToString(), expected);
        }

        /// <summary>
        /// Check SourceText property on various tokens
        /// </summary>
        [TestMethod]
        public void TestSourceText() => TestTokenProperty(nameof(Token.SourceText), token => token.SourceText);
    }
}
