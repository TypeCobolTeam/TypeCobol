using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Reflection;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.Test.ProtocolTests;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class ProtocolTest
    {
        private static Dictionary<string, LspMethodDefinition> DiscoverLspMethods()
        {
            var result = new Dictionary<string, LspMethodDefinition>();
            var assembly = typeof(TypeCobolLanguageServer).Assembly;
            foreach (var type in assembly.GetTypes()) // All types
            {
                foreach (var field in type.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)) // Having static fields
                {
                    if (typeof(LspMethodDefinition).IsAssignableFrom(field.FieldType)) // Whose type is an LSP method definition
                    {
                        // Get method definition
                        var lspMethodDefinition = (LspMethodDefinition)field.GetValue(null);
                        Debug.Assert(lspMethodDefinition != null);

                        // Discard empty messages
                        if (lspMethodDefinition.Types.All(t => t == null || t == typeof(object)))
                        {
                            Console.WriteLine($"No data to test in method '{lspMethodDefinition.Method}', skipping...");
                            continue;
                        }

                        result.Add(lspMethodDefinition.Method, lspMethodDefinition);
                    }
                }
            }

            return result;
        }

        private static void TestMessageDirectory(string path, Dictionary<string, LspMethodDefinition> lspMethods)
        {
            // Collect test files from dir and check consistency
            var testSet = TestSet.Build(path);
            testSet.Validate(lspMethods);

            // Test each message using the test server and collect errors
            var failedTests = new List<(string FileName, Exception Exception)>();
            var testServer = new JsonRpcTestServer(lspMethods.Values);
            foreach (var testMessage in testSet)
            {
                try
                {
                    testServer.Test(testMessage);
                }
                catch (Exception exception)
                {
                    failedTests.Add((testMessage.FileName, exception));
                }
            }

            // Fail if errors encountered
            if (failedTests.Count > 0)
            {
                Console.WriteLine();
                foreach (var failedTest in failedTests)
                {
                    Console.WriteLine($"[{failedTest.FileName}] failed !");
                    Console.WriteLine(failedTest.Exception.Message);
                    Console.WriteLine();
                }

                Assert.Fail($"{failedTests.Count} test(s) failed !");
            }
        }

        /// <summary>
        /// Basic test to validate the test framework itself. Each type of message is tested:
        /// - notification, sent or received
        /// - request, sent or received
        /// - response, sent or received
        /// </summary>
        [TestMethod]
        public void TestFakeMessages()
        {
            var testNotification = new NotificationType("test/notification", typeof(FakeParam));
            var testRequest = new RequestType("test/request", typeof(FakeParam), typeof(FakeResult), null);
            var lspMethods = new Dictionary<string, LspMethodDefinition>
            {
                { testNotification.Method, testNotification },
                { testRequest.Method, testRequest }
            };

            TestMessageDirectory(Path.Combine("ProtocolTests", "Fakes"), lspMethods);
        }

        /// <summary>
        /// Actual test for LSP messages.
        /// </summary>
        [TestMethod]
        public void TestAllMessages() => TestMessageDirectory(Path.Combine("ProtocolTests", "Messages"), DiscoverLspMethods());
    }
}
