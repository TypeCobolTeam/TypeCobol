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
            foreach (var type in assembly.GetTypes())
            {
                foreach (var field in type.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    if (typeof(LspMethodDefinition).IsAssignableFrom(field.FieldType))
                    {
                        var lspMethodDefinition = (LspMethodDefinition)field.GetValue(null);
                        Debug.Assert(lspMethodDefinition != null);

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
            var testSet = TestSet.Build(path);
            testSet.Validate(lspMethods);

            var testServer = new JsonRpcTestServer(new TestMessageServer(), lspMethods.Values);
            foreach (var testMessage in testSet)
            {
                testServer.Test(testMessage);
            }
        }

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
            TestMessageDirectory(@"ProtocolTests\Fakes", lspMethods);
        }

        [TestMethod]
        public void TestAllMessages() => TestMessageDirectory(@"ProtocolTests\Messages", DiscoverLspMethods());
    }
}
