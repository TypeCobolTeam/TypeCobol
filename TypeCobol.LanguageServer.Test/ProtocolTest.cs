using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.Test.ProtocolTests;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class ProtocolTest
    {
        private static void TestMessageDirectory(string path, Dictionary<string, LspMethod> lspMethods)
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
            var lspMethods = new Dictionary<string, LspMethod>
            {
                { testNotification.Method, new LspMethod(testNotification) },
                { testRequest.Method, new LspMethod(testRequest) }
            };
            TestMessageDirectory(@"ProtocolTests\Fakes", lspMethods);
        }

        [TestMethod]
        public void TestAllMessages() => TestMessageDirectory(@"ProtocolTests\Messages", LspMethod.DiscoverLspMethods());
    }
}
