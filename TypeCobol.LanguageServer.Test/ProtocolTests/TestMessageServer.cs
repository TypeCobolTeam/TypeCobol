using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal class TestMessageServer : IMessageServer
    {
        private record struct Expected(JToken Json, bool IsResponse);

        private Expected? _expected;

        public TestMessageServer()
        {
            _expected = null;
        }

        public void SendMessage(string message)
        {
            if (!_expected.HasValue)
            {
                return;
            }

            var expectedJson = _expected.Value.Json;
            var actualJsonMessage = JToken.Parse(message);
            var actualJson = _expected.Value.IsResponse ? actualJsonMessage["result"] : actualJsonMessage["params"];
            bool equals = JToken.DeepEquals(expectedJson, actualJson);
            if (!equals)
            {
                var error = new StringBuilder();
                error.AppendLine("Expected:");
                error.AppendLine(expectedJson.ToString(Formatting.Indented));
                error.AppendLine("but found:");
                if (actualJson == null)
                {
                    error.AppendLine("<NULL>");
                }
                else
                {
                    error.AppendLine(actualJson.ToString(Formatting.Indented));
                }
                Assert.Fail(error.ToString());
            }

            _expected = null;
        }

        public void WriteServerLog(string trace)
        {
            // Do nothing, not part of this test
        }

        public void Expect(JToken json, bool isResponse)
        {
            _expected = new Expected(json, isResponse);
        }
    }
}
