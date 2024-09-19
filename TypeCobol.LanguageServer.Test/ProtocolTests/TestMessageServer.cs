using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal record struct Expected(JToken Json, bool IsResponse);

    internal class TestMessageServer : IMessageServer
    {
        public Expected? Expected { get; set; }

        public TestMessageServer()
        {
            Expected = null;
        }

        public void SendMessage(string message)
        {
            if (!Expected.HasValue)
            {
                return;
            }

            var expectedJson = Expected.Value.Json;
            var actualJsonMessage = JToken.Parse(message);
            var actualJson = Expected.Value.IsResponse ? actualJsonMessage["result"] : actualJsonMessage["params"];
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
        }

        public void WriteServerLog(string trace)
        {
            Console.WriteLine(trace);
        }
    }
}
