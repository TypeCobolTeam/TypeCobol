using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal record struct Expected(JToken Json, bool IsResponse);

    /// <summary>
    /// Custom implementation of IMessageServer, this class is responsible for sending
    /// messages to the client. So in our test framework, the goal is to check that the
    /// JSON about to be sent is identical to the expected one.
    /// </summary>
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
                /*
                 * Not expecting a message: do nothing.
                 * This happens when:
                 * - the server attempts to reply to a request it received during a test. The response sent is not part of the
                 * test of the received request
                 * - the test server sends a fake request to the client, when preparing to actually test the received response.
                 * The fake request is not part of the test.
                 */
                return;
            }

            var expectedJson = Expected.Value.Json;
            var actualJsonMessage = JToken.Parse(message);

            // Unwrap either params or result depending on message nature
            var actualJson = Expected.Value.IsResponse ? actualJsonMessage["result"] : actualJsonMessage["params"];

            // Assert equal JSONs, using DeepEquals comparison method
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

                throw new Exception(error.ToString());
            }
        }

        public void WriteServerLog(string trace)
        {
            Console.WriteLine(trace);
        }
    }
}
