using System.Collections.Generic;
using System.IO;
using TypeCobol.Server.Serialization;

namespace TypeCobol.Server
{
    internal class Commands
    {
        private static Dictionary<int,Command> Codes = new Dictionary<int,Command>();
        public static void Register(int code, Command command) { Codes.Add(code, command); }
        public static Command Get(int code) { return Codes[code]; }
    }

    internal interface Command {
        void execute();
    }

    internal class Parse : Command {

        private Parser Parser;
        private Stream Input;
        private Stream Output;
        private StringSerializer Deserializer;
        private CodeElementsListSerializer Serializer;

        public Parse(Parser parser, Stream istream, Stream ostream) {
            this.Parser = parser;
            this.Input  = istream;
            this.Output = ostream;
            this.Deserializer = new StringSerializer();
            this.Serializer = new CodeElementsListSerializer();
        }
        public void execute() {
            string text = this.Deserializer.Deserialize(this.Input);
            Parser.Parse(new TypeCobol.Compiler.Text.TextString(text));
            this.Serializer.Serialize(this.Output, Parser.CodeElements);
        }
    }
}
