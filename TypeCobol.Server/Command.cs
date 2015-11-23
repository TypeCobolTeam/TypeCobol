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

    internal abstract class AbstractCommand : Command {
        internal Parser Parser;
        internal Stream Input;
        internal Stream Output;
        private IntegerSerializer ReturnCodeSerializer = new IntegerSerializer();

        public AbstractCommand(Parser parser, Stream istream, Stream ostream) {
            this.Parser = parser;
            this.Input  = istream;
            this.Output = ostream;
        }

        public abstract void execute();

        internal void SerializeReturnCode(int code) {
            this.ReturnCodeSerializer.Serialize(Output, code);
        }
    }

    internal class Initialize : AbstractCommand {
        private StringSerializer Deserializer = new StringSerializer();

        public Initialize(Parser parser, Stream istream, Stream ostream)
          : base(parser, istream, ostream) { }

        public override void execute() {
            string path = Deserializer.Deserialize(Input);
            Parser.Init(path);
            SerializeReturnCode(0);
        }
    }

    internal class Parse : AbstractCommand {
        private StringSerializer Deserializer = new StringSerializer();
        private CodeElementsListSerializer Serializer = new CodeElementsListSerializer();

        public Parse(Parser parser, Stream istream, Stream ostream)
          : base(parser, istream, ostream) { }

        public override void execute() {
            string path = Deserializer.Deserialize(Input);
            string text = this.Deserializer.Deserialize(Input);
            Parser.Parse(path, new TypeCobol.Compiler.Text.TextString(text));
            SerializeReturnCode(0);
            Serializer.Serialize(Output, Parser.CodeElements);
        }
    }
}
