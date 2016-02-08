using System.Collections.Generic;
using System.Diagnostics;
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
        private TextChangedEventSerializer EventDeserializer = new TextChangedEventSerializer();
        private CodeElementsListSerializer Serializer = new CodeElementsListSerializer();

        public Parse(Parser parser, Stream istream, Stream ostream)
          : base(parser, istream, ostream) { }

        public override void execute() {
            var sw = Stopwatch.StartNew();
            var time0 = sw.ElapsedMilliseconds;
            string path = Deserializer.Deserialize(Input);
            var e = EventDeserializer.Deserialize(Input);
            var time1 = sw.ElapsedMilliseconds;
            Parser.Parse(path, e);
            var time2 = sw.ElapsedMilliseconds;
            SerializeReturnCode(0);
            Serializer.Lines = Parser.Snapshot.Lines;
            Serializer.Serialize(Output, Parser.CodeElements);
            var time3 = sw.ElapsedMilliseconds;
            sw.Stop();

            var str = new System.Text.StringBuilder();
            str.Append("Total time: ").Append(time3-time0).Append("ms  Parse : ").Append(time2-time1).
                Append("ms  Serialize: ").Append(time3-time2 + time1-time0).Append("ms  (unpack:").Append(time1-time0).Append("ms, pack:").Append(time3-time2).Append("ms)");
            System.Console.WriteLine(str);
        }
    }
}
