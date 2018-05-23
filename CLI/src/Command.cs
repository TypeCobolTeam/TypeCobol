using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Pipes;
using TypeCobol.Server.Serialization;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Server
{
    internal class Commands
    {
        private static Dictionary<int,Command> Codes = new Dictionary<int,Command>();
        public static void Register(int code, Command command) { Codes.Add(code, command); }
        public static Command Get(int code) { return Codes[code]; }
    }

    internal interface Command {
        void execute(NamedPipeServerStream pipe);
    }

    internal abstract class AbstractCommand : Command {
        internal readonly Parser Parser;
        internal readonly Stream Input;
        internal readonly Stream Output;
        private readonly IntegerSerializer _returnCodeSerializer = new IntegerSerializer();

        protected AbstractCommand(Parser parser, Stream istream, Stream ostream) {
            this.Parser = parser;
            this.Input  = istream;
            this.Output = ostream;
        }

        public abstract void execute(NamedPipeServerStream pipe);

        internal void SerializeReturnCode(int code) {
            this._returnCodeSerializer.Serialize(Output, code);
        }
    }

    internal class Initialize : AbstractCommand {
        private StringSerializer Deserializer = new StringSerializer();

        public Initialize(Parser parser, Stream istream, Stream ostream)
            : base(parser, istream, ostream) { }

        public override void execute(NamedPipeServerStream pipe) {
            string path = Deserializer.Deserialize(Input);
            Parser.Init(path, new Compiler.Directives.TypeCobolOptions { ExecToStep = ExecutionStep.Generate });
            SerializeReturnCode(0);
        }
    }

    internal class Parse : AbstractCommand {
        private readonly StringSerializer _deserializer = new StringSerializer();
        private readonly TextChangedEventSerializer _eventDeserializer = new TextChangedEventSerializer();
        private readonly CodeElementsListSerializer _serializer = new CodeElementsListSerializer();

        public Parse(Parser parser, Stream istream, Stream ostream)
            : base(parser, istream, ostream) { }

        public override void execute(NamedPipeServerStream pipe) {
            var sw = Stopwatch.StartNew();
            var time0 = sw.ElapsedMilliseconds;
            string path = _deserializer.Deserialize(Input);
            var e = _eventDeserializer.Deserialize(Input);
            var time1 = sw.ElapsedMilliseconds;
            Parser.Parse(path, e);
            var time2 = sw.ElapsedMilliseconds;
            SerializeReturnCode(0);
            _serializer.Lines = Parser.Results.CodeElementsDocumentSnapshot.Lines;
            _serializer.Serialize(Output, Parser.Results.CodeElementsDocumentSnapshot.CodeElements);
            var time3 = sw.ElapsedMilliseconds;
            sw.Stop();

            var str = new System.Text.StringBuilder();
            str .Append("Total time: ")
                .Append(time3-time0).Append("ms  ")
                .Append("Parse : ").Append(time2-time1).Append("ms  ")
                .Append("Serialize: ").Append(time3-time2 + time1-time0).Append("ms  ")
                .Append("unpack:").Append(time1-time0).Append("ms, ")
                .Append("pack:").Append(time3-time2).Append("ms)");
            System.Console.WriteLine(str);
        }
    }

    /// <summary>
    /// RunCommandLine class implementing AbstractCommand members.  
    /// This is called when client passes value "68".
    /// </summary>
    internal class RunCommandLine : AbstractCommand
    {
        public RunCommandLine(Parser parser, Stream istream, Stream ostream)
            : base(parser, istream, ostream) { }

        /// <summary>
        /// Execute method.
        /// </summary>
        /// <param name="pipe">NamedPipeServerStream</param>
        public override void execute(NamedPipeServerStream pipe)
        {
            ReturnCode returnCode = ReturnCode.Success;
            ConfigSerializer configSerializer = new ConfigSerializer();
            byte[] messagebuffer = new byte[100];
            byte[] originalbuffer = new byte[100];
            int i = 0;
          
            do
            {
                pipe.Read(messagebuffer, 0, messagebuffer.Length);
                System.Buffer.BlockCopy(messagebuffer, 0, originalbuffer, i, messagebuffer.Length);
                Array.Resize(ref originalbuffer, originalbuffer.Length + 100);
                messagebuffer = new byte[100];
                i = i + 100;
            }
            while (!pipe.IsMessageComplete);

            try
            {
                TypeCobolConfiguration config = new TypeCobolConfiguration();
                config = configSerializer.Deserialize(originalbuffer);
                config.Format = TypeCobolOptionSet.CreateFormat(config.EncFormat, ref config);
                returnCode = CLI.runOnce(config); //Try to run TypeCobol ad get status in returnCode
            }
            catch
            {
                returnCode = ReturnCode.FatalError; 
            }
            finally
            {
                //Write a "reponse" to the client which is waiting
                pipe.WriteByte((byte)returnCode);
            }
        }
    }
}
