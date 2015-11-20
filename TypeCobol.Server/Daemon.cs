using System.IO; // Stream[Reader|Writer], IOException
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using System.Collections.Generic;
using TypeCobol.Server.Serialization;

namespace TypeCobol.Server
{

class Server {

	static void Main(string[] args) {
        Parser parser = new Parser("TypeCobol.Server");
        string pipename = "TypeCobol.pipe";
        if (args.Length > 0) pipename = args[0];

        var pipe = new NamedPipeServerStream(pipename, PipeDirection.InOut, 4);
        Commands.Register(66, new Parse(parser, pipe, pipe));
        Commands.Register(67, new Initialize(parser, pipe, pipe));
        IntegerSerializer decoder = new IntegerSerializer();
		while (true) {
			//System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect on \""+pipename+"\" ...");
			pipe.WaitForConnection(); // blocking
			//System.Console.WriteLine("Client connected.");
			try {
				int code = decoder.Deserialize(pipe);
                var command = Commands.Get(code);
                command.execute();
			}
			catch (IOException ex) { System.Console.WriteLine("Error: {0}", ex.Message); }
			catch (System.Runtime.Serialization.SerializationException ex ) { System.Console.WriteLine("Error: {0}", ex.Message); }
			finally { pipe.Disconnect(); }
		}
        pipe.Close();
	}

}

}
