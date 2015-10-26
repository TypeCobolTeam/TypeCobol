using System.IO; // Stream[Reader|Writer], IOException
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using System.Collections.Generic;
using MsgPack.Serialization;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Server
{

// The following seem have no importance regarding msgpack serialization:
// - how the class to serialize is named
// - in what order fields are declared or initialized: 
//   they are always serialized by msgpack in alphabetical order,
//   excepted if one uses [MessagePackMemberAttribute(<0-based position>)]
// - wheter or not the field to serialize is a C# property

public class MsgPackCodeElement {
	[MessagePackMemberAttribute(0)]
	public CodeElementType Type;
	[MessagePackMemberAttribute(1)]
	public int Begin;
	[MessagePackMemberAttribute(2)]
	public int End { get; set; }
	[MessagePackMemberAttribute(5)]
	public string Text;
	[MessagePackMemberAttribute(3)]
	public int LineFirst;
	[MessagePackMemberAttribute(4)]
	public int LineLast;
}

class Server {

	static void Main(string[] args) {
        Parser parser = new Parser("TypeCobol.Server");
		while (true) {
			var pipe = new NamedPipeServerStream("testpipe", PipeDirection.InOut, 4);
			System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect ...");
			pipe.WaitForConnection(); // blocking

			System.Console.WriteLine("Client connected.");
			try {
				var imarshaller = MessagePackSerializer.Get<int>();
				var smarshaller = MessagePackSerializer.Get<string>();
				var tmarshaller = MessagePackSerializer.Get<List<MsgPackCodeElement>>();
				int code = imarshaller.Unpack(pipe);
				System.Console.WriteLine("Order "+code);
				if (code == 66) { // let's parse!
					string text = smarshaller.Unpack(pipe);
					parser.Parse(new TypeCobol.Compiler.Text.TextString(text));
					List<MsgPackCodeElement> list = new List<MsgPackCodeElement>();
                    foreach(CodeElement e in parser.CodeElements) {
                        // okay, we know it: this conversion is ugly
                        // reason of it: MsgPack.Cli won't let us get a Serializer on CodeElement because it is abstract
                        // so we have to explore other ways to solve this:
                        // - use a concrete class somewhere
                        // - try SimpleMessagePack
                        // - ... ?
                        System.Console.WriteLine("["+e.Type+"] "+e.ConsumedTokens.Count+" tokens, \""+e.Text+"\"; ToString=\""+e.ToString()+"\"");
                        list.Add(new MsgPackCodeElement {
                                Type = e.Type,
                                Begin = e.ConsumedTokens[0].Column-1,
                                End = e.ConsumedTokens[e.ConsumedTokens.Count-1].EndColumn,
                                Text = e.Text,
                                LineFirst = e.ConsumedTokens[0].Line-1,
                                LineLast  = e.ConsumedTokens[e.ConsumedTokens.Count-1].Line-1,
                            });
                    }
					tmarshaller.Pack(pipe, list);
					System.Console.WriteLine(list.Count+" CodeElements sent.");
				}

				pipe.Disconnect();
			} 
			catch (IOException ex) { System.Console.WriteLine("Error: {0}", ex.Message); }
			catch (System.Runtime.Serialization.SerializationException ex ) { System.Console.WriteLine("Error: {0}", ex.Message); }
			finally { System.Console.WriteLine("Close pipe."); pipe.Close(); }
		}
	}

}

}
