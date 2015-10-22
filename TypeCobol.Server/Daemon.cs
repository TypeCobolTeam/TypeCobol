using System.IO; // Stream[Reader|Writer], IOException
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using System.Collections.Generic;
using MsgPack.Serialization;

namespace TypeCobol.Server
{

// The following seem have no importance regarding msgpack serialization:
// - how the class to serialize is named
// - in what order fields are declared or initialized: 
//   they are always serialized by msgpack in alphabetical order,
//   excepted if one uses [MessagePackMemberAttribute(<0-based position>)]
// - wheter or not the field to serialize is a C# property
public class DummyCodeElement {
	[MessagePackMemberAttribute(0)]
	public DummyType Type = DummyType.Unknown;
	[MessagePackMemberAttribute(1)]
	public int Begin;
	[MessagePackMemberAttribute(2)]
	public int End { get; set; }
	[MessagePackMemberAttribute(3)]
	public string Text;
	//[NonSerializedAttribute]
	public string Tmp;
}

public enum DummyType {
	First,
	Second,
	Last,
	Unknown,
}

class Server {

	static void Main(string[] args) {
		while (true) {
			var pipe = new NamedPipeServerStream("testpipe", PipeDirection.InOut, 4);
			System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect ...");
			pipe.WaitForConnection(); // blocking

			System.Console.WriteLine("Client connected.");
			try {
				var imarshaller = MessagePackSerializer.Get<int>();
				var smarshaller = MessagePackSerializer.Get<string>();
				var tmarshaller = MessagePackSerializer.Get<List<DummyCodeElement>>();
				int code = imarshaller.Unpack(pipe);
				System.Console.WriteLine("Order "+code);
				if (code == 66) { // let's parse!
					string text = smarshaller.Unpack(pipe);
					System.Console.WriteLine("Parse text: "+text);

					var e1 = new DummyCodeElement {
							Begin = 0,
							End = 10,
							Text = text.Substring(0,10),
							Tmp = "won't be",
						};
					var e2 = new DummyCodeElement {
							Tmp = "packed at all",
							End = 20,
							Text = text.Substring(12,20),
							Begin = 12,
							Type = DummyType.Second,
						};
					var list = new List<DummyCodeElement>() {e1, e2};
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
