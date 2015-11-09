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

public class MsgPackCodeElement {
	[MessagePackMemberAttribute(0)]
	public TypeCobol.Compiler.CodeElements.CodeElementType Type;
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
	[MessagePackMemberAttribute(6)]
    public IList<MsgPackToken> Tokens;
	[MessagePackMemberAttribute(7)]
    public List<MsgPackError> Errors;
}

public class MsgPackToken {
	[MessagePackMemberAttribute(0)]
    public int Type;
	[MessagePackMemberAttribute(1)]
    public int Begin;
	[MessagePackMemberAttribute(2)]
    public int Length;
	[MessagePackMemberAttribute(3)]
    public int Line;
}

public class MsgPackError {
	[MessagePackMemberAttribute(0)]
    public int Begin;
	[MessagePackMemberAttribute(1)]
    public int End;
	[MessagePackMemberAttribute(2)]
    public string Message;
	[MessagePackMemberAttribute(3)]
    public int Severity;
	[MessagePackMemberAttribute(4)]
    public int Category;
	[MessagePackMemberAttribute(5)]
    public int Code;
}

class Server {

	static void Main(string[] args) {
        Parser parser = new Parser("TypeCobol.Server");
        string pipename = "TypeCobol.pipe";
        if (args.Length > 0) pipename = args[0];
		while (true) {
			var pipe = new NamedPipeServerStream(pipename, PipeDirection.InOut, 4);
			//System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect on \""+pipename+"\" ...");
			pipe.WaitForConnection(); // blocking

			System.Console.WriteLine("Client connected.");
			try {
				var imarshaller = MessagePackSerializer.Get<int>();
				var smarshaller = MessagePackSerializer.Get<string>();
				var tmarshaller = MessagePackSerializer.Get<List<MsgPackCodeElement>>();
				var emarshaller = MessagePackSerializer.Get<List<MsgPackError>>();
				int code = imarshaller.Unpack(pipe);
				if (code == 66) { // let's parse!
					string text = smarshaller.Unpack(pipe);
					parser.Parse(new TypeCobol.Compiler.Text.TextString(text));
					List<MsgPackCodeElement> list = new List<MsgPackCodeElement>();
                    foreach(TypeCobol.Compiler.CodeElements.CodeElement e in parser.CodeElements) {
                        // okay, we know it: this conversion is ugly
                        // reason of it: MsgPack.Cli won't let us get a Serializer on CodeElement because it is abstract
                        // so we have to explore other ways to solve this:
                        // - use a concrete class somewhere
                        // - try SimpleMessagePack
                        // - ... ?
//                        System.Console.WriteLine("["+e.Type+"] "+e.ConsumedTokens.Count+" tokens, \""+e.Text+"\"; ToString=\""+e.ToString()+"\", Errors="+e.Diagnostics.Count);
                        var element = new MsgPackCodeElement {
                                Type = e.Type,
                                Begin = e.ConsumedTokens[0].Column-1,
                                End = e.ConsumedTokens[e.ConsumedTokens.Count-1].EndColumn,
                                Text = e.Text,
                                LineFirst = e.ConsumedTokens[0].Line-1,
                                LineLast  = e.ConsumedTokens[e.ConsumedTokens.Count-1].Line-1,
                            };
                        element.Tokens = new List<MsgPackToken>();
                        foreach(TypeCobol.Compiler.Scanner.Token token in e.ConsumedTokens) {
                            element.Tokens.Add(new MsgPackToken {
                                    Type = (int)token.TokenFamily,
                                    Begin = token.Column-1,
                                    Length = token.Length,
                                    Line = token.Line-1,
                                });
                        }
                        element.Errors = new List<MsgPackError>();
                        foreach(TypeCobol.Compiler.Diagnostics.Diagnostic error in e.Diagnostics) {
                            element.Errors.Add(new MsgPackError {
                                    Begin = error.ColumnStart-1,
                                    End = error.ColumnEnd,
                                    Message = error.Message,
                                    Severity = (int)error.Info.Severity,
                                    Category = (int)error.Info.Category,
                                    Code = error.Info.Code,
                                });
                        }
					    if (element.Errors.Count > 0 ) System.Console.WriteLine(element.Errors.Count+" Error(s) to send.");
                        list.Add(element);
                    }
					tmarshaller.Pack(pipe, list);
					System.Console.WriteLine(list.Count+" CodeElements sent.");
				}

				pipe.Disconnect();
			} 
			catch (IOException ex) { System.Console.WriteLine("Error: {0}", ex.Message); }
			catch (System.Runtime.Serialization.SerializationException ex ) { System.Console.WriteLine("Error: {0}", ex.Message); }
			finally { pipe.Close(); }
		}
	}

}

}
