using MsgPack.Serialization;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Server.Serialization
{
    public class IntSerializer {
        private MessagePackSerializer<int> Marshaller = MessagePackSerializer.Get<int>();
        public bool Serialize(Stream output, int data) {
            Marshaller.Pack(output, data);
            return true;
        }
        public int Deserialize(Stream input) {
            return Marshaller.Unpack(input);
        }
    }

    public class StringSerializer {
        private MessagePackSerializer<string> Marshaller = MessagePackSerializer.Get<string>();
        public bool Serialize(Stream output, string data) {
            Marshaller.Pack(output, data);
            return true;
        }
        public string Deserialize(Stream input) {
            return Marshaller.Unpack(input);
        }
    }

    public class CodeElementsListSerializer {
        private MessagePackSerializer<List<MsgPackCodeElement>> Marshaller = MessagePackSerializer.Get<List<MsgPackCodeElement>>();
        public bool Serialize(Stream output, IEnumerable<TypeCobol.Compiler.CodeElements.CodeElement> data) {
            List<MsgPackCodeElement> list = new List<MsgPackCodeElement>();
            foreach(TypeCobol.Compiler.CodeElements.CodeElement e in data) {
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
                //if (element.Errors.Count > 0) System.Console.WriteLine(element.Errors.Count+" Error(s) to send.");
                list.Add(element);
            }
            Marshaller.Pack(output, list);
            System.Console.WriteLine(list.Count+" CodeElements sent.");
            return true;
        }

        public List<MsgPackCodeElement> Deserialize(Stream input) {
            return Marshaller.Unpack(input);
        }
    }


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

}
