using SimpleMsgPack;
using System.IO;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Server.Serialization
{
    public abstract class Serializer<T> {
        internal MsgPack msgpack = new MsgPack();

        public bool Serialize(Stream output, T data) {
            Encode(data);
            msgpack.Encode2Stream(output);
            return true;
        }

        public byte[] Serialize(T data) {
            Encode(data);
            return msgpack.Encode2Bytes();
        }
        internal abstract void Encode(T data);

        public T Deserialize(Stream input) {
            msgpack.DecodeFromStream(input);
            return Decode();
        }
        public T Deserialize(byte[] input) {
            msgpack.DecodeFromBytes(input);
            return Decode();
        }
        internal abstract T Decode();
    }

    public class IntegerSerializer: Serializer<int> {
        internal override void Encode(int data)  { msgpack.AsInteger = data; }
        internal override int Decode() { return (int)msgpack.AsInteger; }
    }

    public class StringSerializer: Serializer<string> {
        internal override void Encode(string data)  { msgpack.AsString = data; }
        internal override string Decode() { return msgpack.AsString; }
    }

    public class CodeElementSerializer: Serializer<CodeElement> {
    public static int[] STUB;
        public ISearchableReadOnlyList<ICodeElementsLine> Lines;

        internal override void Encode(CodeElement data) {
            Encode(msgpack, data, Lines);
        }

        internal static void Encode(MsgPack msgpack, CodeElement data, ISearchableReadOnlyList<ICodeElementsLine> lines) {
            var first = data.ConsumedTokens[0];
            var last  = data.ConsumedTokens[data.ConsumedTokens.Count-1];
            msgpack.ForcePathObject("Type").AsInteger = (int)data.Type;
            msgpack.ForcePathObject("Begin").AsInteger = first.Column-1;
            msgpack.ForcePathObject("End").AsInteger = last.EndColumn;
            msgpack.ForcePathObject("LineFirst").AsInteger = GetLine(first, lines);
            msgpack.ForcePathObject("LineLast").AsInteger = GetLine(last, lines);
            foreach(Token token in data.ConsumedTokens) {
                var child = msgpack.ForcePathObject("Tokens").AddArrayChild();
                child.ForcePathObject("Type").AsInteger = (int)token.TokenFamily;
                child.ForcePathObject("Begin").AsInteger = token.Column-1;
                child.ForcePathObject("Length").AsInteger = token.Length;
                child.ForcePathObject("Line").AsInteger = GetLine(token, lines);
                child.ForcePathObject("Text").AsString = token.Text;
            }
            foreach(TypeCobol.Compiler.Diagnostics.Diagnostic error in data.Diagnostics) {
                var child = msgpack.ForcePathObject("Errors").AddArrayChild();
                child.ForcePathObject("Begin").AsInteger = error.ColumnStart-1;
                child.ForcePathObject("End").AsInteger = error.ColumnEnd;
                child.ForcePathObject("Message").AsString = error.Message;
                child.ForcePathObject("Severity").AsInteger = (int)error.Info.Severity;
                child.ForcePathObject("Category").AsInteger = (int)error.Info.Category;
                child.ForcePathObject("Code").AsInteger = error.Info.Code;
            }
        }
        private static int GetLine(Token token, ISearchableReadOnlyList<ICodeElementsLine> lines) {
            if (lines == null)
                //return token.Line;
                throw new System.ArgumentNullException("this.Line must be set from the source document snapshot");
            return lines.IndexOf(token.TokensLine, token.TokensLine.InitialLineIndex);
        }

        internal override CodeElement Decode() {
            return Decode(msgpack);
        }

        internal static CodeElement Decode(MsgPack msgpack) {
            int type = (int)msgpack.ForcePathObject("Type").AsInteger;
            int begin = (int)msgpack.ForcePathObject("Begin").AsInteger;
            int end   = (int)msgpack.ForcePathObject("End").AsInteger;
            int lfirst = (int)msgpack.ForcePathObject("LineFirst").AsInteger;
            int llast  = (int)msgpack.ForcePathObject("LineLast").AsInteger;
            foreach (MsgPack item in msgpack.ForcePathObject("Tokens")) {
                int ttype = (int)item.ForcePathObject("Type").AsInteger;
                int tbegin = (int)item.ForcePathObject("Begin").AsInteger;
                int tlen = (int)item.ForcePathObject("Length").AsInteger;
                int tline = (int)item.ForcePathObject("Line").AsInteger;
                string ttext = item.ForcePathObject("Text").AsString;
            }
            foreach (MsgPack item in msgpack.ForcePathObject("Errors")) {
                int ebegin = (int)item.ForcePathObject("Begin").AsInteger;
                int eend = (int)item.ForcePathObject("End").AsInteger;
                string emessage = item.ForcePathObject("Message").AsString;
                int eseverity = (int)item.ForcePathObject("Severity").AsInteger;
                int ecategory = (int)item.ForcePathObject("Category").AsInteger;
                int ecode = (int)item.ForcePathObject("Code").AsInteger;
            }
            return null; //TODO CodeElementFactory.Create(CodeElementType),
                        // then fill in the blanks of the CodeElement instance
                       // (C#-side CodeElement decoding wasn't necessary at the time of implementation)
        }
    }

    public class CodeElementsListSerializer: Serializer<IEnumerable<CodeElement>> {
        public ISearchableReadOnlyList<ICodeElementsLine> Lines;

        internal override void Encode(IEnumerable<CodeElement> data) {
            foreach(var e in data) {
                var item = msgpack.ForcePathObject("CodeElements").AddArrayChild();
                CodeElementSerializer.Encode(item, e, Lines);
            }
        }
        internal override IEnumerable<CodeElement> Decode() {
            var result = new List<CodeElement>();
            foreach (MsgPack item in msgpack.ForcePathObject("CodeElements")) {
                var e = CodeElementSerializer.Decode(item);
                result.Add(e);
            }
            return result;
        }
    }

    public class TextChangedEventSerializer: Serializer<TextChangedEvent> {
        internal override void Encode(TextChangedEvent data) {
            foreach(var change in data.TextChanges) {
                var item = msgpack.ForcePathObject("Events").AddArrayChild();
                item.ForcePathObject("Type").AsInteger = (int)change.Type;
                item.ForcePathObject("Line").AsInteger = change.LineIndex;
                item.ForcePathObject("Text").AsString = change.NewLine.Text;
            }
        }
        internal override TextChangedEvent Decode() {
            var result = new TextChangedEvent();
            foreach (MsgPack item in msgpack.ForcePathObject("Events")) {
                TextChangeType type = (TextChangeType)item.ForcePathObject("Type").AsInteger;
                int line = (int)item.ForcePathObject("Line").AsInteger;
                string text = item.ForcePathObject("Text").AsString;
                ITextLine snapshot = new TextLineSnapshot(line, text, null);
                result.TextChanges.Add(new TextChange(type, line, snapshot));
            }
            return result;
        }
    }
}