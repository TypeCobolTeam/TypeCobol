using SimpleMsgPack;
using System.IO;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Scanner;
using System;
using TypeCobol.Codegen.Config;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Server.Serialization
{
    public abstract class Serializer<T> {
        internal MsgPack msgpack;

        public bool Serialize(Stream output, T data) {
            msgpack = new MsgPack();
            Encode(data);
            msgpack.Encode2Stream(output);
            return true;
        }

        public byte[] Serialize(T data) {
            msgpack = new MsgPack();
            Encode(data);
            return msgpack.Encode2Bytes();
        }
        internal abstract void Encode(T data);

        public T Deserialize(Stream input) {
            msgpack = new MsgPack();
            msgpack.DecodeFromStream(input);
            return Decode();
        }
        public T Deserialize(byte[] input) {
            msgpack = new MsgPack();
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

    /// <summary>
    /// ConfigSerializer class to encode/decode config object between client & server communication. 
    /// </summary>
    public class ConfigSerializer : Serializer<TypeCobolConfiguration>
    {
        internal override void Encode(TypeCobolConfiguration data) { Encode(msgpack, data); }
        internal override TypeCobolConfiguration Decode() { return Decode(msgpack); }

        internal static void Encode(MsgPack msgpack, TypeCobolConfiguration data)
        {
            msgpack.ForcePathObject("ErrorFile").AsString = string.IsNullOrEmpty(data.ErrorFile)?string.Empty:data.ErrorFile;
            msgpack.ForcePathObject("skeletonPath").AsString = string.IsNullOrEmpty(data.skeletonPath) ? string.Empty : data.skeletonPath;
            msgpack.ForcePathObject("ExecToStep").AsString = data.ExecToStep.ToString();
            msgpack.ForcePathObject("EncFormat").AsString = string.IsNullOrEmpty(data.EncFormat) ? string.Empty : data.EncFormat;
            msgpack.ForcePathObject("HaltOnMissingCopyFilePath").AsString = string.IsNullOrEmpty(data.HaltOnMissingCopyFilePath) ? string.Empty : data.HaltOnMissingCopyFilePath;
            msgpack.ForcePathObject("AutoRmarks").AsString = Convert.ToString(data.AutoRemarks);
            msgpack.ForcePathObject("Telemetry").AsString = Convert.ToString(data.Telemetry);
            msgpack.ForcePathObject("OutputFormat").AsString = Convert.ToString(data.OutputFormat);




            MsgPack item;
            item = msgpack.ForcePathObject("InputFiles");

            if (data.InputFiles != null && data.InputFiles.Count > 0)
            {
                foreach (string inputFile in data.InputFiles)
                {
                    var child = item.AddArrayChild();
                    child.ForcePathObject("InputFile").AsString = inputFile;
                }
            }

            item = msgpack.ForcePathObject("OutputFiles");

            if (data.OutputFiles !=null && data.OutputFiles.Count>0)
            {
                foreach (string outputFile in data.OutputFiles)
                {
                    var child = item.AddArrayChild();
                    child.ForcePathObject("OutputFile").AsString = outputFile;
                }
            }

            item = msgpack.ForcePathObject("CopyFolders");

            if (data.CopyFolders != null && data.CopyFolders.Count > 0)
            {
                foreach (string copyFolder in data.CopyFolders)
                {
                    var child = item.AddArrayChild();
                    child.ForcePathObject("CopyFolder").AsString = copyFolder;
                }
            }

            item = msgpack.ForcePathObject("Copies");

            if (data.Copies != null && data.Copies.Count > 0)
            {
                foreach (string copy in data.Copies)
                {
                    var child = item.AddArrayChild();
                    child.ForcePathObject("Copy").AsString = copy;
                }
            }


            item = msgpack.ForcePathObject("Dependencies");
            if (data.Dependencies != null)
            {
                foreach (string dependecy in data.Dependencies)
                {
                    var child = item.AddArrayChild();
                    child.ForcePathObject("Dependency").AsString = dependecy;
                }
            }
        }
        internal static TypeCobolConfiguration Decode(MsgPack msgpack)
        {
            TypeCobolConfiguration config = new TypeCobolConfiguration();
            config.ErrorFile = msgpack.ForcePathObject("ErrorFile").AsString;
            config.skeletonPath = msgpack.ForcePathObject("skeletonPath").AsString;
            Enum.TryParse(msgpack.ForcePathObject("ExecToStep").AsString, true, out config.ExecToStep); //Get string processing step into enum value
            config.EncFormat = msgpack.ForcePathObject("EncFormat").AsString;
            config.HaltOnMissingCopyFilePath = msgpack.ForcePathObject("HaltOnMissingCopyFilePath").AsString;
            config.AutoRemarks = msgpack.ForcePathObject("AutoRmarks").AsString.Equals("True", StringComparison.InvariantCultureIgnoreCase);
            config.Telemetry = msgpack.ForcePathObject("Telemetry").AsString.Equals("True", StringComparison.InvariantCultureIgnoreCase);
            Enum.TryParse(msgpack.ForcePathObject("OutputFormat").AsString, true, out config.OutputFormat); //Get string processing step into enum value

            List<string> InputFiles = new List<string>();
            foreach (MsgPack item in msgpack.ForcePathObject("InputFiles"))
            {
                InputFiles.Add(item.ForcePathObject("InputFile").AsString);
            }
            config.InputFiles = InputFiles;

            List<string> OutputFiles = new List<string>();
            foreach (MsgPack item in msgpack.ForcePathObject("OutputFiles"))
            {
                OutputFiles.Add(item.ForcePathObject("OutputFile").AsString);
            }
            config.OutputFiles = OutputFiles;

            List<string> CopyFolders = new List<string>();
            foreach (MsgPack item in msgpack.ForcePathObject("CopyFolders"))
            {
                CopyFolders.Add(item.ForcePathObject("CopyFolder").AsString);
            }
            config.CopyFolders = CopyFolders;

            List<string> Copies = new List<string>();
            foreach (MsgPack item in msgpack.ForcePathObject("Copies"))
            {
                Copies.Add(item.ForcePathObject("Copy").AsString);
            }
            config.Copies = Copies;
            var dependencies = new List<string>();
            foreach (MsgPack item in msgpack.ForcePathObject("Dependencies"))
            {
                dependencies.Add(item.ForcePathObject("Dependency").AsString);
            }
            config.Dependencies = dependencies;

            return config;
        }

    }

    public class CodeElementSerializer : Serializer<CodeElement> {
        public ISearchableReadOnlyList<ICodeElementsLine> Lines;

        internal override void Encode(CodeElement data) {
            Encode(msgpack, data, Lines);
        }

        internal static void Encode(MsgPack msgpack, CodeElement data, ISearchableReadOnlyList<ICodeElementsLine> lines) {
            var first = data.ConsumedTokens[0];
            var last  = data.ConsumedTokens[data.ConsumedTokens.Count-1];

            msgpack.ForcePathObject("Type").AsString = data.Type.ToString();
            msgpack.ForcePathObject("Begin").AsInteger = first.Column-1;
            msgpack.ForcePathObject("End").AsInteger = last.EndColumn;
            msgpack.ForcePathObject("LineFirst").AsInteger = GetLine(first, lines);
            msgpack.ForcePathObject("LineLast").AsInteger = GetLine(last, lines);
            MsgPack item;
            item = msgpack.ForcePathObject("Tokens");
            foreach(Token token in data.ConsumedTokens) {
                var child = item.AddArrayChild();
                child.ForcePathObject("Type").AsInteger = (int)token.TokenFamily;
                child.ForcePathObject("Begin").AsInteger = token.Column-1;
                child.ForcePathObject("Length").AsInteger = token.Length;
                child.ForcePathObject("Line").AsInteger = GetLine(token, lines);
                child.ForcePathObject("Text").AsString = token.Text;
            }
            item = msgpack.ForcePathObject("Errors");
            foreach(TypeCobol.Compiler.Diagnostics.Diagnostic error in data.Diagnostics) {
                var child = item.AddArrayChild();
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
            var item = msgpack.ForcePathObject("CodeElements");
            foreach(var e in data) {
                CodeElementSerializer.Encode(item.AddArrayChild(), e, Lines);
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
                item.AsArray.Add((int)change.Type);//Type
                item.AsArray.Add(change.LineIndex);//Line
                item.AsArray.Add(change.NewLine.Text);//Text
            }
        }
        internal override TextChangedEvent Decode() {
            var result = new TextChangedEvent();
            foreach (MsgPack item in msgpack.ForcePathObject("Events")) {
                TextChangeType type = (TextChangeType)item.AsArray[0].AsInteger;//Type
                int line = (int)item.AsArray[1].AsInteger;//Line
                string text = item.AsArray[2].AsString;//Text
                ITextLine snapshot = new TextLineSnapshot(line, text, null);
                result.TextChanges.Add(new TextChange(type, line, snapshot));
            }
            //System.Console.WriteLine("TextChangedEventSerializer.Decode(): decoded "+result.TextChanges.Count+" events.");
            //foreach(var change in result.TextChanges) System.Console.WriteLine(" - "+change.ToString());
            return result;
        }
    }
}