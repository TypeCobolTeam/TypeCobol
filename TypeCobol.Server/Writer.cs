using System.Xml;
using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Server {



interface ErrorWriter {
	void Write();
	void Flush();
}



public abstract class AbstractErrorWriter: ErrorWriter {

	internal Dictionary<string,List<Diagnostic>> Errors  { get; set; }
	internal Dictionary<string,string> Inputs { get; set; }
	public IList<string> Outputs { get; set; }

	public AbstractErrorWriter() {
		Errors  = new Dictionary<string,List<Diagnostic>>();
		Inputs  = new Dictionary<string,string>();
		Outputs = new List<string>();
	}

	public void AddErrors(string key, IEnumerable<Diagnostic> errors) {
		List<Diagnostic> list;
		try { list = Errors[key]; }
		catch(KeyNotFoundException ex) {
			list = new List<Diagnostic>();
			Errors[key] = list;
			Inputs[key] = GenerateNumber().ToString();
		}
		list.AddRange(errors);
	}

	private int count = 1;
	private int GenerateNumber() { return count++; }

	public abstract void Write();
	public abstract void Flush();
}



public class XMLWriter: AbstractErrorWriter {
	private XmlWriter writer;

	public XMLWriter(System.IO.TextWriter writer) {
		this.writer = System.Xml.XmlWriter.Create(writer);
	}

	public override void Write() {
		writeHead();
		writeInputs();
		foreach(var key in Errors.Keys) {
			foreach(var error in Errors[key])
				writeMessage(Inputs[key], error);
		}
		writeOutputs();
		writeTail();
	}

	public override void Flush() {
		writer.Close();
	}

	private void writeHead() {
		writer.WriteStartElement("BUILD");
		writer.WriteStartElement("PACKAGE");
	}

	private void writeTail() {
		writer.WriteStartElement("STATEMENTTABLE");
		writer.WriteEndElement();// STATEMENTTABLE
		writer.WriteEndElement();// PACKAGE
		writer.WriteEndElement();// BUILD
	}

	private void writeInputs() {
		writer.WriteStartElement("FILEREFERENCETABLE");
		writer.WriteElementString("FILECOUNT", Inputs.Count.ToString());
		foreach(var path in Inputs.Keys)
			writeFile(Inputs[path], new System.IO.FileInfo(path).FullName);
		writer.WriteEndElement();// FILEREFERENCETABLE
	}

	private void writeOutputs() {
		writer.WriteStartElement("OUTFILEREFERENCETABLE");
		writer.WriteElementString("OUTFILECOUNT", Outputs.Count.ToString());
		int c = 1;
		foreach(var path in Outputs)
			writeFile((c++).ToString(), new System.IO.FileInfo(path).FullName, "OUT");
		writer.WriteEndElement();// FILEREFERENCETABLE
	}

	private void writeFile(string id, string name, string prefix = "") {
		writer.WriteStartElement(prefix+"FILE");
		writer.WriteElementString(prefix+"FILENUMBER", id);
		writer.WriteElementString(prefix+"FILENAME", name);
		writer.WriteEndElement();// FILE
	}

	private void writeMessage(string id, Diagnostic error) {
		writer.WriteStartElement("MESSAGE");
		writer.WriteElementString("MSGNUMBER", "TC"+AsIBMSuffix(error.Info.Severity));
		writer.WriteElementString("MSGLINE", "1"); //TODO
		writer.WriteElementString("MSGFILE", id);
		writer.WriteElementString("MSGTEXT", error.Message);
		writer.WriteEndElement();// MESSAGE
	}

	private static string AsIBMSuffix(Compiler.Diagnostics.Severity severity) {
		if (severity == Compiler.Diagnostics.Severity.Error) return "-E";
		if (severity == Compiler.Diagnostics.Severity.Warning) return "-W";
		if (severity == Compiler.Diagnostics.Severity.Info) return "-I";
		throw new System.NotImplementedException("Unsupported severity: "+severity);
	}
}



public class ConsoleWriter: AbstractErrorWriter {
	private System.IO.TextWriter writer;

	public ConsoleWriter(System.IO.TextWriter writer) {
		this.writer = writer;
	}

	public override void Write() {
		foreach(var key in Errors.Keys) {
			write(key, Errors[key]);
		}
	}

	private void write(string title, IList<Diagnostic> errors) {
		writer.WriteLineAsync();
		writer.WriteAsync(errors.Count.ToString());
		writer.WriteAsync(" error");
		writer.WriteAsync((errors.Count>1)?"s":"");
		writer.WriteAsync(" in \"");
		writer.WriteAsync(title);
		writer.WriteLineAsync("\":");
		foreach(var e in errors) writer.WriteLineAsync(e.ToString());
	}

	public override void Flush() {
		writer.Close();
	}
}

}
