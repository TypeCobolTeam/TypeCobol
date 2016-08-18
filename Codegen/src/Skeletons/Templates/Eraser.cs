namespace TypeCobol.Codegen.Skeletons.Templates {

	using System;
	using System.Collections.Generic;

public class Eraser: Solver {
	private string input;
	private IEnumerable<string> words;
	private bool padded;
	private string output = null;

	public Eraser(string text, IEnumerable<string> words, bool padded = true) {
		this.input = text;
		this.words = words;
		this.padded = padded;
	}
	public string Replace(string template, Dictionary<string,object> variables, string delimiter) {
		return Replace();
	}
	public string Replace() {
		Run();
		return this.output;
	}
	public bool Run() {
		if (output != null) return false;
		bool somethingDone = false;
		this.output = this.input;
		foreach(var word in words) {
			if (!somethingDone) somethingDone = this.output.Contains(word);
			string padding = this.padded? new String(' ', word.Length) : "";
			this.output = this.output.Replace(word, padding);
		}
		return somethingDone;
	}
}

}
