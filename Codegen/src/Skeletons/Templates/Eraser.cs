namespace TypeCobol.Codegen.Skeletons.Templates {

	using System;
	using System.Collections.Generic;
	using System.Text.RegularExpressions;

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
		return output;
	}
	public bool Run() {
		if (output != null) return false;
		bool somethingDone = false;
		output = input;
		foreach(var word in words) {
			string pattern = string.Format(@"\b{0}\b", word);
			var regex = new Regex(pattern, RegexOptions.IgnoreCase);
			var match = regex.Match(output);
			if (match.Success) {
				somethingDone = true;
				string padding = this.padded? new String(' ', word.Length) : "";
				output = regex.Replace(output, padding, match.Groups.Count, match.Index);
			}
		}
		return somethingDone;
	}
}

}
