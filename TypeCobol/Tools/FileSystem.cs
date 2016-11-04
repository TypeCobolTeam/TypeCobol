namespace TypeCobol.Tools {

	using System.Collections.Generic;
	using System.IO;

/// <summary>
/// For this utility method to be more easily found, I would have created an extension method
/// for System.IO.Path class instead, but C# doesn't allow static method extension -passing a class
/// instead of an instance to the extension method was too difficult, I guess ?
/// The alternative was to create a pointless wrapper class ...
/// How could a language sell with such half-baked syntactic sugar ?
/// </summary>
public static class FileSystem {
	public static List<string> GetFiles(string path, string[] patterns = null, bool recursive = true) {
		var results = new List<string>();
		if (File.Exists(path)) {
			results.Add(path);
		} else
		if (Directory.Exists(path)) {
			var option = recursive? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
			if (patterns == null) {
				results.AddRange(Directory.EnumerateFiles(path, "*", option));
			} else
			foreach(var pattern in patterns) {
				results.AddRange(Directory.EnumerateFiles(path, pattern, option));
			}
		}
		return results;
	}
}


}
