using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen {

	class App {
		static void Main(string[] args) {
			var files = new List<string>();
			foreach(string arg in args) files.AddRange(GetParameterFiles(arg));
			var skeletons = new List<Skeleton>();
			foreach(var file in files) {
				string extension = Path.GetExtension(file);
				var parser = Config.Config.CreateParser(extension);
				skeletons.AddRange(parser.Parse(file));
			}
			System.Console.WriteLine(skeletons.Count+" skeleton"+(skeletons.Count>1?"s":"")+':');
			foreach(var s in skeletons) System.Console.Write(s.ToString());
		}

		private static IEnumerable<string> GetParameterFiles(string path) {
			var files = new List<string>();
			if (File.Exists(path)) {
				files.Add(path);
			} else
			if (Directory.Exists(path)) {
				System.Console.WriteLine("\""+path+"\" is a directory.");
			}
			return files;
		}
	}

}
