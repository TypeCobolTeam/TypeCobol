using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen {

	class Program {
		static void Main(string[] args) {
			var files = new List<string>();
			foreach(string arg in args) files.AddRange(GetParameterFiles(arg));
			var skeletons = new List<Skeleton>();
			foreach(var file in files) {
				string extension = Path.GetExtension(file);
				var parser = GetParser(extension);
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

		private static Config.ConfigParser GetParser(string extension) {
			var map = new Dictionary<string,Config.ConfigParser>();
			map.Add(".xml", new Config.XmlParser());
			var defaultext = ".xml";
			try { return map[extension.ToLower()]; }
			catch(System.Exception) { return map[defaultext]; }
		}
	}

}
