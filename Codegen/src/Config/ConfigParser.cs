using System;
using System.IO;
using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen.Config {
	/// <summary><see cref="Skeleton"/> parser interface.</summary>
	public interface ConfigParser {
		List<Skeleton> Parse(string path);
	}

	public class Config {
        /// <summary>
        /// The Map the association a configuration file extension to its Class parser.
        /// </summary>
		private static readonly Dictionary<string,Type> _parsers = new Dictionary<string,Type> {
			{ ".xml", typeof(XmlParser) },
		};

		public static ConfigParser CreateParser(string extension) {
			Type type;
			try { type = _parsers[extension.ToLower()]; }
			catch(System.Exception) { type = _parsers[".xml"]; }
			return (ConfigParser)Activator.CreateInstance(type);
		}

		public static List<Skeleton> Parse(string path) {
			return CreateParser(Path.GetExtension(path)).Parse(path);
        }
	}
}
