using System.Xml;
using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons;
using System.IO;

namespace TypeCobol.Codegen.Config {

	/// <summary>XML Skeleton file parser<see cref="Skeleton"/> parser.</summary>
	public class XmlParser: ConfigParser {
		internal static string TAG_SKELETON     = "skeleton";
		internal static string TAG_CONDITION_LIST = "conditions";
		internal static string TAG_CONDITION      = "condition";
        internal static string ATTR_NODE        = "node";
		internal static string ATTR_CLAUSE      = "clause";
		internal static string TAG_PATTERN_LIST = "patterns";
		internal static string TAG_PATTERN      = "pattern";
		internal static string ATTR_NAME        = "name";
		internal static string ATTR_GROUP       = "group";
		internal static string ATTR_LOCATION    = "location";
		internal static string ATTR_ACTION      = "action";
		internal static string ATTR_VARIABLES   = "var";
        internal static string ATTR_POSITION = "position";

        /// <summary>Parses an XML file.</summary>
        /// <param name="path">Path to an XML file</param>
        /// <returns>List of <see cref="Skeleton"/> defined in <paramref name="path"/></returns>
        public List<Skeleton> Parse(string path)
        {
            string[] files = null;
            //If path contains "*"
            if (path.Contains("*"))
            {
                string fileName = Path.GetFileName(path);
                path = string.Concat(Path.GetDirectoryName(path), "\\");
                files = Directory.GetFiles(path, fileName);
            }
            //If path contains only directory.
            else if (Path.GetFileName(path).Equals(string.Empty))
            {
                files = Directory.GetFiles(path, "*.*");
            }
            //If above conditions fail, it means the path has a specific file name.
            else
            {
                files = new string[1];
                files[0] = path;
            }

            var skeletons = new List<Skeleton>();
            var factory = new SkeletonFactory();
            foreach (var filePath in files)
            {
                var xml = new XmlDocument();
                xml.Load(filePath);

                foreach (var node in xml.ChildNodes)
                {
                    var e = node as XmlElement;
                    if (e == null) continue;
                    foreach (var child in e.ChildNodes)
                    {
                        var ce = child as XmlElement;
                        if (ce == null) continue;
                        if (!ce.LocalName.ToLower().Equals(TAG_SKELETON)) continue;
                        skeletons.Add(factory.Create(ce));
                    }
                }
            }
            return skeletons;
        }

		internal static string GetAttribute(XmlElement e, string name, string defaultvalue = null) {
			string a = e.GetAttribute(name);
			if (a == null || a.Length == 0) return defaultvalue;
			return a;
		}
		internal static Dictionary<string,string> GetAttributes(XmlElement e) {
			var attrs = new Dictionary<string,string>();
			foreach(XmlAttribute attr in e.Attributes) {
				attrs[attr.LocalName.ToLower()] = attr.Value;
			}
			return attrs;
		}
	}



	interface Factory<T> {
		/// <summary>Creates an object from a given <see cref="System.Xml.XMLElement"/>.</summary>
		/// <typeparam name="T">Type of object created</param>
		/// <param name="e">XML element</param>
		/// <returns>Created object</returns>
		T Create(XmlElement e);
	}

	internal class SkeletonFactory: Factory<Skeleton> {
		private static ConditionListFactory CFactory = new ConditionListFactory();
		private static PatternListFactory PFactory = new PatternListFactory();
		public Skeleton Create(XmlElement e) {
			var skeleton = new Skeleton();
			skeleton.Conditions = new List<Condition>();
			skeleton.Patterns = new List<Pattern>();
			foreach(var child in e.ChildNodes) {
				var ce = child as XmlElement;
				if (ce == null) continue;
				skeleton.Name = XmlParser.GetAttribute(e, XmlParser.ATTR_NAME);
				string name = ce.LocalName.ToLower();
				if (name.Equals(XmlParser.TAG_CONDITION_LIST)) {
					skeleton.Conditions.AddRange(CFactory.Create(ce));
				} else
				if (name.Equals(XmlParser.TAG_PATTERN_LIST)) {
					skeleton.Patterns.AddRange(PFactory.Create(ce));
				}

			}

			skeleton.Properties = new List<string>();
			string vars = e.GetAttribute(XmlParser.ATTR_VARIABLES);
			foreach(var var in vars.Split(',')) {
				string property = var.Trim();
				if (property.Length > 0) skeleton.Properties.Add(property);
			}

			return skeleton;
		}
	}

	internal class ConditionListFactory: Factory<List<Condition>> {
		private static ConditionFactory Factory = new ConditionFactory();
		public List<Condition> Create(XmlElement e) {
			var conditions = new List<Condition>();
			foreach(var child in e.ChildNodes) {
				var ce = child as XmlElement;
				if (ce == null) continue;
				if (!ce.LocalName.ToLower().Equals(XmlParser.TAG_CONDITION)) continue;
				conditions.Add(Factory.Create(ce));
			}
			return conditions;
		}
	}

	internal class ConditionFactory: Factory<Condition> {
		private Dictionary<string,List<System.Type>> namespaces = new Dictionary<string,List<System.Type>>();

		public Condition Create(XmlElement e) {
			var condition = new ConditionOnNode();
			condition.Attributes = XmlParser.GetAttributes(e);
			if (condition.Attributes.ContainsKey("node")) {
				condition.Node = GetType(condition.Attributes["node"]);
				condition.Attributes.Remove("node");
			}
			return condition;
		}

		private System.Type GetType(string fullname) {
			int index = fullname.LastIndexOf('.');
			string nspace = fullname.Substring(0, index);
			if (!namespaces.ContainsKey(nspace))
				namespaces[nspace] = TypeCobol.Tools.Reflection.GetTypesInNamespace(nspace);
			foreach(var type in namespaces[nspace])
				if (type.FullName.Equals(fullname)) return type;
			throw new System.ArgumentException("Undefined type: "+fullname);
		}
	}

	internal class PatternListFactory: Factory<List<Pattern>> {
		private static PatternFactory Factory = new PatternFactory();
		public List<Pattern> Create(XmlElement e) {
			var patterns = new List<Pattern>();
			foreach(var child in e.ChildNodes) {
				var ce = child as XmlElement;
				if (ce == null) continue;
				if (!ce.LocalName.ToLower().Equals(XmlParser.TAG_PATTERN)) continue;
				patterns.Add(Factory.Create(ce));
			}
			return patterns;
		}
	}

	internal class PatternFactory: Factory<Pattern> {
		public Pattern Create(XmlElement e) {
			var pattern = new Pattern();
			pattern.Name = XmlParser.GetAttribute(e, XmlParser.ATTR_NAME);
			pattern.Group = XmlParser.GetAttribute(e, XmlParser.ATTR_GROUP);
			pattern.Location = XmlParser.GetAttribute(e, XmlParser.ATTR_LOCATION);
			pattern.Action = XmlParser.GetAttribute(e, XmlParser.ATTR_ACTION);
			pattern.Variables = new Dictionary<string,string>();
			string vars = e.GetAttribute(XmlParser.ATTR_VARIABLES);
			foreach(var var in vars.Split(',')) {
				var kv = var.Split('=');
				if (kv.Length != 2) continue;
				string key = kv[0].Trim();
				string value = kv[1].Trim().ToLower();
				pattern.Variables.Add(key, value);
			}

            string pos = XmlParser.GetAttribute(e, XmlParser.ATTR_POSITION);
            if (pos != null)
            {
                try
                {
                    pattern.Position = int.Parse(pos);
                }
                catch (System.FormatException)
                {
                    //do not set pattern.Position
                }
            }

            pattern.Template = e.InnerText;
			return pattern;
		}
	}
}
