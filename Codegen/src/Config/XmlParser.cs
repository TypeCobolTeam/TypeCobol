using System.Xml;
using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen.Config {

	/// <summary>XML file <see cref="Skeleton"/> parser.</summary>
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
		internal static string ATTR_VARIABLES   = "var";

		/// <summary>Parses an XML file.</summary>
		/// <param name="path">Path to an XML file</param>
		/// <returns>List of <see cref="Skeleton"/> defined in <paramref name="path"/></returns>
		public List<Skeleton> Parse(string path) {
			var xml = new XmlDocument();
			xml.Load(path);

			var skeletons = new List<Skeleton>();
			var factory = new SkeletonFactory();
			foreach(var node in xml.ChildNodes) {
				var e = node as XmlElement;
				if (e == null) continue;
				foreach(var child in e.ChildNodes) {
					var ce = child as XmlElement;
					if (ce == null) continue;
					if (!ce.LocalName.ToLower().Equals(TAG_SKELETON)) continue;
					skeletons.Add(factory.Create(ce));
				}
			}
			return skeletons;
		}

		internal static string GetAttribute(XmlElement e, string name, string defaultvalue = null) {
			string a = e.GetAttribute(name);
			if (a == null || a.Length == 0) return defaultvalue;
			return a;
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
			return skeleton;
		}
	}

	internal class ConditionListFactory: Factory<List<Condition>> {
		private static ConditionFactory Factory = new ConditionFactory();

		public ConditionListFactory() {
			//assembly = Assembly.GetExecutingAssembly();
			//TypeCobol.Tools.Reflection.GetTypesInNamespace();
		}
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
			string node = XmlParser.GetAttribute(e, XmlParser.ATTR_NODE);
			condition.Node   = GetType(node);
			condition.Clause = XmlParser.GetAttribute(e, XmlParser.ATTR_CLAUSE);
			return condition;
		}

		private System.Type GetType(string fullname) {
			int index = fullname.LastIndexOf('.');
			string nspace = fullname.Substring(0, index);
			if (!namespaces.ContainsKey(nspace))
				namespaces[nspace] = TypeCobol.Tools.Reflection.GetTypesInNamespace(nspace);
			foreach(var type in namespaces[nspace])
				if (type.FullName.Equals(fullname)) return type;
			return null;
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
			pattern.Variables = new Dictionary<string,string>();
			string vars = e.GetAttribute(XmlParser.ATTR_VARIABLES);
			foreach(var var in vars.Split(',')) {
				var kv = var.Split('=');
				if (kv.Length != 2) continue;
				string key = kv[0].Trim().Substring(1);//remove starting %
				string value = kv[1].Trim();
				pattern.Variables.Add(key, value);
			}
			pattern.Template = e.InnerText;
			return pattern;
		}
	}
}
