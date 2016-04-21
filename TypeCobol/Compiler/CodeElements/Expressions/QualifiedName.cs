using System.Collections.Generic;
using System.Collections.Specialized;

namespace TypeCobol.Compiler.CodeElements.Expressions {

	public interface QualifiedName: IList<string> {
		string Head { get; }
		bool IsExplicit { get; }
	}



	public class QualifiedTableElementName {
		OrderedDictionary names = new OrderedDictionary();

		public void Add(string name, Subscript index) {
			names.Add(name, index);
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			foreach(var key in names.Keys) {
				str.Append(key);
				var subscript = names[key] as Subscript;
				if (subscript != null) str.Append('[').Append(subscript.ToString()).Append(']');
				str.Append('.');
			}
			if (names.Count > 0) str.Length -= 1;
			return str.ToString();
		}



		/// <summary>Factory method.</summary>
		/// <param name="identifier">Parsed identifier for this name qualification</param>
		/// <param name="data">Data declaration the created name will fully-qualify</param>
		/// <param name="messages">Error messages. If there are some, there is something wrong with <paramref name="identifier"/>'s name qualification</param>
		/// <returns></returns>
		public static QualifiedTableElementName Create(Identifier identifier, DataDescriptionEntry data, out List<string> messages) {
			var names = CreatePairs(identifier, data, out messages);
			var qelement = new TypeCobol.Compiler.CodeElements.Expressions.QualifiedTableElementName();
			foreach(var pair in names) qelement.Add(pair.Item1,pair.Item2);
			return qelement;
		}

		private static List<System.Tuple<string,Subscript>> CreatePairs(Identifier identifier, DataDescriptionEntry data, out List<string> errors) {
			var names = new List<System.Tuple<string,Subscript>>();
			errors = new List<string>();
			var subscripts = new List<Subscript>();
			subscripts.AddRange((identifier as Subscriptable).Subscripts);
			subscripts.Reverse();
			int c = 0;
			var current = data;
			while (current != null) {
				string name = current.QualifiedName[current.QualifiedName.Count-1];
				TypeCobol.Compiler.CodeElements.Expressions.Subscript subscript = null;
				if (current.IsTableOccurence) {
					if (c >= subscripts.Count) {
						errors.Add(current.QualifiedName+" needs subscripting.");
					} else {
						subscript = subscripts[c];
						if (subscript.IsJustAnOffset) {
							int os = int.Parse(subscript.offset.ToString());
//							if (os < current.MinOccurencesCount) errors.Add(current.Name+" has out of bounds subscripting: "+os+" < min="+current.MinOccurencesCount);
							if (os > current.MaxOccurencesCount) errors.Add(current.Name+" has out of bounds subscripting: "+os+" > max="+current.MaxOccurencesCount);
						} //else TODO: check if subscript.dataname is subscripted too
					}
					c++;
				}
				names.Add(new System.Tuple<string,TypeCobol.Compiler.CodeElements.Expressions.Subscript>(name, subscript));
				current = current.TopLevel;
			}
			if (c < subscripts.Count) errors.Add("Too much subscripts !");
			names.Reverse();
			return names;
		}

	}
}
