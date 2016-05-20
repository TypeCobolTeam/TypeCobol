using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// A name that exists within a hierarchy of names can be made unique 
    /// by specifying one or more higher-level names in the hierarchy. 
    /// The higher-level names are called qualifiers, and the process by which 
    /// such names are made unique is called qualification.
    /// </summary>
    public class QualifiedSymbolReference : SymbolReference
    {
        public QualifiedSymbolReference(SymbolReference qualifiedSymbol, SymbolReference qualifierSymbol) :
            base(qualifiedSymbol.NameLiteral, qualifiedSymbol.Type)
        {
            QualifiedSymbol = qualifiedSymbol;
            QualifierSymbol = qualifierSymbol;
        }

        public SymbolReference QualifiedSymbol { get; private set; }

        public SymbolReference QualifierSymbol { get; private set; }

        /// <summary>
        /// Used to resolve the symbol reference in a hierarchy of names
        /// </summary>
        public override string DefinitionPathPattern
        {
            get
            {
                return "\\." + QualifiedSymbol.Name + "\\..*" + QualifiedSymbol.DefinitionPathPattern;
            }
        }
    }

    /// <summary>
    /// Specific case of qualified symbol name with no hierarchy of names : 
    /// paragraphNameReference (IN | OF) sectionNameReference
    /// </summary>
    public class QualifiedParagraphNameReference : SymbolReference
    {
        public QualifiedParagraphNameReference(SymbolReference paragraphName, SymbolReference sectionName) :
            base(paragraphName.NameLiteral, paragraphName.Type)
        {
            SectionName = sectionName;
        }        

        public SymbolReference SectionName { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (SectionName == null)
            {
                return base.ToString();
            }
            else
            {
                return base.ToString() + " IN " + SectionName.ToString();
            }
        }
    }

    /// <summary>
    /// Unique case of qualified external name : 
    /// textName (IN | OF) libraryName
    /// </summary>
    public class QualifiedTextName : ExternalName
    {
        public QualifiedTextName(ExternalName textName, ExternalName libraryName) :
            base(textName.NameLiteral, textName.Type)
        {
            LibraryName = libraryName;
        }

        public ExternalName LibraryName { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (LibraryName == null)
            {
                return base.ToString();
            }
            else
            {
                return base.ToString() + " IN " + LibraryName.ToString();
            }
        }
    }
}


// -- OLD CODE --

namespace TypeCobol.Compiler.CodeElements.Expressions {

    /* Extract of SymbolInformation
        
        /// <summary>
        /// Several names can be qualified by the names of enclosing scopes, for example :
        ///    paragraphName IN sectionName
        ///    dataName1 IN dataName2 IN fileName
        ///    conditionName IN dataName3 IN dataName4
        /// In this case, this property contains the ordered list of qualifying tokens, for exemple :
        ///    paragraphName => QualifiedBy = { sectionName }
        ///    dataName1 => QualifiedBy = { dataName2, fileName }
        ///    conditionName => QualifiedBy = { dataName3, dataName4 }
        /// </summary>
        public Token[] QualifedBy { get; set; }

        /// <summary>
        /// Several names can be qualified by the names of enclosing scopes, for example :
        ///    paragraphName IN sectionName
        ///    dataName1 IN dataName2 IN fileName
        ///    conditionName IN dataName3 IN dataName4
        /// In this case, this property contains the ordered list of qualifying tokens, for exemple :
        ///    sectionName => QualifierFor = paragraphName
        ///    fileName => QualifierFor = dataName2
        ///    dataName2 => QualifierFor = dataName1
        /// </summary>
        public Token QualifierFor { get; set; }

     */

    public interface QualifiedName: IList<string> {
		string Head { get; }
		bool IsExplicit { get; }
	}

	public interface Subscripted {
		Subscript this[string name] { get; }
		IEnumerable<Subscript> Subscripts { get; }
	}



	public class SyntacticQualifiedName: QualifiedName {
		public Symbol Symbol { get; private set; }
		public IList<DataName> DataNames { get; private set; }
		public FileName FileName { get; private set; }
		public bool IsExplicit { get; private set; }

		public SyntacticQualifiedName(Symbol symbol, IList<DataName> datanames = null, FileName filename = null, bool isExplicit = false) {
			this.Symbol = symbol;
			this.DataNames = datanames != null ? datanames : new List<DataName>();
			this.FileName = filename;
			this.IsExplicit = isExplicit;
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			foreach (string name in this) str.Append(name).Append('.');
			str.Length -= 1;
			return str.ToString();
		}

		public string Head {
			get {
				if (Symbol == null) return null;
				return Symbol.Name;
			}
		}

		public IEnumerator<string> GetEnumerator() {
			if (FileName != null) yield return FileName.Name;
			foreach (var dataname in DataNames) yield return dataname.Name;
			if (Symbol != null) yield return Symbol.Name;
		}
		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() { return GetEnumerator(); }

		public int Count {
			get { return DataNames.Count+(FileName!=null?2:1); }
		}
		public bool IsReadOnly {
			get { return true; }
		}
		public void Add(string item)    { throw new System.NotSupportedException(); }
		public bool Remove(string item) { throw new System.NotSupportedException(); }
		public void Clear()             { throw new System.NotSupportedException(); }
		public bool Contains(string item) {
			foreach(string name in this)
				if (name.Equals(item)) return true;
			return false;
		}
		public void CopyTo(string[] array, int index) {
			if (array == null) throw new System.ArgumentNullException();
			if (index < 0) throw new System.ArgumentOutOfRangeException();
			if (array.Length < index+Count) throw new System.ArgumentException();
			int c = 0;
			foreach(string name in this) {
				array[index+c] = name;
				c++;
			}
		}

		public string this[int index] {
			get {
				int c = 0;
				foreach(string name in this)
					if (c == index) return name;
					else c++;
				throw new System.ArgumentOutOfRangeException(index+" outside of [0,"+Count+"[");
			}
			set { throw new System.NotSupportedException(); }
		}
		public int IndexOf(string item) {
			int c = 0;
			foreach(string name in this)
				if (name.Equals(item)) return c;
				else c++;
			return -1;
		}
		public void Insert(int index, string item) { throw new System.NotSupportedException(); }
		public void RemoveAt(int index)            { throw new System.NotSupportedException(); }
	}



	public class SubscriptedQualifiedName: Subscripted, QualifiedName {
		protected List<KeyValuePair<string,Subscript>> names = new List<KeyValuePair<string,Subscript>>();

		public SubscriptedQualifiedName(bool isExplicit = true) {
			this.IsExplicit = isExplicit;
		}

		internal void Add(string name,Subscript subscript) {
			names.Add(new KeyValuePair<string,Subscript>(name, subscript));
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			foreach(var item in names) {
				str.Append(item.Key);
				if (item.Value != null) str.Append('[').Append(item.Value.ToString()).Append(']');
				str.Append('.');
			}
			if (names.Count > 0) str.Length -= 1;
			return str.ToString();
		}


		public string Head {
			get {
				if (names.Count < 1) return null;
				return names[names.Count-1].Key;
			}
		}
		public bool IsExplicit { get; private set; }

		public IEnumerator<string> GetEnumerator() {
			foreach (var item in names)
				yield return (string)item.Key;
		}
		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() { return GetEnumerator(); }

		public int Count {
			get { return names.Count; }
		}
		public bool IsReadOnly {
			get { return false; }
		}
		public void Add(string name)               { names.Add(new KeyValuePair<string,Subscript>(name, null)); }
		public void Insert(int index, string name) { names.Insert(index, new KeyValuePair<string,Subscript>(name, null)); }
		public void RemoveAt(int index)            { names.RemoveAt(index); }
		public bool Remove(string name) {
			foreach(var item in names)
				if (item.Key.Equals(name)) names.Remove(item);
			return true;
		}
		public void Clear()                        { names.Clear(); }
		public bool Contains(string name) {
			foreach(string n in this)
				if (n.Equals(name)) return true;
			return false;
		}
		public void CopyTo(string[] array, int index) {
			if (array == null) throw new System.ArgumentNullException();
			if (index < 0) throw new System.ArgumentOutOfRangeException();
			if (array.Length < index+Count) throw new System.ArgumentException();
			int c = 0;
			foreach(string name in this) {
				array[index+c] = name;
				c++;
			}
		}

		public string this[int index] {
			get {
				int c = 0;
				foreach(string name in this)
					if (c == index) return name;
					else c++;
				throw new System.ArgumentOutOfRangeException(index+" outside of [0,"+Count+"[");
			}
			set { Insert(index, value); }
		}
		public int IndexOf(string item) {
			int c = 0;
			foreach(string name in this)
				if (name.Equals(item)) return c;
				else c++;
			return -1;
		}

		public Subscript this[string name] {
			get {
				foreach(var item in names)
					if (item.Key.Equals(name)) return item.Value;
					//TODO what if same name more than once?
				return null;
			}
		}
		public IEnumerable<Subscript> Subscripts {
			get {
				foreach(var item in names)
					if (item.Value != null)
						yield return item.Value;
			}
		}





		/// <summary>Factory method.</summary>
		/// <param name="identifier">Parsed identifier for this name qualification</param>
		/// <param name="data">Data declaration the created name will fully-qualify</param>
		/// <param name="messages">Error messages. If there are some, there is something wrong with <paramref name="identifier"/>'s name qualification</param>
		/// <returns></returns>
		public static SubscriptedQualifiedName Create(Identifier identifier, DataDescriptionEntry data, out List<string> messages) {
			var names = CreatePairs(identifier, data, out messages);
			var qelement = new TypeCobol.Compiler.CodeElements.Expressions.SubscriptedQualifiedName();
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
					if (c < subscripts.Count) subscript = subscripts[c];
					c++;
				}
				names.Add(new System.Tuple<string,TypeCobol.Compiler.CodeElements.Expressions.Subscript>(name, subscript));
				current = current.TopLevel;
			}
			if (c < subscripts.Count) errors.Add(identifier.Name+": too many subscripts ("+subscripts.Count+" vs expected="+c+')');
			names.Reverse();
			return names;
		}
	}
}
