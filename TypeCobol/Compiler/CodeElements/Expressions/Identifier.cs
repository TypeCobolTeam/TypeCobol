using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    public class Pointer : Expression {
        public Identifier identifier { get; private set; }
        public Pointer(Identifier identifier) { this.identifier = identifier; }
        public override string ToString() { return ("->( " + identifier + ")"); }
    }

    public class Subscript
    {
        public QualifiedName dataname { get; set; }
        public IndexName indexname { get; set; }
        public SyntaxNumber offset { get; set; }
        public char op { get; set; }
        public bool all { get; set; }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder("");
            if (all) res.Append("all");
            if (dataname != null) res.Append(dataname);
            if (indexname != null) res.Append(indexname);
            res.Append(op);
            if (offset != null) res.Append(offset);
            if (res.Length > 0) return res.ToString();
            return "?";
        }
    }



    public interface Identifier : Expression {
        QualifiedName Name { get; }
        void SetReferenceModifier(Substring substring);
    }

    public class DataReference : Identifier
    {
        public QualifiedName Name { get; private set; }
        public IList<Subscript> Subscripts { get; private set; }
        public Substring ReferenceModifier { get; set; }

        public DataReference(QualifiedName name, IList<Subscript> subscripts = null, Substring substring = null)
        {
            this.Name = name;
            this.Subscripts = subscripts != null? subscripts : new List<Subscript>();
            this.ReferenceModifier = substring;
        }

        public void SetReferenceModifier(Substring substring) { this.ReferenceModifier = substring; }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(this.Name.ToString());
            if (this.Subscripts.Count > 0)
            {
                str.Append("( ");
                foreach (var subscript in this.Subscripts) str.Append(subscript).Append(", ");
                if (str[str.Length - 2] == ',') str.Length -= 2;
                str.Append(')');
            }
            if (this.ReferenceModifier != null) str.Append(this.ReferenceModifier.ToString());
            return str.ToString();
        }
    }

    public class Condition : LogicalExpression, Identifier
    {
        /// <summary>ConditionName</summary>
        public QualifiedName Name { get; private set; }
        public IList<Subscript> Subscripts { get; private set; }

        public Condition(QualifiedName name, IList<Subscript> subscripts = null)
        {
            this.Name = name;
            this.Subscripts = subscripts != null? subscripts : new List<Subscript>();
        }

        public void SetReferenceModifier(Substring substring) {
            throw new System.InvalidOperationException("Conditions cannot be reference modified!");
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(this.Name.ToString());
            if (this.Subscripts.Count > 0)
            {
                str.Append("( ");
                foreach (var subscript in this.Subscripts) str.Append(subscript).Append(", ");
                if (str[str.Length - 2] == ',') str.Length -= 2;
                str.Append(')');
            }
            return str.ToString();
        }
    }

	public class QualifiedName: IList<string> {
		public Symbol Symbol { get; private set; }
		public IList<DataName> DataNames { get; private set; }
		public FileName FileName { get; private set; }
		public bool IsExplicit { get; private set; }

		public QualifiedName(Symbol symbol, IList<DataName> datanames = null, FileName filename = null, bool isExplicit = false) {
			this.Symbol = symbol;
			this.DataNames = datanames != null ? datanames : new List<DataName>();
			this.FileName = filename;
			this.IsExplicit = isExplicit;
		}

		public override string ToString() {
			var str = new StringBuilder();
			foreach (string name in this) str.Append(name).Append('.');
			str.Length -= 1;
			return str.ToString();
		}

		public IEnumerator<string> GetEnumerator() {
			if (FileName != null) yield return FileName.Name;
			foreach (var dataname in DataNames) yield return dataname.Name;
			yield return Symbol.Name;
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

    public class QualifiedProcedureName
    {
        public ParagraphName ParagraphName { get; private set; }
        public SectionName   SectionName   { get; private set; }

        public QualifiedProcedureName(ParagraphName paragraphname, SectionName sectionname)
        {
            this.ParagraphName = paragraphname;
            this.SectionName = sectionname;
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (this.ParagraphName != null)
            {
                str.Append(this.ParagraphName);
                if (this.SectionName != null) str.Append('.');
            }
            if (this.SectionName != null) str.Append(this.SectionName);
            return str.ToString();
        }
    }

    public class Substring
    {
        public ArithmeticExpression Left  { get; private set; }
        public ArithmeticExpression Right { get; private set; }

        public Substring(ArithmeticExpression left, ArithmeticExpression right)
        {
            this.Left = left;
            this.Right = right;
        }

        public override string ToString()
        {
            var str = new StringBuilder("(");
            str.Append(this.Left != null?  this.Left.ToString() : "?");
            str.Append(':');
            str.Append(this.Right!= null? this.Right.ToString() : "?");
            return str.Append(')').ToString();
        }
    }



    public class SpecialRegister : Identifier
    {
        /// <summary>
        /// Reference to the special register symbol in the source text
        /// </summary>
        public Symbol Symbol { get; private set; }
        public QualifiedName Name {
            get { return new QualifiedName(Symbol); }
            private set { throw new System.InvalidOperationException(); }
        }

        public Substring Substring { get; set; }

        public SpecialRegister(Symbol symbol, Substring substring = null)
        {
            this.Symbol = symbol;
            this.Substring = substring;
        }

        public void SetReferenceModifier(Substring substring) { this.Substring = substring; }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(Symbol != null ? Symbol.ToString() : "?");
            if (this.Substring != null) str.Append(this.Substring.ToString());
            return str.ToString();
        }
    }



    public class FunctionReference : Identifier
    {
        /// <summary>
        /// Reference to the intrinsic function symbol in the source text
        /// </summary>
        public Symbol Symbol { get; private set; }
        public Substring Substring { get; set; }
        public IList<FunctionParameter> Parameters { get; private set; }
        public QualifiedName Name {
            get { return new QualifiedName(Symbol); }
            private set { throw new System.InvalidOperationException(); }
        }

        public FunctionReference(Symbol symbol, IList<FunctionParameter> parameters = null)
        {
            this.Symbol = symbol;
            this.Parameters = parameters != null ? parameters : new List<FunctionParameter>();
        }

        public void SetReferenceModifier(Substring substring) { this.Substring = substring; }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(Symbol != null ? Symbol.ToString() : "?");
            if (this.Parameters.Count > 0)
            {
                str.Append("( ");
                foreach (var parameter in this.Parameters) str.Append(parameter).Append(", ");
                if (str[str.Length - 2] == ',') str.Length -= 2;
                str.Append(')');
            }
            if (this.Substring != null) str.Append(this.Substring.ToString());
            return str.ToString();
        }
    }

    public class FunctionParameter : Expression
    {
        public FunctionParameter(Identifier identifier) { this.Value = identifier; }
        public FunctionParameter(Literal literal) { this.Value = literal; }
        public FunctionParameter(ArithmeticExpression expression) { this.Value = expression; }

        public Expression Value;

        public override string ToString()
        {
            if (Value != null) return Value.ToString();
            return "?";
        }
    }

    public class LinageCounter : FileName, Identifier
    {
        public LinageCounter(Token filename) : base(filename) { }
        public QualifiedName Name {
            get { return new QualifiedName(this); }
            private set { throw new System.InvalidOperationException(); }
        }

        public Substring Substring { get; set; }
        public void SetReferenceModifier(Substring substring) { this.Substring = substring; }
    }

    public class Address : Identifier
    {
        public DataReference Identifier { get; set; }
        public QualifiedName Name {
            get { return Identifier.Name; }
            private set { throw new System.InvalidOperationException(); }
        }
        public Address(DataReference identifier) { this.Identifier = identifier; }
        public void SetReferenceModifier(Substring substring) { if (this.Identifier != null) this.Identifier.SetReferenceModifier(substring); }
    }

    public class Length : Identifier
    {
        public DataReference Identifier { get; set; }
        public QualifiedName Name {
            get { return Identifier.Name; }
            private set { throw new System.InvalidOperationException(); }
        }
        public Length(DataReference identifier) { this.Identifier = identifier; }
        public void SetReferenceModifier(Substring substring) { if (this.Identifier != null) this.Identifier.SetReferenceModifier(substring); }
    }

    public static class IdentifierUtils
    {
        public static bool IsSubscripted(Identifier identifier) {
            DataReference data = identifier as DataReference;
            if (data != null) return data.Subscripts != null && data.Subscripts.Count > 0;
            Address address = identifier as Address;
            if (address != null) return address.Identifier.Subscripts != null && address.Identifier.Subscripts.Count > 0;
            Length len = identifier as Length;
            if (len != null) return len.Identifier.Subscripts != null && len.Identifier.Subscripts.Count > 0;
            return false;
        }
        public static bool IsReferenceModified(Identifier identifier)
        {
            DataReference data = identifier as DataReference;
            if (data != null) return data.ReferenceModifier != null;
            SpecialRegister reg = identifier as SpecialRegister;
            if (reg != null) return reg.Substring != null;
            FunctionReference fun = identifier as FunctionReference;
            if (fun != null) return fun.Substring != null;
            LinageCounter linage = identifier as LinageCounter;
            if (linage != null) return linage.Substring != null;
            Address address = identifier as Address;
            if (address != null) return address.Identifier.ReferenceModifier != null;
            Length len = identifier as Length;
            if (len != null) return len.Identifier.ReferenceModifier != null;
            return false;
        }
    }
}
