using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Obsolete
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

		public bool IsJustAnOffset {
			get {
				if (offset == null || op == '+' || op == '-' || dataname != null  || indexname != null || all) return false;
				try { int value = int.Parse(offset.ToString()); return true; }
				catch(System.Exception ex) { System.Console.WriteLine("! "+offset.ToString()); }
				return false;
			}
		}

        public override string ToString()
        {
            StringBuilder res = new StringBuilder("");
            if (all) res.Append("all");
            if (dataname != null) res.Append(dataname);
            if (indexname != null) res.Append(indexname);
            if (op == '+' || op == '-') res.Append(op);
            if (offset != null) res.Append(offset);
            if (res.Length > 0) return res.ToString();
            return "?";
        }
    }



	public interface Identifier : Expression {
		QualifiedName Name { get; }
	}
	public interface Subscriptable {
		IList<Subscript> Subscripts { get; }
		void UpdateSubscripting(SubscriptedQualifiedName e);
	}
	public interface ReferenceModifiable {
		Substring ReferenceModifier { get; set; }
	}

	public class DataReference : Identifier, Subscriptable, ReferenceModifiable
	{
		public QualifiedName Name { get; private set; }
		public IList<Subscript> Subscripts { get; private set; }
		public Substring ReferenceModifier { get; set; }

		public DataReference(QualifiedName name, IList<Subscript> subscripts = null, Substring substring = null) {
			this.Name = name;
			this.Subscripts = subscripts != null? subscripts : new List<Subscript>();
			this.ReferenceModifier = substring;
		}

		public override string ToString() {
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

		public void UpdateSubscripting(SubscriptedQualifiedName e) {
			Name = e;
			Subscripts = null;
		}
	}

    public class Condition : LogicalExpression, Identifier, Subscriptable
    {
        /// <summary>ConditionName</summary>
        public QualifiedName Name { get; private set; }
        public IList<Subscript> Subscripts { get; private set; }

        public Condition(QualifiedName name, IList<Subscript> subscripts = null)
        {
            this.Name = name;
            this.Subscripts = subscripts != null? subscripts : new List<Subscript>();
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

		public void UpdateSubscripting(SubscriptedQualifiedName e) {
			Name = e;
			Subscripts = null;
		}
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



	public class SpecialRegister : Identifier, ReferenceModifiable
	{
		/// <summary>
		/// Reference to the special register symbol in the source text
		/// </summary>
		public Symbol Symbol { get; private set; }
		public QualifiedName Name {
			get { return new SyntacticQualifiedName(Symbol); }
			private set { throw new System.InvalidOperationException(); }
		}

		public Substring ReferenceModifier { get; set; }

		public SpecialRegister(Symbol symbol, Substring substring = null) {
			this.Symbol = symbol;
			this.ReferenceModifier = substring;
		}

		public override string ToString() {
			var str = new StringBuilder();
			str.Append(Symbol != null ? Symbol.ToString() : "?");
			if (this.ReferenceModifier != null) str.Append(this.ReferenceModifier.ToString());
			return str.ToString();
		}
	}



	public class FunctionReference : Identifier, ReferenceModifiable
	{
		/// <summary>
		/// Reference to the intrinsic function symbol in the source text
		/// </summary>
		public Symbol Symbol { get; private set; }
		public Substring ReferenceModifier { get; set; }
		public IList<FunctionParameter> Parameters { get; private set; }
		public QualifiedName Name {
			get { return new SyntacticQualifiedName(Symbol); }
			private set { throw new System.InvalidOperationException(); }
		}

		public FunctionReference(Symbol symbol, IList<FunctionParameter> parameters = null) {
			this.Symbol = symbol;
			this.Parameters = parameters != null ? parameters : new List<FunctionParameter>();
		}

		public override string ToString() {
			var str = new StringBuilder();
			str.Append(Symbol != null ? Symbol.ToString() : "?");
			if (this.Parameters.Count > 0)
			{
				str.Append("( ");
				foreach (var parameter in this.Parameters) str.Append(parameter).Append(", ");
				if (str[str.Length - 2] == ',') str.Length -= 2;
				str.Append(')');
			}
			if (this.ReferenceModifier != null) str.Append(this.ReferenceModifier.ToString());
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

	public class LinageCounter : FileName, Identifier, ReferenceModifiable
	{
		public LinageCounter(Token filename) : base(filename) { }
		public QualifiedName Name {
			get { return new SyntacticQualifiedName(this); }
			private set { throw new System.InvalidOperationException(); }
		}

		public Substring ReferenceModifier { get; set; }
	}

	public class Address : Identifier, ReferenceModifiable
	{
		public DataReference Identifier { get; set; }
		public QualifiedName Name {
			get { return Identifier.Name; }
			private set { throw new System.InvalidOperationException(); }
		}
		public Address(DataReference identifier) { this.Identifier = identifier; }
		public Substring ReferenceModifier {
			get {
				if (this.Identifier != null)
					return this.Identifier.ReferenceModifier;
				else return null;
			}
			set {
				if (this.Identifier != null)
					this.Identifier.ReferenceModifier = value;
			}
		}
	}

	public class Length : Identifier, ReferenceModifiable
	{
		public DataReference Identifier { get; set; }
		public QualifiedName Name {
			get { return Identifier.Name; }
			private set { throw new System.InvalidOperationException(); }
		}
		public Length(DataReference identifier) { this.Identifier = identifier; }
		public Substring ReferenceModifier {
			get {
				if (this.Identifier != null)
					return this.Identifier.ReferenceModifier;
				else return null;
			}
			set {
				if (this.Identifier != null)
					this.Identifier.ReferenceModifier = value;
			}
		}
	}

	public static class IdentifierUtils
	{
		public static bool IsSubscripted(Identifier identifier) {
			var array = identifier as Subscriptable;
			var name = identifier.Name as Subscripted;
			return (array != null && array.Subscripts.Count > 0)
				|| (name != null && new List<Subscript>(name.Subscripts).Count > 0);
		}
		public static bool IsReferenceModified(Identifier identifier) {
			var substring = identifier as ReferenceModifiable;
			return substring != null && substring.ReferenceModifier != null;
		}

		public static QualifiedName GetQualifiedName(Expression expression) {
			if (!(expression is Identifier)) return null;
			if (expression is SpecialRegister) return null;
			if (expression is FunctionReference) return null;
			return ((Identifier)expression).Name;
		}
		public static DataType GetDataType(Expression expression) {
			var literal = expression as Literal;
			if (literal == null) return null;
			if (literal.IsNumeric) return DataType.Numeric;
			if (literal.IsBoolean) return DataType.Boolean;
			else return DataType.Alphanumeric;
		}
	}
}
