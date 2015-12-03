using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    public class Pointer : Expression
    {
        public IdentifierOld identifier { get; private set; }
        public Pointer(IdentifierOld identifier)
        {
            this.identifier = identifier;
        }
        public override string ToString()
        {
            return ("->( " + identifier + ")");
        }
    }

    public class IdentifierOld : Expression
    {
        public Token token { get; set; }
        public bool ROUNDED = false;
        public Token LINAGE_COUNTER = null;
        public INOFList inof = new INOFList();
        public SubscriptList subscripts = new SubscriptList();
        
        public override string ToString()
        {
            string token = this.token != null ? this.token.Text : base.ToString();
            StringBuilder res = new StringBuilder(token);
            res.Append(inof);
            res.Append(subscripts);
            return res.ToString();
        }
    }
    
    public class INOF<S> where S : Symbol
    {
        public SymbolReference<S> reference { get; private set; }

        public INOF(S symbol)
        {
            this.reference = new SymbolReference<S>(symbol);
        }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder("");
            res.Append("<");
            res.Append(reference);
            if (res.Length > 0) return res.ToString();
            return "?";
        }
    }

    public class INOFList
    {
        public List<INOF<DataName>> datanames = new List<INOF<DataName>>();
        public List<INOF<FileName>> filenames = new List<INOF<FileName>>();

        public void AddDataName(DataName dataName)
        {
            datanames.Add(new INOF<DataName>(dataName));
        }

        public void AddFileName(FileName fileName)
        {
            filenames.Add(new INOF<FileName>(fileName));
        }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder();
            foreach (var dataname in datanames) res.Append(dataname);
            foreach (var filename in filenames) res.Append(filename);
            return res.ToString();
        }
    }

    public class Subscript
    {
        public QualifiedName<DataName> dataname { get; set; }
        public SymbolReference<IndexName> indexname { get; set; }
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

    public class SubscriptList
    {
        private List<Subscript> subscripts = new List<Subscript>();       

        public void AddSubscript(Subscript subscript)
        {
            subscripts.Add(subscript);
        }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder();
            if (subscripts.Count > 0)
            {
                res.Append("( ");
                foreach (var subscript in subscripts) res.Append(subscript).Append(", ");
                res.Append(')');
            }
            return res.ToString();
        }
    }



    public interface Identifier : Expression {
        void SetReferenceModifier(Substring substring);
    }

    public class DataReference : Identifier
    {
        public QualifiedName<DataName> Name { get; private set; }
        public IList<Subscript> Subscripts { get; private set; }
        public Substring ReferenceModifier { get; set; }

        public DataReference(QualifiedName<DataName> name, IList<Subscript> subscripts = null, Substring substring = null)
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
        public QualifiedName<ConditionName> Name { get; private set; }
        public IList<Subscript> Subscripts { get; private set; }

        public Condition(QualifiedName<ConditionName> name, IList<Subscript> subscripts = null)
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

    public class QualifiedName<T> where T: Symbol
    {
        public SymbolReference<T> Name { get; private set; }
        public IList<SymbolReference<DataName>> DataNames { get; private set; }
        public SymbolReference<FileName> FileName { get; private set; }

        public QualifiedName(SymbolReference<T> dataname, IList<SymbolReference<DataName>> datanames = null, SymbolReference<FileName> filename = null)
        {
            this.Name = dataname;
            this.DataNames = datanames != null ? datanames : new List<SymbolReference<DataName>>();
            this.FileName = filename;
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (this.FileName != null) str.Append(this.FileName).Append('.');
            foreach (var dataname in this.DataNames) str.Append(dataname).Append('.');
            str.Append(this.Name);
            return str.ToString();
        }
    }

    public class QualifiedProcedureName
    {
        public SymbolReference<ParagraphName> ParagraphName { get; private set; }
        public SymbolReference<SectionName>   SectionName   { get; private set; }

        public QualifiedProcedureName(SymbolReference<ParagraphName> paragraphname, SymbolReference<SectionName> sectionname)
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

    public class LinageCounter : SymbolReference<FileName>, Identifier
    {
        public LinageCounter(FileName filename) : base(filename) { }

        public Substring Substring { get; set; }
        public void SetReferenceModifier(Substring substring) { this.Substring = substring; }
    }

    public class Address : Identifier
    {
        public DataReference Identifier { get; set; }
        public Address(DataReference identifier) { this.Identifier = identifier; }
        public void SetReferenceModifier(Substring substring) { if (this.Identifier != null) this.Identifier.SetReferenceModifier(substring); }
    }

    public class Length : Identifier
    {
        public DataReference Identifier { get; set; }
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
