using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    class Pointer : Expression
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
    class IdentifierOld : Expression
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
        public SymbolReference<DataName> dataname { get; set; }
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



    public interface Identifier : Expression { }

    public class DataReference : Identifier
    {
        public QualifiedDataName name { get; private set; }
        public List<Subscript> subscripts { get; private set; }
        public Substring substring { get; private set; }

        public DataReference(QualifiedDataName name, List<Subscript> subscripts = null, Substring substring = null)
        {
            this.name = name;
            this.subscripts = this.subscripts != null? subscripts : new List<Subscript>();
            this.substring = substring;
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(name.ToString());
            if (this.subscripts.Count > 0)
            {
                str.Append("( ");
                foreach (var subscript in this.subscripts) str.Append(subscript).Append(", ");
                if (str[str.Length - 2] == ',') str.Length -= 2;
                str.Append(')');
            }
            if (this.substring != null) str.Append(this.substring.ToString());
            return str.ToString();
        }
    }

    public class QualifiedDataName
    {
        public SymbolReference<DataName> dataname { get; private set; }
        public List<SymbolReference<DataName>> datanames { get; private set; }
        public SymbolReference<FileName> filename { get; private set; }

        public QualifiedDataName(SymbolReference<DataName> dataname, List<SymbolReference<DataName>> datanames = null, SymbolReference<FileName> filename = null)
        {
            this.dataname = dataname;
            this.datanames = this.datanames != null? datanames : new List<SymbolReference<DataName>>();
            this.filename = filename;
        }

        public void InsertParentData(int index, DataName dataname)
        {
            this.datanames.Insert(index, new SymbolReference<DataName>(dataname));
        }

        public void SetParentFileName(FileName filename)
        {
            this.filename = new SymbolReference<FileName>(filename);
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (this.filename != null) str.Append(this.filename).Append('.');
            foreach (var dataname in this.datanames) str.Append(dataname).Append('.');
            str.Append(this.dataname);
            return str.ToString();
        }
    }

    public class Substring
    {
        public ArithmeticExpression left  { get; private set; }
        public ArithmeticExpression right { get; private set; }

        public Substring(ArithmeticExpression left, ArithmeticExpression right)
        {
            this.left = left;
            this.right = right;
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append(left != null?  left.ToString() : "?");
            str.Append(":");
            str.Append(right!= null? right.ToString() : "?");
            return base.ToString();
        }
    }



    public class SpecialRegister : Identifier
    {
        public Token token { get; private set; }
        public Substring substring { get; private set; }

        public SpecialRegister(Token token, Substring substring = null)
        {
            this.token = token;
            this.substring = substring;
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (token != null) str.Append(token.Text);
            else str.Append('?');
            if (this.substring != null) str.Append(this.substring.ToString());
            return str.ToString();
        }
    }



    public class FunctionReference : Identifier
    {
        public Token token { get; private set; }
        public List<FunctionParameter> parameters { get; private set; }

        public FunctionReference(Token token, List<FunctionParameter> parameters = null)
        {
            this.token = token;
            this.parameters = this.parameters != null? parameters : new List<FunctionParameter>();
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (token != null) str.Append(token.Text);
            else str.Append('?');
            if (this.parameters.Count > 0)
            {
                str.Append("( ");
                foreach (var parameter in this.parameters) str.Append(parameter).Append(", ");
                if (str[str.Length - 2] == ',') str.Length -= 2;
                str.Append(')');
            }
            return str.ToString();
        }
    }

    public class FunctionParameter : Expression
    {
        public FunctionParameter(Identifier identifier) { this.Value = identifier; }
        public FunctionParameter(Literal literal) { this.Value = literal; }
        public FunctionParameter(FunctionReference function) { this.Value = function; }

        public Expression Value;

        public override string ToString()
        {
            if (Value != null) return Value.ToString();
            return "?";
        }
    }
}
