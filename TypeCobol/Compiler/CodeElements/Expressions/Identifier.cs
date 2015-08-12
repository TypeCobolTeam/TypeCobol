using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    class Pointer : Expression
    {
        public Identifier identifier { get; private set; }
        public Pointer(Identifier identifier)
        {
            this.identifier = identifier;
        }
        public override string ToString()
        {
            return ("->( " + identifier + ")");
        }
    }
    class Identifier : Expression
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
}
