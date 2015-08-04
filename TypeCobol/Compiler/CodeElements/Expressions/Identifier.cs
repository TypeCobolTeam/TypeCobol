using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    class Identifier : Expression
    {
        public Identifier(CobolCodeElementsParser.IdentifierRoundedContext context)
            : this(context.identifier())
        {
            ROUNDED = context.ROUNDED() != null;
        }

        public Identifier(CobolCodeElementsParser.IdentifierContext context)
        {
            token = ParseTreeUtils.GetFirstToken(context);
            if (context.identifierFormat1() != null)
            {
                InitializeIdentifierFormat1(context.identifierFormat1());
            } else
            if (context.identifierFormat2() != null)
            {
                InitializeIdentifierFormat2(context.identifierFormat2());
            } else
            if (context.identifierFormat3() != null)
            {
                InitializeIdentifierFormat3(context.identifierFormat3());
            }
        }

        public Token token { get; private set; }
        public bool ROUNDED = false;
        public Token LINAGE_COUNTER = null;
        public INOFList inof = new INOFList();
        public SubscriptList subscripts = new SubscriptList();

        private void InitializeIdentifierFormat1(CobolCodeElementsParser.IdentifierFormat1Context context)
        {
            if (context.dataName() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(context.dataName());
                SymbolReference<DataName> data = new SymbolReference<DataName>(new DataName(token));
                //TODO and ... now ?
            }
            inof.AddDataNames(context.inOrOfDataName());
            inof.AddFileName(context.inOrOfFileName());

            if (context.subscript() != null)
            {
                foreach (var subscript in context.subscript())
                {
                    subscripts.Add(subscript);
                }
            }

            //TODO: reference modifiers
        }

        private void InitializeIdentifierFormat2(CobolCodeElementsParser.IdentifierFormat2Context context)
        {
            var condition = context.conditionName(); //TODO
            inof.AddDataNames(context.inOrOfDataName());
            inof.AddFileName(context.inOrOfFileName());
        }

        private void InitializeIdentifierFormat3(CobolCodeElementsParser.IdentifierFormat3Context context)
        {
            LINAGE_COUNTER = ParseTreeUtils.GetFirstToken(context.LINAGE_COUNTER());
            inof.AddFileName(context.inOrOfFileName());
        }



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
        public bool IN { get; private set; }
        public bool OF { get; private set; }

        public INOF(S symbol, bool IN, bool OF)
        {
            this.reference = new SymbolReference<S>(symbol);
            this.IN = IN;
            this.OF = OF;
        }

        public override string ToString()
        {
            StringBuilder res = new StringBuilder("");
            if (IN) res.Append(">");
            if (OF) res.Append("<");
            res.Append(reference);
            if (res.Length > 0) return res.ToString();
            return "?";
        }
    }

    public class INOFList
    {
        public List<INOF<DataName>> datanames = new List<INOF<DataName>>();
        public List<INOF<FileName>> filenames = new List<INOF<FileName>>();

        public void AddDataNames(IReadOnlyList<CobolCodeElementsParser.InOrOfDataNameContext> context)
        {
            if (context == null) return;
            foreach (var inof in context) AddDataName(inof);
        }

        public void AddDataName(CobolCodeElementsParser.InOrOfDataNameContext context)
        {
            if (context == null || context.dataName() == null) return;
            DataName dataname = new DataName(ParseTreeUtils.GetFirstToken(context.dataName().UserDefinedWord()));
            datanames.Add(new INOF<DataName>(dataname, context.IN() != null, context.OF() != null));
        }
        public void AddFileName(CobolCodeElementsParser.InOrOfFileNameContext context)
        {
            if (context == null || context.fileName() == null) return;
            FileName filename = new FileName(ParseTreeUtils.GetFirstToken(context.fileName().UserDefinedWord()));
            filenames.Add(new INOF<FileName>(filename, context.IN() != null, context.OF() != null));
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

        private void InitializeSubscriptOperatorAndLiteral(Subscript subscript,
            Antlr4.Runtime.Tree.ITerminalNode plus,
            Antlr4.Runtime.Tree.ITerminalNode minus,
            Antlr4.Runtime.Tree.ITerminalNode integer)
        {
            if (plus != null) subscript.op = '+';
            if (minus != null) subscript.op = '-';
            if (integer != null) subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(integer));
        }

        public void Add(CobolCodeElementsParser.SubscriptContext context)
        {
            if (context == null) return;

            Subscript subscript = new Subscript();
            if (context.subscriptLine1() != null)
            {
                InitializeSubscriptOperatorAndLiteral(subscript, null, null, context.subscriptLine1().IntegerLiteral());
            }
            if (context.subscriptLine2() != null)
            {
                Token token = ParseTreeUtils.GetTokenFromTerminalNode(context.subscriptLine2().ALL());
                subscript.indexname = new SymbolReference<IndexName>(new IndexName(token));
            }
            if (context.subscriptLine3() != null)
            {
                if (context.subscriptLine3().dataName() != null)
                {
                    Token token = ParseTreeUtils.GetTokenFromTerminalNode(context.subscriptLine3().dataName().UserDefinedWord());
                    subscript.dataname = new SymbolReference<DataName>(new DataName(token));
                }
                InitializeSubscriptOperatorAndLiteral(subscript, context.subscriptLine3().PlusOperator(), context.subscriptLine3().MinusOperator(), context.subscriptLine3().IntegerLiteral());
            }
            if (context.subscriptLine4() != null)
            {
                if (context.subscriptLine4().indexName() != null)
                {
                    Token token = ParseTreeUtils.GetTokenFromTerminalNode(context.subscriptLine4().indexName().UserDefinedWord());
                    subscript.indexname = new SymbolReference<IndexName>(new IndexName(token));
                }
                InitializeSubscriptOperatorAndLiteral(subscript, context.subscriptLine4().PlusOperator(), context.subscriptLine4().MinusOperator(), context.subscriptLine4().IntegerLiteral());
            }
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
