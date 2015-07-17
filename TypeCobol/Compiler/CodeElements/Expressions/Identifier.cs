using System.Collections.Generic;
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
            token = ParseTreeUtils.GetFirstToken(context); // TODO forget token altogether ? but then how ToString?
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
        public List<INOF<FileName>> filenames = new List<INOF<FileName>>();
        public List<INOF<DataName>> datanames = new List<INOF<DataName>>();

        private void AddDataNameToCodeElement(CobolCodeElementsParser.InOrOfDataNameContext context)
        {
            if (context.dataName() != null)
            {
                DataName dataname = new DataName(ParseTreeUtils.GetFirstToken(context.dataName().UserDefinedWord()));
                datanames.Add(new INOF<DataName>(dataname, context.IN() != null, context.OF() != null));
            }
        }
        private void AddFileNameToCodeElement(CobolCodeElementsParser.InOrOfFileNameContext context)
        {
            if (context.fileName() != null)
            {
                FileName filename = new FileName(ParseTreeUtils.GetFirstToken(context.fileName().UserDefinedWord()));
                filenames.Add(new INOF<FileName>(filename, context.IN() != null, context.OF() != null));
            }
        }

        private void InitializeSubscript(CobolCodeElementsParser.SubscriptContext subscript)
        {
            if (subscript.subscriptLine1() != null)
            {
                new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(subscript.subscriptLine1().IntegerLiteral())); // TODO
            }
            if (subscript.subscriptLine2() != null)
            {
                subscript.subscriptLine2().ALL();
            }
            if (subscript.subscriptLine3() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(subscript.subscriptLine3().dataName());
                SymbolReference<DataName> dataname = new SymbolReference<DataName>(new DataName(token)); // TODO
            }
            if (subscript.subscriptLine4() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(subscript.subscriptLine4().indexName());
                SymbolReference<IndexName> dataname = new SymbolReference<IndexName>(new IndexName(token)); // TODO
            }
        }

        private void InitializeIdentifierFormat1(CobolCodeElementsParser.IdentifierFormat1Context context)
        {
            if (context.dataName() != null)
            {
                Token token = ParseTreeUtils.GetFirstToken(context.dataName());
                SymbolReference<DataName> data = new SymbolReference<DataName>(new DataName(token));
                //TODO and ... now ?
            }
            if (context.inOrOfDataName() != null)
            {
                foreach (var inof in context.inOrOfDataName())
                {
                    AddDataNameToCodeElement(inof);
                }
            }
            if (context.inOrOfFileName() != null)
            {
                AddFileNameToCodeElement(context.inOrOfFileName());
            }

            if (context.subscript() != null)
            {
                foreach (var subscript in context.subscript())
                {
                    InitializeSubscript(subscript);
                }
            }

            //TODO: reference modifiers
        }

        private void InitializeIdentifierFormat2(CobolCodeElementsParser.IdentifierFormat2Context context)
        {
            var condition = context.conditionName(); //TODO
            if (context.inOrOfDataName() != null)
            {
                foreach (var inof in context.inOrOfDataName())
                {
                    AddDataNameToCodeElement(inof);
                }
            }
            if (context.inOrOfFileName() != null)
            {
                AddFileNameToCodeElement(context.inOrOfFileName());
            }
        }

        private void InitializeIdentifierFormat3(CobolCodeElementsParser.IdentifierFormat3Context context)
        {
            LINAGE_COUNTER = ParseTreeUtils.GetFirstToken(context.LINAGE_COUNTER());
            if (context.inOrOfFileName() != null)
            {
                AddFileNameToCodeElement(context.inOrOfFileName());
            }
        }



        public override string TextValue()
        {
            if (token == null)
            {
                return base.ToString();
            }
            else
            {
                return token.Text;
            }
        }

        public override string ToString()
        {
            return TextValue();
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
    }
}
