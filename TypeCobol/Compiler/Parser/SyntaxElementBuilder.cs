using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    internal static class SyntaxElementBuilder
    {
        public static SyntaxNumber CreateSyntaxNumber(CobolCodeElementsParser.NumericLiteralContext context)
        {
            if (context.IntegerLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.DecimalLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.DecimalLiteral()));
            }
            if (context.FloatingPointLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.FloatingPointLiteral()));
            }
            if (context.ZERO() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZERO()));
            }
            if (context.ZEROS() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROS()));
            }
            if (context.ZEROES() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROES()));
            }
            throw new InvalidOperationException("This is not a number!");
        }

        public static SyntaxString CreateSyntaxString(CobolCodeElementsParser.AlphanumOrNationalLiteralContext context)
        {
            bool all = context.ALL() != null;//TODO
            if (context.NullTerminatedAlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.NullTerminatedAlphanumericLiteral()));
            }
            if (context.alphanumOrNationalLiteralBase() != null)
            {
                return CreateSyntaxString(context.alphanumOrNationalLiteralBase());
            }
            throw new InvalidOperationException("This is not a string!");
        }

        public static SyntaxString CreateSyntaxString(CobolCodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
        {
            if (context.AlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.AlphanumericLiteral()));
            }
            if (context.HexadecimalAlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.HexadecimalAlphanumericLiteral()));
            }
            throw new InvalidOperationException("This is not a string!");
        }

        public static SyntaxNational CreateSyntaxNational(CobolCodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
        {
            if (context.NationalLiteral() != null)
            {
                return new SyntaxNational(ParseTreeUtils.GetTokenFromTerminalNode(context.NationalLiteral()));
            }
            if (context.HexadecimalNationalLiteral() != null)
            {
                return new SyntaxNational(ParseTreeUtils.GetTokenFromTerminalNode(context.HexadecimalNationalLiteral()));
            }
            throw new InvalidOperationException("This is not a national!");
        }

        public static Literal CreateLiteral(CobolCodeElementsParser.LiteralContext context)
        {
            if (context.numericLiteral() != null)
            {
                SyntaxNumber numberValue = CreateSyntaxNumber(context.numericLiteral());
                return new Literal(numberValue);
            }
            else if (context.alphanumOrNationalLiteral() != null)
            {
                SyntaxString stringValue = CreateSyntaxString(context.alphanumOrNationalLiteral());
                return new Literal(stringValue);
            }
            else
            {
                return null;
            }
        }

        public static Identifier CreateIdentifier(CobolCodeElementsParser.IdentifierRoundedContext context)
        {
            Identifier identifier = CreateIdentifier(context);
            identifier.ROUNDED = context.ROUNDED() != null;
            return identifier;
        }

        public static Identifier CreateIdentifier(CobolCodeElementsParser.IdentifierContext context)
        {
            Identifier identifier = new Identifier();
            /*identifier.token = ParseTreeUtils.GetFirstToken(context);
            if (context.identifierFormat1() != null)
            {
                InitializeIdentifierFormat1(context.identifierFormat1());
            }
            else
            if (context.identifierFormat2() != null)
            {
                InitializeIdentifierFormat2(context.identifierFormat2());
            }
            else
            if (context.identifierFormat3() != null)
            {
                InitializeIdentifierFormat3(context.identifierFormat3());
            }*/
            return identifier;
        }

        /*
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
        */

        /*public static DataName CreateDataName(CobolCodeElementsParser.InOrOfDataNameContext context)
        {
            if (context == null || context.dataName() == null) return null;
            DataName dataname = new DataName(ParseTreeUtils.GetFirstToken(context.dataName().UserDefinedWord()));
            return dataname;
        }
        public static FileName CreateFileName(CobolCodeElementsParser.InOrOfFileNameContext context)
        {
            if (context == null || context.fileName() == null) return null;
            FileName filename = new FileName(ParseTreeUtils.GetFirstToken(context.fileName().UserDefinedWord()));
            return filename;
        }*/

        public static Subscript CreateSubscript(CobolCodeElementsParser.SubscriptContext context)
        {
            if (context == null) return null;

            Subscript subscript = new Subscript();
            /*
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
            */
            return subscript;
        }

        private static void InitializeSubscriptOperatorAndLiteral(Subscript subscript,
            Antlr4.Runtime.Tree.ITerminalNode plus,
            Antlr4.Runtime.Tree.ITerminalNode minus,
            Antlr4.Runtime.Tree.ITerminalNode integer)
        {
            if (plus != null) subscript.op = '+';
            if (minus != null) subscript.op = '-';
            if (integer != null) subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(integer));
        }
    }

    

    
}
