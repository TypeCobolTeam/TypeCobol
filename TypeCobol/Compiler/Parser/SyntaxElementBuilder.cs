using System;
using System.Collections.Generic;
using Antlr4.Runtime.Tree;
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

            if (context.figurativeConstant() != null)
            {
                if (context.figurativeConstant().HIGH_VALUE() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().HIGH_VALUE()));
                }
                if (context.figurativeConstant().HIGH_VALUES() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().HIGH_VALUES()));
                }
                if (context.figurativeConstant().LOW_VALUE() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().LOW_VALUE()));
                }
                if (context.figurativeConstant().LOW_VALUES() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().LOW_VALUES()));
                }
                if (context.figurativeConstant().NULL() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().NULL()));
                }
                if (context.figurativeConstant().NULLS() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().NULLS()));
                }
                if (context.figurativeConstant().QUOTE() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().QUOTE()));
                }
                if (context.figurativeConstant().QUOTES() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().QUOTES()));
                }
                if (context.figurativeConstant().SPACE() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().SPACE()));
                }
                if (context.figurativeConstant().SPACES() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().SPACES()));
                }
                if (context.figurativeConstant().ZERO() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().ZERO()));
                }
                if (context.figurativeConstant().ZEROES() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().ZEROES()));
                }
                if (context.figurativeConstant().ZEROS() != null)
                {
                    return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.figurativeConstant().ZEROS()));
                }
                throw new InvalidOperationException("Figurative constant not recognised");
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
            //TODO figurative constant
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





        internal static IList<Identifier> CreateIdentifiers(IReadOnlyList<CobolCodeElementsParser.IdentifierContext> context)
        {
            IList<Identifier> identifiers = new List<Identifier>();
            if (context != null)
                foreach (var identifier in context)
                {
                   var i = CreateIdentifier(identifier);
                    if (i != null) identifiers.Add(i);
                }
            return identifiers;
        }

        public static Identifier CreateIdentifier(CobolCodeElementsParser.IdentifierContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateIdentifier(context.dataNameReferenceOrSpecialRegisterOrFunctionIdentifier());
            if (identifier != null ) identifier.SetReferenceModifier(CreateReferenceModifier(context.referenceModifier()));
            return identifier;
        }


        private static Identifier CreateIdentifier(CobolCodeElementsParser.DataNameReferenceOrSpecialRegisterOrFunctionIdentifierContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataNameReference(context.dataNameReference());
            if (identifier != null ) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            return null;
        }

        private static Identifier CreateFunctionReference(CobolCodeElementsParser.FunctionIdentifierContext context)
        {
            if (context == null || context.FUNCTION() == null) return null;
            var symbol = new FunctionName(ParseTreeUtils.GetFirstToken(context.intrinsicFunctionName()));
            var parameters = CreateFunctionParameters(context.argument());
            if (symbol != null || parameters != null) return new FunctionReference(symbol, parameters);
            return null;
        }

        private static IList<FunctionParameter> CreateFunctionParameters(IReadOnlyList<CobolCodeElementsParser.ArgumentContext> context)
        {
            if (context == null) return null;
            IList<FunctionParameter> parameters = new List<FunctionParameter>();
            foreach (var argument in context) parameters.Add(CreateFunctionParameter(argument));
            return parameters;
        }

        private static FunctionParameter CreateFunctionParameter(CobolCodeElementsParser.ArgumentContext context)
        {
            if (context.literal() != null) return new FunctionParameter(CreateLiteral(context.literal()));
            if (context.arithmeticExpression() != null) return new FunctionParameter(new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression()));
            if (context.identifier() != null) return new FunctionParameter(CreateIdentifier(context.identifier()));
            return null;
        }

        private static Identifier CreateSpecialRegister(CobolCodeElementsParser.SpecialRegisterContext context)
        {
            if (context == null) return null;
            return new SpecialRegister(new SpecialRegisterName(ParseTreeUtils.GetFirstToken(context)));
        }

        private static DataReference CreateDataNameReference(CobolCodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            QualifiedDataName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static QualifiedDataName CreateQualifiedName(CobolCodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateQualifiedName(context.qualifiedDataName());
        }

        public static QualifiedDataName CreateQualifiedName(CobolCodeElementsParser.QualifiedDataNameContext context)
        {
            SymbolReference<DataName> dataname = null;
            if (context.dataNameBase() != null) dataname = CreateDataName(context.dataNameBase().dataName());
            List<SymbolReference<DataName>> datanames = CreateDataNames(context.dataName());
            SymbolReference<FileName> filename = CreateFileName(context.fileName());
            return new QualifiedDataName(dataname, datanames, filename);
        }

        private static List<SymbolReference<DataName>> CreateDataNames(IReadOnlyList<CobolCodeElementsParser.DataNameContext> context)
        {
            if (context == null) return null;
            List<SymbolReference<DataName>> datanames = new List<SymbolReference<DataName>>();
            foreach (var dataname in context) datanames.Add(CreateDataName(dataname));
            datanames.Reverse();
            return datanames;
        }

        public static SymbolReference<DataName> CreateDataName(CobolCodeElementsParser.DataNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<DataName>(new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }
        public static SymbolReference<FileName> CreateFileName(CobolCodeElementsParser.FileNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<FileName>(new FileName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }

        public static SymbolReference<IndexName> CreateIndexName(CobolCodeElementsParser.IndexNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<IndexName>(new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }

        internal static QualifiedProcedureName CreateProcedureName(CobolCodeElementsParser.ProcedureNameContext context)
        {
            SymbolReference<ParagraphName> paragraphname = null;
            if (context.paragraphName() != null) paragraphname = CreateParagraphName(context.paragraphName());
            SymbolReference<SectionName> sectionname = null;
            if (context.sectionName() != null) sectionname = CreateSectionName(context.sectionName());
            return new QualifiedProcedureName(paragraphname, sectionname);
        }

        public static SymbolReference<ParagraphName> CreateParagraphName(CobolCodeElementsParser.ParagraphNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<ParagraphName>(new ParagraphName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }

        public static SymbolReference<SectionName> CreateSectionName(CobolCodeElementsParser.SectionNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<SectionName>(new SectionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }

        public static SymbolReference<XmlSchemaName> CreateXmlSchemaName(CobolCodeElementsParser.XmlSchemaNameContext context)
        {
            if (context == null) return null;
            return new SymbolReference<XmlSchemaName>(new XmlSchemaName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord())));
        }

        private static IList<Subscript> CreateSubscripts(CobolCodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSubscripts(context.subscript());
        }

        private static IList<Subscript> CreateSubscripts(IReadOnlyList<CobolCodeElementsParser.SubscriptContext> context)
        {
            if (context == null) return null;
            var subscripts = new List<Subscript>();
            foreach (var subscript in context) subscripts.Add(CreateSubscript(subscript));
            return subscripts;
        }


        public static Subscript CreateSubscript(CobolCodeElementsParser.SubscriptContext context)
        {
            if (context == null) return null;

            Subscript subscript = new Subscript();
            if (context.IntegerLiteral() != null)
            {
                subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.ALL() != null)
            {
                var token = ParseTreeUtils.GetTokenFromTerminalNode(context.ALL());
                subscript.indexname = new SymbolReference<IndexName>(new IndexName(token));
            }
            if (context.dataName() != null)
            {
                subscript.dataname = CreateDataName(context.dataName());
                InitializeSubscriptOperatorAndLiteral(subscript, context.withRelativeSubscripting());
            }
            if (context.indexName() != null)
            {
                subscript.indexname = CreateIndexName(context.indexName());
                InitializeSubscriptOperatorAndLiteral(subscript, context.withRelativeSubscripting());
            }
            return subscript;
        }

        private static void InitializeSubscriptOperatorAndLiteral(Subscript subscript, CobolCodeElementsParser.WithRelativeSubscriptingContext context)
        {
            if (context.PlusOperator() != null) subscript.op = '+';
            if (context.MinusOperator() != null) subscript.op = '-';
            if (context.IntegerLiteral() != null) subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
        }

        private static Substring CreateReferenceModifier(CobolCodeElementsParser.ReferenceModifierContext context)
        {
            if (context == null) return null;
            var builder = new ArithmeticExpressionBuilder();
            ArithmeticExpression left = null, right = null;
            if (context.leftMostCharacterPosition() != null)
                left = builder.CreateArithmeticExpression(context.leftMostCharacterPosition().arithmeticExpression());
            if (context.length() != null)
                right = builder.CreateArithmeticExpression(context.length().arithmeticExpression());
            if (left == null && right == null) return null;
            return new Substring(left, right);
        }



        private static Address CreateAddressOf(CobolCodeElementsParser.AddressOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataNameReference() == null) return null;
            return new Address(CreateDataNameReference(context.dataNameReference()));
        }

        private static Length CreateLengthOf(CobolCodeElementsParser.LengthOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataNameReference() == null) return null;
            return new Length(CreateDataNameReference(context.dataNameReference()));
        }

        private static LinageCounter CreateLinageCounter(CobolCodeElementsParser.LinageCounterSpecialRegisterDeclContext context)
        {
            if (context == null || context.fileName() == null) return null;
            return new LinageCounter(CreateFileName(context.fileName()).Symbol);
        }




        internal static MnemonicForEnvironmentName CreateMnemonic(CobolCodeElementsParser.MnemonicForEnvironmentNameContext context)
        {
            if (context == null) return null;
            return new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        internal static MnemonicOrEnvironmentName CreateMnemonic(CobolCodeElementsParser.MnemonicOrEnvironmentNameContext context)
        {
            if (context == null) return null;
            return new MnemonicOrEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        public static Index CreateIndex(CobolCodeElementsParser.IndexNameContext indexName)
        {
            if (indexName == null) return null;
            return new Index(new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(indexName.UserDefinedWord())));
        }

        public static Expression CreateProcedurePointer(CobolCodeElementsParser.ProcedurePointerContext procedurePointer)
        {
            //TODO
            throw new NotImplementedException();
        }

        public static Expression CreateFunctionPointer(CobolCodeElementsParser.FunctionPointerContext functionPointer)
        {
            //TODO
            throw new NotImplementedException();
        }

        public static Expression CreatePointerDataItem(CobolCodeElementsParser.PointerDataItemContext pointerDataItem)
        {
            //TODO
            throw new NotImplementedException();
        }

        public static Expression CreateObjectReferenceId(CobolCodeElementsParser.ObjectReferenceIdContext objectReferenceId)
        {
            //TODO
            throw new NotImplementedException();
        }

        public static Expression CreateAddressOfIdentifier(ITerminalNode address, CobolCodeElementsParser.IdentifierContext identifier)
        {
            throw new NotImplementedException();
        }



        public static Expression CreateIdentifierOrLiteral(CobolCodeElementsParser.IdentifierOrLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return SyntaxElementBuilder.CreateLiteral(context.literal());
            return null;
        }


        internal static Expression CreateEncoding(CobolCodeElementsParser.CodepageContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
                return CreateIdentifier(context.identifier());
            if (context.IntegerLiteral() != null)
                return new Literal(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral())));
            return null;
        }
    }

}
