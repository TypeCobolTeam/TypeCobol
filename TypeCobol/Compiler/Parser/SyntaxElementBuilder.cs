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
            if (context == null) return null;
            if (context.numericLiteral() != null)
            {
                SyntaxNumber numberValue = CreateSyntaxNumber(context.numericLiteral());
                return new Literal(numberValue);
            }
            return CreateLiteral(context.alphanumOrNationalLiteral());
        }

        internal static Literal CreateLiteral(CobolCodeElementsParser.AlphanumOrNationalLiteralContext context)
        {
            if (context == null) return null;
            SyntaxString stringValue = CreateSyntaxString(context);
            var literal = new Literal(stringValue);
            literal.All = context.ALL() != null;
            return literal;
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
            return new FunctionReference(symbol, parameters);
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
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static QualifiedName CreateQualifiedName(CobolCodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateQualifiedName(context.qualifiedDataName());
        }

        public static QualifiedName CreateQualifiedName(CobolCodeElementsParser.QualifiedDataNameContext context)
        {
            if (context == null) return null;
            DataName name = null;
            if (context.dataNameBase() != null) name = CreateDataName(context.dataNameBase().dataName());
            List<DataName> datanames = CreateDataNames(context.dataName());
            datanames.Reverse();
            FileName filename = CreateFileName(context.fileName());
            return new QualifiedName(name, datanames, filename);
        }

        public static QualifiedName CreateQualifiedName(CobolCodeElementsParser.QualifiedConditionNameContext context)
        {
            if (context == null) return null;
            ConditionName name = CreateConditionName(context.conditionName());
            List<DataName> datanames = CreateDataNames(context.dataName());
            datanames.Reverse();
            FileName filename = CreateFileName(context.fileName());
            return new QualifiedName(name, datanames, filename);
        }

        internal static IList<QualifiedName> CreateQualifiedNames(IReadOnlyList<CobolCodeElementsParser.QualifiedDataNameContext> context)
        {
            var names = new List<QualifiedName>();
            foreach (var name in context)
            {
                var x = CreateQualifiedName(name);
                if (x != null) names.Add(x);
            }
            return names;
        }

        internal static AlphabetName CreateAlphabetName(CobolCodeElementsParser.AlphabetNameContext context)
        {
            if (context == null) return null;
            return new AlphabetName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static ClassName CreateClassName(CobolCodeElementsParser.ClassNameContext context)
        {
            if (context == null) return null;
            return new ClassName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static List<DataName> CreateDataNames(IReadOnlyList<CobolCodeElementsParser.DataNameContext> context)
        {
            List<DataName> datanames = new List<DataName>();
            if (context != null)
                foreach (var dataname in context)
                {
                    var name = CreateDataName(dataname);
                    if (name != null) datanames.Add(name);
                }
            return datanames;
        }

        public static DataName CreateDataName(CobolCodeElementsParser.DataNameContext context)
        {
            if (context == null) return null;
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        // only used for data description entry
        public static ConditionName CreateConditionName(CobolCodeElementsParser.DataNameContext context)
        {
            if (context == null) return null;
            return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static ConditionName CreateConditionName(CobolCodeElementsParser.ConditionNameContext context)
        {
            if (context == null) return null;
            return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static IList<FileName> CreateFileNames(IReadOnlyList<CobolCodeElementsParser.FileNameContext> context)
        {
            List<FileName> filenames = new List<FileName>();
            if (context != null)
                foreach (var filename in context)
                {
                    var name = CreateFileName(filename);
                    if (name != null) filenames.Add(name);
                }
            filenames.Reverse();
            return filenames;
        }

        public static FileName CreateFileName(CobolCodeElementsParser.FileNameContext context)
        {
            if (context == null) return null;
            return new FileName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static LinageCounter CreateLinageCounter(CobolCodeElementsParser.FileNameContext context) {
            if (context == null) return null;
            return new LinageCounter(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        private static LinageCounter CreateLinageCounter(CobolCodeElementsParser.LinageCounterSpecialRegisterDeclContext context) {
            if (context == null || context.fileName() == null) return null;
            var filename = CreateFileName(context.fileName());
            return new LinageCounter(filename.NameToken);
        }

        public static IndexName CreateIndexName(CobolCodeElementsParser.IndexNameContext context)
        {
            if (context == null) return null;
            return new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static IList<QualifiedProcedureName> CreateProcedureNames(IReadOnlyList<CobolCodeElementsParser.ProcedureNameContext> context)
        {
            List<QualifiedProcedureName> procedurenames = new List<QualifiedProcedureName>();
            if (context != null)
                foreach (var procedurename in context)
                {
                    var name = CreateProcedureName(procedurename);
                    if (name != null) procedurenames.Add(name);
                }
            procedurenames.Reverse();
            return procedurenames;
        }

        internal static QualifiedProcedureName CreateProcedureName(CobolCodeElementsParser.ProcedureNameContext context)
        {
            ParagraphName paragraphname = CreateParagraphName(context.paragraphName());
            SectionName sectionname = CreateSectionName(context.sectionName());
            return new QualifiedProcedureName(paragraphname, sectionname);
        }

        public static ParagraphName CreateParagraphName(CobolCodeElementsParser.ParagraphNameContext context)
        {
            if (context == null) return null;
            return new ParagraphName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static SectionName CreateSectionName(CobolCodeElementsParser.SectionNameContext context)
        {
            if (context == null) return null;
            return new SectionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static XmlSchemaName CreateXmlSchemaName(CobolCodeElementsParser.XmlSchemaNameContext context)
        {
            if (context == null) return null;
            return new XmlSchemaName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        private static IList<Subscript> CreateSubscripts(CobolCodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSubscripts(context.subscript());
        }

        public static IList<Subscript> CreateSubscripts(IReadOnlyList<CobolCodeElementsParser.SubscriptContext> context)
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
                subscript.indexname = new IndexName(token);
            }
            if (context.qualifiedDataName() != null)
            {
                subscript.dataname = CreateQualifiedName(context.qualifiedDataName());
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
            if (context == null) return;
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

        internal static IList<FigurativeConstant> CreateFigurativeConstants(IReadOnlyList<CobolCodeElementsParser.FigurativeConstantContext> context)
        {
            IList<FigurativeConstant> constants = new List<FigurativeConstant>();
            foreach (var c in context)
            {
                var constant = CreateFigurativeConstant(c);
                if (constant != null) constants.Add(constant);
            }
            return constants;
        }

        internal static FigurativeConstant CreateFigurativeConstant(CobolCodeElementsParser.FigurativeConstantContext context)
        {
            if (context.HIGH_VALUE()  != null) return CreateFigurativeConstant(context.HIGH_VALUE());
            if (context.HIGH_VALUES() != null) return CreateFigurativeConstant(context.HIGH_VALUES());
            if (context.LOW_VALUE()  != null) return CreateFigurativeConstant(context.LOW_VALUE());
            if (context.LOW_VALUES() != null) return CreateFigurativeConstant(context.LOW_VALUES());
            if (context.NULL()  != null) return CreateFigurativeConstant(context.NULL());
            if (context.NULLS() != null) return CreateFigurativeConstant(context.NULLS());
            if (context.SPACE()  != null) return CreateFigurativeConstant(context.SPACE());
            if (context.SPACES() != null) return CreateFigurativeConstant(context.SPACES());
            if (context.QUOTE()  != null) return CreateFigurativeConstant(context.QUOTE());
            if (context.QUOTES() != null) return CreateFigurativeConstant(context.QUOTES());
            if (context.ZERO()   != null) return CreateFigurativeConstant(context.ZERO());
            if (context.ZEROS()  != null) return CreateFigurativeConstant(context.ZEROS());
            if (context.ZEROES() != null) return CreateFigurativeConstant(context.ZEROES());
            if (context.SymbolicCharacter() != null) return CreateFigurativeConstant(context.SymbolicCharacter());
            return null;
        }

        private static FigurativeConstant CreateFigurativeConstant(ITerminalNode node)
        {
            return new FigurativeConstant(new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(node)));
        }

        internal static int CreateInteger(ITerminalNode node) {
            var token = ParseTreeUtils.GetTokenFromTerminalNode(node);
            return (int)((TypeCobol.Compiler.Scanner.IntegerLiteralValue)token.LiteralValue).Number;
        }
    }

}
