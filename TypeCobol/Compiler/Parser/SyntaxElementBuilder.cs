using System;
using System.Collections.Generic;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    internal static class SyntaxElementBuilder
    {
        public static SyntaxNumber CreateSyntaxNumber(CodeElementsParser.NumericLiteralContext context)
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

        public static SyntaxString CreateSyntaxString(CodeElementsParser.AlphanumericLiteralContext context)
        {
            if (context.NullTerminatedAlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.NullTerminatedAlphanumericLiteral()));
            }
            if (context.alphanumericLiteralBase() != null)
            {
                return CreateSyntaxString(context.alphanumericLiteralBase());
            }
            throw new InvalidOperationException("This is not a string!");
        }

        public static SyntaxString CreateSyntaxString(CodeElementsParser.AlphanumericLiteralBaseContext context)
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
                var figurativeConstant = context.figurativeConstant();
                return CreateSyntaxString(figurativeConstant);
                throw new InvalidOperationException("Figurative constant not recognised");
            }
            throw new InvalidOperationException("This is not a string!");
        }

        public static SyntaxString CreateSyntaxString(CodeElementsParser.AlphanumOrNationalLiteralContext context)
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

        public static SyntaxString CreateSyntaxString(CodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
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
                var figurativeConstant = context.figurativeConstant();
                return CreateSyntaxString(figurativeConstant);
            }
            throw new InvalidOperationException("This is not a string!");
        }

        private static SyntaxString CreateSyntaxString(CodeElementsParser.FigurativeConstantContext figurativeConstant)
        {
            if (figurativeConstant.HIGH_VALUE() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.HIGH_VALUE()));
            }
            if (figurativeConstant.HIGH_VALUES() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.HIGH_VALUES()));
            }
            if (figurativeConstant.LOW_VALUE() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.LOW_VALUE()));
            }
            if (figurativeConstant.LOW_VALUES() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.LOW_VALUES()));
            }
            if (figurativeConstant.NULL() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.NULL()));
            }
            if (figurativeConstant.NULLS() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.NULLS()));
            }
            if (figurativeConstant.QUOTE() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.QUOTE()));
            }
            if (figurativeConstant.QUOTES() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.QUOTES()));
            }
            if (figurativeConstant.SPACE() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.SPACE()));
            }
            if (figurativeConstant.SPACES() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.SPACES()));
            }
            if (figurativeConstant.ZERO() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.ZERO()));
            }
            if (figurativeConstant.ZEROES() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.ZEROES()));
            }
            if (figurativeConstant.ZEROS() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(figurativeConstant.ZEROS()));
            }
            throw new InvalidOperationException("Figurative constant not recognised");
        }

        public static SyntaxNational CreateSyntaxNational(CodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
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

        public static Literal CreateLiteral(CodeElementsParser.LiteralContext context)
        {
            if (context == null) return null;
            if (context.numericLiteral() != null)
            {
                SyntaxNumber numberValue = CreateSyntaxNumber(context.numericLiteral());
                return new Literal(numberValue);
            }
            return CreateLiteral(context.alphanumOrNationalLiteral());
        }

        internal static Literal CreateLiteral(CodeElementsParser.AlphanumericLiteralContext context)
        {
            if (context == null) return null;
            SyntaxString stringValue = CreateSyntaxString(context);
            var literal = new Literal(stringValue);
            literal.All = context.ALL() != null;
            return literal;
        }

        internal static Literal CreateLiteral(CodeElementsParser.AlphanumOrNationalLiteralContext context)
        {
            if (context == null) return null;
            SyntaxString stringValue = CreateSyntaxString(context);
            var literal = new Literal(stringValue);
            literal.All = context.ALL() != null;
            return literal;
        }




        
        internal static IList<Identifier> CreateIdentifiers(IReadOnlyList<CodeElementsParser.IdentifierContext> context)
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

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateIdentifier(context.dataNameReferenceOrSpecialRegisterOrFunctionIdentifier());
            if (identifier != null ) identifier.SetReferenceModifier(CreateReferenceModifier(context.referenceModifier()));
            return identifier;
        }
        
        public static Token GetSymbolTokenIfIdentifierIsOneUserDefinedWord(CodeElementsParser.IdentifierContext identifier)
        {
            if (identifier.referenceModifier() == null)
            {
                var dataNameReference = identifier.dataNameReferenceOrSpecialRegisterOrFunctionIdentifier().dataNameReference();
                if (dataNameReference != null)
                {
                    if (dataNameReference.subscript() == null)
                    {
                        var qualifiedDataName = dataNameReference.qualifiedDataName();
                        if (qualifiedDataName.dataName() == null && qualifiedDataName.fileName() == null)
                        {
                            return ParseTreeUtils.GetFirstToken(qualifiedDataName.dataNameBase());
                        }
                    }
                }
            }
            return null;
        }

        private static Identifier CreateIdentifier(CodeElementsParser.DataNameReferenceOrSpecialRegisterOrFunctionIdentifierContext context)
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

        private static Identifier CreateFunctionReference(CodeElementsParser.FunctionIdentifierContext context)
        {
            if (context == null || context.FUNCTION() == null) return null;
            var symbol = new FunctionName(ParseTreeUtils.GetFirstToken(context.intrinsicFunctionName()));
            var parameters = CreateFunctionParameters(context.argument());
            return new FunctionReference(symbol, parameters);
        }

        private static IList<FunctionParameter> CreateFunctionParameters(IReadOnlyList<CodeElementsParser.ArgumentContext> context)
        {
            if (context == null) return null;
            IList<FunctionParameter> parameters = new List<FunctionParameter>();
            foreach (var argument in context) parameters.Add(CreateFunctionParameter(argument));
            return parameters;
        }

        private static FunctionParameter CreateFunctionParameter(CodeElementsParser.ArgumentContext context)
        {
            if (context.literal() != null) return new FunctionParameter(CreateLiteral(context.literal()));
            //if (context.arithmeticExpression() != null) return new FunctionParameter(new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression()));
            if (context.identifier() != null) return new FunctionParameter(CreateIdentifier(context.identifier()));
            return null;
        }

        private static Identifier CreateSpecialRegister(CodeElementsParser.SpecialRegisterContext context)
        {
            if (context == null) return null;
            return new SpecialRegister(new SpecialRegisterName(ParseTreeUtils.GetFirstToken(context)));
        }

        private static DataReference CreateDataNameReference(CodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateQualifiedName(context.qualifiedDataName());
        }

        public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameContext context)
        {
            if (context == null) return null;
            DataName name = null;
            if (context.dataNameBase() != null) name = CreateDataName(context.dataNameBase().dataName());
            List<DataName> datanames = CreateDataNames(context.dataName());
            datanames.Reverse();
            FileName filename = CreateFileName(context.fileName());
            return new QualifiedName(name, datanames, filename);
        }

        public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedConditionNameContext context)
        {
            if (context == null) return null;
            ConditionName name = CreateConditionName(context.conditionName());
            List<DataName> datanames = CreateDataNames(context.dataName());
            datanames.Reverse();
            FileName filename = CreateFileName(context.fileName());
            return new QualifiedName(name, datanames, filename);
        }

        internal static IList<QualifiedName> CreateQualifiedNames(IReadOnlyList<CodeElementsParser.QualifiedDataNameContext> context)
        {
            var names = new List<QualifiedName>();
            foreach (var name in context)
            {
                var x = CreateQualifiedName(name);
                if (x != null) names.Add(x);
            }
            return names;
        }

        internal static AlphabetName CreateAlphabetName(CodeElementsParser.AlphabetNameContext context)
        {
            if (context == null) return null;
            return new AlphabetName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static ClassName CreateClassName(CodeElementsParser.ClassNameDefinitionContext context)
        {
            if (context == null) return null;
            return new ClassName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static ClassName CreateClassName(CodeElementsParser.ClassNameReferenceContext context)
        {
            if (context == null) return null;
            return new ClassName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameContext> context)
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

        public static DataName CreateDataName(CodeElementsParser.DataNameContext context)
        {
            if (context == null) return null;
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        // only used for data description entry
        public static ConditionName CreateConditionName(CodeElementsParser.DataNameContext context)
        {
            if (context == null) return null;
            return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static ConditionName CreateConditionName(CodeElementsParser.ConditionNameContext context)
        {
            if (context == null) return null;
            return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static IList<FileName> CreateFileNames(IReadOnlyList<CodeElementsParser.FileNameContext> context)
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

        public static FileName CreateFileName(CodeElementsParser.FileNameContext context)
        {
            if (context == null) return null;
            return new FileName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static LinageCounter CreateLinageCounter(CodeElementsParser.FileNameContext context) {
            if (context == null) return null;
            return new LinageCounter(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        private static LinageCounter CreateLinageCounter(CodeElementsParser.LinageCounterSpecialRegisterDeclContext context) {
            if (context == null || context.fileName() == null) return null;
            var filename = CreateFileName(context.fileName());
            return new LinageCounter(filename.NameToken);
        }

        public static IndexName CreateIndexName(CodeElementsParser.IndexNameContext context)
        {
            if (context == null) return null;
            return new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        internal static IList<QualifiedProcedureName> CreateProcedureNames(IReadOnlyList<CodeElementsParser.ProcedureNameContext> context)
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

        internal static QualifiedProcedureName CreateProcedureName(CodeElementsParser.ProcedureNameContext context)
        {
            if (context.paragraphNameOrSectionNameReference() != null)
            {
                Token symbolToken = ParseTreeUtils.GetFirstToken(context.paragraphNameOrSectionNameReference());
                // TO DO : use here the symbol table to check if the name is a paragraph or a section
                // In the meantime, because sections are obsolete, we just assume it is always a paragraph name
                ParagraphName paragraphName = new ParagraphName(symbolToken);
                return new QualifiedProcedureName(paragraphName, null);
            }
            else //if(context.qualifiedParagraphNameReference() != null)
            {
                Token paragraphToken = ParseTreeUtils.GetFirstToken(context.qualifiedParagraphNameReference().paragraphNameReference());
                Token sectionToken = ParseTreeUtils.GetFirstToken(context.qualifiedParagraphNameReference().sectionNameReference());
                ParagraphName paragraphName = new ParagraphName(paragraphToken);
                SectionName sectionName = new SectionName(sectionToken);
                return new QualifiedProcedureName(paragraphName, sectionName);
            }
        }

        public static ParagraphName CreateParagraphName(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            if (context == null) return null;
            return new ParagraphName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static SectionName CreateSectionName(CodeElementsParser.SectionNameDefinitionContext context)
        {
            if (context == null) return null;
            return new SectionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static XmlSchemaName CreateXmlSchemaName(CodeElementsParser.XmlSchemaNameContext context)
        {
            if (context == null) return null;
            return new XmlSchemaName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSubscripts(context.subscript());
        }

        public static IList<Subscript> CreateSubscripts(IReadOnlyList<CodeElementsParser.SubscriptContext> context)
        {
            if (context == null) return null;
            var subscripts = new List<Subscript>();
            foreach (var subscript in context) subscripts.Add(CreateSubscript(subscript));
            return subscripts;
        }


        public static Subscript CreateSubscript(CodeElementsParser.SubscriptContext context)
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
            //if (context.indexName() != null)
            //{
            //    subscript.indexname = CreateIndexName(context.indexName());
            //    InitializeSubscriptOperatorAndLiteral(subscript, context.withRelativeSubscripting());
            //}
            return subscript;
        }

        private static void InitializeSubscriptOperatorAndLiteral(Subscript subscript, CodeElementsParser.WithRelativeSubscriptingContext context)
        {
            if (context == null) return;
            if (context.PlusOperator() != null) subscript.op = '+';
            if (context.MinusOperator() != null) subscript.op = '-';
            if (context.IntegerLiteral() != null) subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
        }

        private static Substring CreateReferenceModifier(CodeElementsParser.ReferenceModifierContext context)
        {
            if (context == null) return null;
            var builder = new ArithmeticExpressionBuilder();
            ArithmeticExpression left = null, right = null;
            //if (context.leftMostCharacterPosition() != null)
            //    left = builder.CreateArithmeticExpression(context.leftMostCharacterPosition().arithmeticExpression());
            //if (context.length() != null)
            //    right = builder.CreateArithmeticExpression(context.length().arithmeticExpression());
            if (left == null && right == null) return null;
            return new Substring(left, right);
        }



        private static Address CreateAddressOf(CodeElementsParser.AddressOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataNameReference() == null) return null;
            return new Address(CreateDataNameReference(context.dataNameReference()));
        }

        private static Length CreateLengthOf(CodeElementsParser.LengthOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataNameReference() == null) return null;
            return new Length(CreateDataNameReference(context.dataNameReference()));
        }




        internal static MnemonicForEnvironmentName CreateMnemonic(CodeElementsParser.MnemonicForEnvironmentNameContext context)
        {
            if (context == null) return null;
            return new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        internal static MnemonicOrEnvironmentName CreateMnemonic(CodeElementsParser.MnemonicOrEnvironmentNameContext context)
        {
            if (context == null) return null;
            return new MnemonicOrEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        public static Index CreateIndex(CodeElementsParser.IndexNameContext indexName)
        {
            if (indexName == null) return null;
            return new Index(new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(indexName.UserDefinedWord())));
        }

        public static Expression CreateAddressOfIdentifier(ITerminalNode address, CodeElementsParser.IdentifierContext identifier)
        {
            throw new NotImplementedException();
        }



        public static Expression CreateIdentifierOrLiteral(CodeElementsParser.IdentifierOrLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return SyntaxElementBuilder.CreateLiteral(context.literal());
            return null;
        }


        internal static Expression CreateEncoding(CodeElementsParser.CodepageContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
                return CreateIdentifier(context.identifier());
            if (context.IntegerLiteral() != null)
                return new Literal(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral())));
            return null;
        }

        internal static IList<FigurativeConstant> CreateFigurativeConstants(IReadOnlyList<CodeElementsParser.FigurativeConstantContext> context)
        {
            IList<FigurativeConstant> constants = new List<FigurativeConstant>();
            foreach (var c in context)
            {
                var constant = CreateFigurativeConstant(c);
                if (constant != null) constants.Add(constant);
            }
            return constants;
        }

        internal static FigurativeConstant CreateFigurativeConstant(CodeElementsParser.FigurativeConstantContext context)
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

		internal static string CreateString(CodeElementsParser.AlphanumOrHexadecimalLiteralContext context) {
			var str = GetString(context);
			if (str == null) return null;
			if (str[0] == 'X' || str[0] == 'x') str = str.Substring(1);
			return str.Substring(1,str.Length-2); // remove '' or ""
		}
		private static string GetString(CodeElementsParser.AlphanumOrHexadecimalLiteralContext context) {
			if (context.AlphanumericLiteral() != null) return context.AlphanumericLiteral().Symbol.Text;
			if (context.HexadecimalAlphanumericLiteral() != null) return context.HexadecimalAlphanumericLiteral().Symbol.Text;
			return null;
		}
	}

}
