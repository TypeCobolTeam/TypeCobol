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
            Identifier identifier = CreateDataReferenceOrConditionReference(context.dataReferenceOrConditionReference());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
			var array = identifier as ReferenceModified;
            if (array != null) array.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrIndexNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrIndexName(context.dataReferenceOrConditionReferenceOrIndexName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
			var array = identifier as ReferenceModified;
            if (array != null) array.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrFileNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrFileName(context.dataReferenceOrConditionReferenceOrFileName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
			var array = identifier as ReferenceModified;
            if (array != null) array.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrClassNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrClassName(context.dataReferenceOrConditionReferenceOrClassName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
			var array = identifier as ReferenceModified;
            if (array != null) array.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

		public static Token GetSymbolTokenIfIdentifierIsOneUserDefinedWord(CodeElementsParser.IdentifierOrClassNameContext identifier) {
			if (identifier.referenceModifier() == null) {
				var dataReferenceOrConditionReferenceOrClassName = identifier.dataReferenceOrConditionReferenceOrClassName();
				if (dataReferenceOrConditionReferenceOrClassName != null) {
					if (dataReferenceOrConditionReferenceOrClassName.subscript() == null) {
						var qualifiedName = dataReferenceOrConditionReferenceOrClassName.qualifiedDataNameOrQualifiedConditionNameOrClassName();
						var legacy = qualifiedName.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
						if (legacy != null) {
							if (legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference() == null) 
								return ParseTreeUtils.GetFirstToken(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
						} else {
							if (qualifiedName.qDataOrFileOrUPSI() == null)
								return ParseTreeUtils.GetFirstToken(qualifiedName.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
						}
					}
				}
			}
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

        private static DataReference CreateDataReference(CodeElementsParser.DataReferenceContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static DataReference CreateDataReferenceOrConditionReference(CodeElementsParser.DataReferenceOrConditionReferenceContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrIndexName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrFileName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;

        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrClassName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = CreateSubscripts(context);
            if (name != null || subscripts != null) return new DataReference(name, subscripts);
            return null;
        }

		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceContext context) {
			if (context == null) return null;
			return CreateQualifiedName(context.qualifiedDataName());
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context) {
			if (context == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = context.legacyQualifiedDataNameOrIndexName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReferenceOrIndexNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
			} else {
				name = CreateDataName(context.dataNameReferenceOrIndexNameReference());
				qualifiers = CreateDataNames(context.qDataOrFile());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceContext context) {
			if (context == null) return null;
			var c = context.qualifiedDataNameOrQualifiedConditionName();
			if (c == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = c.legacyQualifiedDataNameOrConditionName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				name = CreateDataName(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
				qualifiers = CreateDataNames(c.qDataOrFileOrUPSI());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context) {
			if (context == null) return null;
			var c = context.qualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (c == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = c.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				name = CreateDataName(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
				qualifiers = CreateDataNames(c.qDataOrFileOrUPSI());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context) {
			if (context == null) return null;
			var c = context.qualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (c == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = c.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				name = CreateDataName(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
				qualifiers = CreateDataNames(c.qDataOrFileOrUPSI());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context) {
			if (context == null) return null;
			var c = context.qualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (c == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = c.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				name = CreateDataName(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
				qualifiers = CreateDataNames(c.qDataOrFileOrUPSI());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}

		public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameContext context) {
			if (context == null) return null;
			DataName name;
			List<DataName> qualifiers;
			var legacy = context.legacyQualifiedDataName();
			if (legacy != null) {
				name = CreateDataName(legacy.dataNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
			} else {
				name = CreateDataName(context.dataNameReference());
				qualifiers = CreateDataNames(context.qDataOrFile());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedConditionNameContext context) {
			if (context == null) return null;
			ConditionName name;
			List<DataName> qualifiers;
			var legacy = context.legacyQualifiedConditionName();
			if (legacy != null) {
				name = CreateConditionName(legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference());
				qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				name = CreateConditionName(context.conditionNameReferenceOrConditionForUPSISwitchNameReference());
				qualifiers = CreateDataNames(context.qDataOrFileOrUPSI());
			}
			return CreateQualifiedName(name, qualifiers, legacy != null, legacy == null);
		}
		private static QualifiedName CreateQualifiedName(Symbol name, List<DataName> qualifiers, bool reverse, bool isExplicit) {
			if (reverse) qualifiers.Reverse();
			// TODO: need to lookup symbol table to distinguish data name and file name
			FileName filename = null; // may be first element of qualifiers
			return new QualifiedName(name, qualifiers, filename, isExplicit);
		}

		internal static IList<QualifiedName> CreateQualifiedNames(IReadOnlyList<CodeElementsParser.QualifiedDataNameContext> context) {
			var names = new List<QualifiedName>();
			foreach (var name in context) {
				var x = CreateQualifiedName(name);
				if (x != null) names.Add(x);
			}
			return names;
		}

        internal static AlphabetName CreateAlphabetName(CodeElementsParser.AlphabetNameReferenceContext context)
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

		private static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.QDataOrFileContext> context) {
			var list = new List<CodeElementsParser.DataNameReferenceOrFileNameReferenceContext>();
			foreach(var c in context)
				if (c.dataNameReferenceOrFileNameReference() != null)
					list.Add(c.dataNameReferenceOrFileNameReference());
			//TODO: subscripting
			return CreateDataNames(list);
		}

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceContext> context)
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
                
        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceOrFileNameReferenceContext> context)
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

		private static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.QDataOrFileOrUPSIContext> context) {
			var list = new List<CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext>();
			foreach(var c in context)
				if (c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference() != null)
					list.Add(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			//TODO: subscripting
			return CreateDataNames(list);
		}

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext> context)
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

        public static DataName CreateDataName(CodeElementsParser.DataNameDefinitionContext context)
        {
            if (context == null) return null;
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static DataName CreateDataName(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        // TO DO : create a ConditionName object here ...
        public static DataName CreateDataName(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            if (context == null) return null;
            return new DataName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static ConditionName CreateConditionName(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            if (context == null) return null;
            // TO DO : lookup symbol table to determine the type of the symbol
            return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }
		internal static ConditionName CreateConditionName(CodeElementsParser.ConditionNameDefinitionContext context) {
			if (context == null) return null;
			// TO DO : lookup symbol table to determine the type of the symbol
			return new ConditionName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
		}

        internal static IList<FileName> CreateFileNames(IReadOnlyList<CodeElementsParser.FileNameReferenceContext> context)
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

        public static FileName CreateFileName(CodeElementsParser.FileNameReferenceContext context)
        {
            if (context == null) return null;
            return new FileName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static LinageCounter CreateLinageCounter(CodeElementsParser.FileNameReferenceContext context) {
            if (context == null) return null;
            return new LinageCounter(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        private static LinageCounter CreateLinageCounter(CodeElementsParser.LinageCounterSpecialRegisterDeclContext context) {
            if (context == null || context.fileNameReference() == null) return null;
            var filename = CreateFileName(context.fileNameReference());
            return new LinageCounter(filename.NameToken);
        }

        public static IndexName CreateIndexName(CodeElementsParser.IndexNameDefinitionContext context)
        {
            if (context == null) return null;
            return new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

        public static IndexName CreateIndexName(CodeElementsParser.IndexNameReferenceContext context)
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

        public static XmlSchemaName CreateXmlSchemaName(CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            if (context == null) return null;
            return new XmlSchemaName(ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord()));
        }

		private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceContext context) {
			if (context == null) return null;
			//TODO: subscripting
			return CreateSubscripts(context.subscript());
		}

		private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceContext context) {
			if (context == null) return null;
			var subscripts = CreateSubscripts(context.subscript());
			//TODO: subscripting (probably not more correct than other CreateSubscripts() methods, but allows SEARCH test to pass)
			if (context.qualifiedDataNameOrQualifiedConditionName() != null) {
				var s = CreateSubscript(context.qualifiedDataNameOrQualifiedConditionName().subscript());
				if (s != null) subscripts.Add(s);
			}
			return subscripts;
		}

		private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context) {
			if (context == null) return null;
			//TODO: subscripting
			return CreateSubscripts(context.subscript());
		}

		private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context) {
			if (context == null) return null;
			//TODO: subscripting
			return CreateSubscripts(context.subscript());
		}

		private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context) {
			if (context == null) return null;
			//TODO: subscripting
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
            if (context.qualifiedDataNameOrIndexName() != null)
            {
                subscript.dataname = CreateQualifiedName(context.qualifiedDataNameOrIndexName());
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
            if (context == null || context.dataReference() == null) return null;
            return new Address(CreateDataReference(context.dataReference()));
        }

        private static Length CreateLengthOf(CodeElementsParser.LengthOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataReference() == null) return null;
            return new Length(CreateDataReference(context.dataReference()));
        }




        internal static MnemonicForEnvironmentName CreateMnemonic(CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            if (context == null) return null;
            return new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        internal static MnemonicOrEnvironmentName CreateMnemonic(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            if (context == null) return null;
            return new MnemonicOrEnvironmentName(ParseTreeUtils.GetFirstToken(context));
        }

        public static Index CreateIndex(CodeElementsParser.IndexNameReferenceContext indexName)
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
            if (context.symbolicCharacterReference() != null) return CreateFigurativeConstant(context.symbolicCharacterReference().SymbolicCharacter());
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
