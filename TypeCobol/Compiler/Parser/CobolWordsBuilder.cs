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
    internal static class CobolWordsBuilder
    {
        // --- Compile-time constant values used in the Cobol grammar ---

        internal static BooleanValue CreateBooleanValue(CodeElementsParser.BooleanValueContext context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new BooleanValue(valueToken);
        }

        internal static IntegerValue CreateIntegerValue(CodeElementsParser.IntegerValueContext context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new IntegerValue(valueToken);
        }

        internal static NumericValue CreateNumericValue(CodeElementsParser.NumericValueContext context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new NumericValue(valueToken);
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue1Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new CharacterValue(valueToken);
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue2Context context)
        {
            if (context.figurativeConstant() != null && context.figurativeConstant().symbolicCharacterReference() != null)
            {
                SymbolReference symbolicCharacterReference = CreateSymbolReference(context.figurativeConstant().symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter);
                return new CharacterValue(symbolicCharacterReference);
            }
            else
            {
                Token valueToken = ParseTreeUtils.GetFirstToken(context);
                return new CharacterValue(valueToken);
            }
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue3Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new CharacterValue(valueToken);
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue4Context context)
        {
            if (context.figurativeConstant() != null && context.figurativeConstant().symbolicCharacterReference() != null)
            {
                SymbolReference symbolicCharacterReference = CreateSymbolReference(context.figurativeConstant().symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter);
                return new CharacterValue(symbolicCharacterReference);
            }
            else
            {
                Token valueToken = ParseTreeUtils.GetFirstToken(context);
                return new CharacterValue(valueToken);
            }
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue1Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue2Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue3Context context)
        {
            if (context.figurativeConstant() != null && context.figurativeConstant().symbolicCharacterReference() != null)
            {
                SymbolReference symbolicCharacterReference = CreateSymbolReference(context.figurativeConstant().symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter);
                return new AlphanumericValue(symbolicCharacterReference);
            }
            else
            {
                Token valueToken = ParseTreeUtils.GetFirstToken(context);
                return new AlphanumericValue(valueToken);
            }
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue4Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue5Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue6Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue7Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue8Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue9Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue10Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static AlphanumericValue CreateAlphanumericValue(CodeElementsParser.AlphanumericValue11Context context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(valueToken);
        }

        internal static EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue1Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        internal static EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue2Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        internal static EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue3Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        internal static RepeatedCharacterValue CreateRepeatedCharacterValue(CodeElementsParser.RepeatedCharacterValue1Context context)
        {
            if (context.figurativeConstant() != null && context.figurativeConstant().symbolicCharacterReference() != null)
            {
                SymbolReference symbolicCharacterReference = CreateSymbolReference(context.figurativeConstant().symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter);
                return new RepeatedCharacterValue(null, symbolicCharacterReference);
            }
            else
            {
                Token valueToken = ParseTreeUtils.GetFirstToken(context);
                return new RepeatedCharacterValue(null, valueToken);
            }
        }

        internal static RepeatedCharacterValue CreateRepeatedCharacterValue(CodeElementsParser.RepeatedCharacterValue2Context context)
        {
            Token optionalALLToken = null;
            if (context.allFigurativeConstant() != null)
            {
                optionalALLToken = ParseTreeUtils.GetFirstToken(context);
            }

            CodeElementsParser.FigurativeConstantContext figurativeConstantContext = context.figurativeConstant();
            if(context.allFigurativeConstant() != null && context.allFigurativeConstant().figurativeConstant() != null)
            {
                figurativeConstantContext = context.allFigurativeConstant().figurativeConstant();
            }

            if (figurativeConstantContext != null && figurativeConstantContext.symbolicCharacterReference() != null)
            {
                SymbolReference symbolicCharacterReference = CreateSymbolReference(figurativeConstantContext.symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter);
                return new RepeatedCharacterValue(optionalALLToken, symbolicCharacterReference);
            }
            else
            {
                IParseTree valueNode = figurativeConstantContext;
                if(valueNode == null)
                {
                    valueNode = context.allFigurativeConstant().notNullTerminatedAlphanumericOrNationalLiteralToken();
                }
                Token valueToken = ParseTreeUtils.GetFirstToken(valueNode);
                return new RepeatedCharacterValue(optionalALLToken, valueToken);
            }
        }

        internal static NullPointerValue CreateNullPointerValue(CodeElementsParser.NullPointerValueContext context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new NullPointerValue(valueToken);
        }

        internal static Value CreateValue(CodeElementsParser.Value1Context context)
        {            
            if(context.numericValue() != null)
            {
                NumericValue numericValue = CreateNumericValue(context.numericValue());
                return new Value(numericValue);
            }
            else if (context.alphanumericValue2() != null)
            {
                AlphanumericValue alphanumericValue = CreateAlphanumericValue(context.alphanumericValue2());
                return new Value(alphanumericValue);
            }
            else if (context.repeatedCharacterValue2() != null)
            {
                RepeatedCharacterValue repeatedCharacterValue = CreateRepeatedCharacterValue(context.repeatedCharacterValue2());
                return new Value(repeatedCharacterValue);
            }
            else
            {
                throw new InvalidOperationException();
            }
        }

        internal static Value CreateValue(CodeElementsParser.Value2Context context)
        {
            if (context.numericValue() != null)
            {
                NumericValue numericValue = CreateNumericValue(context.numericValue());
                return new Value(numericValue);
            }
            else if (context.alphanumericValue2() != null)
            {
                AlphanumericValue alphanumericValue = CreateAlphanumericValue(context.alphanumericValue2());
                return new Value(alphanumericValue);
            }
            else if (context.repeatedCharacterValue2() != null)
            {
                RepeatedCharacterValue repeatedCharacterValue = CreateRepeatedCharacterValue(context.repeatedCharacterValue2());
                return new Value(repeatedCharacterValue);
            }
            else if (context.nullPointerValue() != null)
            {
                NullPointerValue nullPointerValue = CreateNullPointerValue(context.nullPointerValue());
                return new Value(nullPointerValue);
            }
            else
            {
                throw new InvalidOperationException();
            }
        }


        // --- Cobol symbol definitions and symbol references ---

        internal static SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            return new SymbolDefinition(nameLiteral, symbolType);
        }

        internal static SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition2Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue2());
            return new SymbolDefinition(nameLiteral, symbolType);
        }

        internal static SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition4Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            return new SymbolDefinition(nameLiteral, symbolType);
        }

        internal static SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            return new SymbolDefinition(nameLiteral, symbolType);
        }

        internal static SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition11Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue11());
            return new SymbolDefinition(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference2Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue2());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference4Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference9Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue9());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference10Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue10());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference11Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue11());
            return new SymbolReference(nameLiteral, symbolType);
        }

        internal static AmbiguousSymbolReference CreateAmbiguousSymbolReference(CodeElementsParser.AmbiguousSymbolReference1Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            return new AmbiguousSymbolReference(nameLiteral, candidateTypes);
        }

        internal static AmbiguousSymbolReference CreateAmbiguousSymbolReference(CodeElementsParser.AmbiguousSymbolReference4Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            return new AmbiguousSymbolReference(nameLiteral, candidateTypes);
        }

        internal static SymbolDefinitionOrReference CreateSymbolDefinitionOrReference(CodeElementsParser.SymbolDefinitionOrReference1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            return new SymbolDefinitionOrReference(nameLiteral, symbolType);
        }

        internal static SymbolDefinitionOrReference CreateSymbolDefinitionOrReference(CodeElementsParser.SymbolDefinitionOrReference4Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            return new SymbolDefinitionOrReference(nameLiteral, symbolType);
        }

        internal static ExternalName CreateExternalName(CodeElementsParser.ExternalName1Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue1(), enumType);
            return new ExternalName(nameLiteral, symbolType);
        }

        internal static ExternalName CreateExternalName(CodeElementsParser.ExternalName2Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue2(), enumType);
            return new ExternalName(nameLiteral, symbolType);
        }

        internal static ExternalName CreateExternalName(CodeElementsParser.ExternalName3Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue3(), enumType);
            return new ExternalName(nameLiteral, symbolType);
        }

        internal static ExternalName CreateExternalName(CodeElementsParser.ExternalName5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            return new ExternalName(nameLiteral, symbolType);
        }

        internal static ExternalNameOrSymbolReference CreateExternalNameOrSymbolReference(CodeElementsParser.ExternalNameOrSymbolReference4Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            return new ExternalNameOrSymbolReference(nameLiteral, candidateTypes);
        }

        internal static ExternalNameOrSymbolReference CreateExternalNameOrSymbolReference(CodeElementsParser.ExternalNameOrSymbolReference5Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            return new ExternalNameOrSymbolReference(nameLiteral, candidateTypes);
        }



        // -- OLD CODE --

        internal static IList<Identifier> CreateIdentifiers(IReadOnlyList<CodeElementsParser.IdentifierContext> context) {
			IList<Identifier> identifiers = new List<Identifier>();
			if (context != null) {
				foreach (var identifier in context) {
				   var i = CreateIdentifier(identifier);
					if (i != null) identifiers.Add(i);
				}
			}
			return identifiers;
		}

		public static Identifier CreateIdentifier(CodeElementsParser.IdentifierContext context) {
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
			var substring = identifier as ReferenceModifiable;
			if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
			return identifier;
		}

		public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrIndexNameContext context) {
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
			var substring = identifier as ReferenceModifiable;
			if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
			return identifier;
		}

		public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrFileNameContext context) {
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
			var substring = identifier as ReferenceModifiable;
			if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
			return identifier;
		}

		public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrClassNameContext context) {
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
			var substring = identifier as ReferenceModifiable;
			if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
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

		private static DataReference CreateDataReference(CodeElementsParser.DataReferenceContext context) {
			if (context == null) return null;
			QualifiedName name = CreateQualifiedName(context);
			IList<Subscript> subscripts = null;
			if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
			return new DataReference(name, subscripts);
		}

		private static DataReference CreateDataReferenceOrConditionReference(CodeElementsParser.DataReferenceOrConditionReferenceContext context) {
			if (context == null) return null;
			QualifiedName name = CreateQualifiedName(context);
			IList<Subscript> subscripts = null;
			if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
			return new DataReference(name, subscripts);
		}

		private static DataReference CreateDataReferenceOrConditionReferenceOrIndexName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context) {
			if (context == null) return null;
			QualifiedName name = CreateQualifiedName(context);
			IList<Subscript> subscripts = null;
			if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
			return new DataReference(name, subscripts);
		}

		private static DataReference CreateDataReferenceOrConditionReferenceOrFileName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context) {
			if (context == null) return null;
			QualifiedName name = CreateQualifiedName(context);
			IList<Subscript> subscripts = null;
			if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
			return new DataReference(name, subscripts);
		}

		private static DataReference CreateDataReferenceOrConditionReferenceOrClassName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context) {
			if (context == null) return null;
			QualifiedName name = CreateQualifiedName(context);
			IList<Subscript> subscripts = null;
			if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
			return new DataReference(name, subscripts);
		}

		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceContext context) {
			if (context == null) return null;
			return CreateQualifiedName(context.qualifiedDataName());
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context) {
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataNameOrIndexName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReferenceOrIndexNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFile()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReferenceOrIndexNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceContext ctxt) {
			if (ctxt == null) return null;
			var context = ctxt.qualifiedDataNameOrQualifiedConditionName();
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataNameOrConditionName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFileOrUPSI()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext ctxt) {
			if (ctxt == null) return null;
			var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFileOrUPSI()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext ctxt) {
			if (ctxt == null) return null;
			var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFileOrUPSI()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext ctxt) {
			if (ctxt == null) return null;
			var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFileOrUPSI()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}

		public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameContext context) {
			if (context == null) return null;
			var legacy = context.legacyQualifiedDataName();
			if (legacy != null) {
				var dataname = CreateDataName(legacy.dataNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFile()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateDataName(context.dataNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedConditionNameContext context) {
			if (context == null) return null;
			var legacy = context.legacyQualifiedConditionName();
			if (legacy != null) {
				var dataname = CreateConditionName(legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference());
				var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
				return CreateQualifiedName(dataname, qualifiers, true, false);
			} else {
				var qname = new SubscriptedQualifiedName();
				Symbol symbol;
				foreach(var c in context.qDataOrFileOrUPSI()) {
					if (c == null) continue; //TODO? else qname.IsExplicit = false;
					symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
					AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
				}
				symbol = CreateConditionName(context.conditionNameReferenceOrConditionForUPSISwitchNameReference());
				AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
				return qname;
			}
		}
		private static void AddToSubscriptedQualifiedName(SubscriptedQualifiedName qname, Symbol symbol, CodeElementsParser.SubscriptContext context) {
			string name = symbol!=null?symbol.Name:null;
			var subscript = CreateSubscript(context);
			var pair = new Tuple<string,Subscript>(name,subscript);//TODO? test name|subscript == null
			if (pair != null) qname.Add(pair.Item1, pair.Item2);
			//TODO? else qname.IsExplicit = false;
		}
		private static QualifiedName CreateQualifiedName(Symbol name, List<DataName> qualifiers, bool reverse, bool isExplicit) {
			if (reverse) qualifiers.Reverse();
			// TODO: need to lookup symbol table to distinguish data name and file name
			FileName filename = null; // may be first element of qualifiers
			return new SyntacticQualifiedName(name, qualifiers, filename, isExplicit);
		}

		internal static IList<QualifiedName> CreateQualifiedNames(IReadOnlyList<CodeElementsParser.QualifiedDataNameContext> context) {
			var names = new List<QualifiedName>();
			foreach (var name in context) {
				var x = CreateQualifiedName(name);
				if (x != null) names.Add(x);
			}
			return names;
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

               

        public static Expression CreateAddressOfIdentifier(ITerminalNode address, CodeElementsParser.IdentifierContext identifier)
        {
            throw new NotImplementedException();
        }



        public static Expression CreateIdentifierOrLiteral(CodeElementsParser.IdentifierOrLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null) return CobolWordsBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return CobolWordsBuilder.CreateLiteral(context.literal());
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
	}

}
