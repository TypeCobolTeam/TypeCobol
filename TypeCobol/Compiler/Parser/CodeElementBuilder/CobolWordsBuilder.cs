using System;
using System.Linq;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using System.Collections.Generic;
using Antlr4.Runtime;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.Parser
{
    internal class CobolWordsBuilder
    {

        internal IDictionary<Token, SymbolInformation> symbolInformationForTokens { get; private set; }

        public CobolWordsBuilder(IDictionary<Token, SymbolInformation> symbolInformationForTokens)
        {
            this.symbolInformationForTokens = symbolInformationForTokens;
        }

        private void AddToSymbolInformations(AlphanumericValue nameLiteral, SymbolInformation symbolInfo)
        {
            if (nameLiteral != null && nameLiteral.Token != null && symbolInfo != null)
                symbolInformationForTokens[nameLiteral.Token] = symbolInfo;
        }

        // --- Compile-time constant values used in the Cobol grammar ---

        internal static BooleanValue CreateBooleanValue(IParseTree context)
        {
            if (context == null) return null;
            return new BooleanValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal static IntegerValue CreateIntegerValue(CodeElementsParser.IntegerValueContext context)
        {
            return new IntegerValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal static IntegerValue CreateIntegerValue(CodeElementsParser.IntegerValue2Context context)
        {
            return new IntegerValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal static NumericValue CreateNumericValue(CodeElementsParser.NumericValueContext context)
        {
            return new NumericValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue1Context context)
        {
            return new CharacterValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue2Context context)
        {
            if (context.figurativeConstant() != null && context.figurativeConstant().symbolicCharacterReference() != null)
                return new CharacterValue(CreateSymbolReference(context.figurativeConstant().symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter));
            return new CharacterValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal static CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue3Context context)
        {
            return new CharacterValue(ParseTreeUtils.GetFirstToken(context));
        }

        internal CharacterValue CreateCharacterValue(CodeElementsParser.CharacterValue4Context context)
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

        [CanBeNull]
        internal AlphanumericValue CreateAlphanumericValue([CanBeNull] CodeElementsParser.AlphanumericValue3Context context)
        {
            if (context == null) return null;
            var c = context.figurativeConstant();
            if (c != null && c.symbolicCharacterReference() != null)
            {
                return new AlphanumericValue(CreateSymbolReference(c.symbolicCharacterReference().symbolReference10(), SymbolType.SymbolicCharacter));
            }
            Token token = ParseTreeUtils.GetFirstToken(context);
            return new AlphanumericValue(token);
        }
        internal AlphanumericValue CreateAlphanumericValue(ParserRuleContext context)
        {
            Token token = ParseTreeUtils.GetFirstToken(context);
            if (token == null) return null;
            return new AlphanumericValue(token);
        }
        internal AlphanumericValue CreateAlphanumericValue(ITerminalNode node)
        {
            var token = ParseTreeUtils.GetFirstToken(node);
            if (token == null) return null;
            // [COBOL 2002]
            if (token.TokenType == TokenType.DATE) token.TokenType = TokenType.UserDefinedWord;
            // [/COBOL 2002]
            return new AlphanumericValue(token);
        }

        internal EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue1Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        internal EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue2Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        internal EnumeratedValue CreateEnumeratedValue(CodeElementsParser.EnumeratedValue3Context context, Type enumType)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new EnumeratedValue(valueToken, enumType);
        }

        [CanBeNull]
        internal RepeatedCharacterValue CreateRepeatedCharacterValue([CanBeNull]CodeElementsParser.RepeatedCharacterValue1Context context)
        {
            if (context == null) return null;
            try
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
            catch (InvalidOperationException) { return null; }
        }

        internal RepeatedCharacterValue CreateRepeatedCharacterValue(CodeElementsParser.RepeatedCharacterValue2Context context)
        {
            Token optionalALLToken = null;
            if (context.allFigurativeConstant() != null)
            {
                optionalALLToken = ParseTreeUtils.GetFirstToken(context);
            }

            CodeElementsParser.FigurativeConstantContext figurativeConstantContext = context.figurativeConstant();
            if (context.allFigurativeConstant() != null && context.allFigurativeConstant().figurativeConstant() != null)
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
                if (valueNode == null)
                {
                    valueNode = context.allFigurativeConstant().notNullTerminatedAlphanumericOrNationalLiteralToken();
                }
                Token valueToken = ParseTreeUtils.GetFirstToken(valueNode);
                return new RepeatedCharacterValue(optionalALLToken, valueToken);
            }
        }

        [CanBeNull]
        internal RepeatedCharacterValue CreateRepeatedCharacterValue([CanBeNull]CodeElementsParser.RepeatedCharacterValue3Context context)
        {
            if (context == null) return null;
            try
            {
                Token token = ParseTreeUtils.GetFirstToken(context);
                return new RepeatedCharacterValue(null, token);
            }
            catch (InvalidOperationException) { return null; }
        }

        internal NullPointerValue CreateNullPointerValue(CodeElementsParser.NullPointerValueContext context)
        {
            Token valueToken = ParseTreeUtils.GetFirstToken(context);
            return new NullPointerValue(valueToken);
        }

        internal Value CreateValue(CodeElementsParser.Value1Context context)
        {
            if (context.numericValue() != null)
                return new Value(CreateNumericValue(context.numericValue()));
            if (context.alphanumericValue2() != null)
                return new Value(CreateAlphanumericValue(context.alphanumericValue2()));
            if (context.repeatedCharacterValue2() != null)
                return new Value(CreateRepeatedCharacterValue(context.repeatedCharacterValue2()));
            throw new InvalidOperationException();
        }

        internal Value CreateValue(CodeElementsParser.Value2Context context)
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
                throw new InvalidOperationException("" + context);
            }
        }


        // --- Cobol symbol definitions and symbol references ---

        internal SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        internal SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition2Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue2());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        [CanBeNull]
        internal SymbolDefinition CreateSymbolDefinition([CanBeNull] CodeElementsParser.SymbolDefinition4Context context, SymbolType symbolType)
        {
            if (context == null) return null;
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        internal SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        internal SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition11Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue11());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        internal SymbolDefinition CreateSymbolDefinition(CodeElementsParser.SymbolDefinition12Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue12());
            var symbolDefinition = new SymbolDefinition(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinition);
            return symbolDefinition;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference2Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue2());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference4Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference9Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue9());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference10Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue10());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference11Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue11());
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal SymbolReference CreateSymbolReference(CodeElementsParser.SymbolReference12Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = null;
            if (context.alphanumericValue4() != null)
            {
                nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            }
            else if (context.DATE() != null)
            {
                nameLiteral = new AlphanumericValue(ParseTreeUtils.GetFirstToken(context.DATE()));
            }
            var symbolReference = new SymbolReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolReference);
            return symbolReference;
        }

        internal AmbiguousSymbolReference CreateAmbiguousSymbolReference(CodeElementsParser.AmbiguousSymbolReference1Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            var ambiguousSymbolReference = new AmbiguousSymbolReference(nameLiteral, candidateTypes);
            AddToSymbolInformations(nameLiteral, ambiguousSymbolReference);
            return ambiguousSymbolReference;
        }

        internal AmbiguousSymbolReference CreateAmbiguousSymbolReference(CodeElementsParser.AmbiguousSymbolReference4Context context, SymbolType[] candidateTypes)
        {
            var nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            var ambiguousSymbolReference = new AmbiguousSymbolReference(nameLiteral, candidateTypes);
            AddToSymbolInformations(nameLiteral, ambiguousSymbolReference);
            return ambiguousSymbolReference;
        }

        internal SymbolDefinitionOrReference CreateSymbolDefinitionOrReference(CodeElementsParser.SymbolDefinitionOrReference1Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue1());
            var symbolDefinitionOrReference = new SymbolDefinitionOrReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinitionOrReference);
            return symbolDefinitionOrReference;
        }

        internal SymbolDefinitionOrReference CreateSymbolDefinitionOrReference(CodeElementsParser.SymbolDefinitionOrReference4Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            var symbolDefinitionOrReference = new SymbolDefinitionOrReference(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, symbolDefinitionOrReference);
            return symbolDefinitionOrReference;
        }

        internal ExternalName CreateExternalName(CodeElementsParser.ExternalName1Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue1(), enumType);
            var externalName = new ExternalName(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, externalName);
            return externalName;
        }

        internal ExternalName CreateExternalName(CodeElementsParser.ExternalName2Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue2(), enumType);
            var externalName = new ExternalName(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, externalName);
            return externalName;
        }

        internal ExternalName CreateExternalName(CodeElementsParser.ExternalName3Context context, SymbolType symbolType, Type enumType)
        {
            AlphanumericValue nameLiteral = CreateEnumeratedValue(context.enumeratedValue3(), enumType);
            var externalName = new ExternalName(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, externalName);
            return externalName;
        }

        internal ExternalName CreateExternalName(CodeElementsParser.ExternalName5Context context, SymbolType symbolType)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            var externalName = new ExternalName(nameLiteral, symbolType);
            AddToSymbolInformations(nameLiteral, externalName);
            return externalName;
        }

        internal ExternalNameOrSymbolReference CreateExternalNameOrSymbolReference(CodeElementsParser.ExternalNameOrSymbolReference4Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue4());
            var externalNameOrSymbolReference = new ExternalNameOrSymbolReference(nameLiteral, candidateTypes);
            AddToSymbolInformations(nameLiteral, externalNameOrSymbolReference);
            return externalNameOrSymbolReference;
        }

        internal ExternalNameOrSymbolReference CreateExternalNameOrSymbolReference(CodeElementsParser.ExternalNameOrSymbolReference5Context context, SymbolType[] candidateTypes)
        {
            AlphanumericValue nameLiteral = CreateAlphanumericValue(context.alphanumericValue5());
            var externalNameOrSymbolReference = new ExternalNameOrSymbolReference(nameLiteral, candidateTypes);
            AddToSymbolInformations(nameLiteral, externalNameOrSymbolReference);
            return externalNameOrSymbolReference;
        }


        // --- Specific symbol types ---

        internal SymbolDefinition CreateProgramNameDefinition(CodeElementsParser.ProgramNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition5(), SymbolType.ProgramName);
        }

        internal SymbolReference CreateProgramNameReference(CodeElementsParser.ProgramNameReference1Context context)
        {
            return CreateSymbolReference(context.symbolReference1(), SymbolType.ProgramName);
        }

        internal SymbolReference CreateProgramNameReference(CodeElementsParser.ProgramNameReference2Context context)
        {
            if (context == null) return null;
            return CreateSymbolReference(context.symbolReference5(), SymbolType.ProgramName);
        }

        internal SymbolDefinition CreateProgramEntryDefinition(CodeElementsParser.ProgramEntryDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition1(), SymbolType.ProgramEntry);
        }

        internal AmbiguousSymbolReference CreateProgramNameReferenceOrProgramEntryReference(CodeElementsParser.ProgramNameReferenceOrProgramEntryReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference1(), new SymbolType[] { SymbolType.ProgramName, SymbolType.ProgramEntry });
        }

        internal SymbolDefinition CreateSectionNameDefinition(CodeElementsParser.SectionNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition12(), SymbolType.SectionName);
        }

        internal SymbolReference CreateSectionNameReference(CodeElementsParser.SectionNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.SectionName);
        }

        internal SymbolDefinition CreateParagraphNameDefinition(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition12(), SymbolType.ParagraphName);
        }

        internal SymbolReference CreateParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.ParagraphName);
        }

        internal AmbiguousSymbolReference CreateParagraphNameReferenceOrSectionNameReference(CodeElementsParser.ParagraphNameReferenceOrSectionNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.ParagraphName, SymbolType.SectionName });
        }

        internal SymbolDefinition CreateClassNameDefinition(CodeElementsParser.ClassNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ClassName);
        }

        internal SymbolReference CreateClassNameReference(CodeElementsParser.ClassNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.ClassName);
        }

        internal SymbolDefinitionOrReference CreateClassNameDefOrRef(CodeElementsParser.ClassNameDefOrRefContext context)
        {
            return CreateSymbolDefinitionOrReference(context.symbolDefinitionOrReference4(), SymbolType.ClassName);
        }

        internal SymbolDefinitionOrReference CreateExternalClassNameDefOrRef(CodeElementsParser.ExternalClassNameDefOrRefContext context)
        {
            return CreateSymbolDefinitionOrReference(context.symbolDefinitionOrReference1(), SymbolType.ExternalClassName);
        }

        internal SymbolDefinition CreateMethodNameDefinition(CodeElementsParser.MethodNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition2(), SymbolType.MethodName);
        }

        internal SymbolReference CreateMethodNameReference(CodeElementsParser.MethodNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference2(), SymbolType.MethodName);
        }

        internal SymbolDefinition CreateFunctionNameDefinition(CodeElementsParser.FunctionNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.MethodName);
        }

        internal SymbolReference CreateFunctionNameReference(CodeElementsParser.FunctionNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.MethodName);
        }

        internal SymbolDefinition CreateMnemonicForEnvironmentNameDefinition(CodeElementsParser.MnemonicForEnvironmentNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.MnemonicForEnvironmentName);
        }

        [CanBeNull]
        internal SymbolReference CreateMnemonicForEnvironmentNameReference([CanBeNull] CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSymbolReference(context.symbolReference4(), SymbolType.MnemonicForEnvironmentName);
        }

        internal ExternalNameOrSymbolReference CreateMnemonicForEnvironmentNameReferenceOrEnvironmentName(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            return CreateExternalNameOrSymbolReference(context.externalNameOrSymbolReference4(), new SymbolType[] { SymbolType.EnvironmentName, SymbolType.MnemonicForEnvironmentName });
        }

        internal SymbolDefinition CreateMnemonicForUPSISwitchNameDefinition(CodeElementsParser.MnemonicForUPSISwitchNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.MnemonicForUPSISwitchName);
        }

        internal SymbolReference CreateMnemonicForUPSISwitchNameReference(CodeElementsParser.MnemonicForUPSISwitchNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.MnemonicForUPSISwitchName);
        }

        internal SymbolDefinition CreateConditionForUPSISwitchNameDefinition(CodeElementsParser.ConditionForUPSISwitchNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ConditionForUPSISwitchName);
        }

        internal SymbolDefinition CreateSymbolicCharacterDefinition(CodeElementsParser.SymbolicCharacterDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition11(), SymbolType.SymbolicCharacter);
        }

        internal SymbolReference CreateSymbolicCharacterReference(CodeElementsParser.SymbolicCharacterReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference10(), SymbolType.SymbolicCharacter);
        }

        internal SymbolDefinition CreateAlphabetNameDefinition(CodeElementsParser.AlphabetNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.AlphabetName);
        }

        internal SymbolReference CreateAlphabetNameReference(CodeElementsParser.AlphabetNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.AlphabetName);
        }

        internal SymbolReference CreateIntrinsicAlphabetNameReference(CodeElementsParser.IntrinsicAlphabetNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference10(), SymbolType.AlphabetName);
        }

        internal SymbolReference CreateAlphabetName(CodeElementsParser.AlphabetNameContext context)
        {
            if (context.alphabetNameReference() != null)
            {
                return CreateAlphabetNameReference(context.alphabetNameReference());
            }
            else
            {
                return CreateIntrinsicAlphabetNameReference(context.intrinsicAlphabetNameReference());
            }
        }

        internal SymbolDefinition CreateCharacterClassNameDefinition(CodeElementsParser.CharacterClassNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.CharacterClassName);
        }

        internal SymbolReference CreateCharacterClassNameReference(CodeElementsParser.CharacterClassNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.CharacterClassName);
        }

        // [COBOL 2002]
        internal SymbolDefinition CreateDataTypeNameDefinition(CodeElementsParser.DataNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.DataName);
        }

        internal SymbolReference CreateQualifiedDataTypeReference(CodeElementsParser.Cobol2002TypeClauseContext context)
        {
            var pgmNameContext = context.programNameReference3(); //Get program name Context
            var typeNameRef = context.typeNameReference();
            if (typeNameRef == null)
                return null;
            var dataNameContext = typeNameRef.UserDefinedWord(); //Get variable/type name Context
            if (dataNameContext == null)
                dataNameContext = context.typeNameReference().DATE();

            Token pgmToken, dataToken = null;
            AlphanumericValue pgmAlph, dataAlpha = null;
            SymbolReference pgmSymbol, dataSymbol, symbolReference = null;

            dataToken = ParseTreeUtils.GetFirstToken(dataNameContext);
            dataAlpha = new AlphanumericValue(dataToken);
            dataSymbol = new SymbolReference(dataAlpha, SymbolType.DataName);

            if (pgmNameContext != null)
            {
                pgmToken = ParseTreeUtils.GetFirstToken(pgmNameContext);
                pgmAlph = new AlphanumericValue(pgmToken);
                pgmSymbol = new SymbolReference(pgmAlph, SymbolType.ProgramName);

                symbolReference = new QualifiedSymbolReference(dataSymbol, pgmSymbol);
            }
            else
                symbolReference = dataSymbol;

            symbolInformationForTokens[dataAlpha.Token] = symbolReference;
            return symbolReference;
        }
        // [/COBOL 2002]

        [CanBeNull]
        internal SymbolDefinition CreateDataNameDefinition([CanBeNull] CodeElementsParser.DataNameDefinitionContext context)
        {
            if (context == null) return null;
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.DataName);
        }

        [CanBeNull]
        internal SymbolReference CreateDataNameReference([CanBeNull] CodeElementsParser.DataNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSymbolReference(context.symbolReference4(), SymbolType.DataName);
        }

        internal SymbolReference CreateInstrinsicDataNameReference(CodeElementsParser.IntrinsicDataNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference9(), SymbolType.DataName);
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.FileName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.IndexName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.FileName, SymbolType.MnemonicForUPSISwitchName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName });
        }


        internal AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrTCFunctionProcedure(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.TCFunctionName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.IndexName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.FileName });
        }

        internal AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.ClassName });
        }

        internal SymbolDefinition CreateConditionNameDefinition(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ConditionName);
        }

        internal AmbiguousSymbolReference CreateConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName });
        }

        internal SymbolDefinition CreateIndexNameDefinition(CodeElementsParser.IndexNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.IndexName);
        }

        internal SymbolReference CreateIndexNameReference(CodeElementsParser.IndexNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.IndexName);
        }

        internal SymbolReference CreateIndexNameReference(CodeElementsParser.QualifiedIndexNameContext context)
        {
            CodeElementsParser.SymbolReference4Context head = null;

            //Detect if it's Cobol Qualified (IN|OF)
            if (context.indexName != null)
                return new SymbolReference(CreateAlphanumericValue(context.indexName), SymbolType.IndexName);
            else //Else it typecobol qualified
            {
                head = context.TcHeadDefiniiton;
                if (context.children.Any(x => x.Payload is Token && ((Token)x.Payload).TokenType != TokenType.UserDefinedWord && ((Token)x.Payload).TokenType != TokenType.QualifiedNameSeparator))
                    return null; //If not UserDefiedWord or QualifiedSeprator it's a mistake. 
            }
            var tail = context.symbolReference4();
            tail = tail.Where(t => t != head).ToArray();
                Array.Reverse(tail);

            return CreateQualifiedIndexName(head, tail, false);
        }

        private SymbolReference CreateQualifiedIndexName(CodeElementsParser.SymbolReference4Context head, CodeElementsParser.SymbolReference4Context[] tail, bool isCOBOL = true)
        {
            if (head == null)
                return null; //If head is null -> retrun null, we can't create the QualifiedReference properly.
            var reference = CreateQualifiedSymbolReference(new SymbolReference(CreateAlphanumericValue(head), SymbolType.IndexName), new SymbolReference(CreateAlphanumericValue(tail[0]), SymbolType.IndexName), isCOBOL);
            for (int c = 1; c < tail.Length; c++) reference = CreateQualifiedSymbolReference(reference, new SymbolReference(CreateAlphanumericValue(tail[c]), SymbolType.IndexName), isCOBOL);
            symbolInformationForTokens[reference.NameLiteral.Token] = reference;
            return reference;
        }

        internal SymbolDefinition CreateFileNameDefinition(CodeElementsParser.FileNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.FileName);
        }

        [CanBeNull]
        internal SymbolReference CreateFileNameReference([CanBeNull] CodeElementsParser.FileNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSymbolReference(context.symbolReference4(), SymbolType.FileName);
        }

        internal SymbolDefinition CreateXmlSchemaNameDefinition(CodeElementsParser.XmlSchemaNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.XmlSchemaName);
        }

        [CanBeNull]
        internal SymbolReference CreateXmlSchemaNameReference([CanBeNull] CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            if (context == null) return null;
            return CreateSymbolReference(context.symbolReference4(), SymbolType.XmlSchemaName);
        }


        // --- Qualified names : give explicit context to resolve ambiguous name references ---

        [CanBeNull]
        internal SymbolReference CreateProcedureName([CanBeNull] CodeElementsParser.ProcedureNameContext context)
        {
            if (context == null) return null;
            if (context.paragraphNameReferenceOrSectionNameReference() != null)
                return CreateParagraphNameReferenceOrSectionNameReference(context.paragraphNameReferenceOrSectionNameReference());
            else return CreateQualifiedParagraphNameReference(context.qualifiedParagraphNameReference());
        }

        internal SymbolReference CreateQualifiedParagraphNameReference(CodeElementsParser.QualifiedParagraphNameReferenceContext context)
        {
            var c = context.cobolQualifiedParagraphNameReference();
            if (c != null) return CreateQualifiedParagraphNameReference(c.paragraphNameReference(), c.sectionNameReference());
            var tc = context.tcQualifiedParagraphNameReference();
            return CreateQualifiedParagraphNameReference(tc.paragraphNameReference(), tc.sectionNameReference(), false);
        }
        private SymbolReference CreateQualifiedParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext head, CodeElementsParser.SectionNameReferenceContext tail, bool isCOBOL = true)
        {
            var reference = CreateQualifiedSymbolReference(CreateParagraphNameReference(head), CreateSectionNameReference(tail), isCOBOL);
            symbolInformationForTokens[reference.NameLiteral.Token] = reference;
            return reference;
        }

        [CanBeNull]
        internal SymbolReference CreateQualifiedDataName([CanBeNull] CodeElementsParser.QualifiedDataNameContext context)
        {
            if (context == null) return null;
            if (context.dataNameReference() != null) return CreateDataNameReference(context.dataNameReference());
            return CreateQualifiedDataName(context.qualifiedDataName1());
        }
        internal SymbolReference CreateQualifiedDataNameOrIndexName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context)
        {
            if (context.dataNameReferenceOrIndexNameReference() != null) return CreateDataNameReferenceOrIndexNameReference(context.dataNameReferenceOrIndexNameReference());
            return CreateQualifiedDataName(context.qualifiedDataName1());
        }
        private SymbolReference CreateQualifiedDataName(CodeElementsParser.QualifiedDataName1Context context)
        {
            if (context == null) return null;
            var c = context.cobolQualifiedDataName1();
            if (c != null) return CreateQualifiedDataName(c.dataNameReference(), c.dataNameReferenceOrFileNameReference());
            var tc = context.tcQualifiedDataName1();
            return CreateQualifiedDataName(tc.dataNameReference(), tc.dataNameReferenceOrFileNameReference(), false);
        }
        private SymbolReference CreateQualifiedDataName(CodeElementsParser.DataNameReferenceContext head, CodeElementsParser.DataNameReferenceOrFileNameReferenceContext[] tail, bool isCOBOL = true)
        {
            SymbolReference qname = CreateDataNameReference(head);
            if (tail != null && tail.Length > 0)
            {
                SymbolReference current = CreateDataNameReferenceOrFileNameReference(tail[tail.Length - 1]);
                SymbolReference last = null;
                for (int i = tail.Length - 2; i >= 0; i--)
                {
                    last = current;
                    current = CreateDataNameReferenceOrFileNameReference(tail[i]);
                    current = CreateQualifiedSymbolReference(current, last, isCOBOL);
                }
                qname = CreateQualifiedSymbolReference(qname, current, isCOBOL);
            }
            symbolInformationForTokens[qname.NameLiteral.Token] = qname;
            return qname;
        }
        private SymbolReference CreateQualifiedSymbolReference(SymbolReference head, SymbolReference tail, bool isCOBOL = true)
        {
            if (isCOBOL) return new QualifiedSymbolReference(head, tail);
            else return new TypeCobolQualifiedSymbolReference(head, tail);
        }

        internal SymbolReference CreateRecordName(CodeElementsParser.RecordNameContext context)
        {
            // Could add here a specific property to mark the data name as a record name
            return CreateQualifiedDataName(context.qualifiedDataName());
        }

        internal SymbolReference CreateQualifiedConditionName(CodeElementsParser.QualifiedConditionNameContext context)
        {
            var c = context.cobolQualifiedConditionName();
            if (c != null) return CreateQualifiedConditionName(c.conditionNameReferenceOrConditionForUPSISwitchNameReference(), c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
            var tc = context.tcQualifiedConditionName();
            var tail = tc.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference();
            Array.Reverse(tail);
            return CreateQualifiedConditionName(tc.conditionNameReferenceOrConditionForUPSISwitchNameReference(), tail, false);
        }

        private SymbolReference CreateQualifiedConditionName(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext head, CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext[] tail, bool isCOBOL = true)
        {
            SymbolReference qname = CreateConditionNameReferenceOrConditionForUPSISwitchNameReference(head);
            if (tail != null)
            {
                foreach (var context in tail)
                {
                    var part = CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(context);
                    qname = CreateQualifiedSymbolReference(qname, part, isCOBOL);
                }
            }
            symbolInformationForTokens[qname.NameLiteral.Token] = qname;
            return qname;
        }

        internal SymbolReference CreateQualifiedDataNameOrQualifiedConditionName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference() != null)
            {
                return CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrTCFunctionProcedure(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference() != null)
            {
                return CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrTCFunctionProcedure(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionNameOrTCFunctionProcedure(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }


        private SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrTCFunctionProcedure(CodeElementsParser.QualifiedDataNameOrQualifiedConditionName1Context context)
        {
            var c = context.cobolQualifiedDataNameOrQualifiedConditionName1();
            if (c != null) return CreateQualifiedDataNameOrQualifiedConditionNameTCFunctionProcedure(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(), c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
            var tc = context.tcQualifiedDataNameOrQualifiedConditionName1();
            var tail = tc.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference();
            Array.Reverse(tail);
            return CreateQualifiedDataNameOrQualifiedConditionNameTCFunctionProcedure(tc.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(), tail, false);
        }

        private SymbolReference CreateQualifiedDataNameOrQualifiedConditionName1(CodeElementsParser.QualifiedDataNameOrQualifiedConditionName1Context context)
        {
            var c = context.cobolQualifiedDataNameOrQualifiedConditionName1();
            if (c != null) return CreateQualifiedDataNameOrQualifiedConditionName1(c.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(), c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
            var tc = context.tcQualifiedDataNameOrQualifiedConditionName1();

            if (tc.children.Any(x => x.Payload is Token && ((Token)x.Payload).TokenType != TokenType.UserDefinedWord && ((Token)x.Payload).TokenType != TokenType.QualifiedNameSeparator))
                return null; //If not UserDefiedWord or QualifiedSeprator it's a mistake. 

            var tail = tc.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference();
            Array.Reverse(tail);
            return CreateQualifiedDataNameOrQualifiedConditionName1(tc.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(), tail, false);
        }
        private SymbolReference CreateQualifiedDataNameOrQualifiedConditionName1(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext head, CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext[] tail, bool isCOBOL = true)
        {
            if (head == null)
                return null; //If head is null -> retrun null, we can't create the QualifiedReference properly.
            var reference = CreateQualifiedSymbolReference(CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(head), CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(tail[0]), isCOBOL);
            for (int c = 1; c < tail.Length; c++) reference = CreateQualifiedSymbolReference(reference, CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(tail[c]), isCOBOL);
            symbolInformationForTokens[reference.NameLiteral.Token] = reference;
            return reference;
        }
        private SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameTCFunctionProcedure(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext head, CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext[] tail, bool isCOBOL = true)
        {
            var reference = CreateQualifiedSymbolReference(CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrTCFunctionProcedure(head), CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(tail[0]), isCOBOL);
            for (int c = 1; c < tail.Length; c++) reference = CreateQualifiedSymbolReference(reference, CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(tail[c]), isCOBOL);
            symbolInformationForTokens[reference.NameLiteral.Token] = reference;
            return reference;
        }

        internal SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrIndexName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrIndexNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference() != null)
            {
                return CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrFileName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrFileNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference() != null)
            {
                return CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrClassName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrClassNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference() != null)
            {
                return CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }


        // --- Specific external names ---

        /// <summary>
        /// System devices or standard system actions taken by the compiler.
        /// </summary>
        public enum EnvironmentName
        {
            // System logical input unit : ACCEPT
            SYSIN, SYSIPT,
            // System logical output unit : DISPLAY
            SYSOUT, SYSLIST, SYSLST,
            // System punch device : DISPLAY
            SYSPUNCH, SYSPCH,
            // Console : ACCEPT and DISPLAY
            CONSOLE,
            // Skip to channel 1 through channel 12, respectively : WRITE ADVANCING
            C01, C02, C03, C04, C05, C06, C07, C08, C09, C10, C11, C12,
            // Suppress spacing : WRITE ADVANCING
            CSP,
            // Pocket select 1 through 5 on punch devices : WRITE ADVANCING
            S01, S02, S03, S04, S05,
            // Advanced Function Printing : WRITE ADVANCING
            AFP_5A
        }

        internal ExternalName CreateEnvironmentName(CodeElementsParser.EnvironmentNameContext context)
        {
            return CreateExternalName(context.externalName1(), SymbolType.EnvironmentName, typeof(EnvironmentName));
        }

        /// <summary>
        /// A 1-byte user-programmable status indicator (UPSI) switch.
        /// </summary>
        public enum UPSISwitchNameEnum
        {
            UPSI_0, UPSI_1, UPSI_2, UPSI_3, UPSI_4, UPSI_5, UPSI_6, UPSI_7
        }

        internal ExternalName CreateUPSISwitchName(CodeElementsParser.UpsiSwitchNameContext context)
        {
            return CreateExternalName(context.externalName1(), SymbolType.UPSISwitchName, typeof(UPSISwitchNameEnum));
        }

        internal ExternalName CreateTextName(CodeElementsParser.TextNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.TextName);
        }

        internal ExternalName CreateLibraryName(CodeElementsParser.LibraryNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.LibraryName);
        }

        internal ExternalName CreateQualifiedTextName(CodeElementsParser.QualifiedTextNameContext context)
        {
            ExternalName textName = CreateTextName(context.textName());
            if (context.libraryName() == null)
            {
                return textName;
            }
            else
            {
                ExternalName libraryName = CreateLibraryName(context.libraryName());
                var qualifiedTextName = new QualifiedTextName(textName, libraryName);
                symbolInformationForTokens[qualifiedTextName.NameLiteral.Token] = qualifiedTextName;
                return qualifiedTextName;
            }
        }

        internal ExternalName CreateAssignmentName(CodeElementsParser.AssignmentNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.AssignmentName);
        }

        internal ExternalNameOrSymbolReference CreateAssignmentNameOrFileNameReference(CodeElementsParser.AssignmentNameOrFileNameReferenceContext context)
        {
            return CreateExternalNameOrSymbolReference(context.externalNameOrSymbolReference5(), new SymbolType[] { SymbolType.AssignmentName, SymbolType.FileName });
        }

        /// <summary>
        /// Intrinsic function names defined by the Cobol language.
        /// </summary>
        public enum FunctionNameEnum
        {
            ACOS, ANNUITY, ASIN, ATAN,
            CHAR, COS, CURRENT_DATE,
            DATE_OF_INTEGER, DATE_TO_YYYYMMDD, DAY_OF_INTEGER, DAY_TO_YYYYDDD,
            DISPLAY_OF, FACTORIAL,
            INTEGER, INTEGER_OF_DATE, INTEGER_OF_DAY, INTEGER_PART,
            LENGTH, LOG, LOG10, LOWER_CASE,
            MAX, MEAN, MEDIAN, MIDRANGE, MIN, MOD,
            NATIONAL_OF, NUMVAL, NUMVAL_C,
            ORD, ORD_MAX, ORD_MIN,
            PRESENT_VALUE,
            RANDOM, RANGE, REM, REVERSE,
            SIN, SQRT, STANDARD_DEVIATION, SUM,
            TAN,
            ULENGTH, UPOS, UPPER_CASE, USUBSTR, USUPPLEMENTARY, UVALID, UWIDTH,
            VARIANCE,
            WHEN_COMPILED,
            YEAR_TO_YYYY
        }

        internal ExternalName CreateIntrinsicFunctionName(CodeElementsParser.IntrinsicFunctionNameContext context)
        {
            return CreateExternalName(context.externalName2(), SymbolType.IntrinsicFunctionName, typeof(FunctionNameEnum));
        }

        /// <summary>
        /// Names of specialized Cobol preprocessors or coprocessors
        /// </summary>
        public enum ExecTranslatorNameEnum
        {
            // DB2 coprocessor
            SQL,
            // IMS SQL coprocessor
            SQLIMS,
            // Integrated CICS translator
            CICS,
            // Integrated CICS translator
            DLI
        }

        internal ExternalName CreateExecTranslatorName(CodeElementsParser.ExecTranslatorNameContext context)
        {
            return CreateExternalName(context.externalName3(), SymbolType.ExecTranslatorName, typeof(ExecTranslatorNameEnum));
        }


        // --- Compiler enumerations ---

        /// <summary>
        /// With the *CONTROL (or *CBL) statement, you can selectively display or suppress
        /// the listing of source code, object code, and storage maps throughout the source text. 
        /// </summary>
        public enum ControlCblOption
        {
            // 
            // These are not reserved words, but the only possible values are the following
            SOURCE, NOSOURCE,
            LIST, NOLIST,
            MAP, NOMAP
        }

        internal EnumeratedValue CreateControlCblOption(CodeElementsParser.ControlCblOptionContext context)
        {
            return CreateEnumeratedValue(context.enumeratedValue1(), typeof(ControlCblOption));
        }

        /// <summary>
        /// The RECORDING MODE clause specifies the format of the physical records in a QSAM file. 
        /// The clause is ignored for a VSAM file.
        /// </summary>
        public enum RecordingModeEnum
        {
            // Recording mode F (fixed)
            F,
            // Recording mode V (variable)
            V,
            // Recording mode U (fixed or variable)
            U,
            // Recording mode S (spanned)
            S
        }

        internal EnumeratedValue CreateRecordingMode(CodeElementsParser.RecordingModeContext context)
        {
            return CreateEnumeratedValue(context.enumeratedValue1(), typeof(RecordingModeEnum));
        }
    }
}
