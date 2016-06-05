using System;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
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


        // --- Specific symbol types ---

        internal static SymbolDefinition CreateProgramNameDefinition(CodeElementsParser.ProgramNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition5(), SymbolType.ProgramName);
        }

        internal static SymbolReference CreateProgramNameReference(CodeElementsParser.ProgramNameReference1Context context)
        {
            return CreateSymbolReference(context.symbolReference1(), SymbolType.ProgramName);
        }

        internal static SymbolReference CreateProgramNameReference(CodeElementsParser.ProgramNameReference2Context context)
        {
            return CreateSymbolReference(context.symbolReference5(), SymbolType.ProgramName);
        }

        internal static SymbolDefinition CreateProgramEntryDefinition(CodeElementsParser.ProgramEntryDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition1(), SymbolType.ProgramEntry);
        }

        internal static AmbiguousSymbolReference CreateProgramNameReferenceOrProgramEntryReference(CodeElementsParser.ProgramNameReferenceOrProgramEntryReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference1(), new SymbolType[] { SymbolType.ProgramName, SymbolType.ProgramEntry });
        }

        internal static SymbolDefinition CreateSectionNameDefinition(CodeElementsParser.SectionNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.SectionName);
        }

        internal static SymbolReference CreateSectionNameReference(CodeElementsParser.SectionNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.SectionName);
        }

        internal static SymbolDefinition CreateParagraphNameDefinition(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ParagraphName);
        }

        internal static SymbolReference CreateParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.ParagraphName);
        }

        internal static AmbiguousSymbolReference CreateParagraphNameReferenceOrSectionNameReference(CodeElementsParser.ParagraphNameReferenceOrSectionNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.ParagraphName, SymbolType.SectionName });
        }

        internal static SymbolDefinition CreateClassNameDefinition(CodeElementsParser.ClassNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ClassName);
        }

        internal static SymbolReference CreateClassNameReference(CodeElementsParser.ClassNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.ClassName);
        }

        internal static SymbolDefinitionOrReference CreateClassNameDefOrRef(CodeElementsParser.ClassNameDefOrRefContext context)
        {
            return CreateSymbolDefinitionOrReference(context.symbolDefinitionOrReference4(), SymbolType.ClassName);
        }

        internal static SymbolDefinitionOrReference CreateExternalClassNameDefOrRef(CodeElementsParser.ExternalClassNameDefOrRefContext context)
        {
            return CreateSymbolDefinitionOrReference(context.symbolDefinitionOrReference1(), SymbolType.ExternalClassName);
        }

        internal static SymbolDefinition CreateMethodNameDefinition(CodeElementsParser.MethodNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition2(), SymbolType.MethodName);
        }

        internal static SymbolReference CreateMethodNameReference(CodeElementsParser.MethodNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference2(), SymbolType.MethodName);
        }

        internal static SymbolDefinition CreateMnemonicForEnvironmentNameDefinition(CodeElementsParser.MnemonicForEnvironmentNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.MnemonicForEnvironmentName);
        }

        internal static SymbolReference CreateMnemonicForEnvironmentNameReference(CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.MnemonicForEnvironmentName);
        }

        internal static ExternalNameOrSymbolReference CreateMnemonicForEnvironmentNameReferenceOrEnvironmentName(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            return CreateExternalNameOrSymbolReference(context.externalNameOrSymbolReference4(), new SymbolType[] { SymbolType.EnvironmentName, SymbolType.MnemonicForEnvironmentName });
        }

        internal static SymbolDefinition CreateMnemonicForUPSISwitchNameDefinition(CodeElementsParser.MnemonicForUPSISwitchNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.MnemonicForUPSISwitchName);
        }

        internal static SymbolReference CreateMnemonicForUPSISwitchNameReference(CodeElementsParser.MnemonicForUPSISwitchNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.MnemonicForUPSISwitchName);
        }

        internal static SymbolDefinition CreateConditionForUPSISwitchNameDefinition(CodeElementsParser.ConditionForUPSISwitchNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ConditionForUPSISwitchName);
        }

        internal static SymbolDefinition CreateSymbolicCharacterDefinition(CodeElementsParser.SymbolicCharacterDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition11(), SymbolType.SymbolicCharacter);
        }

        internal static SymbolReference CreateSymbolicCharacterReference(CodeElementsParser.SymbolicCharacterReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference10(), SymbolType.SymbolicCharacter);
        }

        internal static SymbolDefinition CreateAlphabetNameDefinition(CodeElementsParser.AlphabetNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.AlphabetName);
        }

        internal static SymbolReference CreateAlphabetNameReference(CodeElementsParser.AlphabetNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.AlphabetName);
        }

        internal static SymbolReference CreateIntrinsicAlphabetNameReference(CodeElementsParser.IntrinsicAlphabetNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference10(), SymbolType.AlphabetName);
        }

        internal static SymbolReference CreateAlphabetName(CodeElementsParser.AlphabetNameContext context)
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

        internal static SymbolDefinition CreateCharacterClassNameDefinition(CodeElementsParser.CharacterClassNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.CharacterClassName);
        }

        internal static SymbolReference CreateCharacterClassNameReference(CodeElementsParser.CharacterClassNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.CharacterClassName);
        }

        internal static SymbolDefinition CreateDataNameDefinition(CodeElementsParser.DataNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.DataName);
        }

        internal static SymbolReference CreateDataNameReference(CodeElementsParser.DataNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.DataName);
        }

        internal static SymbolReference CreateInstrinsicDataNameReference(CodeElementsParser.IntrinsicDataNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference9(), SymbolType.DataName);
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.FileName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.IndexName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.FileName, SymbolType.MnemonicForUPSISwitchName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.IndexName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.FileName });
        }

        internal static AmbiguousSymbolReference CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.ClassName });
        }

        internal static SymbolDefinition CreateConditionNameDefinition(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.ConditionName);
        }

        internal static AmbiguousSymbolReference CreateConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            return CreateAmbiguousSymbolReference(context.ambiguousSymbolReference4(), new SymbolType[] { SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName });
        }

        internal static SymbolDefinition CreateIndexNameDefinition(CodeElementsParser.IndexNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.IndexName);
        }

        internal static SymbolReference CreateIndexNameReference(CodeElementsParser.IndexNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.IndexName);
        }

        internal static SymbolDefinition CreateFileNameDefinition(CodeElementsParser.FileNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.FileName);
        }

        internal static SymbolReference CreateFileNameReference(CodeElementsParser.FileNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.FileName);
        }

        internal static SymbolDefinition CreateXmlSchemaNameDefinition(CodeElementsParser.XmlSchemaNameDefinitionContext context)
        {
            return CreateSymbolDefinition(context.symbolDefinition4(), SymbolType.XmlSchemaName);
        }

        internal static SymbolReference CreateXmlSchemaNameReference(CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            return CreateSymbolReference(context.symbolReference4(), SymbolType.XmlSchemaName);
        }


        // --- Specific external names ---

        /// <summary>
        /// System devices or standard system actions taken by the compiler.
        /// </summary>
        public enum EnvironmentNameEnum
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

        internal static ExternalName CreateEnvironmentName(CodeElementsParser.EnvironmentNameContext context)
        {
            return CreateExternalName(context.externalName1(), SymbolType.EnvironmentName, typeof(EnvironmentNameEnum));
        }

        /// <summary>
        /// A 1-byte user-programmable status indicator (UPSI) switch.
        /// </summary>
        public enum UPSISwitchNameEnum
        {
            UPSI_0, UPSI_1, UPSI_2, UPSI_3, UPSI_4, UPSI_5, UPSI_6, UPSI_7
        }

        internal static ExternalName CreateUPSISwitchName(CodeElementsParser.UpsiSwitchNameContext context)
        {
            return CreateExternalName(context.externalName1(), SymbolType.UPSISwitchName, typeof(UPSISwitchNameEnum));
        }

        internal static ExternalName CreateTextName(CodeElementsParser.TextNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.TextName);
        }

        internal static ExternalName CreateLibraryName(CodeElementsParser.LibraryNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.LibraryName);
        }

        internal static ExternalName CreateQualifiedTextName(CodeElementsParser.QualifiedTextNameContext context)
        {
            ExternalName textName = CreateTextName(context.textName());
            if (context.libraryName() == null)
            {
                return textName;
            }
            else
            {
                ExternalName libraryName = CreateLibraryName(context.libraryName());
                return new QualifiedTextName(textName, libraryName);
            }
        }

        internal static ExternalName CreateAssignmentName(CodeElementsParser.AssignmentNameContext context)
        {
            return CreateExternalName(context.externalName5(), SymbolType.AssignmentName);
        }

        internal static ExternalNameOrSymbolReference CreateAssignmentNameOrFileNameReference(CodeElementsParser.AssignmentNameOrFileNameReferenceContext context)
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

        internal static ExternalName CreateIntrinsicFunctionName(CodeElementsParser.IntrinsicFunctionNameContext context)
        {
            return CreateExternalName(context.externalName2(), SymbolType.FunctionName, typeof(FunctionNameEnum));
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

        internal static ExternalName CreateExecTranslatorName(CodeElementsParser.ExecTranslatorNameContext context)
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

        internal static EnumeratedValue CreateControlCblOption(CodeElementsParser.ControlCblOptionContext context)
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
        
        internal static EnumeratedValue CreateRecordingMode(CodeElementsParser.RecordingModeContext context)
        {
            return CreateEnumeratedValue(context.enumeratedValue1(), typeof(RecordingModeEnum));
        }        
	}
}
