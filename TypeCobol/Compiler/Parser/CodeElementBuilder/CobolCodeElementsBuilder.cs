using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CobolCodeElementsBuilder : CodeElementsBaseListener
    {
        private CodeElement _ce;
        private ParserRuleContext Context;

        /// <summary>CodeElement object resulting of the visit the parse tree</summary>
        public CodeElement CodeElement {
            get { return _ce; }
            private set {
                _ce = value;
                if (value != null) Dispatcher.OnCodeElement(value, Context);
            }
        }
        public CodeElementDispatcher Dispatcher { get; internal set; }

        /// <summary>
        /// Initialization code run before parsing each new CodeElement
        /// </summary>
        public override void EnterCodeElement(CodeElementsParser.CodeElementContext context)
        {
            CodeElement = null;
            Context = null;
            symbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
            CobolWordsBuilder = new CobolWordsBuilder(symbolInformationForTokens);
            CobolExpressionsBuilder = new CobolExpressionsBuilder(CobolWordsBuilder);
            CobolStatementsBuilder = new CobolStatementsBuilder(CobolWordsBuilder, CobolExpressionsBuilder);
        }

        private IDictionary<Token, SymbolInformation> symbolInformationForTokens;
        private CobolWordsBuilder CobolWordsBuilder { get; set; }
        private CobolExpressionsBuilder CobolExpressionsBuilder { get; set; }
        private CobolStatementsBuilder CobolStatementsBuilder { get; set; }

        /// <summary>
        /// Code run after parsing each new CodeElement
        /// </summary>
        public override void ExitCodeElement(CodeElementsParser.CodeElementContext context)
        {
            if(CodeElement != null && symbolInformationForTokens.Keys.Count > 0)
            {
                CodeElement.SymbolInformationForTokens = symbolInformationForTokens;
            }
        }
        
        // Code structure

        // -- Program --

        public override void EnterProgramIdentification(CodeElementsParser.ProgramIdentificationContext context)
        {
            var programIdentification = new ProgramIdentification();

            programIdentification.ProgramName = CobolWordsBuilder.CreateProgramNameDefinition(context.programNameDefinition());
            if (context.COMMON() != null)
            {
                programIdentification.IsCommon = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.COMMON()));
            }
            if (context.INITIAL() != null)
            {
                programIdentification.IsInitial = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.INITIAL()));
            }
            if (context.RECURSIVE() != null)
            {
                programIdentification.IsRecursive = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.RECURSIVE()));
            }
            programIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

            Context = context;
            CodeElement = programIdentification;
        }

        public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context)
        {
            var programEnd = new ProgramEnd();

            programEnd.ProgramName = CobolWordsBuilder.CreateProgramNameReference(context.programNameReference2());

            Context = context;
            CodeElement = programEnd;
        }

        // -- Class --

        public override void EnterClassIdentification(CodeElementsParser.ClassIdentificationContext context)
        {
            var classIdentification = new ClassIdentification();

            classIdentification.ClassName = CobolWordsBuilder.CreateClassNameDefinition(context.classNameDefinition());
            classIdentification.InheritsFrom = CobolWordsBuilder.CreateClassNameReference(context.inheritsFromClassName);
            classIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

            Context = context;
            CodeElement = classIdentification;
        }

        public override void EnterClassEnd(CodeElementsParser.ClassEndContext context)
        {
            var classEnd = new ClassEnd();

            classEnd.ClassName = CobolWordsBuilder.CreateClassNameReference(context.classNameReference());

            Context = context;
            CodeElement = classEnd;
        }

        public override void EnterFactoryIdentification(CodeElementsParser.FactoryIdentificationContext context)
        {
            Context = context;
            CodeElement = new FactoryIdentification();
        }

        public override void EnterFactoryEnd(CodeElementsParser.FactoryEndContext context)
        {
            Context = context;
            CodeElement = new FactoryEnd();
        }

        public override void EnterObjectIdentification(CodeElementsParser.ObjectIdentificationContext context)
        {
            Context = context;
            CodeElement = new ObjectIdentification();
        }

        public override void EnterObjectEnd(CodeElementsParser.ObjectEndContext context)
        {
            Context = context;
            CodeElement = new ObjectEnd();
        }

        public override void EnterMethodIdentification(CodeElementsParser.MethodIdentificationContext context)
        {
            var methodIdentification = new MethodIdentification();

            methodIdentification.MethodName = CobolWordsBuilder.CreateMethodNameDefinition(context.methodNameDefinition());
            methodIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

            Context = context;
            CodeElement = methodIdentification;
        }

        public override void EnterMethodEnd(CodeElementsParser.MethodEndContext context)
        {
            var methodEnd = new MethodEnd();

            methodEnd.MethodName = CobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());

            Context = context;
            CodeElement = methodEnd;
        }

        // --- Authoring properties common to all identification divisions ---

        internal AuthoringProperties CreateAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context)
        {
            var authoringProperties = new AuthoringProperties();

            if (context.authorParagraph().Length > 0)
            {
                var alphanumericValueContexts = context.authorParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
                authoringProperties.Author = CreateAlphanumericValues(alphanumericValueContexts);
            }
            if (context.dateCompiledParagraph().Length > 0)
            {
                var alphanumericValueContexts = context.dateCompiledParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
                authoringProperties.DateCompiled = CreateAlphanumericValues(alphanumericValueContexts);
            }
            if (context.dateWrittenParagraph().Length > 0)
            {
                var alphanumericValueContexts = context.dateWrittenParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
                authoringProperties.DateWritten = CreateAlphanumericValues(alphanumericValueContexts);
            }
            if (context.installationParagraph().Length > 0)
            {
                var alphanumericValueContexts = context.installationParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
                authoringProperties.Installation = CreateAlphanumericValues(alphanumericValueContexts);
            }
            if (context.securityParagraph().Length > 0)
            {
                var alphanumericValueContexts = context.securityParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
                authoringProperties.Security = CreateAlphanumericValues(alphanumericValueContexts);
            }

            return authoringProperties;
        }

        private AlphanumericValue[] CreateAlphanumericValues(CodeElementsParser.AlphanumericValue6Context[] alphanumericValueContexts)
        {
            AlphanumericValue[] alphanumericValues = new AlphanumericValue[alphanumericValueContexts.Length];
            for (int i = 0; i < alphanumericValueContexts.Length; i++)
            {
                alphanumericValues[i] = CobolWordsBuilder.CreateAlphanumericValue(alphanumericValueContexts[i]);
            }
            return alphanumericValues;
        }

        // -- Environment Division --

        public override void EnterEnvironmentDivisionHeader(CodeElementsParser.EnvironmentDivisionHeaderContext context)
        {
            Context = context;
            CodeElement = new EnvironmentDivisionHeader();
        }

        public override void EnterConfigurationSectionHeader(CodeElementsParser.ConfigurationSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new ConfigurationSectionHeader();
        }

        // -- Configuration Section --

        public override void EnterSourceComputerParagraph(CodeElementsParser.SourceComputerParagraphContext context)
        {
            var paragraph = new SourceComputerParagraph();

            if(context.computerName != null)
            {
                paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
            }
            if(context.DEBUGGING() != null)
            {
                paragraph.DebuggingMode = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(context.DEBUGGING()));
            }

            Context = context;
            CodeElement = paragraph;
        }

        public override void EnterObjectComputerParagraph(CodeElementsParser.ObjectComputerParagraphContext context)
        {
            var paragraph = new ObjectComputerParagraph();

            if(context.computerName != null)
            {
                paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
            }
            if(context.memorySizeClause() != null)
            {
                var memorySizeClauseContext = context.memorySizeClause();
                paragraph.MemorySize = CobolWordsBuilder.CreateIntegerValue(memorySizeClauseContext.integerValue());
                if(memorySizeClauseContext.WORDS() != null)
                {
                    paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Words,
                        ParseTreeUtils.GetFirstToken(memorySizeClauseContext.WORDS()));
                }
                else if (memorySizeClauseContext.CHARACTERS() != null)
                {
                    paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Characters,
                        ParseTreeUtils.GetFirstToken(memorySizeClauseContext.CHARACTERS()));
                }
                else if (memorySizeClauseContext.MODULES() != null)
                {
                    paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Modules,
                        ParseTreeUtils.GetFirstToken(memorySizeClauseContext.MODULES()));
                }
            }
            if(context.programCollatingSequenceClause() != null)
            {
                var collatingSeqClauseContext = context.programCollatingSequenceClause();
                paragraph.CollatingSequence = CobolWordsBuilder.CreateAlphabetName(collatingSeqClauseContext.alphabetName());
            }
            if(context.segmentLimitClause() != null)
            {
                var segmentLimitClauseContext = context.segmentLimitClause();
                paragraph.SegmentLimit = CobolWordsBuilder.CreateIntegerValue(
                    segmentLimitClauseContext.priorityNumber().integerValue());
            }

            Context = context;
            CodeElement = paragraph;
        }

        public override void EnterSpecialNamesParagraph(CodeElementsParser.SpecialNamesParagraphContext context)
        {
            var paragraph = new SpecialNamesParagraph();
            
            if(context.upsiSwitchNameClause() != null && context.upsiSwitchNameClause().Length > 0)
            {
                foreach (var upsiSwitchNameContext in context.upsiSwitchNameClause())
                {
                    var upsiSwitchName = CobolWordsBuilder.CreateUPSISwitchName(upsiSwitchNameContext.upsiSwitchName());
                    if (upsiSwitchNameContext.mnemonicForUPSISwitchNameDefinition() != null)
                    {
                        var mnemonicForUPSISwitchName = CobolWordsBuilder.CreateMnemonicForUPSISwitchNameDefinition(
                            upsiSwitchNameContext.mnemonicForUPSISwitchNameDefinition());
                        if(paragraph.MnemonicsForUPSISwitchNames == null)
                        {
                            paragraph.MnemonicsForUPSISwitchNames = new Dictionary<SymbolDefinition, ExternalName>();
                        }
                        paragraph.MnemonicsForUPSISwitchNames.Add(mnemonicForUPSISwitchName, upsiSwitchName);
                    }
                    if(upsiSwitchNameContext.conditionNamesForUPSISwitch() != null)
                    {
                        if (paragraph.ConditionNamesForUPSISwitchStatus == null)
                        {
                            paragraph.ConditionNamesForUPSISwitchStatus = new Dictionary<SymbolDefinition, Tuple<ExternalName, UPSISwitchStatus>>();
                        }
                        if (upsiSwitchNameContext.conditionNamesForUPSISwitch().offConditionNameForUPSISwitch() != null)
                        {
                            var conditionForUPSISwitchName = CobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
                                upsiSwitchNameContext.conditionNamesForUPSISwitch().offConditionNameForUPSISwitch().conditionForUPSISwitchNameDefinition());
                            paragraph.ConditionNamesForUPSISwitchStatus.Add(conditionForUPSISwitchName,
                                new Tuple<ExternalName, UPSISwitchStatus>(upsiSwitchName, UPSISwitchStatus.Off));
                        }
                        if (upsiSwitchNameContext.conditionNamesForUPSISwitch().onConditionNameForUPSISwitch() != null)
                        {
                            var conditionForUPSISwitchName = CobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
                                upsiSwitchNameContext.conditionNamesForUPSISwitch().onConditionNameForUPSISwitch().conditionForUPSISwitchNameDefinition());
                            paragraph.ConditionNamesForUPSISwitchStatus.Add(conditionForUPSISwitchName,
                                new Tuple<ExternalName, UPSISwitchStatus>(upsiSwitchName, UPSISwitchStatus.On));
                        }
                    }
                }
                
            }
            if (context.environmentNameClause() != null && context.environmentNameClause().Length > 0)
            {
                if(paragraph.MnemonicsForEnvironmentNames == null)
                {
                    paragraph.MnemonicsForEnvironmentNames = new Dictionary<SymbolDefinition, ExternalName>();
                }
                foreach(var environmentNameContext in context.environmentNameClause())
                {
                    var environmentName = CobolWordsBuilder.CreateEnvironmentName(
                        environmentNameContext.environmentName());
                    var mnemonicForEnvironmentName = CobolWordsBuilder.CreateMnemonicForEnvironmentNameDefinition(
                        environmentNameContext.mnemonicForEnvironmentNameDefinition());
                    paragraph.MnemonicsForEnvironmentNames.Add(mnemonicForEnvironmentName, environmentName);
                }
            }
            if (context.alphabetClause() != null && context.alphabetClause().Length > 0)
            {
                if(paragraph.AlphabetNames == null)
                {
                    paragraph.AlphabetNames = new Dictionary<SymbolDefinition, CollatingSequence>();
                }
                foreach(var alphabetContext in context.alphabetClause())
                {
                    var alphabetName = CobolWordsBuilder.CreateAlphabetNameDefinition(alphabetContext.alphabetNameDefinition());
                    if(alphabetContext.intrinsicAlphabetNameReference() != null)
                    {
                        var intrinsicCollatingSequence = new InstrinsicCollatingSequence();
                        intrinsicCollatingSequence.IntrinsicAlphabetName = CobolWordsBuilder.CreateIntrinsicAlphabetNameReference(
                            alphabetContext.intrinsicAlphabetNameReference());
                        paragraph.AlphabetNames.Add(alphabetName, intrinsicCollatingSequence);
                    }
                    else if(alphabetContext.userDefinedCollatingSequence() != null && alphabetContext.userDefinedCollatingSequence().Length > 0)
                    {
                        var userDefinedCollatingSequence = new UserDefinedCollatingSequence();
                        userDefinedCollatingSequence.CharacterSets = new CharacterSetInCollatingSequence[alphabetContext.userDefinedCollatingSequence().Length];
                        for (int i = 0; i < alphabetContext.userDefinedCollatingSequence().Length; i++)
                        {
                            var userDefinedCSContext = alphabetContext.userDefinedCollatingSequence()[i];
                            if (userDefinedCSContext.charactersInCollatingSequence() != null)
                            {
                                var charsInCSContext = userDefinedCSContext.charactersInCollatingSequence();
                                var characters = CreateCharactersInCollatingSequence(charsInCSContext);
                                userDefinedCollatingSequence.CharacterSets[i] = characters;
                            }
                            else if (userDefinedCSContext.charactersRange() != null)
                            {
                                var charactersRangeContext = userDefinedCSContext.charactersRange();
                                CharactersRangeInCollatingSequence charactersRange = CreateCharactersRange(charactersRangeContext);
                                userDefinedCollatingSequence.CharacterSets[i] = charactersRange;
                            }
                            else if (userDefinedCSContext.charactersEqualSet() != null)
                            {
                                var charactersEqualSetContext = userDefinedCSContext.charactersEqualSet();
                                var charactersEqualSet = new CharactersEqualSetInCollatingSequence();
                                charactersEqualSet.EqualCharacters = new CharacterInCollatingSequence[charactersEqualSetContext.characterInCollatingSequence().Length];
                                for (int j = 0; j < charactersEqualSetContext.characterInCollatingSequence().Length; j++)
                                {
                                    var characterInCSContext = charactersEqualSetContext.characterInCollatingSequence()[j];
                                    charactersEqualSet.EqualCharacters[j] = CreateCharacterInCollatingSequence(characterInCSContext);
                                }
                                userDefinedCollatingSequence.CharacterSets[i] = charactersEqualSet;
                            }
                        }
                        paragraph.AlphabetNames.Add(alphabetName, userDefinedCollatingSequence);
                    }
                }
            }
            if (context.symbolicCharactersClause() != null && context.symbolicCharactersClause().Length > 0)
            {
                if(paragraph.SymbolicCharacters == null)
                {
                    paragraph.SymbolicCharacters = new Dictionary<SymbolDefinition, Tuple<IntegerValue, SymbolReference>>();
                }
                foreach(var symbolicCharactersContext in context.symbolicCharactersClause())
                {
                    SymbolReference alphabetName = null;
                    if (symbolicCharactersContext.alphabetNameReference() != null)
                    {
                        alphabetName = CobolWordsBuilder.CreateAlphabetNameReference(symbolicCharactersContext.alphabetNameReference());
                    }
                    foreach(var symbolicCharOPContext in symbolicCharactersContext.symbolicCharactersOrdinalPositions())
                    {
                        for (int i = 0; i < Math.Min(symbolicCharOPContext.symbolicCharacterDefinition().Length, symbolicCharOPContext.ordinalPositionInCollatingSequence().Length); i++)
                        {
                            var symbolicCharacter = CobolWordsBuilder.CreateSymbolicCharacterDefinition(symbolicCharOPContext.symbolicCharacterDefinition()[i]);
                            var ordinalPosition = CobolWordsBuilder.CreateIntegerValue(symbolicCharOPContext.ordinalPositionInCollatingSequence()[i].integerValue());
                            paragraph.SymbolicCharacters.Add(symbolicCharacter,
                                new Tuple<IntegerValue, SymbolReference>(ordinalPosition, alphabetName));
                        }
                    }
                }
            }
            if (context.classClause() != null && context.classClause().Length > 0)
            {
                if(paragraph.CharsetClassNames == null)
                {
                    paragraph.CharsetClassNames = new Dictionary<SymbolDefinition, UserDefinedCollatingSequence>();
                }
                foreach(var classContext in context.classClause())
                {
                    var characterClassName = CobolWordsBuilder.CreateCharacterClassNameDefinition(classContext.characterClassNameDefinition());
                    var userDefinedCharacterClass = new UserDefinedCollatingSequence();
                    userDefinedCharacterClass.CharacterSets = new CharacterSetInCollatingSequence[classContext.userDefinedCharacterClass().Length];
                    for (int i = 0; i < classContext.userDefinedCharacterClass().Length; i++)
                    {
                        var userDefinedCCContext = classContext.userDefinedCharacterClass()[i];
                        if (userDefinedCCContext.charactersInCollatingSequence() != null)
                        {
                            userDefinedCharacterClass.CharacterSets[i] = CreateCharactersInCollatingSequence(
                                userDefinedCCContext.charactersInCollatingSequence());
                        }
                        else if (userDefinedCCContext.charactersRange() != null)
                        {
                            userDefinedCharacterClass.CharacterSets[i] = CreateCharactersRange(
                                userDefinedCCContext.charactersRange());
                        }
                    }
                    paragraph.CharsetClassNames.Add(characterClassName, userDefinedCharacterClass);
                }
            }
            if (context.currencySignClause() != null && context.currencySignClause().Length > 0)
            {
                if(paragraph.CurrencySymbols == null)
                {
                    paragraph.CurrencySymbols = new Dictionary<AlphanumericValue, CharacterValue>();
                }
                foreach (var currencySignContext in context.currencySignClause())
                {
                    var currencySign = CobolWordsBuilder.CreateAlphanumericValue(currencySignContext.alphanumericValue1());
                    CharacterValue characterValue = null;
                    if (currencySignContext.characterValue1() != null)
                    {
                        characterValue = CobolWordsBuilder.CreateCharacterValue(currencySignContext.characterValue1());
                    }
                    paragraph.CurrencySymbols.Add(currencySign, characterValue);
                }
            }
            if (context.decimalPointClause() != null && context.decimalPointClause().Length > 0)
            {
                var decimalPointContext = context.decimalPointClause()[0];
                if (decimalPointContext.COMMA() != null)
                {
                    paragraph.DecimalPointIsComma = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(decimalPointContext.COMMA()));
                }
            }
            if (context.xmlSchemaClause() != null && context.xmlSchemaClause().Length > 0)
            {
                if(paragraph.XmlSchemaNames == null)
                {
                    paragraph.XmlSchemaNames = new Dictionary<SymbolDefinition, ExternalName>();
                }
                foreach (var xmlSchemaContext in context.xmlSchemaClause())
                {
                    var xmlSchemName = CobolWordsBuilder.CreateXmlSchemaNameDefinition(xmlSchemaContext.xmlSchemaNameDefinition());
                    var assignmentName = CobolWordsBuilder.CreateAssignmentName(xmlSchemaContext.assignmentName());
                    paragraph.XmlSchemaNames.Add(xmlSchemName, assignmentName);
                }
            }

            Context = context;
            CodeElement = paragraph;
        }

        private CharactersRangeInCollatingSequence CreateCharactersRange(CodeElementsParser.CharactersRangeContext charactersRangeContext)
        {
            var charactersRange = new CharactersRangeInCollatingSequence();
            charactersRange.StartCharacter = CreateCharacterInCollatingSequence(
                charactersRangeContext.startCharacter);
            charactersRange.EndCharacter = CreateCharacterInCollatingSequence(
                charactersRangeContext.endCharacter);
            return charactersRange;
        }

        private CharactersInCollatingSequence CreateCharactersInCollatingSequence(CodeElementsParser.CharactersInCollatingSequenceContext charsInCSContext)
        {
            var charactersInCollatingSequence = new CharactersInCollatingSequence();
            if (charsInCSContext.alphanumericValue1() != null)
            {
                charactersInCollatingSequence.CharactersInAlphanmericValue =
                    CobolWordsBuilder.CreateAlphanumericValue(charsInCSContext.alphanumericValue1());
            }
            else if (charsInCSContext.ordinalPositionInCollatingSequence() != null)
            {
                charactersInCollatingSequence.OrdinalPositionInCollatingSequence =
                    CobolWordsBuilder.CreateIntegerValue(charsInCSContext.ordinalPositionInCollatingSequence().integerValue());
            }
            return charactersInCollatingSequence;
        }

        private CharacterInCollatingSequence CreateCharacterInCollatingSequence(CodeElementsParser.CharacterInCollatingSequenceContext charInCSContext)
        {
            var characterInCollatingSequence = new CharacterInCollatingSequence();
            if (charInCSContext.characterValue2() != null)
            {
                characterInCollatingSequence.CharacterValue =
                    CobolWordsBuilder.CreateCharacterValue(charInCSContext.characterValue2());
            }
            else if (charInCSContext.ordinalPositionInCollatingSequence() != null)
            {
                characterInCollatingSequence.OrdinalPositionInCollatingSequence =
                    CobolWordsBuilder.CreateIntegerValue(charInCSContext.ordinalPositionInCollatingSequence().integerValue());
            }
            return characterInCollatingSequence;
        }

        public override void EnterRepositoryParagraph(CodeElementsParser.RepositoryParagraphContext context)
        {
            var paragraph = new RepositoryParagraph();

            if(context.repositoryClassDeclaration() != null &  context.repositoryClassDeclaration().Length > 0)
            {
                if(paragraph.ClassNames == null)
                {
                    paragraph.ClassNames = new Dictionary<SymbolDefinitionOrReference, SymbolDefinitionOrReference>();
                }
                foreach(var repositoryClassDeclContext in context.repositoryClassDeclaration())
                {
                    var className = CobolWordsBuilder.CreateClassNameDefOrRef(repositoryClassDeclContext.classNameDefOrRef());
                    SymbolDefinitionOrReference externalClassName = null;
                    if(repositoryClassDeclContext.externalClassNameDefOrRef() != null)
                    {
                        externalClassName = CobolWordsBuilder.CreateExternalClassNameDefOrRef(
                            repositoryClassDeclContext.externalClassNameDefOrRef());
                    }
                    paragraph.ClassNames.Add(className, externalClassName);
                }
            }

            Context = context;
            CodeElement = paragraph;
        }

        // -- Input-output Section --

        public override void EnterInputOutputSectionHeader(CodeElementsParser.InputOutputSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new InputOutputSectionHeader();
        }

        public override void EnterFileControlParagraphHeader(CodeElementsParser.FileControlParagraphHeaderContext context)
        {
            Context = context;
            CodeElement = new FileControlParagraphHeader();
        }

        public override void EnterFileControlEntry(CodeElementsParser.FileControlEntryContext context)
        {
            var entry = new FileControlEntry();

            if (context.selectClause() != null)
            {
                entry.FileName = CobolWordsBuilder.CreateFileNameDefinition(context.selectClause().fileNameDefinition());
                if (context.selectClause().OPTIONAL() != null)
                {
                    entry.IsOptional = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(context.selectClause().OPTIONAL()));
                }
            }
            if (context.assignClause() != null && context.assignClause().Length > 0)
            {
                var assignClauseContext = context.assignClause()[0];
                entry.ExternalDataSet = CobolWordsBuilder.CreateAssignmentName(assignClauseContext.assignmentName()[0]);
            }
            if (context.reserveClause() != null && context.reserveClause().Length > 0)
            {
                var reserveClauseContext = context.reserveClause()[0];
                entry.ReserveIOBuffersCount = CobolWordsBuilder.CreateIntegerValue(reserveClauseContext.integerValue());
            }
            if (context.accessModeClause() != null && context.accessModeClause().Length > 0)
            {
                var accessModeClauseContext = context.accessModeClause()[0];
                if (accessModeClauseContext.SEQUENTIAL() != null)
                {
                    entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Sequential,
                        ParseTreeUtils.GetFirstToken(accessModeClauseContext.SEQUENTIAL()));
                }
                else if (accessModeClauseContext.RANDOM() != null)
                {
                    entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Random,
                        ParseTreeUtils.GetFirstToken(accessModeClauseContext.RANDOM()));
                }
                else if (accessModeClauseContext.DYNAMIC() != null)
                {
                    entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Dynamic,
                        ParseTreeUtils.GetFirstToken(accessModeClauseContext.DYNAMIC()));
                }
            }
            if (context.fileStatusClause() != null && context.fileStatusClause().Length > 0)
            {
                var fileStatusClauseContext = context.fileStatusClause()[0];
                entry.FileStatus = CobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.fileStatus);
                if (fileStatusClauseContext.vsamReturnCode != null)
                {
                    entry.VSAMReturnCode = CobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.vsamReturnCode);
                }
            }

            SyntaxProperty<FileRecordsOrganization> recordsOrganization = null;
            CharacterVariable paddingCharacter = null;
            Token recordDelimiter = null;
            SymbolReference recordKey = null;
            AlternateRecordKey[] alternateRecordKeys = null;
            SymbolReference relativeKey = null;
            SymbolReference password = null;
            if (context.organizationClause() != null && context.organizationClause().Length > 0)
            {
                var organizationClauseContext = context.organizationClause()[0];
                if (organizationClauseContext.SEQUENTIAL() != null)
                {
                    recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Sequential,
                        ParseTreeUtils.GetFirstToken(organizationClauseContext.SEQUENTIAL()));
                }
                else if (organizationClauseContext.INDEXED() != null)
                {
                    recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Indexed,
                        ParseTreeUtils.GetFirstToken(organizationClauseContext.INDEXED()));
                }
                else if (organizationClauseContext.RELATIVE() != null)
                {
                    recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Relative,
                        ParseTreeUtils.GetFirstToken(organizationClauseContext.RELATIVE()));
                }
                if (organizationClauseContext.LINE() != null)
                {
                    recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.LineSequential,
                        ParseTreeUtils.GetFirstToken(organizationClauseContext.LINE()));
                }
            }
            if (context.paddingCharacterClause() != null && context.paddingCharacterClause().Length > 0)
            {
                var paddingCharacterClauseContext = context.paddingCharacterClause()[0];
                paddingCharacter = CobolExpressionsBuilder.CreateCharacterVariable(paddingCharacterClauseContext.characterVariable());
            }
            if (context.recordDelimiterClause() != null && context.recordDelimiterClause().Length > 0)
            {
                var recordDelimiterClauseContext = context.recordDelimiterClause()[0];
                if (recordDelimiterClauseContext.STANDARD_1() != null)
                {
                    recordDelimiter = ParseTreeUtils.GetFirstToken(recordDelimiterClauseContext.STANDARD_1());
                }
                else if (recordDelimiterClauseContext.literalOrUserDefinedWordOReservedWordExceptCopy() != null)
                {
                    recordDelimiter = ParseTreeUtils.GetFirstToken(recordDelimiterClauseContext.literalOrUserDefinedWordOReservedWordExceptCopy());
                }
            }
            if (context.recordKeyClause() != null && context.recordKeyClause().Length > 0)
            {
                var recordKeyClauseContext = context.recordKeyClause()[0];
                recordKey = CobolWordsBuilder.CreateDataNameReference(recordKeyClauseContext.dataNameReference());
            }
            if (context.alternateRecordKeyClause() != null && context.alternateRecordKeyClause().Length > 0)
            {
                alternateRecordKeys = new AlternateRecordKey[context.alternateRecordKeyClause().Length];
                for (int i = 0; i < context.alternateRecordKeyClause().Length; i++)
                {
                    var alternateRecordKeyClauseContext = context.alternateRecordKeyClause()[i];
                    alternateRecordKeys[i] = new AlternateRecordKey();
                    alternateRecordKeys[i].RecordKey = CobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.recordKey);
                    if (alternateRecordKeyClauseContext.DUPLICATES() != null)
                    {
                        alternateRecordKeys[i].AllowDuplicates = new SyntaxProperty<bool>(true,
                            ParseTreeUtils.GetFirstToken(alternateRecordKeyClauseContext.DUPLICATES()));
                    }
                    if (alternateRecordKeyClauseContext.password != null)
                    {
                        alternateRecordKeys[i].Password = CobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.password);
                    }
                }
            }
            if (context.relativeKeyClause() != null && context.relativeKeyClause().Length > 0)
            {
                var relativeKeyClauseContext = context.relativeKeyClause()[0];
                relativeKey = CobolWordsBuilder.CreateDataNameReference(relativeKeyClauseContext.dataNameReference());
            }
            if (context.passwordClause() != null && context.passwordClause().Length > 0)
            {
                var passwordClauseContext = context.passwordClause()[0];
                password = CobolWordsBuilder.CreateDataNameReference(passwordClauseContext.dataNameReference());
            }
            if (recordsOrganization != null)
            {
                switch (recordsOrganization.Value)
                {
                    case FileRecordsOrganization.Sequential:
                    case FileRecordsOrganization.LineSequential:
                        var sequentialFileStructure = new SequentialFileStructure();
                        sequentialFileStructure.RecordsOrganization = recordsOrganization;
                        sequentialFileStructure.PaddingCharacter = paddingCharacter;
                        sequentialFileStructure.RecordDelimiter = recordDelimiter;
                        sequentialFileStructure.Password = password;
                        entry.Structure = sequentialFileStructure;
                        break;
                    case FileRecordsOrganization.Indexed:
                        var indexedFileStructure = new IndexedFileStructure();
                        indexedFileStructure.RecordsOrganization = recordsOrganization;
                        indexedFileStructure.RecordKey = recordKey;
                        indexedFileStructure.Password = password;
                        indexedFileStructure.AlternateRecordKeys = alternateRecordKeys;
                        entry.Structure = indexedFileStructure;
                        break;
                    case FileRecordsOrganization.Relative:
                        var relativeFileStructure = new RelativeFileStructure();
                        relativeFileStructure.RecordsOrganization = recordsOrganization;
                        relativeFileStructure.RelativeKey = relativeKey;
                        relativeFileStructure.Password = password;
                        entry.Structure = relativeFileStructure;
                        break;
                }
            }

            Context = context;
            CodeElement = entry;
        }
        
        public override void EnterIoControlParagraphHeader(CodeElementsParser.IoControlParagraphHeaderContext context)
        {
            Context = context;
            CodeElement = new IOControlParagraphHeader();
        }

        public override void EnterIoControlEntry(CodeElementsParser.IoControlEntryContext context)
        {
            IOControlEntry entry = null;
            if (context.rerunClause() != null)
            {
                var rerunClauseContext = context.rerunClause();
                var rerunEntry = new RerunIOControlEntry();
                rerunEntry.OnExternalDataSetOrFileName = CobolWordsBuilder.CreateAssignmentNameOrFileNameReference(
                    rerunClauseContext.assignmentNameOrFileNameReference());
                if (rerunClauseContext.RECORDS() != null)
                {
                    rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryRecordCount,
                        ParseTreeUtils.GetFirstToken(rerunClauseContext.RECORDS()));
                    rerunEntry.EveryRecordCount = CobolWordsBuilder.CreateIntegerValue(rerunClauseContext.integerValue());
                }
                else if (rerunClauseContext.REEL() != null)
                {
                    rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryEndOfReelUnit,
                        ParseTreeUtils.GetFirstToken(rerunClauseContext.REEL()));
                }
                else if (rerunClauseContext.UNIT() != null)
                {
                    rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryEndOfReelUnit,
                        ParseTreeUtils.GetFirstToken(rerunClauseContext.UNIT()));
                }
                if (rerunClauseContext.fileNameReference() != null)
                {
                    rerunEntry.OfFileName = CobolWordsBuilder.CreateFileNameReference(rerunClauseContext.fileNameReference());
                }
                entry = rerunEntry;
            }
            else if (context.sameAreaClause() != null)
            {
                var sameAreaClauseContext = context.sameAreaClause();
                var sameAreaEntry = new SameAreaIOControlEntry();
                if (sameAreaClauseContext.RECORD() != null)
                {
                    sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameRecordArea,
                        ParseTreeUtils.GetFirstToken(sameAreaClauseContext.RECORD()));
                }
                else if (sameAreaClauseContext.SORT_ARG() != null)
                {
                    sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameSortArea,
                        ParseTreeUtils.GetFirstToken(sameAreaClauseContext.SORT_ARG()));
                }
                else if (sameAreaClauseContext.SORT_MERGE() != null)
                {
                    sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameSortMergeArea,
                        ParseTreeUtils.GetFirstToken(sameAreaClauseContext.SORT_MERGE()));
                }
                sameAreaEntry.FileNames = new SymbolReference[sameAreaClauseContext.fileNameReference().Length];
                for (int i = 0; i < sameAreaClauseContext.fileNameReference().Length; i++)
                {
                    var fileNameReferenceContext = sameAreaClauseContext.fileNameReference()[i];
                    sameAreaEntry.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
                }
                entry = sameAreaEntry;
            }
            else if (context.multipleFileTapeClause() != null)
            {
                var multipleFTClauseContext = context.multipleFileTapeClause();
                var multipleFileEntry = new MultipleFileTapeIOControlEntry();
                multipleFileEntry.PhysicalReelOfTape = new PhysicalReelOfTape[multipleFTClauseContext.physicalReelOfTape().Length];
                for (int i = 0; i < multipleFTClauseContext.physicalReelOfTape().Length; i++)
                {
                    var physicalReelOfTapeContext = multipleFTClauseContext.physicalReelOfTape()[i];
                    var physicalReelOfTape = new PhysicalReelOfTape();
                    physicalReelOfTape.FileName = CobolWordsBuilder.CreateFileNameReference(physicalReelOfTapeContext.fileNameReference());
                    if (physicalReelOfTapeContext.integerValue() != null)
                    {
                        physicalReelOfTape.FilePosition = CobolWordsBuilder.CreateIntegerValue(physicalReelOfTapeContext.integerValue());
                    }
                    multipleFileEntry.PhysicalReelOfTape[i] = physicalReelOfTape;
                }
                entry = multipleFileEntry;
            }
            else if (context.applyWriteOnlyClause() != null)
            {
                var applyWOClauseContext = context.applyWriteOnlyClause();
                var applyWOEntry = new ApplyWriteOnlyIOControlEntry();
                applyWOEntry.FileNames = new SymbolReference[applyWOClauseContext.fileNameReference().Length];
                for (int i = 0; i < applyWOClauseContext.fileNameReference().Length; i++)
                {
                    var fileNameReferenceContext = applyWOClauseContext.fileNameReference()[i];
                    applyWOEntry.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
                }
                entry = applyWOEntry;
            }

            Context = context;
            CodeElement = entry;
        }

        // -- Data Division --

        public override void EnterDataDivisionHeader(CodeElementsParser.DataDivisionHeaderContext context)
        {
            Context = context;
            CodeElement = new DataDivisionHeader();
        }

        public override void EnterFileSectionHeader(CodeElementsParser.FileSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new FileSectionHeader();
        }

        public override void EnterWorkingStorageSectionHeader(CodeElementsParser.WorkingStorageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new WorkingStorageSectionHeader();
        }

        public override void EnterLocalStorageSectionHeader(
            CodeElementsParser.LocalStorageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new LocalStorageSectionHeader();
        }

        public override void EnterLinkageSectionHeader(CodeElementsParser.LinkageSectionHeaderContext context)
        {
            Context = context;
            CodeElement = new LinkageSectionHeader();
        }

        public override void EnterFileDescriptionEntry(CodeElementsParser.FileDescriptionEntryContext context)
        {
            var entry = new FileDescriptionEntry();

            if (context.FD() != null)
            {
                entry.Type = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.File,
                    ParseTreeUtils.GetFirstToken(context.FD()));
            }
            else if (context.SD() != null)
            {
                entry.Type = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.SortMergeFile,
                    ParseTreeUtils.GetFirstToken(context.SD()));
            }
            entry.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());

            if (context.externalClause() != null && context.externalClause().Length > 0)
            {
                var externalClauseContext = context.externalClause()[0];
                entry.IsExternal = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(externalClauseContext.EXTERNAL()));
            }
            if (context.globalClause() != null)
            {
                var globalClauseContext = context.globalClause()[0];
                entry.IsGlobal = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(globalClauseContext.GLOBAL()));
            }
            if (context.blockContainsClause() != null && context.blockContainsClause().Length > 0)
            {
                var blockContainsClauseContext = context.blockContainsClause()[0];
                entry.MaxBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.maxNumberOfBytes);
                if (blockContainsClauseContext.minNumberOfBytes != null)
                {
                    entry.MinBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.minNumberOfBytes);
                }
                if (blockContainsClauseContext.CHARACTERS() != null)
                {
                    entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Characters,
                        ParseTreeUtils.GetFirstToken(blockContainsClauseContext.CHARACTERS()));
                }
                else if (blockContainsClauseContext.RECORDS() != null)
                {
                    entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Records,
                        ParseTreeUtils.GetFirstToken(blockContainsClauseContext.RECORDS()));
                }
            }
            if (context.recordClause() != null && context.recordClause().Length > 0)
            {
                var recordClauseContext = context.recordClause()[0];
                if (recordClauseContext.numberOfBytes != null)
                {
                    entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.numberOfBytes);
                    entry.MaxRecordSize = entry.MinRecordSize;
                }
                else if (recordClauseContext.minNumberOfBytes != null)
                {
                    entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.minNumberOfBytes);
                    entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.maxNumberOfBytes);
                }
                else if (recordClauseContext.VARYING() != null)
                {
                    if (recordClauseContext.fromNumberOfBytes != null)
                    {
                        entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.fromNumberOfBytes);
                    }
                    if (recordClauseContext.toNumberOfBytes != null)
                    {
                        entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.toNumberOfBytes);
                    }
                    if (recordClauseContext.dataNameReference() != null)
                    {
                        entry.RecordSizeDependingOn = CobolWordsBuilder.CreateDataNameReference(recordClauseContext.dataNameReference());
                    }
                }
            }
            if (context.labelRecordsClause() != null && context.labelRecordsClause().Length > 0)
            {
                var labelRecordClauseContext = context.labelRecordsClause()[0];
                if (labelRecordClauseContext.STANDARD() != null)
                {
                    entry.LabelRecordType = new SyntaxProperty<LabelRecordType>(LabelRecordType.StandardLabels,
                        ParseTreeUtils.GetFirstToken(labelRecordClauseContext.STANDARD()));
                }
                if (labelRecordClauseContext.OMITTED() != null)
                {
                    entry.LabelRecordType = new SyntaxProperty<LabelRecordType>(LabelRecordType.Omitted,
                        ParseTreeUtils.GetFirstToken(labelRecordClauseContext.OMITTED()));
                }
                else if (labelRecordClauseContext.dataNameReference() != null && labelRecordClauseContext.dataNameReference().Length > 0)
                {
                    entry.LabelRecords = new SymbolReference[labelRecordClauseContext.dataNameReference().Length];
                    for (int i = 0; i < labelRecordClauseContext.dataNameReference().Length; i++)
                    {
                        entry.LabelRecords[i] =
                            CobolWordsBuilder.CreateDataNameReference(labelRecordClauseContext.dataNameReference()[i]);
                    }
                }
            }
            if (context.valueOfClause() != null)
            {
                var valueOfClauseContext = context.valueOfClause()[0];
                entry.ValueOfLabelRecords = new Dictionary<SymbolReference, Variable>();
                for (int i = 0; i < valueOfClauseContext.qualifiedDataName().Length; i++)
                {
                    entry.ValueOfLabelRecords.Add(
                        CobolWordsBuilder.CreateQualifiedDataName(valueOfClauseContext.qualifiedDataName()[i]),
                        CobolExpressionsBuilder.CreateVariable(valueOfClauseContext.variable5()[i]));
                }
            }
            if (context.dataRecordsClause() != null && context.dataRecordsClause().Length > 0)
            {
                var dataRecordClauseContext = context.dataRecordsClause()[0];
                entry.DataRecords = new SymbolReference[dataRecordClauseContext.dataNameReference().Length];
                for (int i = 0; i < dataRecordClauseContext.dataNameReference().Length; i++)
                {
                    entry.DataRecords[i] =
                        CobolWordsBuilder.CreateDataNameReference(dataRecordClauseContext.dataNameReference()[i]);
                }
            }
            if (context.linageClause() != null && context.linageClause().Length > 0)
            {
                var linageClauseContext = context.linageClause()[0];
                if (linageClauseContext.numberOfLinesInPage != null)
                {
                    entry.LogicalPageBodyLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInPage);
                }
                if (linageClauseContext.firstLineNumberOfFootingArea != null)
                {
                    entry.LogicalPageFootingLineNumber = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.firstLineNumberOfFootingArea);
                }
                if (linageClauseContext.numberOfLinesInTopMargin != null)
                {
                    entry.LogicalPageTopMarginLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInTopMargin);
                }
                if (linageClauseContext.numberOfLinesInBottomMargin != null)
                {
                    entry.LogicalPageBottomMarginLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInBottomMargin);
                }
            }
            if (context.recordingModeClause() != null && context.recordingModeClause().Length > 0)
            {
                var recordingModeClauseContext = context.recordingModeClause()[0];
                entry.RecordingMode = CobolWordsBuilder.CreateRecordingMode(recordingModeClauseContext.recordingMode());
            }

            Context = context;
            CodeElement = entry;
        }

        public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context)
        {
            if (context.dataRenamesEntry() != null || context.dataConditionEntry() != null)
            {
                // For levels 66 and 88, the DataDefinitionEntry is created by the following methods
                // - EnterDataRenamesEntry
                // - EnterDataConditionEntry
                return;
            }
            if (context.redefinesClause() != null)
            {
                // Redefines clause is not a separate rule in the grammar for optimization puroposes,
                // but we pretend here that it is a separate rule
                EnterDataRedefinesEntry(context);
                return;
            }

            var entry = new DataDescriptionEntry();

            entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber().integerValue());
            if (context.dataNameDefinition() != null)
            {
                entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
            }
            else if (context.FILLER() != null)
            {
                entry.IsFiller = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(context.FILLER()));
            }
            else
            {
                entry.IsFiller = new SyntaxProperty<bool>(true, null);
            }
            if (context.pictureClause() != null && context.pictureClause().Length > 0)
            {
                var pictureClauseContext = context.pictureClause()[0];
                entry.Picture = CobolWordsBuilder.CreateAlphanumericValue(pictureClauseContext.pictureCharacterString);
            }
            if (context.blankWhenZeroClause() != null && context.blankWhenZeroClause().Length > 0)
            {
                var blankClauseContext = context.blankWhenZeroClause()[0];
                Token zeroToken = null;
                if (blankClauseContext.ZERO() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZERO());
                }
                else if (blankClauseContext.ZEROS() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROS());
                }
                else
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROES());
                }
                entry.IsBlankWhenZero = new SyntaxProperty<bool>(true, zeroToken);
            }
            if (context.externalClause() != null && context.externalClause().Length > 0)
            {
                var externalClauseContext = context.externalClause()[0];
                entry.IsExternal = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(externalClauseContext.EXTERNAL()));

                // Check coherence
                if (entry.IsExternal != null && entry.LevelNumber.Value != 1)
                {
                    DiagnosticUtils.AddError(entry, "External is only allowed for level 01", externalClauseContext);
                }
            }
            if (context.globalClause() != null && context.globalClause().Length > 0)
            {
                var globalClauseContext = context.globalClause()[0];
                entry.IsGlobal = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(globalClauseContext.GLOBAL()));
            }
            if (context.justifiedClause() != null && context.justifiedClause().Length > 0)
            {
                var justifiedClauseContext = context.justifiedClause()[0];
                Token justifiedToken = null;
                if (justifiedClauseContext.JUSTIFIED() != null)
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUSTIFIED());
                }
                else
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUST());
                }
                entry.IsJustified = new SyntaxProperty<bool>(true, justifiedToken);
            }
            if (context.groupUsageClause() != null && context.groupUsageClause().Length > 0)
            {
                var groupUsageClauseContext = context.groupUsageClause()[0];
                entry.IsJustified = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(groupUsageClauseContext.NATIONAL()));
            }
            if (context.occursClause() != null && context.occursClause().Length > 0)
            {
                var occursClauseContext = context.occursClause()[0];
                if (occursClauseContext.minNumberOfOccurences != null)
                {
                    entry.MinOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.minNumberOfOccurences);
                }
                if (occursClauseContext.maxNumberOfOccurences != null)
                {
                    entry.MaxOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.maxNumberOfOccurences);
                }
                if (entry.MinOccurencesCount == null && entry.MaxOccurencesCount != null)
                {
                    entry.MinOccurencesCount = entry.MaxOccurencesCount;
                }
                if (occursClauseContext.UNBOUNDED() != null)
                {
                    entry.HasUnboundedNumberOfOccurences = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(occursClauseContext.UNBOUNDED()));
                }
                if (occursClauseContext.varNumberOfOccurences != null)
                {
                    entry.OccursDependingOn = CobolExpressionsBuilder.CreateNumericVariable(occursClauseContext.varNumberOfOccurences);
                }
                if (occursClauseContext.tableSortingKeys() != null && occursClauseContext.tableSortingKeys().Length > 0)
                {
                    int keysCount = 0;
                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
                    {
                        keysCount += tableSortingKeysContext.dataNameReference().Length;
                    }
                    entry.TableSortingKeys = new TableSortingKey[keysCount];
                    int keyIndex = 0;
                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
                    {
                        SyntaxProperty<SortDirection> sortDirection = null;
                        if (tableSortingKeysContext.ASCENDING() != null)
                        {
                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Ascending,
                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.ASCENDING()));
                        }
                        else
                        {
                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Descending,
                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.DESCENDING()));
                        }
                        foreach (var dataNameReference in tableSortingKeysContext.dataNameReference())
                        {
                            SymbolReference sortKey = CobolWordsBuilder.CreateDataNameReference(dataNameReference);
                            entry.TableSortingKeys[keyIndex] = new TableSortingKey(sortKey, sortDirection);
                            keyIndex++;
                        }
                    }
                }
                if (occursClauseContext.indexNameDefinition() != null && occursClauseContext.indexNameDefinition().Length > 0)
                {
                    entry.Indexes = new SymbolDefinition[occursClauseContext.indexNameDefinition().Length];
                    for (int i = 0; i < occursClauseContext.indexNameDefinition().Length; i++)
                    {
                        var indexNameDefinition = occursClauseContext.indexNameDefinition()[i];
                        entry.Indexes[i] = CobolWordsBuilder.CreateIndexNameDefinition(indexNameDefinition);
                    }
                }
            }
            if (context.signClause() != null && context.signClause().Length > 0)
            {
                var signClauseContext = context.signClause()[0];
                if (signClauseContext.LEADING() != null)
                {
                    entry.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Leading,
                        ParseTreeUtils.GetFirstToken(signClauseContext.LEADING()));
                }
                else
                {
                    entry.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Trailing,
                        ParseTreeUtils.GetFirstToken(signClauseContext.TRAILING()));
                }
                if (signClauseContext.SEPARATE() != null)
                {
                    entry.SignIsSeparate = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(signClauseContext.SEPARATE()));
                }
            }
            if (context.synchronizedClause() != null && context.synchronizedClause().Length > 0)
            {
                var synchronizedClauseContext = context.synchronizedClause()[0];
                if (synchronizedClauseContext.SYNCHRONIZED() != null)
                {
                    entry.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNCHRONIZED()));
                }
                else
                {
                    entry.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNC()));
                }
            }
            if (context.usageClause() != null && context.usageClause().Length > 0)
            {
                var usageClauseContext = context.usageClause()[0];
                if (usageClauseContext.BINARY() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Binary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.BINARY()));
                }
                else if (usageClauseContext.COMP() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Binary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP()));
                }
                else if (usageClauseContext.COMPUTATIONAL() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Binary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL()));
                }
                else if (usageClauseContext.COMP_4() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Binary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP_4()));
                }
                else if (usageClauseContext.COMPUTATIONAL_4() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Binary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL_4()));
                }
                else if (usageClauseContext.COMP_1() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.FloatingPöint,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP_1()));
                }
                else if (usageClauseContext.COMPUTATIONAL_1() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.FloatingPöint,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL_1()));
                }
                else if (usageClauseContext.COMP_2() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.LongFloatingPöint,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP_2()));
                }
                else if (usageClauseContext.COMPUTATIONAL_2() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.LongFloatingPöint,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL_2()));
                }
                else if (usageClauseContext.PACKED_DECIMAL() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.PackedDecimal,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.PACKED_DECIMAL()));
                }
                else if (usageClauseContext.COMP_3() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.PackedDecimal,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP_3()));
                }
                else if (usageClauseContext.COMPUTATIONAL_3() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.PackedDecimal,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL_3()));
                }
                else if (usageClauseContext.COMP_5() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.NativeBinary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMP_5()));
                }
                else if (usageClauseContext.COMPUTATIONAL_5() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.NativeBinary,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.COMPUTATIONAL_5()));
                }
                else if (usageClauseContext.DISPLAY_ARG() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Display,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.DISPLAY_ARG()));
                }
                else if (usageClauseContext.DISPLAY_1() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.DBCS,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.DISPLAY_1()));
                }
                else if (usageClauseContext.INDEX() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Index,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.INDEX()));
                }
                else if (usageClauseContext.NATIONAL() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.National,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.NATIONAL()));
                }
                else if (usageClauseContext.OBJECT() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.ObjectReference,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.OBJECT()));
                }
                else if (usageClauseContext.REFERENCE() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.ObjectReference,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.REFERENCE()));
                }
                else if (usageClauseContext.POINTER() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.Pointer,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.POINTER()));
                }
                else if (usageClauseContext.FUNCTION_POINTER() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.FunctionPointer,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.FUNCTION_POINTER()));
                }
                else if (usageClauseContext.PROCEDURE_POINTER() != null)
                {
                    entry.Usage = new SyntaxProperty<DataUsage>(DataUsage.ProcedurePointer,
                        ParseTreeUtils.GetFirstToken(usageClauseContext.PROCEDURE_POINTER()));
                }
            }
            if (context.valueClause() != null && context.valueClause() != null)
            {
                var valueClauseContext = context.valueClause()[0];
                entry.InitialValue = CobolWordsBuilder.CreateValue(valueClauseContext.value2());
            }

            /* // [Cobol 2002]
			entry.IsTypeDefinition = context.cobol2002TypedefClause() != null;
			if (entry.IsTypeDefinition && entry.Name != null) {
				bool strong = context.cobol2002TypedefClause().STRONG() != null;
				entry.DataType = new DataType(entry.Name.Name, strong);
			}

		    var cobol2002TypeClause = DataDescriptionChecker.GetContext(entry, context.cobol2002TypeClause());
			if (cobol2002TypeClause != null) {
				Token token = null;
				if (cobol2002TypeClause.UserDefinedWord() != null)
					token = ParseTreeUtils.GetTokenFromTerminalNode(cobol2002TypeClause.UserDefinedWord());
				if (cobol2002TypeClause.DATE() != null)
					token = ParseTreeUtils.GetTokenFromTerminalNode(cobol2002TypeClause.DATE());
				if (token != null) entry.Picture = "TYPE:"+token.Text.ToUpper();
			}
            // [/Cobol 2002] */

            Context = context;
            CodeElement = entry;
        }

        private void EnterDataRedefinesEntry(CodeElementsParser.DataDescriptionEntryContext context)
        {
            var entry = new DataRedefinesEntry();

            entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber().integerValue());
            if (context.dataNameDefinition() != null)
            {
                entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
            }
            else if (context.FILLER() != null)
            {
                entry.IsFiller = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(context.FILLER()));
            }
            else
            {
                entry.IsFiller = new SyntaxProperty<bool>(true, null);
            }
            if (context.redefinesClause() != null)
            {
                entry.RedefinesDataName = CobolWordsBuilder.CreateDataNameReference(context.redefinesClause().dataNameReference());
            }

            Context = context;
            CodeElement = entry;
        }

        public override void EnterDataRenamesEntry(CodeElementsParser.DataRenamesEntryContext context)
        {
            var entry = new DataRenamesEntry();

            entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber().integerValue());
            entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
            if (context.renamesClause().dataNameReference() != null)
            {
                entry.RenamesFromDataName = CobolWordsBuilder.CreateDataNameReference(context.renamesClause().dataNameReference());
            }
            else if (context.renamesClause().dataNamesRange() != null)
            {
                entry.RenamesFromDataName = CobolWordsBuilder.CreateDataNameReference(
                    context.renamesClause().dataNamesRange().startDataName);
                entry.RenamesToDataName = CobolWordsBuilder.CreateDataNameReference(
                    context.renamesClause().dataNamesRange().endDataName);
            }

            Context = context;
            CodeElement = entry;
        }

        public override void EnterDataConditionEntry(CodeElementsParser.DataConditionEntryContext context)
        {
            var entry = new DataConditionEntry();

            entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber().integerValue());
            entry.DataName = CobolWordsBuilder.CreateConditionNameDefinition(context.conditionNameDefinition());
            if (context.valueClauseForCondition() != null && context.valueClauseForCondition().value1() != null && context.valueClauseForCondition().value1().Length > 0)
            {
                entry.ConditionValues = new Value[context.valueClauseForCondition().value1().Length];
                for (int i = 0; i < context.valueClauseForCondition().value1().Length; i++)
                {
                    entry.ConditionValues[i] = CobolWordsBuilder.CreateValue(context.valueClauseForCondition().value1()[i]);
                }
            }
            if (context.valueClauseForCondition() != null && context.valueClauseForCondition().valuesRange() != null && context.valueClauseForCondition().valuesRange().Length > 0)
            {
                entry.ConditionValuesRanges = new ValuesRange[context.valueClauseForCondition().valuesRange().Length];
                for (int i = 0; i < context.valueClauseForCondition().valuesRange().Length; i++)
                {
                    var valuesRangeContext = context.valueClauseForCondition().valuesRange()[i];
                    var valuesRange = new ValuesRange(
                        CobolWordsBuilder.CreateValue(valuesRangeContext.startValue),
                        CobolWordsBuilder.CreateValue(valuesRangeContext.endValue));
                    entry.ConditionValuesRanges[i] = valuesRange;
                }
            }

            Context = context;
            CodeElement = entry;
        }

        // -- Procedure Division --

        public override void EnterProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateProcedureDivisionHeader(context);
        }

        public override void EnterDeclarativesHeader(CodeElementsParser.DeclarativesHeaderContext context)
        {
            Context = context;
            CodeElement = new DeclarativesHeader();
        }

        public override void EnterDeclarativesEnd(CodeElementsParser.DeclarativesEndContext context)
        {
            Context = context;
            CodeElement = new DeclarativesEnd();
        }

        public override void EnterUseStatement(CodeElementsParser.UseStatementContext context)
        {
            Context = context;
            if (context.useStatementForExceptionDeclarative() != null)
            {
                CodeElement = CreateUseStatementForExceptionDeclarative(context.useStatementForExceptionDeclarative());
            }
            else if (context.useStatementForDebuggingDeclarative() != null)
            {
                CodeElement = CreateUseStatementForDebuggingDeclarative(context.useStatementForDebuggingDeclarative());
            }
        }

        internal UseAfterIOExceptionStatement CreateUseStatementForExceptionDeclarative(CodeElementsParser.UseStatementForExceptionDeclarativeContext context)
        {
            var statement = new UseAfterIOExceptionStatement();
            if (context.GLOBAL() != null)
            {
                statement.IsGlobal = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(context.GLOBAL()));
            }
            if (context.fileNameReference() != null && context.fileNameReference().Length > 0)
            {
                statement.FileNames = new SymbolReference[context.fileNameReference().Length];
                for (int i = 0; i < context.fileNameReference().Length; i++)
                {
                    statement.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference()[i]);
                }
            }
            if (context.INPUT() != null)
            {
                statement.OpenMode = new SyntaxProperty<OpenMode>(OpenMode.INPUT,
                    ParseTreeUtils.GetFirstToken(context.INPUT()));
            }
            else if (context.OUTPUT() != null)
            {
                statement.OpenMode = new SyntaxProperty<OpenMode>(OpenMode.OUTPUT,
                    ParseTreeUtils.GetFirstToken(context.OUTPUT()));
            }
            else if (context.I_O() != null)
            {
                statement.OpenMode = new SyntaxProperty<OpenMode>(OpenMode.IO,
                    ParseTreeUtils.GetFirstToken(context.I_O()));
            }
            else if (context.EXTEND() != null)
            {
                statement.OpenMode = new SyntaxProperty<OpenMode>(OpenMode.EXTEND,
                    ParseTreeUtils.GetFirstToken(context.EXTEND()));
            }
            return statement;
        }

        internal UseForDebuggingProcedureStatement CreateUseStatementForDebuggingDeclarative(CodeElementsParser.UseStatementForDebuggingDeclarativeContext context)
        {
            var statement = new UseForDebuggingProcedureStatement();
            if (context.procedureName() != null && context.procedureName().Length > 0)
            {
                statement.ProcedureNames = new SymbolReference[context.procedureName().Length];
                for (int i = 0; i < context.procedureName().Length; i++)
                {
                    statement.ProcedureNames[i] = CobolWordsBuilder.CreateProcedureName(context.procedureName()[i]);
                }
            }
            if (context.ALL() != null)
            {
                statement.AllProcedures = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(context.ALL()));
            }
            return statement;
        }

        // -- Section --

        public override void EnterSectionHeader(CodeElementsParser.SectionHeaderContext context)
        {
            var sectionHeader = new SectionHeader();

            sectionHeader.SectionName = CobolWordsBuilder.CreateSectionNameDefinition(context.sectionNameDefinition());
            if (context.priorityNumber() != null)
            {
                sectionHeader.PriorityNumber = CobolWordsBuilder.CreateIntegerValue(context.priorityNumber().integerValue());
            }

            Context = context;
            CodeElement = sectionHeader;
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            paragraphHeader.ParagraphName = CobolWordsBuilder.CreateParagraphNameDefinition(context.paragraphNameDefinition());

            Context = context;
            CodeElement = paragraphHeader;
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CodeElementsParser.SentenceEndContext context)
        {
            Context = context;
            CodeElement = new SentenceEnd();
        }
        
        // -- Statements --

        public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context)
        {
            Context = context;
            if (context.acceptDataTransfer() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateAcceptDataTransferStatement(context.acceptDataTransfer());
            }
            else if(context.acceptSystemDateTime() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateAcceptSystemDateTime(context.acceptSystemDateTime());
            }
        }

        public override void EnterAddStatement(CodeElementsParser.AddStatementContext context)
        {
            Context = context;
            if(context.addSimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateAddStatement(context.addSimple());
            }
            else if (context.addGiving() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateAddGivingStatement(context.addGiving());
            }
            else if (context.addCorresponding() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateAddCorrespondingStatement(context.addCorresponding());
            }
        }

        public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context)
        {
            Context = context;
            CodeElement = new AddStatementEnd();
        }

        public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateAlterStatement(context);
        }

        public override void EnterCallStatement(CodeElementsParser.CallStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateCallStatement(context);
        }

        public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context)
        {
            Context = context;
            CodeElement = new CallStatementEnd();
        }

        public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateCancelStatement(context);
        }

        public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateCloseStatement(context);
        }

        public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateComputeStatement(context);
        }

        public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context)
        {
            Context = context;
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context)
        {
            Context = context;
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateDeleteStatement(context);
        }

        public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context)
        {
            Context = context;
            CodeElement = new DeleteStatementEnd();
        }

        public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateDisplayStatement(context);
        }

        public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context)
        {
            Context = context;
            if (context.divideSimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateDivideStatement(context.divideSimple());
            }
            else if (context.divideGiving() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateDivideGivingStatement(context.divideGiving());
            }
            else if (context.divideRemainder() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateDivideRemainderStatement(context.divideRemainder());
            }
        }

        public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context)
        {
            Context = context;
            CodeElement = new DivideStatementEnd();
        }
        
        public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateEntryStatement(context);
        }

        public override void EnterEvaluateStatement(CodeElementsParser.EvaluateStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateEvaluateStatement(context); ;
        }

        public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context)
        {
            Context = context;
            CodeElement = new EvaluateStatementEnd();
        }

        public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateExecStatement(context);
        }

        public override void EnterExitStatement(CodeElementsParser.ExitStatementContext context)
        {
            Context = context;
            CodeElement = new ExitStatement();
        }

        public override void EnterExitMethodStatement(CodeElementsParser.ExitMethodStatementContext context)
        {
            Context = context;
            CodeElement = new ExitMethodStatement();
        }

        public override void EnterExitProgramStatement(CodeElementsParser.ExitProgramStatementContext context)
        {
            Context = context;
            CodeElement = new ExitProgramStatement();
        }
                
        public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context)
        {
            Context = context;
            CodeElement = new GobackStatement();
        }

        public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context)
        {
            Context = context;
            if (context.gotoSimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateGotoStatement(context.gotoSimple());
            }
            if (context.gotoConditional() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateGotoConditionalStatement(context.gotoConditional());
            }
        }

        public override void EnterIfStatement(CodeElementsParser.IfStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateIfStatement(context);
        }

        public override void EnterElseCondition(CodeElementsParser.ElseConditionContext context)
        {
            Context = context;
            CodeElement = new ElseCondition();
        }

        public override void EnterIfStatementEnd(CodeElementsParser.IfStatementEndContext context)
        {
            Context = context;
            CodeElement = new IfStatementEnd();
        }

        public override void EnterNextSentenceStatement(CodeElementsParser.NextSentenceStatementContext context)
        {
            Context = context;
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterInitializeStatement(CodeElementsParser.InitializeStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateInitializeStatement(context);
        }

        public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateInspectStatement(context);
        }

        public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateInvokeStatement(context);
        }

        public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateMergeStatement(context);
        }

        public override void EnterMoveStatement(CodeElementsParser.MoveStatementContext context)
        {
            Context = context;
            if (context.moveSimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateMoveStatement(context.moveSimple());
            }
            else if (context.moveCorresponding() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateMoveCorrespondingStatement(context.moveCorresponding());
            }
        }

        public override void EnterMultiplyStatement(CodeElementsParser.MultiplyStatementContext context)
        {
            Context = context;
            if (context.multiplySimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateMultiplyStatement(context.multiplySimple());
            }
            else if (context.multiplyGiving() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateMultiplyGivingStatement(context.multiplyGiving());
            }
        }

        public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context)
        {
            Context = context;
            CodeElement = new MultiplyStatementEnd();
        }
        
        public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateOpenStatement(context);
        }

        public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreatePerformStatement(context);
        }

        public override void EnterPerformProcedureStatement(CodeElementsParser.PerformProcedureStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreatePerformProcedureStatement();
        }

        public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context)
        {
            Context = context;
            CodeElement = new PerformStatementEnd();
        }

        public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateReadStatement(context);
        }

        public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context)
        {            
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateReleaseStatement(context);
        }

        public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateReturnStatement(context);
        }

        public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReturnStatementEnd();
        }
        
        public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateRewriteStatement(context);
        }

        public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new RewriteStatementEnd();
        }

        public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context)
        {
            Context = context;
            if (context.serialSearch() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSerialSearchStatement(context.serialSearch());
            }
            else if (context.binarySearch() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateBinarySearchStatement(context.binarySearch());
            }
        }

        public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context)
        {
            Context = context;
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterSetStatement(CodeElementsParser.SetStatementContext context)
        {
            Context = context;
            if (context.setStatementForAssignation() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSetStatementForAssignation(context.setStatementForAssignation());
            }
            else if (context.setStatementForIndexes() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSetStatementForIndexes(context.setStatementForIndexes());
            }
            else if (context.setStatementForSwitches() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSetStatementForSwitches(context.setStatementForSwitches());
            }
            else if (context.setStatementForConditions() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSetStatementForConditions(context.setStatementForConditions());
            }
        }

        public override void EnterSortStatement(CodeElementsParser.SortStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateSortStatement(context);
        }

        public override void EnterStartStatement(CodeElementsParser.StartStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateStartStatement(context);
        }

        public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context)
        {
            Context = context;
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStopStatement(CodeElementsParser.StopStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateStopStatement(context);
        }

        public override void EnterStringStatement(CodeElementsParser.StringStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateStringStatement(context);
        }

        public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context)
        {
            Context = context;
            CodeElement = new StringStatementEnd();
        }

        public override void EnterSubtractStatement(CodeElementsParser.SubtractStatementContext context)
        {
            Context = context;
            if (context.subtractSimple() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSubtractStatement(context.subtractSimple());
            }
            else if (context.subtractGiving() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSubtractGivingStatement(context.subtractGiving());
            }
            else if (context.subtractCorresponding() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateSubtractCorrespondingStatement(context.subtractCorresponding());
            }
        }

        public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context)
        {
            Context = context;
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateUnstringStatement(context);
        }

        public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context)
        {
            Context = context;
            CodeElement = new UnstringStatementEnd();
        }

        public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateWriteStatement(context);
        }

        public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new WriteStatementEnd();
        }
        
        public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateXmlGenerateStatement(context);
        }

        public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context)
        {
            Context = context;
            CodeElement = CobolStatementsBuilder.CreateXmlParseStatement(context);
        }

        public override void EnterXmlStatementEnd(CodeElementsParser.XmlStatementEndContext context)
        {
            Context = context;
            CodeElement = new XmlStatementEnd();
        }

        // -- Statement conditions --

        public override void EnterAtEndCondition(CodeElementsParser.AtEndConditionContext context)
        {
            Context = context;
            CodeElement = new AtEndCondition();
        }

        public override void EnterNotAtEndCondition(CodeElementsParser.NotAtEndConditionContext context)
        {
            Context = context;
            CodeElement = new NotAtEndCondition();
        }

        public override void EnterAtEndOfPageCondition(CodeElementsParser.AtEndOfPageConditionContext context)
        {
            Context = context;
            CodeElement = new AtEndOfPageCondition();
        }

        public override void EnterNotAtEndOfPageCondition(CodeElementsParser.NotAtEndOfPageConditionContext context)
        {
            Context = context;
            CodeElement = new NotAtEndOfPageCondition();
        }
        
        public override void EnterWhenCondition(CodeElementsParser.WhenConditionContext context)
        {
            Context = context;
            CodeElement = new WhenCondition();
        }

        public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context)
        {
            Context = context;
            CodeElement = new WhenOtherCondition();
        }

        public override void EnterInvalidKeyCondition(CodeElementsParser.InvalidKeyConditionContext context)
        {
            Context = context;
            CodeElement = new InvalidKeyCondition();
        }

        public override void EnterNotInvalidKeyCondition(CodeElementsParser.NotInvalidKeyConditionContext context)
        {
            Context = context;
            CodeElement = new NotInvalidKeyCondition();
        }

        public override void EnterOnExceptionCondition(CodeElementsParser.OnExceptionConditionContext context)
        {
            Context = context;
            CodeElement = new OnExceptionCondition();
        }

        public override void EnterNotOnExceptionCondition(CodeElementsParser.NotOnExceptionConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnExceptionCondition();
        }

        public override void EnterOnOverflowCondition(CodeElementsParser.OnOverflowConditionContext context)
        {
            Context = context;
            CodeElement = new OnOverflowCondition();
        }

        public override void EnterNotOnOverflowCondition(CodeElementsParser.NotOnOverflowConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnOverflowCondition();
        }

        public override void EnterOnSizeErrorCondition(CodeElementsParser.OnSizeErrorConditionContext context)
        {
            Context = context;
            CodeElement = new OnSizeErrorCondition();
        }

        public override void EnterNotOnSizeErrorCondition(CodeElementsParser.NotOnSizeErrorConditionContext context)
        {
            Context = context;
            CodeElement = new NotOnSizeErrorCondition();
        }
    }
}
