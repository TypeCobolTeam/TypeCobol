using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    ///     Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CobolCodeElementBuilder : CodeElementsBaseListener
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
        ///     Initialization code run before parsing each new CodeElement
        /// </summary>
        public override void EnterCodeElement(CodeElementsParser.CodeElementContext context)
        {
            CodeElement = null;
            Context = null;
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

        internal static AuthoringProperties CreateAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context)
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

        private static AlphanumericValue[] CreateAlphanumericValues(CodeElementsParser.AlphanumericValue6Context[] alphanumericValueContexts)
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

        private static CharactersInCollatingSequence CreateCharactersInCollatingSequence(CodeElementsParser.CharactersInCollatingSequenceContext charsInCSContext)
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
                        CobolExpressionsBuilder.CreateQualifiedDataName(valueOfClauseContext.qualifiedDataName()[i]),
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

            /* [TYPECOBOL]
			entry.IsTypeDefinition = context.tcExtTypedefClause() != null;
			if (entry.IsTypeDefinition && entry.Name != null) {
				bool strong = context.tcExtTypedefClause().STRONG() != null;
				entry.DataType = new DataType(entry.Name.Name, strong);
			}

			foreach(var typeclause in context.tcExtTypeClause())
			{
			    var token = ParseTreeUtils.GetTokenFromTerminalNode(typeclause.UserDefinedWord());
			    if (token != null) entry.Picture = "TYPE:"+token.Text;
			}
               [/TYPECOBOL] */

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
            var procedureDivisionHeader = new ProcedureDivisionHeader();

            if (context.programInputParameters() != null)
            {
                foreach (var inputParametersContext in context.programInputParameters())
                {
                    SyntaxProperty<ReceivingMode> receivingMode = null;
                    if (inputParametersContext.REFERENCE() != null)
                    {
                        receivingMode = new SyntaxProperty<ReceivingMode>(ReceivingMode.ByReference,
                            ParseTreeUtils.GetFirstToken(inputParametersContext.REFERENCE()));
                    }
                    else if (inputParametersContext.VALUE() != null)
                    {
                        receivingMode = new SyntaxProperty<ReceivingMode>(ReceivingMode.ByValue,
                            ParseTreeUtils.GetFirstToken(inputParametersContext.VALUE()));
                    }
                    foreach (var storageAreaContext in inputParametersContext.storageArea2())
                    {
                        if (procedureDivisionHeader.InputParameters == null)
                        {
                            procedureDivisionHeader.InputParameters = new List<InputParameter>();
                        }

                        var inputParameter = new InputParameter {
                            ReceivingMode = receivingMode,
                            StorageArea = CobolExpressionsBuilder.CreateStorageArea(storageAreaContext) };
                        procedureDivisionHeader.InputParameters.Add(inputParameter);
                    }
                }
            }

            if (context.programOutputParameter() != null)
            {
                procedureDivisionHeader.OutputParameter =
                    CobolExpressionsBuilder.CreateStorageArea(context.programOutputParameter().storageArea2());
            }

            Context = context;
            CodeElement = procedureDivisionHeader;
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
            => Continue here
            Context = context;
            var builder = new StatementsBuilder();
            if (context.useStatementForExceptionDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForExceptionDeclarative());
            else
            if (context.useStatementForDebuggingDeclarative() != null)
                CodeElement = builder.CreateUseStatement(context.useStatementForDebuggingDeclarative());
            else
                Console.WriteLine("?TODO: USE?");
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
        
        // Statements

        public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateAcceptStatement(context);
        }
        
          ///////////////////////////
         // ARITHMETIC STATEMENTS //
        ///////////////////////////

        public override void EnterAddStatementFormat1(CodeElementsParser.AddStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat2(CodeElementsParser.AddStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            if (context.GIVING() != null)
            {
                builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                    context.identifierRounded());
            }

            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementFormat3(CodeElementsParser.AddStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('+');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context)
        {
            Context = context;
            CodeElement = new AddStatementEnd();
        }

        public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context)
        {
            Context = context;
            CodeElement = new ComputeStatementBuilder().CreateComputeStatement(context);
        }
        public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context)
        {
            Context = context;
            CodeElement = new ComputeStatementEnd();
        }

        public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context)
        {
            Context = context;
            CodeElement = new DivideStatementBuilder().CreateStatement(context);
        }
        public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context)
        {
            Context = context;
            CodeElement = new DivideStatementEnd();
        }



        public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context)
        {
            var statement = new AlterStatement();
            // context.procedureName().Length %2 != 0 can never happen outside of syntax errors
            AlterStatement.Alter alter = null;
            foreach (var p in context.procedureName())
            {
                if (alter == null) {
                    alter = new AlterStatement.Alter();
                    alter.Procedure1 = CobolWordsBuilder.CreateProcedureName(p);
                } else {
                    alter.Procedure2 = CobolWordsBuilder.CreateProcedureName(p);
                    statement.Items.Add(alter);
                    alter = null;
                }
            }
            Context = context;
            CodeElement = statement;
        }

        public override void EnterCallStatement(CodeElementsParser.CallStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateCallStatement(context);
        }
        public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context)
        {
            Context = context;
            CodeElement = new CallStatementEnd();
        }

        public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context)
        {
            var statement = new CancelStatement();
            if (context.programNameReference1() != null)
            {
                foreach (var c in context.programNameReference1())
                {
                    if (c.alphanumericLiteral() != null)
                    {
                        var item = CobolWordsBuilder.CreateLiteral(c.alphanumericLiteral());
                        statement.Items.Add(item);
                    }
                }
            }
            if (context.programNameFromData() != null)
            {
                foreach (var c in context.programNameFromData())
                {
                    if (c.identifier() != null)
                    {
                        var item = CobolWordsBuilder.CreateIdentifier(c.identifier());
                        statement.Items.Add(item);
                    }
                }
            }
            Context = context;
            CodeElement = statement;
        }

        public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context)
        {
            Context = context;
            CodeElement = new ContinueStatement();
        }

        public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context)
        {
            var statement = new DeleteStatement();
            statement.FileName = CobolWordsBuilder.CreateFileName(context.fileNameReference());
            Context = context;
            CodeElement = statement;
        }
        public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context)
        {
            Context = context;
            CodeElement = new DeleteStatementEnd();
        }

        /// <summary>
        ///     Create a MnemonicOrEnvironmentName from a token.
        ///     This method first check if the token match an environment name from EnvironmentNameEnum
        ///     If so, it's an EnvironmentName
        ///     otherwise, it's a mnemonic environment name
        /// </summary>
        /// <param name="mnemonicOrEnvironmentName">a token corresponding to environment or a mnemonic environment name</param>
        /// <returns>A MnemonicOrEnvironmentName of the correct CodeElementType: EnvironmentName or MnemonicForEnvironmentName</returns>
        public static MnemonicOrEnvironmentName CreateMnemonicOrEnvironmentName(Token mnemonicOrEnvironmentName)
        {
            EnvironmentNameEnum envNameValue;
            if (Enum.TryParse(mnemonicOrEnvironmentName.Text, true, out envNameValue))
            {
                return new EnvironmentName(mnemonicOrEnvironmentName, envNameValue);
            }
            //if this happens, it means it's a mnemonic environment name
            return new MnemonicForEnvironmentName(mnemonicOrEnvironmentName);
        }

        public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context)
        {
            var statement = new DisplayStatement();

            //Identifiers & literals
            if (context.identifierOrLiteral() != null)
            {
                var expressions = new List<Expression>();
                foreach (CodeElementsParser.IdentifierOrLiteralContext idOrLiteral in context.identifierOrLiteral())
                {
                    Expression identifier = CreateIdentifierOrLiteral(idOrLiteral, statement, "Display");
                    if (identifier != null)
                    {
                        expressions.Add(identifier);
                    }
                }
                statement.IdentifierOrLiteral = expressions;
            }
            //else don't set the displayStement. It will remains null

            //(mnemonic) Environment name
            if (context.uponEnvironmentName() != null)
            {
                Token mnemonicOrEnvironmentName = ParseTreeUtils.GetFirstToken(context.uponEnvironmentName().mnemonicForEnvironmentNameReferenceOrEnvironmentName());
                if (mnemonicOrEnvironmentName != null)
                {
                    statement.UponMnemonicOrEnvironmentName = new MnemonicOrEnvironmentName(mnemonicOrEnvironmentName);
//            EnvironmentNameEnum envNameValue;
//            if (Enum.TryParse(mnemonicOrEnvironmentName.Text, true, out envNameValue))
//            {
//                return new EnvironmentName(mnemonicOrEnvironmentName, envNameValue);
//            }
//            else
//            {
//                //if this happens, it means it's a mnemonic environment name
//                return new MnemonicForEnvironmentName(mnemonicOrEnvironmentName);
//            }
                }
            } //else don't set UponMnemonicOrEnvironmentName. it will remains null

			statement.IsWithNoAdvancing = context.withNoAdvancing() != null;

            Context = context;
            CodeElement = statement;
        }

        /// <summary>
        /// </summary>
        /// <param name="idOrLiteral"></param>
        /// <param name="statement">Only used in case of error to link the error with the current statement</param>
        /// <param name="statementName">Only used in case of error to have the name of the current statement</param>
        /// <returns></returns>
        public Expression CreateIdentifierOrLiteral(CodeElementsParser.IdentifierOrLiteralContext idOrLiteral, CodeElement statement,
            string statementName)
        {
            if (idOrLiteral.identifier() != null)
            {
                return CobolWordsBuilder.CreateIdentifier(idOrLiteral.identifier());
            }
            if (idOrLiteral.literal() != null)
            {
                return CobolWordsBuilder.CreateLiteral(idOrLiteral.literal());
            }
                //TODO manage figurativeConstant here or as a literal ?

            DiagnosticUtils.AddError(statement, statementName + ": required <identifier> or <literal>", idOrLiteral);
            return null;
        }

        public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context)
        {
            var statement = new EntryStatement();
            if (context.programEntryDefinition() != null)
            {
                statement.ProgramName = CobolWordsBuilder.CreateLiteral(context.programEntryDefinition().alphanumericLiteral());
            }
            foreach(var by in context.byReferenceOrByValueIdentifiers()) {
                var u = new EntryStatement.Using<Identifier>();
                var identifiers = CobolWordsBuilder.CreateIdentifiers(by.identifier());
                foreach (var i in identifiers) u.Add(i);
                u.ByValue = by.VALUE() != null;
                statement.Usings.Add(u);
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context)
        {
            var statement = new ExecStatement();
            Token node = null;
            if (context.execTranslatorName() != null)
            {
                node = ParseTreeUtils.GetTokenFromTerminalNode(context.execTranslatorName().ExecTranslatorName());
            }
            if (node != null) statement.Compiler = node.Text;
            var str = new StringBuilder();
            foreach (var line in context.ExecStatementText())
            {
                node = ParseTreeUtils.GetTokenFromTerminalNode(line);
                if (node != null) str.Append(node.Text);
            }
            statement.Code = str.ToString();

            Context = context;
            CodeElement = statement;
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

        public override void EnterExitStatement(CodeElementsParser.ExitStatementContext context)
        {
            Context = context;
            CodeElement = new ExitStatement();
        }

        public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context)
        {
            Context = context;
            CodeElement = new GobackStatement();
        }

        public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateGotoStatement(context);
        }


        public override void EnterIfStatement(CodeElementsParser.IfStatementContext context)
        {
            var statement = new IfStatement();
            if (context.conditionalExpression() != null) {
                statement.condition = new LogicalExpressionBuilder().createCondition(context.conditionalExpression());
            }
            Context = context;
            CodeElement = statement;
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


        public override void EnterEvaluateStatement(CodeElementsParser.EvaluateStatementContext context)
        {
            Context = context;
            CodeElement = new EvaluateStatement();
        }

        public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context)
        {
            Context = context;
            CodeElement = new EvaluateStatementEnd();
        }


        public override void EnterInitializeStatement(CodeElementsParser.InitializeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInitializeStatement(context);
        }

        public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInspectStatement(context);
        }

        public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateInvokeStatement(context);
        }

        public override void EnterMoveStatement(CodeElementsParser.MoveStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateMoveStatement(context);
        }

        public override void EnterMultiplyStatementFormat1(CodeElementsParser.MultiplyStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatementFormat2(CodeElementsParser.MultiplyStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('×');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context)
        {
            Context = context;
            CodeElement = new MultiplyStatementEnd();
        }


        public override void EnterNextSentenceStatement(CodeElementsParser.NextSentenceStatementContext context)
        {
            Context = context;
            CodeElement = new NextSentenceStatement();
        }

        public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateOpenStatement(context);
        }

        public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateCloseStatement(context);
        }

        public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateReadStatement(context);
        }

        public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReadStatementEnd();
        }

        public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateWriteStatement(context);
        }

        public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new WriteStatementEnd();
        }

        public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context)
        {
            Context = context;
            CodeElement = new FileOperationBuilder().CreateRewriteStatement(context);
        }

        public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context)
        {
            Context = context;
            CodeElement = new RewriteStatementEnd();
        }


        public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context)
        {
            Context = context;
            CodeElement = new PerformStatement();
        }

        public override void EnterPerformProcedureStatement(CodeElementsParser.PerformProcedureStatementContext context)
        {
            Context = context;
            CodeElement = new PerformProcedureStatement();
        }

        public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context)
        {
            Context = context;
            CodeElement = new PerformStatementEnd();
        }



        public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context)
        {
            var statement = new ReleaseStatement();
            statement.RecordName = CobolWordsBuilder.CreateQualifiedName(context.qualifiedDataName());
            statement.From = CobolWordsBuilder.CreateIdentifier(context.identifier());
            Context = context;
            CodeElement = statement;
        }

        public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateReturnStatement(context);
        }
        public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context)
        {
            Context = context;
            CodeElement = new ReturnStatementEnd();
        }

        public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateSearchStatement(context);
        }
        public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context)
        {
            Context = context;
            CodeElement = new SearchStatementEnd();
        }

        public override void EnterSetStatementForAssignation(CodeElementsParser.SetStatementForAssignationContext context)
        {
            var statement = new SetStatementForAssignation();
            if (context.identifier() != null)
            {
                statement.Receiving = new List<Expression>();
                foreach ( var identifierContext in context.identifier()) {
                    Expression receiving;
                    if (identifierContext != null)
                    {
                        receiving = CobolWordsBuilder.CreateIdentifier(identifierContext);
                    }
                    else break;
                    statement.Receiving.Add(receiving);
                }
            }

            if (context.setStatementForAssignationSending() != null)
            {
               if (context.setStatementForAssignationSending().identifier() != null)
                {
                    statement.Sending = CobolWordsBuilder.CreateIdentifier(context.setStatementForAssignationSending().identifier());
                }
                else if (context.setStatementForAssignationSending().IntegerLiteral() != null)
                {
                    statement.Sending = new Number(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().IntegerLiteral())));
                }
                else if (context.setStatementForAssignationSending().TRUE() != null)
                {
                    statement.Sending = new Literal(new SyntaxBoolean(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().TRUE())));
                }
                else if (context.setStatementForAssignationSending().FALSE() != null)
                {
                    statement.Sending = new Literal(new SyntaxBoolean(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().FALSE())));
                }
                else if (context.setStatementForAssignationSending().NULL() != null)
                {
                    statement.Sending = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULL()));
                }
                else if (context.setStatementForAssignationSending().NULLS() != null)
                {
                    statement.Sending = new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().NULLS()));
                }
                else if (context.setStatementForAssignationSending().SELF() != null)
                {
                    statement.Sending =
                        new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.setStatementForAssignationSending().SELF()));
                }
            }

            Context = context;
            CodeElement = statement;
        }


        public override void EnterSetStatementForIndexes(CodeElementsParser.SetStatementForIndexesContext context)
        {
            var statement = new SetStatementForIndex();

            if (context.indexNameReference() != null)
            {
                var indexs = new List<Index>();
                foreach (var indexNameContext in context.indexNameReference())
                {
                    indexs.Add(CobolWordsBuilder.CreateIndex(indexNameContext));
                }
                statement.ReceivingIndexs = indexs;
            }
			statement.UpBy   = (context.UP() != null);
			statement.DownBy = (context.DOWN() != null);

            if (context.identifier() != null)
            {
                statement.SendingField = CobolWordsBuilder.CreateIdentifier(context.identifier());
            } 
            else if (context.IntegerLiteral() != null)
            {
                statement.SendingField = new Number(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral())));
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterSetStatementForSwitches(CodeElementsParser.SetStatementForSwitchesContext context)
        {
            var statement = new SetStatementForSwitches();

            if (context.setStatementForSwitchesWhat() != null)
            {
                var setExternalSwitchs = new List<SetExternalSwitch>();
                foreach (var switchesWhatContext in context.setStatementForSwitchesWhat())
                {
                    var setExternalSwitch = new SetExternalSwitch();
                    
                    if (switchesWhatContext.mnemonicForUPSISwitchNameReference() != null)
                    {
                        var mnemonics = new List<MnemonicForEnvironmentName>();
                        foreach (var mnemonicContext in switchesWhatContext.mnemonicForUPSISwitchNameReference())
                        {
                           mnemonics.Add(new MnemonicForEnvironmentName(ParseTreeUtils.GetFirstToken(mnemonicContext)));
                        }
                        setExternalSwitch.MnemonicForEnvironmentNames = mnemonics;
                    }
					setExternalSwitch.ToOn = (switchesWhatContext.ON() != null);
					setExternalSwitch.ToOff= (switchesWhatContext.OFF() != null);
					setExternalSwitchs.Add(setExternalSwitch);
                }
                statement.SetExternalSwitches = setExternalSwitchs;
            }

            Context = context;
            CodeElement = statement;
        }



        public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateMergeStatement(context);
        }

        public override void EnterSortStatement(CodeElementsParser.SortStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateSortStatement(context);
        }

        public override void EnterStartStatement(CodeElementsParser.StartStatementContext context)
        {
            var statement = new StartStatement();
            statement.FileName = CobolWordsBuilder.CreateFileName(context.fileNameReference());
            statement.DataName = CobolWordsBuilder.CreateQualifiedName(context.qualifiedDataName());
            if (context.relationalOperator() != null)
                statement.Operator = new LogicalExpressionBuilder().CreateOperator(context.relationalOperator());
            
            Context = context;
            CodeElement = statement;
        }
        public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context)
        {
            Context = context;
            CodeElement = new StartStatementEnd();
        }

        public override void EnterStopStatement(CodeElementsParser.StopStatementContext context)
        {
            var statement = new StopStatement();
            if (context.literal() != null)
                statement.Literal = CobolWordsBuilder.CreateLiteral(context.literal());
            statement.IsStopRun = context.RUN() != null;

            Context = context;
            CodeElement = statement;
        }

        public override void EnterStringStatement(CodeElementsParser.StringStatementContext context)
        {
            var statement = new StringStatement();

            if (context.stringStatementWhat() != null)
            {
                var statementWhatList = new List<StringStatementWhat>();
                foreach (CodeElementsParser.StringStatementWhatContext stringStatementWhatContext in context.stringStatementWhat())
                {
                    var stringStatementWhat = new StringStatementWhat();

                    if (stringStatementWhatContext.identifierToConcat != null)
                    {
                        var identifierToConcat = new List<Expression>();
                        foreach (
                            CodeElementsParser.IdentifierOrLiteralContext idOrLiteral in
                                stringStatementWhatContext.identifierOrLiteral())
                        {
                            identifierToConcat.Add(CreateIdentifierOrLiteral(idOrLiteral, statement, "String"));
                        }
                        stringStatementWhat.IdentifierToConcat = identifierToConcat;
                    }
                    //else don't set IdentifierToConcat. It will remains null


					if (stringStatementWhatContext.stringStatementDelimiter() != null) {
						if (stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral() != null) {
							stringStatementWhat.DelimiterIdentifier =
								CreateIdentifierOrLiteral(stringStatementWhatContext.stringStatementDelimiter().identifierOrLiteral(), statement, "String");
						} else {
							stringStatementWhat.DelimitedBySize = (stringStatementWhatContext.stringStatementDelimiter().SIZE() != null);
						}
					}
                    statementWhatList.Add(stringStatementWhat);
                }

                statement.StringStatementWhat = statementWhatList;
            }
            //else don't set statement.StringStatementWhat


            if (context.identifierInto != null)
            {
                statement.IntoIdentifier = CobolWordsBuilder.CreateIdentifier(context.identifierInto);
            } //else don't set statement.IntoIdentifier


            if (context.stringStatementWith() != null)
            {
                statement.PointerIdentifier = CobolWordsBuilder.CreateIdentifier(context.stringStatementWith().identifier());
            } //else don't set statement.PointerIdentifier

            Context = context;
            CodeElement = statement;
        }

        public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context)
        {
            Context = context;
            CodeElement = new StringStatementEnd();
        }

        public override void EnterSubtractStatementFormat1(CodeElementsParser.SubtractStatementFormat1Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat1Statement(context.identifierOrNumericLiteral(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat2(CodeElementsParser.SubtractStatementFormat2Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat2Statement(context.identifierOrNumericLiteral(), context.identifierOrNumericLiteralTmp(),
                context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementFormat3(CodeElementsParser.SubtractStatementFormat3Context context)
        {
            var builder = new ArithmeticStatementBuilder('-');
            builder.InitializeFormat3Statement(context.identifier(), context.identifierRounded());
            Context = context;
            CodeElement = builder.statement;
        }

        public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context)
        {
            Context = context;
            CodeElement = new SubtractStatementEnd();
        }

        public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context)
        {
            var statement = new UnstringStatement();

            if (context.unstringIdentifier != null)
            {
                statement.UnstringIdentifier = CobolWordsBuilder.CreateIdentifier(context.unstringIdentifier);
            }

            if (context.unstringDelimited() != null)
            {
                if (context.unstringDelimited().delimitedBy != null)
                {
                    statement.DelimitedBy = CreateIdentifierOrLiteral(context.unstringDelimited().delimitedBy, statement, "unstring");
                }

                if (context.unstringDelimited().ustringOthersDelimiters() != null)
                {
                    var otherDelimiters = new List<Expression>();
                    foreach (
                        CodeElementsParser.UstringOthersDelimitersContext ustringOthersDelimitersContext in
                            context.unstringDelimited().ustringOthersDelimiters())
                    {
                        otherDelimiters.Add(CreateIdentifierOrLiteral(ustringOthersDelimitersContext.identifierOrLiteral(), statement,
                            "Unstring"));
                    }
                    statement.OtherDelimiters = otherDelimiters;
                }
            }

            if (context.unstringReceiver() != null)
            {
                var unstringReceiverList = new List<UnstringReceiver>();
                foreach (CodeElementsParser.UnstringReceiverContext unstringReceiverContext in context.unstringReceiver())
                {
                    var unstringReceiver = new UnstringReceiver();
                    if (unstringReceiverContext.intoIdentifier != null)
                    {
                        unstringReceiver.IntoIdentifier = CobolWordsBuilder.CreateIdentifier(unstringReceiverContext.intoIdentifier);
                    }
                    if (unstringReceiverContext.unstringDelimiter() != null &&
                        unstringReceiverContext.unstringDelimiter().identifier() != null)
                    {
                        unstringReceiver.DelimiterIdentifier =
                            CobolWordsBuilder.CreateIdentifier(unstringReceiverContext.unstringDelimiter().identifier());
                    }
                    if (unstringReceiverContext.unstringCount() != null && unstringReceiverContext.unstringCount().identifier() != null)
                    {
                        unstringReceiver.CountIdentifier =
                            CobolWordsBuilder.CreateIdentifier(unstringReceiverContext.unstringCount().identifier());
                    }
                    unstringReceiverList.Add(unstringReceiver);
                }
                statement.UnstringReceivers = unstringReceiverList;
            }

            if (context.unstringPointer() != null && context.unstringPointer().identifier() != null)
            {
                statement.WithPointer = CobolWordsBuilder.CreateIdentifier(context.unstringPointer().identifier());
            }

            if (context.unstringTallying() != null && context.unstringTallying().identifier() != null)
            {
                statement.Tallying = CobolWordsBuilder.CreateIdentifier(context.unstringTallying().identifier());
            }

            Context = context;
            CodeElement = statement;
        }

        public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context)
        {
            Context = context;
            CodeElement = new UnstringStatementEnd();
        }        

        public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateXmlGenerateStatement(context);
        }

        public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateXmlParseStatement(context);
        }

        public override void EnterXmlStatementEnd(CodeElementsParser.XmlStatementEndContext context)
        {
            Context = context;
            CodeElement = new XmlStatementEnd();
        }

        // Statement conditions

        public override void EnterWhenCondition(CodeElementsParser.WhenConditionContext context)
        {
            Context = context;
            CodeElement = new WhenConditionalExpression();
        }

        public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context)
        {
            Context = context;
            CodeElement = new WhenOtherCondition();
        }

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

        // -- Symbols --

        // ** Program names and Program entries **

        public override void EnterProgramNameDefinition(CodeElementsParser.ProgramNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramEntryDefinition(CodeElementsParser.ProgramEntryDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramEntry);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference1(CodeElementsParser.ProgramNameReference1Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference2(CodeElementsParser.ProgramNameReference2Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReferenceOrProgramEntryReference(CodeElementsParser.ProgramNameReferenceOrProgramEntryReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ProgramName, SymbolType.ProgramEntry };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references
        // => programNameFromDataOrProgramEntryFromData
        // => programNameFromDataOrProgramEntryFromDataOrProcedurePointerOrFunctionPointer

        // ** Section names and Paragraph names **

        public override void EnterSectionNameDefinition(CodeElementsParser.SectionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameDefinition(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSectionNameReference(CodeElementsParser.SectionNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameOrSectionNameReference(CodeElementsParser.ParagraphNameOrSectionNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.SectionName, SymbolType.ParagraphName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void ExitQualifiedParagraphNameReference(CodeElementsParser.QualifiedParagraphNameReferenceContext context)
        {
            if (context.sectionNameReference() != null)
            {
                Token qualifiedToken = ParseTreeUtils.GetFirstToken(context.paragraphNameReference());
                Token[] qualifierTokens = new Token[] { ParseTreeUtils.GetFirstToken(context.sectionNameReference()) };
                UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
            }
        }

        // ** Class names and Method names **

        public override void EnterClassNameDefinition(CodeElementsParser.ClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameDefinition(CodeElementsParser.MethodNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameReference(CodeElementsParser.ClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameDefOrRef(CodeElementsParser.ClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.ClassName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
        
        public override void EnterExternalClassNameDefOrRef(CodeElementsParser.ExternalClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ExternalClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameReference(CodeElementsParser.MethodNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references 
        // => methodNameFromData

        // ** Environment names, UPSI switch names and associated Mnemonics, Condition  **

        public override void EnterEnvironmentName(CodeElementsParser.EnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.EnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterUpsiSwitchName(CodeElementsParser.UpsiSwitchNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.UPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameDefinition(CodeElementsParser.MnemonicForEnvironmentNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameDefinition(CodeElementsParser.MnemonicForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionForUPSISwitchNameDefinition(CodeElementsParser.ConditionForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReference(CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReferenceOrEnvironmentName(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.MnemonicForEnvironmentName, SymbolType.EnvironmentName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameReference(CodeElementsParser.MnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName, SymbolType.MnemonicForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Character sets **

        public override void EnterSymbolicCharacterDefinition(CodeElementsParser.SymbolicCharacterDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSymbolicCharacterReference(CodeElementsParser.SymbolicCharacterReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameDefinition(CodeElementsParser.AlphabetNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameReference(CodeElementsParser.AlphabetNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameDefinition(CodeElementsParser.CharacterClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameReference(CodeElementsParser.CharacterClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Data item, Data conditions and Indexes **

        public override void EnterDataNameDefinition(CodeElementsParser.DataNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReference(CodeElementsParser.DataNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSpecialRegister(CodeElementsParser.SpecialRegisterContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        
        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
        public override void EnterConditionNameDefinition(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Condition name reference already handled above :
        // => public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)

        // Condition name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
		public override void ExitQualifiedDataName(CodeElementsParser.QualifiedDataNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrIndexName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrIndexNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedConditionName(CodeElementsParser.QualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrIndexName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrFileName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrFileNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrClassName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrClassNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}

		private void GetTokensForSymbolInformation(out Token qualifiedToken, out Token[] qualifierTokens, IParseTree node, IParseTree[] nodes) {
			if (node != null)  qualifiedToken  = ParseTreeUtils.GetFirstToken(node);
			else qualifiedToken = null;
			if (nodes != null) qualifierTokens = nodes.Select(ctx => ParseTreeUtils.GetFirstToken(ctx)).ToArray();
			else qualifierTokens = null;
		}

		private void UpdateSymbolInformationForQualifiedNames(Token qualifiedToken, Token[] qualifierTokens) {
			if (qualifiedToken != null) {
				SymbolInformation qualifiedSymbolInfo = CodeElement.SymbolInformationForTokens[qualifiedToken];
				qualifiedSymbolInfo.QualifedBy = qualifierTokens;
			}
			if (qualifierTokens != null)
			foreach (var qualifierToken in qualifierTokens) {
				SymbolInformation qualifierSymbolInfo = CodeElement.SymbolInformationForTokens[qualifierToken];
				qualifierSymbolInfo.QualifierFor = qualifiedToken;
			}
		}

        public override void EnterIndexNameDefinition(CodeElementsParser.IndexNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterIndexNameReference(CodeElementsParser.IndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
               
        // ** Files **

        public override void EnterFileNameDefinition(CodeElementsParser.FileNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterFileNameReference(CodeElementsParser.FileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameDefinition(CodeElementsParser.XmlSchemaNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameReference(CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentName(CodeElementsParser.AssignmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.AssignmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentNameOrFileNameReference(CodeElementsParser.AssignmentNameOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.AssignmentName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Runtime functions **

		public override void EnterIntrinsicFunctionName(CodeElementsParser.IntrinsicFunctionNameContext context) {
			ITerminalNode node = null;
			if (context.UserDefinedWord() != null) node = context.UserDefinedWord();
			else
			if (context.FunctionName() != null) node = context.FunctionName();
			else
			if (context.LENGTH() != null) node = context.LENGTH();
			else
			if (context.RANDOM() != null) node = context.RANDOM();
			else
			if (context.WHEN_COMPILED() != null) node = context.WHEN_COMPILED();
			Token symbolToken = ParseTreeUtils.GetFirstToken(node);
			SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.FunctionName);
			CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterExecTranslatorName(CodeElementsParser.ExecTranslatorNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.ExecTranslatorName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
    }
}
