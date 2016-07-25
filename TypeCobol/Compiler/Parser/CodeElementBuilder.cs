using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.CodeElements.Functions;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    ///     Build a CodeElement object while visiting its parse tree
    /// </summary>
    internal class CodeElementBuilder : CodeElementsBaseListener
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


		private CobolWordsBuilder CobolWordsBuilder { get; set; }
		private CobolExpressionsBuilder CobolExpressionsBuilder { get; set; }
		private CobolStatementsBuilder CobolStatementsBuilder { get; set; }

		/// <summary>Initialization code run before parsing each new COBOL CodeElement</summary>
		public override void EnterCodeElement(CodeElementsParser.CodeElementContext context) {
			CodeElement = null;
			Context = null;
			CobolWordsBuilder = new CobolWordsBuilder(new Dictionary<Token, SymbolInformation>());
			CobolExpressionsBuilder = new CobolExpressionsBuilder(CobolWordsBuilder);
			CobolStatementsBuilder = new CobolStatementsBuilder(CobolWordsBuilder, CobolExpressionsBuilder);
		}

		/// <summary>Code run after parsing each new CodeElement</summary>
		public override void ExitCodeElement(CodeElementsParser.CodeElementContext context) {
			if(CodeElement != null && CobolWordsBuilder.symbolInformationForTokens.Keys.Count > 0) {
				CodeElement.SymbolInformationForTokens = CobolWordsBuilder.symbolInformationForTokens;
			}
		}

		// Code structure

		  ////////////////////
		 // IDENTIFICATION //
		////////////////////



		 // PROGRAM IDENTIFICATION
		////////////////////////////

		public override void EnterProgramIdentification(CodeElementsParser.ProgramIdentificationContext context) {
			var programIdentification = new ProgramIdentification();
			programIdentification.ProgramName = CobolWordsBuilder.CreateProgramNameDefinition(context.programNameDefinition());
			if (context.COMMON() != null) {
				programIdentification.IsCommon = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.COMMON()));
			}
			if (context.INITIAL() != null) {
				programIdentification.IsInitial = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.INITIAL()));
			}
			if (context.RECURSIVE() != null) {
				programIdentification.IsRecursive = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.RECURSIVE()));
			}
			programIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = programIdentification;
		}

		public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context) {
			var programEnd = new ProgramEnd();
			programEnd.ProgramName = CobolWordsBuilder.CreateProgramNameReference(context.programNameReference2());

			Context = context;
			CodeElement = programEnd;
		}



		 // CLASS IDENTIFICATION
		//////////////////////////

		public override void EnterClassIdentification(CodeElementsParser.ClassIdentificationContext context) {
			var classIdentification = new ClassIdentification();
			classIdentification.ClassName = CobolWordsBuilder.CreateClassNameDefinition(context.classNameDefinition());
			classIdentification.InheritsFrom = CobolWordsBuilder.CreateClassNameReference(context.inheritsFromClassName);
			classIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = classIdentification;
		}

		public override void EnterClassEnd(CodeElementsParser.ClassEndContext context) {
			var classEnd = new ClassEnd();
			classEnd.ClassName = CobolWordsBuilder.CreateClassNameReference(context.classNameReference());

			Context = context;
			CodeElement = classEnd;
		}



		 // FACTORY IDENTIFICATION
		////////////////////////////

		public override void EnterFactoryIdentification(CodeElementsParser.FactoryIdentificationContext context) {
			Context = context;
			CodeElement = new FactoryIdentification();
		}

		public override void EnterFactoryEnd(CodeElementsParser.FactoryEndContext context) {
			Context = context;
			CodeElement = new FactoryEnd();
		}

		 // OBJECT IDENTIFICATION
		///////////////////////////

		public override void EnterObjectIdentification(CodeElementsParser.ObjectIdentificationContext context) {
			Context = context;
			CodeElement = new ObjectIdentification();
		}

		public override void EnterObjectEnd(CodeElementsParser.ObjectEndContext context) {
			Context = context;
			CodeElement = new ObjectEnd();
		}

		 // METHOD IDENTIFICATION
		///////////////////////////

		public override void EnterMethodIdentification(CodeElementsParser.MethodIdentificationContext context) {
			var methodIdentification = new MethodIdentification();
			methodIdentification.MethodName = CobolWordsBuilder.CreateMethodNameDefinition(context.methodNameDefinition());
			methodIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = methodIdentification;
		}

		public override void EnterMethodEnd(CodeElementsParser.MethodEndContext context) {
			var methodEnd = new MethodEnd();
			methodEnd.MethodName = CobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());

			Context = context;
			CodeElement = methodEnd;
		}

		// --- Authoring properties common to all identification divisions ---

		internal AuthoringProperties CreateAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context) {
			var authoringProperties = new AuthoringProperties();
			if (context.authorParagraph().Length > 0) {
				var alphanumericValueContexts = context.authorParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Author = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateCompiledParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateCompiledParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.DateCompiled = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateWrittenParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateWrittenParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.DateWritten = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.installationParagraph().Length > 0) {
				var alphanumericValueContexts = context.installationParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Installation = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.securityParagraph().Length > 0) {
				var alphanumericValueContexts = context.securityParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Security = CreateAlphanumericValues(alphanumericValueContexts);
			}
			return authoringProperties;
		}

		private AlphanumericValue[] CreateAlphanumericValues(CodeElementsParser.AlphanumericValue6Context[] contexts) {
			AlphanumericValue[] alphanumericValues = new AlphanumericValue[contexts.Length];
			for (int i = 0; i < contexts.Length; i++) {
				alphanumericValues[i] = CobolWordsBuilder.CreateAlphanumericValue(contexts[i]);
			}
			return alphanumericValues;
		}



		  //////////////////////////
		 // ENVIRONMENT DIVISION //
		//////////////////////////

		public override void EnterEnvironmentDivisionHeader(CodeElementsParser.EnvironmentDivisionHeaderContext context) {
			Context = context;
			CodeElement = new EnvironmentDivisionHeader();
		}



		 // CONFIGURATION SECTION
		///////////////////////////

		public override void EnterConfigurationSectionHeader(CodeElementsParser.ConfigurationSectionHeaderContext context) {
			Context = context;
			CodeElement = new ConfigurationSectionHeader();
		}

		// --- SOURCE-COMPUTER PARAGRAPH ---

		public override void EnterSourceComputerParagraph(CodeElementsParser.SourceComputerParagraphContext context)
		{
			var paragraph = new SourceComputerParagraph();
			if(context.computerName != null) {
				paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
			}
			if(context.DEBUGGING() != null) {
				paragraph.DebuggingMode = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.DEBUGGING()));
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- OBJECT-COMPUTER PARAGRAPH ---

		public override void EnterObjectComputerParagraph(CodeElementsParser.ObjectComputerParagraphContext context) {
			var paragraph = new ObjectComputerParagraph();
			if(context.computerName != null) {
				paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
			}
			if(context.memorySizeClause() != null) {
				var memorySizeClauseContext = context.memorySizeClause();
				paragraph.MemorySize = CobolWordsBuilder.CreateIntegerValue(memorySizeClauseContext.integerValue());
				if(memorySizeClauseContext.WORDS() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Words, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.WORDS()));
				} else
				if (memorySizeClauseContext.CHARACTERS() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Characters, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.CHARACTERS()));
				} else
				if (memorySizeClauseContext.MODULES() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Modules, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.MODULES()));
				}
			}
			if(context.programCollatingSequenceClause() != null) {
				var collatingSeqClauseContext = context.programCollatingSequenceClause();
				paragraph.CollatingSequence = CobolWordsBuilder.CreateAlphabetName(collatingSeqClauseContext.alphabetName());
			}
			if(context.segmentLimitClause() != null) {
				var segmentLimitClauseContext = context.segmentLimitClause();
				paragraph.SegmentLimit = CobolWordsBuilder.CreateIntegerValue(segmentLimitClauseContext.priorityNumber().integerValue());
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- SPECIAL-NAMES PARAGRAPH ---

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

		private CharactersRangeInCollatingSequence CreateCharactersRange(CodeElementsParser.CharactersRangeContext context) {
			var charactersRange = new CharactersRangeInCollatingSequence();
			charactersRange.StartCharacter = CreateCharacterInCollatingSequence(context.startCharacter);
			charactersRange.EndCharacter = CreateCharacterInCollatingSequence(context.endCharacter);
			return charactersRange;
		}

		private CharactersInCollatingSequence CreateCharactersInCollatingSequence(CodeElementsParser.CharactersInCollatingSequenceContext context) {
			var chars = new CharactersInCollatingSequence();
			if (context.alphanumericValue1() != null) {
				chars.CharactersInAlphanmericValue = CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue1());
			} else
			if (context.ordinalPositionInCollatingSequence() != null) {
				chars.OrdinalPositionInCollatingSequence = CobolWordsBuilder.CreateIntegerValue(context.ordinalPositionInCollatingSequence().integerValue());
			}
			return chars;
		}

		private CharacterInCollatingSequence CreateCharacterInCollatingSequence(CodeElementsParser.CharacterInCollatingSequenceContext context) {
			var chars = new CharacterInCollatingSequence();
			if (context.characterValue2() != null) {
				chars.CharacterValue = CobolWordsBuilder.CreateCharacterValue(context.characterValue2());
			} else
			if (context.ordinalPositionInCollatingSequence() != null) {
				chars.OrdinalPositionInCollatingSequence = CobolWordsBuilder.CreateIntegerValue(context.ordinalPositionInCollatingSequence().integerValue());
			}
			return chars;
		}

		// --- REPOSITORY PARAGRAPH ---

		public override void EnterRepositoryParagraph(CodeElementsParser.RepositoryParagraphContext context) {
			var paragraph = new RepositoryParagraph();
			if(context.repositoryClassDeclaration() != null &  context.repositoryClassDeclaration().Length > 0) {
				if(paragraph.ClassNames == null) {
					paragraph.ClassNames = new Dictionary<SymbolDefinitionOrReference, SymbolDefinitionOrReference>();
				}
				foreach(var c in context.repositoryClassDeclaration()) {
					var className = CobolWordsBuilder.CreateClassNameDefOrRef(c.classNameDefOrRef());
					SymbolDefinitionOrReference externalClassName = null;
					if(c.externalClassNameDefOrRef() != null) {
						externalClassName = CobolWordsBuilder.CreateExternalClassNameDefOrRef(c.externalClassNameDefOrRef());
					}
					paragraph.ClassNames.Add(className, externalClassName);
				}
			}

			Context = context;
			CodeElement = paragraph;
		}



		 // INPUT-OUTPUT SECTION
		//////////////////////////

		public override void EnterInputOutputSectionHeader(CodeElementsParser.InputOutputSectionHeaderContext context) {
			Context = context;
			CodeElement = new InputOutputSectionHeader();
		}

		public override void EnterFileControlParagraphHeader(CodeElementsParser.FileControlParagraphHeaderContext context) {
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
		
		public override void EnterIoControlParagraphHeader(CodeElementsParser.IoControlParagraphHeaderContext context) {
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





		  ///////////////////
		 // DATA DIVISION //
		///////////////////

		public override void EnterDataDivisionHeader(CodeElementsParser.DataDivisionHeaderContext context) {
			Context = context;
			CodeElement = new DataDivisionHeader();
		}

		public override void EnterFileSectionHeader(CodeElementsParser.FileSectionHeaderContext context) {
			Context = context;
			CodeElement = new FileSectionHeader();
		}

		public override void EnterWorkingStorageSectionHeader(CodeElementsParser.WorkingStorageSectionHeaderContext context) {
			Context = context;
			CodeElement = new WorkingStorageSectionHeader();
		}

		public override void EnterLocalStorageSectionHeader(CodeElementsParser.LocalStorageSectionHeaderContext context) {
			Context = context;
			CodeElement = new LocalStorageSectionHeader();
		}

		public override void EnterLinkageSectionHeader(CodeElementsParser.LinkageSectionHeaderContext context) {
			Context = context;
			CodeElement = new LinkageSectionHeader();
		}

		 // FILE DESCRIPTION ENTRY
		////////////////////////////

		public override void EnterFileDescriptionEntry(CodeElementsParser.FileDescriptionEntryContext context) {
			var entry = new FileDescriptionEntry();

			if (context.FD() != null)
				entry.Type = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.File, ParseTreeUtils.GetFirstToken(context.FD()));
			else
			if (context.SD() != null)
				entry.Type = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.SortMergeFile, ParseTreeUtils.GetFirstToken(context.SD()));

			entry.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());

			if (context.externalClause() != null && context.externalClause().Length > 0) {
				var externalClauseContext = context.externalClause()[0];
				entry.IsExternal = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(externalClauseContext.EXTERNAL()));
			}
			if (context.globalClause() != null) {
				var globalClauseContext = context.globalClause()[0];
				entry.IsGlobal = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(globalClauseContext.GLOBAL()));
			}
			if (context.blockContainsClause() != null && context.blockContainsClause().Length > 0) {
				var blockContainsClauseContext = context.blockContainsClause()[0];
				entry.MaxBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.maxNumberOfBytes);
				if (blockContainsClauseContext.minNumberOfBytes != null) {
					entry.MinBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.minNumberOfBytes);
				}
				if (blockContainsClauseContext.CHARACTERS() != null) {
					entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Characters,
						ParseTreeUtils.GetFirstToken(blockContainsClauseContext.CHARACTERS()));
				} else
				if (blockContainsClauseContext.RECORDS() != null) {
					entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Records, ParseTreeUtils.GetFirstToken(blockContainsClauseContext.RECORDS()));
				}
			}
			if (context.recordClause() != null && context.recordClause().Length > 0) {
				var recordClauseContext = context.recordClause()[0];
				if (recordClauseContext.numberOfBytes != null) {
					entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.numberOfBytes);
					entry.MaxRecordSize = entry.MinRecordSize;
				} else
				if (recordClauseContext.minNumberOfBytes != null) {
					entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.minNumberOfBytes);
					entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.maxNumberOfBytes);
				} else
				if (recordClauseContext.VARYING() != null) {
					if (recordClauseContext.fromNumberOfBytes != null)
						entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.fromNumberOfBytes);
					if (recordClauseContext.toNumberOfBytes != null)
						entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.toNumberOfBytes);
					if (recordClauseContext.dataNameReference() != null)
						entry.RecordSizeDependingOn = CobolWordsBuilder.CreateDataNameReference(recordClauseContext.dataNameReference());
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

		public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context) {
			if(context.dataRenamesEntry() != null || context.dataConditionEntry() != null) {
				// For levels 66 and 88, the DataDescriptionEntry is created by the following methods
				// - EnterDataRenamesEntry
				// - EnterDataConditionEntry
				return;
			}

			DataDescriptionEntry entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.DataName = SyntaxElementBuilder.CreateDataName(context.dataNameDefinition());
			//entry.IsFiller = (dataname == null || context.FILLER() != null);

			var redefines = context.redefinesClause();
			if (redefines != null)
                entry.RedefinesDataName = SyntaxElementBuilder.CreateDataName(redefines.dataNameReference());

			var picture = DataDescriptionChecker.GetContext(entry, context.pictureClause(), false);
			if (picture != null) entry.Picture = picture.PictureCharacterString().GetText();

			var blank = DataDescriptionChecker.GetContext(entry, context.blankWhenZeroClause(), false);
			entry.IsBlankWhenZero = blank != null && blank.BLANK() != null;
			var external = DataDescriptionChecker.GetContext(entry, context.externalClause(), false);
			entry.IsExternal = external != null && external.EXTERNAL() != null;
			var global = DataDescriptionChecker.GetContext(entry, context.globalClause(), false);
			entry.IsGlobal = global != null && global.GLOBAL() != null;
			var justified = DataDescriptionChecker.GetContext(entry, context.justifiedClause(), false);
			entry.IsJustified = justified != null && (justified.JUSTIFIED() != null || justified.JUST() != null);
			var sync = DataDescriptionChecker.GetContext(entry, context.synchronizedClause(), false);
			entry.IsSynchronized = (sync != null) && (sync.SYNC() != null || sync.SYNCHRONIZED() != null || sync.LEFT() != null || sync.RIGHT() != null);
			var group = DataDescriptionChecker.GetContext(entry, context.groupUsageClause(), false);
			entry.IsGroupUsageNational = group != null && (group.GROUP_USAGE() != null || group.NATIONAL() != null);
			UpdateDataDescriptionEntryWithUsageClause(entry, DataDescriptionChecker.GetContext(entry, context.usageClause(), false));
			UpdateDataDescriptionEntryWithSignClause(entry, DataDescriptionChecker.GetContext(entry, context.signClause(), false));
			UpdateDataDescriptionEntryWithOccursClause(entry, DataDescriptionChecker.GetContext(entry, context.occursClause(), false));
			UpdateDataDescriptionEntryWithValueClause(entry, DataDescriptionChecker.GetContext(entry, context.valueClause(), false));

            // [Cobol 2002]
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
            // [/Cobol 2002]

            Context = context;
			CodeElement = entry;
		}

		public override void EnterDataRenamesEntry(CodeElementsParser.DataRenamesEntryContext context) {
			DataDescriptionEntry entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.DataName = SyntaxElementBuilder.CreateDataName(context.dataNameDefinition());
            //entry.IsFiller = (dataname == null || context.FILLER() != null);


			var names = SyntaxElementBuilder.CreateQualifiedNames(context.renamesClause().qualifiedDataName());
			if (names.Count > 0) entry.RenamesFromDataName = names[0];
			if (names.Count > 1) entry.RenamesToDataName   = names[1];
			//note: "RENAMES THRU dataname" will yield "from" initialized and "to" uninitialized

			Context = context;
			CodeElement = entry;
		}

		public override void EnterDataConditionEntry(CodeElementsParser.DataConditionEntryContext context) {
			var entry = new DataDescriptionEntry();
			if (context.levelNumber() != null && context.levelNumber().IntegerLiteral() != null) {
				entry.LevelNumber = SyntaxElementBuilder.CreateInteger(context.levelNumber().IntegerLiteral());
			}
			entry.ConditionName = SyntaxElementBuilder.CreateConditionName(context.conditionNameDefinition());
			entry.IsConditionNameDescription = true;
			UpdateDataDescriptionEntryWithValueClauseForCondition(entry, context.valueClauseForCondition());

			Context = context;
			CodeElement = entry;
		}

        private void UpdateDataDescriptionEntryWithSignClause(DataDescriptionEntry entry, CodeElementsParser.SignClauseContext context)
        {
            if (context == null) return;
            entry.SignPosition = SignPosition.None;
            if (context.TRAILING() != null) entry.SignPosition = SignPosition.Trailing;
            if (context.LEADING()  != null) entry.SignPosition = SignPosition.Leading;
            entry.IsSignSeparate = context.SEPARATE() != null;
        }

        private void UpdateDataDescriptionEntryWithUsageClause(DataDescriptionEntry entry, CodeElementsParser.UsageClauseContext context)
        {
            if (context == null) return;
            entry.Usage = CreateDataUsage(context);
            entry.ObjectReference = SyntaxElementBuilder.CreateClassName(context.classNameReference());
        }

        private void UpdateDataDescriptionEntryWithOccursClause(DataDescriptionEntry entry, CodeElementsParser.OccursClauseContext context)
        {
            if (context == null) return;
            entry.IsTableOccurence = true;

            bool isVariable = (context.occursDependingOn() != null);
            if (isVariable) {
                entry.OccursDependingOn = SyntaxElementBuilder.CreateDataName(context.occursDependingOn().dataNameReference());
            }
            isVariable = isVariable || (context.UNBOUNDED() != null) || (context.TO() != null);

            var integers = context.IntegerLiteral();
            if (integers != null) {
                isVariable = isVariable || (integers.Length == 2);
                if (integers.Length == 0) {
                    if (isVariable) {
                            // 1) OCCURS UNBOUNDED DEPENDING ON...
                        entry.MinOccurencesCount = 1;
                        entry.MaxOccurencesCount = Int32.MaxValue;
                    }
                    // else;   2) OCCURS ... -syntax error (fixed length, exact missing)
                } else
                if (integers.Length == 1) {
                    if (isVariable) {
                        if (context.UNBOUNDED() != null) {
                            // 3) OCCURS min TO UNBOUNDED DEPENDING ON...
                            // 4) OCCURS min UNBOUNDED DEPENDING ON... -syntax error (TO missing)
                            entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                            entry.NoMaxOccurencesCount = true;
                        } else {
                            // 5) OCCURS max DEPENDING ON...
                            // 6) OCCURS min TO DEPENDING ON... -syntax error (max missing)
                            // 7) OCCURS TO max DEPENDING ON... -syntax error (min missing)
                            // WARNING! due to our grammar, we cannot discriminate between 6) and 7)
                            // this shouldn't be a problem as both cases are syntax errors
                            entry.MinOccurencesCount = 1;
                            entry.MaxOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                        }
                    } else {
                            // 8) OCCURS exact ... (fixed length)
                        entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                        entry.MaxOccurencesCount = entry.MinOccurencesCount;
                    }
                } else { // isVariable == true && integers.Length == 2
                            // 9) OCCURS min TO max DEPENDING ON...
                            //10) OCCURS min max DEPENDING ON... -syntax error (TO missing)
                    entry.MinOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[0]);
                    entry.MaxOccurencesCount = SyntaxElementBuilder.CreateInteger(integers[1]);
                }
            }

            var keys = context.occursKeys();
            if (keys != null) {
                entry.TableOccurenceKeys = new List<DataName>();
                entry.TableOccurenceKeyDirections = new List<KeyDirection>();
                foreach(var key in keys) {
                    var direction = KeyDirection.None;
                    if (key.ASCENDING()  != null) direction = KeyDirection.Ascending;
                    if (key.DESCENDING() != null) direction = KeyDirection.Descending;
                    foreach(var name in key.dataNameReference()) {
                        var data = SyntaxElementBuilder.CreateDataName(name);
                        if (data == null) continue;
                        entry.TableOccurenceKeys.Add(data);
                        entry.TableOccurenceKeyDirections.Add(direction);
                    }
                }
            }

            var indexes = context.indexNameDefinition();
            if (indexes != null) {
                entry.IndexedBy = new List<IndexName>();
                foreach(var index in indexes) entry.IndexedBy.Add(SyntaxElementBuilder.CreateIndexName(index));
            }
        }

        private DataUsage CreateDataUsage(CodeElementsParser.UsageClauseContext context)
        {
            if (context.BINARY() != null
             || context.COMP()   != null || context.COMPUTATIONAL()   != null
             || context.COMP_4() != null || context.COMPUTATIONAL_4() != null) return DataUsage.Binary;
            if (context.COMP_1() != null || context.COMPUTATIONAL_1() != null) return DataUsage.FloatingPöint;
            if (context.COMP_2() != null || context.COMPUTATIONAL_2() != null) return DataUsage.LongFloatingPöint;
            if (context.PACKED_DECIMAL() != null
             || context.COMP_3() != null || context.COMPUTATIONAL_3() != null) return DataUsage.PackedDecimal;
            if (context.COMP_5() != null || context.COMPUTATIONAL_5() != null) return DataUsage.NativeBinary;
            if (context.DISPLAY_ARG() != null) return DataUsage.Display;
            if (context.DISPLAY_1()   != null) return DataUsage.DBCS;
            if (context.INDEX() != null) return DataUsage.Index;
            if (context.NATIONAL() != null) return DataUsage.National;
            if (context.OBJECT() != null || context.REFERENCE() != null) return DataUsage.ObjectReference;
            if (context.POINTER() != null) return DataUsage.Pointer;
            if (context.FUNCTION_POINTER()  != null) return DataUsage.FunctionPointer;
            if (context.PROCEDURE_POINTER() != null) return DataUsage.ProcedurePointer;
            return DataUsage.None;
        }

        private void UpdateDataDescriptionEntryWithValueClause(DataDescriptionEntry entry, CodeElementsParser.ValueClauseContext context)
        {
            if (context == null) return;
            var value = context.literal();
            if (value != null) entry.InitialValue = SyntaxElementBuilder.CreateLiteral(value); // format 1
            entry.IsInitialValueNull = (context.NULL() != null || context.NULLS() != null); // format 3
        }

        private void UpdateDataDescriptionEntryWithValueClauseForCondition(DataDescriptionEntry entry, CodeElementsParser.ValueClauseForConditionContext context)
        {
            if (context == null) return;
            var values = context.literal();
            if (values.Length > 0) entry.InitialValue = SyntaxElementBuilder.CreateLiteral(values[0]); // format 1 and 2
            if (values.Length > 1) entry.ThroughValue = SyntaxElementBuilder.CreateLiteral(values[1]); // format 2
        }

		  ////////////////////////
		 // PROCEDURE DIVISION //
		////////////////////////

		public override void EnterProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context) {
            Context = context;
            CodeElement = new ProcedureDivisionHeader();
		}
		public override void EnterUsingPhrase(CodeElementsParser.UsingPhraseContext context) {
			var inputs = CobolStatementsBuilder.CreateInputParameters(context.programInputParameters());
			((ProcedureDivisionHeader)CodeElement).InputParameters = inputs;
		}
		public override void EnterReturningPhrase(CodeElementsParser.ReturningPhraseContext context) {
			var receiving = CobolExpressionsBuilder.CreateStorageArea(context.programOutputParameter().storageArea2());
			((Returning)CodeElement).ReturningParameter = receiving;
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

        // -- Section --

        public override void EnterSectionHeader(CodeElementsParser.SectionHeaderContext context)
        {
            var sectionHeader = new SectionHeader();

            Token sectionName = ParseTreeUtils.GetFirstToken(context.sectionNameDefinition());
            if (sectionName != null)
            {
                sectionHeader.SectionName = new SectionName(sectionName);
            }

            Token priorityNumber = ParseTreeUtils.GetFirstToken(context.priorityNumber());
            if (priorityNumber != null)
            {
                sectionHeader.PriorityNumber = new SyntaxNumber(priorityNumber);
            }

            Context = context;
            CodeElement = sectionHeader;
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            Token paragraphName = ParseTreeUtils.GetFirstToken(context.paragraphNameDefinition());
            if (paragraphName != null)
            {
                paragraphHeader.ParagraphName = new ParagraphName(paragraphName);
            }

            Context = context;
            CodeElement = paragraphHeader;
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CodeElementsParser.SentenceEndContext context)
        {
            Context = context;
            CodeElement = new SentenceEnd();
        }





		  ////////////////
		 // PARAGRAPHS //
		////////////////


		 // STATEMENTS
		////////////////

		// --- ARITHMETIC STATEMENTS ---

		public override void EnterAddStatement(CodeElementsParser.AddStatementContext context) {
			Context = context;
			if(context.addSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddStatement(context.addSimple());
			} else
			if (context.addGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddGivingStatement(context.addGiving());
			} else
			if (context.addCorresponding() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddCorrespondingStatement(context.addCorresponding());
			}
		}
		public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context) {
			Context = context;
			CodeElement = new AddStatementEnd();
		}

		public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateComputeStatement(context);
		}
		public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context) {
			Context = context;
			CodeElement = new ComputeStatementEnd();
		}

		public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context) {
			Context = context;
			if (context.divideSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideStatement(context.divideSimple());
			} else
			if (context.divideGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideGivingStatement(context.divideGiving());
			} else
			if (context.divideRemainder() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideRemainderStatement(context.divideRemainder());
			}
		}
		public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context) {
			Context = context;
			CodeElement = new DivideStatementEnd();
		}

		public override void EnterMultiplyStatement(CodeElementsParser.MultiplyStatementContext context) {
			Context = context;
			if (context.multiplySimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateMultiplyStatement(context.multiplySimple());
			} else
			if (context.multiplyGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateMultiplyGivingStatement(context.multiplyGiving());
			}
		}
		public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context) {
			Context = context;
			CodeElement = new MultiplyStatementEnd();
		}

		public override void EnterSubtractStatement(CodeElementsParser.SubtractStatementContext context) {
			Context = context;
			if (context.subtractSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractStatement(context.subtractSimple());
			}
			else
			if (context.subtractGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractGivingStatement(context.subtractGiving());
			} else
			if (context.subtractCorresponding() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractCorrespondingStatement(context.subtractCorresponding());
			}
		}
		public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context) {
			Context = context;
			CodeElement = new SubtractStatementEnd();
		}



		// --- FILE STATEMENTS ---
		
		public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateOpenStatement(context);
		}

		public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateCloseStatement(context);
		}

		public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReadStatement(context);
		}

		public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context) {
			Context = context;
			CodeElement = new ReadStatementEnd();
		}

		public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateWriteStatement(context);
		}

		public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context) {
			Context = context;
			CodeElement = new WriteStatementEnd();
		}
		
		public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateRewriteStatement(context);
		}

		public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context) {
			Context = context;
			CodeElement = new RewriteStatementEnd();
		}



		// --- ACCEPT ---

		public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context) {
			Context = context;
			if (context.acceptDataTransfer() != null) {
				CodeElement = CobolStatementsBuilder.CreateAcceptDataTransferStatement(context.acceptDataTransfer());
			} else
			if(context.acceptSystemDateTime() != null) {
				CodeElement = CobolStatementsBuilder.CreateAcceptSystemDateTime(context.acceptSystemDateTime());
			}
		}

		// --- ALTER ---

		public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context) {            
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateAlterStatement(context);
		}

		// --- CALL ---

		public override void EnterCallStatement(CodeElementsParser.CallStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateCallStatement(context);
		}

		public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context) {
			Context = context;
			CodeElement = new CallStatementEnd();
		}

		// --- CANCEL ---

		public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateCancelStatement(context);
		}

		// --- CONTINUE ---

		public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context) {
			Context = context;
			CodeElement = new ContinueStatement();
		}

		// --- DELETE ---

		public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateDeleteStatement(context);
		}

		public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context) {
			Context = context;
			CodeElement = new DeleteStatementEnd();
		}

		// --- DISPLAY ---

		public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateDisplayStatement(context);
		}

		// --- ENTRY ---
		
		public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateEntryStatement(context);
		}

		// --- EVALUATE ---

		public override void EnterEvaluateStatement(CodeElementsParser.EvaluateStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateEvaluateStatement(context); ;
		}

		public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context) {
			Context = context;
			CodeElement = new EvaluateStatementEnd();
		}

		// --- EXEC ---

		public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateExecStatement(context);
		}

		// --- EXIT ---

		public override void EnterExitStatement(CodeElementsParser.ExitStatementContext context) {
			Context = context;
			CodeElement = new ExitStatement();
		}

		public override void EnterExitMethodStatement(CodeElementsParser.ExitMethodStatementContext context) {
			Context = context;
			CodeElement = new ExitMethodStatement();
		}

		public override void EnterExitProgramStatement(CodeElementsParser.ExitProgramStatementContext context) {
			Context = context;
			CodeElement = new ExitProgramStatement();
		}

		// --- GOBACK ---
				
		public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context) {
			Context = context;
			CodeElement = new GobackStatement();
		}

		// --- GOTO ---

		public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context) {
			Context = context;
			if (context.gotoSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateGotoStatement(context.gotoSimple());
			}
			if (context.gotoConditional() != null) {
				CodeElement = CobolStatementsBuilder.CreateGotoConditionalStatement(context.gotoConditional());
			}
		}



		// --- IF ---

		public override void EnterIfStatement(CodeElementsParser.IfStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateIfStatement(context);
		}
		public override void EnterElseCondition(CodeElementsParser.ElseConditionContext context) {
			Context = context;
			CodeElement = new ElseCondition();
		}
		public override void EnterNextSentenceStatement(CodeElementsParser.NextSentenceStatementContext context) {
			Context = context;
			CodeElement = new NextSentenceStatement();
		}
		public override void EnterIfStatementEnd(CodeElementsParser.IfStatementEndContext context) {
			Context = context;
			CodeElement = new IfStatementEnd();
		}



		// --- INITIALIZE ---

		public override void EnterInitializeStatement(CodeElementsParser.InitializeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateInitializeStatement(context);
		}

		// --- INSPECT ---

		public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateInspectStatement(context);
		}

		// --- INVOKE ---

		public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateInvokeStatement(context);
		}

		// --- MERGE ---

		public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateMergeStatement(context);
		}



		// --- PERFORM ---

		public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreatePerformStatement(context);
		}

		public override void EnterPerformProcedureStatement(CodeElementsParser.PerformProcedureStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreatePerformProcedureStatement(context);
		}

		public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context) {
			Context = context;
			CodeElement = new PerformStatementEnd();
		}



		// --- RELEASE	 ---

		public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context) {            
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReleaseStatement(context);
		}

		// --- RETURN ---

		public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReturnStatement(context);
		}
		public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context) {
			Context = context;
			CodeElement = new ReturnStatementEnd();
		}

		// --- SEARCH ---

		public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context) {
			Context = context;
			if (context.serialSearch() != null) {
				CodeElement = CobolStatementsBuilder.CreateSerialSearchStatement(context.serialSearch());
			} else
			if (context.binarySearch() != null) {
				CodeElement = CobolStatementsBuilder.CreateBinarySearchStatement(context.binarySearch());
			}
		}
		public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context) {
			Context = context;
			CodeElement = new SearchStatementEnd();
		}

		// --- SORT ---

		public override void EnterSortStatement(CodeElementsParser.SortStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateSortStatement(context);
		}

		// --- START ---

		public override void EnterStartStatement(CodeElementsParser.StartStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStartStatement(context);
		}
		public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context) {
			Context = context;
			CodeElement = new StartStatementEnd();
		}

		// --- STOP ---

		public override void EnterStopStatement(CodeElementsParser.StopStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStopStatement(context);
		}

		// --- STRING ---

		public override void EnterStringStatement(CodeElementsParser.StringStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStringStatement(context);
		}
		public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context) {
			Context = context;
			CodeElement = new StringStatementEnd();
		}

		// --- UNSTRING ---

		public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateUnstringStatement(context);
		}
		public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context) {
			Context = context;
			CodeElement = new UnstringStatementEnd();
		}

		// --- XML STATEMENTS ---

		public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateXmlGenerateStatement(context);
		}

		public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateXmlParseStatement(context);
		}

		public override void EnterXmlStatementEnd(CodeElementsParser.XmlStatementEndContext context) {
			Context = context;
			CodeElement = new XmlStatementEnd();
		}

		// --- SET STATEMENT ---

		public override void EnterSetStatement(CodeElementsParser.SetStatementContext context) {
			if (context.setStatementForAssignation() != null) {
				Context = context.setStatementForAssignation();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForAssignation(context.setStatementForAssignation());
			} else
			if (context.setStatementForIndexes() != null) {
				Context = context.setStatementForIndexes();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForIndexes(context.setStatementForIndexes());
			} else
			if (context.setStatementForSwitches() != null) {
				Context = context.setStatementForSwitches();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForSwitches(context.setStatementForSwitches());
			} else
			if (context.setStatementForConditions() != null) {
				Context = context.setStatementForConditions();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForConditions(context.setStatementForConditions());
			}
		}


//TODO
        public override void EnterMoveStatement(CodeElementsParser.MoveStatementContext context)
        {
            Context = context;
            CodeElement = new StatementsBuilder().CreateMoveStatement(context);
        }



        public override void EnterUseStatement(CodeElementsParser.UseStatementContext context)
        {
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


		  //////////////////////////
		 // STATEMENT CONDITIONS //
		//////////////////////////

		public override void EnterAtEndCondition(CodeElementsParser.AtEndConditionContext context) {
			Context = context;
			CodeElement = new AtEndCondition();
		}
		public override void EnterNotAtEndCondition(CodeElementsParser.NotAtEndConditionContext context) {
			Context = context;
			CodeElement = new NotAtEndCondition();
		}

		public override void EnterAtEndOfPageCondition(CodeElementsParser.AtEndOfPageConditionContext context) {
			Context = context;
			CodeElement = new AtEndOfPageCondition();
		}
		public override void EnterNotAtEndOfPageCondition(CodeElementsParser.NotAtEndOfPageConditionContext context) {
			Context = context;
			CodeElement = new NotAtEndOfPageCondition();
		}

		public override void EnterWhenCondition(CodeElementsParser.WhenConditionContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateWhenCondition(context);
		}
		public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context) {
			Context = context;
			CodeElement = new WhenOtherCondition();
		}

		public override void EnterWhenSearchCondition(CodeElementsParser.WhenSearchConditionContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateWhenSearchCondition(context);
		}

		public override void EnterInvalidKeyCondition(CodeElementsParser.InvalidKeyConditionContext context) {
			Context = context;
			CodeElement = new InvalidKeyCondition();
		}
		public override void EnterNotInvalidKeyCondition(CodeElementsParser.NotInvalidKeyConditionContext context) {
			Context = context;
			CodeElement = new NotInvalidKeyCondition();
		}

		public override void EnterOnExceptionCondition(CodeElementsParser.OnExceptionConditionContext context) {
			Context = context;
			CodeElement = new OnExceptionCondition();
		}

		public override void EnterNotOnExceptionCondition(CodeElementsParser.NotOnExceptionConditionContext context) {
			Context = context;
			CodeElement = new NotOnExceptionCondition();
		}

		public override void EnterOnOverflowCondition(CodeElementsParser.OnOverflowConditionContext context) {
			Context = context;
			CodeElement = new OnOverflowCondition();
		}
		public override void EnterNotOnOverflowCondition(CodeElementsParser.NotOnOverflowConditionContext context) {
			Context = context;
			CodeElement = new NotOnOverflowCondition();
		}

		public override void EnterOnSizeErrorCondition(CodeElementsParser.OnSizeErrorConditionContext context) {
			Context = context;
			CodeElement = new OnSizeErrorCondition();
		}
		public override void EnterNotOnSizeErrorCondition(CodeElementsParser.NotOnSizeErrorConditionContext context) {
			Context = context;
			CodeElement = new NotOnSizeErrorCondition();
		}


/*
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
*/

// [TYPECOBOL]
		public override void EnterFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context) {
			var visibility = context.PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;
			QualifiedName name = null;
			if (context.UserDefinedWord() != null) {
				var token = ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord());
				name = new URI(token.Text);
			}
			Context = context;
			CodeElement = new FunctionDeclarationHeader(name, visibility);
		}
		public override void EnterInputPhrase(CodeElementsParser.InputPhraseContext context) {
			var profile = new FunctionDeclarationProfile(CodeElement as ProcedureDivisionHeader);
			profile.InputParameters = CobolStatementsBuilder.CreateInputParameters(context.programInputParameters());
			CodeElement = profile;
		}
		public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
			var div = CodeElement as ProcedureDivisionHeader;
			if (div != null) CodeElement = new FunctionDeclarationProfile(div);
			var profile = (FunctionDeclarationProfile)CodeElement;
			foreach(var output in context.storageArea2())
				profile.OutputParameters.Add(CobolExpressionsBuilder.CreateStorageArea(output));
		}
		public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
			Context = context;
			CodeElement = new FunctionDeclarationEnd();
		}
// [/TYPECOBOL]

    }
}
