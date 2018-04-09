grammar TypeCobolProgram;

import CobolProgramClass;

// TCRFUN_LIBRARY_COPY
programAttributes: ProgramIdentification LibraryCopy?;

// TCRFUN_DECLARATION_AS_NESTED_PROGRAM
procedureDivision:
	ProcedureDivisionHeader declaratives? (functionDeclaration | section)*;

// - custom header
// TCRFUN_DECLARATION_NO_IDENTIFICATION_DIVISION
// TCRFUN_DECLARATION_NO_ENVIRONMENT_DIVISION
// TCRFUN_MANDATORY_END_DECLARE
functionDeclaration:
	FunctionDeclarationHeader
	dataDivision?
	functionProcedureDivision?
	FunctionDeclarationEnd
	;

// TCRFUN_DECLARATION_NO_DECLARATIVES
functionProcedureDivision:
	ProcedureDivisionHeader section*;

procedureStyleCallConditional:
	ProcedureStyleCall
	CallStatementEnd?;

//GLOBALSS_POSITION Global-storage section is declared after file section and before working-storage section
dataDivision:
	DataDivisionHeader 
	fileSection? 
	globalStorageSection?
	workingStorageSection? 
	localStorageSection? 
	linkageSection?;


globalStorageSection:
	GlobalStorageSectionHeader 
	(dataDefinitionEntry SentenceEnd?)*;
