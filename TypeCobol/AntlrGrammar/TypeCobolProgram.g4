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

functionProcedureDivision:
	ProcedureDivisionHeader 
	declaratives? 
	section*;

procedureStyleCallConditional:
	ProcedureStyleCall
	CallStatementEnd?;