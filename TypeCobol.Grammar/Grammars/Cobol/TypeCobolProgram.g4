grammar TypeCobolProgram;

import CobolProgramClass;

procedureDivision:
	ProcedureDivisionHeader declaratives? (section | functionDeclaration)*;

// - custom header
// - no ENVIRONMENT DIVISION
// - mandatory END-DECLARE
functionDeclaration:
	FunctionDeclarationHeader
	functionDataDivision?
	functionProcedureDivision?
	FunctionDeclarationEnd
	;

// no FILE SECTION
functionDataDivision:
	DataDivisionHeader workingStorageSection? localStorageSection? linkageSection?;

// - custom header
// - no DECLARATIVES
functionProcedureDivision:
	FunctionProcedureDivisionHeader section*;
