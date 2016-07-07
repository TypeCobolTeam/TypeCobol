grammar TypeCobolProgram;

import CobolProgramClass;

procedureDivision:
	ProcedureDivisionHeader declaratives? (functionDeclaration | section)*;

// - custom header
// - no ENVIRONMENT DIVISION
// - mandatory END-DECLARE
functionDeclaration:
	FunctionDeclarationHeader
	dataDivision?
	ProcedureDivisionHeader
		section*
	FunctionDeclarationEnd
	;

// no FILE SECTION
functionDataDivision:
	DataDivisionHeader workingStorageSection? localStorageSection? linkageSection?;
