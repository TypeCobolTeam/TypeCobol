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
	functionProcedureDivision?
	FunctionDeclarationEnd
	;

functionProcedureDivision:
	ProcedureDivisionHeader section*;
