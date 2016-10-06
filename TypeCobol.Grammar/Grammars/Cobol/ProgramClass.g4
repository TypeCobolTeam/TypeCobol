grammar ProgramClass;

//import CobolProgramClass;
import TypeCobolProgram;

// We don't know what is the starting rule of the included grammar,
// so just add a dummy rule. This dummy rule is named with the hope
// it won't conflict with any other rule of the included grammar.
// It is however important this dummy rule comes LAST in the file,
// so it won't be considered the starting rule of the end grammar.
dummyPhase2Rule: EOF;