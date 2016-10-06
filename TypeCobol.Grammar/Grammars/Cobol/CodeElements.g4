// IBM Enterprise Cobol 5.1 for zOS

// -----------------------------------------------------------------------
// Grammar for the FIRST step of the Cobol parser : flat view of the Cobol
// syntax, useful for incremental parsing. The goal here is to produce a 
// LIST of syntax nodes instead of TREE (because it is easier to reuse).
// -----------------------------------------------------------------------

grammar CodeElements;

// importing boths grammar raises the following error message:
// "repeated grammar prequel spec (options, tokens, or import); please merge"
// so we switch between grammars depending wether or not we want TypeCobol
//import CobolCodeElements;
import TypeCobolCodeElements;

// We don't know what is the starting rule of the included grammar,
// so just add a dummy rule. This dummy rule is named with the hope
// it won't conflict with any other rule of the included grammar.
// It is however important this dummy rule comes LAST in the file,
// so it won't be considered the starting rule of the end grammar.
dummyPhase1Rule: EOF;