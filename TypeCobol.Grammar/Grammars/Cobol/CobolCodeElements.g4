// IBM Enterprise Cobol 5.1 for zOS

// -----------------------------------------------------------------------
// Grammar for the FIRST step of the Cobol parser : flat view of the Cobol
// syntax, useful for incremental parsing. The goal here is to produce a 
// LIST of syntax nodes instead of TREE (because it is easier to reuse).
// -----------------------------------------------------------------------

grammar CobolCodeElements;

// importing boths grammar raises the following error message:
// "repeated grammar prequel spec (options, tokens, or import); please merge"
// so we switch between grammars depending wether or not we want TypeCobol
//import CobolCodeElementsNew;
import TypeCobolCodeElements;

// --- Starting parser rule for PHASE 1 of parsing ---

cobolCodeElements: codeElement* EOF;