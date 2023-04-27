namespace TypeCobol
{
    public enum ExecutionStep
    {
        Scanner = 0,        //This will only run the scanner to analyze the tokens
        Preprocessor,       //This will only analyze the preprocessor instructions
        CodeElement,        //This will only create the code elements from the tokens
        AST,                //This will allow to build the abstract syntax tree
        SemanticCrossCheck, //This will allow the parser to go to fully semantic check phase
        CodeAnalysis,       //This will run code analysis against quality rules set
        Generate            //This will generate an output file containing generated Cobol
    }
}
