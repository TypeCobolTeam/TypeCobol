namespace TypeCobol
{
    public enum ExecutionStep
    {
        Scanner = 0,    //This will only run the scanner to analyse the tokens
        Preprocessor,   //This will only analyse the preprocessor instructions
        SyntaxCheck,    //This will only check the syntax
        SemanticCheck,  //This will allow to do a semantic phase but not complete
        CrossCheck,     //This will allow the parser to go to fully semantic check phase
        QualityCheck,   //This will run code analysis against quality rules set
        Generate        //This will generate an output file containing generated Cobol
    }
}
