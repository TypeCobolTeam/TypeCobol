using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol
{
    public enum ExecutionStep
    {
        Scanner = 0,    //This will only run the scanner to analyse the tokens
        Preprocessor,   //This will only analyse the preprocessor instructions
        SyntaxCheck,    //This will only check the syntax
        SemanticCheck,  //This will allow to do a semantic phase but not complete
        CrossCheck,     //This will allow the parser to go to fully semantic check phase
        Generate,       //This will generate an output file containing generated Cobol
    }
}
