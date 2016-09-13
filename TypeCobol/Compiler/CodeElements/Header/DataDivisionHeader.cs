using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The DATA DIVISION of a COBOL source program describes, in a structured manner, all the data to be processed by the program. 
    /// </summary>
    public class DataDivisionHeader : CodeElement
    {
        public DataDivisionHeader() : base(CodeElementType.DataDivisionHeader)
        { }
    }
}
