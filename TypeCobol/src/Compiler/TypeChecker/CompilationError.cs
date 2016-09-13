
using System;
namespace TypeCobol.Compiler.TypeChecker
{
    /// <summary>
    /// Temporary diagnostic class for the demo
    /// </summary>
    public class CompilationError
    {
        public int LineNumber { get; set; }
        public int StartColumn { get; set; }
        public int EndColumn { get; set; }

        public string Message { get; set; }
    }
}
