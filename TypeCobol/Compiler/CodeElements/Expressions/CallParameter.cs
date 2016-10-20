using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Represents a call instruction to a program, method or function
    /// </summary>
    public class CallSite
    {
        /// <summary>
        /// Name of the called program, method, or function
        /// </summary>
        public SymbolReference CallTarget { get; set; }

        /// <summary>
        /// Parameters shared with the called program, method, or function
        /// </summary>
        public CallSiteParameter[] Parameters { get; set; }
    }

    /// <summary>
    /// Parameters shared in a call instruction to program, method, or function
    /// </summary>
    public class CallSiteParameter
    {
        /// <summary>
        /// Technique used for sharing the parameter between the caller and the callee
        /// </summary>
        public ParameterSharingMode SharingMode { get; set; }

        /// <summary>
        /// Reference to a storage area, literal value or expression
        /// </summary>
        public VariableOrExpression StorageAreaOrValue { get; set; }
    }
    
    /// <summary>
    /// Technique used for sharing one parameter between the caller and the callee
    /// </summary>
    public enum ParameterSharingMode
    {
        /// <summary>
        /// If the BY REFERENCE phrase is either specified or implied for a parameter, 
        /// the corresponding data item in the calling program occupies the same storage area 
        /// as the data item in the called program. 
        /// </summary>
        ByReference,
        /// <summary>
        /// If the BY CONTENT phrase is specified or implied for a parameter, the sender of the
        /// parameter creates a local copy of the data to another storage area before sharing it
        /// by reference with the receiver of the data. 
        /// Changes to the parameter in the called program do not affect the corresponding argument
        /// in the calling program. 
        /// </summary>
        ByContent,
        /// <summary>
        /// If the BY VALUE phrase is specified or implied for an argument, the value of the argument
        /// is passed, not a reference to the sending data item. The called program can modify 
        /// the formal parameter that corresponds to the BY VALUE argument, but any such changes do
        /// not affect the argument in the called program.
        /// Although BY VALUE arguments are primarily intended for communication with non-COBOL programs 
        /// (such as C), they can also be used for COBOL-to-COBOL invocations. In this case, BY VALUE
        /// must be specified or implied for both the argument in the CALL USING phrase and the 
        /// corresponding formal parameter in the PROCEDURE DIVISION USING phrase. 
        /// </summary>
        ByValue
    }

    /// <summary>
    /// Represents an entry point in a program, method or function which can be the target of a call
    /// </summary>
    public class CallTarget
    {
        /// <summary>
        /// Name of the entry point
        /// </summary>
        public SymbolDefinition Name { get; set; }

        /// <summary>
        /// Parameters shared with the calling program, method, or function
        /// </summary>
        public CallTargetParameter[] Parameters { get; set; }
    }

    /// <summary>
    /// Parameters shared with a caller at the entry point of a program, method, or function
    /// </summary>
    public class CallTargetParameter
    {
        /// <summary>
        /// Technique used for sharing the parameter between the caller and the callee
        /// </summary>
        public ParameterSharingMode SharingMode { get; set; }

        /// <summary>
        /// Direction in which the data flows to or from a program, method, or function entry point
        /// </summary>
        public ParameterPassingDirection PassingDirection { get; set; }

        /// <summary>
        /// Reference to a storage area shared with the caller
        /// </summary>
        public StorageArea StorageArea { get; set; }
    }

    /// <summary>
    /// Direction in which the data flows to or from a program, method, or function entry point.
    /// In : data received by the entry point from its caller, Out : data returned b ythe entry point ot its caller.
    /// </summary>
    public enum ParameterPassingDirection
    {
        In,
        InOut,
        Out
    }
}
