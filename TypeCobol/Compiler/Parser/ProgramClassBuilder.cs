using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Build a Program or Class object while visiting its parse tree
    /// </summary>
    public class ProgramClassBuilder : CobolProgramClassBaseListener
    {
        /// <summary>
        /// Program object resulting of the visit the parse tree
        /// </summary>
        public Program Program { get; private set; }

        // Programs can be nested => track current programs being analyzed
        private Stack<Program> programsStack = null;

        /// <summary>
        /// Class object resulting of the visit the parse tree
        /// </summary>
        public Class Class { get; private set; }

        /// <summary>
        /// List of syntax diagnostics gathered while transversing the parse tree
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }

        /// <summary>
        /// Initialization code run before parsing each new Program or Class
        /// </summary>
        public override void EnterCobolCompilationUnit(CobolProgramClassParser.CobolCompilationUnitContext context)
        {
            Program = null;
            Class = null;
            Diagnostics = new List<Diagnostic>();
        }

        public override void EnterCobolProgram(CobolProgramClassParser.CobolProgramContext context)
        {
            Program currentProgram = null;
            if (Program == null)
            {
                currentProgram = new SourceProgram();
                Program = currentProgram;
                programsStack = new Stack<Program>();
                programsStack.Push(Program);
            }
            else
            {
                currentProgram = new NestedProgram(programsStack.Peek());
                programsStack.Push(currentProgram);
            }

            currentProgram.Identification = (ProgramIdentification)context.ProgramIdentification().Symbol;            
        }

        public override void ExitCobolProgram(CobolProgramClassParser.CobolProgramContext context)
        {
            if(programsStack != null)
            {
                programsStack.Pop();
            }
        }
    }
}
