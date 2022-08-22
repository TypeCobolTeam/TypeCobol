namespace TypeCobol.Compiler.CodeElements {

    public class ProgramIdentification : NamedCodeElement
    {
        public ProgramIdentification() : base(CodeElementType.ProgramIdentification) { }

        /// <summary>
        /// program-name
        /// A user-defined word or alphanumeric literal, but not a figurative constant,
        /// that identifies your program. 
        /// It must follow the following rules of
        /// formation, depending on the setting of the PGMNAME compiler option:
        /// PGMNAME(COMPAT)
        /// The name can be up to 30 characters in length.
        /// Only the hyphen, underscore, digits 0-9, and alphabetic characters
        /// are allowed in the name when it is specified as a user-defined
        /// word.
        /// At least one character must be alphabetic.
        /// The hyphen cannot be the first or last character.
        /// If program-name is an alphanumeric literal, the rules for the name
        /// are the same except that the extension characters $, #, and @ can be
        /// included in the name of the outermost program and the
        /// underscore can be the first character.
        /// A program-name can be referenced only by the CALL statement, the CANCEL statement, 
        /// the SET statement, or the END PROGRAM marker.
        /// A separately compiled program and all of its directly and indirectly contained programs 
        /// must have unique program-names within that separately compiled program.
        /// </summary>
        public SymbolDefinition ProgramName { get; set; }

        public override string Name => ProgramName?.Name;

        /// <summary>
        /// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
        /// The optional paragraphs are: 
        /// AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY
        /// </summary>
        public AuthoringProperties AuthoringProperties { get; set; }
        
        /// <summary>
        /// INITIAL
        /// Specifies that when program-name is called, program-name and any programs
        /// contained (nested) within it are placed in their initial state. The initial
        /// attribute is not supported for programs compiled with the THREAD
        /// option.
        /// </summary>
        public SyntaxProperty<bool> Initial { get; set; }
        public bool IsInitial { get { return Initial != null && Initial.Value; } }

        // -- ONLY IN OUTERMOST PROGRAM --

        /// <summary>
        /// RECURSIVE
        /// An optional clause that allows COBOL programs to be recursively
        /// reentered.
        /// You can specify the RECURSIVE clause only on the outermost program of
        /// a compilation unit. Recursive programs cannot contain nested
        /// subprograms.
        /// If the RECURSIVE clause is specified, program-name can be recursively
        /// reentered while a previous invocation is still active. If the RECURSIVE
        /// clause is not specified, an active program cannot be recursively reentered.
        /// The RECURSIVE clause is required for programs compiled with the
        /// THREAD option.
        /// </summary>
        public SyntaxProperty<bool> Recursive { get; set; }
        public bool IsRecursive { get { return Recursive != null && Recursive.Value; } }

        // -- ONLY IN NESTED PROGRAMS --

        /// <summary>
        /// COMMON
        /// Specifies that the program named by program-name is contained (that is,
        /// nested) within another program and can be called from siblings of the
        /// common program and programs contained within them. The COMMON
        /// clause can be used only in nested programs.
        /// </summary>
        public SyntaxProperty<bool> Common { get; set; }
        public bool IsCommon { get { return Common != null && Common.Value; } }

        public override string ToString() {
			var sb = new System.Text.StringBuilder(base.ToString());
			sb.AppendLine("- ProgramName = " + ProgramName);
			sb.AppendLine("- IsInitial = " + IsInitial);
			sb.AppendLine("- IsRecursive = " + IsRecursive);
			sb.AppendLine("- IsCommon = " + IsCommon);
			if (AuthoringProperties != null) {
				sb.Append(AuthoringProperties);
			}
			return sb.ToString();
		}
    }



public class LibraryCopyCodeElement: CodeElement {
	public LibraryCopyCodeElement(): base(CodeElementType.LibraryCopy) { }
	public ExternalName Name { get; set; }
}

}

