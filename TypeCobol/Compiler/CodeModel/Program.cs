using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CodeModel
{
    /// <summary>
    /// Node that can contains a ProcedureCall (private or public)
    /// </summary>
    public interface IProcCaller {
        /// <summary>
        /// The Dictionary of all Procedure Style Calls performed by this Program.
        /// Dictionary<hash:string, Tuple<qualified_items:IList<SymbolReference>, proc:TypeCobol.Compiler.Nodes.ProcedureStyleCall>>
        /// This values is calculated by the Generator during the Qualifier Action.
        /// </summary>
        Dictionary<string, Tuple<IList<SymbolReference>, ProcedureStyleCall>> ProcStyleCalls { get; set; }
        
        string Name { get; }
    }

    /// <summary>
    /// A COBOL source program is a syntactically correct set of COBOL statements.
    /// </summary>
    public class Program : GenericNode<ProgramIdentification>, CodeElementHolder<ProgramIdentification>, IProcCaller, IDocumentable
    {
        public Program(ProgramIdentification codeElement) : base(codeElement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public override string ID
        {
            get { return "program"; }
        }

        public override string Name
        {
            get { return Identification != null ? (Identification.ProgramName.Name != null ? Identification.ProgramName.Name : ID) : ID; }
        }

        //TODO: As to change in the future when implementing the full namespace functionnality.
        public string Namespace { get { return Identification != null ? Identification.ProgramName.Name : null; } }

        /// <summary>
        /// True if the current program is contained in another program.
        /// </summary>
        public virtual bool IsNested => false;
        public virtual bool IsStacked => false;
        public virtual bool IsMainProgram => true;

        // -- IDENTIFICATION DIVISION --


        private ProgramIdentification _identificaton;
        /// <summary>
        /// Program name, Initial / 
        /// </summary>
        public ProgramIdentification Identification
        {
            get
            {
                if (_identificaton != null) return _identificaton;
                _identificaton = this.CodeElement;
                return _identificaton;
            }
        }

        // -- ENVIRONMENT DIVISION --

        /// <summary>
        /// The FILE-CONTROL paragraph associates each file in the COBOL program with
        /// an external data set, and specifies file organization, access mode, and other
        /// information.
        /// </summary>
        public IDictionary<SymbolDefinition, FileControlEntry> FileConnectors { get; set; }

        /// <summary>
        /// The I-O-CONTROL paragraph specifies when checkpoints are to be taken 
        /// and the storage areas to be shared by different files. 
        /// Specifies information needed for efficient transmission of data between the
        /// external data set and the COBOL program.
        /// This paragraph is optional in a COBOL program.
        /// </summary>
        public IList<IOControlEntry> IOControlEntries { get; set; }

        // -- DATA DIVISION --

        // The DATA DIVISION of a COBOL source program describes, in a structured manner, all the data to be processed by the program. 

        /// <summary>
        /// The FILE SECTION defines the structure of data files.
        /// file-description-entry 
        /// Provides information about the physical structure and identification of a file, and gives the record-names associated with that file. 
        /// A single run-unit-level file connector is shared by all programs and methods that contain a declaration of a given external file.
        /// record-description-entry 
        /// A set of data description entries that describe the particular records contained within a particular file. 
        /// More than one record description entry can be specified; each is an alternative description of the same record storage area.
        /// Data areas described in the FILE SECTION are not available for processing unless the file that contains the data area is open.
        /// </summary>
        public IDictionary<SymbolDefinition, FileDescription> FileDescriptions { get; set; }


        /// <summary>
        /// The Dictionary of all Procedure Style Calls performed by this Program.
        /// Dictionary<hash:string, Tuple<qualified_items:IList<SymbolReference>, proc:TypeCobol.Compiler.Nodes.ProcedureStyleCall>>
        /// This values is calculated by the Generator during the Qualifier Action.
        /// </summary>
        public Dictionary<string, Tuple<IList<SymbolReference>, TypeCobol.Compiler.Nodes.ProcedureStyleCall>> ProcStyleCalls
        {
            get;
            set;
        }


        // -- PROCEDURE DIVISION --


        // -- NESTED PROGRAMs --

        /// <summary>
        /// A nested program is a program that is contained in another program.
        /// </summary>
        public IList<NestedProgram> NestedPrograms { get; set; }
    }

    /// <summary>
    /// Outermost program of a compilation unit.
    /// </summary>
    public class SourceProgram: Program {

		public SourceProgram(SymbolTable EnclosingScope, ProgramIdentification codeElement) : base(codeElement)
		{
			SymbolTable = new SymbolTable(new SymbolTable(EnclosingScope, SymbolTable.Scope.Declarations), SymbolTable.Scope.Program);
        }

        // -- ENVIRONMENT DIVISION --

        /// <summary>
        /// The SOURCE-COMPUTER paragraph describes the computer on which the source
        /// text is to be compiled.
        /// </summary>
        public SourceComputerParagraph SourceComputerProperties { get; set; }

        /// <summary>
        /// /// <summary>
        /// The OBJECT-COMPUTER paragraph specifies the system for which the object
        /// program is designated.
        /// </summary>
        public ObjectComputerParagraph ObjectComputerProperties { get; set; }

        /// <summary>
        /// The SPECIAL-NAMES paragraph:
        /// - Relates IBM-specified environment-names to user-defined mnemonic-names
        /// - Relates alphabet-names to character sets or collating sequences
        /// - Specifies symbolic characters
        /// - Relates class names to sets of characters
        /// - Specifies one or more currency sign values and defines a picture symbol to
        ///   represent each currency sign value in PICTURE clauses
        /// - Specifies that the functions of the comma and decimal point are to be
        ///   interchanged in PICTURE clauses and numeric literals
        /// - Relates xml-schema-names to ddnames or environment variable names
        ///   identifying files containing XML schemas
        /// </summary>
        public SpecialNamesParagraph SpecialNamesDefinitions { get; set; }

        /// <summary>
        /// The REPOSITORY paragraph is used in a program or class definition to identify all
        /// the object-oriented classes that are intended to be referenced in that program or
        /// class definition. Optionally, the REPOSITORY paragraph defines associations
        /// between class-names and external class-names.
        /// </summary>
        public RepositoryParagraph RepositoryOfClassNames { get; set; }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    
    /// <summary>
    /// A nested program is a program that is contained in another program.
    /// Nested programs can be directly or indirectly contained in the containing program.     
    /// Nested programs are not supported for programs compiled with the THREAD option
    /// </summary>
	public class NestedProgram: Program {
		public NestedProgram(Program containingProgram, ProgramIdentification codeElement) : base(codeElement) {
			ContainingProgram = containingProgram;
            SymbolTable globalTable = containingProgram.SymbolTable.GetTableFromScope(SymbolTable.Scope.Global); //Get Parent Global Table
            var globalNestedSymbolTable = new SymbolTable(globalTable, SymbolTable.Scope.Global); //Create a new Global symbol table for this nested program and his childrens programs
            //It allows to goes down with global variable and ensure that nested global variables and types are not accessible to parent program. 

            SymbolTable = new SymbolTable(globalNestedSymbolTable, SymbolTable.Scope.Declarations);
            SymbolTable = new SymbolTable(SymbolTable, SymbolTable.Scope.Program);
		}

        public override bool IsNested => true;
        public override bool IsMainProgram => false;

        /// <summary>A nested program is a program that is contained in another program.</summary>
		public Program ContainingProgram { get; private set; }
        
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class StackedProgram: SourceProgram {
        public StackedProgram(SymbolTable EnclosingScope, ProgramIdentification codeElement) : base(EnclosingScope, codeElement)
        {
        }

        public override bool IsStacked => true;
        public override bool IsMainProgram => false;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
