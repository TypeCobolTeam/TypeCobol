﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    /// <summary>
    /// A COBOL source program is a syntactically correct set of COBOL statements.
    /// </summary>
    public abstract class Program
    {
        public Program() {
            SyntaxTree = new SyntaxTree();
        }

        /// <summary>
        /// True if the current program is contained in another program.
        /// </summary>
        public bool IsNested { get; protected set; }

        // -- IDENTIFICATION DIVISION --

        /// <summary>
        /// Program name, Initial / 
        /// </summary>
        public ProgramIdentification Identification { get; set; }        

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
        /// Table of symbols defined in this program.
        /// Includes WORKING-STORAGE, LOCAL-STORAGE and LINKAGE data.
        /// </summary>
        public SymbolTable SymbolTable;

		public SymbolTable CurrentTable {
			get { return SyntaxTree.CurrentNode.SymbolTable; }
		}

        /// <summary>
        /// Abstract Syntax Tree of this program.
        /// Syntax trees of nested programs (if any) are nodes/subtrees of this one.
        /// </summary>
        public SyntaxTree SyntaxTree;

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

		public SourceProgram(SymbolTable EnclosingScope) {
			IsNested = false;
			SymbolTable = new SymbolTable(EnclosingScope);
			SyntaxTree.Root.SymbolTable = SymbolTable;
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
    }
    
    /// <summary>
    /// A nested program is a program that is contained in another program.
    /// Nested programs can be directly or indirectly contained in the containing program.     
    /// Nested programs are not supported for programs compiled with the THREAD option
    /// </summary>
	public class NestedProgram: Program {
		public NestedProgram(Program containingProgram) {
			IsNested = true;
			ContainingProgram = containingProgram;
			SymbolTable = new SymbolTable(containingProgram.SymbolTable);
			SyntaxTree.Root.SymbolTable = SymbolTable;
		}

        /// <summary>A nested program is a program that is contained in another program.</summary>
		public Program ContainingProgram { get; private set; }
	}
}
