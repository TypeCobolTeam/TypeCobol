using System;
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
        public Program()
        { }

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
        public IDictionary<FileName, FileControlEntry> FileConnectors { get; set; }

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
        public IDictionary<FileName, FileDescription> FileDescriptions { get; set; }

        /// <summary>
        /// The WORKING-STORAGE SECTION describes data records that are not part of data files but are developed and processed by a program or method. 
        /// The WORKING-STORAGE SECTION also describes data items whose values are assigned in the source program or method and do not change during execution of the object program.
        /// The WORKING-STORAGE SECTION for programs (and methods) can also describe external data records, which are shared by programs and methods throughout the run unit.
        /// </summary>
        public IList<DataDescriptionEntry> WorkingStorageData { get; set; }

        /// <summary>
        /// The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
        /// On each invocation, data items defined in the LOCAL-STORAGE SECTION are reallocated. 
        /// Each data item that has a VALUE clause is initialized to the value specified in that clause.
        /// For nested programs, data items defined in the LOCAL-STORAGE SECTION are allocated upon each invocation of the containing outermost program. 
        /// However, each data item is reinitialized to the value specified in its VALUE clause each time the nested program is invoked.
        /// </summary>
        public IList<DataDescriptionEntry> LocalStorageData { get; set; }

        /// <summary>
        /// The LINKAGE SECTION describes data made available from another program or method. 
        /// Record description entries and data item description entries in the LINKAGE SECTION provide names and descriptions, 
        /// but storage within the program or method is not reserved because the data area exists elsewhere.
        /// Data items defined in the LINKAGE SECTION of the called program or invoked
        /// method can be referenced within the PROCEDURE DIVISION of that program if
        /// and only if they satisfy one of the conditions as listed in the topic.
        /// - They are operands of the USING phrase of the PROCEDURE DIVISION header
        ///   or the ENTRY statement.
        /// - They are operands of SET ADDRESS OF, CALL ... BY REFERENCE ADDRESS
        ///   OF, or INVOKE ... BY REFERENCE ADDRESS OF.
        /// - They are defined with a REDEFINES or RENAMES clause, the object of which
        ///   satisfies the above conditions.
        /// - They are items subordinate to any item that satisfies the condition in the rules
        ///   above.
        /// - They are condition-names or index-names associated with data items that satisfy
        ///   any of the above conditions.
        /// </summary>
        public IList<DataDescriptionEntry> LinkageData { get; set; }

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
    public class SourceProgram : Program
    {
        public SourceProgram()
        {
            IsNested = false;
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
    public class NestedProgram : Program 
    {
        public NestedProgram(Program containingProgram)
        {
            IsNested = true;
            ContainingProgram = containingProgram;
        }

        /// <summary>
        /// A nested program is a program that is contained in another program.
        /// </summary>
        public Program ContainingProgram { get; private set; }
    }
}
