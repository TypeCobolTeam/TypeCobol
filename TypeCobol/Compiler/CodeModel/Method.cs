using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    /// <summary>
    /// A COBOL method definition describes a method. 
    /// You can specify method definitions only within the factory paragraph and the object paragraph of a class definition.
    /// Methods defined in an object definition are instance methods. An instance method in
    /// a given class can access:
    /// - Data defined in the DATA DIVISION of the object paragraph of that class (instance data)
    /// - Data defined in the DATA DIVISION of that instance method (method data)
    /// An instance method cannot directly access instance data defined in a parent class,
    /// factory data defined in its own class, or method data defined in another method of
    /// its class. It must invoke a method to access such data.
    /// Methods defined in a factory definition are factory methods. A factory method in a
    /// given class can access:
    /// - Data defined in the DATA DIVISION of the factory paragraph of that class (factory data)
    /// - Data defined in the DATA DIVISION of that factory method (method data)
    /// A factory method cannot directly access factory data defined in a parent class,
    /// instance data defined in its own class, or method data defined in another method
    /// of its class. It must invoke a method to access such data.
    /// Methods can be invoked from COBOL programs and methods, and they can be
    /// invoked from Java programs. A method can execute an INVOKE statement that
    /// directly or indirectly invokes itself. Therefore, COBOL methods are implicitly
    /// recursive (unlike COBOL programs, which support recursion only if the
    /// RECURSIVE attribute is specified in the program-ID paragraph.)
    /// </summary>
    public class Method
    {
        // -- IDENTIFICATION DIVISION --
        
        /// <summary>
        /// Method name, Authoring properties
        /// </summary>
        public MethodIdentification Identification { get; set; }

        // -- ENVIRONMENT DIVISION --

        /// <summary>
        /// The FILE-CONTROL paragraph associates each file in the COBOL program with
        /// an external data set, and specifies file organization, access mode, and other
        /// information.
        /// </summary>
        public IDictionary<SymbolDefinition, FileControlEntry> FileConnectors { get; set; }

        /// <summary>
        /// The I-O-CONTROL paragraph of the input-output section specifies when checkpoints 
        /// are to be taken and the storage areas to be shared by different files. 
        /// Specifies information needed for efficient transmission of data between the
        /// external data set and the COBOL program.
        /// This paragraph is optional in a COBOL program.
        /// </summary>
        public IList<IOControlEntry> IOControlEntries { get; set; }

        // -- DATA DIVISION --

        // A method data division contains data description entries for data accessible within the method. 
        // A method data division can contain a LOCAL-STORAGE SECTION or a WORKING-STORAGE SECTION, or both. 
        // The term method data applies to both.
        // Method data in LOCAL-STORAGE is dynamically allocated and initialized on each invocation of the method; 
        // method data in WORKING-STORAGE is static and persists across invocations of the method.

        /// <summary>
        /// The FILE SECTION defines the structure of data files.
        /// file-description-entry 
        /// Provides information about the physical structure and identification of a file, and gives the record-names associated with that file. 
        /// A single run-unit-level file connector is shared by all programs and methods that contain a declaration of a given external file.
        /// A method FILE SECTION can define external files only. 
        /// record-description-entry 
        /// A set of data description entries that describe the particular records contained within a particular file. 
        /// More than one record description entry can be specified; each is an alternative description of the same record storage area.
        /// Data areas described in the FILE SECTION are not available for processing unless the file that contains the data area is open.
        /// </summary>
        public IDictionary<SymbolDefinition, FileDescriptionEntry> FileDescriptions { get; set; }

        /// <summary>
        /// A single copy of the WORKING-STORAGE for a method is statically allocated on the first invocation of the method and persists in a last-used state for the duration of the run unit. 
        /// The same copy is used whenever the method is invoked regardless of which object instance the method is invoked upon. 
        /// If a VALUE clause is specified on a method WORKING-STORAGE data item, the data item is initialized to the VALUE clause value on the first invocation. 
        /// If the EXTERNAL clause is specified on a data description entry in a method WORKING-STORAGE SECTION, a single copy of the storage for that data item is allocated once for the duration of the run unit. 
        /// That storage is shared by all programs and methods in the run unit that contain a definition for the external data item. 
        /// </summary>
        public IList<DataDescriptionEntry> WorkingStorageData { get; set; }

        /// <summary>
        /// The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
        /// On each invocation, data items defined in the LOCAL-STORAGE SECTION are reallocated. 
        /// Each data item that has a VALUE clause is initialized to the value specified in that clause.
        /// For methods, a separate copy of the data defined in LOCAL-STORAGE is allocated and initialized on each invocation of the method. 
        /// The storage allocated for the data is freed when the method returns.
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

        /*
        USING (((BY? REFERENCE) | (BY? VALUE))? dataName+)+

        RETURNING dataName

        // declaratives
        sectionName SECTION priorityNumber? 
        useStatementForExceptionDeclarative | useStatementForDebuggingDeclarative
        paragraph* 

        // sections
        (sectionName SECTION priorityNumber?)?
        paragraph+

        // paragraph
        (paragraphName?)
        sentence+

        // sentence
        statement+ PeriodSeparator
        */
    }
}
