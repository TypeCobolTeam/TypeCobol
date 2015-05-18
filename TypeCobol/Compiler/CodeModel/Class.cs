using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    /// <summary>
    /// Enterprise COBOL provides object-oriented syntax to facilitate interoperation of 
    /// COBOL and Java programs.
    /// You can use object-oriented syntax to:
    /// - Define classes, with methods and data implemented in COBOL
    /// - Create instances of Java or COBOL classes
    /// - Invoke methods on Java or COBOL objects
    /// - Write classes that inherit from Java classes or from other COBOL classes
    /// - Define and invoke overloaded methods
    /// </summary>
    public class Class
    {
        // -- IDENTIFICATION DIVISION --

        /// <summary>
        /// Class name, Iherits from class name, Authoring properties
        /// </summary>
        public ClassIdentification Identification { get; set; }

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
        /// class definition.
        /// </summary>
        public RepositoryParagraph RepositoryOfClassNames { get; set; }

        // -- Factory IDENTIFICATION DIVISION (optional) --

        /// <summary>
        /// A factory object is the single common object that is shared by all object instances of
        /// the class. The factory definition contains factory data and factory methods.
        /// </summary>
        public FactoryIdentification FactoryIdentification { get; set; }

        // -- Factory DATA DIVISION --

        // The factory data division contains data description entries for factory object data (factory data). 
        // Factory data is defined in the WORKING-STORAGE SECTION of the factory paragraph of a class definition. 
        
        /// <summary>
        /// The data described in the WORKING-STORAGE SECTION of a factory paragraph is factory data. 
        /// A single copy of factory data is statically allocated when the factory object for the class is created. 
        /// Factory data persists in a last-used state for the duration of the run unit. 
        /// Factory data can be initialized by VALUE clauses specified in data declarations or by logic specified in a factory method.
        /// </summary>
        public IList<DataDescriptionEntry> FactoryData { get; set; }
        
        // -- Factory PROCEDURE DIVISION --

        /// <summary>
        /// Methods defined in a factory definition are factory methods. A factory method in a
        /// given class can access:
        /// - Data defined in the DATA DIVISION of the factory paragraph of that class (factory data)
        /// - Data defined in the DATA DIVISION of that factory method (method data)
        /// A factory method cannot directly access factory data defined in a parent class,
        /// instance data defined in its own class, or method data defined in another method
        /// of its class. It must invoke a method to access such data.
        /// </summary>
        public IList<Method> FactoryMethods { get; set; }

        // -- Object IDENTIFICATION DIVISION (optional) --

        /// <summary>
        /// The object definition is the portion of a class definition that defines the instance objects of 
        /// the class. The object definition contains object data and object methods.
        /// </summary>
        public ObjectIdentification ObjectIdentification { get; set; }

        // -- Object DATA DIVISION --

        // The object data division contains data description entries for instance object data (instance data). 
        // Instance data is defined in the WORKING-STORAGE SECTION of the object paragraph of a class definition. 
        
        /// <summary>
        /// The data described in the WORKING-STORAGE SECTION of an object paragraph is object instance data, usually called instance data. 
        /// A separate copy of instance data is statically allocated for each object instance when the object is instantiated. 
        /// Instance data persists in a last-used state until the object instance is freed by the Java runtime system. 
        /// Instance data can be initialized by VALUE clauses specified in data declarations or by logic specified in an instance method. 
        /// </summary>
        public IList<DataDescriptionEntry> ObjectData { get; set; }

        // -- Object PROCEDURE DIVISION --

        /// <summary>
        /// Methods defined in an object definition are instance methods. An instance method in
        /// a given class can access:
        /// - Data defined in the DATA DIVISION of the object paragraph of that class (instance data)
        /// - Data defined in the DATA DIVISION of that instance method (method data)
        /// An instance method cannot directly access instance data defined in a parent class,
        /// factory data defined in its own class, or method data defined in another method of
        /// its class. It must invoke a method to access such data.
        /// </summary>
        public IList<Method> ObjectMethods { get; set; }
    }
}
