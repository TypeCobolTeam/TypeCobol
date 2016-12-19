using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p356:
    /// The INVOKE statement can create object instances of a COBOL or Java class and
    /// can invoke a method defined in a COBOL or Java class.
    /// </summary>
    public class InvokeStatement : StatementElement
    {
        public InvokeStatement() : base(CodeElementType.InvokeStatement, StatementType.InvokeStatement)
        { }

        /// <summary>
        /// p356:
        /// class-name-1
        /// When class-name-1 is specified together with literal-1 or identifier-2, the
        /// INVOKE statement invokes a static or factory method of the class
        /// referenced by class-name-1. literal-1 or identifier-2 specifies the name of the
        /// method that is to be invoked. The method must be a static method if
        /// class-name-1 is a Java class; the method must be a factory method if
        /// class-name-1 is a COBOL class.
        ///
        /// When class-name-1 is specified together with NEW, the INVOKE statement
        /// creates a new object that is an instance of class class-name-1.
        ///
        /// You must specify class-name-1 in the REPOSITORY paragraph of the
        /// configuration section of the class or program that contains the INVOKE
        /// statement.
        /// 
        /// p356:
        /// identifier-1
        /// Must be defined as USAGE OBJECT REFERENCE. The contents of
        /// identifier-1 specify the object on which a method is invoked.
        /// When identifier-1 is specified, either literal-1 or identifier-2 must be specified,
        /// identifying the name of the method to be invoked.
        ///
        /// The results of the INVOKE statement are undefined if either:
        /// * identifier-1 does not contain a valid reference to an object.
        /// * identifier-1 contains NULL.
        /// </summary>
        public SymbolReferenceVariable ClassNameOrObjectReference { get; set; }
                
        /// <summary>
        /// p357:
        /// SELF
        /// An implicit reference to the object used to invoke the currently executing
        /// method. When SELF is specified, the INVOKE statement must appear
        /// within the PROCEDURE DIVISION of a method.
        /// </summary>
        public SyntaxProperty<bool> SelfOjectIdentifier { get; set; }
         
        /// <summary>
        /// p357:
        /// SUPER
        /// An implicit reference to the object that was used to invoke the currently
        /// executing method. The resolution of the method to be invoked will ignore
        /// any methods declared in the class definition of the currently executing
        /// method and methods defined in any class derived from that class; thus the
        /// method invoked will be one that is inherited from an ancestor class.
        /// </summary>
        public SyntaxProperty<bool> SuperObjectIdentifier { get; set; }

        /// <summary>
        /// p357:
        /// literal-1
        /// The value of literal-1 is the name of the method to be invoked. The
        /// referenced object must support the method identified by literal-1.
        /// literal-1 must be an alphanumeric literal or a national literal.
        /// literal-1 is interpreted in a case-sensitive manner. The method name, the
        /// number of arguments, and the data types of the arguments in the USING
        /// phrase of the INVOKE statement are used to select the method with
        /// matching signature that is supported by the object. The method can be
        /// overloaded.
        ///
        /// p357:
        /// identifier-2
        /// A data item of category alphabetic, alphanumeric, or national that at run
        /// time contains the name of the method to be invoked. The referenced object
        /// must support the method identified by identifier-2.
        ///
        /// If identifier-2 is specified, identifier-1 must be defined as USAGE OBJECT
        /// REFERENCE without any optional phrases; that is, identifier-1 must be a
        /// universal object reference.
        ///
        /// The content of identifier-2 is interpreted in a case-sensitive manner. The
        /// method name, the number of arguments, and the data types of the
        /// arguments in the USING phrase of the INVOKE statement are used to
        /// select the method with matching signature that is supported by the object.
        ///
        /// The method can be overloaded.
        /// </summary>
        public SymbolReferenceVariable MethodName { get; set; }

        /// <summary>
        /// p357:
        /// The NEW operand specifies that the INVOKE statement is to create a new
        /// object instance of the class class-name-1. class-name-1 must be specified.
        ///
        /// When class-name-1 is implemented in Java, the USING phrase of the
        /// INVOKE statement can be specified. The number of arguments and the
        /// data types of the arguments in the USING phrase of the INVOKE
        /// statement are used to select the Java constructor with matching signature
        /// that is supported by the class. An object instance of class class-name-1 is
        /// allocated, the selected constructor (or the default constructor) is executed,
        /// and a reference to the created object is returned.
        ///
        /// When class-name-1 is implemented in COBOL, the USING phrase of the
        /// INVOKE statement must not be specified. An object instance of class
        /// class-name-1 is allocated, instance data items are initialized to the values
        /// specified in associated VALUE clauses, and a reference to the created object
        /// is returned.
        ///
        /// When NEW is specified, you must also specify a RETURNING phrase as
        /// described in “RETURNING phrase” on page 359.
        /// </summary>
        public SyntaxProperty<bool> ConstructorMethod { get; set; }

        /// <summary>
        /// p358:
        /// USING phrase
        /// The USING phrase specifies arguments that are passed to the target method. The
        /// argument data types and argument linkage conventions are restricted to those
        /// supported by Java. See “BY VALUE phrase” for details.
        /// 
        /// BY VALUE phrase
        /// Arguments specified in an INVOKE statement must be passed BY VALUE.
        /// The BY VALUE phrase specifies that the value of the argument is passed, not a
        /// reference to the sending data item. The invoked method can modify the formal
        /// parameter that corresponds to an argument passed by value, but changes do not
        /// affect the argument because the invoked method has access only to a temporary
        /// copy of the sending data item.
        ///
        /// identifier-3
        /// Must be an elementary data item in the DATA DIVISION. The data type of
        /// identifier-3 must be one of the types supported for Java interoperation, as
        /// listed in “Interoperable data types for COBOL and Java” on page 361.
        /// Miscellaneous cases that are also supported as identifier-3 are listed in
        /// “Miscellaneous argument types for COBOL and Java” on page 362, with
        /// their corresponding Java type.
        /// See Conformance requirements for arguments for additional requirements
        /// that apply to identifier-3.
        ///
        /// literal-2
        /// Must be of a type suitable for Java interoperation and must exactly match
        /// the type of the corresponding parameter in the target method. Supported
        /// literal forms are listed in “Miscellaneous argument types for COBOL and
        /// Java” on page 362, with their corresponding Java type.
        /// literal-2 must not be a DBCS literal.
        ///
        /// LENGTH OF identifier-3
        /// Specifies that the length of identifier-3 is passed as an argument in the
        /// LENGTH OF special register. A LENGTH OF special register passed BY
        /// VALUE is treated as a PIC 9(9) binary value. For information about the
        /// LENGTH OF special register, see “LENGTH OF” on page 19.
        ///
        /// pp358-359:
        /// Conformance requirements for arguments
        ///
        /// When identifier-3 is an object reference, certain rules apply.
        /// The rules are:
        /// * A class-name must be specified in the data description entry for that object
        /// reference. That is, identifier-3 must not be a universal object reference.
        /// * The specified class-name must reference a class that is exactly the class of the
        /// corresponding parameter in the invoked method. That is, the class of identifier-3
        /// must not be a subclass or a superclass of the corresponding parameter's class.
        ///
        /// When identifier-3 is not an object reference, the following rules apply:
        /// * If the target method is implemented in COBOL, the description of identifier-3
        /// must exactly match the description of the corresponding formal parameter in the
        /// target method.
        /// * If the target method is implemented in Java, the description of identifier-3 must
        /// correspond to the Java type of the formal parameter in the target method, as
        /// specified in “Interoperable data types for COBOL and Java” on page 361.
        ///
        /// Usage note: Adherence to conformance requirements for arguments is the
        /// responsibility of the programmer. Conformance requirements are not verified by
        /// the compiler.
        /// </summary>
        public IList<CallSiteParameter> InputParameters { get; set; }

        /// <summary>
        /// p459:
        /// The RETURNING phrase specifies a data item that will contain the value returned
        /// from the invoked method. You can specify the RETURNING phrase on the
        /// INVOKE statement when invoking methods that are written in COBOL or Java.
        ///
        /// identifier-4
        /// The RETURNING data item. identifier-4:
        /// * Must be defined in the DATA DIVISION
        /// * Must not be reference-modified
        /// * Is not changed if an EXCEPTION occurs
        /// The data type of identifier-4 must be one of the types supported for Java
        /// interoperation, as listed in “Interoperable data types for COBOL and Java”
        /// on page 361.
        ///
        /// See Conformance requirements for the RETURNING item for additional
        /// requirements that apply to identifier-4.
        ///
        /// If identifier-4 is specified and the target method is written in COBOL, the
        /// target method must have a RETURNING phrase in its PROCEDURE
        /// DIVISION header. When the target method returns, its return value is
        /// assigned to identifier-4 using the rules for the SET statement if identifier-4 is
        /// described with USAGE OBJECT REFERENCE; otherwise, the rules for the
        /// MOVE statement are used.
        ///
        /// The RETURNING data item is an output-only parameter. On entry to the called
        /// method, the initial state of the PROCEDURE DIVISION RETURNING data item
        /// has an undefined and unpredictable value. You must initialize the PROCEDURE
        /// DIVISION RETURNING data item in the invoked method before you reference its
        /// value. The value that is passed back to the invoker is the final value of the
        /// PROCEDURE DIVISION RETURNING data item when the invoked method
        /// returns.
        ///
        /// See Managing local and global references in the Enterprise COBOL Programming Guide
        /// for discussion of local and global object references as defined in Java. These
        /// attributes affect the life-time of object references.
        ///
        /// Usage note: The RETURN-CODE special register is not set by execution of
        /// INVOKE statements.
        ///
        /// pp459-460:
        /// Conformance requirements for the RETURNING item
        /// For INVOKE statements that specify class-name-1 NEW, the RETURNING phrase is
        /// required.
        ///
        /// The returning item must be one of the following ones:
        /// * A universal object reference
        /// * An object reference specifying class-name-1
        /// * An object reference specifying a superclass of class-name-1
        ///
        /// For INVOKE statements without the NEW phrase, the RETURNING item specified
        /// in the method invocation and in the corresponding target method must satisfy the
        /// following requirements:
        /// * The presence or absence of a return value must be the same on the INVOKE
        /// statement and in the target method.
        /// * If the RETURNING item is not an object reference, the following rules apply:
        ///   – If the target method is implemented in COBOL, the returning item in the
        ///   INVOKE statement and the RETURNING item in the target method must
        ///   have an identical data description entry.
        ///   – If the target method is implemented in Java, the returning item in the
        ///   INVOKE statement must correspond to the Java type of the method result, as
        ///   described in “Interoperable data types for COBOL and Java” on page 361.
        /// * If the RETURNING item is an object reference, the RETURNING item specified
        /// in the INVOKE statement must be an object reference typed exactly to the class
        /// of the returning item specified in the target method. That is, the class of
        /// identifier-4 must not be a subclass or a superclass of the class of the returning
        /// item in the target method.
        ///
        /// Usage note: Adherence to conformance requirements for returning items is the
        /// responsibility of the programmer. Conformance requirements are not verified by
        /// the compiler.
        /// </summary>
        public CallSiteParameter OutputParameter { get; set; }
    }
}
