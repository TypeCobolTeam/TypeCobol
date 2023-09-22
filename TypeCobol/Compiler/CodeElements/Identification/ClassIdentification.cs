using System;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Class IDENTIFICATION DIVISION
    /// For a class, the first paragraph of the IDENTIFICATION DIVISION must
    /// be the CLASS-ID paragraph. The other paragraphs are optional and can
    /// appear in any order.
    /// </summary>
    public class ClassIdentification : CodeElement
    {
        public ClassIdentification() : base(CodeElementType.ClassIdentification)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaA;
        /// <summary>
        /// class-name
        /// A user-defined word that identifies the class. class-name can optionally
        /// have an entry in the REPOSITORY paragraph of the configuration section
        /// of the class definition.
        /// </summary>
        public SymbolDefinition ClassName { get; set; }

        /// <summary>
        /// INHERITS
        /// A clause that defines class-name-1 to be a subclass (or derived class) of
        /// class-name-2 (the parent class). class-name-1 cannot directly or indirectly
        /// inherit from class-name-1.
        /// class-name-2
        /// The name of a class inherited by class-name-1. You must specify class-name-2
        /// in the REPOSITORY paragraph of the configuration section of the class
        /// definition.
        /// The semantics of inheritance are as defined by Java. All classes must be derived
        /// directly or directly from the java.lang.Object class.
        /// Java supports single inheritance; that is, no class can inherit directly from more
        /// than one parent. Only one class-name can be specified in the INHERITS phrase of
        /// a class definition.
        /// </summary>
        public SymbolReference InheritsFrom { get; set; }

        /// <summary>
        /// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
        /// The optional paragraphs are: 
        /// AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY
        /// </summary>
        public AuthoringProperties AuthoringProperties { get; set; }
        
        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- ClassName = " + ClassName);
            sb.AppendLine("- InheritsFromClassName = " + InheritsFrom);
            if(AuthoringProperties != null)
            {
                sb.Append(AuthoringProperties);
            }
            return sb.ToString();
        }
    }
}
