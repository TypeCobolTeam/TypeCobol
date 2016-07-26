using System;

namespace TypeCobol.Compiler.CodeElements
{
    // Hierarchy of classes :
    // --------------------
    // SymbolInformation
    //     SymbolDefinition
    //     SymbolReference
    //         AmbiguousSymbolReference
    //             ExternalNameOrSymbolReference
    //         QualifiedSymbolReference
    //     SymbolDefinitionOrReference
    //     ExternalName
    //         QualifiedTextName

    /// <summary>
    /// Properties of a symbol Token in the Cobol grammar
    /// </summary>
    public abstract class SymbolInformation: Named
    {
        public SymbolInformation(SyntaxValue<string> nameLiteral, SymbolRole role, SymbolType type)
        {
            NameLiteral = nameLiteral;
            Role = role;
            Type = type;
        }

        /// <summary>
        /// Token defining the name of the symbol in source text
        /// </summary>
        public SyntaxValue<string> NameLiteral { get; private set; }
        
        /// <summary>
        /// Name of the symbol
        /// </summary>
        public string Name { get { return NameLiteral.Value; } }

        /// <summary>
        /// Role of this symbol Token
        /// </summary>
        public SymbolRole Role { get; protected set; }
        
        /// <summary>
        /// Type of the symbol
        /// </summary>
        public SymbolType Type { get; private set; }

        // -- Override Equals & GetHashCode --

        public override bool Equals(object obj)
        {
            SymbolInformation otherSymbol = obj as SymbolInformation;
            if (otherSymbol == null)
            {
                return false;
            }
            else
            {
                return Name.Equals(otherSymbol.Name, StringComparison.OrdinalIgnoreCase) &&
                       Type == otherSymbol.Type;
            }
        }

        public override int GetHashCode()
        {
            return Type.GetHashCode() * 11 + Name.GetHashCode();
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Name;
        }       
    }

    /// <summary>
    /// Role of a symbol Token in the Cobol grammar
    /// </summary>
    public enum SymbolRole
    {
        SymbolDefinition,
        SymbolReference,
        SymbolDefinitionOrReference,  // class names in the repository paragraph can be either references or definitions
        ExternalName,
        ExternalNameOrSymbolReference // mnemonicForEnvironmentName or EnvironmentName, assignmentName or FileName
    }
    
    /// <summary>
    /// Declaration of a new symbol in the Cobol syntax
    /// </summary>
    public class SymbolDefinition : SymbolInformation
    {
        public SymbolDefinition(SyntaxValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.SymbolDefinition, type)
        { }
    }

    /// <summary>
    /// Reference to a previously defined symbol in the Cobol syntax
    /// </summary>
    public class SymbolReference : SymbolInformation
    {
        public SymbolReference(SyntaxValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.SymbolReference, type)
        {
            IsAmbiguous = false;
            IsQualifiedReference = false;
        }

        /// <summary>
        /// True of the type of the symbol reference is ambiguous 
        /// during the first parsing phase
        /// </summary>
        public bool IsAmbiguous { get; protected set; }
        
        /// <summary>
        /// True if the symbol reference is a combination of child and
        /// parent symbols in a symbols hierarchy
        /// </summary>
        public bool IsQualifiedReference { get; protected set; }

        /// <summary>
        /// Used to resolve the symbol reference in a hierarchy of names
        /// </summary>
        public virtual string DefinitionPathPattern
        {
            get
            {
                return "\\." + Name + "$"; 
            }
        }
    }

    /// <summary>
    /// Reference to a previously defined symbol in the Cobol syntax - when its type is ambiguous 
    /// </summary>
    public class AmbiguousSymbolReference : SymbolReference
    {
        public AmbiguousSymbolReference(SyntaxValue<string> nameLiteral, SymbolType[] candidateTypes) :
            base(nameLiteral, SymbolType.TO_BE_RESOLVED)
        {
            IsAmbiguous = true;
            CandidateTypes = candidateTypes;
        }

        /// <summary>
        /// During the first parsing stage, the syntax alone does not always
        /// enable the parser to guess a unique symbol type for a given token.
        /// The parser inserts in this property a list of all possible symbol
        /// types for this token : only a lookup in the symbol tables at a
        /// later stage will resolve the ambiguity and give a final value
        /// to the Type property.
        /// </summary>
        public SymbolType[] CandidateTypes { get; set; }
    }

    /// <summary>
    /// A name that exists within a hierarchy of names can be made unique 
    /// by specifying one or more higher-level names in the hierarchy. 
    /// The higher-level names are called qualifiers, and the process by which 
    /// such names are made unique is called qualification.
    /// </summary>
    public class QualifiedSymbolReference : SymbolReference
    {
        public QualifiedSymbolReference(SymbolReference qualifiedSymbol, SymbolReference qualifierSymbol) :
            base(qualifiedSymbol.NameLiteral, qualifiedSymbol.Type)
        {
            IsAmbiguous = qualifiedSymbol.IsAmbiguous;
            IsQualifiedReference = true;
            QualifiedSymbol = qualifiedSymbol;
            QualifierSymbol = qualifierSymbol;
        }

        public SymbolReference QualifiedSymbol { get; private set; }

        public SymbolReference QualifierSymbol { get; private set; }

        /// <summary>
        /// Used to resolve the symbol reference in a hierarchy of names
        /// </summary>
        public override string DefinitionPathPattern
        {
            get
            {
                return "\\." + QualifiedSymbol.Name + "\\..*" + QualifiedSymbol.DefinitionPathPattern;
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return QualifiedSymbol.ToString() + " IN " + QualifierSymbol.ToString();
        }
    }

    /// <summary>
    /// Role ambiguity between :
    /// Declaration of a new symbol in the Cobol syntax
    public class SymbolDefinitionOrReference : SymbolInformation
    {
        public SymbolDefinitionOrReference(SyntaxValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.SymbolDefinitionOrReference, type)
        { }    
    }

    /// <summary>
    /// Reference to an external name implicitely defined in :
    /// - the compiler (FunctionName, ExecTranslatorName)
    /// - the compilation environment (TextName, LibraryName)
    /// - the execution environment (AssignmentName, EnvironmentName, UPSISwitchName)
    /// </summary>
    public class ExternalName : SymbolInformation
    {
        public ExternalName(SyntaxValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.ExternalName, type)
        { }
    }

    /// <summary>
    /// Unique case of qualified external name : 
    /// textName (IN | OF) libraryName
    /// </summary>
    public class QualifiedTextName : ExternalName
    {
        public QualifiedTextName(ExternalName textName, ExternalName libraryName) :
            base(textName.NameLiteral, textName.Type)
        {
            TextName = textName;
            LibraryName = libraryName;
        }

        public ExternalName TextName { get; private set; }

        public ExternalName LibraryName { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (LibraryName == null)
            {
                return base.ToString();
            }
            else
            {
                return base.ToString() + " IN " + LibraryName.ToString();
            }
        }
    }

    /// <summary>
    /// Role ambiguity between :
    /// Reference to an external name defined by the environment
    /// Reference to a previously defined symbol in the Cobol syntax
    /// </summary>
    public class ExternalNameOrSymbolReference : AmbiguousSymbolReference
    {
        public ExternalNameOrSymbolReference(SyntaxValue<string> nameLiteral, SymbolType[] candidateTypes) :
            base(nameLiteral, candidateTypes)
        {
            Role = SymbolRole.ExternalNameOrSymbolReference;
        }
    }
}
