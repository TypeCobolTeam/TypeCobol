using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Properties of a symbol Token in the Cobol grammar
    /// </summary>
    public abstract class SymbolInformation
    {
        public SymbolInformation(LiteralValue<string> nameLiteral, SymbolRole role, SymbolType type)
        {
            NameLiteral = nameLiteral;
            Role = role;
            Type = type;
        }

        /// <summary>
        /// Token defining the name of the symbol in source text
        /// </summary>
        public LiteralValue<string> NameLiteral { get; private set; }
        
        /// <summary>
        /// Name of the symbol
        /// </summary>
        public string Name { get { return NameLiteral.Value; } }

        /// <summary>
        /// Role of this symbol Token
        /// </summary>
        public SymbolRole Role { get; set; }
        
        /// <summary>
        /// Type of the symbol
        /// </summary>
        public SymbolType Type { get; set; }

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
    /// Properties of a symbol Token in the Cobol grammar - when its type is ambiguous
    /// </summary>
    public abstract class AmbiguousSymbolInformation : SymbolInformation
    {
        public AmbiguousSymbolInformation(LiteralValue<string> nameLiteral, SymbolRole role, SymbolType[] candidateTypes) :
            base(nameLiteral, role, SymbolType.TO_BE_RESOLVED)
        {
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
        public SymbolDefinition(LiteralValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.SymbolDefinition, type)
        { }
    }

    /// <summary>
    /// Reference to a previously defined symbol in the Cobol syntax
    /// </summary>
    public class SymbolReference : SymbolInformation
    {
        public SymbolReference(LiteralValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.SymbolReference, type)
        { }
    }

    /// <summary>
    /// Reference to a previously defined symbol in the Cobol syntax - when its type is ambiguous 
    /// </summary>
    public class AmbiguousSymbolReference : AmbiguousSymbolInformation
    {
        public AmbiguousSymbolReference(LiteralValue<string> nameLiteral, SymbolType[] candidateTypes) :
            base(nameLiteral, SymbolRole.SymbolReference, candidateTypes)
        { }
    }

    /// <summary>
    /// Role ambiguity between :
    /// Declaration of a new symbol in the Cobol syntax
    /// Reference to a previously defined symbol in the Cobol syntax
    /// </summary>
    public class SymbolDefinitionOrReference : SymbolInformation
    {
        public SymbolDefinitionOrReference(LiteralValue<string> nameLiteral, SymbolType type) :
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
        public ExternalName(LiteralValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.ExternalName, type)
        { }
    }

    /// <summary>
    /// Role ambiguity between :
    /// Reference to an external name defined by the environment
    /// Reference to a previously defined symbol in the Cobol syntax
    /// </summary>
    public class ExternalNameOrSymbolReference : AmbiguousSymbolInformation
    {
        public ExternalNameOrSymbolReference(LiteralValue<string> nameLiteral, SymbolType[] candidateTypes) :
            base(nameLiteral, SymbolRole.ExternalNameOrSymbolReference, candidateTypes)
        { }
    }
}
