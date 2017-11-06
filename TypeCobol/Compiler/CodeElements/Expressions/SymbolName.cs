using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements.Expressions;

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
    //             TypeCobolQualifiedSymbolReference
    //     SymbolDefinitionOrReference
    //     ExternalName
    //         QualifiedTextName

    /// <summary>
    /// Properties of a symbol Token in the Cobol grammar
    /// </summary>
    public abstract class SymbolInformation : IVisitable
    {
        protected SymbolInformation(SyntaxValue<string> nameLiteral, SymbolRole role, SymbolType type)
        {
            NameLiteral = nameLiteral;
            Role = role;
            Type = type;
        }

        /// <summary>
        /// Token defining the name of the symbol in source text
        /// </summary>
        public SyntaxValue<string> NameLiteral { get; private set; }

		/// <summary>Symbol name</summary>
		public virtual string Name { get { return NameLiteral.Value; } }

        /// <summary>
        /// Role of this symbol Token
        /// </summary>
        public SymbolRole Role { get; protected set; }
        
        /// <summary>
        /// Type of the symbol
        /// </summary>
        public SymbolType Type { get; private set; }

        public virtual bool IsOrCanBeOfType(SymbolType symbolType)
        {
            return Type == symbolType;
        }

        public virtual bool IsOrCanBeOfType(params SymbolType[] symbolTypes)
        {
            return symbolTypes.Contains(Type);
        }

        public virtual bool IsOrCanBeOnlyOfTypes(params SymbolType[] symbolTypes)
        {
            return symbolTypes.Contains(Type);
        }

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

		public override int GetHashCode() { return Type.GetHashCode() * 11 + Name.GetHashCode(); }
        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, NameLiteral);
        }

        public override string ToString() {	return Name; }
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
    
	/// <summary>Declaration of a new symbol in the Cobol syntax</summary>
	public class SymbolDefinition: SymbolInformation {
		public SymbolDefinition(SyntaxValue<string> nameLiteral, SymbolType type)
			: base(nameLiteral, SymbolRole.SymbolDefinition, type) { }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }
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

		public SymbolReference(SymbolDefinition symbol)
			: this(symbol.NameLiteral, symbol.Type) { }

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

        public bool IsTypeCobolQualifiedReference { get; set; }

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

        

        private URI _uri;
        public URI URI {
            get { return _uri ?? (_uri = new URI(Name)); }
        }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
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

        public override bool IsOrCanBeOfType(SymbolType symbolType)
        {
            return CandidateTypes.Contains(symbolType);
        }

        public override bool IsOrCanBeOfType([NotNull] params SymbolType[] symbolTypes)
        {
            return CandidateTypes.Intersect(symbolTypes).Any();
        }

        public override bool IsOrCanBeOnlyOfTypes(params SymbolType[] symbolTypes)
        {
            return !CandidateTypes.Except(symbolTypes).Any();
        }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }

        public static void ApplyCandidatesTypes(SymbolReference symbolReference, SymbolType[] symbolTypes)
        {
            if (symbolReference.IsAmbiguous)
            {
                if (symbolReference.IsQualifiedReference)
                {
                    var qualifiedSymbolReference = (QualifiedSymbolReference)symbolReference;
                    ApplyCandidatesTypes(qualifiedSymbolReference.Head, symbolTypes);
                    ApplyCandidatesTypes(qualifiedSymbolReference.Tail, symbolTypes);

                }
                else
                {
                    ((AmbiguousSymbolReference)symbolReference).CandidateTypes = symbolTypes;
                }
            }
        }
    }

	/// <summary>
	/// A name that exists within a hierarchy of names can be made unique
	/// by specifying one or more higher-level names in the hierarchy.
	/// The higher-level names are called qualifiers, and the process by which
	/// such names are made unique is called qualification.
	/// </summary>
	public class QualifiedSymbolReference: SymbolReference, IList<SymbolReference> {
		public QualifiedSymbolReference(SymbolReference head, SymbolReference tail): base(head.NameLiteral, head.Type) {
			IsAmbiguous = head.IsAmbiguous || tail.IsAmbiguous;
			IsQualifiedReference = true;
			Head = head;
			Tail = tail;

		}

	    public SymbolReference Head { get; private set; }
		public SymbolReference Tail { get; private set; }
		public SymbolReference First {
			get {
				var head = Head;
				while (head.IsQualifiedReference) {
					head = ((QualifiedSymbolReference)head).Head;
				}
				return head;
			}
		}

        public override bool IsOrCanBeOfType(SymbolType symbolType) {
            return Head.IsOrCanBeOfType(symbolType) || Tail.IsOrCanBeOfType(symbolType);
        }

        public override bool IsOrCanBeOfType([NotNull] params SymbolType[] symbolTypes) {
            return Head.IsOrCanBeOfType(symbolTypes) || Tail.IsOrCanBeOfType(symbolTypes);
        }

        public override bool IsOrCanBeOnlyOfTypes(params SymbolType[] symbolTypes) {
            return Head.IsOrCanBeOnlyOfTypes(symbolTypes) || Tail.IsOrCanBeOnlyOfTypes(symbolTypes);
        }

        /// <summary>Used to resolve the symbol reference in a hierarchy of names</summary>
        public override string DefinitionPathPattern {
			get { return "\\." + Head.Name + "\\..*" + Tail.DefinitionPathPattern; }
		}

		public override string ToString() { return Head + " IN " + Tail; }

		public override string Name { get { return Tail.Name+'.'+Head.Name; } }




		public bool IsReadOnly { get { return true; } }

		public int Count { get { return AsList().Count; } }

		IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
		public IEnumerator<SymbolReference> GetEnumerator() { return AsList().GetEnumerator(); }
		public IList<SymbolReference> AsList() {
			var refs = new List<SymbolReference>();
			if (Head is QualifiedSymbolReference)
				 refs.AddRange(((QualifiedSymbolReference)Head).AsList());
			else refs.Add(Head);
			if (Tail is QualifiedSymbolReference)
				 refs.AddRange(((QualifiedSymbolReference)Tail).AsList());
			else refs.Add(Tail);
			return refs;
		}

	    public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Head, Tail);
	    }

	    // UNIMPLEMENTED BECAUSE OF LAZYNESS

        public int IndexOf(SymbolReference item) {
			throw new NotImplementedException("TODO");
		}

		public void Insert(int index,SymbolReference item) {
			throw new NotImplementedException();
		}

		public void RemoveAt(int index) {
			throw new NotImplementedException();
		}

		public SymbolReference this[int index] {
			get {
				throw new NotImplementedException("TODO");
			}
			set {
				throw new NotImplementedException();
			}
		}

		public void Add(SymbolReference item) {
			throw new NotImplementedException();
		}

		public void Clear() {
			throw new NotImplementedException();
		}

		public bool Contains(SymbolReference item) {
			throw new NotImplementedException("TODO");
		}

		public void CopyTo(SymbolReference[] array, int index) {
			throw new NotImplementedException();
		}

		public bool Remove(SymbolReference item) {
			throw new NotImplementedException();
		}
	}
	public class TypeCobolQualifiedSymbolReference: QualifiedSymbolReference {
	    public TypeCobolQualifiedSymbolReference(SymbolReference head, SymbolReference tail) : base(head, tail)
	    {
	        IsTypeCobolQualifiedReference = true;
	    }

	    public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
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

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }
    }

    /// <summary>
    /// Reference to an external name implicitely defined in :
    /// - the compiler (TCFunctionName, ExecTranslatorName)
    /// - the compilation environment (TextName, LibraryName)
    /// - the execution environment (AssignmentName, EnvironmentName, UPSISwitchName)
    /// </summary>
    public class ExternalName : SymbolInformation
    {
        public ExternalName(SyntaxValue<string> nameLiteral, SymbolType type) :
            base(nameLiteral, SymbolRole.ExternalName, type)
        { }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }
    }

	/// <summary>Unique case of qualified external name: textName (IN | OF) libraryName</summary>
	public class QualifiedTextName: ExternalName {
		public QualifiedTextName([NotNull] ExternalName textName, ExternalName libraryName)
				: base(textName.NameLiteral, textName.Type) {
			TextName = textName;
			LibraryName = libraryName;
		}

		public ExternalName TextName { get; private set; }
		public ExternalName LibraryName { get; private set; }

		public override string ToString() {
			if (LibraryName == null) return base.ToString();
			return base.ToString() + " IN " + LibraryName;
		}

		public override string Name { get { return LibraryName.Name+'.'+TextName.Name; } }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, TextName, LibraryName);
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

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }
    }
}
