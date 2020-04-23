using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents any symbol that contain other symbols (i.e. ProgramSymbol or NamespaceSymbol)
    /// </summary>
    public abstract class ScopeSymbol : Symbol, IScope
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        protected ScopeSymbol(string name, Kinds kind)
            : base(name, kind)
        {
        }

        public virtual Domain<TypedefSymbol> Types
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> FileData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> GlobalStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> WorkingStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> LocalStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> LinkageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<SectionSymbol> Sections
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<ParagraphSymbol> Paragraphs
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<FunctionSymbol> Functions
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<ProgramSymbol> Programs
        {
            get { return null; }
            protected set { ; }
        }

        /// <summary>
        /// Compute the path represented by a Symbol Reference
        /// </summary>
        /// <param name="symRef">The Symbol Reference instance</param>
        /// <returns>The corresponding Path in the COBOL IN|OF ORDER. The paths are return ed in lower cases</returns>
        public static string[] SymbolReferenceToPath(SymbolReference symRef)
        {
            string[] paths = null;
            IList<SymbolReference> refs = null;

            if (symRef.IsQualifiedReference)
            {//Path in reverse order DVZF0OS3::EventList --> {EventList, DVZF0OS3}
                QualifiedSymbolReference qualifiedSymbolReference = symRef as QualifiedSymbolReference;
                refs = qualifiedSymbolReference.AsList();
            }
            else
            {
                refs = new List<SymbolReference>() { symRef };
            }

            paths = new string[refs.Count];
            for (int i = 0; i < refs.Count; i++)
            {
                paths[i] = refs[i].Name;
            }

            return paths;
        }
    }
}
