using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Typedef Symbol
    /// </summary>
    public class TypedefSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public TypedefSymbol(string name)
            : base(name)
        {
            base.Kind = Kinds.Typedef;
        }

        /// <summary>
        /// Get the Scope of symbol associated to the given symbol name.
        /// </summary>
        /// <param name="path">The Symbol's path to get the Scope, the path is in reverse order à la COBOL.</param>
        /// <returns>The Multi Symbol set of all symbol corresponding to the given path.</returns>
        public Container<VariableSymbol>.Entry Get(string[] path)
        {
            return Get(path, null, null);
        }

        /// <summary>
        /// Get the Scope of symbol associated to the given symbol name.
        /// </summary>
        /// <param name="path">The Symbol's path to get the Scope, the path is in reverse order à la COBOL.</param>
        /// <param name="foundSymbolPaths">The list of paths corresponding to symbol found in the return Scope.
        /// This parameter can be set to null if found variables paths are not requested</param>
        /// <param name="foundSymbolTypedPaths">The list of typed paths corresponding to symbol found in the return Scope.
        /// This parameter can be set to null if found variables typed paths are not requested</param>
        /// <returns>The Multi Symbol set of all symbol corresponding to the given path.</returns>
        public Container<VariableSymbol>.Entry Get(string[] path, List<Symbol[]> foundSymbolPaths, List<Symbol[]> foundSymbolTypedPaths)
        {
            if (path == null || path.Length == 0 || path[0] == null)
                return null;

            foundSymbolPaths?.Clear();
            foundSymbolTypedPaths?.Clear();

            string name = path[0];
            var results = new Container<VariableSymbol>.Entry(name);

            DomainLookup lookup = new DomainLookup();
            //Add our parent and ourself to the path variable.
            LookupContext ctx = new LookupContext(name);
            ctx.Path.AddLast(Owner);
            ctx.Path.AddLast(this);
            ctx.TypedPath.AddLast(Owner);
            ctx.TypedPath.AddLast(this);

            this.Accept(lookup, ctx);
            foreach (var candidate in ctx.Candidates)
            {
                if (IsPathMatching(path, candidate.Item2))
                {
                    results.Add(candidate.Item1);
                    foundSymbolPaths?.Add(candidate.Item2);
                    foundSymbolTypedPaths?.Add(candidate.Item3);
                }
            }
            return results;
        }

        /// <summary>
        /// Does the given path matches the Symbol path
        /// </summary>
        /// <param name="path">Path</param>
        /// <param name="symbolPath">Symbol Path</param>
        /// <returns></returns>
        private static bool IsPathMatching(string[] path, Symbol[] symbolPath)
        {
            int currentSymbolIndex = symbolPath.Length - 1;
            for (int i = 0; i < path.Length; i++)
            {
                switch (i)
                {
                    case 0:
                        string name = symbolPath[currentSymbolIndex].Name;
                        if (!path[i].Equals(name, StringComparison.OrdinalIgnoreCase))
                            return false;
                        break;
                    default:
                        {
                            currentSymbolIndex = LookupParentIndexOfName(currentSymbolIndex, symbolPath, path[i]);
                            if (currentSymbolIndex < 0)
                                return false;
                        }
                        break;
                }
            }
            return true;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="currentSymbolIndex">The current symbol index in the symbolPath array</param>
        /// <param name="symbolPath">The Array of symbols</param>
        /// <param name="name">Parent name looked for</param>
        /// <returns></returns>
        private static int LookupParentIndexOfName(int currentSymbolIndex, Symbol[] symbolPath, string name)
        {
            while (currentSymbolIndex > 0)
            {
                if (symbolPath[currentSymbolIndex - 1].Name.Equals(name, StringComparison.OrdinalIgnoreCase))
                    return currentSymbolIndex - 1;
                currentSymbolIndex--;
            }
            return -1;
        }

        /// <summary>
        /// The Lookup context class
        /// </summary>
        private class LookupContext
        {
            /// <summary>
            /// base name being looked up.
            /// </summary>
            public string Name;
            /// <summary>
            /// Current Path
            /// </summary>
            public LinkedList<Symbol> Path;
            /// <summary>
            /// Current Typed Path
            /// </summary>
            public LinkedList<Symbol> TypedPath;
            /// <summary>
            /// Matching variable, patching symbol paths, typed matching symbol path
            /// </summary>
            public LinkedList<Tuple<VariableSymbol, Symbol[], Symbol[]>> Candidates;
            public LookupContext(string name)
            {
                Name = name;
                Path = new LinkedList<Symbol>();
                TypedPath = new LinkedList<Symbol>();
                Candidates = new LinkedList<Tuple<VariableSymbol, Symbol[], Symbol[]>>();
            }
        }

        /// <summary>
        /// Typedef Symbol Domain Completer, this a completer of a TypeDef Symbol domain completer
        /// with unresolved Variable having a Typedef type at parsing time.
        /// 
        /// </summary>
        private class DomainLookup : AbstractSymbolAndTypeVisitor<LookupContext, LookupContext>
        {
            /// <summary>
            /// Default Symbol completion
            /// </summary>
            /// <param name="s">The current symbol being visited</param>
            /// <param name="ctx">The TypeDef symbol being completed</param>
            /// <returns>The Completed TypeSymbol instance</returns>
            public override LookupContext VisitSymbol(Symbol s, LookupContext ctx)
            {
                s.Type?.Accept(this, ctx);
                return ctx;
            }

            public override LookupContext VisitVariableSymbol(VariableSymbol s, LookupContext ctx)
            {
                ctx.Path.AddLast(s);
                ctx.TypedPath.AddLast(s);
                if (s.Name.Equals(ctx.Name, StringComparison.OrdinalIgnoreCase))
                {
                    ctx.Candidates.AddLast(new Tuple<VariableSymbol, Symbol[], Symbol[]>(s, ctx.Path.ToArray(), ctx.TypedPath.ToArray()));
                }
                s.Type?.Accept(this, ctx);
                ctx.Path.RemoveLast();
                ctx.TypedPath.RemoveLast();
                return ctx;
            }

            public override LookupContext VisitVariableTypeSymbol(VariableTypeSymbol s, LookupContext ctx)
            {
                ctx.Path.AddLast(s);
                ctx.TypedPath.AddLast(s);
                if (s.Typedef != null)
                {
                    ctx.TypedPath.AddLast(s.Typedef);
                }
                if (s.Name.Equals(ctx.Name, StringComparison.OrdinalIgnoreCase))
                {
                    ctx.Candidates.AddLast(new Tuple<VariableSymbol, Symbol[], Symbol[]>(s, ctx.Path.ToArray(), ctx.TypedPath.ToArray()));
                }
                s.Type?.Accept(this, ctx);
                ctx.Path.RemoveLast();
                ctx.TypedPath.RemoveLast();
                if (s.Typedef != null)
                {
                    ctx.TypedPath.RemoveLast();
                }
                return ctx;
            }

            /// <summary>
            /// Default Type completion
            /// </summary>
            /// <param name="t">The current type being completed</param>
            /// <param name="tdSym">The TypeDef symbol being completed</param>
            /// <returns>The Completed TypeSymbol instance</returns>
            public override LookupContext VisitType(Types.Type t, LookupContext ctx)
            {
                t.TypeComponent?.Accept(this, ctx);
                return ctx;
            }

            public override LookupContext VisitGroupType(Types.GroupType t, LookupContext ctx)
            {
                foreach (var field in t.Fields)
                {
                    field.Accept(this, ctx);
                }
                return ctx;
            }
        }

        /// <summary>
        /// Dump this symbol in the given TextWriter instance
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(this.Level.ToString("00"));
            tw.Write(' ');
            tw.Write(Name);
            tw.Write(" TYPEDEF ");
            if (Type != null)
                this.Type.Dump(tw, 0);
            else
                tw.Write("???");
            DumpSymbolFlags(this.Flag, tw);
            tw.Write('.');
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitTypedefSymbol(this, arg); }
    }
}
