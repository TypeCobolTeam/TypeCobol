using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A variable declared has having a type that comes from a TypeDef.
    /// Such variable is expanded to have the expanded type from a TypeDef
    /// </summary>
    public class VariableTypeSymbol : VariableSymbol
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="tdSym">The associated TypeDef symbol</param>
        public VariableTypeSymbol(string name, TypedefSymbol tdSym) : base(name)
        {
            System.Diagnostics.Debug.Assert(tdSym != null);
            SetFlag(Flags.HasATypedefType, true);
            Typedef = tdSym;
        }

        /// <summary>
        /// The Type of a variable whose type comes from a TYPEDEF can be set letter when
        /// The TYPEDEF symbol is resolved. 
        /// </summary>
        public override Types.Type Type
        {
            get
            {
                if (base.Type == null && Typedef?.Type != null)
                {
                    base.Type = Typedef.Type;
                }
                return base.Type;
            }
            set => base.Type = value;
        }

        /// <summary>
        /// The Typedef symbol
        /// </summary>
        public TypedefSymbol Typedef
        {
            get;
            private set;
        }

        //public override string TypedName => Typedef != null && Typedef.Type != null && !Typedef.Type.HasFlag(Flags.BuiltinType) ? (Name + Typedef.Name) : Name;
        public override string TypedName => Typedef != null ? (Name + '.' + Typedef.Name) : Name;

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
            tw.Write(' ');
            bool bHasDot = false;
            if (Type != null)
            {
                if (Type.Tag == Type.Tags.Typedef || Type.TypeComponent?.Tag == Type.Tags.Typedef)
                {
                    tw.Write("TYPE ");
                    this.Type.Dump(tw, 0);
                }
                else
                {
                    if (Type.TypeComponent?.Tag == Type.Tags.Group)
                    {
                        tw.WriteLine(".");
                        this.Type.Dump(tw, indentLevel + 1);
                        bHasDot = true;
                    }
                    else
                    {
                        this.Type.Dump(tw, 0);
                    }
                }
            }
            else 
                tw.Write("???");
            DumpSymbolFlags(this.Flag, tw);
            if (!bHasDot)
                tw.WriteLine('.');
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitVariableTypeSymbol(this, arg); }
    }
}
