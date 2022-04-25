using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Model
{
    /// <summary>
    /// Base class for all SQL objects built during CodeElement phase.
    /// </summary>
    public abstract class SqlObject : IVisitable
    {
        //To help implement DumpContent methods in various SqlObjects
        protected static void DumpProperty(TextWriter output, string name, object value, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write($"{indent}- {name} = ");
            switch (value)
            {
                case null:
                    output.WriteLine("<NULL>");
                    break;
                case SqlObject sqlObject:
                    sqlObject.Dump(output, indentLevel + 1);
                    break;
                case SymbolReference symbolReference:
                    //Special case for QualifiedSymbolReference which is also IEnumerable, we want to use SymbolReference.ToString() here
                    output.WriteLine(symbolReference.ToString());
                    break;
                case System.Collections.IEnumerable enumerable:
                {
                    output.WriteLine("[");
                    int index = 0;
                    foreach (var item in enumerable)
                    {
                        DumpProperty(output, $"{name}[{index}]", item, indentLevel + 1);
                        index++;
                    }
                    output.WriteLine(indent + "]");
                    break;
                }
                default:
                    output.WriteLine(value);
                    break;
            }
        }

        //TODO How to set and add tokens automatically for all SQL objects ?
        public IList<Token> ConsumedTokens { get; internal set; }

        public bool AcceptVisitor(ISqlVisitor visitor)
        {
            bool continueVisit = visitor.BeginSqlObject(this) && VisitSqlObject(visitor);
            visitor.EndSqlObject(this);
            return continueVisit;
        }

        public void Dump(TextWriter output, int indentLevel)
        {
            output.WriteLine(GetType().Name); //Start directly at current position with type name, indent is used for content only
            DumpContent(output, indentLevel);
        }

        protected virtual void DumpContent(TextWriter output, int indentLevel) { }

        protected abstract bool VisitSqlObject([NotNull] ISqlVisitor visitor);
    }
}
