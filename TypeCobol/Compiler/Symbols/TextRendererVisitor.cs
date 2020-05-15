using System.Collections.Generic;
using System.IO;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Visitor to build a text representation of a symbol or a type.
    /// </summary>
    public class TextRendererVisitor : AbstractSymbolAndTypeVisitor<object, IEnumerable<string>>
    {
        /// <summary>
        /// Target TextWriter.
        /// </summary>
        public TextWriter Output { get; }

        private int _currentIndentLevel;
        private bool _endDeclarationWithDot;//To force writing of dot at end of data declaration in Data Division
        private bool _insideGroupType;//To control who is responsible to write dots when visiting a GroupType

        /// <summary>
        /// Creates a new TextRendererVisitor for a given TextWriter instance.
        /// </summary>
        /// <param name="output">The non-null TextWriter to write to.</param>
        public TextRendererVisitor([NotNull] TextWriter output)
        {
            System.Diagnostics.Debug.Assert(output != null);
            Output = output;
            _currentIndentLevel = 0;
            _endDeclarationWithDot = false;
            _insideGroupType = false;
        }

        private void Indent() => Output.Write(new string(' ', 2 * _currentIndentLevel));

        /// <summary>
        /// Forwards visit to Symbol's type including interesting modifiers in the process.
        /// </summary>
        /// <param name="symbol">Currently visited symbol.</param>
        private void WriteSymbolType(Symbol symbol)
        {
            var modifiers = GetModifiers();
            if (symbol.Type != null)
            {
                symbol.Type.Accept(this, modifiers);
            }
            else
            {
                Output.Write(" ???");
                if (modifiers.Any())
                {
                    foreach (var modifier in modifiers)
                    {
                        Output.Write(' ');
                        Output.Write(modifier);
                    }
                }
            }

            //Collect interesting symbol flags to render
            IEnumerable<string> GetModifiers()
            {
                if (symbol.HasFlag(Symbol.Flags.Strict)) yield return "STRICT";
                if (symbol.HasFlag(Symbol.Flags.Private)) yield return "PRIVATE";
                if (symbol.HasFlag(Symbol.Flags.Public)) yield return "PUBLIC";
                if (symbol.HasFlag(Symbol.Flags.Global)) yield return "GLOBAL";
            }
        }

        #region Render symbols

        /// <summary>
        /// Dump the given symbol into the TextWriter associated with this instance.
        /// </summary>
        /// <param name="symbol">Symbol to dump.</param>
        /// <param name="indentLevel">Initial indentation level.</param>
        public void Dump([NotNull] Symbol symbol, int indentLevel = 0)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            System.Diagnostics.Debug.Assert(indentLevel >= 0);
            _currentIndentLevel = indentLevel;
            symbol.Accept(this, null);
        }

        public override object VisitSymbol(Symbol symbol, IEnumerable<string> modifiers)
        {
            //Default behavior
            Output.Write(symbol.Name);
            return null;
        }

        public override object VisitProgramSymbol(ProgramSymbol program, IEnumerable<string> modifiers)
        {
            Indent();
            Output.WriteLine("IDENTIFICATION DIVISION.");
            Indent();
            Output.Write("PROGRAM-ID. ");
            Output.Write(program.Name);
            Output.WriteLine('.');
            WriteDataDivision(program);
            Indent();
            Output.Write("PROCEDURE DIVISION");
            WriteSymbolType(program);
            Output.WriteLine(".");
            WriteFunctions(program);
            WriteNestedPrograms(program);
            Indent();
            Output.Write("END PROGRAM ");
            Output.Write(program.Name);
            Output.Write(".");
            return null;
        }

        private void WriteDataDivision(ProgramSymbol program)
        {
            //All data declarations must end with dot inside Data Division.
            _endDeclarationWithDot = true;
            Indent();
            Output.WriteLine("DATA DIVISION.");
            WriteFileSection(program);
            WriteGlobalSection(program);
            WriteWorkingSection(program);
            WriteLocalSection(program);
            WriteLinkageSection(program);
            _endDeclarationWithDot = false;
        }

        private void WriteFileSection(ProgramSymbol program)
        {
            WriteSection("FILE", program.FileData);
        }

        private void WriteGlobalSection(ProgramSymbol program)
        {
            WriteSection("GLOBAL-STORAGE", program.GlobalStorageData);
        }

        private void WriteWorkingSection(ProgramSymbol program)
        {
            WriteSection("WORKING-STORAGE", program.WorkingStorageData);
        }

        private void WriteLocalSection(ProgramSymbol program)
        {
            WriteSection("LOCAL-STORAGE", program.LocalStorageData);
        }

        private void WriteLinkageSection(ProgramSymbol program)
        {
            WriteSection("LINKAGE", program.LinkageData);
        }

        private void WriteSection(string sectionName, Domain<VariableSymbol> sectionVariables)
        {
            if (sectionVariables.Any())
            {
                Indent();
                Output.Write(sectionName);
                Output.WriteLine(" SECTION.");
                foreach (var variable in sectionVariables)
                {
                    variable.Accept(this, null);
                    Output.WriteLine();
                }
            }
        }

        private void WriteFunctions(ProgramSymbol program)
        {
            foreach (var function in program.Functions)
            {
                function.Accept(this, null);
                Output.WriteLine();
            }
        }

        private void WriteNestedPrograms(ProgramSymbol program)
        {
            foreach (var nestedProgram in program.Programs)
            {
                nestedProgram.Accept(this, null);
                Output.WriteLine();
            }
        }

        public override object VisitFunctionSymbol(FunctionSymbol function, IEnumerable<string> modifiers)
        {
            Indent();
            Output.Write("DECLARE PROCEDURE ");
            Output.Write(function.Name);
            WriteSymbolType(function);
            Output.WriteLine(".");
            WriteDataDivision(function);
            Indent();
            Output.WriteLine("PROCEDURE DIVISION.");
            Indent();
            Output.WriteLine("    .");
            Indent();
            Output.Write("END-DECLARE.");
            return null;
        }

        public override object VisitVariableSymbol(VariableSymbol variable, IEnumerable<string> modifiers)
        {
            WriteVariableSymbol(variable, false);
            return null;
        }

        public override object VisitTypedVariableSymbol(TypedVariableSymbol typedVariable, IEnumerable<string> modifiers)
        {
            WriteVariableSymbol(typedVariable, true);
            return null;
        }

        private void WriteVariableSymbol(VariableSymbol variable, bool typed)
        {
            Indent();
            Output.Write(variable.Level.ToString("00"));
            Output.Write(' ');
            Output.Write(variable.Name);
            if (typed) Output.Write(" TYPE");
            WriteSymbolType(variable);
            if (_endDeclarationWithDot && !_insideGroupType) Output.Write('.');
        }

        public override object VisitRedefinesSymbol(RedefinesSymbol redefines, IEnumerable<string> modifiers)
        {
            Indent();
            Output.Write(redefines.Level.ToString("00"));
            Output.Write(' ');
            Output.Write(redefines.Name);
            Output.Write(" REDEFINES ");
            Output.Write(string.Join(".", redefines.RedefinedPath));
            WriteSymbolType(redefines);
            if (_endDeclarationWithDot && !_insideGroupType) Output.Write('.');
            return null;
        }

        public override object VisitTypedefSymbol(TypedefSymbol typedef, IEnumerable<string> modifiers)
        {
            Indent();
            Output.Write(typedef.Level.ToString("00"));
            Output.Write(' ');
            Output.Write(typedef.Name);
            Output.Write(" TYPEDEF");
            WriteSymbolType(typedef);
            Output.Write('.');//Test not required here as Typedef can't be declared outside of DataDiv and not inside groups
            return null;
        }

        #endregion

        #region Render types

        /// <summary>
        /// Dump the given type into the TextWriter associated with this instance.
        /// </summary>
        /// <param name="type">Type to dump.</param>
        /// <param name="indentLevel">Initial indent level.</param>
        public void Dump([NotNull] Type type, int indentLevel = 0)
        {
            System.Diagnostics.Debug.Assert(type != null);
            System.Diagnostics.Debug.Assert(indentLevel >= 0);
            _currentIndentLevel = indentLevel;
            type.Accept(this, null);
        }

        public override object VisitType(Type type, IEnumerable<string> modifiers)
        {
            //Default behavior
            Output.Write(' ');
            Output.Write(type.Usage);
            return null;
        }

        public override object VisitProgramType(ProgramType program, IEnumerable<string> modifiers)
        {
            //Program parameters : USING keyword iw written inline and each parameter is on a separate indented line
            if (program.Usings != null && program.Usings.Count > 0)
            {
                Output.Write(" USING");
                _currentIndentLevel++;
                foreach (var parameter in program.Usings)
                {
                    Output.WriteLine();
                    Indent();
                    if (parameter.HasFlag(Symbol.Flags.ByValue))
                        Output.Write("BY VALUE ");
                    else if (parameter.HasFlag(Symbol.Flags.ByReference))
                        Output.Write("BY REFERENCE ");
                    else if (parameter.HasFlag(Symbol.Flags.ByContent))
                        Output.Write("BY CONTENT ");
                    Output.Write(parameter.Name);
                }
                _currentIndentLevel--;
            }

            //Return variable : on a separate indented line
            if (program.ReturnVariable != null)
            {
                _currentIndentLevel++;
                Output.WriteLine();
                Indent();
                Output.Write("RETURNING ");
                Output.Write(program.ReturnVariable.Name);
                _currentIndentLevel--;
            }

            return null;
        }

        private void WriteModifiers(IEnumerable<string> modifiers)
        {
            if (modifiers != null)
            {
                foreach (var modifier in modifiers)
                {
                    Output.Write(' ');
                    Output.Write(modifier);
                }
            }
        }

        public override object VisitFunctionType(FunctionType function, IEnumerable<string> modifiers)
        {
            WriteModifiers(modifiers);

            //Write parameters grouped by direction, direction is on a separate line and each parameter is on a separate line
            if (function.Parameters != null && function.Parameters.Count > 0)
            {
                _currentIndentLevel++;
                foreach (var group in function.Parameters.GroupBy(GetParameterDirection))
                {
                    Output.WriteLine();
                    Indent();
                    Output.Write(group.Key);//Write INPUT, IN-OUT, OUTPUT
                    _currentIndentLevel++;
                    foreach (var parameter in group)
                    {
                        Output.WriteLine();
                        parameter.Accept(this, null);
                    }
                    _currentIndentLevel--;
                }
                _currentIndentLevel--;

                string GetParameterDirection(VariableSymbol parameter)
                {
                    if (parameter.HasFlag(Symbol.Flags.Input)) return "INPUT";
                    if (parameter.HasFlag(Symbol.Flags.Inout)) return "IN-OUT";
                    if (parameter.HasFlag(Symbol.Flags.Output)) return "OUTPUT";
                    return "???";
                }
            }

            //Write returning variable, same format as INPUT/IN-OUT/OUTPUT: keyword on a single line, symbol on a separate line below
            if (function.ReturnSymbol != null)
            {
                _currentIndentLevel++;
                Output.WriteLine();
                Indent();
                Output.Write("RETURNING");
                _currentIndentLevel++;
                Output.WriteLine();
                function.ReturnSymbol.Accept(this, null);
                _currentIndentLevel--;
                _currentIndentLevel--;
            }

            return null;
        }

        public override object VisitPictureType(PictureType picture, IEnumerable<string> modifiers)
        {
            Output.Write(" PIC ");
            Output.Write(picture.Picture);
            if (picture.Usage != Type.UsageFormat.None)
            {
                Output.Write(' ');
                Output.Write(picture.Usage);
            }
            WriteModifiers(modifiers);
            return null;
        }

        public override object VisitArrayType(ArrayType array, IEnumerable<string> modifiers)
        {
            WriteModifiers(modifiers);

            //same logic as WriteSymbolType but with array "modifiers" to include MinOccur and MaxOccur
            if (array.ElementType != null)
            {
                array.ElementType.Accept(this, GetArrayModifiers());
            }
            else
            {
                Output.Write(" ???");
                WriteModifiers(GetArrayModifiers());
            }

            return null;

            IEnumerable<string> GetArrayModifiers()
            {
                yield return "OCCURS";
                yield return array.MinOccur.ToString();
                yield return "TO";
                yield return array.MaxOccur.ToString();
                yield return "TIMES";
            }
        }

        public override object VisitGroupType(GroupType group, IEnumerable<string> modifiers)
        {
            group.LeadingType?.Accept(this, null);
            WriteModifiers(modifiers);
            if (_endDeclarationWithDot) Output.Write('.');

            //Remember if we are at root level to restore _insideGroupType at the end of the visit
            bool isRoot = !_insideGroupType;

            //Enter the GroupType itself
            _insideGroupType = true;
            _currentIndentLevel++;
            VariableSymbol[] fields = group.Fields.ToArray();
            for (int i = 0; i < fields.Length; i++)
            {
                Output.WriteLine();
                fields[i].Accept(this, null);
                //Dot must be added manually except for the last child
                if (_endDeclarationWithDot && i != fields.Length - 1) Output.Write('.');
            }
            _currentIndentLevel--;
            _insideGroupType = !isRoot;

            return null;
        }

        public override object VisitTypedefType(TypedefType typedef, IEnumerable<string> modifiers)
        {
            //Do not expand TypedefType here ! They may be cyclic.
            Output.Write(' ');
            Output.Write(typedef.Symbol.Name);
            return null;
        }

        #endregion
    }
}
