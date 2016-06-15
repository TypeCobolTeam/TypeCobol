using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics {


    class ReadOnlyPropertiesChecker : ProgramListener
    {

        private static string[] READONLY_DATATYPES = { "TC-DATE"};

        public IList<Type> GetCodeElements()
        {
            return new List<Type> { typeof(TypeCobol.Compiler.CodeModel.SymbolWriter), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext c, Program program)
        {
            var element = e as TypeCobol.Compiler.CodeModel.SymbolWriter;
            var table = program.SymbolTable;
            foreach (var pair in element.Symbols)
            {
                if (pair.Item2 == null) continue; // no receiving item
                var lr = table.Get(pair.Item2);
                if (lr.Count != 1) continue; // ambiguity or not referenced; not my job
                var receiving = lr[0];
                checkReadOnly(e, receiving);
            }
        }

        private static void checkReadOnly(CodeElement e, DataDescriptionEntry receiving)
        {
            if (receiving.TopLevel == null) return;
            if (receiving.TopLevel.DataType == null) return;
            foreach (var type in READONLY_DATATYPES)
            {
                if (type.Equals(receiving.TopLevel.DataType.Name.ToLower()))
                {
                    DiagnosticUtils.AddError(e, type + " properties are read-only");
                }
            }
        }
    }

    class FunctionChecker : ProgramListener
    {
        public IList<Type> GetCodeElements()
        {
            return new List<Type> { typeof(TypeCobol.Compiler.CodeModel.IdentifierUser), };
        }

        public void OnCodeElement(CodeElement e, ParserRuleContext context, Program program)
        {
            var element = e as TypeCobol.Compiler.CodeModel.IdentifierUser;
            foreach (var identifier in element.Identifiers)
            {
                CheckIdentifier(e, program.SymbolTable, identifier);
            }
        }

        private static void CheckIdentifier(CodeElement e, SymbolTable table, Identifier identifier)
        {
            var fun = identifier as FunctionReference;
            if (fun == null) return;// we only check functions
            var def = table.GetFunction(fun.Name);
            if (def == null) return;// ambiguity is not our job
            if (fun.Parameters.Count > def.Parameters.Count)
                DiagnosticUtils.AddError(e,
                    "Function " + def.Name + " only takes " + def.Parameters.Count + " parameters");
            for (int c = 0; c < def.Parameters.Count; c++)
            {
                var expected = def.Parameters[c];
                if (c < fun.Parameters.Count)
                {
                    var actual = fun.Parameters[c].Value;
                    if (actual is Identifier)
                    {
                        var found = table.Get(((Identifier)actual).Name);
                        if (found.Count != 1) continue;// ambiguity is not our job
                        var type = found[0].DataType;
                        try
                        {
                            if (!type.Name.ToUpper().Equals(expected.Type.Name.ToUpper()))
                            {
                                //Console.WriteLine("param=" + expected);
                                //Console.WriteLine("param.Definition=" + expected.Definition);
                                DiagnosticUtils.AddError(e,
                                    "Function " + def.Name + " expected parameter " + (c + 1) + " of type " +
                                    expected.Type + " (actual: " + type + ')');
                            }
                        }
                        catch (NullReferenceException)
                        {
                            //var length = found[0].MemoryArea.Length;
                            //if (length > expected.Length)
                            //	DiagnosticUtils.AddError(e, "Function "+def.Name+" expected parameter "+(c+1)+" of max length "+expected.Length+" (actual: "+length+')');
                        }
                    }
                    else
                    {
                        DiagnosticUtils.AddError(e, "Function " + def.Name + " is missing parameter " + (c + 1) + " of type " + expected.Type);
                    }
                }
            }
        }
    }


}