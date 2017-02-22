using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Server
{
    /// <summary>
    /// CLI class contains runOnce method & other private methods to parse.
    /// </summary>
    class CLI
    {
        /// <summary>
        /// runOnce method to parse the input file(s).
        /// </summary>
        /// <param name="config">Config</param>
        internal static void runOnce(Config config)
        {
            TextWriter w;
            if (config.ErrorFile == null) w = Console.Error;
            else
            {
                if (File.Exists(config.ErrorFile))
                    w = new StreamWriter(config.ErrorFile);
                else
                    w = File.CreateText(config.ErrorFile);
            }
            AbstractErrorWriter writer;
            if (config.IsErrorXML) writer = new XMLWriter(w);
            else writer = new ConsoleWriter(w);
            writer.Outputs = config.OutputFiles;

            var parser = new Parser();
            parser.CustomSymbols = LoadCopies(writer, config.Copies, config.Format);

            for (int c = 0; c < config.InputFiles.Count; c++)
            {
                string path = config.InputFiles[c];
                try { parser.Init(path, config.Format, config.CopyFolders); }
                catch (Exception ex)
                {
                    AddError(writer, ex.Message, path, "parserInit");
                    continue;
                }
                parser.Parse(path);

                if (parser.Results.CodeElementsDocumentSnapshot == null)
                {
                    AddError(writer, "File \"" + path + "\" has syntactic error(s) preventing codegen (CodeElements).", path, "parsing");
                    continue;
                }
                else if (parser.Results.ProgramClassDocumentSnapshot == null)
                {
                    AddError(writer, "File \"" + path + "\" has semantic error(s) preventing codegen (ProgramClass).", path, "parsing");
                    continue;
                }
                var allDiags = parser.Results.AllDiagnostics();
                int errors = allDiags.Count;
                writer.AddErrors(path, parser.Converter.AsDiagnostics(allDiags));

                if (config.Codegen && errors == 0)
                {
                    var skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
#if GENERATOR2
					var codegen = new TypeCobol.Codegen.Generators.DefaultGenerator(parser, new StreamWriter(config.OutputFiles[c]), skeletons);
#else
                    var codegen = new TypeCobol.Codegen.Generator(new StreamWriter(config.OutputFiles[c]), parser.Results.TokensLines, skeletons);
#endif
                    var program = parser.Results.ProgramClassDocumentSnapshot.Program;
                    codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, ColumnsLayout.CobolReferenceFormat);
                }
            }
            writer.Write();
            writer.Flush();
            w.Flush();
            w.Dispose();
            w.Close();
        }

        /// <summary>
        /// LoadCopies method.
        /// </summary>
        /// <param name="writer">AbstractErrorWriter</param>
        /// <param name="paths">List<string></param>
        /// <param name="copyDocumentFormat">DocumentFormat</param>
        /// <returns>SymbolTable</returns>
        private static SymbolTable LoadCopies(AbstractErrorWriter writer, List<string> paths, DocumentFormat copyDocumentFormat)
        {
            var parser = new Parser();
            var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);

            var copies = new List<string>();
            foreach (string path in paths) copies.AddRange(Tools.FileSystem.GetFiles(path, parser.Extensions, false));

            foreach (string path in copies)
            {
                try
                {
                    parser.Init(path, copyDocumentFormat);
                    parser.Parse(path);

                    foreach (var diagnostic in parser.Results.AllDiagnostics())
                    {
                        AddError(writer, "Error during parsing of " + path + ": " + diagnostic, path,
                            "intrinsicLoading");
                    }
                    if (parser.Results.ProgramClassDocumentSnapshot.Program == null)
                    {
                        AddError(writer, "Error: Your Intrisic types/functions are not included into a program.", path,
                            "intrinsicLoading");
                        continue;
                    }

                    var symbols = parser.Results.ProgramClassDocumentSnapshot.Program.SymbolTable;
                    foreach (var types in symbols.Types)
                        foreach (var type in types.Value)
                            table.AddType((Compiler.Nodes.TypeDefinition)type);
                    foreach (var functions in symbols.Functions)
                        foreach (var function in functions.Value)
                            table.AddFunction((Compiler.Nodes.FunctionDeclaration)function);
                    //TODO check if types or functions are already there
                }
                catch (Exception e)
                {
                    AddError(writer, e.Message + "\n" + e.StackTrace, path, "intrinsicLoading");
                }
            }
            return table;
        }

        /// <summary>
        /// AddError method to collect errors.
        /// </summary>
        /// <param name="writer">AbstractErrorWriter</param>
        /// <param name="message">string</param>
        /// <param name="path">string</param>
        /// <param name="errorCode">string</param>
        private static void AddError(AbstractErrorWriter writer, string message, string path, string errorCode)
        {
            var error = new TypeCobol.Tools.Diagnostic();
            error.Message = message;
            error.Code = errorCode;
            try { error.Source = writer.Inputs[path]; }
            catch (KeyNotFoundException) { error.Source = writer.Count.ToString(); }
            writer.AddErrors(path, error);
            Console.WriteLine(error.Message);
        }

        /// <summary>
        /// CreateFormat method to get the format name.
        /// </summary>
        /// <param name="encoding">string</param>
        /// <param name="config">Config</param>
        /// <returns>DocumentFormat</returns>
        internal static Compiler.DocumentFormat CreateFormat(string encoding, ref Config config)
        {
            config.EncFormat = encoding;

            if (encoding == null) return null;
            if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
            if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
            /*if (encoding.ToLower().Equals("rdz"))*/
            return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
        }
    }
}
