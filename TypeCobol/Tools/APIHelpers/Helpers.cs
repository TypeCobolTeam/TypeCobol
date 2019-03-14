using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.CustomExceptions;
using String = System.String;

namespace TypeCobol.Tools.APIHelpers
{
    public static class Helpers
    {
        public static SymbolTable LoadIntrinsic(List<string> paths, DocumentFormat intrinsicDocumentFormat, EventHandler<DiagnosticsErrorEvent> diagEvent)
        {
            var parser = new Parser();
            var diagnostics = new List<Diagnostic>();
            var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
            var instrincicFiles = new List<string>();

            foreach (string path in paths) instrincicFiles.AddRange(FileSystem.GetFiles(path, parser.Extensions, false));

            foreach (string path in instrincicFiles)
            {
                try
                {
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.CrossCheck }, intrinsicDocumentFormat);
                    parser.Parse(path);

                    diagnostics.AddRange(parser.Results.AllDiagnostics());

                    if (diagEvent != null && diagnostics.Count > 0)
                    {
                        diagnostics.ForEach(d => diagEvent(null, new DiagnosticsErrorEvent() { Path = path, Diagnostic = d }));
                    }

                    if (parser.Results.ProgramClassDocumentSnapshot.Root.Programs == null || parser.Results.ProgramClassDocumentSnapshot.Root.Programs.Count() == 0)
                    {
                        throw new CopyLoadingException("Your Intrisic types/functions are not included into a program.", path, null, logged: true, needMail: false);
                    }

                    foreach (var program in parser.Results.ProgramClassDocumentSnapshot.Root.Programs)
                    {
                        var symbols = program.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);

                        if (symbols.Types.Count == 0 && symbols.Functions.Count == 0)
                        {
                            diagEvent?.Invoke(null, new DiagnosticsErrorEvent() { Path = path, Diagnostic = new ParserDiagnostic("No types and no procedures/functions found", 1, 1, 1, null, MessageCode.Warning) });
                            continue;
                        }

                        table.CopyAllTypes(symbols.Types);
                        table.CopyAllFunctions(symbols.Functions);
                    }
                }
                catch (CopyLoadingException copyException)
                {
                    throw copyException;
                }
                catch (Exception e)
                {
                    throw new CopyLoadingException(e.Message + "\n" + e.StackTrace, path, e, logged: true, needMail: true);
                }
            }
            return table;
        }

        public static SymbolTable LoadDependencies([NotNull] List<string> paths, DocumentFormat format, SymbolTable intrinsicTable,
            [NotNull] List<string> inputFiles, List<string> copyFolders, EventHandler<DiagnosticsErrorEvent> diagEvent)
        {

            var parser = new Parser(intrinsicTable);
            var diagnostics = new List<Diagnostic>();
            var table = new SymbolTable(intrinsicTable, SymbolTable.Scope.Namespace); //Generate a table of NameSPace containing the dependencies programs based on the previously created intrinsic table. 

            var dependencies = new List<string>();
            string[] extensions = { ".tcbl", ".cbl", ".cpy" };
            foreach (var path in paths)
            {
                var dependenciesFound = Tools.FileSystem.GetFiles(path, extensions, true);
                //Issue #668, warn if dependencies path are invalid
                if (diagEvent != null && dependenciesFound.Count == 0)
                {
                    diagEvent(null, new DiagnosticsErrorEvent() { Path = path, Diagnostic = new ParserDiagnostic(path + ", no dependencies found", 1, 1, 1, null, MessageCode.DependenciesLoading) });
                }
                dependencies.AddRange(dependenciesFound); //Get File by name or search the directory for all files
            }

#if EUROINFO_RULES
            //Create list of inputFileName according to our naming convention in the case of an usage with RDZ
            var programsNames = new List<string>();
            foreach (var inputFile in inputFiles)
            {
                string PgmName = null;
                foreach (var line in File.ReadLines(inputFile))
                {
                    if (line.StartsWith("       PROGRAM-ID.", StringComparison.InvariantCultureIgnoreCase))
                    {
                        PgmName = line.Split('.')[1].Trim();
                        break;
                    }
                }

                programsNames.Add(PgmName);

            }
#endif

            foreach (string path in dependencies)
            {


#if EUROINFO_RULES
                //Issue #583, ignore a dependency if the same file will be parsed as an input file just after

                string depFileNameRaw = Path.GetFileNameWithoutExtension(path).Trim();

                if (depFileNameRaw != null)
                {
                    // substring in case of MYPGM.rdz.tcbl
                    var depFileName = depFileNameRaw.Substring(0,
                        depFileNameRaw.IndexOf(".", StringComparison.Ordinal) != -1 ?
                            depFileNameRaw.IndexOf(".", StringComparison.Ordinal) :
                            depFileNameRaw.Length
                    );
                    if (programsNames.Any(inputFileName => String.Compare(depFileName, inputFileName, StringComparison.OrdinalIgnoreCase) == 0))
                    {
                        continue;

                    }
                }
#endif
                try
                {
                    parser.CustomSymbols = table; //Update SymbolTable
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCheck }, format, copyFolders);
                    parser.Parse(path); //Parse the dependency file

                    diagnostics.AddRange(parser.Results.AllDiagnostics());

                    if (diagEvent != null && diagnostics.Count > 0)
                    {
                        diagnostics.ForEach(d => diagEvent(null, new DiagnosticsErrorEvent() { Path = path, Diagnostic = d }));
                    }

                    if (parser.Results.TemporaryProgramClassDocumentSnapshot.Root.Programs == null || !parser.Results.TemporaryProgramClassDocumentSnapshot.Root.Programs.Any())
                    {
                        throw new DepedenciesLoadingException("Your dependency file is not included into a program", path, null, logged: true, needMail: false);
                    }

                    foreach (var program in parser.Results.TemporaryProgramClassDocumentSnapshot.Root.Programs)
                    {
                        var declarationTable = program.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);
                        var globalTable = program.SymbolTable.GetTableFromScope(SymbolTable.Scope.Global);

                        var previousPrograms = table.GetPrograms();
                        foreach (var previousProgram in previousPrograms)
                        {
                            previousProgram.SymbolTable.GetTableFromScope(SymbolTable.Scope.Namespace).AddProgram(program);
                        }


                        //If there is no public types or functions, then call diagEvent
                        if (diagEvent != null
                            && !globalTable.Types.Values.Any(tds => tds.Any(td => td.CodeElement.Visibility == AccessModifier.Public))            //No Public Types in Global table
                            && !declarationTable.Types.Values.Any(tds => tds.Any(td => td.CodeElement.Visibility == AccessModifier.Public))       //No Public Types in Declaration table
                            && !declarationTable.Functions.Values.Any(fds => fds.Any(fd => fd.CodeElement.Visibility == AccessModifier.Public)))  //No Public Functions in Declaration table
                        {
                            diagEvent(null, new DiagnosticsErrorEvent() { Path = path, Diagnostic = new ParserDiagnostic(string.Format("No public types or procedures/functions found in {0}", program.Name), 1, 1, 1, null, MessageCode.Warning) });
                            continue;
                        }
                        table.AddProgram(program); //Add program to Namespace symbol table
                    }
                }
                catch (DepedenciesLoadingException depLoadingEx)
                {
                    throw depLoadingEx;
                }
                catch (Exception e)
                {
                    throw new DepedenciesLoadingException(e.Message + "\n" + e.StackTrace, path, e);
                }
            }

            //Reset symbolTable of all dependencies 



            return table;
        }
    }

    public class DiagnosticsErrorEvent : EventArgs
    {
        public string Path { get; set; }
        public Diagnostic Diagnostic { get; set; }
    }
}
