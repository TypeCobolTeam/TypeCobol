using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Domain.Validator;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Test.Domain
{
    [TestClass]
    public class SemanticDomainTest
    {
        private class FailErrorReporter : IValidationErrorReporter
        {
            public static readonly FailErrorReporter Instance = new FailErrorReporter();

            private FailErrorReporter()
            {

            }

            public void Report(ValidationError validationError)
            {
                Assert.Fail(validationError.Message);
            }
        }

        private class CompareMessagesErrorReporter : IValidationErrorReporter
        {
            private readonly string[] _expectedMessages;
            private int _index;

            public CompareMessagesErrorReporter(params string[] expectedMessages)
            {
                _expectedMessages = expectedMessages ?? new string[0];
                _index = 0;
            }

            public void Report(ValidationError validationError)
            {
                Assert.IsTrue(_index < _expectedMessages.Length);
                Assert.AreEqual(_expectedMessages[_index++], validationError.Message);
            }
        }

        private static string GetTestLocation()
        {
            return Path.Combine(Directory.GetCurrentDirectory(), @"..\..\TypeCobol.Test");
        }

        /// <summary>
        /// This test test that the REDEFINES that uses a variable from a TYPEDEF is correctly expanded.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void ExpanderRedefines()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "ExpanderRedefines.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            var vars = currentProgram.ResolveReference(new string[] { "idt" }, false);
            Assert.IsTrue(vars.Count == 1);
        }

        /// <summary>
        /// This Test tests the expansion of a Type Currency inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void CurrencyTypeExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Type-Currency.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Get oldCurrency symbol
            var oldCurrency = currentProgram.ResolveReference(new string[] { "oldCurrency" }, false);
            Assert.IsTrue(oldCurrency.Count == 1);
            Assert.IsNotNull(oldCurrency.Symbol.Type);
            var oldCurrencyOriginalType = oldCurrency.Symbol.Type;

            //Get myCurrency1 symbol
            var myCurrency1 = currentProgram.ResolveReference(new string[] { "myCurrency1" }, false);
            Assert.IsTrue(myCurrency1.Count == 1);
            Assert.IsNotNull(myCurrency1.Symbol.Type);
            Assert.IsTrue(myCurrency1.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(myCurrency1.Symbol.Type == BuiltinTypes.CurrencyType);

            //Get myCurrency2 symbol
            var myCurrency2 = currentProgram.ResolveReference(new string[] { "myCurrency2" }, false);
            Assert.IsTrue(myCurrency2.Count == 1);
            Assert.IsNotNull(myCurrency2.Symbol.Type);
            Assert.IsTrue(myCurrency2.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(myCurrency2.Symbol.Type == BuiltinTypes.CurrencyType);

            //Get myCurrency3 symbol
            var myCurrency3 = currentProgram.ResolveReference(new string[] { "myCurrency3" }, false);
            Assert.IsTrue(myCurrency3.Count == 1);
            Assert.IsNotNull(myCurrency3.Symbol.Type);
            Assert.IsTrue(myCurrency3.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(myCurrency3.Symbol.Type == BuiltinTypes.CurrencyType);

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);
            //After expansion
            Assert.AreEqual(oldCurrencyOriginalType, oldCurrency.Symbol.Type);

            Assert.IsNotNull(myCurrency1.Symbol.Type);
            Assert.IsTrue(myCurrency1.Symbol.Type.Tag == Type.Tags.Picture);
            Assert.IsTrue(myCurrency1.Symbol.Type == BuiltinTypes.CurrencyType.TypeComponent);

            Assert.IsNotNull(myCurrency2.Symbol.Type);
            Assert.IsTrue(myCurrency2.Symbol.Type.Tag == Type.Tags.Picture);
            Assert.IsTrue(myCurrency2.Symbol.Type == BuiltinTypes.CurrencyType.TypeComponent);

            Assert.IsNotNull(myCurrency3.Symbol.Type);
            Assert.IsTrue(myCurrency3.Symbol.Type.Tag == Type.Tags.Picture);
            Assert.IsTrue(myCurrency3.Symbol.Type == BuiltinTypes.CurrencyType.TypeComponent);
        }

        /// <summary>
        /// This Test tests the expansion of a Type Date inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void DateTypeExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Type-Date.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Get olddate symbol
            var olddate = currentProgram.ResolveReference(new string[] { "olddate" }, false);
            Assert.IsTrue(olddate.Count == 1);
            Assert.IsNotNull(olddate.Symbol.Type);
            var olddateOriginalType = olddate.Symbol.Type;

            //Get today symbol
            var today = currentProgram.ResolveReference(new string[] { "today" }, false);
            Assert.IsTrue(today.Count == 1);
            Assert.IsNotNull(today.Symbol.Type);
            Assert.IsTrue(today.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(today.Symbol.Type == BuiltinTypes.DateType);

            //Before expansion there are no YYYY, MM, DD variables in the program
            var yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 0);
            var mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 0);
            var dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 0);

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);
            Assert.AreEqual(olddateOriginalType, olddate.Symbol.Type);
            Type te_today = today.Symbol.Type;
            Assert.IsNotNull(te_today);
            Assert.IsTrue(te_today.Tag == Type.Tags.Group);

            //After expansion there are YYYY, MM, DD variables in the program
            yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 3);
            mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 3);
            dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 3);
//            string @dump_today_type = @"  02 YYYY PIC 9(4).
//  02 MM PIC 9(2).
//  02 DD PIC 9(2).
//";
//            string te_today_string = te_today.ToString();
//            Assert.AreEqual(dump_today_type, te_today_string);

//            string dump_today = @"01 today .
//    02 YYYY PIC 9(4).
//    02 MM PIC 9(2).
//    02 DD PIC 9(2).
//";

//            string today_str = today.Symbol.ToString();
//            Assert.AreEqual(dump_today, today_str);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "GlobalStorage")]
        public void GlobalStorageVisibilityCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "GlobalStorageVisibility.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null, executionStep:ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            Assert.IsTrue(currentProgram.Programs.Count() == 1);
            var nestedPrg = currentProgram.Programs.Single();

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            TypeCobol.Compiler.Scopes.Container<VariableSymbol>.Entry result;

            // Main pgm
            result = currentProgram.ResolveReference(new[] { "var1" }, true);
            Assert.IsTrue(result.Count == 2);
            result = currentProgram.ResolveReference(new[] { "XX", "MyPoint" }, true);
            Assert.IsTrue(result.Count == 2);

            // Nested pgm
            result = nestedPrg.ResolveReference(new[] { "var1" }, true);
            Assert.IsTrue(result.Count == 2);
            result = nestedPrg.ResolveReference(new[] { "var2" }, true);
            Assert.IsTrue(result.Count == 2);
            result = nestedPrg.ResolveReference(new[] { "var3" }, true);
            Assert.IsTrue(result.Count == 2);
            result = nestedPrg.ResolveReference(new[] { "var4" }, true);
            Assert.IsTrue(result.Count == 1);
            result = nestedPrg.ResolveReference(new[] { "XX", "MyPoint" }, true);
            Assert.IsTrue(result.Count == 2);
        }

        /// <summary>
        /// This Test tests the expansion of all Types Date inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void AllDateTypeExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Type-Date.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Before expansion there are no YYYY, MM, DD variables in the program
            var yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 0);
            var mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 0);
            var dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 0);

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            //After expansion there are now YYYY, MM, DD variables in the program
            yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 3);
            mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 3);
            dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 3);

            var today_yyyy = currentProgram.ResolveReference(new string[] { "yyyy", "today" }, false);
            Assert.IsTrue(today_yyyy.Count == 1);
            string today_yyyy_fullname = today_yyyy.Symbol.FullName;
            Assert.AreEqual("TEST-DATE::today::YYYY", today_yyyy_fullname);

            var tomorrow_yyyy = currentProgram.ResolveReference(new string[] { "yyyy", "tomorrow" }, false);
            Assert.IsTrue(tomorrow_yyyy.Count == 1);
            string tomorrow_yyyy_fullname = tomorrow_yyyy.Symbol.FullName;
            Assert.AreEqual("TEST-DATE::tomorrow::YYYY", tomorrow_yyyy_fullname);

            var date1_yyyy = currentProgram.ResolveReference(new string[] { "yyyy", "date1" }, false);
            Assert.IsTrue(date1_yyyy.Count == 1);
            string date1_yyyy_fullname = date1_yyyy.Symbol.FullName;
            Assert.AreEqual("TEST-DATE::groupe::date1::YYYY", date1_yyyy_fullname);

            Assert.AreEqual("YYYY OF today OF TEST-DATE", today_yyyy.Symbol.FullOfName);            
            Assert.AreEqual("YYYY OF tomorrow OF TEST-DATE", tomorrow_yyyy.Symbol.FullOfName);            
            Assert.AreEqual("YYYY OF date1 OF groupe OF TEST-DATE", date1_yyyy.Symbol.FullOfName);
        }

        /// <summary>
        /// This Test tests that a Type Bool is not expanded in a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void BoolTypeNotExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Type-Bool.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Before expansion there are no check-false variables in the program
            var check_false = currentProgram.ResolveReference(new string[] { "check-false" }, false);
            Assert.IsTrue(check_false.Count == 0);
            var managed_false = currentProgram.ResolveReference(new string[] { "managed-false" }, false);
            Assert.IsTrue(check_false.Count == 0);

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            //After expansion there are no check-false variables in the program
            check_false = currentProgram.ResolveReference(new string[] { "check-false" }, false);
            Assert.IsTrue(check_false.Count == 0);
            managed_false = currentProgram.ResolveReference(new string[] { "managed-false" }, false);
            Assert.IsTrue(check_false.Count == 0);

            //Finally check that variables "check" and "managed" are of the Builtin type Bool.
            var check = currentProgram.ResolveReference(new string[] { "check" }, false);
            Assert.IsTrue(check.Count == 1);
            Assert.IsNotNull(check.Symbol.Type);
            Assert.IsNotNull(check.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.AreEqual(check.Symbol.Type, BuiltinTypes.BooleanType);

            //Finally check that variables "check" and "managed" are of the Builtin type Bool.
            var managed = currentProgram.ResolveReference(new string[] { "managed" }, false);
            Assert.IsTrue(managed.Count == 1);
            Assert.IsNotNull(managed.Symbol.Type);
            Assert.IsNotNull(managed.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.AreEqual(managed.Symbol.Type, BuiltinTypes.BooleanType);
        }

        /// <summary>
        /// This Test tests expanding Basic TypeDef types in a program..
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void BasicTypeExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Typedef-public.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;

            //Get the nested program.
            var nestPrgs = mainProgram.GetChildren<NestedProgram>();
            var nestPrg = nestPrgs[0];
            Assert.AreEqual(nestPrg.Name, "Tester");
            ISemanticData data0 = nestPrg.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Program);
            ProgramSymbol nestPrgSym = (ProgramSymbol)symbol0;
            Assert.AreEqual(nestPrgSym.Name, "Tester");

            ///---------------------------
            //Get all TYPEDEF Types
            //----------------------------
            //Lookup the type "typeOfDaysPublic"
            var typeOfDaysPublic = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysPublic" });
            Assert.IsNotNull(typeOfDaysPublic);
            Assert.IsTrue(typeOfDaysPublic.Count == 1);
            Assert.IsNotNull(typeOfDaysPublic.Symbol.Type);
            Assert.IsTrue(typeOfDaysPublic.Symbol.HasFlag(Symbol.Flags.Public | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysPublic.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysPublic.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Lookup the type "typeOfDaysPrivate"
            var typeOfDaysPrivate = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysPrivate" });
            Assert.IsNotNull(typeOfDaysPrivate);
            Assert.IsTrue(typeOfDaysPrivate.Count == 1);
            Assert.IsNotNull(typeOfDaysPrivate.Symbol.Type);
            Assert.IsFalse(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Lookup the type "typeOfDaysLocal"
            var typeOfDaysLocal = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysLocal" });
            Assert.IsNotNull(typeOfDaysLocal);
            Assert.IsTrue(typeOfDaysLocal.Count == 1);
            Assert.IsNotNull(typeOfDaysLocal.Symbol.Type);
            Assert.IsFalse(typeOfDaysLocal.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysLocal.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysLocal.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysLocal.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Before expanding, all variables have a TYPEDEF Type
            TypedefSymbol[] types = new TypedefSymbol[] { typeOfDaysPublic.Symbol, typeOfDaysPrivate.Symbol, typeOfDaysLocal.Symbol };
            VariableSymbol[] vars = new VariableSymbol[3];
            for (int i = 1; i < 4; i++)
            {
                var vari = nestPrgSym.ResolveReference(new string[] { "var" + i }, false);                
                Assert.IsTrue(vari.Count == (i == 3 ? 1 : 3));
                vars[i - 1] = vari.Symbol;
                Assert.IsTrue(vari.Symbol.Type == types[i - 1].Type);
            }

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            //After expanding all variables have a PICTURE type.
            for (int i = 1; i < 4; i++)
            {
                Assert.IsNotNull(vars[i - 1].Type);
                Assert.IsTrue(vars[i - 1].Type.Tag == Type.Tags.Picture);
            }

//            string expandedPrgs = @"IDENTIFICATION DIVISION.
//PROGRAM-ID. TYPEPGM.
//DATA DIVISION.

//PROCEDURE DIVISION.
  

//IDENTIFICATION DIVISION.
//PROGRAM-ID. Tester.
//DATA DIVISION.
//WORKING-STORAGE SECTION.
//01 var1 PIC X(1).
//01 var1 PIC X(1).
//01 var1 PIC X(1).
//01 var2 PIC X(1).
//01 var2 PIC X(1).
//01 var2 PIC X(1).
//01 var3 PIC X(1).


//PROCEDURE DIVISION.

//END PROGRAM Tester.
//END PROGRAM TYPEPGM.";
//            string dumpPrg = currentProgram.ToString();
//            Assert.AreEqual(expandedPrgs, dumpPrg);
        }

        /// <summary>
        /// This Test tests complex type expanding.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void ComplexTypeExpanderCheck()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "ComplexTypeExpand.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //-------------------------
            // rcarray variable.
            //-------------------------
            var rcarray = currentProgram.ResolveReference(new string[] { "rcarray" }, false);
            Assert.IsTrue(rcarray.Count == 1);
            Assert.IsNotNull(rcarray.Symbol.Type);
            Assert.IsTrue(rcarray.Symbol.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(rcarray.Symbol.Type.TypeComponent.Tag == Type.Tags.Typedef);

            //Before expanding there were no X1, Y1, X2, Y2 variables in the program.
            var x1 = currentProgram.ResolveReference(new string[] { "x1" }, false);
            Assert.IsTrue(x1.Count == 0);
            var X1 = currentProgram.ResolveReference(new string[] { "X1" }, false);
            Assert.IsTrue(X1.Count == 0);
            var y1 = currentProgram.ResolveReference(new string[] { "y1" }, false);
            Assert.IsTrue(y1.Count == 0);
            var Y1 = currentProgram.ResolveReference(new string[] { "Y1" }, false);
            Assert.IsTrue(Y1.Count == 0);
            var x2 = currentProgram.ResolveReference(new string[] { "x2" }, false);
            Assert.IsTrue(x2.Count == 0);
            var X2 = currentProgram.ResolveReference(new string[] { "X2" }, false);
            Assert.IsTrue(X2.Count == 0);
            var y2 = currentProgram.ResolveReference(new string[] { "y2" }, false);
            Assert.IsTrue(y2.Count == 0);
            var Y2 = currentProgram.ResolveReference(new string[] { "Y2" }, false);
            Assert.IsTrue(Y2.Count == 0);

            //-------------------------
            // rcpt variable.
            //-------------------------
            var rcpt = currentProgram.ResolveReference(new string[] { "rcpt" }, false);
            Assert.IsTrue(rcpt.Count == 1);
            Assert.IsNotNull(rcpt.Symbol.Type);
            Assert.IsTrue(rcpt.Symbol.Type.Tag == Type.Tags.Typedef);

            //Before expanding there were no x, y, pt1, pt2 variables in the program.
            var x = currentProgram.ResolveReference(new string[] { "x" }, false);
            Assert.IsTrue(x.Count == 0);
            var y = currentProgram.ResolveReference(new string[] { "y" }, false);
            Assert.IsTrue(y.Count == 0);
            var pt1 = currentProgram.ResolveReference(new string[] { "pt1" }, false);
            Assert.IsTrue(pt1.Count == 0);
            var pt2 = currentProgram.ResolveReference(new string[] { "pt2" }, false);
            Assert.IsTrue(pt2.Count == 0);

            //-------------------------
            // grcarray variable.
            //-------------------------
            var grcarray = currentProgram.ResolveReference(new string[] { "grcarray" }, false);
            Assert.IsTrue(grcarray.Count == 1);
            Assert.IsNotNull(grcarray.Symbol.Type);
            Assert.IsTrue(grcarray.Symbol.Type.Tag == Type.Tags.Group);

            //Before expansion
            //____________________
            //There is one rc variable of type PT in the program .
            var rc = currentProgram.ResolveReference(new string[] { "rc" }, false);
            Assert.IsTrue(rc.Count == 1);
            Assert.IsNotNull(rc.Symbol.Type);
            Assert.IsTrue(rc.Symbol.Type.Tag == Type.Tags.Typedef);
            var pt = currentProgram.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "pt" });
            Assert.IsTrue(pt.Count == 1);
            Assert.IsTrue(rc.Symbol.Type == pt.Symbol.Type);

            //There is one arr variable of type RECTARRAY in the program .
            var arr = currentProgram.ResolveReference(new string[] { "arr" }, false);
            Assert.IsTrue(arr.Count == 1);
            Assert.IsNotNull(arr.Symbol.Type);
            Assert.IsTrue(arr.Symbol.Type.Tag == Type.Tags.Typedef);
            var rectarray = currentProgram.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "rectarray" });
            Assert.IsTrue(rectarray.Count == 1);
            Assert.IsTrue(arr.Symbol.Type == rectarray.Symbol.Type);

            //Perform expansion
            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            //Now rcarray is an array of Record.
            Assert.IsNotNull(rcarray.Symbol.Type);
            Assert.IsTrue(rcarray.Symbol.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(rcarray.Symbol.Type.TypeComponent.Tag == Type.Tags.Group);

            //After expanding there are X1, Y1, X2, Y2 variables in the program.
            x1 = currentProgram.ResolveReference(new string[] { "x1" }, false);
            Assert.IsTrue(x1.Count == 1);
            Assert.IsNotNull(x1.Symbol.Type);
            X1 = currentProgram.ResolveReference(new string[] { "X1" }, false);
            Assert.IsTrue(X1.Count == 1);
            Assert.IsNotNull(X1.Symbol.Type);
            Assert.IsNotNull(x1.Symbol == X1.Symbol);
            y1 = currentProgram.ResolveReference(new string[] { "y1" }, false);
            Assert.IsTrue(y1.Count == 1);
            Assert.IsNotNull(y1.Symbol.Type);
            Y1 = currentProgram.ResolveReference(new string[] { "Y1" }, false);
            Assert.IsTrue(Y1.Count == 1);
            Assert.IsNotNull(Y1.Symbol.Type);
            Assert.IsNotNull(y1.Symbol == Y1.Symbol);
            x2 = currentProgram.ResolveReference(new string[] { "x2" }, false);            
            Assert.IsTrue(x2.Count == 1);
            Assert.IsNotNull(x2.Symbol.Type);
            X2 = currentProgram.ResolveReference(new string[] { "X2" }, false);
            Assert.IsTrue(X2.Count == 1);
            Assert.IsNotNull(X2.Symbol.Type);
            Assert.IsNotNull(x2.Symbol == X2.Symbol);
            y2 = currentProgram.ResolveReference(new string[] { "y2" }, false);            
            Assert.IsTrue(y2.Count == 1);
            Assert.IsNotNull(y2.Symbol.Type);
            Y2 = currentProgram.ResolveReference(new string[] { "Y2" }, false);
            Assert.IsTrue(Y2.Count == 1);
            Assert.IsNotNull(Y2.Symbol.Type);
            Assert.IsNotNull(y2.Symbol == Y2.Symbol);

            var rcarray_x1 = currentProgram.ResolveReference(new string[] { "x1", "rcarray" }, false);
            Assert.IsTrue(rcarray_x1.Count == 1);
            Assert.IsTrue(rcarray_x1.Symbol == x1.Symbol);
            var rcarray_y1 = currentProgram.ResolveReference(new string[] { "y1", "rcarray" }, false);
            Assert.IsTrue(rcarray_y1.Count == 1);
            Assert.IsTrue(rcarray_y1.Symbol == y1.Symbol);
            var rcarray_x2 = currentProgram.ResolveReference(new string[] { "x2", "rcarray" }, false);
            Assert.IsTrue(rcarray_x2.Count == 1);
            Assert.IsTrue(rcarray_x2.Symbol == x2.Symbol);
            var rcarray_y2 = currentProgram.ResolveReference(new string[] { "y2", "rcarray" }, false);
            Assert.IsTrue(rcarray_y2.Count == 1);
            Assert.IsTrue(rcarray_y2.Symbol == y2.Symbol);

            //Case Sensitive Tests
            var rcArraY_x1 = currentProgram.ResolveReference(new string[] { "X1", "rcArRaY" }, false);
            Assert.IsTrue(rcArraY_x1.Count == 1);
            Assert.IsTrue(rcArraY_x1.Symbol == x1.Symbol);
            var rcArraY_y1 = currentProgram.ResolveReference(new string[] { "Y1", "rcArRaY" }, false);
            Assert.IsTrue(rcArraY_y1.Count == 1);
            Assert.IsTrue(rcArraY_y1.Symbol == y1.Symbol);
            var rcArraY_x2 = currentProgram.ResolveReference(new string[] { "X2", "rcArRaY" }, false);
            Assert.IsTrue(rcArraY_x2.Count == 1);
            Assert.IsTrue(rcArraY_x2.Symbol == x2.Symbol);
            var rcArraY_y2 = currentProgram.ResolveReference(new string[] { "Y2", "rcArRaY" }, false);
            Assert.IsTrue(rcArraY_y2.Count == 1);
            Assert.IsTrue(rcArraY_y2.Symbol == y2.Symbol);

            //Now rcpt is of type Record
            Assert.IsNotNull(rcpt.Symbol.Type);
            Assert.IsTrue(rcpt.Symbol.Type.Tag == Type.Tags.Group);

            //After expanding there are now x, y, pt1, pt2 variables in the program.
            x = currentProgram.ResolveReference(new string[] { "x" }, false);
            Assert.IsTrue(x.Count == 5);
            y = currentProgram.ResolveReference(new string[] { "y" }, false);
            Assert.IsTrue(y.Count == 5);
            pt1 = currentProgram.ResolveReference(new string[] { "pt1" }, false);
            Assert.IsTrue(pt1.Count == 2);
            Assert.IsNotNull(pt1.ElementAt(0).Type);
            Assert.IsNotNull(pt1.ElementAt(0).Type.Tag == Type.Tags.Group);
            Assert.IsNotNull(pt1.ElementAt(1).Type);
            Assert.IsNotNull(pt1.ElementAt(1).Type.Tag == Type.Tags.Group);
            pt2 = currentProgram.ResolveReference(new string[] { "pt2" }, false);
            Assert.IsTrue(pt2.Count == 2);
            Assert.IsNotNull(pt2.ElementAt(0).Type);
            Assert.IsNotNull(pt2.ElementAt(0).Type.Tag == Type.Tags.Group);
            Assert.IsNotNull(pt2.ElementAt(1).Type);
            Assert.IsNotNull(pt2.ElementAt(1).Type.Tag == Type.Tags.Group);

            var pt1_x = currentProgram.ResolveReference(new string[] { "x", "pt1" }, false);
            Assert.IsTrue(pt1_x.Count == 2);
            var pt1_y = currentProgram.ResolveReference(new string[] { "y", "pt1" }, false);
            Assert.IsTrue(pt1_y.Count == 2);
            var pt2_x = currentProgram.ResolveReference(new string[] { "x", "pt2" }, false);
            Assert.IsTrue(pt2_x.Count == 2);
            var pt2_y = currentProgram.ResolveReference(new string[] { "y", "pt2" }, false);
            Assert.IsTrue(pt2_y.Count == 2);

            var rcpt_pt1_x = currentProgram.ResolveReference(new string[] { "x", "pt1", "rcpt" }, false);
            Assert.IsTrue(rcpt_pt1_x.Count == 1);
            var rcpt_pt1_y = currentProgram.ResolveReference(new string[] { "y", "pt1", "rcpt" }, false);
            Assert.IsTrue(rcpt_pt1_y.Count == 1);
            var rcpt_pt2_x = currentProgram.ResolveReference(new string[] { "x", "pt2", "rcpt" }, false);
            Assert.IsTrue(rcpt_pt2_x.Count == 1);
            var rcpt_pt2_y = currentProgram.ResolveReference(new string[] { "y", "pt2", "rcpt" }, false);
            Assert.IsTrue(rcpt_pt2_y.Count == 1);

            Assert.AreNotSame(rcpt_pt1_x.Symbol, rcpt_pt2_x.Symbol);
            Assert.AreNotSame(rcpt_pt1_y.Symbol, rcpt_pt2_y.Symbol);
            Assert.IsTrue((rcpt_pt1_x.Symbol == x.ElementAt(0) && rcpt_pt2_x.Symbol == x.ElementAt(1)) || (rcpt_pt1_x.Symbol == x.ElementAt(1) && rcpt_pt2_x.Symbol == x.ElementAt(0)));
            Assert.IsTrue((rcpt_pt1_y.Symbol == y.ElementAt(0) && rcpt_pt2_y.Symbol == y.ElementAt(1)) || (rcpt_pt1_y.Symbol == y.ElementAt(1) && rcpt_pt2_y.Symbol == y.ElementAt(0)));

            //There are now two rc variables both of type Record .
            var rc_after = currentProgram.ResolveReference(new string[] { "rc" }, false);
            Assert.IsTrue(rc_after.Count == 2);
            Assert.IsNotNull(rc_after.ElementAt(0).Type);
            Assert.IsNotNull(rc_after.ElementAt(0).Type.Tag == Type.Tags.Group);
            Assert.IsNotNull(rc_after.ElementAt(1).Type);
            Assert.IsNotNull(rc_after.ElementAt(1).Type.Tag == Type.Tags.Group);
            Assert.IsTrue(rc_after.ElementAt(0) != rc_after.ElementAt(1));
            var rc_arr = currentProgram.ResolveReference(new string[] { "rc", "arr" }, false);
            Assert.IsTrue(rc_arr.Count == 1);
            Assert.IsTrue(rc_arr.Symbol == rc_after.ElementAt(0) || rc_arr.Symbol == rc_after.ElementAt(1));

            string prgDump = currentProgram.ToString();
        }

        /// <summary>
        /// This test tests Level renumber after symbol typedef expansion.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void LevelRenumberTypeExpand()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "ComplexTypeExpand.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);

            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //-------------------------
            // typedefs.
            //-------------------------
            var pt = currentProgram.Types.Lookup("pt");
            Assert.IsTrue(pt.Count == 1);
            Assert.IsTrue(pt.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(pt.Symbol.Type.TypeComponent.Tag == Type.Tags.Group);
            GroupType ptTypeContent = (GroupType) pt.Symbol.Type.TypeComponent;
            foreach (var field in ptTypeContent.Fields)
            {
                Assert.IsTrue(field.Level == 5);
            }

            var rectangle = currentProgram.Types.Lookup("rectangle");
            Assert.IsTrue(rectangle.Count == 1);
            Assert.IsTrue(rectangle.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(rectangle.Symbol.Type.TypeComponent.Tag == Type.Tags.Group);
            GroupType rectangleTypeContent = (GroupType) rectangle.Symbol.Type.TypeComponent;
            foreach (var field in rectangleTypeContent.Fields)
            {
                Assert.IsTrue(field.Level == 2);
            }

            var rectpt = currentProgram.Types.Lookup("rectpt");
            Assert.IsTrue(rectpt.Count == 1);
            Assert.IsTrue(rectpt.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(rectpt.Symbol.Type.TypeComponent.Tag == Type.Tags.Group);
            GroupType rectptTypeContent = (GroupType) rectpt.Symbol.Type.TypeComponent;
            foreach (var field in rectptTypeContent.Fields)
            {
                Assert.IsTrue(field.Level == 5);
                Assert.IsTrue(field.Type == pt.Symbol.Type);
            }

            var rectarray = currentProgram.Types.Lookup("rectarray");
            Assert.IsTrue(rectarray.Count == 1);
            Assert.IsTrue(rectarray.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(rectarray.Symbol.Type.TypeComponent.Tag == Type.Tags.Group);
            GroupType rectarrayTypeContent = (GroupType) rectarray.Symbol.Type.TypeComponent;
            var aField = rectarrayTypeContent.Fields.Single();
            Assert.IsTrue(aField.Level == 5);
            Assert.IsTrue(aField.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(aField.Type.TypeComponent.Tag == Type.Tags.Group);
            var rcField = ((GroupType) aField.Type.TypeComponent).Fields.Single();
            Assert.IsTrue(rcField.Level == 10);
            Assert.IsTrue(rcField.Type == rectpt.Symbol.Type);

            //-------------------------
            // rcarray variable.
            //-------------------------
            var rcarray = currentProgram.ResolveReference(new string[] { "rcarray" }, false);
            Assert.IsTrue(rcarray.Count == 1);
            Assert.IsNotNull(rcarray.Symbol.Type);
            Assert.IsTrue(rcarray.Symbol.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(rcarray.Symbol.Type.TypeComponent == rectangle.Symbol.Type);

            //-------------------------
            // rcpt variable.
            //-------------------------
            var rcpt = currentProgram.ResolveReference(new string[] { "rcpt" }, false);
            Assert.IsTrue(rcpt.Count == 1);
            Assert.IsNotNull(rcpt.Symbol.Type);
            Assert.IsTrue(rcpt.Symbol.Type == rectpt.Symbol.Type);

            //-------------------------
            // grcarray variable.
            //-------------------------
            var grcarray = currentProgram.ResolveReference(new string[] { "grcarray" }, false);
            Assert.IsTrue(grcarray.Count == 1);
            Assert.IsNotNull(grcarray.Symbol.Type);
            Assert.IsTrue(grcarray.Symbol.Type.Tag == Type.Tags.Group);
            GroupType grcarrayContent = (GroupType) grcarray.Symbol.Type;
            var rcField2 = grcarrayContent.Fields.ElementAt(0);
            Assert.IsTrue(rcField2.Level == 5);
            Assert.IsTrue(rcField2.Type == pt.Symbol.Type);
            var xxField = grcarrayContent.Fields.ElementAt(1);
            Assert.IsTrue(xxField.Level == 5);
            var grpField = grcarrayContent.Fields.ElementAt(2);
            Assert.IsTrue(grpField.Level == 5);
            Assert.IsTrue(grpField.Type.Tag == Type.Tags.Group);
            var arrField = ((GroupType) grpField.Type).Fields.Single();
            Assert.IsTrue(arrField.Level == 10);
            Assert.IsTrue(arrField.Type == rectarray.Symbol.Type);

            //Perform expansion
            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            //After expanding there are X1, Y1, X2, Y2 variables in the program, will all 02 Level
            //After renumber from 1, X1, Y1, X2, Y2 level has not changed because their levels were already 2
            var x1 = currentProgram.ResolveReference(new string[] { "x1" }, false);
            Assert.IsTrue(x1.Count == 1);
            Assert.IsTrue(x1.Symbol.Level == 2);
            var y1 = currentProgram.ResolveReference(new string[] { "y1" }, false);
            Assert.IsTrue(y1.Count == 1);
            Assert.IsTrue(y1.Symbol.Level == 2);
            var x2 = currentProgram.ResolveReference(new string[] { "x2" }, false);
            Assert.IsTrue(x2.Count == 1);
            Assert.IsTrue(x2.Symbol.Level == 2);
            var y2 = currentProgram.ResolveReference(new string[] { "y2" }, false);
            Assert.IsTrue(y2.Count == 1);
            Assert.IsTrue(y2.Symbol.Level == 2);
            Assert.IsTrue(rcarray.Symbol.Level == 1);

            //After expanding there are x, y, pt1, pt2 variables in the program
            var x = currentProgram.ResolveReference(new string[] { "x" }, false);
            Assert.IsTrue(x.Count == 5);
            var y = currentProgram.ResolveReference(new string[] { "y" }, false);
            Assert.IsTrue(y.Count == 5);
            var pt1 = currentProgram.ResolveReference(new string[] { "pt1" }, false);
            Assert.IsTrue(pt1.Count == 2);
            var pt2 = currentProgram.ResolveReference(new string[] { "pt2" }, false);
            Assert.IsTrue(pt2.Count == 2);

            //Check levels in expanded rcpt
            var rcpt_x = currentProgram.ResolveReference(new string[] { "x", "rcpt" }, false);
            Assert.IsTrue(rcpt_x.Count == 2);
            Assert.IsTrue(rcpt_x.All(v => v.Level == 3));
            var rcpt_y = currentProgram.ResolveReference(new string[] { "x", "rcpt" }, false);
            Assert.IsTrue(rcpt_y.Count == 2);
            Assert.IsTrue(rcpt_y.All(v => v.Level == 3));
            var rcpt_p1 = currentProgram.ResolveReference(new string[] { "pt1", "rcpt" }, false);
            Assert.IsTrue(rcpt_p1.Count == 1);
            Assert.IsTrue(rcpt_p1.Symbol.Level == 2);
            var rcpt_p2 = currentProgram.ResolveReference(new string[] { "pt2", "rcpt" }, false);
            Assert.IsTrue(rcpt_p2.Count == 1);
            Assert.IsTrue(rcpt_p2.Symbol.Level == 2);
            Assert.IsTrue(rcpt.Symbol.Level == 1);

            //Now check the expanded grcarray
            string grcarrayDump0 = grcarray.ToString();
            //01 grcarray.
            //   02 rc.
            //      03 X PIC 9(4).
            //      03 Y PIC 9(4).
            //   02 xx PIC X(9).
            //   02 grp.
            //      03 arr.
            //         04 a.
            //            05 rc.
            //               06 PT1.
            //                  07 X PIC 9(4).
            //                  07 Y PIC 9(4).
            //               06 PT2.
            //                  07 X PIC 9(4).
            //                  07 Y PIC 9(4).
            x = currentProgram.ResolveReference(new string[] { "x", "grcarray" }, false);
            Assert.IsTrue(x.Count == 3);
            y = currentProgram.ResolveReference(new string[] { "y", "grcarray" }, false);
            Assert.IsTrue(y.Count == 3);
            pt1 = currentProgram.ResolveReference(new string[] { "pt1", "grcarray" }, false);
            Assert.IsTrue(pt1.Count == 1);
            Assert.IsTrue(pt1.Symbol.Level == 6);
            pt2 = currentProgram.ResolveReference(new string[] { "pt2", "grcarray" }, false);
            Assert.IsTrue(pt2.Count == 1);
            Assert.IsTrue(pt2.Symbol.Level == 6);

            var rc = currentProgram.ResolveReference(new string[] { "rc" }, false);
            Assert.IsTrue(rc.Count == 2);

            var a = currentProgram.ResolveReference(new string[] { "a" }, false);
            Assert.IsTrue(a.Count == 1);
            Assert.IsTrue(a.Symbol.Level == 4);

            var arr = currentProgram.ResolveReference(new string[] { "arr" }, false);
            Assert.IsTrue(arr.Count == 1);
            Assert.IsTrue(arr.Symbol.Level == 3);

            var grp = currentProgram.ResolveReference(new string[] { "grp" }, false);
            Assert.IsTrue(grp.Count == 1);
            Assert.IsTrue(grp.Symbol.Level == 2);

            var xx = currentProgram.ResolveReference(new string[] { "xx" }, false);
            Assert.IsTrue(xx.Count == 1);
            Assert.IsTrue(xx.Symbol.Level == 2);

            var x_pt1_a = currentProgram.ResolveReference(new string[] { "x", "pt1", "a" }, false);
            Assert.IsTrue(x_pt1_a.Count == 1);
            Assert.IsTrue(x_pt1_a.Symbol.Level == 7);
            var x_pt2_a = currentProgram.ResolveReference(new string[] { "x", "pt2", "a" }, false);
            Assert.IsTrue(x_pt2_a.Count == 1);
            Assert.IsTrue(x_pt2_a.Symbol.Level == 7);
            var y_pt1_a = currentProgram.ResolveReference(new string[] { "y", "pt1", "a" }, false);
            Assert.IsTrue(y_pt1_a.Count == 1);
            Assert.IsTrue(y_pt1_a.Symbol.Level == 7);
            var y_pt2_a = currentProgram.ResolveReference(new string[] { "y", "pt2", "a" }, false);
            Assert.IsTrue(y_pt2_a.Count == 1);
            Assert.IsTrue(y_pt2_a.Symbol.Level == 7);
            Assert.IsTrue(x.ElementAt(0).Level == 3);
            Assert.IsTrue(x.ElementAt(1) == x_pt1_a.Symbol);
            Assert.IsTrue(x.ElementAt(2) == x_pt2_a.Symbol);
            Assert.IsTrue(y.ElementAt(0).Level == 3);
            Assert.IsTrue(y.ElementAt(1) == y_pt1_a.Symbol);
            Assert.IsTrue(y.ElementAt(2) == y_pt2_a.Symbol);

            var rc_a = currentProgram.ResolveReference(new string[] { "rc", "a" }, false);
            Assert.IsTrue(rc_a.Count == 1);
            Assert.IsTrue(rc_a.Symbol.Level == 5);
            Assert.IsTrue(rc.ElementAt(0).Level == 2);
            Assert.IsTrue(rc.ElementAt(1) == rc_a.Symbol);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RedefineSymbol")]
        public void ErrRedefinesImmediatlyPrecede()
        {
            //string path, List<Skeleton> skeletons = null, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null, Compiler.CodeModel.SymbolTable baseSymTable = null            
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "ErrRedefinesImmPrec.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.ErrRedefineWasNotImmediatlyPrec, "RX", 5));
            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[0].Message, d.Message);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RedefineSymbol")]
        public void RedefinesTest()
        {
            //string path, List<Skeleton> skeletons = null, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null, Compiler.CodeModel.SymbolTable baseSymTable = null            
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "Cobol85", "Redefines.rdz.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Get MyVar1 symbol
            var myvar1 = currentProgram.ResolveReference(new string[] {"myvar1"}, false);
            Assert.IsTrue(myvar1.Count == 1);

            //Check Errors : Cannot REDEFINES access MyVar1.
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.ErrRedefineWasNotImmediatlyPrec, myvar1.Symbol.Name, 1));
            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[0].Message, d.Message);

            //Get MyVar2 symbol with PIC X(9)
            var myvar2s = currentProgram.ResolveReference(new string[] { "myvar2" }, false);
            Assert.IsTrue(myvar2s.Count == 3);
            Assert.IsTrue(myvar2s.Distinct().Count() == 3);
            //Get the MyVar2 with PIC X(9).
            var myvar2 = myvar2s.FirstOrDefault(s => s.Type.Tag == Type.Tags.Picture && ((PictureType) s.Type).Length == 9);
            Assert.IsNotNull(myvar2);

            //Check that MyRedifines REDEFINES MyVar2 with PIC x(9)
            var MyRedifines = currentProgram.ResolveReference(new string[] { "myredifines" }, false);
            Assert.IsTrue(MyRedifines.Count == 1);
            Assert.IsTrue(MyRedifines.Symbol.HasFlag(Symbol.Flags.Redefines));
            RedefinesSymbol sref = (RedefinesSymbol) MyRedifines.Symbol;
            Assert.AreEqual(sref.Redefined, myvar2);

            //Check that MyRedifines{i} with 2 <= i <= 4 REDEFINES MyVar2 with PIC x(9)
            for (int i = 2; i <= 4; i++)
            {
                MyRedifines = currentProgram.ResolveReference(new string[] { "myredifines" + i }, false);
                Assert.AreEqual(MyRedifines.Count, 1);
                Assert.IsTrue(MyRedifines.Symbol.HasFlag(Symbol.Flags.Redefines));
                sref = (RedefinesSymbol)MyRedifines.Symbol;
                Assert.AreEqual(sref.Redefined, myvar2);
            }

            //Check transitives REDEFINES: FILTER REDEFINES VarGroupBis REDEFINE VarGroup.
            var VarGroup = currentProgram.ResolveReference(new string[] { "vargroup" }, false);
            Assert.IsTrue(VarGroup.Count == 1);
            Assert.IsFalse(VarGroup.Symbol.HasFlag(Symbol.Flags.Redefines));

            var VarGroupBis = currentProgram.ResolveReference(new string[] { "vargroup-bis" }, false);
            Assert.IsTrue(VarGroupBis.Count == 1);
            Assert.IsTrue(VarGroupBis.Symbol.HasFlag(Symbol.Flags.Redefines));
            Assert.AreEqual(((RedefinesSymbol)VarGroupBis.Symbol).Redefined, VarGroup.Symbol);

            var filter = currentProgram.ResolveReference(new string[] { "filter" }, false);
            Assert.IsTrue(filter.Count == 1);
            Assert.IsTrue(filter.Symbol.HasFlag(Symbol.Flags.Redefines));
            Assert.AreEqual(((RedefinesSymbol)filter.Symbol).Redefined, VarGroupBis.Symbol);
            Assert.AreEqual(((RedefinesSymbol)filter.Symbol).TopRedefined, VarGroup.Symbol);
        }

        /// <summary>
        /// This test tests that RENAMES declaration variable symbol are built.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void RenamesResolve0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "RenamesResolve0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 0);
            //Locate Redefines
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var vars = currentProgram.ResolveReference(new string[] {"yy"}, false);
            Assert.IsTrue(vars.Count == 1);
            string yy = vars.Single().ToString();
            Assert.AreEqual(yy, "66 YY RENAMES RX THRU RY." + System.Environment.NewLine);
        }

        /// <summary>
        /// Simple Rename Test based on TypeCobol.Test/Parser/Programs/Cobol2002.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void RenamePgmTest()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "Cobol2002", "Renames.rdz.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];            

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                0, 0, 0,
                string.Format(TypeCobolResource.CannotRenamesLevel, 1));

            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[0].Message, d.Message);
        }
        /// <summary>
        /// This test test that when renames objects are not found a diagnostics is emitted.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void BadRenames0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "BadRenames0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);


            //Locate Redefines
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var vars = currentProgram.ResolveReference(new string[] { "yy" }, false);
            Assert.IsTrue(vars.Count == 1);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 2);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.RenamesObjectNotFound, "MPOINT.RX"));

            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[0].Message, d.Message);
            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[1].Message, d.Message);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Typedef")]
        public void CircularReferenceType()
        {
            //Re-use COBOL code from existing circular typedef test
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "CircularReferenceType.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var program = document.Results.PrgSymbolTblBuilder.Programs[0];
            var checker = new CyclicTypeChecker();

            //Check non-cyclic type
            var notCyclic = GetTypedef("NotCyclic");
            Assert.IsFalse(checker.Check(notCyclic, FailErrorReporter.Instance));

            //Check Cyclic types
            Dictionary<int, TypedefType> cyclicTypes = new Dictionary<int, TypedefType>();
            for (int i = 1; i <= 8; i++)
            {
                var cyclicType = GetTypedef("Cyclic" + i);
                cyclicTypes.Add(i, cyclicType);
                Assert.IsTrue(checker.Check(cyclicTypes[i]));
            }
            var root = (RootSymbolTable) program.Owner;
            var a1 = (TypedefType) root.LookupType("A1").Symbol.Type;
            var b1 = (TypedefType) root.LookupType("B1").Symbol.Type;
            var c1 = (TypedefType) root.LookupType("C1").Symbol.Type;
            Assert.IsTrue(checker.Check(a1, new CompareMessagesErrorReporter(
                "Circular type reference detected : A1 -> B1 -> C1 -> A1.",
                "Type \"C1\" is unusable because it depends on cyclic type \"A1\".",
                "Type \"B1\" is unusable because it depends on cyclic type \"A1\".")));
            Assert.IsTrue(b1.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(b1.HasFlag(Symbol.Flags.IsCyclic));
            Assert.IsTrue(c1.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(c1.HasFlag(Symbol.Flags.IsCyclic));
            var a2 = (TypedefType)root.LookupType("A2").Symbol.Type;
            var b2 = (TypedefType)root.LookupType("B2").Symbol.Type;
            var c2 = (TypedefType)root.LookupType("C2").Symbol.Type;
            Assert.IsTrue(checker.Check(c2, new CompareMessagesErrorReporter(
                "Circular type reference detected : C2 -> A2 -> B2 -> C2.",
                "Type \"B2\" is unusable because it depends on cyclic type \"C2\".",
                "Type \"A2\" is unusable because it depends on cyclic type \"C2\".")));
            Assert.IsTrue(a2.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(a2.HasFlag(Symbol.Flags.IsCyclic));
            Assert.IsTrue(b2.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(b2.HasFlag(Symbol.Flags.IsCyclic));
            var a3 = (TypedefType)root.LookupType("A3").Symbol.Type;
            var b3 = (TypedefType)root.LookupType("B3").Symbol.Type;
            var c3 = (TypedefType)root.LookupType("C3").Symbol.Type;
            Assert.IsTrue(checker.Check(b3, new CompareMessagesErrorReporter(
                "Circular type reference detected : B3 -> C3 -> A3 -> B3.",
                "Type \"A3\" is unusable because it depends on cyclic type \"B3\".",
                "Type \"C3\" is unusable because it depends on cyclic type \"B3\".")));
            Assert.IsTrue(a3.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(a3.HasFlag(Symbol.Flags.IsCyclic));
            Assert.IsTrue(c3.HasFlag(Symbol.Flags.CheckedForCycles));
            Assert.IsTrue(c3.HasFlag(Symbol.Flags.IsCyclic));

            //Check variables
            for (int i = 1; i <= 8; i++)
            {
                var entry = program.ResolveReference(new[] {"var" + i}, false);
                Assert.IsTrue(entry != null);
                Assert.IsTrue(entry.Count == 1);
                var symbol = entry.Symbol;
                Assert.IsTrue(symbol.Type == cyclicTypes[i]);
            }

            //Check arrays
            for (int i = 1; i <= 8; i++)
            {
                var entry = program.ResolveReference(new[] { "array" + i }, false);
                Assert.IsTrue(entry != null);
                Assert.IsTrue(entry.Count == 1);
                var symbol = entry.Symbol;
                Assert.IsTrue(symbol.Type != null);
                Assert.IsTrue(symbol.Type.Tag == Type.Tags.Array);
                var arrayType = (ArrayType) symbol.Type;
                Assert.IsTrue(arrayType.ElementType == cyclicTypes[i]);
            }

            //Check group structures and types
            var groupEntry = program.ResolveReference(new[] { "group1" }, false);
            Assert.IsTrue(groupEntry != null);
            Assert.IsTrue(groupEntry.Count == 1);
            var group1 = groupEntry.Symbol;
            Assert.IsTrue(group1.Type != null);
            Assert.IsTrue(group1.Type.Tag == Type.Tags.Group);
            var groupTypeOfGroup1 = (GroupType) group1.Type;
            var fields = groupTypeOfGroup1.Fields.ToList();
            Assert.IsTrue(fields.Count == 3);
            var item1 = fields[0];
            Assert.IsTrue(item1.Type != null);
            Assert.IsTrue(item1.Type.Tag == Type.Tags.Picture);
            var item2 = fields[1];
            Assert.IsTrue(item2.Type != null);
            Assert.IsTrue(item2.Type.Tag == Type.Tags.Group);
            var groupTypeOfItem2 = (GroupType) item2.Type;
            var subFields = groupTypeOfItem2.Fields.ToList();
            Assert.IsTrue(subFields.Count == 3);
            var subItem1 = subFields[0];
            Assert.IsTrue(subItem1.Type != null);
            Assert.IsTrue(subItem1.Type.Tag == Type.Tags.Picture);
            var subItem2 = subFields[1];
            Assert.IsTrue(subItem2.Type == cyclicTypes[8]);
            var subItem3 = subFields[2];
            Assert.IsTrue(subItem3.Type != null);
            Assert.IsTrue(subItem3.Type.Tag == Type.Tags.Picture);
            var item3 = fields[2];
            Assert.IsTrue(item3.Type != null);
            Assert.IsTrue(item3.Type.Tag == Type.Tags.Picture);
            groupEntry = program.ResolveReference(new[] { "group2" }, false);
            Assert.IsTrue(groupEntry != null);
            Assert.IsTrue(groupEntry.Count == 1);
            var group2 = groupEntry.Symbol;
            Assert.IsTrue(group2.Type != null);
            Assert.IsTrue(group2.Type.Tag == Type.Tags.Group);
            var groupTypeOfGroup2 = (GroupType) group2.Type;
            fields = groupTypeOfGroup2.Fields.ToList();
            Assert.IsTrue(fields.Count == 2);
            var myVar1 = fields[0];
            Assert.IsTrue(myVar1.Type == cyclicTypes[8]);
            var myVar2 = fields[1];
            Assert.IsTrue(myVar2.Type == cyclicTypes[8]);

            TypedefType GetTypedef(string name)
            {
                var entry = program.Types.Lookup(name);
                Assert.IsTrue(entry != null);
                Assert.IsTrue(entry.Count == 1);
                var symbol = entry.Symbol;
                Assert.IsTrue(symbol.Type != null);
                Assert.IsTrue(symbol.Type.Tag == Type.Tags.Typedef);
                return (TypedefType)symbol.Type;
            }
        }

        /// <summary>
        /// This test is for testing non global variable access with Working or local storage section thru the procedure division.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityProgramVariable0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisPrgVar0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            //Get the Procedure Division Node.
            var procDivs = mainProgram.GetChildren<ProcedureDivision>();
            var procDiv = procDivs[0];
            //Get the Semantic Data
            ISemanticData data = procDiv.SemanticData;
            Assert.IsNotNull(data);
            Assert.IsTrue(data.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol = (Symbol) data;
            Assert.IsTrue(symbol.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym = (ProgramSymbol) symbol;
            var var1 = prgSym.ResolveReference(new string[] { "var1" }, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 1);
            Assert.IsTrue(var1.Symbol.HasFlag(Symbol.Flags.WORKING_STORAGE));
            Assert.IsFalse(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var1.Symbol.Name, "Var1");
            var var2 = prgSym.ResolveReference(new string[] { "var2" }, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 1);
            Assert.IsTrue(var2.Symbol.HasFlag(Symbol.Flags.LOCAL_STORAGE));
            Assert.IsFalse(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var2.Symbol.Name, "Var2");            
        }

        /// <summary>
        /// This test is for testing global variable access with Working or local storage section thru the procedure division,
        /// and also Global-Storage section variable..
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityProgramGlobalVariable0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisPrgGblVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            //Get the Procedure Division Node.
            var procDivs = mainProgram.GetChildren<ProcedureDivision>();
            var procDiv = procDivs[0];
            //Get the Semantic Data
            ISemanticData data = procDiv.SemanticData;
            Assert.IsNotNull(data);
            Assert.IsTrue(data.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol = (Symbol)data;
            Assert.IsTrue(symbol.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym = (ProgramSymbol)symbol;
            var var1 = prgSym.ResolveReference(new string[] { "var1" }, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 1);
            Assert.IsTrue(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var1.Symbol.Name, "Var1");
            var var2 = prgSym.ResolveReference(new string[] { "var2" }, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 1);
            Assert.IsTrue(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var2.Symbol.Name, "Var2");
            var var0 = prgSym.ResolveReference(new string[] { "var0" }, false);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 1);
            Assert.IsTrue(var0.Symbol.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
            Assert.IsFalse(var0.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var0.Symbol.Name, "Var0");

        }

        /// <summary>
        /// This test tests that a procedure with a program only access GLOBAL-STORAGE variables and not WORKING-STORAGE or
        /// LOCAL-STORAGE sections variable even if they are GLOBALs.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityProgramProcedureVariable0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisPrgProcVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            //Get the Procedure Division Node.
            var procDivs = mainProgram.GetChildren<ProcedureDivision>();
            var procDiv = procDivs[0];

            //Get the declared procedure.
            var funDecls = procDiv.GetChildren<FunctionDeclaration>();
            var funDecl = funDecls[0];

            //Get function,'s PROCEDURE DIVISION
            var funProcDivs = funDecl.GetChildren<ProcedureDivision>();
            var funProcDiv = funProcDivs[0];

            //Get the sentence that contains the DISPLAY instructions.
            var sentences = funProcDiv.GetChildren<Sentence>();
            var sentence = sentences[0];

            //Get Display instructions.
            var displays = sentence.GetChildren<Display>();

            //------------------------------------------------------------------
            // DISPLAY Var0
            //------------------------------------------------------------------
            var display0 = displays[0];
            ISemanticData data0 = display0.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsFalse(symbol0.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym0 = (FunctionSymbol)symbol0;

            //Without lookup upward enclosing programs "var0" cannot be found
            var var0 = prgSym0.ResolveReference(new string[] { "var0" }, false);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 0);
            //But with looking upward inclosing programs "var0" is found as it is in GLOBAL-STORAGE section
            var0 = prgSym0.ResolveReference(new string[] { "var0" }, true);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 1);
            Assert.IsTrue(var0.Symbol.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
            Assert.IsFalse(var0.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var0.Symbol.Name, "Var0");

            //------------------------------------------------------------------
            // DISPLAY Var1
            //------------------------------------------------------------------
            var display1 = displays[1];
            ISemanticData data1 = display1.SemanticData;
            Assert.IsNotNull(data1);
            Assert.IsTrue(data1.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol1 = (Symbol)data1;
            Assert.IsFalse(symbol1.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol1.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym1 = (FunctionSymbol)symbol1;
            //Without lookup upward enclosing programs "var1" cannot be found
            var var1 = prgSym0.ResolveReference(new string[] { "var1" }, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);
            //With looking upward inclosing programs "var1" is not found has is visibility is not GLOBAL-STORAGE.
            var1 = prgSym0.ResolveReference(new string[] { "var1" }, true);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);

            //------------------------------------------------------------------
            // DISPLAY Var2
            //------------------------------------------------------------------
            var display2 = displays[2];
            ISemanticData data2 = display2.SemanticData;
            Assert.IsNotNull(data2);
            Assert.IsTrue(data2.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol2 = (Symbol)data2;
            Assert.IsFalse(symbol2.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol2.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym2 = (FunctionSymbol)symbol2;
            //Without lookup upward enclosing programs "var2" cannot be found
            var var2 = prgSym0.ResolveReference(new string[] { "var2" }, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
            //With looking upward inclosing programs "var2" is not found has is visibility is not GLOBAL-STORAGE.
            //even if it's visibility is GLOBAL.
            var2 = prgSym0.ResolveReference(new string[] { "var2" }, true);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
        }

        /// <summary>
        /// This test tests that a nested program have only access to GLOBAL and GLOBAL-STORAGE variables of the enclosing program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityNestedProgramVariable0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisNestedPrgVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;

            //Get the nested program.
            var nestPrgs = mainProgram.GetChildren<NestedProgram>();
            var nestPrg = nestPrgs[0];

            //Get function,'s PROCEDURE DIVISION
            var nestProcDivs = nestPrg.GetChildren<ProcedureDivision>();
            var nestProcDiv = nestProcDivs[0];

            //Get the sentence that contains the DISPLAY instructions.
            var sentences = nestProcDiv.GetChildren<Sentence>();
            var sentence = sentences[0];

            //Get Display instructions.
            var displays = sentence.GetChildren<Display>();

            //------------------------------------------------------------------
            // DISPLAY Var0
            //------------------------------------------------------------------
            var display0 = displays[0];
            ISemanticData data0 = display0.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym0 = (ProgramSymbol)symbol0;

            //Without lookup upward enclosing programs "var0" cannot be found
            var var0 = prgSym0.ResolveReference(new string[] { "var0" }, false);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 0);
            //But with looking upward inclosing programs "var0" is found as it is in GLOBAL-STORAGE section
            var0 = prgSym0.ResolveReference(new string[] { "var0" }, true);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 1);
            Assert.IsTrue(var0.Symbol.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
            Assert.IsFalse(var0.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var0.Symbol.Name, "Var0");

            //------------------------------------------------------------------
            // DISPLAY Var1
            //------------------------------------------------------------------
            var display1 = displays[1];
            ISemanticData data1 = display1.SemanticData;
            Assert.IsNotNull(data1);
            Assert.IsTrue(data1.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol1 = (Symbol)data1;
            Assert.IsTrue(symbol1.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym1 = (ProgramSymbol)symbol1;

            //Without lookup upward enclosing programs "var1" cannot be found
            var var1 = prgSym1.ResolveReference(new string[] { "var1" }, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);
            //But with looking upward inclosing programs "var1" is found as it is in GLOBAL.
            var1 = prgSym1.ResolveReference(new string[] { "var1" }, true);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 1);
            Assert.IsTrue(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var1.Symbol.Name, "Var1");

            //------------------------------------------------------------------
            // DISPLAY Var2
            //------------------------------------------------------------------
            var display2 = displays[2];
            ISemanticData data2 = display2.SemanticData;
            Assert.IsNotNull(data2);
            Assert.IsTrue(data2.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol2 = (Symbol)data2;
            Assert.IsTrue(symbol2.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym2 = (ProgramSymbol)symbol2;

            //Without lookup upward enclosing programs "var2" cannot be found
            var var2 = prgSym2.ResolveReference(new string[] { "var2" }, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
            //But with looking upward inclosing programs "var2" is found as it is in GLOBAL.
            var2 = prgSym2.ResolveReference(new string[] { "var2" }, true);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 1);
            Assert.IsTrue(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var2.Symbol.Name, "Var2");

            //------------------------------------------------------------------
            // DISPLAY NoVar1
            // NoVar1 is not accessible as is it not GLOBAL in the Working of
            // the enclosing program.
            //------------------------------------------------------------------
            var display3 = displays[3];
            ISemanticData data3 = display3.SemanticData;
            Assert.IsNotNull(data3);
            Assert.IsTrue(data3.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol3 = (Symbol)data3;
            Assert.IsTrue(symbol3.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym3 = (ProgramSymbol)symbol3;

            //Without lookup upward enclosing programs "novar1" cannot be found
            var novar1 = prgSym3.ResolveReference(new string[] { "novar1" }, false);
            Assert.IsNotNull(novar1);
            Assert.IsTrue(novar1.Count == 0);
            //But with looking upward inclosing programs "novar1" is not found as it is not GLOBAL.
            novar1 = prgSym3.ResolveReference(new string[] { "novar1" }, true);
            Assert.IsNotNull(novar1);
            Assert.IsTrue(novar1.Count == 0);

            //------------------------------------------------------------------
            // DISPLAY NoVar2
            // NoVar1 is not accessible as is it not GLOBAL in the Working of
            // the enclosing program.
            //------------------------------------------------------------------
            var display4 = displays[4];            
            ISemanticData data4 = display4.SemanticData;
            Assert.IsNotNull(data4);
            Assert.IsTrue(data4.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol4 = (Symbol)data4;
            Assert.IsTrue(symbol4.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym4 = (ProgramSymbol)symbol4;

            //Without lookup upward enclosing programs "novar2" cannot be found
            var novar2 = prgSym4.ResolveReference(new string[] { "novar2" }, false);
            Assert.IsNotNull(novar2);
            Assert.IsTrue(novar2.Count == 0);
            //But with looking upward inclosing programs "novar1" is not found as it is not GLOBAL.
            novar1 = prgSym3.ResolveReference(new string[] { "novar2" }, true);
            Assert.IsNotNull(novar2);
            Assert.IsTrue(novar2.Count == 0);
        }

        /// <summary>
        /// This test tests that a nested program have only access to GLOBAL and GLOBAL-STORAGE variables of the enclosing program.
        /// Same than VisibilityNestedProgramVariable0 but using SymbolReference of DISPLAY Instructuions.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityNestedProgramVariable1()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisNestedPrgVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;

            //Get the nested program.
            var nestPrgs = mainProgram.GetChildren<NestedProgram>();
            var nestPrg = nestPrgs[0];

            //Get function,'s PROCEDURE DIVISION
            var nestProcDivs = nestPrg.GetChildren<ProcedureDivision>();
            var nestProcDiv = nestProcDivs[0];

            //Get the sentence that contains the DISPLAY instructions.
            var sentences = nestProcDiv.GetChildren<Sentence>();
            var sentence = sentences[0];

            //Get Display instructions.
            var displays = sentence.GetChildren<Display>();

            //------------------------------------------------------------------
            // DISPLAY Var0
            //------------------------------------------------------------------
            var display0 = displays[0];
            ISemanticData data0 = display0.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym0 = (ProgramSymbol)symbol0;

            //Without lookup upward enclosing programs "var0" cannot be found
            Assert.IsTrue(display0.CodeElement.Type == CodeElementType.DisplayStatement);
            DisplayStatement displayStmt0 = (DisplayStatement)display0.CodeElement;
            Assert.IsTrue(displayStmt0.Variables != null && displayStmt0.Variables.Length == 1);
            var srefVar0 = displayStmt0.Variables[0].MainSymbolReference;
            Assert.AreEqual(srefVar0.Name, "Var0");
            var var0 = prgSym0.ResolveReference(srefVar0, false);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 0);
            //But with looking upward inclosing programs "var0" is found as it is in GLOBAL-STORAGE section
            var0 = prgSym0.ResolveReference(srefVar0, true);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 1);
            Assert.IsTrue(var0.Symbol.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
            Assert.IsFalse(var0.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var0.Symbol.Name, "Var0");

            //------------------------------------------------------------------
            // DISPLAY Var1
            //------------------------------------------------------------------
            var display1 = displays[1];
            ISemanticData data1 = display1.SemanticData;
            Assert.IsNotNull(data1);
            Assert.IsTrue(data1.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol1 = (Symbol)data1;
            Assert.IsTrue(symbol1.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym1 = (ProgramSymbol)symbol1;

            //Without lookup upward enclosing programs "var1" cannot be found
            Assert.IsTrue(display1.CodeElement.Type == CodeElementType.DisplayStatement);
            DisplayStatement displayStmt1 = (DisplayStatement)display1.CodeElement;
            Assert.IsTrue(displayStmt1.Variables != null && displayStmt1.Variables.Length == 1);
            var srefVar1 = displayStmt1.Variables[0].MainSymbolReference;
            Assert.AreEqual(srefVar1.Name, "Var1");
            var var1 = prgSym1.ResolveReference(srefVar1, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);
            //But with looking upward inclosing programs "var1" is found as it is in GLOBAL.
            var1 = prgSym1.ResolveReference(srefVar1, true);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 1);
            Assert.IsTrue(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var1.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var1.Symbol.Name, "Var1");

            //------------------------------------------------------------------
            // DISPLAY Var2
            //------------------------------------------------------------------
            var display2 = displays[2];
            ISemanticData data2 = display2.SemanticData;
            Assert.IsNotNull(data2);
            Assert.IsTrue(data2.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol2 = (Symbol)data2;
            Assert.IsTrue(symbol2.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym2 = (ProgramSymbol)symbol2;

            //Without lookup upward enclosing programs "var2" cannot be found
            Assert.IsTrue(display2.CodeElement.Type == CodeElementType.DisplayStatement);
            DisplayStatement displayStmt2 = (DisplayStatement)display2.CodeElement;
            Assert.IsTrue(displayStmt2.Variables != null && displayStmt2.Variables.Length == 1);
            var srefVar2 = displayStmt2.Variables[0].MainSymbolReference;
            Assert.AreEqual(srefVar2.Name, "Var2");
            var var2 = prgSym2.ResolveReference(srefVar2, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
            //But with looking upward inclosing programs "var2" is found as it is in GLOBAL.
            var2 = prgSym2.ResolveReference(srefVar2, true);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 1);
            Assert.IsTrue(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.IsFalse(var2.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.GLOBAL_STORAGE)));
            Assert.AreEqual(var2.Symbol.Name, "Var2");

            //------------------------------------------------------------------
            // DISPLAY NoVar1
            // NoVar1 is not accessible as is it not GLOBAL in the Working of
            // the enclosing program.
            //------------------------------------------------------------------
            var display3 = displays[3];
            ISemanticData data3 = display3.SemanticData;
            Assert.IsNotNull(data3);
            Assert.IsTrue(data3.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol3 = (Symbol)data3;
            Assert.IsTrue(symbol3.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym3 = (ProgramSymbol)symbol3;

            //Without lookup upward enclosing programs "novar1" cannot be found
            Assert.IsTrue(display3.CodeElement.Type == CodeElementType.DisplayStatement);
            DisplayStatement displayStmt3 = (DisplayStatement)display3.CodeElement;
            Assert.IsTrue(displayStmt3.Variables != null && displayStmt3.Variables.Length == 1);
            var srefNoVar1 = displayStmt3.Variables[0].MainSymbolReference;
            Assert.AreEqual(srefNoVar1.Name, "NoVar1");
            var novar1 = prgSym3.ResolveReference(srefNoVar1, false);
            Assert.IsNotNull(novar1);
            Assert.IsTrue(novar1.Count == 0);
            //But with looking upward inclosing programs "novar1" is not found as it is not GLOBAL.
            novar1 = prgSym3.ResolveReference(srefNoVar1, true);
            Assert.IsNotNull(novar1);
            Assert.IsTrue(novar1.Count == 0);

            //------------------------------------------------------------------
            // DISPLAY NoVar2
            // NoVar1 is not accessible as is it not GLOBAL in the Working of
            // the enclosing program.
            //------------------------------------------------------------------
            var display4 = displays[4];
            ISemanticData data4 = display4.SemanticData;
            Assert.IsNotNull(data4);
            Assert.IsTrue(data4.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol4 = (Symbol)data4;
            Assert.IsTrue(symbol4.Kind == Symbol.Kinds.Program);
            ProgramSymbol prgSym4 = (ProgramSymbol)symbol4;

            //Without lookup upward enclosing programs "novar2" cannot be found
            Assert.IsTrue(display4.CodeElement.Type == CodeElementType.DisplayStatement);
            DisplayStatement displayStmt4 = (DisplayStatement)display4.CodeElement;
            Assert.IsTrue(displayStmt4.Variables != null && displayStmt4.Variables.Length == 1);
            var srefNoVar2 = displayStmt4.Variables[0].MainSymbolReference;
            Assert.AreEqual(srefNoVar2.Name, "NoVar2");
            var novar2 = prgSym4.ResolveReference(srefNoVar2, false);
            Assert.IsNotNull(novar2);
            Assert.IsTrue(novar2.Count == 0);
            //But with looking upward inclosing programs "novar1" is not found as it is not GLOBAL.
            novar1 = prgSym3.ResolveReference(srefNoVar2, true);
            Assert.IsNotNull(novar2);
            Assert.IsTrue(novar2.Count == 0);
        }

        /// <summary>
        /// This test tests that a procedure with a nested program only access GLOBAL-STORAGE variables and not WORKING-STORAGE or
        /// LOCAL-STORAGE sections variable even if they are GLOBALs.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityProgramNestedProcedureVariable0()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "VisPrgNestedProcVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];
            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;

            //Get the nested program.
            var nestPrgs = mainProgram.GetChildren<NestedProgram>();
            var nestPrg = nestPrgs[0];

            //Get the Procedure Division Node of the Nested Program.
            var procDivs = nestPrg.GetChildren<ProcedureDivision>();
            var procDiv = procDivs[0];

            //Get the declared nested procedure.
            var funDecls = procDiv.GetChildren<FunctionDeclaration>();
            var funDecl = funDecls[0];

            //Get function,'s PROCEDURE DIVISION
            var funProcDivs = funDecl.GetChildren<ProcedureDivision>();
            var funProcDiv = funProcDivs[0];

            //Get the sentence that contains the DISPLAY instructions.
            var sentences = funProcDiv.GetChildren<Sentence>();
            var sentence = sentences[0];

            //Get Display instructions.
            var displays = sentence.GetChildren<Display>();

            //------------------------------------------------------------------
            // DISPLAY Var0
            //------------------------------------------------------------------
            var display0 = displays[0];
            ISemanticData data0 = display0.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsFalse(symbol0.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym0 = (FunctionSymbol)symbol0;

            //Without lookup upward enclosing programs "var0" cannot be found
            var var0 = prgSym0.ResolveReference(new string[] { "var0" }, false);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 0);
            //But with looking upward inclosing programs "var0" is found as it is in GLOBAL-STORAGE section
            var0 = prgSym0.ResolveReference(new string[] { "var0" }, true);
            Assert.IsNotNull(var0);
            Assert.IsTrue(var0.Count == 1);
            Assert.IsTrue(var0.Symbol.HasFlag(Symbol.Flags.GLOBAL_STORAGE));
            Assert.IsFalse(var0.Symbol.HasFlag((Symbol.Flags)((ulong)Symbol.Flags.LOCAL_STORAGE | (ulong)Symbol.Flags.WORKING_STORAGE | (ulong)Symbol.Flags.Global)));
            Assert.AreEqual(var0.Symbol.Name, "Var0");

            //------------------------------------------------------------------
            // DISPLAY Var1
            //------------------------------------------------------------------
            var display1 = displays[1];
            ISemanticData data1 = display1.SemanticData;
            Assert.IsNotNull(data1);
            Assert.IsTrue(data1.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol1 = (Symbol)data1;
            Assert.IsFalse(symbol1.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol1.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym1 = (FunctionSymbol)symbol1;
            //Without lookup upward enclosing programs "var1" cannot be found
            var var1 = prgSym0.ResolveReference(new string[] { "var1" }, false);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);
            //With looking upward inclosing programs "var1" is not found has is visibility is not GLOBAL-STORAGE.
            var1 = prgSym0.ResolveReference(new string[] { "var1" }, true);
            Assert.IsNotNull(var1);
            Assert.IsTrue(var1.Count == 0);

            //------------------------------------------------------------------
            // DISPLAY Var2
            //------------------------------------------------------------------
            var display2 = displays[2];
            ISemanticData data2 = display2.SemanticData;
            Assert.IsNotNull(data2);
            Assert.IsTrue(data2.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol2 = (Symbol)data2;
            Assert.IsFalse(symbol2.Kind == Symbol.Kinds.Program);
            Assert.IsTrue(symbol2.Kind == Symbol.Kinds.Function);
            FunctionSymbol prgSym2 = (FunctionSymbol)symbol2;
            //Without lookup upward enclosing programs "var2" cannot be found
            var var2 = prgSym0.ResolveReference(new string[] { "var2" }, false);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
            //With looking upward inclosing programs "var2" is not found has is visibility is not GLOBAL-STORAGE.
            //even if it's visibility is GLOBAL.
            var2 = prgSym0.ResolveReference(new string[] { "var2" }, true);
            Assert.IsNotNull(var2);
            Assert.IsTrue(var2.Count == 0);
        }

        /// <summary>
        /// This test tests the visibility public/private of a typedef within a nested program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void TypeDefPublicPrivateVisibilityNestedProgram()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "Typedef-public.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;            

            //Get the nested program.
            var nestPrgs = mainProgram.GetChildren<NestedProgram>();
            var nestPrg = nestPrgs[0];
            Assert.AreEqual(nestPrg.Name, "Tester");
            ISemanticData data0 = nestPrg.SemanticData;
            Assert.IsNotNull(data0);
            Assert.IsTrue(data0.SemanticKind == SemanticKinds.Symbol);
            Symbol symbol0 = (Symbol)data0;
            Assert.IsTrue(symbol0.Kind == Symbol.Kinds.Program);
            ProgramSymbol nestPrgSym = (ProgramSymbol)symbol0;
            Assert.AreEqual(nestPrgSym.Name, "Tester");

            //Lookup the type "typeOfDays"
            var typeOfDaysPublic = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysPublic" });
            Assert.IsNotNull(typeOfDaysPublic);
            Assert.IsTrue(typeOfDaysPublic.Count == 1);
            Assert.IsNotNull(typeOfDaysPublic.Symbol.Type);
            Assert.IsTrue(typeOfDaysPublic.Symbol.HasFlag(Symbol.Flags.Public | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysPublic.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysPublic.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //-------------------------------------------------
            //Get the variable var1 and its type accessibility.
            //-------------------------------------------------
            var var1 = nestPrgSym.ResolveReference(new string[] {"var1"}, false);
            Assert.IsTrue(var1.Count == 3);
            foreach (var var1Symbol in var1)
            {
                Assert.IsTrue(var1Symbol.Type == typeOfDaysPublic.Symbol.Type);
            }
            //Check the the type was accessible
            Assert.IsTrue(nestPrgSym.IsTypeAccessible(typeOfDaysPublic.Symbol));

            //Lookup the type "typeOfDaysPrivate"
            var typeOfDaysPrivate = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysPrivate" });
            Assert.IsNotNull(typeOfDaysPrivate);
            Assert.IsTrue(typeOfDaysPrivate.Count == 1);
            Assert.IsNotNull(typeOfDaysPrivate.Symbol.Type);
            Assert.IsFalse(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //-------------------------------------------------
            //Get the variable var2 and its type accessibility.
            //-------------------------------------------------
            var var2 = nestPrgSym.ResolveReference(new string[] { "var2" }, false);
            Assert.IsTrue(var2.Count == 3);
            foreach (var var2Symbol in var2)
            {
                Assert.IsTrue(var2Symbol.Type == typeOfDaysPrivate.Symbol.Type);
            }
            //Check the the type was accessible
            Assert.IsTrue(nestPrgSym.IsTypeAccessible(typeOfDaysPrivate.Symbol));

            //Lookup the type "typeOfDaysLocal"
            var typeOfDaysLocal = nestPrgSym.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "typeOfDaysLocal" });
            Assert.IsNotNull(typeOfDaysLocal);
            Assert.IsTrue(typeOfDaysLocal.Count == 1);
            Assert.IsNotNull(typeOfDaysLocal.Symbol.Type);
            Assert.IsFalse(typeOfDaysLocal.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysLocal.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysLocal.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysLocal.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //-------------------------------------------------
            //Get the variable var3 and its type accessibility.
            //-------------------------------------------------
            var var3 = nestPrgSym.ResolveReference(new string[] { "var3" }, false);
            Assert.IsTrue(var3.Count == 1);
            Assert.IsTrue(var3.Symbol.Type == typeOfDaysLocal.Symbol.Type);
            //Check the the type was not accessible
            Assert.IsFalse(nestPrgSym.IsTypeAccessible(typeOfDaysLocal.Symbol));
        }

        /// <summary>
        /// This Test tests the expansion of a Type Currency inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void ProcedureCallPublicPrivateVisibility()
        {
            string path = Path.Combine(GetTestLocation(), "Parser", "Programs", "TypeCobol", "ProcedureCall-PublicPrivate.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            var PGM1 = document.Results.PrgSymbolTblBuilder.Programs[0];
            var PGM2 = document.Results.PrgSymbolTblBuilder.Programs[1];
            var PGM3 = document.Results.PrgSymbolTblBuilder.Programs[2];

            var MyPublicProcedure = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "MyPublicProcedure", "PGM2" });
            Assert.IsNotNull(MyPublicProcedure);
            Assert.AreEqual(1, MyPublicProcedure.Count);
            Assert.AreEqual(MyPublicProcedure.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(MyPublicProcedure.Symbol.Owner, PGM2);
            Assert.IsTrue(PGM1.IsFunctionAccessible(MyPublicProcedure.Symbol));

            MyPublicProcedure = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "MyPublicProcedure", "PGM3" });
            Assert.IsNotNull(MyPublicProcedure);
            Assert.AreEqual(1, MyPublicProcedure.Count);
            Assert.AreEqual(MyPublicProcedure.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(MyPublicProcedure.Symbol.Owner, PGM3);
            Assert.IsTrue(PGM1.IsFunctionAccessible(MyPublicProcedure.Symbol));

            var Pgm1PrivateValidateDateFormat = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "Pgm1PrivateValidateDateFormat"});
            Assert.IsNotNull(Pgm1PrivateValidateDateFormat);
            Assert.AreEqual(1, Pgm1PrivateValidateDateFormat.Count);
            Assert.AreEqual(Pgm1PrivateValidateDateFormat.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(Pgm1PrivateValidateDateFormat.Symbol.Owner, PGM1);
            Assert.IsTrue(PGM1.IsFunctionAccessible(Pgm1PrivateValidateDateFormat.Symbol));

            Pgm1PrivateValidateDateFormat = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "Pgm1PrivateValidateDateFormat", "PGM1" });
            Assert.IsNotNull(Pgm1PrivateValidateDateFormat);
            Assert.AreEqual(1, Pgm1PrivateValidateDateFormat.Count);
            Assert.AreEqual(Pgm1PrivateValidateDateFormat.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(Pgm1PrivateValidateDateFormat.Symbol.Owner, PGM1);
            Assert.IsTrue(PGM1.IsFunctionAccessible(Pgm1PrivateValidateDateFormat.Symbol));

            var Pgm2PrivateValidateDateFormat = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "Pgm2PrivateValidateDateFormat" });
            Assert.IsNull(Pgm2PrivateValidateDateFormat);

            Pgm2PrivateValidateDateFormat = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "Pgm2PrivateValidateDateFormat", "PGM2" });
            Assert.IsNotNull(Pgm2PrivateValidateDateFormat);
            Assert.AreEqual(1, Pgm2PrivateValidateDateFormat.Count);
            Assert.AreEqual(Pgm2PrivateValidateDateFormat.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(Pgm2PrivateValidateDateFormat.Symbol.Owner, PGM2);
            Assert.IsFalse(PGM1.IsFunctionAccessible(Pgm2PrivateValidateDateFormat.Symbol));
            Assert.IsTrue(PGM2.IsFunctionAccessible(Pgm2PrivateValidateDateFormat.Symbol));

            var check = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "check", "PGM1" });
            Assert.IsNotNull(check);
            Assert.AreEqual(1, check.Count);
            Assert.AreEqual(check.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(check.Symbol.Owner, PGM1);
            Assert.IsTrue(PGM1.IsFunctionAccessible(check.Symbol));

            check = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "check", "PGM2" });
            Assert.IsNotNull(check);
            Assert.AreEqual(1, check.Count);
            Assert.AreEqual(check.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(check.Symbol.Owner, PGM2);
            Assert.IsTrue(PGM1.IsFunctionAccessible(check.Symbol));

            var check2 = PGM1.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "check2", "PGM2" });
            Assert.IsNotNull(check2);
            Assert.AreEqual(2, check2.Count);
            Assert.AreNotEqual(check2.ElementAt(0), check2.ElementAt(1));
            Assert.AreEqual(check2.ElementAt(0).Kind, Symbol.Kinds.Function);
            Assert.AreEqual(check2.ElementAt(0).Owner, PGM2);
            Assert.IsTrue(PGM1.IsFunctionAccessible(check2.ElementAt(0)));
            Assert.AreEqual(check2.ElementAt(1).Kind, Symbol.Kinds.Function);
            Assert.AreEqual(check2.ElementAt(1).Owner, PGM2);
            Assert.IsTrue(PGM1.IsFunctionAccessible(check2.ElementAt(1)));

            Pgm2PrivateValidateDateFormat = PGM2.ReverseResolveFunction(document.Results.RootSymbolTable, new string[] { "Pgm2PrivateValidateDateFormat", "PGM2" });
            Assert.IsNotNull(Pgm2PrivateValidateDateFormat);
            Assert.AreEqual(1, Pgm2PrivateValidateDateFormat.Count);
            Assert.AreEqual(Pgm2PrivateValidateDateFormat.Symbol.Kind, Symbol.Kinds.Function);
            Assert.AreEqual(Pgm2PrivateValidateDateFormat.Symbol.Owner, PGM2);
            Assert.IsTrue(PGM2.IsFunctionAccessible(Pgm2PrivateValidateDateFormat.Symbol));
            Assert.IsFalse(PGM1.IsFunctionAccessible(Pgm2PrivateValidateDateFormat.Symbol));
            Assert.IsFalse(PGM3.IsFunctionAccessible(Pgm2PrivateValidateDateFormat.Symbol));
        }

        /// <summary>
        /// This test test that the REDEFINES that uses a variable from a TYPEDEF is correctly expanded.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
        public void ExpanderInterProg()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "ExpanderInterProg.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            ProgramExpander expander = new ProgramExpander(FailErrorReporter.Instance);
            expander.Expand(currentProgram);

            var vars = currentProgram.ResolveReference(new string[] { "td-var42", "var1" }, false);
            Assert.IsTrue(vars.Count == 1);
        }

        /// <summary>
        /// This Test tests all various accessibility on type, by considering from which kind of program or procedure a type from
        /// another program or procedure is asked for visibility access.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void TypeVisibility_Prg_NestedPrg_Proc_Stacked()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "TypeVisNestedPrgAndProc.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 2);
            ProgramSymbol currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];//TypeVisNestedPrgAndProc
            SourceProgram mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.AreEqual(currentProgram, mainProgram.SemanticData);

            //Get the nested program.
            var nestedPrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[]{"Nested" });
            Assert.IsTrue(nestedPrgEntry.Count == 1);
            ProgramSymbol nestedProgram = nestedPrgEntry.Symbol;//Nested
            IList<NestedProgram> nestedPrgs = mainProgram.GetChildren<NestedProgram>();
            NestedProgram nestedPrg = nestedPrgs[0];
            Assert.AreEqual(nestedProgram, nestedPrg.SemanticData);

            //Get the Nested program of the Nested Program: Nested2
            var nestedNestedPrgs = nestedPrg.GetChildren<NestedProgram>();
            var nestedNestedPrg = nestedNestedPrgs[0];
            var nested2PrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested2" });
            Assert.IsTrue(nested2PrgEntry.Count == 1);
            ProgramSymbol nested2Program = nested2PrgEntry.Symbol;//Nested2
            IList<NestedProgram> nested2Prgs = nestedPrg.GetChildren<NestedProgram>();
            NestedProgram nested2Prg = nested2Prgs[0];
            Assert.AreEqual(nested2Program, nested2Prg.SemanticData);

            //Get the Nested program Nested21
            var nestedNested21Prgs = nested2Prg.GetChildren<NestedProgram>();
            var nestedNested21Prg = nestedNested21Prgs[0];
            var nested21PrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21" });
            Assert.IsTrue(nested21PrgEntry.Count == 1);
            var nested21PrgEntryBis = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21", "Nested2" });
            Assert.IsTrue(nested21PrgEntryBis.Count == 1);
            Assert.AreEqual(nested21PrgEntry.Symbol, nested21PrgEntryBis.Symbol);
            var nested21PrgEntryTer = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(nested21PrgEntryTer.Count == 1);
            Assert.AreEqual(nested21PrgEntry.Symbol, nested21PrgEntryTer.Symbol);
            ProgramSymbol nested21Program = nested21PrgEntry.Symbol;//Nested21
            IList<NestedProgram> nested21Prgs = nested2Prg.GetChildren<NestedProgram>();
            NestedProgram nested21Prg = nested21Prgs[0];
            Assert.AreEqual(nested21Program, nested21Prg.SemanticData);

            //-----------------------------------------------
            //Immediatly Resolve nested procedures of Nested2
            //------------------------------------------------
            //NestedProcLocal
            var NestedProcLocalEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcLocal" });
            Assert.IsTrue(NestedProcLocalEntry.Count == 1);
            var NestedProcLocalEntryBis = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcLocal", "Nested" });
            Assert.IsTrue(NestedProcLocalEntryBis.Count == 1);
            Assert.AreEqual(NestedProcLocalEntry.Symbol, NestedProcLocalEntryBis.Symbol);


            //NestedProcPrivate
            var NestedProcPrivateEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPrivate" });
            Assert.IsTrue(NestedProcPrivateEntry.Count == 1);

            //NestedProcPublic
            var NestedProcPublicEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPublic" });
            Assert.IsTrue(NestedProcPublicEntry.Count == 1);

            //-------------------------------------
            ///Resolve TypeVisStackedPrg
            //-------------------------------------
            var TypeVisStackedPrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "TypeVisStackedPrg" });
            Assert.IsTrue(TypeVisStackedPrgEntry.Count == 1);

            //---------------------------------------
            //Resolve all types of the MAIN Program.
            //---------------------------------------
            var POINTMainGblEntry = document.Results.RootSymbolTable.ResolveType(new string[] {"POINTMainGbl", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTMainGblEntry.Count == 1);

            var POINTMainPrivEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTMainPriv", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTMainPrivEntry.Count == 1);

            var POINTMainLocalEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTMainLocal", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTMainLocalEntry.Count == 1);

            var POINTMainPublicEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTMainPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTMainPublicEntry.Count == 1);

            //---------------------------------------
            //Resolve all types of the Nested Program.
            //---------------------------------------
            var typ = currentProgram.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "POINTNestedPriv" });

            var POINTNestedGblEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedGbl", "Nested" });
            Assert.IsTrue(POINTNestedGblEntry.Count == 1);

            var POINTNestedPrivEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedPriv", "Nested" });
            Assert.IsTrue(POINTNestedPrivEntry.Count == 1);//As it is private it is defined in TypeVisNestedPrgAndProc
            var POINTNestedPrivEntry2 = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedPriv", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTNestedPrivEntry2.Count == 1);
            Assert.AreEqual(POINTNestedPrivEntry.Symbol, POINTNestedPrivEntry2.Symbol);

            var POINTNestedLocalEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedLocal", "Nested" });
            Assert.IsTrue(POINTNestedLocalEntry.Count == 1);

            var POINTNestedPublicEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedPublic", "Nested" });
            Assert.IsTrue(POINTNestedPublicEntry.Count == 1);//As it is public it is defined in TypeVisNestedPrgAndProc
            var POINTNestedPublicEntry2 = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTNestedPublicEntry.Count == 1);
            Assert.AreEqual(POINTNestedPublicEntry.Symbol, POINTNestedPublicEntry2.Symbol);

            //-----------------------------------------
            //Resolve all types of the Nested2 Program.
            //-----------------------------------------
            var typ2 = currentProgram.ReverseResolveType(document.Results.RootSymbolTable, new string[] { "POINTNestedNestedPriv" });

            var POINTNestedNestedGblEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedGbl", "Nested2" });
            Assert.IsTrue(POINTNestedNestedGblEntry.Count == 1);

            var POINTNestedNestedPrivEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedPriv", "Nested2" });
            Assert.IsTrue(POINTNestedNestedPrivEntry.Count == 1);//As it is private it is defined in TypeVisNestedPrgAndProc
            var POINTNestedNestedPrivEntry2 = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedPriv", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTNestedNestedPrivEntry2.Count == 1);
            Assert.AreEqual(POINTNestedNestedPrivEntry.Symbol, POINTNestedNestedPrivEntry2.Symbol);

            var POINTNestedNestedLocalEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedLocal", "Nested2" });
            Assert.IsTrue(POINTNestedNestedLocalEntry.Count == 1);

            var POINTNestedNestedPublicEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedPublic", "Nested2" });
            Assert.IsTrue(POINTNestedNestedPublicEntry.Count == 1);//As it is public it is defined in TypeVisNestedPrgAndProc
            var POINTNestedNestedPublicEntry2 = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTNestedNestedPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(POINTNestedNestedPublicEntry.Count == 1);
            Assert.AreEqual(POINTNestedNestedPublicEntry.Symbol, POINTNestedNestedPublicEntry2.Symbol);

            //-----------------------------------------
            //Resolve all types of the Stacked Program.
            //-----------------------------------------
            var typStacked = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTStackedPriv" });

            var POINTStackedGblEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTStackedGbl", "TypeVisStackedPrg" });
            Assert.IsTrue(POINTStackedGblEntry.Count == 1);

            var POINTStackedPrivEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTStackedPriv", "TypeVisStackedPrg" });
            Assert.IsTrue(POINTStackedPrivEntry.Count == 1);

            var POINTStackedLocalEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTStackedLocal", "TypeVisStackedPrg" });
            Assert.IsTrue(POINTStackedLocalEntry.Count == 1);

            var POINTStackedPublicEntry = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTStackedPublic", "TypeVisStackedPrg" });
            Assert.IsTrue(POINTStackedPublicEntry.Count == 1);

            //-----------------------------------------------
            // PERFORM Accessibility tests From Main Program
            //-----------------------------------------------
            // Type POINTMainGbl is only visible in the Main TypeVisNestedPrgAndProc Program
            // and its nested programs, not in nested procedures and not in the stacked program
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTMainGblEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTMainGblEntry.Symbol));

            // Type POINTMainPriv is only visible in the Main TypeVisNestedPrgAndProc Program
            // its nested programs, its nested procedures and not in the stacked program
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTMainPrivEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTMainPrivEntry.Symbol));

            // Type POINTMainLocal is only visible its TypeVisNestedPrgAndProc Program
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTMainLocalEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTMainLocalEntry.Symbol));

            // Type POINTMainPublic is visible anywhere
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTMainPublicEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTMainPublicEntry.Symbol));


            //---------------------------------------------------
            // PERFORM Accessibility tests From "Nested" Program
            //---------------------------------------------------
            // Type POINTNestedGbl is only visible in the Program called Nested"
            // and its nested programs, not in its nested procedures and not in the stacked program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedGblEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedGblEntry.Symbol));

            // Type POINTNestedPriv is only visible in the Main TypeVisNestedPrgAndProc Program
            // its nested programs, its nested procedures and not in the stacked program
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedPrivEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedPrivEntry.Symbol));

            // Type POINTNestedLocal is only visible in the "Nested" Program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedLocalEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedLocalEntry.Symbol));

            // Type POINTNestedPublic is visible anywhere
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedPublicEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedPublicEntry.Symbol));

            //---------------------------------------------------------
            // PERFORM Accessibility tests From "Nested2" Program
            //---------------------------------------------------------
            // Type POINTNestedNestedGbl is only visible in the Program called Nested2"
            // and its nested programs, not in its nested procedures and not in the stacked program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedNestedGblEntry.Symbol));

            // Type POINTNestedNestedPriv is only visible in the Main TypeVisNestedNestedPrgAndProc Program
            // its nested programs, its nested procedures and not in the stacked program
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedNestedPrivEntry.Symbol));

            // Type POINTNestedNestedLocal is only visible in the "Nested2" Program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedNestedLocalEntry.Symbol));

            // Type POINTNestedNestedPublic is visible anywhere
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTNestedNestedPublicEntry.Symbol));

            //---------------------------------------------
            // TESTING type inside Function
            //---------------------------------------------
            var POINTFunc = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTFunc"});
            Assert.AreEqual(3, POINTFunc.Count);
            var POINTFuncLocal = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTFunc", "NestedProcLocal" });
            Assert.IsTrue(POINTFuncLocal.Count == 1);
            var POINTFuncPrivate = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTFunc", "NestedProcPrivate" });
            Assert.IsTrue(POINTFuncPrivate.Count == 1);
            var POINTFuncPublic = document.Results.RootSymbolTable.ResolveType(new string[] { "POINTFunc", "NestedProcPublic" });
            Assert.IsTrue(POINTFuncPublic.Count == 1);

            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTFuncLocal.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTFuncLocal.Symbol));

            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTFuncPrivate.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTFuncPrivate.Symbol));

            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTFuncPublic.Symbol));
            Assert.IsFalse(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTFuncPublic.Symbol));

            //--------------------------------------------------
            // PERFORM Accessibility tests From Stacked Program
            //--------------------------------------------------
            // Type POINTStackedGbl is only visible in the Stacked TypeVisStackedPrg Program
            // and its nested programs, not in nested procedures and not in the stacked program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTStackedGblEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTStackedGblEntry.Symbol));

            // Type POINTStackedPriv is only visible in the Stacked TypeVisStackedPrg Program
            // its nested programs, its nested procedures and not in the stacked program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTStackedPrivEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTStackedPrivEntry.Symbol));

            // Type POINTStackedLocal is only visible its TypeVisStackedPrg Program
            Assert.IsFalse(currentProgram.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsFalse(nestedProgram.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsFalse(nested2Program.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsFalse(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTStackedLocalEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTStackedLocalEntry.Symbol));

            // Type POINTStackedPublic is visible anywhere
            Assert.IsTrue(currentProgram.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(nestedProgram.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(nested2Program.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcLocalEntry.Symbol.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPrivateEntry.Symbol.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(NestedProcPublicEntry.Symbol.IsTypeAccessible(POINTStackedPublicEntry.Symbol));
            Assert.IsTrue(TypeVisStackedPrgEntry.Symbol.IsTypeAccessible(POINTStackedPublicEntry.Symbol));

        }

        /// <summary>
        /// This Test tests all various accessibility on functions/Procedures, by considering from which kind of program or procedure a function from
        /// another program or procedure is asked for visibility access.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void FunctionVisibility_Prg_NestedPrg_Proc_Stacked()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "TypeVisNestedPrgAndProc.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 2);
            ProgramSymbol currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];//TypeVisNestedPrgAndProc
            SourceProgram mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.AreEqual(currentProgram, mainProgram.SemanticData);

            //Get the nested program.
            var nestedPrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested" });
            Assert.IsTrue(nestedPrgEntry.Count == 1);
            ProgramSymbol nestedProgram = nestedPrgEntry.Symbol;//Nested
            IList<NestedProgram> nestedPrgs = mainProgram.GetChildren<NestedProgram>();
            NestedProgram nestedPrg = nestedPrgs[0];
            Assert.AreEqual(nestedProgram, nestedPrg.SemanticData);

            //Get the Nested program of the Nested Program: Nested2
            var nestedNestedPrgs = nestedPrg.GetChildren<NestedProgram>();
            var nestedNestedPrg = nestedNestedPrgs[0];
            var nested2PrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested2" });
            Assert.IsTrue(nested2PrgEntry.Count == 1);
            ProgramSymbol nested2Program = nested2PrgEntry.Symbol;//Nested2
            IList<NestedProgram> nested2Prgs = nestedPrg.GetChildren<NestedProgram>();
            NestedProgram nested2Prg = nested2Prgs[0];
            Assert.AreEqual(nested2Program, nested2Prg.SemanticData);

            //Get the Nested program Nested21
            var nestedNested21Prgs = nested2Prg.GetChildren<NestedProgram>();
            var nestedNested21Prg = nestedNested21Prgs[0];
            var nested21PrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21" });
            Assert.IsTrue(nested21PrgEntry.Count == 1);
            var nested21PrgEntryBis = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21", "Nested2" });
            Assert.IsTrue(nested21PrgEntryBis.Count == 1);
            Assert.AreEqual(nested21PrgEntry.Symbol, nested21PrgEntryBis.Symbol);
            var nested21PrgEntryTer = document.Results.RootSymbolTable.ResolveProgram(new string[] { "Nested21", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(nested21PrgEntryTer.Count == 1);
            Assert.AreEqual(nested21PrgEntry.Symbol, nested21PrgEntryTer.Symbol);
            ProgramSymbol nested21Program = nested21PrgEntry.Symbol;//Nested21
            IList<NestedProgram> nested21Prgs = nested2Prg.GetChildren<NestedProgram>();
            NestedProgram nested21Prg = nested21Prgs[0];
            Assert.AreEqual(nested21Program, nested21Prg.SemanticData);

            //-----------------------------------------------
            //Immediatly Resolve nested procedures of Nested2
            //------------------------------------------------
            //NestedProcLocal
            var NestedProcLocalEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcLocal" });
            Assert.IsTrue(NestedProcLocalEntry.Count == 1);
            var NestedProcLocalEntryBis = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcLocal", "Nested" });
            Assert.IsTrue(NestedProcLocalEntryBis.Count == 1);
            Assert.AreEqual(NestedProcLocalEntry.Symbol, NestedProcLocalEntryBis.Symbol);


            //NestedProcPrivate
            var NestedProcPrivateEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPrivate" });
            Assert.IsTrue(NestedProcPrivateEntry.Count == 1);

            //NestedProcPublic
            var NestedProcPublicEntry = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPublic" });
            Assert.IsTrue(NestedProcPublicEntry.Count == 1);

            //-------------------------------------
            ///Resolve TypeVisStackedPrg
            //-------------------------------------
            var TypeVisStackedPrgEntry = document.Results.RootSymbolTable.ResolveProgram(new string[] { "TypeVisStackedPrg" });
            Assert.IsTrue(TypeVisStackedPrgEntry.Count == 1);

            //-------------------------------------------
            // RESOLVE all Functions/Procedures
            //-------------------------------------------
            var ProcLocal = document.Results.RootSymbolTable.ResolveFunction(new string[] { "ProcLocal" , "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(ProcLocal.Count == 1);
            var ProcPrivate = document.Results.RootSymbolTable.ResolveFunction(new string[] { "ProcPrivate", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(ProcPrivate.Count == 1);
            var ProcPublic = document.Results.RootSymbolTable.ResolveFunction(new string[] { "ProcPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(ProcPublic.Count == 1);

            var NestedProcLocal = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcLocal", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(NestedProcLocal.Count == 1);
            var NestedProcPrivate = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPrivate", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(NestedProcPrivate.Count == 1);
            var NestedProcPublic = document.Results.RootSymbolTable.ResolveFunction(new string[] { "NestedProcPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(NestedProcPublic.Count == 1);

            var Nested2ProcLocal = document.Results.RootSymbolTable.ResolveFunction(new string[] { "Nested2ProcLocal", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(Nested2ProcLocal.Count == 1);
            var Nested2ProcPrivate = document.Results.RootSymbolTable.ResolveFunction(new string[] { "Nested2ProcPrivate", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(Nested2ProcPrivate.Count == 1);
            var Nested2ProcPublic = document.Results.RootSymbolTable.ResolveFunction(new string[] { "Nested2ProcPublic", "TypeVisNestedPrgAndProc" });
            Assert.IsTrue(Nested2ProcPublic.Count == 1);

            var StackedProcLocal = document.Results.RootSymbolTable.ResolveFunction(new string[] { "StackedProcLocal", "TypeVisStackedPrg" });
            Assert.IsTrue(StackedProcLocal.Count == 1);
            var StackedProcPrivate = document.Results.RootSymbolTable.ResolveFunction(new string[] { "StackedProcPrivate", "TypeVisStackedPrg" });
            Assert.IsTrue(StackedProcPrivate.Count == 1);
            var StackedProcPublic = document.Results.RootSymbolTable.ResolveFunction(new string[] { "StackedProcPublic", "TypeVisStackedPrg" });
            Assert.IsTrue(StackedProcPublic.Count == 1);

            //--------------------------------------------------------
            // Check Functions Call from Program and Nested Prpogram
            //--------------------------------------------------------
            Assert.IsTrue((currentProgram.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((currentProgram.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((currentProgram.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((currentProgram.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((currentProgram.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((currentProgram.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((nestedProgram.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((nestedProgram.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((nestedProgram.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((nestedProgram.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((nestedProgram.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((nested2Program.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((nested2Program.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((nested2Program.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((nested2Program.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((nested2Program.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsFalse((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((TypeVisStackedPrgEntry.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            //--------------------------------------------------------
            // Check Functions Call from Functions
            //--------------------------------------------------------
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((ProcLocal.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((ProcLocal.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((ProcLocal.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((ProcLocal.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
#else
            Assert.IsFalse((ProcPrivate.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
#endif
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((ProcPrivate.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((ProcPrivate.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((ProcPrivate.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((ProcPrivate.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
#else
            Assert.IsFalse((ProcPublic.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
#endif
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((ProcPublic.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((ProcPublic.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((ProcPublic.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((ProcPublic.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((NestedProcLocal.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((NestedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((NestedProcLocal.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((NestedProcLocal.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcLocal.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((NestedProcPrivate.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
#else
            Assert.IsFalse((NestedProcPrivate.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
#endif
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((NestedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((NestedProcPrivate.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((NestedProcPrivate.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPrivate.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((NestedProcPublic.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
#else
            Assert.IsFalse((NestedProcPublic.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
#endif
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((NestedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((NestedProcPublic.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((NestedProcPublic.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((NestedProcPublic.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((Nested2ProcLocal.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcLocal.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcLocal.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((Nested2ProcLocal.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcLocal.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((Nested2ProcPrivate.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcPrivate.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
#else
            Assert.IsFalse((Nested2ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
#endif
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcPrivate.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((Nested2ProcPrivate.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPrivate.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((Nested2ProcPublic.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcPublic.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
#else
            Assert.IsFalse((Nested2ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
#endif
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsFalse((Nested2ProcPublic.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsFalse((Nested2ProcPublic.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((Nested2ProcPublic.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcLocal.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
#else
            Assert.IsFalse((StackedProcPrivate.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
#endif
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPrivate.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(ProcPublic.Symbol)));
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(NestedProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(NestedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(NestedProcPublic.Symbol)));
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcLocal.Symbol)));
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(Nested2ProcPublic.Symbol)));
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
#else
            Assert.IsFalse((StackedProcPublic.Symbol.IsFunctionAccessible(StackedProcLocal.Symbol)));
#endif
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(StackedProcPrivate.Symbol)));
            Assert.IsTrue((StackedProcPublic.Symbol.IsFunctionAccessible(StackedProcPublic.Symbol)));

        }

        /// <summary>
        /// 
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Types")]
        public void TypedefDuplicated()
        {
            string path = Path.Combine(GetTestLocation(), "SemanticDomain", "TypedefDuplicated.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null, ExecutionStep.SemanticCheck);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            var currentProgram = document.Results.PrgSymbolTblBuilder.Programs[0];

            //Check that there is one error and the Typedef User is reclared twice.
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.TypeAlreadyDeclared, "User"));
            Assert.AreEqual<string>(document.Results.PrgSymbolTblBuilder.Diagnostics[0].Message, d.Message);
        }
    }
}
