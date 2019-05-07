using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using System.Diagnostics;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Test.Utils;
using System.Text;
using TypeCobol.Compiler.Parser;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Test.Domain
{
    [TestClass]
    public class SemanticDomainTest
    {
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;

        [TestInitialize]
        public void TestInitialize()
        {
            SymbolTableBuilder.Root = null;
            //Create a default configurations for options
            DefaultConfig = new TypeCobolConfiguration();
            if (File.Exists(DefaultIntrinsicPath))
            {
                DefaultConfig.Copies.Add(DefaultIntrinsicPath);
            }

            DefaultConfig.Dependencies.Add(Path.Combine(Directory.GetCurrentDirectory(), "resources", "dependencies"));
            SymbolTableBuilder.Config = DefaultConfig;

            //Force the creation of the Global Symbol Table
            var global = SymbolTableBuilder.Root;

            //Allocate a static Program Symbol Table Builder
            BuilderNodeListenerFactory = () =>
            {
                Builder = new ProgramSymbolTableBuilder();
                return Builder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(BuilderNodeListenerFactory);
        }

        private static void RemovePrograms(ProgramSymbol prog)
        {
            foreach (var nestPrg in prog.Programs)
            {
                SymbolTableBuilder.Root.RemoveProgram(prog);
                RemovePrograms(nestPrg);
            }
            SymbolTableBuilder.Root.RemoveProgram(prog);            
        }

        [TestCleanup]
        public void TestCleanup()
        {
            if (BuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(BuilderNodeListenerFactory);
                if (Builder.Programs.Count != 0)
                {
                    foreach (var prog in Builder.Programs)
                    {
                        RemovePrograms(prog);
                    }
                }
            }
            SymbolTableBuilder.Root = null;
        }

        /// <summary>
        /// This Test tests the expansion of a Type Date inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
#if DOMAIN_CHECKER
        [Ignore]//Ignore because CrossCheck performs a program expansion
#endif
        public void DateTypeExpanderCheck()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "TypeCobol", "Type-Date.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

            //Get olddate symbol
            var olddate = currentProgram.ResolveReference(new string[] { "olddate" }, false);
            Assert.IsTrue(olddate.Count == 1);
            Assert.IsNotNull(olddate.Symbol.Type);
            TypedefExpander tdExpander = new TypedefExpander(currentProgram);
            Type te_olddate = olddate.Symbol.Type.Accept(tdExpander, olddate.Symbol);
            Assert.AreEqual(te_olddate, olddate.Symbol.Type);

            //Get today symbol
            var today = currentProgram.ResolveReference(new string[] { "today" }, false);
            Assert.IsTrue(today.Count == 1);
            Assert.IsNotNull(today.Symbol.Type);
            Assert.IsTrue(today.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(today.Symbol.Type.TypeComponent == BuiltinTypes.DateType);

            //Before expansion there are no YYYY, MM, DD variables in the program
            var yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 0);
            var mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 0);
            var dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 0);

            SymbolExpander symExpander = new SymbolExpander(currentProgram, tdExpander);
            today.Symbol.Accept(symExpander, null);
            Type te_today = today.Symbol.Type;
            Assert.IsNotNull(te_today);
            Assert.IsTrue(te_today.Tag == Type.Tags.Group);

            //After expansion there are YYYY, MM, DD variables in the program
            yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 1);
            mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 1);
            dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 1);
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

        /// <summary>
        /// This Test tests the expansion of all Types Date inside a program.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "TypeExpander")]
#if DOMAIN_CHECKER
        [Ignore]//Ignore because CrossCheck performs a program expansion
#endif
        public void AllDateTypeExpanderCheck()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "TypeCobol", "Type-Date.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

            //Before expansion there are no YYYY, MM, DD variables in the program
            var yyyy = currentProgram.ResolveReference(new string[] { "yyyy" }, false);
            Assert.IsTrue(yyyy.Count == 0);
            var mm = currentProgram.ResolveReference(new string[] { "mm" }, false);
            Assert.IsTrue(mm.Count == 0);
            var dd = currentProgram.ResolveReference(new string[] { "dd" }, false);
            Assert.IsTrue(dd.Count == 0);

            SymbolExpander symExpander = new SymbolExpander(currentProgram);
            currentProgram.Accept(symExpander, null);

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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "TypeCobol", "Type-Bool.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.FreeTextFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

            //Before expansion there are no check-false variables in the program
            var check_false = currentProgram.ResolveReference(new string[] { "check-false" }, false);
            Assert.IsTrue(check_false.Count == 0);
            var managed_false = currentProgram.ResolveReference(new string[] { "managed-false" }, false);
            Assert.IsTrue(check_false.Count == 0);

            SymbolExpander symExpander = new SymbolExpander(currentProgram);
            currentProgram.Accept(symExpander, null);

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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "TypeCobol", "Typedef-public.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
            
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
            //Lookup the type "typeOfDays"
            var typeOfDays = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDays" }, false);
            Assert.IsNotNull(typeOfDays);
            Assert.IsTrue(typeOfDays.Count == 1);
            Assert.IsNotNull(typeOfDays.Symbol.Type);
            Assert.IsTrue(typeOfDays.Symbol.HasFlag(Symbol.Flags.Public | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDays.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDays.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Lookup the type "typeOfDaysPrivate"
            var typeOfDaysPrivate = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDaysPrivate" }, false);
            Assert.IsNotNull(typeOfDaysPrivate);
            Assert.IsTrue(typeOfDaysPrivate.Count == 1);
            Assert.IsNotNull(typeOfDaysPrivate.Symbol.Type);
            Assert.IsFalse(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysPrivate.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Lookup the type "typeOfDaysNoModifier"
            var typeOfDaysNoModifier = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDaysNoModifier" }, false);
            Assert.IsNotNull(typeOfDaysNoModifier);
            Assert.IsTrue(typeOfDaysNoModifier.Count == 1);
            Assert.IsNotNull(typeOfDaysNoModifier.Symbol.Type);
            Assert.IsFalse(typeOfDaysNoModifier.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //Before expanding, all variables have a TYPEDEF Type
            TypedefSymbol[] types = new TypedefSymbol[] { typeOfDays.Symbol, typeOfDaysPrivate.Symbol, typeOfDaysNoModifier.Symbol };
            VariableSymbol[] vars = new VariableSymbol[4];
            for (int i = 1; i < 4; i++)
            {
                var vari = nestPrgSym.ResolveReference(new string[] { "var" + i }, false);                
                Assert.IsTrue(vari.Count == (i == 1 ? 3 : 1));
                vars[i - 1] = vari.Symbol;
                Assert.IsTrue(vari.Symbol.Type == types[i-1].Type);
            }

            SymbolExpander symExpander = new SymbolExpander(currentProgram);
            currentProgram.Accept(symExpander, null);

            //After expanding all variables have a PICTURE type.
            for (int i = 1; i < 4; i++)
            {
                Assert.IsNotNull(vars[i - 1].Type);
                Assert.IsTrue(vars[i-1].Type.Tag == Type.Tags.Picture);
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "ComplexTypeExpand.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

            //-------------------------
            //Expand rcarray variable.
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

            SymbolExpander symExpander = new SymbolExpander(currentProgram);

            //Now rcarray is an array of Record.
            rcarray.Symbol.Accept(symExpander, null);
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

            //-------------------------
            //Expand rcpt variable.
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

            //No rcpt is of type Record
            rcpt.Symbol.Accept(symExpander, currentProgram);
            Assert.IsNotNull(rcpt.Symbol.Type);
            Assert.IsTrue(rcpt.Symbol.Type.Tag == Type.Tags.Group);

            //After expanding there are now x, y, pt1, pt2 variables in the program.
            x = currentProgram.ResolveReference(new string[] { "x" }, false);
            Assert.IsTrue(x.Count == 2);
            y = currentProgram.ResolveReference(new string[] { "y" }, false);
            Assert.IsTrue(y.Count == 2);
            pt1 = currentProgram.ResolveReference(new string[] { "pt1" }, false);
            Assert.IsTrue(pt1.Count == 1);
            Assert.IsNotNull(pt1.Symbol.Type);
            Assert.IsNotNull(pt1.Symbol.Type.Tag == Type.Tags.Group);
            pt2 = currentProgram.ResolveReference(new string[] { "pt2" }, false);
            Assert.IsTrue(pt2.Count == 1);
            Assert.IsNotNull(pt2.Symbol.Type);
            Assert.IsNotNull(pt2.Symbol.Type.Tag == Type.Tags.Group);

            var pt1_x = currentProgram.ResolveReference(new string[] { "x", "pt1" }, false);
            Assert.IsTrue(pt1_x.Count == 1);            
            var pt1_y = currentProgram.ResolveReference(new string[] { "y", "pt1" }, false);
            Assert.IsTrue(pt1_y.Count == 1);
            var pt2_x = currentProgram.ResolveReference(new string[] { "x", "pt2" }, false);
            Assert.IsTrue(pt2_x.Count == 1);
            var pt2_y = currentProgram.ResolveReference(new string[] { "y", "pt2" }, false);
            Assert.IsTrue(pt2_y.Count == 1);

            Assert.AreNotSame(pt1_x.Symbol, pt2_x.Symbol);
            Assert.AreNotSame(pt1_y.Symbol, pt2_y.Symbol);
            Assert.IsTrue((pt1_x.Symbol == x[0] && pt2_x.Symbol == x[1])|| (pt1_x.Symbol == x[1] && pt2_x.Symbol == x[0]) );
            Assert.IsTrue((pt1_y.Symbol == y[0] && pt2_y.Symbol == y[1]) || (pt1_y.Symbol == y[1] && pt2_y.Symbol == y[0]));

            //-------------------------
            //Expand grcarray variable.
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
            var pt = currentProgram.ReverseResolveType(SymbolTableBuilder.Root, new string[] {"pt"}, false);
            Assert.IsTrue(pt.Count == 1);
            Assert.IsTrue(rc.Symbol.Type == pt.Symbol.Type);

            //There is one arr variable of type RECTARRAY in the program .
            var arr = currentProgram.ResolveReference(new string[] { "arr" }, false);
            Assert.IsTrue(arr.Count == 1);
            Assert.IsNotNull(arr.Symbol.Type);
            Assert.IsTrue(arr.Symbol.Type.Tag == Type.Tags.Typedef);
            var rectarray = currentProgram.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "rectarray" }, false);
            Assert.IsTrue(rectarray.Count == 1);
            Assert.IsTrue(arr.Symbol.Type == rectarray.Symbol.Type);

            //Now expansion
            grcarray.Symbol.Accept(symExpander, null);

            //After expansion
            //____________________
            //There are now two rc variables both of type Record .
            var rc_after = currentProgram.ResolveReference(new string[] { "rc" }, false);
            Assert.IsTrue(rc_after.Count == 2);
            Assert.IsNotNull(rc_after[0].Type);
            Assert.IsNotNull(rc_after[0].Type.Tag == Type.Tags.Group);
            Assert.IsNotNull(rc_after[1].Type);
            Assert.IsNotNull(rc_after[1].Type.Tag == Type.Tags.Group);
            Assert.IsTrue(rc_after[0] != rc_after[1]);
            var rc_arr = currentProgram.ResolveReference(new string[] { "rc", "arr" }, false);
            Assert.IsTrue(rc_arr.Count == 1);
            Assert.IsTrue(rc_arr.Symbol == rc_after[0] || rc_arr.Symbol == rc_after[1]);

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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "ComplexTypeExpand.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);

            var currentProgram = Builder.Programs[0];
            //-------------------------
            //Expand rcarray variable.
            //-------------------------
            var rcarray = currentProgram.ResolveReference(new string[] { "rcarray" }, false);
            Assert.IsTrue(rcarray.Count == 1);
            Assert.IsNotNull(rcarray.Symbol.Type);
            Assert.IsTrue(rcarray.Symbol.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(rcarray.Symbol.Type.TypeComponent.Tag == Type.Tags.Typedef);

            SymbolExpander symExpander = new SymbolExpander(currentProgram);
            rcarray.Symbol.Accept(symExpander, null);
            //After expanding there are X1, Y1, X2, Y2 variables in the program, will all 02 Level
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
            LevelRenumber levRenum = new LevelRenumber();
            rcarray.Symbol.Accept(levRenum, 1);
            //After renumber from 1, X1, Y1, X2, Y2 level has not changed because their levels were already 2
            Assert.IsTrue(x1.Symbol.Level == 2);
            Assert.IsTrue(y1.Symbol.Level == 2);
            Assert.IsTrue(x2.Symbol.Level == 2);
            Assert.IsTrue(y2.Symbol.Level == 2);

            //-------------------------
            //Expand rcpt variable.
            //-------------------------
            var rcpt = currentProgram.ResolveReference(new string[] { "rcpt" }, false);
            Assert.IsTrue(rcpt.Count == 1);
            Assert.IsNotNull(rcpt.Symbol.Type);
            Assert.IsTrue(rcpt.Symbol.Type.Tag == Type.Tags.Typedef);

            rcpt.Symbol.Accept(symExpander, null);
            //After expanding there were x, y, pt1, pt2 variables in the program
            //They are all of level 05.
            var x = currentProgram.ResolveReference(new string[] { "x" }, false);
            Assert.IsTrue(x.Count == 2);
            Assert.IsTrue(x[0].Level == 5);
            Assert.IsTrue(x[1].Level == 5);
            var y = currentProgram.ResolveReference(new string[] { "y" }, false);
            Assert.IsTrue(y.Count == 2);
            Assert.IsTrue(y[0].Level == 5);
            Assert.IsTrue(y[1].Level == 5);
            var pt1 = currentProgram.ResolveReference(new string[] { "pt1" }, false);
            Assert.IsTrue(pt1.Count == 1);
            Assert.IsTrue(pt1.Symbol.Level == 5);
            var pt2 = currentProgram.ResolveReference(new string[] { "pt2" }, false);
            Assert.IsTrue(pt2.Count == 1);
            Assert.IsTrue(pt2.Symbol.Level == 5);

            rcpt.Symbol.Accept(levRenum, 1);

            //After Level renumber, all x and y are of level 3, pt1 and pt2 of level 2
            Assert.IsTrue(rcpt.Symbol.Level == 1);
            Assert.IsTrue(x[0].Level == 3);
            Assert.IsTrue(x[1].Level == 3);
            Assert.IsTrue(y[0].Level == 3);
            Assert.IsTrue(y[1].Level == 3);
            Assert.IsTrue(pt1.Symbol.Level == 2);
            Assert.IsTrue(pt2.Symbol.Level == 2);
            //-------------------------
            //Expand grcarray variable.
            //-------------------------
            var grcarray = currentProgram.ResolveReference(new string[] { "grcarray" }, false);
            Assert.IsTrue(grcarray.Count == 1);
            Assert.IsNotNull(grcarray.Symbol.Type);
            Assert.IsTrue(grcarray.Symbol.Type.Tag == Type.Tags.Group);

            grcarray.Symbol.Accept(symExpander, null);
            //After the expansion the situation is like this
            string grcarrayDump0 = grcarray.ToString();
            //        01 grcarray.
            //05 rc.
            //    05 X PIC 9(4).
            //    05 Y PIC 9(4).
            //05 xx PIC X(9).
            //05 grp.
            //    10 arr.
            //        05 a.
            //                          10 rc.
            //                05 PT1.
            //                    05 X PIC 9(4).
            //                    05 Y PIC 9(4).
            //                05 PT2.
            //                    05 X PIC 9(4).
            //                    05 Y PIC 9(4).
            x = currentProgram.ResolveReference(new string[] { "x", "grcarray" }, false);
            Assert.IsTrue(x.Count == 3);
            Assert.IsTrue(x[0].Level == 5);
            Assert.IsTrue(x[1].Level == 5);
            Assert.IsTrue(x[2].Level == 5);

            var x_pt1_a = currentProgram.ResolveReference(new string[] { "x", "pt1", "a" }, false);
            Assert.IsTrue(x_pt1_a.Count == 1);
            Assert.IsTrue(x_pt1_a.Symbol.Level == 5);
            var y_pt1_a = currentProgram.ResolveReference(new string[] { "y", "pt1", "a" }, false);
            Assert.IsTrue(y_pt1_a.Count == 1);
            Assert.IsTrue(y_pt1_a.Symbol.Level == 5);
            var x_pt2_a = currentProgram.ResolveReference(new string[] { "x", "pt2", "a" }, false);
            Assert.IsTrue(x_pt2_a.Count == 1);
            Assert.IsTrue(x_pt2_a.Symbol.Level == 5);
            var y_pt2_a = currentProgram.ResolveReference(new string[] { "y", "pt2", "a" }, false);
            Assert.IsTrue(y_pt2_a.Count == 1);
            Assert.IsTrue(y_pt2_a.Symbol.Level == 5);

            y = currentProgram.ResolveReference(new string[] { "y", "grcarray" }, false);
            Assert.IsTrue(y.Count == 3);
            Assert.IsTrue(y[0].Level == 5);
            Assert.IsTrue(y[1].Level == 5);
            Assert.IsTrue(y[2].Level == 5);

            var a = currentProgram.ResolveReference(new string[] { "a" }, false);
            Assert.IsTrue(a.Count == 1);
            Assert.IsTrue(a.Symbol.Level == 5);
            var a_rc = currentProgram.ResolveReference(new string[] { "rc", "a" }, false);
            Assert.IsTrue(a_rc.Count == 1);
            Assert.IsTrue(a_rc.Symbol.Level == 10);

            var a_pt1 = currentProgram.ResolveReference(new string[] { "pt1", "a" }, false);
            Assert.IsTrue(a_pt1.Count == 1);
            Assert.IsTrue(a_pt1.Symbol.Level == 5);
            var a_pt2 = currentProgram.ResolveReference(new string[] { "pt2", "a" }, false);
            Assert.IsTrue(a_pt2.Count == 1);
            Assert.IsTrue(a_pt2.Symbol.Level == 5);

            grcarray.Symbol.Accept(levRenum, 1);
            //After Level renumber the situation is like that.
            string grcarrayDump1 = grcarray.Symbol.ToString();
            //        01 grcarray.
            //02 rc.
            //    03 X PIC 9(4).
            //    03 Y PIC 9(4).
            //02 xx PIC X(9).
            //02 grp.
            //    03 arr.
            //        04 a.
            //                          05 rc.
            //                06 PT1.
            //                    07 X PIC 9(4).
            //                    07 Y PIC 9(4).
            //                06 PT2.
            //                    07 X PIC 9(4).
            //                    07 Y PIC 9(4).
            for (int i = 0; i < 3; i++)
            {
                if (x[i].Owner != a_pt1.Symbol && x[i].Owner != a_pt2.Symbol)
                {
                    Assert.IsTrue(x[i].Level == 3);
                }
                if (y[i].Owner != a_pt1.Symbol && y[i].Owner != a_pt2.Symbol)
                {
                    Assert.IsTrue(y[i].Level == 3);
                }
            }

            Assert.IsTrue(x_pt1_a.Symbol.Level == 7);
            Assert.IsTrue(y_pt1_a.Symbol.Level == 7);
            Assert.IsTrue(x_pt2_a.Symbol.Level == 7);
            Assert.IsTrue(y_pt2_a.Symbol.Level == 7);

            a = currentProgram.ResolveReference(new string[] { "a" }, false);
            Assert.IsTrue(a.Symbol.Level == 4);
            Assert.IsTrue(a_rc.Symbol.Level == 5);
            Assert.IsTrue(a_pt1.Symbol.Level == 6);
            Assert.IsTrue(a_pt2.Symbol.Level == 6);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RedefineSymbol")]
        public void ErrRedefinesImmediatlyPrecede()
        {
            //string path, List<Skeleton> skeletons = null, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null, Compiler.CodeModel.SymbolTable baseSymTable = null            
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "ErrRedefinesImmPrec.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.ErrRedefineWasNotImmediatlyPrec, "RX", 5));
            Assert.AreEqual<string>(Builder.Diagnostics[0].Message, d.Message);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RedefineSymbol")]
        public void RedefinesTest()
        {
            //string path, List<Skeleton> skeletons = null, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null, Compiler.CodeModel.SymbolTable baseSymTable = null            
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "Cobol85", "Redefines.rdz.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

            //Get MyVar1 symbol
            var myvar1 = currentProgram.ResolveReference(new string[] {"myvar1"}, false);
            Assert.IsTrue(myvar1.Count == 1);

            //Check Errors : Cannot REDEFINES access MyVar1.
            Assert.IsTrue(Builder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.ErrRedefineWasNotImmediatlyPrec, myvar1.Symbol.Name, 1));
            Assert.AreEqual<string>(Builder.Diagnostics[0].Message, d.Message);

            //Get MyVar2 symbol with PIC X(9)
            var myvar2s = currentProgram.ResolveReference(new string[] { "myvar2" }, false);
            Assert.IsTrue(myvar2s.Count == 3);            
            Assert.IsTrue(myvar2s[0] != myvar2s[1] && myvar2s[0] != myvar2s[2] && myvar2s[1] != myvar2s[2]);
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
            Assert.AreEqual(((RedefinesSymbol)filter.Symbol).ToppestRedefined, VarGroup.Symbol);
        }

        /// <summary>
        /// This test tests that RENAMES declaration variable symbol are built.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void RenamesResolve0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "RenamesResolve0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Diagnostics.Count == 0);
            //Locate Redefines
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
            var vars = currentProgram.ResolveReference(new string[] {"yy"}, false);
            Assert.IsTrue(vars.Count == 1);
            string yy = vars[0].ToString();
            Assert.AreEqual(yy, "66 YY RENAMES RX THRU RY." +Environment.NewLine);
        }

        /// <summary>
        /// Simple Rename Test based on TypeCobol.Test/Parser/Programs/Cobol2002.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void RenamePgmTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "Cobol2002", "Renames.rdz.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];            

            Assert.IsTrue(Builder.Diagnostics.Count == 1);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser,
                0, 0, 0,
                string.Format(TypeCobolResource.CannotRenamesLevel, 1));

            Assert.AreEqual<string>(Builder.Diagnostics[0].Message, d.Message);
        }
        /// <summary>
        /// This test test that when renames objects are not found a diagnostics is emitted.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "RenamesSymbol")]
        public void BadRenames0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "BadRenames0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);


            //Locate Redefines
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
            var vars = currentProgram.ResolveReference(new string[] { "yy" }, false);
            Assert.IsTrue(vars.Count == 1);

            Assert.IsTrue(Builder.Diagnostics.Count == 2);
            Diagnostic d = new Diagnostic(MessageCode.SemanticTCErrorInParser, 0, 0, 0,
                string.Format(TypeCobolResource.RenamesObjectNotFound, "MPOINT.RX"));

            Assert.AreEqual<string>(Builder.Diagnostics[0].Message, d.Message);
            Assert.AreEqual<string>(Builder.Diagnostics[1].Message, d.Message);
        }

        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Typedef")]
        public void TypedefCyclic0()
        {
            //string path, List<Skeleton> skeletons = null, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null, Compiler.CodeModel.SymbolTable baseSymTable = null            
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "TypedefCyclic0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
            var pointEntry = currentProgram.Types.Lookup("POINT");
            Assert.IsNotNull(pointEntry);
            Assert.IsTrue(pointEntry.Count == 1);
            var tPoint = pointEntry.Symbol;
            Assert.IsTrue(tPoint.Type != null);
            Assert.IsTrue(tPoint.Type.Tag == Type.Tags.Typedef);
            CyclicTypeChecker checker = new CyclicTypeChecker();
            Assert.IsFalse(tPoint.Type.Accept(checker, null));

            pointEntry = currentProgram.Types.Lookup("POINT-CYC0");
            Assert.IsNotNull(pointEntry);
            Assert.IsTrue(pointEntry.Count == 1);
            var tPointCyc0 = pointEntry.Symbol;
            Assert.IsTrue(tPointCyc0.Type != null);
            Assert.IsTrue(tPointCyc0.Type.Tag == Type.Tags.Typedef);
            checker = new CyclicTypeChecker();
            Assert.IsTrue(tPointCyc0.Type.Accept(checker, null));
            Assert.IsTrue(checker.CyclicType == tPointCyc0.Type);

            pointEntry = currentProgram.Types.Lookup("POINT-CYC1");
            Assert.IsNotNull(pointEntry);
            Assert.IsTrue(pointEntry.Count == 1);
            var tPointCyc1 = pointEntry.Symbol;
            Assert.IsTrue(tPointCyc1.Type != null);
            Assert.IsTrue(tPointCyc1.Type.Tag == Type.Tags.Typedef);
            checker = new CyclicTypeChecker();
            Assert.IsTrue(tPointCyc1.Type.Accept(checker, null));
            Assert.IsTrue(checker.CyclicType == tPointCyc0.Type);

            //NO P1, P2, P3 and P4 symbols
            for (int i = 1; i <= 4; i++)
            {
                var refs0 = currentProgram.ResolveReference(new string[] {"p" + i}, false);
                Assert.IsNotNull(refs0);
                Assert.IsTrue(refs0.Count == 0);
            }

            //Just Check that P5 if of type POINT which we know is cyclic.
            var refs = currentProgram.ResolveReference(new string[] {"p5"}, false);
            Assert.IsNotNull(refs);
            Assert.IsTrue(refs.Count == 1);
            var p5 = refs.Symbol;
            Assert.IsTrue(p5.Type == tPointCyc0.Type);

            //Just Check that P6 if of type POINT-CYC1 which we know is cyclic.
            refs = currentProgram.ResolveReference(new string[] {"p6"}, false);
            Assert.IsNotNull(refs);
            Assert.IsTrue(refs.Count == 1);
            var p6 = refs.Symbol;
            Assert.IsTrue(p6.Type == tPointCyc1.Type);

            //Just Check that P7 if of type Array of POINT-CYC0 which we know leads cyclic.
            refs = currentProgram.ResolveReference(new string[] {"p7"}, false);
            Assert.IsNotNull(refs);
            Assert.IsTrue(refs.Count == 1);
            var p7 = refs.Symbol;
            Assert.IsTrue(p7.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(p7.Type.TypeComponent == tPointCyc0.Type);
            checker = new CyclicTypeChecker();
            Assert.IsTrue(p7.Type.Accept(checker, null));
            Assert.IsTrue(checker.CyclicType == tPointCyc0.Type);

            //Just Check that P8 if of type Array of POINT-CYC1 which we know leads cyclic.
            refs = currentProgram.ResolveReference(new string[] {"p8"}, false);
            Assert.IsNotNull(refs);
            Assert.IsTrue(refs.Count == 1);
            var p8 = refs.Symbol;
            Assert.IsTrue(p8.Type.Tag == Type.Tags.Array);
            Assert.IsTrue(p8.Type.TypeComponent == tPointCyc1.Type);
            checker = new CyclicTypeChecker();
            Assert.IsTrue(p8.Type.Accept(checker, null));
            Assert.IsTrue(checker.CyclicType == tPointCyc0.Type);

            //Just Check that P9 is a RECORD with a field XX of a cyclic type.
            refs = currentProgram.ResolveReference(new string[] {"p9"}, false);
            Assert.IsNotNull(refs);
            Assert.IsTrue(refs.Count == 1);
            var p9 = refs.Symbol;
            Assert.IsTrue(p9.Type.Tag == Type.Tags.Group);
            checker = new CyclicTypeChecker();
            Assert.IsTrue(p9.Type.Accept(checker, null));
            Assert.IsTrue(checker.CyclicType == tPointCyc0.Type);
            var xx_refs = currentProgram.ResolveReference(new string[] {"xx"}, false);
            //One of the XX has type is P9::XX
            Assert.IsTrue(xx_refs.Count == 2);
            Assert.IsTrue((xx_refs[0].Type == tPointCyc1.Type && xx_refs[1].Type == tPoint.Type) ||
                          (xx_refs[1].Type == tPointCyc1.Type && xx_refs[0].Type == tPoint.Type)
            );

        }

        /// <summary>
        /// This test is for testing non global variable access with Working or local storage section thru the procedure division.
        /// </summary>
        [TestMethod]
        [TestCategory("SemanticDomain")]
        [TestProperty("Object", "Visibility")]
        public void VisibilityProgramVariable0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisPrgVar0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisPrgGblVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
#if DOMAIN_CHECKER
        [Ignore]//Because visibility are not the same using CrossChecker
#endif
        public void VisibilityProgramProcedureVariable0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisPrgProcVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisNestedPrgVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisNestedPrgVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
#if DOMAIN_CHECKER
        [Ignore]//Because visibility are not the same using CrossChecker
#endif
        public void VisibilityProgramNestedProcedureVariable0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "SemanticDomain", "VisPrgNestedProcVar0.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Parser", "Programs", "TypeCobol", "Typedef-public.rdz.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            var currentProgram = Builder.Programs[0];

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
            var typeOfDays = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDays" }, false);
            Assert.IsNotNull(typeOfDays);
            Assert.IsTrue(typeOfDays.Count == 1);
            Assert.IsNotNull(typeOfDays.Symbol.Type);
            Assert.IsTrue(typeOfDays.Symbol.HasFlag(Symbol.Flags.Public | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDays.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDays.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //-------------------------------------------------
            //Get the variable var1 and its type accessibility.
            //-------------------------------------------------
            var var1 = nestPrgSym.ResolveReference(new string[] {"var1"}, false);
            Assert.IsTrue(var1.Count == 3);
            for (int i = 0; i < 3; i++)
            {
                VariableSymbol v1 = var1[i];
                Assert.IsTrue(v1.Type == typeOfDays.Symbol.Type);
            }
            //Check the the type was accessible
            Assert.IsTrue(nestPrgSym.IsTypeAccessible(typeOfDays.Symbol));

            //Lookup the type "typeOfDaysPrivate"
            var typeOfDaysPrivate = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDaysPrivate" }, false);
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
            Assert.IsTrue(var2.Count == 1);
            Assert.IsTrue(var2.Symbol.Type == typeOfDaysPrivate.Symbol.Type);
            //Check the the type was accessible
            Assert.IsTrue(nestPrgSym.IsTypeAccessible(typeOfDaysPrivate.Symbol));

            //Lookup the type "typeOfDaysNoModifier"
            var typeOfDaysNoModifier = nestPrgSym.ReverseResolveType(SymbolTableBuilder.Root, new string[] { "typeOfDaysNoModifier" }, false);
            Assert.IsNotNull(typeOfDaysNoModifier);
            Assert.IsTrue(typeOfDaysNoModifier.Count == 1);
            Assert.IsNotNull(typeOfDaysNoModifier.Symbol.Type);
            Assert.IsFalse(typeOfDaysNoModifier.Symbol.HasFlag(Symbol.Flags.Public));
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.HasFlag(Symbol.Flags.Private | Symbol.Flags.Strict));
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.Type.Tag == Type.Tags.Typedef);
            Assert.IsTrue(typeOfDaysNoModifier.Symbol.Type.TypeComponent.Tag == Type.Tags.Picture);

            //-------------------------------------------------
            //Get the variable var3 and its type accessibility.
            //-------------------------------------------------
            var var3 = nestPrgSym.ResolveReference(new string[] { "var3" }, false);
            Assert.IsTrue(var3.Count == 1);
            Assert.IsTrue(var3.Symbol.Type == typeOfDaysNoModifier.Symbol.Type);
            //Check the the type was accessible
            Assert.IsTrue(nestPrgSym.IsTypeAccessible(typeOfDaysNoModifier.Symbol));

        }
    }
}
