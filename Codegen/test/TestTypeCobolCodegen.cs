using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Codegen {

	[TestClass]
	public class TestTypeCobolCodegen {

        [TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseEmpty() {			
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseTypes() {
			string file = Path.Combine("TypeCobol", "Types");
            CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl");
            file = Path.Combine("TypeCobol", "Types2");
        }

		[TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseBooleans() {
			string file = Path.Combine("TypeCobol","TypeBOOL");
            CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl");
		}

        [TestMethod]
        [TestCategory("Codegen")]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void TypeBoolValue()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypeBoolValue") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefInnerBool()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefInnerBool") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefInnerBool2()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefInnerBool2") + ".rdz.cbl");
        }

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseUnsafe() {
			string file = Path.Combine("TypeCobol","unsafe");
            CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void QualifiedNamesInsideFunctions() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "QualifiedNamesInsideFunctions") +".rdz.cbl");
            //CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "QualifiedNamesInsideFunctions2") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseQualifiedNames()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "QualifiedNames") + ".rdz.cbl");
        }

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
        public void ParseQualifReferenceModifier()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "QualifReferenceModifier") + ".rdz.cbl");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
        public void ParseQualifReferenceModifierProc()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "QualifReferenceModifierProc") + ".rdz.cbl");
		}        

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctions() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Function")+".rdz.cbl");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctionsDeclaration() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "FunDeclare") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "FunDeclareWithExec") + ".rdz.cbl");
        }


        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseProcedureWithExec()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenerateAsNested", "FunDeclareWithExec-PrivateOnly") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenerateAsNested", "FunDeclareWithExec-PublicOnly") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenerateAsNested", "FunDeclareWithExec-PublicPrivate") + ".rdz.cbl");
        }


        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseLibrary() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Library")+".rdz.cbl");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Library2")+".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Library4") + ".rdz.tcbl");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void CallPublicProcFromPrivateProc() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPrivateProc") +".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPrivateProc3") +".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPrivateProc4") +".rdz.tcbl");
        }

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void CallPublicProcFromPublicProc() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPublicProc") +".rdz.tcbl");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPublicProc-DeclarativesNoDebug") +".rdz.tcbl");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcFromPublicProc-DeclarativesWithDebug") +".rdz.tcbl");
		}
	    
		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void CallPublicProcInsideProcOfSameProgram() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CallPublicProcInsideProcOfSameProgram") +".rdz.tcbl");
		}

	    [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseProcedureCall() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","ProcedureCall")+".rdz.cbl");
		}

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseProcedureCallPrivateQualifier() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-PrivateQualifier") +".rdz.tcbl");
		}
        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseProcedureCallPublic() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-Public") +".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-Public2") + ".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-Public3") + ".rdz.tcbl");
		}

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void ParseProcedureCallPublicAndDeclaratives()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-PublicAndDeclaratives") + ".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-PublicAndDeclaratives2") + ".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-PublicAndDeclaratives3") + ".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureCall-PublicAndDeclaratives4") + ".rdz.tcbl");
        }

	    [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseProcCallWithQualified()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcCallWithQualified") + ".rdz.tcbl");
        }

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void Codegen() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Codegen")+".rdz.cbl");
		}

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void CopyReplace4Colon()
        {
            string dir = System.IO.Directory.GetCurrentDirectory();
            string copies = Path.Combine(dir, "resources", "input", "TypeCobol", "CopyReplace4Colon");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CopyReplace4Colon", "CopyReplace4Colon") + ".rdz.cbl",
                null, new List<string>() { copies });
        }

        [TestMethod][Ignore]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseSuccessiveCalls() {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","SuccessiveCalls")+".rdz.cbl");
		}

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
        public void ParseTypeDefLevel88()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypeDefLevel88") + ".rdz.cbl");
		}

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseTypedefPublicPrivate()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Typedef") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseTypedefNoPic()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefNoPic") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseTypedefWithComments()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefComments") + ".rdz.tcbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureScalarTypedef") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseLineExceed()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed2") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseLineExceedAutoSplit()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed3") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed4") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed5") + ".rdz.cbl");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "LineExceed6") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseMoveUnsafe()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "MoveUnsafe") + ".rdz.tcbl");
        }        

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseSetBoolProcedure()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "SetBoolProcedure") + ".rdz.tcbl");
        }        

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ParseVariableTypeExternal()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "VariableTypeExternal") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ProcPublicWithoutDataDivision()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcPublicWithoutDataDivision") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefFixedFormatCutLine()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefFixedFormatCutLine") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifBoolSet()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifBoolSet") + ".rdz.tcbl");
        }
        
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifBoolSetOverCol72()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifBoolSetOverCol72") + ".rdz.tcbl");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void TypedefQualifMultipleBoolSet()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifMultipleBoolSet") + ".rdz.tcbl");
	    }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void Non72ColumnOverflowCheck()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Non72ColumnOverflowCheck") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn2()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn2") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn3()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn3") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn4()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn4") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn5()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn5") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn6()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn6") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn7()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn7") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifDependOn8()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifDependOn8") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy1()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy1") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy2()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy2") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy3()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy3") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy4()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy4") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy5()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy5") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy6()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy6") + ".rdz.tcbl");
        }


        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy7()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy7") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy8()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy8") + ".rdz.tcbl");
        }
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy9()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy9") + ".rdz.tcbl");
        }
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypedefQualifIndexedBy10()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy10") + ".rdz.tcbl");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void TypedefQualifIndexedBy11()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy11") + ".rdz.tcbl");
	    }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void TypedefQualifIndexedBy12()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefQualifIndexedBy12") + ".rdz.tcbl");
	    }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void IndexesAndAnonymousDataDef()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "IndexesAndAnonymousDataDef") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void BooleanTester()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "BooleanTester") + ".rdz.tcbl");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void GlobalBoolean()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GlobalBoolean") + ".rdz.tcbl");
	    }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void DeclarativesTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Declaratives") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TypeCobolVersionTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TCOBVersion") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void ProcedureSubscriptTest()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProcedureSubscript") + ".rdz.tcbl", "TestTypeCobolVersion");
	    }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void PointersTest()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Pointers") + ".rdz.tcbl", "TestTypeCobolVersion");
	    }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void ProgramParameterCompUsageTypesTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "ProgramParameterCompUsageTypes") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void CobolLineSplit_IN_VarTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CobolLineSplit_IN_Var") + ".rdz.tcbl", "TestTypeCobolVersion");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CobolLineSplit_IN_Var1") + ".rdz.tcbl", "TestTypeCobolVersion");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CobolLineSplit_IN_Var2") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenQualifiedBoolVarTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenQualifiedBoolVar") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenMoveCorrTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenMoveCorr") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenCorrStatementsTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenCorrStatements") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenContinueInsideTypdefTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenContinueInsideTypdef") + ".rdz.tcbl", "TestTypeCobolVersion");
        }


        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void Gen_73_80_RemoveTextTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Gen_73_80_RemoveText") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenGlobalKeywordTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenGlobalKeyword") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void SetUnsafeTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "SetUnsafe") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GenTCobVersionAfterOptionsTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "GenTCobVersionAfterOptions") + ".rdz.cbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void CopyReplaceInProcLinkage()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "CopyReplaceInProcLinkage") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void MisPlaceCopyInstrWithProcMetaDataTest()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "MisPlaceCopyInstrWithProcMetaData") + ".rdz.tcbl", "TestTypeCobolVersion");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "MisPlaceCopyInstrWithProcMetaData2") + ".rdz.tcbl", "TestTypeCobolVersion");
        }


	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void DeclarativesWithProcedures()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "DeclarativesWithProcedures") + ".tcbl", "TestTypeCobolVersion");
	    }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void DeclarativesWithProcedures2()
	    {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "DeclarativesWithProcedures2") + ".tcbl", "TestTypeCobolVersion");
	    }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void DeclarativesWithInstructionsWithinTest()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "DeclarativesWithInstructionsWithin") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void DeclarativesInsideProcedure()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "DeclarativesInsideProcedure") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void FormalizedCommentsTest()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "FormalizedComments") + ".tcbl", "TestTypeCobolVersion");
	    }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void MultilinesCommentsTest()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "MultilinesComments") + ".tcbl", "TestTypeCobolVersion");
	    }

	    [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void MoveUnsafeToQualifiedInsideFunction()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "MoveUnsafeToQualifiedInsideFunction") + ".rdz.tcbl", "TestTypeCobolVersion");
	    }
	    
        [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void TypedefBodyInsideCopy()
	    {    
	        string dir = System.IO.Directory.GetCurrentDirectory();
	        string copies = Path.Combine(dir, "resources", "input", "TypeCobol", "TypedefCopys");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypedefCopys", "TypedefBodyInsideCopy") + ".rdz.tcbl",
	            null, new List<string>() {copies});
	    }

        /// <summary>
        /// This test is for the issue https://github.com/TypeCobolTeam/TypeCobol/issues/1490
        /// </summary>
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void EvaluateWhenGroupInProc()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "EvaluateWhenGroupInProc") + ".tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GlobalStorageLineMapping()
        {
            CodegenTestUtils.ParseGenerateCompareWithLineMapping(Path.Combine("TypeCobol", "Global_Storage", "GlobalStorage") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
	    [TestCategory("Codegen")]
	    [TestProperty("Time", "fast")]
	    public void GlobalStorage()
	    {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Global_Storage", "GlobalStorage") + ".rdz.tcbl", "TestTypeCobolVersion");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Global_Storage", "GlobalStorageWithUsingTypeDef") + ".rdz.tcbl", "TestTypeCobolVersion");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Global_Storage", "CopyConfigSectionOnly") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GlobalStorage_TypedefWithIndexedArray()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Global_Storage", "GlobalStorageWithTypedefAndIndex") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void GlobalStorageWith_6_73_80()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Global_Storage", "GlobalStorage2") + ".rdz.tcbl", "TestTypeCobolVersion");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void OmittableParameters()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "OmittableParameters") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void PreserveLineBreaksAndIndents()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "PreserveLineBreaksAndIndents") + ".rdz.tcbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void WrongGenCodeIndexLevel88()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "1366_WrongGenCodeIndexLevel88") + ".rdz.tcbl");
        }

#if EUROINFO_RULES
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void RemarksGeneration()
        {
            string cpyCopiesPath = Path.Combine("resources", "input", "TypeCobol", "Remarks", "CpyCopies.lst");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Remarks", "RemarksLess") + ".rdz.cbl", cpyCopyNamesMapFilePath: cpyCopiesPath);
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Remarks", "RemarksPartial") + ".rdz.cbl", cpyCopyNamesMapFilePath: cpyCopiesPath);
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Remarks", "RemarksNonUsed") + ".rdz.cbl", cpyCopyNamesMapFilePath: cpyCopiesPath);
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "Remarks", "NestedProgram") + ".rdz.cbl", cpyCopyNamesMapFilePath: cpyCopiesPath);
        }
#endif
    }
}
