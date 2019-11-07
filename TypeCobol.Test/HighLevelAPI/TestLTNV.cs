﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Tools.APIHelpers;

namespace TypeCobol.Test.HighLevelAPI {

    [TestClass]


    public class TestLTNV {

        private static readonly string Root = PlatformUtils.GetPathForProjectFile("HighLevelAPI");

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void TestGetLTNVCopy() {
#if !EUROINFO_RULES
            return;
#endif

            var errors = new List<Exception>();

            var rootPath = Root + Path.DirectorySeparatorChar + "LTNV";


            ParseAndTestGetLTNVCopys(rootPath, "FO200001.rdz.cbl", true, errors, new List<string> {"FO200001"},
                new Dictionary<string, string>()
                {
                    {"YFO2FAW", "YFO2FAL"},
                    {"YFO2FRW", "YFO2FRE"},
                    {"YFO2E1L", "FO2E01"},
                    {"YFO2S1L", "FO2S01"}
                });

            ParseAndTestGetLTNVCopys(rootPath, "FOOABCDE.rdz.cbl", true, errors, new List<string> { "FOOABCDE" },
                new Dictionary<string, string>()
                {
                    { "YFOOFAW", "FOOFAW" },
                    { "YFOOFRW", "FOOFRW" },
                    { "YFOOHEW", "FOOHEW" },
                    { "YFOOT00", "FOOT00" },
                    { "YFOOT01", "FOOT01" },
                    { "YFOOT10", "FOOT10" },
                    { "YFOOT11", "FOOT11" },
                    { "YFOOT14", "FOOT12" },
                    { "YFOOT15", "FOOT13" }
                });

            if (errors.Count > 0)
            {
                var str = new System.Text.StringBuilder();
                foreach (var ex in errors) str.Append(ex.Message + "\n" + ex.StackTrace);
                throw new Exception(str.ToString());
            }
        }


        private static void ParseAndTestGetLTNVCopys(string rootPath, string path, bool autoRemarks, List < Exception> errors, IList<string> programsName ,params IDictionary<string, string>[] expected )
        {
            Assert.IsTrue(programsName.Count == expected.Length);//check if parameter of this method are coherent
            try
            {
                var result = ParseAndGetLTNVCopys(rootPath, path, autoRemarks);
                Assert.IsTrue(result.Count == expected.Length);

                var actualPgmNames = result.Keys.ToList();
                var actualLTNVCopys = result.Values.ToList();


                for (int i = 0; i < expected.Length; i++) {
                    var expectedProgramName = programsName[i];

                    Assert.IsTrue(actualPgmNames[i].Name == expectedProgramName, "error for " + expectedProgramName + " in path "+path+ " != " + actualPgmNames[i].Name + "_");


                    //Check if expected and actualLTNVCopys are the same
                    Assert.IsTrue(actualLTNVCopys[i].OrderBy(k => k.Key).SequenceEqual(expected[i].OrderBy(k => k.Key)), "error for " + expectedProgramName + " in path " + path);
                }
            }
            catch (Exception e)
            {
                errors.Add(e);
            }
        }

        private static IDictionary<Program, IDictionary<string, string>> ParseAndGetLTNVCopys(string rootPath, string path, bool autoRemarks = false)
        {
            var parser = TypeCobol.Parser.Parse(rootPath + Path.DirectorySeparatorChar + path, DocumentFormat.RDZReferenceFormat, autoRemarks);
            var diagnostics = parser.Results.AllDiagnostics();
            // There should be no diagnostics errors
            // warning diagnostics are not considered : for example, test with warning with COPY SUPPRESS is always running
            Assert.IsFalse(diagnostics.Any(d => d.Info.Severity != TypeCobol.Compiler.Diagnostics.Severity.Warning));

            return LTNVHelper.GetLTNVCopy(parser.Results.ProgramClassDocumentSnapshot.Root);
        }
    }
}
