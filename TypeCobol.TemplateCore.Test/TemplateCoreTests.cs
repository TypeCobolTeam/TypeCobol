using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.TemplateCore.SaxParser;
using TypeCobol.TemplateCore.Transpiler;

namespace TypeCobol.TemplateCore.Test
{
    [TestClass]
    public class TemplateCoreTests
    {
        /// <summary>
        /// Test to only validates the XML Skeleton file against the Schema.
        /// </summary>
        [TestMethod]        
        public void SkeletonFileXmlSchemaValidationTest()
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeletons.xml");
            string xsdFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeleton.xsd");

            SkeletonSaxParser parser = new SkeletonSaxParser(xmlFile, xsdFile);
            bool bValidate = parser.Validate();
            Assert.IsTrue(bValidate, bValidate ? "" : parser.ValidationMessage.ToString());
        }

        /// <summary>
        /// Test to both parse and validate on the fly the XML Skeleton file against the Schema.
        /// The Validation will be performed simultaneously during parsing.
        /// </summary>
        [TestMethod]
        public void SkeletonFileXmlSaxParsingWithValidationTest()
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeletons.xml");
            string xsdFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeleton.xsd");

            SkeletonSaxParser parser = new SkeletonSaxParser(xmlFile, xsdFile);
            try
            {
                parser.Parse();
                bool bValidate = parser.ValidationErrorCount == 0 && parser.ValidationWarningCount == 0;
                Assert.IsTrue(bValidate, bValidate ? "" : parser.ValidationMessage.ToString());
            }
            catch(SaxParser.SaxParser.ParsingException e)
            {
                throw e;
            }
        }

        /// <summary>
        /// Test to only parse the XML Skeleton file without validation.
        /// </summary>
        [TestMethod]
        public void SkeletonFileXmlSaxParsingWithoutValidationTest()
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeletons.xml");

            SkeletonSaxParser parser = new SkeletonSaxParser(xmlFile);
            try
            {
                parser.Parse();
            }
            catch (SaxParser.SaxParser.ParsingException e)
            {
                throw e;
            }
        }

        public static string String1 =
@"        @{
        var items = """";
        if (@Model.value.Length == 0) {
        items = @Model.level +""  ""+@Model.name+""-value PIC X VALUE LOW-VALUE."";
        }
        else {
        items = @Model.level + ""  "" + @Model.name + ""-value PIC X VALUE "" +  @Model.value  + ""."";
        }
        }
@items
    88  @Model.name VALUE 'T'.
    88  @Model.name-false VALUE 'F'
                    X'00' thru 'S'
                    'U' thru X'FF'.";

        public static string ResultInterpolationString1 =
@"        
{@items}
    88  {@Model.name} VALUE 'T'.
    88  {@Model.name}-false VALUE 'F'
                    X'00' thru 'S'
                    'U' thru X'FF'.";

        public static string ResultCodeString1 =
@"
        var items = """";
        if (@Model.value.Length == 0) {
        items = @Model.level +""  ""+@Model.name+""-value PIC X VALUE LOW-VALUE."";
        }
        else {
        items = @Model.level + ""  "" + @Model.name + ""-value PIC X VALUE "" +  @Model.value  + ""."";
        }
        ";

        public static string ResultCHarpInterpolationString1 =
@"$@""        
{@items}
    88  {@Model.name} VALUE 'T'.
    88  {@Model.name}-false VALUE 'F'
                    X'00' thru 'S'
                    'U' thru X'FF'.""";

        [TestMethod]
        public void StringInterpolationTest1()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(String1));
            string interpolateString = interpolator.InterpolationString.ToString();
            System.Diagnostics.Debug.Write(interpolateString);
            Assert.AreEqual(ResultInterpolationString1, interpolateString);
        }

        [TestMethod]
        public void CodeStringTest1()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(String1));
            string codeString = interpolator.CodeString.ToString();
            System.Diagnostics.Debug.Write(codeString);
            Assert.AreEqual(ResultCodeString1, codeString);
        }
        [TestMethod]
        public void CSharpStringInterpolationTest1()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(String1));
            string csharpInterpolateString = interpolator.CSharpInterpolationString;
            System.Diagnostics.Debug.Write(csharpInterpolateString);
            Assert.AreEqual(ResultCHarpInterpolationString1, csharpInterpolateString);
        }
    }
}
