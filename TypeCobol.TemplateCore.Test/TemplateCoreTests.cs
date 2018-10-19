using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.TemplateCore.Controller;
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
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "TestSkeletons.xml");
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
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "TestSkeletons.xml");
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
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "TestSkeletons.xml");

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

        public static string ResultMixedCSharpStringInterpolationTest =
@"@SelfResult.Append(@""        "");
        var items = """";
        if (@Model.value.Length == 0) {
        items = @Model.level +""  ""+@Model.name+""-value PIC X VALUE LOW-VALUE."";
        }
        else {
        items = @Model.level + ""  "" + @Model.name + ""-value PIC X VALUE "" +  @Model.value  + ""."";
        }
        @SelfResult.Append(@""
"");@SelfResult.Append($@""{@items}"");@SelfResult.Append(@""
    88  "");@SelfResult.Append($@""{@Model.name}"");@SelfResult.Append(@"" VALUE 'T'.
    88  "");@SelfResult.Append($@""{@Model.name}"");@SelfResult.Append(@""-false VALUE 'F'
                    X'00' thru 'S'
                    'U' thru X'FF'."");";

        public static string StringFirstTransitionInMarkup =
        @"        01  TC-@Model.programName8-FctList-Loaded PIC X(02).
        88 TC-@Model.programName8-FctList-IsLoaded      VALUE 'OK'.";

        public static string StringFirstTransitionInMarkupResult =
        @"        01  TC-{@Model.programName8}-FctList-Loaded PIC X(02).
        88 TC-{@Model.programName8}-FctList-IsLoaded      VALUE 'OK'.";

        public static string StrangeCaseString =
        @"*
*    IF CallIsCopy
*      PERFORM Copy-Process-Mode
*    ELSE
    PERFORM FctList-Process-Mode
    perform INIT-LIBRARY
*    END-IF

    GOBACK.
        @{
        var entries = """";
        int c = 0;
        foreach (var f in %definitions.functions.Public) {
        entries += ""       SET TC-""+%programName8 + ""-"" +f.Hash+""   TO ENTRY \'""+f.Hash+""\'\n"";
        }
        }
 FctList-Process-Mode.
     IF NOT TC-%programName8-FctList-IsLoaded
@entries
       SET TC-%programName8-FctList-IsLoaded TO TRUE
     END-IF
        .
        @{
        var items = """";
        if (%definitions.functions.Public.Count > 0) {
        items += ""     set PntTab-Pnt TO ADDRESS OF TC-""+%programName8+""-PntTab\n"";
        }
        }
@items";

        public static string ResultStrangeCaseString =
@"@SelfResult.Append(@""*
*    IF CallIsCopy
*      PERFORM Copy-Process-Mode
*    ELSE
    PERFORM FctList-Process-Mode
    perform INIT-LIBRARY
*    END-IF

    GOBACK.
"");        
        var entries = """";
        int c = 0;
        foreach (var f in %definitions.functions.Public) {
        entries += ""       SET TC-""+%programName8 + ""-"" +f.Hash+""   TO ENTRY \'""+f.Hash+""\'\n"";
        }
        @SelfResult.Append(@""
 FctList-Process-Mode.
     IF NOT TC-%programName8-FctList-IsLoaded
"");@SelfResult.Append($@""{@entries}"");@SelfResult.Append(@""
       SET TC-%programName8-FctList-IsLoaded TO TRUE
     END-IF
        .
"");        
        var items = """";
        if (%definitions.functions.Public.Count > 0) {
        items += ""     set PntTab-Pnt TO ADDRESS OF TC-""+%programName8+""-PntTab\n"";
        }
        @SelfResult.Append(@""
"");@SelfResult.Append($@""{@items}"");@SelfResult.Append(@"""");";


        /// <summary>
        /// Test the result as the content of an interpolate string.
        /// </summary>
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

        /// <summary>
        /// Test the C# code part of the script.
        /// </summary>
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

        /// <summary>
        /// Test the result as an interpolation String.
        /// </summary>
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

        /// <summary>
        /// Test the transpilation of the script into an unique buffer, test the adding of
        /// Markup text into a target@SelResult variable which will be a StringBuilder instance.
        /// </summary>
        [TestMethod]
        public void MixedCSharpStringInterpolationTest()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(String1));
            string mixedcsharpInterpolateString = interpolator.MixedCodeInterpolationString.ToString();
            System.Diagnostics.Debug.Write(mixedcsharpInterpolateString);
            Assert.AreEqual(ResultMixedCSharpStringInterpolationTest, mixedcsharpInterpolateString);
        }

        /// <summary>
        /// This a First test that only dump the code generated for a Node actions.
        /// </summary>
        [TestMethod]
        public void NodeTranspilationCodeTest0()
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "TestSkeletons.xml");
            string xsdFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeleton.xsd");

            SkeletonSaxParser parser = new SkeletonSaxParser(xmlFile, xsdFile);
            try
            {
                parser.Parse();
                bool bValidate = parser.ValidationErrorCount == 0 && parser.ValidationWarningCount == 0;
                Assert.IsTrue(bValidate, bValidate ? "" : parser.ValidationMessage.ToString());
                SkeletonsController controller = new SkeletonsController(parser.Skeletons);
                controller.CreateNodesModel();
                var node = controller.Nodes["TypeCobol.Compiler.Nodes.DataDescription"];
                string code = node.TranspiledCode;
                System.Diagnostics.Debug.Write(code);
            }
            catch (SaxParser.SaxParser.ParsingException e)
            {
                throw e;
            }
        }

        /// <summary>
        /// This a First test that only dump the code generated for a skeletons file.
        /// </summary>
        [TestMethod]
        public void SkeletonsFileTranspilationTest0()
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();
            string xmlFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "TestSkeletons.xml");
            string xsdFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeleton.xsd");

            SkeletonSaxParser parser = new SkeletonSaxParser(xmlFile, xsdFile);
            try
            {
                parser.Parse();
                bool bValidate = parser.ValidationErrorCount == 0 && parser.ValidationWarningCount == 0;
                Assert.IsTrue(bValidate, bValidate ? "" : parser.ValidationMessage.ToString());
                SkeletonsController controller = new SkeletonsController(parser.Skeletons);
                string code = controller.TranspiledCode;
                controller.CreateNodesModel();                                
                System.Diagnostics.Debug.Write(code);
            }
            catch (SaxParser.SaxParser.ParsingException e)
            {
                throw e;
            }
        }

        /// <summary>
        /// Test with a first transition character @ in a markup.
        /// </summary>
        [TestMethod]
        public void FirstTransitionInMarkupTest()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(StringFirstTransitionInMarkup));
            string interpolateString = interpolator.InterpolationString.ToString();
            System.Diagnostics.Debug.Write(interpolateString);
            Assert.AreEqual(StringFirstTransitionInMarkupResult, interpolateString);
        }
        /// <summary>
        /// Testing astrange case when before an @{ the espace charcater before are seen as SpanKind.Code
        /// inside Markup text rather inside C# Code text.
        /// </summary>
        [TestMethod]
        public void StrangeCaseTest()
        {
            CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
            RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
            Assert.IsTrue(transpiler.Parse(StrangeCaseString));
            string mixedResult = interpolator.MixedCodeInterpolationString.ToString();
            System.Diagnostics.Debug.Write(mixedResult);
            Assert.AreEqual(ResultStrangeCaseString, mixedResult);
        }

    }
}
