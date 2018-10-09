using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.TemplateCore.SaxParser;

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
    }
}
