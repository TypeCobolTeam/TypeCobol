using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Json;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// Generator than create the documentation of all applicable Nodes
    /// </summary>
    class DocumentationGenerator : IGenerator
    {
        private StringBuilder Destination { get; set; }
        public DocumentationGenerator(StringBuilder destination, string typeCobolVersion)
        {
            this.Destination = destination;
            TypeCobolVersion = typeCobolVersion;
        }
        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            Destination.Append("");
            //Add version to output file

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            var docBuilder = new DocumentationBuilder();
            sourceFile.AcceptASTVisitor(docBuilder);

            Destination.Append(SerializeToJson(docBuilder.DTOs));
            // this.Destination contains now the Json Documentation
        }

        public List<Diagnostic> Diagnostics { get; set; }
        public string TypeCobolVersion { get; set; }

        /// <summary>
        /// Serialize the object (this) into Json
        /// </summary>
        /// <param name="documentations"></param>
        /// <param name="isDebug">If true then format the Json to be "Human readable"</param>
        public string SerializeToJson(List<Documentation> documentations, bool isDebug = false)
        {
            StringBuilder sb = new StringBuilder();
            MemoryStream memStream = new MemoryStream();
            var settings = new DataContractJsonSerializerSettings()
                { EmitTypeInformation = EmitTypeInformation.AsNeeded };

            using (var writer = JsonReaderWriterFactory.CreateJsonWriter(
                memStream, Encoding.UTF8, false, isDebug, "  "))
            {
                DataContractJsonSerializer serializer = new DataContractJsonSerializer(typeof(DataToSerialize), settings);
                serializer.WriteObject(writer, new DataToSerialize(TypeCobolVersion, documentations));
                writer.Flush();
            }

            memStream.Position = 0;
            StreamReader sr = new StreamReader(memStream);
            sb.Append(sr.ReadToEnd());
            return sb.ToString();
        }
    }

    [Serializable]
    [XmlInclude(typeof(DocumentationForType))]
    [XmlInclude(typeof(DocumentationForFunction))]
    [XmlInclude(typeof(DocumentationForProgram))]
    [DataContract]
    [KnownType(typeof(DocumentationForType))]
    [KnownType(typeof(DocumentationForFunction))]
    [KnownType(typeof(DocumentationForProgram))]
    public class DataToSerialize
    {
        [XmlArray("TypeCobolVersion")]
        [XmlArrayItem("TypeCobolVersion", typeof(string))]
        [DataMember(EmitDefaultValue = false)]
        public string TypeCobolVersion;

        [XmlArray("Documentations")]
        [XmlArrayItem("Documentations", typeof(List<Documentation>))]
        [DataMember(EmitDefaultValue = false)]
        public List<Documentation> Documentations;

        public DataToSerialize(string typeCobolVersion, List<Documentation> documentations)
        {
            TypeCobolVersion = typeCobolVersion;
            Documentations = documentations;
        }
    }

    class DocumentationBuilder : AbstractAstVisitor
    {
        public List<Documentation> DTOs;

        public DocumentationBuilder()
        {
            DTOs = new List<Documentation>();
        }
        public override bool Visit(TypeDefinition node)
        {
            DataTypeDescriptionEntry ce = node.CodeElement as DataTypeDescriptionEntry;
            if (ce != null && ce.Visibility == AccessModifier.Public)
                DTOs.Add(new DocumentationForType(node));
            return true;
        }
        public override bool Visit(FunctionDeclaration node)
        {
            FunctionDeclarationHeader ce = node.CodeElement as FunctionDeclarationHeader;
            if (ce != null && ce.Visibility == AccessModifier.Public)
                DTOs.Add(new DocumentationForFunction(node));
            return true;
        }
        public override bool Visit(Program node)
        {
            DTOs.Add(new DocumentationForProgram(node));
            return true;
        }

    }
}
