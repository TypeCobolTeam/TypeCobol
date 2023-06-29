using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Text;
using System.Xml.Serialization;
using System.Runtime.Serialization.Json;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.Nodes
{

    /// <summary>
    /// Is implemented on Nodes that are documentable
    /// </summary>
    public interface IDocumentable
    {
    }

    /// <summary>
    /// Documentation is common for Types, Programs and Functions.
    /// It contains all the properties they have in common.
    /// </summary>
    /// <param name="FormCom">The Formalized Comment Code Element</param>
    /// <param name="Name">The Name of the current Node</param>
    /// <param name="Description">Description field value inside the Formalized Comment</param>
    /// <param name="Visibility">AccessModifier of the current Node</param>
    /// <param name="Namespace">Namespace of the current Node</param>
    /// <param name="IsTypeDef">Is true if the current Node is a Type Definition</param>
    /// <param name="IsFunction">Is true if the current Node is a Function</param>
    /// <param name="IsProgram">Is true if the current Node is a Program</param>
    /// <param name="Needs">Needs list corresponding to the Needs field inside the Formalized Comment</param>
    /// <param name="ToDo">ToDo list corresponding to the ToDo field inside the Formalized Comment</param>
    /// <param name="IsDeprecated">Is true if the Deprecated field is present, with or without further information</param>
    /// <param name="Deprecated">Deprecated field value inside the Formalized Comment</param>
    /// <param name="ReplacedBy">ReplacedBy field value inside the Formalized Comment</param>
    /// <param name="Restriction">Restriction field value inside the Formalized Comment</param>
    /// <param name="See">See field value inside the Formalized Comment</param>
    [Serializable]
    [XmlInclude(typeof(DocumentationForType))]
    [XmlInclude(typeof(DocumentationForFunction))]
    [XmlInclude(typeof(DocumentationForProgram))]
    [DataContract]
    [KnownType(typeof(DocumentationForType))]
    [KnownType(typeof(DocumentationForFunction))]
    [KnownType(typeof(DocumentationForProgram))]
    public abstract class Documentation
    {
        [XmlIgnore]
        [IgnoreDataMember]
        public FormalizedCommentDocumentation FormCom { get; set; }

        // Data from Formalized comment 
        [XmlArray("Needs")]
        [XmlArrayItem("Need", typeof(string))]
        [DataMember(EmitDefaultValue = false)]
        public List<string> Needs { get; set; }

        [XmlArray("ToDos")]
        [XmlArrayItem("ToDo", typeof(string))]
        [DataMember(Name = "ToDos", EmitDefaultValue = false)]
        public List<string> ToDo { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string Description { get; set; }

        [IgnoreDataMember]
        public bool IsDeprecated => Deprecated != null;

        [DataMember(EmitDefaultValue = false)]
        public string Deprecated { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string ReplacedBy { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string Restriction { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string See { get; set; }


        // Data from Node
        [DataMember]
        public string Name { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public AccessModifier Visibility { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string Namespace { get; set; }

        [IgnoreDataMember]
        public virtual bool IsTypeDef => false;

        [IgnoreDataMember]
        public virtual bool IsFunction => false;

        [IgnoreDataMember]
        public virtual bool IsProgram => false;

        /// <summary>
        /// Constructor that initialize the properties with a generic Node
        /// </summary>
        /// <param name="node">generic Node, have to be TypeDefinition, FunctionDeclaration, or Program</param>
        protected Documentation(Node node)
        {
            if (node != null)
            {
                Name = node.Name;
                Namespace = node.Root.MainProgram.Namespace;

                // Get the information of the Code Element
                var ce = node.CodeElement as IFormalizedCommentable;
                if (ce?.FormalizedCommentDocumentation != null)
                {
                    FormCom     = ce.FormalizedCommentDocumentation;
                    // Avoid set empty string or list to have less data to transfer
                    Needs       = FormCom.Needs.Any() ? FormCom.Needs : null;
                    ToDo        = FormCom.ToDo.Any()  ? FormCom.ToDo  : null;
                    Description = string.IsNullOrEmpty(FormCom.Description) ? null : FormCom.Description;
                    ReplacedBy  = string.IsNullOrEmpty(FormCom.ReplacedBy)  ? null : FormCom.ReplacedBy;
                    Restriction = string.IsNullOrEmpty(FormCom.Restriction) ? null : FormCom.Restriction;
                    See         = string.IsNullOrEmpty(FormCom.See)         ? null : FormCom.See;
                    Deprecated  = FormCom.Deprecated;
                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        protected Documentation() { }

        public StringBuilder SerializeToXml()
        {
            StringBuilder sb = new StringBuilder();
            Type documentationType = IsTypeDef ? typeof(DocumentationForType) :
                IsFunction ? typeof(DocumentationForFunction) :
                IsProgram ? typeof(DocumentationForProgram) :
                null;
            if (documentationType != null)
            {
                XmlSerializer serializer = new XmlSerializer(documentationType);
                using (StringWriter textWriter = new StringWriter())
                {
                    serializer.Serialize(textWriter, this);
                    sb.Append(textWriter);
                }
            }
            return sb;
        }

        /// <summary>
        /// Serialize the object (this) into Json
        /// </summary>
        /// <param name="isDebug">If true then format the Json to be "Human readable"</param>
        public StringBuilder SerializeToJson(bool isDebug = false)
        {
            StringBuilder sb = new StringBuilder();
            MemoryStream memStream = new MemoryStream();
            var settings = new DataContractJsonSerializerSettings()
            { EmitTypeInformation = EmitTypeInformation.AsNeeded };

            using (var writer = JsonReaderWriterFactory.CreateJsonWriter(
                memStream, Encoding.UTF8, false, isDebug, "  "))
            {
                DataContractJsonSerializer serializer = new DataContractJsonSerializer(typeof(Documentation), settings);
                serializer.WriteObject(writer, this);
                writer.Flush();
            }

            memStream.Position = 0;
            StreamReader sr = new StreamReader(memStream);
            sb.Append(sr.ReadToEnd());
            return sb;
        }
        
        public static Documentation CreateAppropriateDocumentation(IDocumentable node)
        {
            if (node is Program)
            {
                return new DocumentationForProgram(node as Program);
            }
            else if (node is FunctionDeclaration)
            {
                return new DocumentationForFunction(node as FunctionDeclaration);
            }
            else if (node is TypeDefinition)
            {
                return new DocumentationForType(node as TypeDefinition);
            }
            else
                throw new Exception("Documentable Nodes are: Program, FunctionDeclaration, TypeDefinition");
        }
    }


    /// <summary>
    /// DocumentationForType contains the documentation information relative to a Type Definition
    /// </summary>
    /// <param name="Childrens">List of the Type children in case of group otherwise Childrens is null</param>
    /// <param name="IsBlankWheneZero">Is set to true if the Type have the option "BLANK WHEN ZERO"</param>
    /// <param name="Justified">Is set to true if the Type have the option "JUSTIFIED RIGHT"</param>
    /// <param name="DocDataType">Contains the information relative to the type data type(Usage, Occurs, Value, PIC ...)</param>
    [Serializable]
    [DataContract]
    public class DocumentationForType : Documentation
    {
        [XmlArray("Childrens")]
        [XmlArrayItem("Children", typeof(DocumentationTypeChildren))]
        [DataMember(EmitDefaultValue = false)]
        public List<DocumentationTypeChildren> Childrens { get; set; }

        [IgnoreDataMember]
        public override bool IsTypeDef => true;

        [DataMember(EmitDefaultValue = false)]
        public bool IsBlankWhenZero { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public bool Justified { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public DocumentationDataType DocDataType { get; set; }

        /// <summary>
        /// Main Constructor for DocumentationForType
        /// </summary>
        /// <param name="typeDefinition">The Node to serialize</param>
        public DocumentationForType(TypeDefinition typeDefinition) : base(typeDefinition)
        {
            // Get the information of the Code Element
            DataTypeDescriptionEntry ce = typeDefinition.CodeElement as DataTypeDescriptionEntry;
            if (ce != null)
            {
                IsBlankWhenZero = ce.IsBlankWhenZero?.Value ?? false;
                Justified = ce.IsJustified?.Value ?? false;
                Visibility = ce.Visibility;
            }

            // Build the TypeDef DataType in case of the type itself have data information (Usage, Occurs, Value, PIC ...)
            DocDataType = new DocumentationDataType(typeDefinition);

            // Build the Type definition children Tree 
            if (typeDefinition.Children.Any())
            {
                Childrens = new List<DocumentationTypeChildren>();
                foreach (var node in typeDefinition.Children)
                {
                    var child = node as DataDefinition;
                    if (child != null)
                    {
                        Childrens.Add(new DocumentationTypeChildren(child));
                    }
                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        private DocumentationForType() { }
    }


    /// <summary>
    /// DocumentationTypeChildren contains information of a Type Definition children if this Type Definition is a group
    /// </summary>
    /// <param name="Childrens">List of this data children in case of group otherwise Childrens will be null</param>
    /// <param name="Name">List of the Type children in case of group otherwise Childrens is null</param>
    /// <param name="IsBlankWheneZero">Is set to true if current data have the option "BLANK WHEN ZERO"</param>
    /// <param name="Justified">Is set to true if current data have the option "JUSTIFIED RIGHT"</param>
    /// <param name="IsLevel77">Is set to true if current data is defined in level 77</param>
    /// <param name="IsLevel88">Is set to true if current data is defined in level 88</param>
    /// <param name="ConditionValues">(only for level 88 data) contains an array of the equality conditions defined</param>
    /// <param name="ConditionValuesRanges">(only for level 88 data) contains an array of the interval conditions defined</param>
    /// <param name="DocDataType">Contains the information relative to the type data type(Usage, Occurs, Value, PIC ...)</param>
    [Serializable]
    [DataContract]
    public class DocumentationTypeChildren
    {
        // simple struct to store the value of a DataCondition if this data is a range ("88 dataName VALUE 'MinValue' THRU 'MaxValue')
        public struct ConditionValuesRange
        {
            public string MinValue;
            public string MaxValue;
            public ConditionValuesRange(string minValue, string maxValue)
            {
                MinValue = minValue;
                MaxValue = maxValue;
            }
        }

        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsSubGroup => Childrens != null && Childrens.Any();

        [XmlArray("Childrens")]
        [XmlArrayItem("Children", typeof(DocumentationTypeChildren))]
        [DataMember(Name = "Childrens", EmitDefaultValue = false)]
        public List<DocumentationTypeChildren> Childrens { get; set; }

        [XmlArray("ConditionValues")]
        [DataMember(Name = "ConditionValues", EmitDefaultValue = false)]
        public string[] ConditionValues { get; set; }

        [XmlArray("ConditionValuesRanges")]
        [XmlArrayItem("ConditionValuesRange", typeof(ConditionValuesRange))]
        [DataMember(Name = "ConditionValuesRanges", EmitDefaultValue = false)]
        public ConditionValuesRange[] ConditionValuesRanges { get; set; }

        [DataMember]
        public string Name { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public bool IsBlankWhenZero { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public bool Justified { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public bool IsLevel77 { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public bool IsLevel88 { get; set; }

        [DataMember]
        public DocumentationDataType DocDataType { get; set; }


        /// <summary>
        /// Main Constructor for DocumentationTypeChildren
        /// </summary>
        /// <param name="dataDef"></param>
        public DocumentationTypeChildren(DataDefinition dataDef)
        {
            // Get the information of the Code Element
            DataDefinitionEntry ce = dataDef.CodeElement as DataDefinitionEntry;
            if (ce != null)
            {
                IsLevel77 = ce.LevelNumber?.Value == 77;
                IsLevel88 = ce.Type == CodeElementType.DataConditionEntry;
                if (IsLevel88)
                {
                    DataConditionEntry ceCondition = dataDef.CodeElement as DataConditionEntry;
                    if (ceCondition != null)
                    {
                        ConditionValues = ceCondition.ConditionValues?
                            .Select(x => x.ToString()).ToArray();
                        ConditionValuesRanges = ceCondition.ConditionValuesRanges?
                            .Select(x => new ConditionValuesRange(x.MinValue.ToString(), x.MaxValue.ToString())).ToArray();
                    }
                }
                else
                {
                    CommonDataDescriptionAndDataRedefines ceDescAndRedif =
                        dataDef.CodeElement as CommonDataDescriptionAndDataRedefines;
                    if (ceDescAndRedif != null)
                    {
                        IsBlankWhenZero = ceDescAndRedif.IsBlankWhenZero?.Value ?? false;
                        Justified = ceDescAndRedif.IsJustified?.Value ?? false;
                    }
                }
            }
            Name = dataDef.Name;
            DocDataType = new DocumentationDataType(dataDef);

            // continue the Type definition children Tree building if this Data have anu childrens
            if (dataDef.Children.Any())
            {
                Childrens = new List<DocumentationTypeChildren>();
                foreach (var node in dataDef.Children)
                {
                    var subChild = node as DataDefinition;
                    if (subChild != null)
                    {
                        Childrens.Add(new DocumentationTypeChildren(subChild));
                    }
                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        public DocumentationTypeChildren() { }

    }


    /// <summary>
    /// DocumentationForFunction contains the documentation information relative to a Function Declaration
    /// </summary>
    /// <param name="Parameters">Parameters list corresponding to the Parameters field inside the Formalized Comment and completed with the function signature</param>
    [Serializable]
    [DataContract]
    public class DocumentationForFunction : Documentation
    {
        [XmlArray("Parameters")]
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        [DataMember(Name = "Parameters", EmitDefaultValue = false)]
        public List<DocumentationParameter> Parameters { get; set; }

        [IgnoreDataMember]
        public override bool IsFunction => true;

        /// <summary>
        /// Main Constructor for DocumentationForFunction
        /// </summary>
        /// <param name="functionDeclaration">The Node to serialize</param>
        public DocumentationForFunction(FunctionDeclaration functionDeclaration) : base(functionDeclaration)
        {
            FunctionDeclarationHeader ce = functionDeclaration.CodeElement;

            Visibility = ce.Visibility;

            Parameters = new List<DocumentationParameter>();

            foreach (ParameterDescription param in functionDeclaration.Profile.Parameters)
            {
                string info = null;
                if (ce?.FormalizedCommentDocumentation?.Parameters.ContainsKey(param.Name) ?? false)
                    info = ce.FormalizedCommentDocumentation?.Parameters[param.Name];

                Parameters.Add(new DocumentationParameter(param, info));
            }
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        private DocumentationForFunction() { }

    }


    /// <summary>
    /// DocumentationForProgram contains the documentation information relative to a Program Declaration (Source, Stacked or Nested)
    /// </summary>
    /// <param name="Parameters">Parameters list corresponding to the Parameters field inside the Formalized Comment and completed with the program signature</param>
    [Serializable]
    [DataContract]
    public class DocumentationForProgram : Documentation
    {
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        [XmlArray("Parameters")]
        [DataMember(Name = "Parameters", EmitDefaultValue = false)]
        public List<DocumentationParameter> Parameters { get; set; }

        [IgnoreDataMember]
        public override bool IsProgram => true;

        /// <summary>
        /// Main Constructor for DocumentationForFunction
        /// </summary>
        /// <param name="program">The Node to serialize</param>
        public DocumentationForProgram(Program program) : base(program.Children.FirstOrDefault(x => x is ProcedureDivision))
        {
            Name = program.Name;
            ProgramIdentification ce = program.CodeElement as ProgramIdentification;
            ProcedureDivisionHeader procedureDivision = program.Children.FirstOrDefault(x => x is ProcedureDivision)
                ?.CodeElement as ProcedureDivisionHeader;

            var formCom = procedureDivision?.FormalizedCommentDocumentation;

            if (procedureDivision != null)
            {
                if (procedureDivision.UsingParameters != null)
                {
                    Parameters = new List<DocumentationParameter>();
                    foreach (CallTargetParameter param in procedureDivision.UsingParameters)
                    {
                        DataDefinition dataDef = program.SymbolTable.GetVariables(param.StorageArea.SymbolReference)
                            .FirstOrDefault();

                        if (dataDef != null)
                        {
                            // match the formalized comment parameter description with the right parameter
                            string info = null;
                            if (param.StorageArea.SymbolReference != null &&
                                (formCom?.Parameters.ContainsKey(dataDef.Name) ?? false))
                            {
                                info = formCom.Parameters[dataDef.Name];
                            }
                            Parameters.Add(new DocumentationParameter(dataDef, info));
                        }
                    }

                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        private DocumentationForProgram() { }
    }


    /// <summary>
    /// Parameter inside a Function or a Program
    /// </summary>
    /// <param name="Name">The Name of the parameter data</param>
    /// <param name="PassingType">The passing type (INPUT/OUTPUT/INOUT/Unknown)</param>
    /// <param name="Info">The Description of the parameter given inside the Formalized Comment</param>
    /// <param name="DocDataType">   </param>
    [Serializable]
    [DataContract]
    public class DocumentationParameter
    {
        public enum PassingTypes
        {
            Input,
            Output,
            InOut,
            Unknown
        }

        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsInput => PassingType == PassingTypes.Input;

        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsOutput => PassingType == PassingTypes.Output;

        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsInOut => PassingType == PassingTypes.InOut;

        [DataMember]
        public string Name { get; set; }

        [DataMember]
        public PassingTypes PassingType { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string Info { get; set; }

        [DataMember]
        public DocumentationDataType DocDataType { get; set; }

        /// <summary>
        /// Constructor for function parameters
        /// </summary>
        /// <param name="parameter">The parameter Node</param>
        /// <param name="info">The parameter description inside the formalized comment if any</param>
        public DocumentationParameter(ParameterDescription parameter, string info)
        {

            Name = parameter.Name;
            Info = info;
            DocDataType = new DocumentationDataType(parameter);

            int passTypeInt = (int)parameter.PassingType;
            PassingType = passTypeInt < 3 ? (PassingTypes)passTypeInt : PassingTypes.Unknown;
        }

        /// <summary>
        /// Constructor for program parameters
        /// </summary>
        /// <param name="dataDef">The parameter Node</param>
        /// <param name="info">The parameter description inside the formalized comment if any</param>
        /// <param name="determinePassing">set to false to not search the PassingType inside the parameter description</param>
        public DocumentationParameter(DataDefinition dataDef, string info, bool determinePassing = true)
        {

            Name = dataDef.Name;
            Info = info;
            DocDataType = new DocumentationDataType(dataDef);

            if (determinePassing && !string.IsNullOrEmpty(info))
            {
                var processedData = DetermineType(info);
                PassingType = processedData.Item1;
                Info = processedData.Item2;
            }
            else
                PassingType = PassingTypes.Unknown;
        }
        

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        private DocumentationParameter() { }

        /// <summary>
        /// Determine the Passing type with the first word of the parameter description 
        /// </summary>
        /// <param name="info">The parameter description </param>
        /// <returns>A Tuple containing the Passing type and the parameter description without the passing type if any</returns>
        private Tuple<PassingTypes, string> DetermineType(string info)
        {
            string lowerInfo = info.Trim().ToLower();
            PassingTypes passingType;
            string firstWord = new string(lowerInfo.TakeWhile(c => char.IsLetter(c)).ToArray());

            switch (firstWord)
            {
                case "input":
                    passingType = PassingTypes.Input;
                    break;
                case "output":
                    passingType = PassingTypes.Output;
                    break;
                case "inout":
                    passingType = PassingTypes.InOut;
                    break;
                default:
                    passingType = PassingTypes.Unknown;
                    break;
            }

            if (passingType != PassingTypes.Unknown)
            {
                int index;
                for (index = 6; !char.IsLetter(info, index); index++){}
                info = info.Substring(index, info.Length - index).Trim();
            }
            return new Tuple<PassingTypes, string>(passingType, info);
        }
    }
    

    /// <summary>
    /// The DataType useful for the documentation generation
    /// </summary>
    /// <param name="TypeName">If the Data is a Type implementation, this is the Type Name otherwise this is null</param>
    /// <param name="Picture">If the Data is a Picture it contain the Picture Type (ex. S9(5)V(2)) </param>
    /// <param name="Usage">Contain the Data Usage if any </param>
    /// <param name="MaxOccurence">Contain the Data Usage if any </param>
    /// <param name="DefaultValue">Contain the Data Value if any (Does not work for now for other Types than BOOL but may serve for future implementation)</param>
    [Serializable]
    [DataContract]
    public class DocumentationDataType
    {
        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsArray => MaxOccurence > 1;

        [XmlIgnore]
        [IgnoreDataMember]
        public bool IsType => TypeName != null;

        [DataMember(EmitDefaultValue = false)]
        public DataUsage? Usage { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public long MaxOccurence { get; set; }
        
        [DataMember(EmitDefaultValue = false)]
        public string DefaultValue { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string TypeName { get; set; }

        [DataMember(EmitDefaultValue = false)]
        public string Picture { get; set; }

        /// <summary>
        /// Main constructor used for Type Definition, Program Definition and Function Definition
        /// </summary>
        /// <param name="dataDef"> DataDefinition Node used to get DataType information</param>
        public DocumentationDataType(DataDefinition dataDef)
        {
            Usage = dataDef.Usage;
            MaxOccurence = dataDef.MaxOccurencesCount;
            TypeName = dataDef.TypeDefinition?.Name;
            Picture = dataDef.Picture?.Value;
            var ce = dataDef.CodeElement as DataDescriptionEntry;
            if (ce?.InitialValue != null)
                DefaultValue = ce.InitialValue.ToString();
        }

        /// <summary>
        /// Constructor used as Default constructor which is needed for serialization
        /// </summary>
        public DocumentationDataType(DataUsage? usage = null, long maxOccurence = 1,
            string defaultValue = null, string typeName = null, string picture = null)
        {
            Usage        = usage;
            MaxOccurence = maxOccurence;
            DefaultValue = defaultValue;
            TypeName     = typeName;
            Picture      = picture;
        }

        /// <summary>
        /// Default constructor needed for serialization. Do not use it.
        /// </summary>
        public DocumentationDataType()
        {
            
        }
    }

}
