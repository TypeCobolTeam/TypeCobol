using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using Castle.Core.Internal;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.Nodes
{

    /// <summary>
    /// Is implemented on Nodes that are documentable
    /// </summary>
    public interface IDocumented
    {
        string XMLDocumentation { get; }

        bool IsDocumented { get; }
        Documentation Documentation { get; set; }
    }

    /// <summary>
    /// Documentation is common for Types, Programs and Functions.
    /// It contains all the properties they have in common.
    /// </summary>
    /// <param name="FormCom">The Formalized Comment Code Element</param>
    /// <param name="Name">The Name of the current Node</param>
    /// <param name="Description">Description field value inside the Formalized Comment</param>
    /// <param name="Visibility">AccesModifier of the current Node</param>
    /// <param name="Namespace">Namespace of the current Node</param>
    /// <param name="IsTypeDef">Is true if the current Node is a Type Definition</param>
    /// <param name="IsFunction">Is true if the current Node is a Functionn</param>
    /// <param name="IsProgram">Is true if the current Node is a Program</param>
    [Serializable]
    [XmlInclude(typeof(DocumentationForType))]
    [XmlInclude(typeof(DocumentationForFunction))]
    [XmlInclude(typeof(DocumentationForProgram))]
    public abstract class Documentation
    {
        [XmlIgnore]
        public FormalizedCommentDocumentation FormCom { get; set; }

        public string Name { get; set; }
        public string Description { get; set; }
        public AccessModifier? Visibility { get; set; }
        public string Namespace { get; set; }
        public virtual bool IsTypeDef => false;
        public virtual bool IsFunction => false;
        public virtual bool IsProgram => false;

        /// <summary>
        /// Constructor that initialize the propertys with a generic Node
        /// </summary>
        /// <param name="node">generic Node, have to be TypeDefinition, FunctionDeclaration, or Program</param>
        protected Documentation(Node node)
        {
            Name = node.Name;

            if (node is TypeDefinition || node is FunctionDeclaration || node is Program)
            {
                FormalizedCommentDocumentation doc = null;

                // initialisation as TypeDefinition
                if (node is TypeDefinition)
                {
                    DataTypeDescriptionEntry ce = ((TypeDefinition)node).CodeElement as DataTypeDescriptionEntry;
                    if (ce != null)
                    {
                        doc = ce.FormalizedCommentDocumentation;
                        Visibility = ce.Visibility;
                    }

                    Namespace = node.Root.MainProgram.Namespace;
                }
                // initialisation as Function Declaration
                else if (node is FunctionDeclaration)
                {
                    FunctionDeclarationHeader
                        ce = ((FunctionDeclaration)node).CodeElement as FunctionDeclarationHeader;
                    if (ce != null)
                    {
                        doc = ce.FormalizedCommentDocumentation;
                        Visibility = ce.Visibility;
                    }

                    Namespace = node.Root.MainProgram.Namespace;
                }
                // initialisation as Program Declaration
                else
                {
                    ProgramIdentification ce = ((Program)node).CodeElement as ProgramIdentification;
                    if (ce != null) doc = ce.FormalizedCommentDocumentation;
                    Namespace = ((Program)node).Namespace;
                }

                if (doc != null && !doc.Description.IsNullOrEmpty())
                    Description = doc.Description.Trim();
                FormCom = doc;
            }
        }

        /// <summary>
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        protected Documentation() { }
    }


    /// <summary>
    /// DocumentationForType contains the documentation information relative to a Type Definition
    /// </summary>
    /// <param name="Childrens">List of the Type children in case of group otherwise Childrens is null</param>
    /// <param name="IsBlankWheneZero">Is set to true if the Type have the option "BLANK WHENE ZERO"</param>
    /// <param name="Justified">Is set to true if the Type have the option "JUSTIFIED RIGHT"</param>
    /// <param name="DocDataType">Contains the information relative to the type data type(Usage, Occurs, Value, PIC ...)</param>
    [Serializable]
    public class DocumentationForType : Documentation
    {
        [XmlArray("Childrens")]
        [XmlArrayItem("Children", typeof(DocumentationTypeChildren))]
        public List<DocumentationTypeChildren> Childrens { get; set; }

        public override bool IsTypeDef => true;
        public bool IsBlankWheneZero { get; set; }
        public bool Justified { get; set; }
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
                IsBlankWheneZero = ce.IsBlankWhenZero?.Value ?? false;
                Justified = ce.IsJustified?.Value ?? false;
            }

            // Build the TypeDef DataType in case of the type itself have data informations (Usage, Occurs, Value, PIC ...)
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
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        private DocumentationForType() { }
    }


    /// <summary>
    /// DocumentationTypeChildren contains information of a Type Definition children if this Type Definition is a group
    /// </summary>
    /// <param name="Childrens">List of this data children in case of group otherwise Childrens will be null</param>
    /// <param name="Name">List of the Type children in case of group otherwise Childrens is null</param>
    /// <param name="IsBlankWheneZero">Is set to true if current data have the option "BLANK WHENE ZERO"</param>
    /// <param name="Justified">Is set to true if current data have the option "JUSTIFIED RIGHT"</param>
    /// <param name="IsLevel77">Is set to true if current data is defined in level 77</param>
    /// <param name="IsLevel88">Is set to true if current data is defined in level 77</param>
    /// <param name="ConditionValues">(only for level 88 data) contains an array of the equality conditions defined</param>
    /// <param name="ConditionValuesRanges">(only for level 88 data) contains an array of the interval conditions defined</param>
    /// <param name="DocDataType">Contains the information relative to the type data type(Usage, Occurs, Value, PIC ...)</param>
    [Serializable]
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
        public bool IsSubGroup => !Childrens.IsNullOrEmpty();

        [XmlArray("Childrens")]
        [XmlArrayItem("Children", typeof(DocumentationTypeChildren))]
        public List<DocumentationTypeChildren> Childrens { get; set; }

        [XmlArray("ConditionValues")]
        public string[] ConditionValues { get; set; }

        [XmlArray("ConditionValuesRanges")]
        [XmlArrayItem("ConditionValuesRange", typeof(ConditionValuesRange))]
        public ConditionValuesRange[] ConditionValuesRanges { get; set; }

        public string Name { get; set; }
        public bool IsBlankWheneZero { get; set; }
        public bool Justified { get; set; }
        public bool IsLevel77 { get; set; }
        public bool IsLevel88 { get; set; }
        public DocumentationDataType DocDataType { get; set; }


        /// <summary>
        /// Main Constructor for DocumentationTypeChildren
        /// </summary>
        /// <param name="dataDef"></param>
        public DocumentationTypeChildren(DataDefinition dataDef)
        {
            // Get the informations of the Code Element
            DataDefinitionEntry ce = dataDef.CodeElement as DataDefinitionEntry;
            if (ce != null)
            {
                IsLevel77 = ce.LevelNumber?.Value == 77;
                IsLevel88 = ce.LevelNumber?.Value == 88;
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
                    CommonDataDescriptionAndDataRedefines ceDescAndRedif = dataDef.CodeElement as CommonDataDescriptionAndDataRedefines;
                    if (ceDescAndRedif != null)
                    {
                        IsBlankWheneZero = ceDescAndRedif.IsBlankWhenZero?.Value ?? false;
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
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        public DocumentationTypeChildren() { }

    }


    /// <summary>
    /// DocumentationForFunction contains the documentation information relative to a Function Declaraton
    /// </summary>
    /// <param name="Needs">Needs list coresponding to the Needs field inside the Formalized Comment</param>
    /// <param name="ToDo">ToDo list coresponding to the ToDo field inside the Formalized Comment</param>
    /// <param name="Parameters">Parameters list coresponding to the Parameters field inside the Formalized Comment and completed with the function signature</param>
    /// <param name="IsDeprecated">Is true if the Deprecated field is present, with or withour further informations</param>
    /// <param name="Deprecated">Deprecated field value inside the Formalized Comment</param>
    /// <param name="ReplacedBy">ReplacedBy field value inside the Formalized Comment</param>
    /// <param name="Restriction">Restriction field value inside the Formalized Comment</param>
    /// <param name="See">See field value inside the Formalized Comment</param>
    [Serializable]
    public class DocumentationForFunction : Documentation
    {
        [XmlArray("Needs")]
        [XmlArrayItem("Need", typeof(string))]
        public List<string> Needs { get; set; }

        [XmlArray("ToDos")]
        [XmlArrayItem("ToDo", typeof(string))]
        public List<string> ToDo { get; set; }

        [XmlArray("Parameters")]
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        public List<DocumentationParameter> Parameters { get; set; }

        public override bool IsFunction => true;
        public bool IsDeprecated => Deprecated != null;
        public string Deprecated { get; set; }
        public string ReplacedBy { get; set; }
        public string Restriction { get; set; }
        public string See { get; set; }

        /// <summary>
        /// Main Constructor for DocumentationForFunction
        /// </summary>
        /// <param name="functionDeclaration">The Node to serialize</param>
        public DocumentationForFunction(FunctionDeclaration functionDeclaration) : base(functionDeclaration)
        {
            Parameters = new List<DocumentationParameter>();

            FunctionDeclarationHeader ce = functionDeclaration.CodeElement as FunctionDeclarationHeader;
            if (ce != null)
            {
                Visibility = ce.Visibility;
                if (ce.FormalizedCommentDocumentation != null)
                {
                    // Description
                    if (!ce.FormalizedCommentDocumentation.Description.IsNullOrEmpty())
                        Description = ce.FormalizedCommentDocumentation.Description;

                    // Deprecated
                    Deprecated = ce.FormalizedCommentDocumentation.Deprecated;

                    // ReplacedBy
                    if (!ce.FormalizedCommentDocumentation.ReplacedBy.IsNullOrEmpty())
                        ReplacedBy = ce.FormalizedCommentDocumentation.ReplacedBy;

                    // Restriction
                    if (!ce.FormalizedCommentDocumentation.Restriction.IsNullOrEmpty())
                        Restriction = ce.FormalizedCommentDocumentation.Restriction;

                    // See
                    if (!ce.FormalizedCommentDocumentation.See.IsNullOrEmpty())
                        See = ce.FormalizedCommentDocumentation.See;

                    // Needs
                    if (!ce.FormalizedCommentDocumentation.Needs.IsNullOrEmpty())
                        Needs = ce.FormalizedCommentDocumentation.Needs;

                    // ToDo
                    if (!ce.FormalizedCommentDocumentation.ToDo.IsNullOrEmpty())
                        ToDo = ce.FormalizedCommentDocumentation.ToDo;
                }

                foreach (ParameterDescription param in functionDeclaration.Profile.Parameters)
                {
                    string info = null;
                    if (ce.FormalizedCommentDocumentation != null &&
                        ce.FormalizedCommentDocumentation.Parameters.ContainsKey(param.Name))
                        info = ce.FormalizedCommentDocumentation.Parameters[param.Name];

                    Parameters.Add(new DocumentationParameter(param, info));
                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        private DocumentationForFunction() { }

    }


    /// <summary>
    /// DocumentationForProgram contains the documentation information relative to a Program Declaraton (Source, Stacked or Nestead)
    /// </summary>
    /// <param name="Needs">Needs list coresponding to the Needs field inside the Formalized Comment</param>
    /// <param name="ToDo">ToDo list coresponding to the ToDo field inside the Formalized Comment</param>
    /// <param name="Parameters">Parameters list coresponding to the Parameters field inside the Formalized Comment and completed with the program signature</param>
    /// <param name="IsDeprecated">Is true if the Deprecated field is present, with or withour further informations</param>
    /// <param name="Deprecated">Deprecated field value inside the Formalized Comment</param>
    /// <param name="ReplacedBy">ReplacedBy field value inside the Formalized Comment</param>
    /// <param name="Restriction">Restriction field value inside the Formalized Comment</param>
    /// <param name="See">See field value inside the Formalized Comment</param>
    [Serializable]
    public class DocumentationForProgram : Documentation
    {
        [XmlArrayItem("Need", typeof(string))]
        [XmlArray("Needs")]
        public List<string> Needs { get; set; }
        [XmlArrayItem("ToDo", typeof(string))]
        [XmlArray("ToDos")]
        public List<string> ToDo { get; set; }
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        [XmlArray("Parameters")]
        public List<DocumentationParameter> Parameters { get; set; }

        public override bool IsProgram => true;
        public bool IsDeprecated => Deprecated != null;
        public string Deprecated { get; set; }
        public string ReplacedBy { get; set; }
        public string Restriction { get; set; }
        public string See { get; set; }

        /// <summary>
        /// Main Constructor for DocumentationForFunction
        /// </summary>
        /// <param name="program">The Node to serialize</param>
        public DocumentationForProgram(Program program) : base(program)
        {
            // Formalized Comment information initialisation
            ProgramIdentification ce = program.CodeElement as ProgramIdentification;
            if (ce != null && ce.FormalizedCommentDocumentation != null)
            {
                // Description
                if (!ce.FormalizedCommentDocumentation.Description.IsNullOrEmpty())
                    Description = ce.FormalizedCommentDocumentation.Description;

                // Deprecated
                Deprecated = ce.FormalizedCommentDocumentation.Deprecated;

                // ReplacedBy
                if (!ce.FormalizedCommentDocumentation.ReplacedBy.IsNullOrEmpty())
                    ReplacedBy = ce.FormalizedCommentDocumentation.ReplacedBy;

                // Restriction
                if (!ce.FormalizedCommentDocumentation.Restriction.IsNullOrEmpty())
                    Restriction = ce.FormalizedCommentDocumentation.Restriction;

                // See
                if (!ce.FormalizedCommentDocumentation.See.IsNullOrEmpty())
                    See = ce.FormalizedCommentDocumentation.See;

                // Needs
                if (!ce.FormalizedCommentDocumentation.Needs.IsNullOrEmpty())
                    Needs = ce.FormalizedCommentDocumentation.Needs;

                // ToDo
                if (!ce.FormalizedCommentDocumentation.ToDo.IsNullOrEmpty())
                    ToDo = ce.FormalizedCommentDocumentation.ToDo;
            }
            
            // Parameters
            ProcedureDivisionHeader procedureDivision = program.Children.FirstOrDefault(x => x is ProcedureDivision)
                ?.CodeElement as ProcedureDivisionHeader;
            if (procedureDivision != null)
            {
                // Get the using clause of the Procedure Division if any
                var usingParams = procedureDivision.UsingParameters;
                if (!usingParams.IsNullOrEmpty())
                {
                    Parameters = new List<DocumentationParameter>();
                    foreach (var param in usingParams)
                    {
                        // Get the DataDefinition related to each parameters
                        DataDefinition dataDef = program.SymbolTable.GetVariables(param.StorageArea.SymbolReference)
                            .FirstOrDefault();
                        if (dataDef != null)
                        {
                            // match the formalized comment parameter description with the right parameter
                            string info = null;
                            if (ce != null && (ce.FormalizedCommentDocumentation != null &&
                                               ce.FormalizedCommentDocumentation.Parameters.ContainsKey(dataDef.Name)))
                                info = ce.FormalizedCommentDocumentation.Parameters[dataDef.Name];

                            Parameters.Add(new DocumentationParameter(dataDef, info));
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        private DocumentationForProgram() { }
    }


    /// <summary>
    /// Parameter inside a Function or a Program
    /// </summary>
    /// <param name="Name">The Name of the parameter data</param>
    /// <param name="PassingType">The passing type (INPUT/OUTPUT/INOUT/Unknow)</param>
    /// <param name="Info">The Description of the parameter given inside the Formalized Comment</param>
    /// <param name="DocDataType">   </param>
    [Serializable]
    public class DocumentationParameter
    {
        public enum PassingTypes
        {
            Input,
            Output,
            InOut,
            Unknow
        }

        [XmlIgnore]
        public bool IsInput => PassingType == PassingTypes.Input;
        [XmlIgnore]
        public bool IsOutput => PassingType == PassingTypes.Output;
        [XmlIgnore]
        public bool IsInOut => PassingType == PassingTypes.InOut;

        public string Name { get; set; }
        public PassingTypes PassingType { get; set; }
        public string Info { get; set; }
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
            PassingType = passTypeInt < 3 ? (PassingTypes)passTypeInt : PassingTypes.Unknow;
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

            if (determinePassing && !info.IsNullOrEmpty())
            {
                var processedData = DetermineType(info);
                PassingType = processedData.Item1;
                Info = processedData.Item2;
            }
            else
                PassingType = PassingTypes.Unknow;
        }
        

        /// <summary>
        /// Default constructor needed for serialisation. Do not use it.
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
                    passingType = PassingTypes.Unknow;
                    break;
            }

            if (passingType != PassingTypes.Unknow)
            {
                int index;
                for (index = 6; !char.IsLetter(info, index); index++){}
                info = info.Substring(index, info.Length - index).Trim();
            }
            return new Tuple<PassingTypes, string>(passingType, info);
        }
    }
    

    /// <summary>
    /// The DataType usefull for the documentation generation
    /// </summary>
    /// <param name="TypeName">If the Data is a Type implementation, this is the Type Name otherwise this is null</param>
    /// <param name="Picture">If the Data is a Picture it contain the Picture Type (ex. S9(5)V(2)) </param>
    /// <param name="Usage">Contain the Data Usage if any </param>
    /// <param name="MaxOccurence">Contain the Data Usage if any </param>
    /// <param name="DefaultValue">Contain the Data Value if any (Does not work for now for other Types than BOOL but may serve for futur implementation)</param>
    [Serializable]
    public class DocumentationDataType
    {
        [XmlIgnore]
        public bool IsArray => MaxOccurence > 1;
        [XmlIgnore]
        public bool IsType => TypeName != null;

        public DataUsage? Usage { get; set; }
        public long MaxOccurence { get; set; }
        public string DefaultValue { get; set; }
        public string TypeName { get; set; }
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
        /// Constructor used as Default constructor wich is needed for serialisation
        /// </summary>
        public DocumentationDataType(DataUsage? usage=null, long maxOccurence=1, string defaultValue=null, string typeName=null, string picture=null)
        {
            Usage        = usage;
            MaxOccurence = maxOccurence;
            DefaultValue = defaultValue;
            TypeName     = typeName;
            Picture      = picture;
        }

        /// <summary>
        /// Default constructor needed for serialisation. Do not use it.
        /// </summary>
        public DocumentationDataType()
        {
            
        }
    }
}
