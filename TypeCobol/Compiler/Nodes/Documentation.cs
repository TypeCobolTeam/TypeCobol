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

        protected Documentation(Node node)
        {
            Name = node.Name;

            if(node is TypeDefinition || node is FunctionDeclaration || node is Program)
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
                    Namespace = ((Program) node).Namespace;
                }

                if (doc != null && !doc.Description.IsNullOrEmpty())
                    Description = doc.Description.Trim();
                FormCom = doc;
            }
        }

        protected Documentation() {}
    }

    [Serializable]
    public class DocumentationForType : Documentation
    {
        public override bool IsTypeDef => true;

        public DocumentationForType(TypeDefinition typeDefinition) : base(typeDefinition) { }

        private DocumentationForType() {}
    }

    [Serializable]
    public class DocumentationForFunction : Documentation
    {
        public override bool IsFunction => true;
        public bool IsDeprecated => Deprecated != null;
        public string Deprecated { get; set; }
        public string ReplacedBy { get; set; }
        public string Restriction { get; set; }
        public string See { get; set; }
        [XmlArrayItem("Need", typeof(string))]
        [XmlArray("Needs")]
        public List<string> Needs { get; set; }
        [XmlArrayItem("ToDo", typeof(string))]
        [XmlArray("ToDos")]
        public List<string> ToDo { get; set; }
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        [XmlArray("Parameters")]
        public List<DocumentationParameter> Parameters { get; set; }

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

        private DocumentationForFunction() { }

    }

    [Serializable]
    public class DocumentationForProgram : Documentation
    {
        public override bool IsProgram => true;
        public bool IsDeprecated => Deprecated != null;
        public string Deprecated { get; set; }
        public string ReplacedBy { get; set; }
        public string Restriction { get; set; }
        public string See { get; set; }
        [XmlArrayItem("Need", typeof(string))]
        [XmlArray("Needs")]
        public List<string> Needs { get; set; }
        [XmlArrayItem("ToDo", typeof(string))]
        [XmlArray("ToDos")]
        public List<string> ToDo { get; set; }
        [XmlArrayItem("Parameter", typeof(DocumentationParameter))]
        [XmlArray("Parameters")]
        public List<DocumentationParameter> Parameters { get; set; }

        public DocumentationForProgram(Program program) : base(program)
        {
            ProgramIdentification ce = program.CodeElement as ProgramIdentification;
            ProcedureDivisionHeader procedureDivision = program.Children.FirstOrDefault(x => x is ProcedureDivision)
                ?
                .CodeElement as ProcedureDivisionHeader;

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
            if (procedureDivision != null)
            {
                var usingParams = procedureDivision.UsingParameters;
                if (!usingParams.IsNullOrEmpty())
                {
                    Parameters = new List<DocumentationParameter>();
                    foreach (var param in usingParams)
                    {
                        //DataDefinition dataDef = program.GetDataDefinitionFromStorageAreaDictionary(param.StorageArea);
                        DataDefinition dataDef = program.SymbolTable.GetVariables(param.StorageArea.SymbolReference)
                            .FirstOrDefault();
                        if (dataDef != null)
                        {
                            string info = null;
                            if (ce != null && (ce.FormalizedCommentDocumentation != null &&
                                               ce.FormalizedCommentDocumentation.Parameters.ContainsKey(dataDef.Name)))
                                info = ce.FormalizedCommentDocumentation.Parameters[dataDef.Name];

                            if (dataDef.TypeDefinition != null)
                            {
                                // Typed parameter
                                Parameters.Add(new DocumentationParameter(
                                    name: dataDef.Name,
                                    info: info,
                                    typeName: dataDef.TypeDefinition.Name,
                                    passingType: DocumentationParameter.PassingTypes.Unknow,
                                    determinePassing: info != null));

                            }
                            else
                            {
                                // picture parameter
                                Parameters.Add(new DocumentationParameter(
                                    name: dataDef.Name,
                                    info: info,
                                    picInfo: new DocumentationPictureInfo(dataDef.Picture?.Value, dataDef.Usage,
                                        dataDef.MaxOccurencesCount),
                                    passingType: DocumentationParameter.PassingTypes.Unknow,
                                    determinePassing: info != null));
                            }
                        }
                    }
                }
            }
        }
        private DocumentationForProgram() { }
    }

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

        public string Name { get; set; }
        [System.Xml.Serialization.XmlIgnore]
        public bool IsInput => PassingType == PassingTypes.Input;
        [System.Xml.Serialization.XmlIgnore]
        public bool IsOutput => PassingType == PassingTypes.Output;
        [System.Xml.Serialization.XmlIgnore]
        public bool IsInOut => PassingType == PassingTypes.InOut;
        public PassingTypes PassingType { get; set; }
        public string Info { get; set; }
        [System.Xml.Serialization.XmlIgnore]
        public bool IsType => !TypeName.IsNullOrEmpty();
        public string TypeName { get; set; }
        public DocumentationPictureInfo PictureInfo { get; set; }

        // ctor for program if the parameter is a typed variable
        public DocumentationParameter(string name, [CanBeNull] string info, string typeName,
            PassingTypes passingType = PassingTypes.Unknow, bool determinePassing = false)
            : this(name, info, passingType, determinePassing)
        {
            TypeName = typeName;
        }

        // ctor for program if the parameter is a Picture
        public DocumentationParameter(string name, [CanBeNull] string info, DocumentationPictureInfo picInfo,
            PassingTypes passingType = PassingTypes.Unknow, bool determinePassing = false)
            : this(name, info, passingType, determinePassing)
        {
            PictureInfo = picInfo;
        }

        // ctor for program
        private DocumentationParameter(string name, [CanBeNull] string info,
            PassingTypes passingType = PassingTypes.Unknow, bool determinePassing = false)
        {
            Name = name;
            if (determinePassing)
            {
                var processedData = DetermineType(info);
                PassingType = processedData.Item1;
                Info = processedData.Item2;
            }
            else
            {
                PassingType = passingType;
                Info = info;
            }
        }

        // ctor for function
        public DocumentationParameter(ParameterDescription parameter, [CanBeNull] string info)
        {
            int passTypeInt = (int) parameter.PassingType;
            PassingType = passTypeInt < 3 ? (PassingTypes)passTypeInt :  PassingTypes.Unknow;

            Name = parameter.Name;
            Info = info;
            if (parameter.TypeDefinition != null)
                TypeName = parameter.TypeDefinition.Name;
            else
                PictureInfo = new DocumentationPictureInfo(parameter);
        }

        private DocumentationParameter() { }

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
                for (index = 6; !char.IsLetter(info, index); index++)
                {
                }

                info = info.Substring(index, info.Length - index).Trim();
            }

            return new Tuple<PassingTypes, string>(passingType, info);
        }
    }

    [Serializable]
    public class DocumentationPictureInfo
    {
        public string PictureClause { get; set; }

        [System.Xml.Serialization.XmlIgnore]
        public bool IsAnArray => MaxOccurence > 1;
        public long MaxOccurence { get; set; }
        public DataUsage? Usage { get; set; }

        // the pictureText have to be without any spaces in order to use the PictureValidator
        public DocumentationPictureInfo(string pictureText, DataUsage? usage = null, long maxOccurence = 1)
        {
            Usage = usage;
            MaxOccurence = maxOccurence;
            PictureClause = pictureText;
        }

        public DocumentationPictureInfo(ParameterDescription parameter)
            :this(parameter.Picture?.Value, parameter.Usage, parameter.MaxOccurencesCount)
        { }

        private DocumentationPictureInfo() { }
    }

}
