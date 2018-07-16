using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Castle.Core.Internal;
using JetBrains.Annotations;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.CodeElements
{

    /// <summary> 
    /// Is used to specify the Formalized Comments content if any.
    /// </summary> 
    public class Documentation
    {
        /// in the Formalized comments fields can have 3 states:
        /// - missing
        /// - Flag (present but no value is given)
        /// - Key-Value
        public List<Fields> GivenParameters;

        public Dictionary<string, string> Parameters { get; private set; }
        public List<string> Needs { get; private set; }
        public List<string> ToDo { get; private set; }
        public string Description { get; private set; }
        public string Deprecated { get; private set; }
        public string ReplacedBy { get; private set; }
        public string Restriction { get; private set; }
        public string See { get; private set; }

        public enum Fields
        {
            Parameters,
            Needs,
            ToDo,
            Description,
            Deprecated,
            ReplacedBy,
            Restriction,
            See
        }

        // ctor complete the documentation with the context
        public Documentation(CodeElementsParser.FormalizedCommentLineContext[] lines)
        {
            Parameters      = new Dictionary<string, string>();
            Needs           = new List<string>();
            ToDo            = new List<string>();
            GivenParameters = new List<Fields>();

            if (lines.Any())
            {
                Fields currentParameter = Fields.Description;
                foreach (var comLine in lines)
                {
                    if (comLine.formalizedCommentKeyValueParam() != null)
                    {
                        if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam() != null)
                        {
                            if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsDescription() != null)
                                currentParameter = Documentation.Fields.Description;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsParameters() != null)
                                currentParameter = Documentation.Fields.Parameters;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsDeprecated() != null)
                                currentParameter = Documentation.Fields.Deprecated;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsReplacedBy() != null)
                                currentParameter = Documentation.Fields.ReplacedBy;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsRestriction() != null)
                                currentParameter = Documentation.Fields.Restriction;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsNeed() != null)
                                currentParameter = Documentation.Fields.Needs;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsSee() != null)
                                currentParameter = Documentation.Fields.See;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsToDo() != null)
                                currentParameter = Documentation.Fields.ToDo;

                            Add(currentParameter,
                                comLine.formalizedCommentKeyValueParam().FormComsValue()?.Symbol?.Text);
                        }
                        else if (comLine.formalizedCommentKeyValueParam().UserDefinedWord() != null)
                        {
                            if (currentParameter == Documentation.Fields.Parameters && comLine.formalizedCommentKeyValueParam().FormComsValue() != null)
                            {// parameter description line
                                Add(Documentation.Fields.Parameters,
                                    comLine.formalizedCommentKeyValueParam().UserDefinedWord().Symbol.Text,
                                    comLine.formalizedCommentKeyValueParam().FormComsValue()?.Symbol?.Text);
                            }
                        }
                    }
                    else if (comLine.FormComsValue() != null)
                    {
                        Add(currentParameter,
                            comLine.FormComsValue().Symbol?.Text);
                    }
                }
            }
        }

        public void Add(Fields parameter, string value = null)
        {
            value = value == null ? "" : cleanValue(value);
            if (!value.IsNullOrEmpty())
            {
                
                if (!GivenParameters.Contains(parameter))
                    GivenParameters.Add(parameter);

                switch (parameter)
                {
                case Fields.Needs:
                    Needs.Add(value);
                    break;
                case Fields.ToDo:
                    ToDo.Add(value);
                    break;
                case Fields.Description:
                    Description += value;
                    break;
                case Fields.Deprecated:
                    Deprecated += value;
                    break;
                case Fields.ReplacedBy:
                    ReplacedBy += value;
                    break;
                case Fields.Restriction:
                    Restriction += value;
                    break;
                case Fields.See:
                    See += value;
                    break;
                case Fields.Parameters:
                    throw new Exception("Parameters documentation field only accepte key value pair");
                    break;
                default:
                    throw new Exception("Bad documentation parameter given");
                }
            }
        }

        public void Add(Fields parameter, string key,  string value)
        { 
            if (parameter == Fields.Parameters)
            {
                if (key == null || value == null)
                    throw new Exception("Parameters documentation field does not accept null key or value");
                GivenParameters.Add(parameter);
                Parameters.Add(cleanValue(key), cleanValue(value));
            }
            else
                throw new Exception("Only parameters documentation field accepte key value pair");
        }

        public void Add(Fields parameter, KeyValuePair<string, string> item)
        {
            Add(parameter, item.Key, item.Value);
        }

        private string cleanValue(string value)
        {
            var cuttedValue = value.StartsWith(":") ? value.Substring(1, value.Length - 1) : value;
            return cuttedValue.Trim();
        }
    }
}
