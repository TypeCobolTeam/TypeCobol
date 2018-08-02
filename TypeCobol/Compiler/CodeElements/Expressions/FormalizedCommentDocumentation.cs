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
    public class FormalizedCommentDocumentation
    {
        /// in the Formalized comments fields can have 3 states:
        /// - missing
        /// - Flag (present but no value is given)
        /// - Key-Value

        public Dictionary<string, string> Parameters { get; private set; }
        public List<string> Needs { get; private set; }
        public List<string> ToDo { get; private set; }
        public string Description { get; private set; }
        public string Deprecated { get; private set; }
        public string ReplacedBy { get; private set; }
        public string Restriction { get; private set; }
        public string See { get; private set; }

        private string _lastParameterRegistered { get; set; }

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

        // ctor complete the formalizedCommentDocumentation with the context
        public FormalizedCommentDocumentation(CodeElementsParser.FormalizedCommentLineContext[] lines)
        {
            Parameters      = new Dictionary<string, string>();
            Needs           = new List<string>();
            ToDo            = new List<string>();

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
                                currentParameter = FormalizedCommentDocumentation.Fields.Description;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsParameters() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.Parameters;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsDeprecated() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.Deprecated;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsReplacedBy() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.ReplacedBy;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsRestriction() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.Restriction;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsNeed() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.Needs;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsSee() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.See;
                            else if (comLine.formalizedCommentKeyValueParam().formalizedCommentParam()
                                         .FormComsToDo() != null)
                                currentParameter = FormalizedCommentDocumentation.Fields.ToDo;

                            Add(currentParameter,
                                comLine.formalizedCommentKeyValueParam().FormComsValue()?.Symbol?.Text);
                        }
                        else if (comLine.formalizedCommentKeyValueParam().UserDefinedWord() != null)
                        {
                            if (currentParameter == FormalizedCommentDocumentation.Fields.Parameters && comLine.formalizedCommentKeyValueParam().FormComsValue() != null)
                            {// parameter description line
                                Add(FormalizedCommentDocumentation.Fields.Parameters,
                                    comLine.formalizedCommentKeyValueParam().UserDefinedWord().Symbol.Text,
                                    comLine.formalizedCommentKeyValueParam().FormComsValue()?.Symbol?.Text);
                            }
                            else
                                throw new Exception("Wrong keyword given: " + comLine.formalizedCommentKeyValueParam().UserDefinedWord().Symbol.Text);
                        }
                    }
                    else if (comLine.FormComsValue() != null)
                    {
                        Add(currentParameter,
                            comLine.FormComsValue().Symbol?.Text,
                            comLine.MinusOperator() == null);
                    }
                }
            }
        }

        public void Add(Fields parameter, string value = null, bool isContinuation = false)
        {
            value = value == null ? "" : cleanValue(value);
            if (!value.IsNullOrEmpty())
            {
                switch (parameter)
                {
                case Fields.Needs:
                    if (isContinuation)
                        Needs[Needs.LastIndexOf(Needs.Last())] += " " + value;
                    else
                        Needs.Add(value);
                    break;
                case Fields.ToDo:
                    if (isContinuation)
                        ToDo[ToDo.LastIndexOf(ToDo.Last())] += " " + value;
                    else
                        ToDo.Add(value);
                    break;
                case Fields.Description:
                    Description += value + " ";
                    break;
                case Fields.Deprecated:
                    Deprecated += value + " ";
                    break;
                case Fields.ReplacedBy:
                    ReplacedBy += value + " ";
                    break;
                case Fields.Restriction:
                    Restriction += value + " ";
                    break;
                case Fields.See:
                    See += value + " ";
                    break;
                case Fields.Parameters:

                    if (isContinuation)
                        Parameters[_lastParameterRegistered] += (Parameters[_lastParameterRegistered].IsNullOrEmpty()? "" : " ") + value;
                    else
                        throw new Exception("Parameters formalizedCommentDocumentation field only accepte key value pair");
                    break;
                default:
                    throw new Exception("Bad formalizedCommentDocumentation parameter given");
                }
            }
            // For parameters that can be flag or have value
            else if (parameter == Fields.Deprecated)
                Deprecated += value;
            
        }

        public void Add(Fields parameter, string key,  string value)
        { 
            if (parameter == Fields.Parameters)
            {
                if (key == null || value == null)
                    throw new Exception("Parameters formalizedCommentDocumentation field does not accept null key or value");
                Parameters.Add(cleanValue(key), cleanValue(value));
                _lastParameterRegistered = cleanValue(key);
            }
            else
                throw new Exception("Only parameters formalizedCommentDocumentation field accepte key value pair");
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
