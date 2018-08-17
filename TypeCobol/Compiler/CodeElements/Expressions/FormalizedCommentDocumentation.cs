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
    /// Specify the Formalized Comments content if any.
    /// </summary> 
    public class FormalizedCommentDocumentation
    {
        /// in the Formalized comments fields can have 3 states:
        /// - missing
        /// - Flag (present but no value is given) (only Deprecated)
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

        /// <summary>
        /// complete the formalizedCommentDocumentation with the context lines given by ANTLR
        /// </summary>
        /// <param name="lines">Array of lines</param>
        public FormalizedCommentDocumentation(CodeElementsParser.FormalizedCommentLineContext[] lines)
        {
            Parameters      = new Dictionary<string, string>();
            Needs           = new List<string>();
            ToDo            = new List<string>();

            if (lines.Any())
            {
                // Initialize the default parameter with Description in order to add the first value to the description if no '@' or '-' is given
                Fields currentParameter = Fields.Description;
                foreach (var comLine in lines)
                {
                    if (comLine.formalizedCommentOuterLevel() != null)
                    { // The line is a new parameter line

                        // Description keyword
                        if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsDescription() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Description;

                        // Parameters keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsParameters() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Parameters;

                        // Deprecated keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsDeprecated() != null)
                        { 
                            currentParameter = FormalizedCommentDocumentation.Fields.Deprecated;
                                Deprecated += "";
                        }

                        // ReplacedBy keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsReplacedBy() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.ReplacedBy;

                        // Restriction keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsRestriction() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Restriction;

                        // Needs keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsNeed() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Needs;

                        // See keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsSee() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.See;

                        // ToDo keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FormComsToDo() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.ToDo;
                        

                        // finally ad the value to the right parameter if any value is given
                        if (comLine.formalizedCommentOuterLevel().FormComsValue() != null)
                        {
                            Add(currentParameter, comLine.formalizedCommentOuterLevel().FormComsValue().GetText());
                        }

                    }
                    else if (comLine.formalizedCommentInnerLevel() != null)
                    { // The line is a list item or list key-value

                        if (comLine.formalizedCommentInnerLevel().listItemValue != null)
                        { // this is a list item
                            Add(currentParameter,
                                comLine.formalizedCommentInnerLevel().listItemValue.Text);
                        }

                        else if (comLine.formalizedCommentInnerLevel().FormComsValue() != null && currentParameter == Fields.Parameters)
                        { // this is a key-value pair (only for param)
                            string key = "";
                            if (comLine.formalizedCommentInnerLevel().formalizedCommentParam() != null)
                            { // the key is a keyword
                                key = comLine.formalizedCommentInnerLevel().formalizedCommentParam().GetText();
                            }
                            else if (comLine.formalizedCommentInnerLevel().UserDefinedWord() != null)
                            { // the key is a UserDefinedWord
                                key = comLine.formalizedCommentInnerLevel().UserDefinedWord().GetText();
                            }

                            // Funally add the key-value pair to the current parameter
                            Add(currentParameter, key, comLine.formalizedCommentInnerLevel().FormComsValue().GetText());
                        }
                        
                    }
                    else if (comLine.FormComsValue() != null)
                    { // The line is a string continuation
                        Add(currentParameter, comLine.FormComsValue().Symbol?.Text, true);
                    }
                }
            }
        }

        /// <summary>
        /// Add a value to the current parameter
        /// </summary>
        /// <param name="parameter">The current parameter that will recieve the value</param>
        /// <param name="value">The string to store</param>
        /// <param name="isContinuation">if set to true it mean that the value is juste the otherf part of the previous value and have to be concated</param>
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

        /// <summary>
        /// Add a key value pair to a parameter (only for Parameters)
        /// </summary>
        /// <param name="parameter">The parameter to add to (have to be Parameters for now)</param>
        /// <param name="key">The key is coresponding to the parameter name</param>
        /// <param name="value">The description associated to the key</param>
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

        /// <summary>
        /// Is a mirror of the Add methode that add a key value pair to a parameter (only for Parameters)
        /// </summary>
        /// <param name="parameter">The parameter to add to (have to be Parameters for now)</param>
        /// <param name="item">The key-value pair with the parameter name as key and its description as value</param>
        public void Add(Fields parameter, KeyValuePair<string, string> item)
        {
            Add(parameter, item.Key, item.Value);
        }


        /// <summary>
        /// Clean the value of the whitespaces before and after a string and remove the ':' character at the begining of the string if any.
        /// </summary>
        /// <param name="value">The string to clean</param>
        /// <returns>The string cleaned</returns>
        private string cleanValue(string value)
        {
            var cuttedValue = value.StartsWith(":") ? value.Substring(1, value.Length - 1) : value;
            return cuttedValue.Trim();
        }
    }
}
