using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Is implemented on CodeElements that can have a Formalized Comment
    /// </summary>
    public interface IFormalizedCommentable
    {
        FormalizedCommentDocumentation FormalizedCommentDocumentation { get; set; }
    }

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
                                     .FORMALIZED_COMMENTS_DESCRIPTION() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Description;

                        // Parameters keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_PARAMETERS() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Parameters;

                        // Deprecated keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_DEPRECATED() != null)
                        { 
                            currentParameter = FormalizedCommentDocumentation.Fields.Deprecated;
                                Deprecated += "";
                        }

                        // ReplacedBy keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_REPLACED_BY() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.ReplacedBy;

                        // Restriction keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_RESTRICTION() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Restriction;

                        // Needs keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_NEED() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.Needs;

                        // See keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_SEE() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.See;

                        // ToDo keyword
                        else if (comLine.formalizedCommentOuterLevel().formalizedCommentParam()
                                     .FORMALIZED_COMMENTS_TODO() != null)
                            currentParameter = FormalizedCommentDocumentation.Fields.ToDo;
                        

                        // finally ad the value to the right parameter if any value is given
                        if (comLine.formalizedCommentOuterLevel().FORMALIZED_COMMENTS_VALUE() != null)
                        {
                            Add(currentParameter, comLine.formalizedCommentOuterLevel().FORMALIZED_COMMENTS_VALUE().Symbol);
                        }

                    }
                    else if (comLine.formalizedCommentInnerLevel() != null)
                    { // The line is a list item or list key-value

                        if (comLine.formalizedCommentInnerLevel().UserDefinedWord() != null && currentParameter == Fields.Parameters)
                        { // this is a key-value pair (only for Params field)
                            Add(currentParameter,
                                comLine.formalizedCommentInnerLevel().UserDefinedWord().GetText(),
                                comLine.formalizedCommentInnerLevel().FORMALIZED_COMMENTS_VALUE().GetText());
                        }
                        else if (comLine.formalizedCommentInnerLevel().listItemValue != null)
                        { // this is a list item
                            Add(currentParameter,
                                comLine.formalizedCommentInnerLevel().listItemValue);
                        }
                    }
                    else if (comLine.FORMALIZED_COMMENTS_VALUE() != null)
                    { // The line is a string continuation
                        Add(currentParameter, comLine.FORMALIZED_COMMENTS_VALUE().Symbol, true);
                    }
                }
            }
        }

        /// <summary>
        /// Add a value to the current parameter
        /// </summary>
        /// <param name="parameter">The current parameter that will receive the value</param>
        /// <param name="symbol"></param>
        /// <param name="isContinuation">if set to true it means that the value is just the other part of the previous value and have to be concatenated</param>
        public void Add(Fields parameter, IToken symbol = null, bool isContinuation = false)
        {
            string value = symbol?.Text;
            value = value == null ? "" : value.Trim();
            if (!string.IsNullOrEmpty(value))
            {
                switch (parameter)
                {
                case Fields.Needs:
                    if (isContinuation)
                        Needs[Needs.Count -1] += " " + value;
                    else
                        Needs.Add(value);
                    break;
                case Fields.ToDo:
                    if (isContinuation)
                        ToDo[ToDo.Count - 1] += " " + value;
                    else
                        ToDo.Add(value);
                    break;
                case Fields.Description:
                    if (!string.IsNullOrEmpty(Description))
                        Description += " ";
                    Description += value;
                    break;
                case Fields.Deprecated:
                    if (!string.IsNullOrEmpty(Deprecated))
                        Deprecated += " ";
                    Deprecated += value;
                    break;
                case Fields.ReplacedBy:
                    if (!string.IsNullOrEmpty(ReplacedBy))
                        ReplacedBy += " ";
                    ReplacedBy += value;
                    break;
                case Fields.Restriction:
                    if (!string.IsNullOrEmpty(Restriction))
                        Restriction += " ";
                    Restriction += value;
                    break;
                case Fields.See:
                    if (!string.IsNullOrEmpty(See))
                        See += " ";
                    See += value;
                    break;
                case Fields.Parameters:
                    if (isContinuation)
                        Parameters[_lastParameterRegistered] += (Parameters[_lastParameterRegistered].Length == 0 ? string.Empty : " ") + value;
                    else
                    {
                        Token token = symbol as Token;
                        TokensLine tokensLine = token?.TokensLine as TokensLine;
                        tokensLine?.AddDiagnostic(MessageCode.Warning, token, "Parameters formalizedCommentDocumentation field only accept key value pair");
                    }
                    break;
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
        /// <param name="key">The key is corresponding to the parameter name</param>
        /// <param name="value">The description associated to the key</param>
        public void Add(Fields parameter, string key,  string value)
        {
            if (parameter == Fields.Parameters)
            {
                if (!string.IsNullOrEmpty(key) && !string.IsNullOrEmpty(value))
                {
                    var trimmedKey = key.Trim();
                    Parameters.Add(trimmedKey, value.Trim());
                    _lastParameterRegistered = trimmedKey;
                }
            }
        }

        public override string ToString()
        {
            var writer = new StringWriter();
            Write(writer);
            return writer.ToString();
        }

        public void Write(TextWriter writer)
        {
            bool empty = true;

            if (Description != null)
            {
                writer.WriteLine(Description);
                empty = Description.Length == 0;
            }

            if (Restriction != null)
            {
                writer.WriteLine("Restriction: " + Restriction);
                empty = false;
            }

            if (Deprecated != null)
            {
                if (!empty)
                    writer.WriteLine();
                writer.WriteLine(Deprecated != string.Empty ? "Deprecated: " + Deprecated : "Deprecated");
                empty = false;
            }

            if (ReplacedBy != null)
            {
                writer.WriteLine("Replaced By: " + ReplacedBy);
                empty = false;
            }

            if (See != null)
            {
                if (!empty)
                    writer.WriteLine();
                writer.WriteLine("See: " + See);
                empty = false;
            }

            if (Parameters.Count > 0)
            {
                if (!empty)
                    writer.WriteLine();
                writer.WriteLine("Parameters:");
                foreach (var parameter in Parameters)
                {
                    writer.WriteLine("\t-\t" + parameter.Key + ": " + parameter.Value);
                }

                empty = false;
            }

            if (Needs.Count > 0)
            {
                if (!empty)
                    writer.WriteLine();
                writer.WriteLine("Needs:");
                foreach (var need in Needs)
                {
                    writer.WriteLine("\t-\t" + need);
                }

                empty = false;
            }

            if (ToDo.Count > 0)
            {
                if (!empty)
                    writer.WriteLine();
                writer.WriteLine("To do:");
                foreach (var todo in ToDo)
                {
                    writer.WriteLine("\t-\t" + todo);
                }
            }
        }
    }
}
