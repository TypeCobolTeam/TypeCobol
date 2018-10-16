using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Transpiler;
using TypeCobol.TemplateCore.Util;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// The Node Model
    /// </summary>
    public class Node : AttributedEntity, ITranspilable
    {
        /// <summary>
        /// All Guards on this Node
        /// </summary>
        public List<Tuple<List<Condition>, Pattern, Skeleton>> Guards
        {
            get;
            private set;
        }

        /// <summary>
        /// Pattern, Guards mapping.
        /// </summary>
        public Dictionary<Pattern, List<Condition>> PatternGuards
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Node()
        {
            Guards = new List<Tuple<List<Condition>, Pattern, Skeleton>>();
            PatternGuards = new Dictionary<Pattern, List<Condition>>();
        }

        /// <summary>
        /// Get the C# expression for checking if a Pattern can be applied
        /// </summary>
        /// <param name="condition">The list of condition</param>
        /// <param name="pattern">the Pattern to be used</param>
        /// <returns>The condition expression string if any, and Empty string otherwise.</returns>
        private String GetConditionsPatternExpression(List<Condition> conditions, Pattern pattern)
        {
            //Check for the deprecated attribute.
            if (pattern.Attributes.ContainsKey(AttributeNames.Deprecated))
            {
                var attr = pattern.Attributes[AttributeNames.Deprecated];
                if (attr.Value.Equals("true"))
                    return string.Empty;
            }
            StringBuilder expr = new StringBuilder();
            String sep = "";
            //Check for the boolean_property
            if (pattern.Attributes.ContainsKey(AttributeNames.BooleanProperty))
            {
                var attr = pattern.Attributes[AttributeNames.BooleanProperty];
                string property = (string)attr.Value;
                expr.Append($"@Model.{property}");
                sep = " && ";
            }
            //Add all condition as || conditions
            String condSep = sep + '(';
            if (conditions != null && conditions.Count > 0)
            {
                foreach (var condition in conditions)
                {
                    string condExpr = $"@Model.Conditions_{condition.Index}(@Self)";
                    expr.Append(condSep);
                    expr.Append(condExpr);
                    condSep = " || ";
                }
                expr.Append(')');
            }
            return expr.ToString();
        }

        private string _TranspiledCode;
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {
                    bool bHasActions = true;//Do we have actions ?
                    //We must Generate the GetActions method for the Node
                    if (Guards.Count > 0)
                    {
                        System.IO.StringWriter stringWriter = new System.IO.StringWriter();
                        TextCodeWriter codeWriter = new TextCodeWriter(stringWriter);
                        //Generate the method Signature
                        codeWriter.Indent();
                        Attribute nodeAttribue = this.Attributes[AttributeNames.Node];
                        string node = nodeAttribue.Value as String;
                        string methodName = node.Replace('.', '_');
                        codeWriter.WriteLine($"public static List<TypeCobol.Codegen.Actions.Action> {methodName}(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)");
                        codeWriter.WriteLine("{");
                        codeWriter.Indent();
                        codeWriter.WriteLine("List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();");

                        foreach (var guards in Guards)
                        {//For each pattern
                            codeWriter.WriteLine("{");
                            codeWriter.Indent();
                            
                            var conditions = guards.Item1;
                            var pattern = guards.Item2;
                            var skeleton = guards.Item3;
                            //Get the Skeleton's Model name
                            string modelName = skeleton.SkeletonModelName;

                            //Create the Model variable
                            codeWriter.WriteLine($"{modelName} @Model = new {modelName}(@Self);");
                            string condExpr = GetConditionsPatternExpression(conditions, pattern);
                            if (condExpr.Length > 0)
                            {
                                codeWriter.WriteLine($"if ({condExpr})");
                                codeWriter.WriteLine("{");
                                codeWriter.Indent();
                            }

                            //Now emit the Action Pattern code.
                            //1) Create the result variable of the emitted code
                            codeWriter.WriteLine("StringBuilder @SelfResult = new StringBuilder();");
                            //2)  Create the interpolation mixed code, if we have some code.
                            if (pattern.Code != null)
                            {
                                CSharpHtmlRazorInterpolation interpolator = new CSharpHtmlRazorInterpolation();
                                RazorTranspiler transpiler = new CSharpHtmlRazorTranspiler(interpolator);
                                bool bResult = transpiler.Parse(pattern.Code);
                                if (!bResult)
                                {//Transpilation error ==> generate a message
                                    interpolator.WriteErrors();
                                }
                                else
                                {
                                    string mixedcsharpInterpolateString = interpolator.MixedCodeInterpolationString.ToString();
                                    codeWriter.WriteLine(mixedcsharpInterpolateString);
                                }
                            }
                            //if we have an action ==> Create the Action and add it to the list of action.
                            string createActionStmt = pattern.TranspiledCode;
                            codeWriter.WriteLine(createActionStmt);
                            codeWriter.WriteLine("if (@SelfAction != null)");
                            codeWriter.WriteLine("{");
                            codeWriter.Indent();
                            codeWriter.WriteLine("@SelfActions.Add(@SelfAction);");
                            codeWriter.Outdent();
                            codeWriter.WriteLine("}");


                            if (condExpr.Length > 0)
                            {
                                codeWriter.Outdent();
                                codeWriter.WriteLine("}");
                            }

                            codeWriter.Outdent();
                            codeWriter.WriteLine("}");
                        }

                        codeWriter.WriteLine("return @SelfActions;");
                        codeWriter.Outdent();                        
                        codeWriter.WriteLine("}");
                        codeWriter.Outdent();

                        codeWriter.Flush();
                        _TranspiledCode = stringWriter.ToString();
                    }
                }
                return _TranspiledCode;
            }            
        }

        /// <summary>
        /// Add a Pattern with its Guard.
        /// </summary>
        /// <param name="pattern">The pattern to add</param>
        /// <param name="guard">The Guard condition</param>
        /// <param name="skeleton">Owner Skeleton</param>
        internal void AddPatternWithGuard(Pattern pattern, Condition guard, Skeleton skeleton)
        {
            List<Condition> guards = null;
            if (PatternGuards.ContainsKey(pattern))
            {
                guards = PatternGuards[pattern];
            }
            else
            {
                guards = new List<Condition>();
                Guards.Add(new Tuple<List<Condition>, Pattern, Skeleton>(guards, pattern, skeleton));
                PatternGuards[pattern] = guards;
            }
            guards.Add(guard);
        }

        /// <summary>
        /// Visitor Method
        /// </summary>
        /// <typeparam name="R"></typeparam>
        /// <typeparam name="D"></typeparam>
        /// <param name="v"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public R Accept<R, D>(IModelVisitor<R, D> v, D data)
        {
            return v.Visit(this, data);
        }
    }
}
