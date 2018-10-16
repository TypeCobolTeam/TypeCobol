using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Util;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A Single Skeleton template
    /// </summary>
    public class Skeleton : AttributedEntity, ITranspilable
    {
        /// <summary>
        /// Conditions on the Skeleton.
        /// </summary>
        public Conditions Conditions
        {
            get;
            set;
        }

        /// <summary>
        /// Patterns of the Skeleton.
        /// </summary>
        public Patterns Patterns
        {
            get;
            set;
        }

        private string _TranspiledCode;
        /// <summary>
        /// The transpiled code of a Skeleton is a struct declaration of
        /// it underlying environment.
        /// </summary>
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {
                    System.IO.StringWriter stringWriter = new System.IO.StringWriter();
                    TextCodeWriter codeWriter = new TextCodeWriter(stringWriter);
                    Attribute nameAttribute = this.Attributes[AttributeNames.Name];
                    string name = nameAttribute.Value.ToString();
                    name = name.Replace('.', '_');
                    codeWriter.Indent();
                    codeWriter.WriteLine($"struct SkeleTon{name}Model {{");
                    codeWriter.Indent();
                    if (this.Attributes.ContainsKey(AttributeNames.Var))
                    {
                        //------------------------
                        //Output dynamic variables
                        //------------------------
                        Attribute varAttribute = this.Attributes[AttributeNames.Var];
                        string vars = varAttribute.Value.ToString();
                        string[] var_items = vars.Split(',');
                        for (int i = 0; i < var_items.Length; i++)
                        {
                            codeWriter.WriteLine($"public dynamic {var_items[i]};");
                        }

                        codeWriter.WriteLine();
                        //-------------------------------------------------
                        //Now Ouput the constructor with a Node parameters
                        //-------------------------------------------------
                        codeWriter.WriteLine($"public SkeleTon{name}Model(TypeCobol.Compiler.Nodes.Node node)");
                        codeWriter.WriteLine("{");
                        codeWriter.Indent();
                        for (int i = 0; i < var_items.Length; i++)
                        {
                            codeWriter.WriteLine($@"{var_items[i]} = node[""{var_items[i]}""];");
                        }
                        codeWriter.Outdent();
                        codeWriter.WriteLine("}");
                        //----------------------------
                        //Now Output conditions code.
                        //----------------------------
                        string conditions = Conditions.TranspiledCode;
                        if (conditions != null)
                        {
                            codeWriter.WriteLine(conditions);
                        }
                    }
                    codeWriter.WriteLine("}");                    
                    codeWriter.Outdent();
                    codeWriter.Flush();
                    _TranspiledCode = stringWriter.ToString();
                }
                return _TranspiledCode;
            }
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Skeleton()
        {            
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
