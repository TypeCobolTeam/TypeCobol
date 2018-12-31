using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Util;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A Single condition.
    /// </summary>
    public class Condition : AttributedEntity, ITranspilable
    {
        /// <summary>
        /// The Index of this condition in its Condition List.
        /// </summary>
        public int Index
        {
            get;
            set;
        }
        private string _TranspiledCode;
        /// <summary>
        /// The Transpiled code for a condition is in fact a C# array of Tuple<name:string,value:string>
        /// of each condition name/value.
        /// The node condition is always the first one in the array.
        /// </summary>
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {
                    Attribute nameAttribute = this.Attributes[AttributeNames.Node];
                    string node = nameAttribute.Value.ToString();
                    StringBuilder sb = new StringBuilder("new Tuple<string,string>[]{");
                    //Node is the first condition.
                    sb.Append($@"new Tuple<string,string>(""{AttributeNames.Node}"",""{node}"")");
                    foreach (KeyValuePair<string, Attribute> e in this.Attributes)
                    {
                        if (!e.Key.Equals(AttributeNames.Node))
                        {
                            sb.Append($@", new Tuple<string,string>(""{e.Key}"",""{e.Value.Value}"")");
                        }
                    }
                    sb.Append("}");
                    _TranspiledCode = sb.ToString();                    
                }
                return _TranspiledCode;
            }            
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
