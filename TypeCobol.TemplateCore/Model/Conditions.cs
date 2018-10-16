using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Util;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A List of Conditions.
    /// </summary>
    public class Conditions : AttributedEntity, IList<Condition>, ITranspilable
    {
        /// <summary>
        /// The List of conditions.
        /// </summary>
        public IList<Condition> ConditionList
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Conditions()
        {
            ConditionList = new List<Condition>();
        }

        public Condition this[int index]
        { get
            {
                return ConditionList[index];
            }
            set => throw new NotImplementedException();
        }

        public bool IsReadOnly
        {
            get
            {
                return ConditionList.IsReadOnly;
            }
        }

        public int Count
        {
            get
            {
                return ConditionList.Count;
            }
        }

        private string _TranspiledCode;
        /// <summary>
        /// The Transpiled code for the list of conditions is a list of C# methods
        /// and a list of static array of condition tuples.
        /// </summary>
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {
                    System.IO.StringWriter stringWriter = new System.IO.StringWriter();
                    TextCodeWriter codeWriter = new TextCodeWriter(stringWriter);
                    codeWriter.Indent();
                    codeWriter.Indent();
                    for (int i = 0; i < Count; i++)
                    {
                        string condition = this[i].TranspiledCode;
                        codeWriter.WriteLine($"private static Tuple<string,string>[] __ConditionsAttributes_{i} = {condition};");
                        codeWriter.WriteLine($"public bool Conditions_{i}(TypeCobol.Compiler.Nodes.Node node)");
                        codeWriter.WriteLine("{");
                        codeWriter.Indent();
                        codeWriter.WriteLine($"return CheckConditions(node, __ConditionsAttributes_{i});");
                        codeWriter.Outdent();
                        codeWriter.WriteLine("}");
                    }
                    codeWriter.Flush();
                    _TranspiledCode = stringWriter.ToString();
                }
                return _TranspiledCode;
            }            
        }

        public void Add(Condition item)
        {
            ConditionList.Add(item);
        }

        public bool Contains(Condition item)
        {
            return ConditionList.Contains(item);
        }

        public void CopyTo(Condition[] array, int arrayIndex)
        {
            ConditionList.CopyTo(array, arrayIndex);
        }

        public int IndexOf(Condition item)
        {
            return ConditionList.IndexOf(item);
        }

        public void Insert(int index, Condition item)
        {
            ConditionList.Insert(index, item);
        }

        public bool Remove(Condition item)
        {
            return ConditionList.Remove(item);
        }

        public void RemoveAt(int index)
        {
            ConditionList.RemoveAt(index);
        }

        IEnumerator<Condition> IEnumerable<Condition>.GetEnumerator()
        {
            return ConditionList.GetEnumerator();
        }

        public void Clear()
        {
            ConditionList.Clear();
        }

        public IEnumerator GetEnumerator()
        {
            return ConditionList.GetEnumerator();
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
