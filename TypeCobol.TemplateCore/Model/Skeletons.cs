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
    /// The Skeletons model.
    /// </summary>
    public class Skeletons : AttributedEntity, IList<Skeleton>, ITranspilable
    {
        /// <summary>
        /// The List of all single skeletons.
        /// </summary>
        public IList<Skeleton> SkeletonList
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Skeletons()
        {
            SkeletonList = new List<Skeleton>();
        }
        /// <summary>
        /// Add a single skeleton to the model.
        /// </summary>
        /// <param name="skeleton"></param>
        public void Add(Skeleton skeleton)
        {
            SkeletonList.Add(skeleton);
        }

        public int IndexOf(Skeleton item)
        {
            return SkeletonList.IndexOf(item);
        }

        public void Insert(int index, Skeleton item)
        {
            SkeletonList.Insert(index, item);
        }

        public void RemoveAt(int index)
        {
            SkeletonList.RemoveAt(index);
        }

        public bool Contains(Skeleton item)
        {
            return SkeletonList.Contains(item);
        }

        public void CopyTo(Skeleton[] array, int arrayIndex)
        {
            SkeletonList.CopyTo(array, arrayIndex);
        }

        public bool Remove(Skeleton item)
        {
            return SkeletonList.Remove(item);
        }

        IEnumerator<Skeleton> IEnumerable<Skeleton>.GetEnumerator()
        {
            return SkeletonList.GetEnumerator();
        }

        public void Clear()
        {
            SkeletonList.Clear();
        }

        public IEnumerator GetEnumerator()
        {
            return SkeletonList.GetEnumerator();
        }

        public bool IsReadOnly
        {
            get
            {
                return SkeletonList.IsReadOnly;
            }
        }

        public int Count
        {
            get
            {
                return SkeletonList.Count;
            }
        }

        private void WriteCheckConditionsStaticMethod(TextCodeWriter codeWriter)
        {
            codeWriter.Indent();
            codeWriter.WriteLine($"public static bool CheckConditions(TypeCobol.Compiler.Nodes.Node node, Tuple<string,string>[] conditions)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();

            codeWriter.WriteLine("System.Diagnostics.Debug.Assert(typeof(TypeCobol.Compiler.Nodes.Node).IsAssignableFrom(node.GetType()));");
            codeWriter.WriteLine("foreach(var x in conditions)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();

            codeWriter.WriteLine("var property = node[x.Item1];");
            codeWriter.WriteLine(@"if ("" + "".Equals(x.Item2))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("var values = property as System.Collections.ICollection;");
            codeWriter.WriteLine("return values != null && values.Count > 0;");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine(@"else if ("" * "".Equals(x.Item2))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("return (property == null ? null : property.ToString()) != null;");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine(@"else if (!x.Item2.Equals(property == null ? null : property.ToString(), System.StringComparison.InvariantCultureIgnoreCase))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("return false;");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");

            codeWriter.Outdent();
            codeWriter.WriteLine("}");

            codeWriter.WriteLine("return true;");

            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.Outdent();
        }

        private string _TranspiledCode;
        /// <summary>
        /// The Transpilaled code for skeletons header and model classes.
        /// </summary>
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {                    
                    System.IO.StringWriter stringWriter = new System.IO.StringWriter();
                    TextCodeWriter codeWriter = new TextCodeWriter(stringWriter);

                    //---------------------------------------------------
                    //Output static local method for Checking conditions.
                    //---------------------------------------------------
                    WriteCheckConditionsStaticMethod(codeWriter);

                    //---------------------------------
                    //Output all Skeletons class Model
                    //---------------------------------
                    codeWriter.Indent();
                    foreach (Skeleton skeleton in SkeletonList)
                    {
                        string skeletonCode = skeleton.TranspiledCode;
                        codeWriter.WriteLine(skeletonCode);
                    }
                    codeWriter.Outdent();
                    codeWriter.Flush();
                    _TranspiledCode = stringWriter.ToString();
                }
                return _TranspiledCode;
            }
        }

        Skeleton IList<Skeleton>.this[int index]
        {
            get
            {
                return SkeletonList[index];
            }
            set => throw new NotImplementedException();
        }

        /// <summary>
        /// Skeleton List indexer.
        /// </summary>
        /// <param name="i">The index of the request skeleton.</param>
        /// <returns>The Skeleton at the given index position.</returns>
        public Skeleton this[int i]
        {
            get
            {
                return SkeletonList[i];
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
