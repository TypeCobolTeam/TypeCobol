using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// Usefull function for the Generator.
    /// </summary>
    public class GeneratorHelper
    {
        /// <summary>
        /// <summary>
        /// Compute the proper paths of a data defintion. The proper root paths are
        /// root procedures path and data definition variables path.
        /// </summary>
        /// <param name="dataDef">The Data Definition to compute the proper root path</param>
        /// <param name="pathProcedures">[out] root procedures path</param>
        /// <param name="pathVariables">[out]variables path</param>
        public static void ComputeProperPaths(TypeCobol.Compiler.Nodes.DataDefinition dataDef, out List<string> pathProcedures, out List<string> pathVariables)
        {
            pathProcedures = new List<string>();
            pathVariables = new List<string>();
            pathVariables.Add(dataDef.Name);
            var parent = dataDef.Parent;
            while (parent != null)
            {
                if (!(parent is TypeCobol.Compiler.Nodes.DataDefinition))
                {
                    if (!string.IsNullOrEmpty(parent.Name))
                    {
                        pathProcedures.Add(parent.Name);
                    }
                }
                else
                {
                    pathVariables.Add(parent.Name);
                }
                parent = parent.Parent;
            }
            pathProcedures.Reverse();
            pathVariables.Reverse();
        }

        /// <summary>
        /// <summary>
        /// Compute the typed proper paths of a data defintion. The proper root paths are
        /// root procedures path and data definition variables path.
        /// </summary>
        /// <param name="dataNode">The node owning the data dedinition.</param>
        /// <param name="dataDesc">The Data Description to compute the proper root path</param>
        /// <param name="properType"> The Proper type of the data definition </param>
        /// <param name="pathProcedures">[out] root procedures path</param>
        /// <param name="pathVariables">[out]typed variables path</param>
        public static void ComputeTypedProperPaths(Node dataNode, DataDescriptionEntry dataDesc, Compiler.Nodes.TypeDefinition properType, out List<string> pathProcedures, out List<Tuple<string, string>> pathVariables)
        {
            System.Diagnostics.Contracts.Contract.Requires(properType != null);
            pathProcedures = new List<string>();
            pathVariables = new List<Tuple<string, string>>();
            if (properType == null)
                return;
            pathVariables.Add(new Tuple<string, string>(dataDesc.Name, properType.Name));
            Compiler.Nodes.Node parent = dataNode.Parent;
            while (parent != null)
            {
                if (parent is TypeCobol.Compiler.Nodes.DataDefinition)
                {
                    TypeCobol.Compiler.Nodes.DataDefinition dataParent = parent as TypeCobol.Compiler.Nodes.DataDefinition;
                    pathVariables.Add(new System.Tuple<string, string>(dataParent.Name, ""));
                }
                else if (!string.IsNullOrEmpty(parent.Name))
                {
                    pathProcedures.Add(parent.Name);
                }
                parent = parent.Parent;
            }
        }

        /// <summary>
        /// Compute the hash+name of the given qualified name index.
        /// </summary>
        /// <param name="qualified_name"></param>
        /// <returns></returns>
        public static string ComputeIndexHashName(string qualified_name, Node sourceNode)
        {
            string[] items = qualified_name.Split('.');
            string name = items[items.Length - 1];
            string hash = Tools.Hash.CreateCOBOLNameHash(qualified_name.ToLower(), 8, sourceNode);
            string hashName = hash + name;
#if DEBUG
            //System.Diagnostics.StackFrame sf = new System.Diagnostics.StackFrame(1, false);
            //System.Reflection.MethodBase method = sf.GetMethod();
            //System.Diagnostics.Debug.WriteLine(string.Format("[#####]ComputeIndexHashName[#####] Called By: {0} : HASH NAME = {1}({2})", method.Name, qualified_name, hashName));
#endif
            return hashName;
        }

    }
}