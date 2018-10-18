using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Model;
using TypeCobol.TemplateCore.Util;

namespace TypeCobol.TemplateCore.Controller
{
    /// <summary>
    /// Skeletons Controller class, that manages the tranpilation of a set of Skeletons
    /// to their C# code.
    /// </summary>
    public class SkeletonsController : ITranspilable
    {
        /// <summary>
        /// The list of Skeleton
        /// </summary>
        public Skeletons SkeletonsList
        {
            get;
            set;
        }

        /// <summary>
        /// The Nodes 
        /// </summary>
        public Dictionary<string, Node> Nodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Emit static Declaartions of teh Skeleton file.
        /// </summary>
        /// <param name="codeWriter"></param>
        private void EmitStaticDeclarations(TextCodeWriter codeWriter)
        {
            //1) Emit the Node GetAction provider methods Map
            codeWriter.WriteLine("private static Dictionary<string, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>> NodeActionsProviderMap;");
            //2) Emit the Static Constructor
            codeWriter.WriteLine($"static {SkeletonClassName}()");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("NodeActionsProviderMap = new Dictionary<string, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>();");
            foreach(var node in Nodes)
            {
                codeWriter.WriteLine($@"NodeActionsProviderMap[""{node.Key}""]={node.Key.Replace('.', '_')};");
            }
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            //3) Output Skeleton Model declarations.
            string skeletonsCode = SkeletonsList.TranspiledCode;
            codeWriter.WriteLine(skeletonsCode);
        }

        private string SkeletonClassName = "Skeletons";
        private string _TranspiledCode;
        public string TranspiledCode
        {
            get
            {
                if (_TranspiledCode == null)
                {
                    CreateNodesModel();
                    System.IO.StringWriter stringWriter = new System.IO.StringWriter();
                    TextCodeWriter codeWriter = new TextCodeWriter(stringWriter);
                    //1) Emit the class header
                    codeWriter.WriteLine($"public partial class {SkeletonClassName}");
                    codeWriter.WriteLine("{");
                    codeWriter.Indent();
                    //2) Emit the static Constructor
                    EmitStaticDeclarations(codeWriter);
                    //3) Node Actions providers
                    foreach (var node in Nodes)
                    {
                        string nodeCode = node.Value.TranspiledCode;
                        codeWriter.WriteLine(nodeCode);
                    }
                    codeWriter.Outdent();
                    codeWriter.WriteLine("}");
                    codeWriter.Flush();
                    _TranspiledCode = stringWriter.ToString();

                }
                return _TranspiledCode;
            }
        }


        /// <summary>
        /// Empty constructor.
        /// </summary>
        public SkeletonsController() : this(null)
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="skeletons">The list of Skeletons</param>
        public SkeletonsController(Skeletons skeletons)
        {
            this.SkeletonsList = skeletons;            
        }

        /// <summary>
        /// Create the model of Nodes.
        /// </summary>
        public void CreateNodesModel()
        {
            if (Nodes != null)
            {
                return;
            }
            Nodes = new Dictionary<string, Node>();

            //First Construction the Dictionary of Nodes as model of nodes.
            SkeletonsNodesModeler modeler = new SkeletonsNodesModeler();
            Nodes = new Dictionary<string, Node>();
            this.SkeletonsList.Accept(modeler, Nodes);
        }
    }
}
