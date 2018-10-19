using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Model;
using TypeCobol.TemplateCore.SaxParser;
using TypeCobol.TemplateCore.Transpiler;
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
            codeWriter.WriteLine("private static Dictionary<string, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>> NodeActionsProviderMap;");
            //2) Emit the Static Constructor
            codeWriter.WriteLine($"static {SkeletonClassName}()");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("NodeActionsProviderMap = new Dictionary<string, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>>();");
            foreach(var node in Nodes)
            {
                codeWriter.WriteLine($@"NodeActionsProviderMap[""{node.Key}""]={node.Key.Replace('.', '_')};");
            }

            codeWriter.Outdent();
            codeWriter.WriteLine("}");

            //3) Emit Empty constructor
            codeWriter.WriteLine($"public {SkeletonClassName}()");
            codeWriter.WriteLine("{");
            codeWriter.WriteLine("}");

            //4) Output Skeleton Model declarations.
            string skeletonsCode = SkeletonsList.TranspiledCode;
            codeWriter.WriteLine(skeletonsCode);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeWriter"></param>
        /// <returns></returns>
        public void EmitGetActionsMethod(TextCodeWriter codeWriter)
        {
            codeWriter.WriteLine($"public List<TypeCobol.Codegen.Actions.Action> GetActions(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)");
            codeWriter.WriteLine("{");            
            codeWriter.Indent();
            codeWriter.WriteLine("if (@Self == null) return null;");
            codeWriter.WriteLine("string node = @Self.GetType().FullName.Replace('.', '_');");
            codeWriter.WriteLine("if (NodeActionsProviderMap.ContainsKey(node))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>> provider = NodeActionsProviderMap[node];");
            codeWriter.WriteLine("return provider(@Self, @SelfContext);");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine("return null;");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
        }

        private static string DefaultOutputClassName = "Skeletons";
        private string _SkeletonClassName = DefaultOutputClassName;
        public string SkeletonClassName
        {
            get
            {
                return _SkeletonClassName ?? DefaultOutputClassName;
            }
            set
            {
                _SkeletonClassName = value;
            }
        }

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
                    //0) Emit usings
                    codeWriter.WriteLine("using System;");
                    codeWriter.WriteLine("using System.Text;");
                    codeWriter.WriteLine("using System.Collections.Generic;");
                    codeWriter.WriteLine("");

                    codeWriter.WriteLine("namespace TypeCobol.Codegen.Actions");
                    codeWriter.WriteLine("{");
                    codeWriter.Indent();

                    //1) Emit the class header
                    codeWriter.WriteLine($"public partial class {SkeletonClassName}");
                    codeWriter.WriteLine("{");
                    codeWriter.Indent();
                    //2) Emit the static Constructor
                    EmitStaticDeclarations(codeWriter);
                    //3) Emit the GetActions(...) method call
                    EmitGetActionsMethod(codeWriter);
                    //4) Node Actions providers
                    foreach (var node in Nodes)
                    {
                        string nodeCode = node.Value.TranspiledCode;
                        codeWriter.WriteLine(nodeCode);
                    }
                    codeWriter.Outdent();
                    codeWriter.WriteLine("}");
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
        /// Transpile the given input file in the given output file
        /// </summary>
        /// <param name="input">The input file</param>
        /// <param name="output">The output file</param>
        /// <param name="output">The output file</param>
        /// <param name=className">The defautl class name to use</param>
        /// <returns></returns>
        public static bool Transpile(string input, string output, string className = null)
        {
            string currentDir = System.IO.Directory.GetCurrentDirectory();            
            string xsdFile = System.IO.Path.Combine(System.IO.Path.Combine(currentDir, "Xml"), "Skeleton.xsd");
            System.IO.FileInfo fi = new System.IO.FileInfo(xsdFile);

            SkeletonSaxParser parser = fi.Exists ? new SkeletonSaxParser(input, xsdFile) : new SkeletonSaxParser(input);
            try
            {
                parser.Parse();
                bool bValidate = parser.ValidationErrorCount == 0 && parser.ValidationWarningCount == 0;
                if (!bValidate)
                {
                    System.Console.Error.WriteLine(parser.ValidationMessage.ToString());
                    return false;
                }                
            }
            catch (Exception e)
            {
                System.Console.Error.WriteLine(e.Message);
                return false;
            }
            SkeletonsController controller = new SkeletonsController(parser.Skeletons);
            if (className != null)
                controller.SkeletonClassName = className;
            string code = controller.TranspiledCode;
            try
            {                
                using (System.IO.StreamWriter sw = new System.IO.StreamWriter(new FileStream(output, FileMode.Create), Encoding.UTF8))
                {
                    sw.Write(code);
                    sw.Flush();
                }
            }
            catch(Exception e)
            {
                System.Console.Error.WriteLine(e.Message);
                return false;
            }
            return true;
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
