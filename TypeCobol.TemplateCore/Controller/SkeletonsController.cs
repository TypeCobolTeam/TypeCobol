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
            codeWriter.WriteLine("private static Dictionary<System.Type, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>> NodeActionsProviderMap;");
            //2) Emit the Static Constructor
            codeWriter.WriteLine($"static {SkeletonClassName}()");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("NodeActionsProviderMap = new Dictionary<System.Type, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>>();");
            foreach(var node in Nodes)
            {
                codeWriter.WriteLine($@"NodeActionsProviderMap[typeof({node.Key})]={node.Key.Replace('.', '_')};");
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
            codeWriter.WriteLine("System.Type curType = @Self.GetType();");
            codeWriter.WriteLine("List<System.Type> typesMatching = new List<System.Type>();");
            codeWriter.WriteLine("while (curType != null)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("if (NodeActionsProviderMap.ContainsKey(curType))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("typesMatching.Add(curType);");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine("Type[] interfaces = curType.GetInterfaces();");
            codeWriter.WriteLine("Type curIntf = null;");
            codeWriter.WriteLine("//Only look for direct interfaces matching.");
            codeWriter.WriteLine("if (interfaces != null && interfaces.Length > 0)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("foreach (Type intf in interfaces)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("if (NodeActionsProviderMap.ContainsKey(intf))");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("typesMatching.Add(intf);");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");

            codeWriter.Outdent();
            codeWriter.WriteLine("}");

            codeWriter.Outdent();
            codeWriter.WriteLine("}");


            codeWriter.WriteLine("curType = curType.BaseType;");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine("if (typesMatching.Count > 0)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("if (typesMatching.Count == 1)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>> provider = NodeActionsProviderMap[typesMatching[0]];");
            codeWriter.WriteLine("return provider(@Self, @SelfContext);");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine("else");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("List<TypeCobol.Codegen.Actions.Action> allActions = new List<TypeCobol.Codegen.Actions.Action>();");
            codeWriter.WriteLine("foreach(System.Type type in typesMatching)");
            codeWriter.WriteLine("{");
            codeWriter.Indent();
            codeWriter.WriteLine("Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>> provider = NodeActionsProviderMap[type];");
            codeWriter.WriteLine("List<TypeCobol.Codegen.Actions.Action> actions = provider(@Self, @SelfContext);");
            codeWriter.WriteLine("if (actions != null) allActions.AddRange(actions);");
            codeWriter.Outdent();
            codeWriter.WriteLine("}");
            codeWriter.WriteLine("return allActions;");

            codeWriter.Outdent();
            codeWriter.WriteLine("}");
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
                    //0) Emit DO NOT EDIT and usings
                    codeWriter.WriteLine("//-----------------------------------------------------------------------------");
                    codeWriter.WriteLine($"//This file is automatically generated by {System.Reflection.Assembly.GetEntryAssembly().GetName().Name}.");
                    if (InputFile != null)
                    {
                        codeWriter.WriteLine("//From file: " + new FileInfo(InputFile).Name);
                    }
                    codeWriter.WriteLine("//Version: " + System.Reflection.Assembly.GetEntryAssembly().GetName().Version.ToString());
                    codeWriter.WriteLine("//DO NOT EDIT :-|");
                    codeWriter.WriteLine("//-----------------------------------------------------------------------------");
                    codeWriter.WriteLine("using System;");
                    codeWriter.WriteLine("using System.Text;");
                    codeWriter.WriteLine("using System.Collections.Generic;");
                    codeWriter.WriteLine("");

                    codeWriter.WriteLine("namespace TypeCobol.Codegen.Actions");
                    codeWriter.WriteLine("{");
                    codeWriter.Indent();

                    //1) Emit the class header
                    codeWriter.WriteLine($"public partial class {SkeletonClassName} : TypeCobol.Codegen.Actions.IActionsProvider");
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
            controller.InputFile = input;
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
        /// A reference to the Skeleton input file if any, null otherwise.
        /// </summary>
        public String InputFile
        {
            get;
            private set;
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
