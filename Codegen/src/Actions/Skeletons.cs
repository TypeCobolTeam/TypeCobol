using System;
using System.Text;
using System.Collections.Generic;

namespace TypeCobol.Codegen.Actions
{
    public partial class Skeletons : TypeCobol.Codegen.Actions.IActionsProvider
    {
        private static Dictionary<System.Type, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>> NodeActionsProviderMap;
        static Skeletons()
        {
            NodeActionsProviderMap = new Dictionary<System.Type, Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>>>();
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.TypeDefinition)] = TypeCobol_Compiler_Nodes_TypeDefinition;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.DataDescription)] = TypeCobol_Compiler_Nodes_DataDescription;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.Set)] = TypeCobol_Compiler_Nodes_Set;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.CodeElements.VariableWriter)] = TypeCobol_Compiler_CodeElements_VariableWriter;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.LibraryCopy)] = TypeCobol_Compiler_Nodes_LibraryCopy;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.FunctionDeclaration)] = TypeCobol_Compiler_Nodes_FunctionDeclaration;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.Nodes.ProcedureStyleCall)] = TypeCobol_Compiler_Nodes_ProcedureStyleCall;
            NodeActionsProviderMap[typeof(TypeCobol.Compiler.CodeModel.Program)] = TypeCobol_Compiler_CodeModel_Program;
        }
        public Skeletons()
        {
        }
        public static bool CheckConditions(TypeCobol.Compiler.Nodes.Node node, Tuple<string, string>[] conditions)
        {
            System.Diagnostics.Debug.Assert(typeof(TypeCobol.Compiler.Nodes.Node).IsAssignableFrom(node.GetType()));
            for (int i = 1; i < conditions.Length; i++)
            {
                var x = conditions[i];
                var property = node[x.Item1];
                if ("+".Equals(x.Item2))
                {
                    var values = property as System.Collections.ICollection;
                    return values != null && values.Count > 0;
                }
                else if ("*".Equals(x.Item2))
                {
                    return (property == null ? null : property.ToString()) != null;
                }
                else if (!x.Item2.Equals(property == null ? null : property.ToString(), System.StringComparison.InvariantCultureIgnoreCase))
                {
                    return false;
                }
            }
            return true;
        }
        struct SkeleTonTYPEDEFModel
        {
            public SkeleTonTYPEDEFModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.TypeDefinition") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonBOOL_DECLAREModel
        {
            public dynamic level;
            public dynamic name;
            public dynamic value;
            public dynamic global;

            public SkeleTonBOOL_DECLAREModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                level = @Self["level"];
                name = @Self["name"];
                value = @Self["value"];
                global = @Self["global"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.DataDescription"), new Tuple<string, string>("type", "BOOL") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonPOINTER_REDEFINESModel
        {
            public dynamic level;
            public dynamic name;
            public dynamic hash;

            public SkeleTonPOINTER_REDEFINESModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                level = @Self["level"];
                name = @Self["name"];
                hash = @Self["hash"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.DataDescription"), new Tuple<string, string>("Usage", "Pointer"), new Tuple<string, string>("isPointerIncrementation", "true") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonTYPEModel
        {
            public SkeleTonTYPEModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.DataDescription"), new Tuple<string, string>("type", "*") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonBOOL_SETModel
        {
            public dynamic receiver;

            public SkeleTonBOOL_SETModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                receiver = @Self["receiver"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.Set"), new Tuple<string, string>("sender.type", "BOOL"), new Tuple<string, string>("sender", "FALSE") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonPOINTER_INCREMENTModel
        {
            public dynamic displayableReceivers;
            public dynamic sender;
            public dynamic incrementDirection;
            public dynamic needCompute;

            public SkeleTonPOINTER_INCREMENTModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                displayableReceivers = @Self["displayableReceivers"];
                sender = @Self["sender"];
                incrementDirection = @Self["incrementDirection"];
                needCompute = @Self["needCompute"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.Set"), new Tuple<string, string>("receiverUsage", "Pointer") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonUNSAFEModel
        {
            public SkeleTonUNSAFEModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.CodeElements.VariableWriter"), new Tuple<string, string>("unsafe", "true") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonTCRFUN_LIBRARY_COPYModel
        {
            public dynamic copyname;

            public SkeleTonTCRFUN_LIBRARY_COPYModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                copyname = @Self["copyname"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.LibraryCopy") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonFUN_DECLARE_PUBLICModel
        {
            public dynamic definitions;
            public dynamic copyname;
            public dynamic programName8;

            public SkeleTonFUN_DECLARE_PUBLICModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                definitions = @Self["definitions"];
                copyname = @Self["copyname"];
                programName8 = @Self["programName8"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.FunctionDeclaration"), new Tuple<string, string>("visibility", "public") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonFUN_DECLARE_PRIVATEModel
        {
            public dynamic definitions;

            public SkeleTonFUN_DECLARE_PRIVATEModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                definitions = @Self["definitions"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.FunctionDeclaration"), new Tuple<string, string>("visibility", "private") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonFUN_CALLModel
        {
            public dynamic function;
            public dynamic receiver;

            public SkeleTonFUN_CALLModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                function = @Self["function"];
                receiver = @Self["receiver"];
            }
        }

        struct SkeleTonPROC_STYLE_CALL_TO_COBOLModel
        {
            public SkeleTonPROC_STYLE_CALL_TO_COBOLModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.Nodes.ProcedureStyleCall") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }

        struct SkeleTonQUALIFICATIONModel
        {
            public SkeleTonQUALIFICATIONModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
            }
        }

        struct SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel
        {
            public dynamic imports;

            public SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(TypeCobol.Compiler.Nodes.Node @Self)
            {
                imports = @Self["imports"];
            }
            private static Tuple<string, string>[] __ConditionsAttributes_0 = new Tuple<string, string>[] { new Tuple<string, string>("node", "TypeCobol.Compiler.CodeModel.Program") };
            public bool Conditions_0(TypeCobol.Compiler.Nodes.Node @Self)
            {
                return CheckConditions(@Self, __ConditionsAttributes_0);
            }

        }


        public List<TypeCobol.Codegen.Actions.Action> GetActions(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            if (@Self == null) return null;
            System.Type curType = @Self.GetType();
            while (curType != null && !NodeActionsProviderMap.ContainsKey(curType))
            {
                curType = curType.BaseType;
            }
            if (curType != null)
            {
                Func<TypeCobol.Compiler.Nodes.Node, TypeCobol.Codegen.GeneratorActions, List<TypeCobol.Codegen.Actions.Action>> provider = NodeActionsProviderMap[curType];
                return provider(@Self, @SelfContext);
            }
            return null;
        }
        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_TypeDefinition(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonTYPEDEFModel @Model = new SkeleTonTYPEDEFModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "comment", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_DataDescription(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonBOOL_DECLAREModel @Model = new SkeleTonBOOL_DECLAREModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var items = "";
                    if (@Model.value.Length == 0)
                    {
                        items = @Model.level + "  " + @Model.name + "-value PIC X VALUE LOW-VALUE" + @Model.global + ".";
                    }
                    else
                    {
                        items = @Model.level + "  " + @Model.name + "-value PIC X VALUE " + @Model.value + @Model.global + ".";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"
    88  "); @SelfResult.Append($@"{@Model.name}"); @SelfResult.Append(@"       VALUE 'T'.
    88  "); @SelfResult.Append($@"{@Model.name}"); @SelfResult.Append(@"-false VALUE 'F'
                    X'00' thru 'S'
                    'U' thru X'FF'.");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "replace", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPOINTER_REDEFINESModel @Model = new SkeleTonPOINTER_REDEFINESModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var items = "";
                    items = @Model.level + "  " + @Model.name + " POINTER.\n" +
                    @Model.level + " redefines " + @Model.name + ".\n" +
                    "    " + (Int32.Parse(@Model.level) + 1).ToString("00") + " " +
                    (@Model.name.Length.CompareTo(22) != 1 ? @Model.name : @Model.name.Substring(0, 22)) + @Model.hash + " pic S9(05) comp-5.";
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "replace", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonTYPEModel @Model = new SkeleTonTYPEModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "expand", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_Set(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonBOOL_SETModel @Model = new SkeleTonBOOL_SETModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"SET "); @SelfResult.Append($@"{@Model.receiver}"); @SelfResult.Append(@"-false TO TRUE");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, "BOOL.SET", @SelfResult.ToString(), "replace", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPOINTER_INCREMENTModel @Model = new SkeleTonPOINTER_INCREMENTModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var item = "";
                    if (@Model.needCompute)
                    {
                        foreach (var receiver in @Model.displayableReceivers)
                        {
                            item += "COMPUTE " + receiver + " = " + receiver + (@Model.incrementDirection == "Down" ? " - " : " + ") + @Model.sender.ToString() + ".\n";
                        }
                    }
                    else
                    {
                        item += "ADD " + (@Model.incrementDirection == "Down" ? "-" : "") + @Model.sender.ToString() + " to ";
                        foreach (var receiver in @Model.displayableReceivers)
                            item += receiver + ", ";
                    }
                    item = item.Remove(item.Length - 2);
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@item}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, "POINTER.INCREMENT", @SelfResult.ToString(), "replace", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_CodeElements_VariableWriter(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonUNSAFEModel @Model = new SkeleTonUNSAFEModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"UNSAFE");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "erase", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_LibraryCopy(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonTCRFUN_LIBRARY_COPYModel @Model = new SkeleTonTCRFUN_LIBRARY_COPYModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "comment", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonTCRFUN_LIBRARY_COPYModel @Model = new SkeleTonTCRFUN_LIBRARY_COPYModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"COPY "); @SelfResult.Append($@"{@Model.copyname}"); @SelfResult.Append(@" REPLACING ==:"); @SelfResult.Append($@"{@Model.copyname}"); @SelfResult.Append(@":== BY ==FCT==.");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_LIBRARY_COPY", "program.data-division.linkage", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonTCRFUN_LIBRARY_COPYModel @Model = new SkeleTonTCRFUN_LIBRARY_COPYModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"01  CallData.
        05  DescriptionId PIC X(08).
        88 CallIsCopy VALUE '"); @SelfResult.Append($@"{@Model.copyname}"); @SelfResult.Append(@"'.");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_CALL_MODE", "program.data-division.linkage", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_FunctionDeclaration(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonFUN_DECLARE_PUBLICModel @Model = new SkeleTonFUN_DECLARE_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"01  TC-"); @SelfResult.Append($@"{@Model.programName8}"); @SelfResult.Append(@"-FctList-Loaded PIC X(02).
    88 TC-"); @SelfResult.Append($@"{@Model.programName8}"); @SelfResult.Append(@"-FctList-IsLoaded      VALUE 'OK'.");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_IS_LOADED", "program.data-division.working-storage", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonFUN_DECLARE_PUBLICModel @Model = new SkeleTonFUN_DECLARE_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var items = "";
                    if (@Model.definitions.functions.Public.Count > 0)
                    {
                        items += "01 TC-" + @Model.programName8 + "-PntTab.\n";
                        items += "    05 TC-" + @Model.programName8 + "-PntNbr         PIC S9(04) COMP VALUE " + @Model.definitions.functions.Public.Count + ".\n";
                    }
                    foreach (var f in @Model.definitions.functions.Public)
                    {
                        items += "*" + @Model.programName8 + "::" + f.Name + '\n';
                        items += "    05 TC-" + @Model.programName8 + "-" + f.Hash + "-Idt   PIC X(08) VALUE '" + f.Hash + "'.\n";
                        items += "    05 TC-" + @Model.programName8 + "-" + f.Hash + " PROCEDURE-POINTER.\n";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_POINTER_ARRAY", "program.data-division.working-storage", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonFUN_DECLARE_PUBLICModel @Model = new SkeleTonFUN_DECLARE_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var items = "";
                    if (@Model.definitions.functions.Public.Count > 0)
                    {
                        items += "01 PntTab-Pnt POINTER.\n";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_POINTER_LINKAGE", "program.data-division.linkage", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonFUN_DECLARE_PUBLICModel @Model = new SkeleTonFUN_DECLARE_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "expand", null, "program.end", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonFUN_DECLARE_PUBLICModel @Model = new SkeleTonFUN_DECLARE_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"*
*    IF CallIsCopy
*      PERFORM Copy-Process-Mode
*    ELSE
    PERFORM FctList-Process-Mode
    perform INIT-LIBRARY
*    END-IF

    GOBACK.
");
                    var entries = "";
                    int c = 0;
                    foreach (var f in @Model.definitions.functions.Public)
                    {
                        entries += "       SET TC-" + @Model.programName8 + "-" + f.Hash + "   TO ENTRY \'" + f.Hash + "\'\n";
                    }
                    @SelfResult.Append(@"
 FctList-Process-Mode.
     IF NOT TC-"); @SelfResult.Append($@"{@Model.programName8}"); @SelfResult.Append(@"-FctList-IsLoaded
"); @SelfResult.Append($@"{@entries}"); @SelfResult.Append(@"
       SET TC-"); @SelfResult.Append($@"{@Model.programName8}"); @SelfResult.Append(@"-FctList-IsLoaded TO TRUE
     END-IF
        .
");
                    var items = "";
                    if (@Model.definitions.functions.Public.Count > 0)
                    {
                        items += "     set PntTab-Pnt TO ADDRESS OF TC-" + @Model.programName8 + "-PntTab\n";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, "ProcedureDivisionCalleeWithoutExternal", @SelfResult.ToString(), "create", "TCRFUN_CODEGEN_ADAPTABLE_BEHAVIOUR", "program.procedure-division.sentence-([0-9]+).begin", null, true);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonFUN_DECLARE_PRIVATEModel @Model = new SkeleTonFUN_DECLARE_PRIVATEModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "expand", null, "program.end", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_Nodes_ProcedureStyleCall(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonPROC_STYLE_CALL_TO_COBOLModel @Model = new SkeleTonPROC_STYLE_CALL_TO_COBOLModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "expand", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

        public static List<TypeCobol.Codegen.Actions.Action> TypeCobol_Compiler_CodeModel_Program(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext)
        {
            List<TypeCobol.Codegen.Actions.Action> @SelfActions = new List<TypeCobol.Codegen.Actions.Action>();
            {
                SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel @Model = new SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(@Self);
                if ((@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "remarks", null, "NODE", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel @Model = new SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(@Self);
                if (@Model.imports.IsNotEmpty && (@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var items = "";
                    foreach (var pgm in @Model.imports.Programs.Values)
                    {
                        items += "01 TC-" + pgm.Name + " pic X(08) value '" + pgm.Name.ToUpperInvariant() + "'.\n";
                    }
                    if (@Model.imports.HasPublicProcedures)
                    {
                        items += "01 TC-Call          PIC X VALUE 'T'.\n";
                        items += "    88 TC-FirstCall  VALUE 'T'.\n";
                        items += "    88 TC-NthCall    VALUE 'F'\n";
                        items += "                     X'00' thru 'S'\n";
                        items += "                     'U' thru X'FF'.\n";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", null, "program.data-division.working-storage.begin", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel @Model = new SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(@Self);
                if (@Model.imports.IsNotEmpty && (@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"*Common to all librairies used by the program.
01 TC-Library-PntTab.
   05 TC-Library-PntNbr          PIC S9(04) COMP.
   05 TC-Library-Item OCCURS 1000
                        DEPENDING ON TC-Library-PntNbr
                        INDEXED   BY TC-Library-Idx.
       10 TC-Library-Item-Idt      PIC X(08).
       10 TC-Library-Item-Pnt      PROCEDURE-POINTER.
");
                    var items = "";
                    foreach (var pgm in @Model.imports.Programs.Values)
                    {
                        foreach (var proc in pgm.Procedures.Values)
                        {
                            proc.IsNotByExternalPointer = true;
                            items += "*" + pgm.Name + "::" + proc.Name + '\n';
                            items += "01 TC-" + pgm.Name + "-" + proc.Hash + "-Item.\n";
                            items += "   05 TC-" + pgm.Name + "-" + proc.Hash + "-Idt PIC X(08).\n";
                            items += "   05 TC-" + pgm.Name + "-" + proc.Hash + " PROCEDURE-POINTER.\n";
                        }
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append($@"{@items}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", null, "program.data-division.linkage.begin", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel @Model = new SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(@Self);
                if (@Model.imports.HasPublicProcedures && (@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var clause = "";
                    if (@Model.imports.HasPublicProcedures)
                    {
                        clause += "*\n";
                        clause += "    PERFORM TC-INITIALIZATIONS\n";
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@clause}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", null, "program.procedure-division.declaratives-header.end,//program.procedure-division.sentence-([0-9]+).begin,//program.procedure-division.paragraph.sentence-([0-9]+).begin|program.procedure-division.sentence-([0-9]+).begin|program.procedure-division.paragraph.sentence-([0-9]+).begin|program.procedure-division.sentence-0.begin", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            {
                SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel @Model = new SkeleTonPROGRAM_IMPORT_FUN_PUBLICModel(@Self);
                if (@Model.imports.IsNotEmpty && (@Model.Conditions_0(@Self)))
                {
                    StringBuilder @SelfResult = new StringBuilder();
                    @SelfResult.Append(@"");
                    var clause = "";
                    if (@Model.imports.HasPublicProcedures)
                    {
                        clause += "*=================================================================\n";
                        clause += "TC-INITIALIZATIONS.\n";
                        clause += "*=================================================================\n";
                        clause += "     IF TC-FirstCall\n";
                        clause += "          SET TC-NthCall TO TRUE\n";
                        foreach (var pgm in @Model.imports.Programs.Values)
                        {
                            foreach (var proc in pgm.Procedures.Values)
                            {
                                clause += "          SET ADDRESS OF TC-" + pgm.Name + "-" + proc.Hash + "-Item  TO NULL\n";
                            }
                        }
                        clause += "     END-IF\n";
                        clause += "     .\n";
                    }
                    if (!@Model.imports.IsEmpty)
                    {
                        foreach (var pgm in @Model.imports.Programs.Values)
                        {
                            clause += "*=================================================================\n";
                            clause += " TC-LOAD-POINTERS-" + pgm.Name + ".\n";
                            clause += "*=================================================================\n";
                            clause += "     CALL 'ZCALLPGM' USING TC-" + pgm.Name + "\n";
                            clause += "     ADDRESS OF TC-Library-PntTab\n";
                            clause += "     PERFORM VARYING TC-Library-Idx FROM 1 BY 1\n";
                            clause += "     UNTIL TC-Library-Idx > TC-Library-PntNbr\n";
                            clause += "         EVALUATE TC-Library-Item-Idt (TC-Library-Idx)\n";
                            foreach (var proc in pgm.Procedures.Values)
                            {
                                clause += "         WHEN '" + proc.Hash + "'\n";
                                clause += "              SET ADDRESS OF\n";
                                clause += "              TC-" + pgm.Name + "-" + proc.Hash + "-Item\n";
                                clause += "              TO ADDRESS OF\n";
                                clause += "              TC-Library-Item(TC-Library-Idx)\n";
                            }
                            clause += "         WHEN OTHER\n";
                            clause += "              CONTINUE\n";
                            clause += "         END-EVALUATE\n";
                            clause += "     END-PERFORM\n";
                            clause += "     .\n";
                        }
                    }
                    @SelfResult.Append(@"
"); @SelfResult.Append(@"        "); @SelfResult.Append($@"{@clause}"); @SelfResult.Append(@"");
                    TypeCobol.Codegen.Actions.Action @SelfAction = @SelfContext.CreateAction(@Self, null, @SelfResult.ToString(), "create", null, "program.procedure-division.end", null, false);
                    if (@SelfAction != null)
                    {
                        @SelfActions.Add(@SelfAction);
                    }
                }
            }
            return @SelfActions;
        }

    }
}
