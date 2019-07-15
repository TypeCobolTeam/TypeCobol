using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// TypeCobolLinker is responsible for:
    /// - Resolve TypeDefinition.
    ///    - It's the only class that can call SymbolTable.GetType() and set the property TypeDefinition on DataDefinition node
    /// - Check circular reference between types
    /// - Create a path between type reference and TypeDefinition in the SymbolTable (property SymbolTable.TypesReferences)
    ///    - It's the only class that can update SymbolTable.TypesReferences
    ///    - This path is not set for unused types
    ///
    ///
    /// What is the "path" between type ? (SymbolTable.TypesReferences)
    /// ----------------------------------------------------------
    /// The path between type is used to search variables which are located under a typedef
    /// and then find if a variable outside reference this type (directly or indirectly).
    /// The path will link a DataDefinition that use the syntax (type XXXX) to a TypeDefinition.
    ///
    ///
    /// Group1                  TypeA               TypeB                 TypeC
    ///   var1                    A11 type typeB       B1                    C1
    ///    var11 type typeA                             B2                    C2
    ///                                                  B3 type typeC         C3
    /// 
    ///
    /// Eg, with this SymbolReference:
    /// var11::A11::B3::C3
    /// 
    /// We'll first find C3 under TypeC, then we need to know all DataDefinition that references TypeC.
    /// So the path must start with: TypeC->B3
    /// Then again, after B3 is found under TypeB, we need to find DataDefinition that references TypeB.
    /// The path must continue with: TypeB->A11
    /// ...
    /// At the end, the full path path will be: TypeC->B3, TypeB->A11, TypeA->var11
    /// As var11 is a variable outside a TypeDef, the path ends here.
    ///
    /// With the same SymbolReference but fully qualified:
    /// var11::A11::B1::B2::B3::C1::C2::C3
    /// The path will be exactly the same.
    /// The path doesn't contains intermediate group-item.
    ///
    ///
    /// Now when there are more Data/Definition/TypeDef
    /// 
    ///   Group1                  TypeA               TypeB                 TypeC
    ///     var1                    A11 type typeB       B1                    C1
    ///      var11 type typeA                             B2                    C2
    ///                                                    B3 type typeC         C3
    ///   
    ///    Group2                  TypeZ
    ///      var22                    Z11 type typeB
    ///       var222 type typeZ     
    ///
    /// With a SymbolReference with Group2 it's the same logic.
    /// var222::Z11::B3::C3
    /// The path will be:
    /// TypeC->B3, TypeB->Z11, TypeZ->var222
    /// 
    /// 
    /// 
    /// How the path is calculated/stored ? (SymbolTable.TypesReferences)
    /// ------------------------------------------------------------------
    /// The path will always contains a DataDefinition outside a Type.
    /// So we use the SymbolTable of this DataDefinition to store the full path.
    ///
    /// In the previous example, if Group1 and Group2 are in 2 different SymbolTable, it means the full path will be stored twice.
    /// It takes more time to construct the path but it ensure that when we use a path we'll always be sure to find variables accessible/visible to our SymbolTable.
    ///
    /// 
    /// If all paths were common to all SymbolTable, then if Group1 and Group2 are in 2 different programs, there would be 2 downsides:
    ///  - We'll spend time using path that takes us outside our visibility scope.
    ///    - It's not a real problem on our unit test, but on big programs with a lot of dependencies this would be an issue.
    ///  - At the end we need to check if the variable is accessible to our scope
    ///
    ///
    ///
    /// Optimization on path storage (SymbolTable.TypesReferences)
    /// ------------------------------------------------------------
    /// When 2 or more variable inside a typedef reference the same type, TypeCobolLinker will detect that and only store the path once.
    /// 
    /// 
    /// </summary>
    public class TypeCobolLinker 
    {


        /// <summary>
        /// This method will do the 3 actions:
        ///   - Resolve TypeDefinition
        ///   - Check circular references
        ///   - Create a path between type reference and TypeDefinition
        /// </summary>
        /// <param name="typedVariablesOutsideTypedef">Variable outside that use the "type" syntax</param>
        /// <param name="typeThatNeedTypeLinking">Typedef that need all its typed children to be resolved (only use case for now is "Depending on")</param>
        public static void LinkedTypedVariables([NotNull][ItemNotNull] in List<DataDefinition> typedVariablesOutsideTypedef, 
            [NotNull][ItemNotNull] in List<TypeDefinition> typeThatNeedTypeLinking)
        {
            //Stack to detect circular reference between types
            Stack<DataDefinition> currentlyCheckedTypedefStack = new Stack<DataDefinition>();

            foreach (var dataDefinition in typedVariablesOutsideTypedef)
            {
                //Warning, when you ask to parse multiple input files with CLI, then TypeDefinition of typedVariablesOutsideTypedef can already be resolved


                if (ResolveType(dataDefinition)) //If type has been found in SymbolTable
                {
                    //Reference the path between the typedDataDefChild and its TypeDefinition on the SymbolTable on the original DataDefinition outside a typedef
                    if (dataDefinition.SymbolTable.TypesReferences.TryGetValue(dataDefinition.TypeDefinition, out var dataDefsThatReferencedThisType))
                    {
                        //Link between the type and the dataDefinition cannot already be done
                        System.Diagnostics.Debug.Assert(!dataDefsThatReferencedThisType.Contains(dataDefinition));

                        //Type already referenced in our SymbolTable, it means all further typed children of the type have already been linked into this SymbolTable 
                        dataDefsThatReferencedThisType.Add(dataDefinition);
                    }
                    else
                    {
                        dataDefinition.SymbolTable.TypesReferences.Add(dataDefinition.TypeDefinition, new List<DataDefinition> { dataDefinition });

                        //First time this TypeDefinition is added to dataDefinition.SymbolTable, then link all children of the type
                        LinkTypedChildren(dataDefinition.TypeDefinition, currentlyCheckedTypedefStack, dataDefinition.SymbolTable);
                    }
                }
            }


            //Now link type that use depending On
            System.Diagnostics.Debug.Assert(currentlyCheckedTypedefStack.Count == 0, "Stack must be empty");
            foreach (var typeDefinition in typeThatNeedTypeLinking)
            {
                LinkTypedChildren(typeDefinition, currentlyCheckedTypedefStack, typeDefinition.SymbolTable);
            }
        }


        public static void CheckCircularReferences([NotNull] TypeDefinition typeDefinition)
        {
                                 
            if (typeDefinition.TypedChildren.Count == 0                     //no typed children     
                || typeDefinition.TypedChildren[0] == null                  //TypeDefinition of first children could not be resolved
                || typeDefinition.TypedChildren[0].TypeDefinition != null)  //TypeDefinition of first children already resolved
            {
                //In these case, nothing to do because no children or job has already be done
                return;
            }

            //Stack to detect circular reference between types
            Stack<DataDefinition> currentlyCheckedTypedefStack = new Stack<DataDefinition>();
            LinkTypedChildren(typeDefinition, currentlyCheckedTypedefStack);
        }

        /// <summary>
        /// This method will do the 3 actions:
        ///   - Resolve TypeDefinition
        ///   - Check circular references
        ///   - Create a path between type reference and TypeDefinition
        /// 
        /// </summary>
        /// <param name="currentlyCheckedTypedefStack"></param>
        /// <param name="symbolTable"></param>
        /// <param name="typeDefinition"></param>
        private static void LinkTypedChildren([NotNull] TypeDefinition typeDefinition,
            [CanBeNull] Stack<DataDefinition> currentlyCheckedTypedefStack0, [CanBeNull] SymbolTable symbolTable = null)
        {

            if (typeDefinition.TypedChildren.Count == 0)
            {
                return;
            }
            currentlyCheckedTypedefStack0?.Push(typeDefinition);
            LinkTypedChildren0(currentlyCheckedTypedefStack0);
            currentlyCheckedTypedefStack0?.Pop();



            //Only reason to use private method here, is because there are multiple return path in LinkTypedChildren0.
            //So we can easily handle currentlyCheckedTypedefStack push/pop just above
            void LinkTypedChildren0(Stack<DataDefinition> currentlyCheckedTypedefStack)
            {
                //If all typed children of typedef are resolved it means this typedef has already been fully linked
                if (typeDefinition.TypedChildren[typeDefinition.TypedChildren.Count - 1] == null || typeDefinition.TypedChildren[typeDefinition.TypedChildren.Count - 1]?.TypeDefinition != null)
                {
                    //If symbolTable is null, it means we don't want to register link between TypeDefinition and DataDefinition that use it
                    //So we can stop here.
                    if (symbolTable == null)
                    {
                        return;
                    }
                    
                    currentlyCheckedTypedefStack = null; //As typedef has already been linked, then no need to check circular reference
                }


                for (var i = 0; i < typeDefinition.TypedChildren.Count; i++)
                {
                    var typedDataDefChild = typeDefinition.TypedChildren[i];
                    //If a typedDataDefChild is null it means is TypeDefinition could not be resolved or is part of a circular reference.
                    //So let's continue to the next child.
                    if (typedDataDefChild == null)
                    {
                        continue;
                    }


                    //Resolve type of typedDataDefChild if not already done yet
                    if (typedDataDefChild.TypeDefinition == null)
                    {
                        if (!ResolveType(typedDataDefChild)) //Use the symbolTable of this typedDataDefChild to resolve the type
                        {
                            //No TypeDefinition found
                            typeDefinition.TypedChildren[i] = null; //set to null so we don't try to check its TypeDefinition another time.
                            continue;                               //Go to the next typed child
                        }
                    }


                    System.Diagnostics.Debug.Assert(typedDataDefChild.TypeDefinition != null); //type must be resolved now


                    //Detect circular reference
                    if (currentlyCheckedTypedefStack?.Contains(typedDataDefChild.TypeDefinition) == true)
                    {
                        typeDefinition.TypedChildren[i] = null; //set this typed child to null so we don't enter this infinite loop another time
                        DiagnosticUtils.AddError(typedDataDefChild, "Type circular reference detected : "
                                                                    + string.Join(" -> ", currentlyCheckedTypedefStack.Select(t => t.Name)), code: MessageCode.SemanticTCErrorInParser);

                        continue;//Go to the next typedChildren
                                 //Do not make the link in symbolTable.TypesReferences with method ReferenceThisDataDefByThisType to avoid infinite loop in SymbolTable
                    }


                    if (symbolTable != null)//If we need to keep references between typed variable and type
                    {
                        //Reference the path between the typedDataDefChild and its TypeDefinition on the SymbolTable on the original DataDefinition outside a typedef
                        if (!ReferenceThisDataDefByThisType(symbolTable, typedDataDefChild))
                        {
                            continue; //Type was already linked, so stop here
                        }
                    }


                    //Continue to link children of typedDataDefChild.TypeDefinition
                    LinkTypedChildren(typedDataDefChild.TypeDefinition, currentlyCheckedTypedefStack, symbolTable);
                }
            }
        }

        /// <summary>
        /// Lookup the TypeDefinition from the DataType of this dataDefinition.
        /// If no TypeDefinition can be found, then property TypeDefinition stay null.
        /// </summary>
        /// <param name="dataDefinition"></param>
        /// <returns>true if type has been resolved</returns>
        public static bool ResolveType([NotNull] in DataDefinition dataDefinition)
        {
            //Note : dataDefinition.CodeElement cannot be null, only Index have a null CodeElement and Index cannot be typed


            //Special hack until Visibility are fixed (#1081 and #938)
            //As "Global" and "GlobalStorage" Scopes are above "Declarations" they cannot have access to "Declarations" scope.
            //So types in "Declarations" scope cannot be reached from the SymbolTable of a variable declared as global or a variable inside global-storage.

            //But a variable NOT global and not in global-storage can access the SymbolTable "Declarations" and "Global".
            //So if we are in scopes "Global" or "GlobalStorage" we first need to retrieve a SymbolTable under "Declarations" and resolve the type using this SymbolTable.
            List<TypeDefinition> types;
            SymbolTable declarationsSymbolTable;
            if (dataDefinition.SymbolTable.CurrentScope <= SymbolTable.Scope.Global)
            {
                //Retrieve the Scope Declarations by retrieving the SymbolTable of the program which is of Scope "Program".
                //Then use the EnclosingScope which is of scope "Declarations"
                declarationsSymbolTable = dataDefinition.GetProgramNode().SymbolTable.EnclosingScope;
            }
            else
            {
                declarationsSymbolTable = dataDefinition.SymbolTable;
            }
            System.Diagnostics.Debug.Assert(declarationsSymbolTable.CurrentScope >= SymbolTable.Scope.Declarations, "Scope of SymbolTable must be under Declarations until (#1081 and #938) are fixed");

            types = declarationsSymbolTable.GetType(dataDefinition.CodeElement.DataType);
            //End special hack

            //When (#1081 and #938) are fixed, remove the hack above and simply use the following line: 
            //var types = dataDefinition.SymbolTable.GetType(dataDefinition.CodeElement.DataType);



            if (types.Count < 1)
            {
                string message = "TYPE \'" + dataDefinition.CodeElement.DataType + "\' is not referenced";
                DiagnosticUtils.AddError(dataDefinition, message, MessageCode.SemanticTCErrorInParser);
            }
            else if (types.Count > 1)
            {
                string message = "Ambiguous reference to TYPE \'" + dataDefinition.CodeElement.DataType + "\'";
                DiagnosticUtils.AddError(dataDefinition, message, MessageCode.SemanticTCErrorInParser);
            }
            else
            {
                dataDefinition.TypeDefinition = types[0];
                dataDefinition.DataType.RestrictionLevel = types[0].DataType.RestrictionLevel;
                return true;
            }

            return false;
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="symbolTable"></param>
        /// <param name="dataDefinition">Data definition that reference a Type.</param>
        /// <returns>true if reference has been set. False is reference was already made</returns>
        private static bool ReferenceThisDataDefByThisType([NotNull] in SymbolTable symbolTable, [NotNull] in DataDefinition dataDefinition)
        {
            //Reminder on symbolTable.TypesReferences Dictionary
            //Key is TypeDefinition, Value is List<DataDefinition>
            //
            //TypesReferences can be understood/read as:
            //TypeDefinition is referenced by these DataDefinitions


            //Do NOT use the SymbolTable of the DataDefinition because we are linking all types references to the first
            //dataDefinition outside typedef (see explanation at top of the class).
            if (symbolTable.TypesReferences.TryGetValue(dataDefinition.TypeDefinition, out var dataDefsThatReferencedThisType))
            {
                if (dataDefsThatReferencedThisType.Contains(dataDefinition))
                {
                    return false;
                }
                dataDefsThatReferencedThisType.Add(dataDefinition);
            }
            else
            {
                //Same here, don't use the SymbolTable of the DataDefinition
                symbolTable.TypesReferences.Add(dataDefinition.TypeDefinition, new List<DataDefinition> { dataDefinition });
            }

            return true;
        }
    }
}
