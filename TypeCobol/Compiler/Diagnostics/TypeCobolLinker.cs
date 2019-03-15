using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Diagnostics
{
    public class TypeCobolLinker : AbstractAstVisitor
    {
        public override bool BeginNode(Node node)
        {
            return base.BeginNode(node);
        }
        public override bool BeginCodeElement(CodeElement codeElement)
        {
            return false; //We don't want to check deeper than Node
        }

        public override bool Visit(DataDescription dataEntry)
        {
            TypeReferencer(dataEntry, dataEntry.SymbolTable);
            return false; //Visit of children is done by TypeReferencer
        }

        public override bool Visit(Paragraph paragraph)
        {
            return false;
        }

        public override bool Visit(Section section)
        {
            return false;
        }

        public override bool Visit(EnvironmentDivision environmentDivision)
        {
            return false;
        }

        public override bool IsStopVisitingChildren => false;

        /// <summary>
        /// Do not visit children of IF.
        /// Just in case there are statements directly under the procedure division.
        /// </summary>
        public override bool Visit(If _) => false;

        /// <summary>
        /// Do not visit children of Evaluate.
        /// Just in case there are statements directly under the procedure division.
        /// </summary>
        public override bool Visit(Evaluate _) => false;

        /// <summary>
        /// Do not visit children of Perform.
        /// Just in case there are statements directly under the procedure division.
        /// </summary>
        public override bool Visit(Perform _) => false;

        public override bool Visit(Declaratives declaratives)
        {
            return false;
        }

        public override bool Visit(ProcedureDivision procedureDivision)
        {
            var parent = procedureDivision.Parent as Program;
            return parent != null && base.Visit(procedureDivision);
        }

        public override bool Visit(FunctionDeclaration funcDeclare)
        {
            //Get Function declaration parameters
            var parameters = funcDeclare.Profile.Parameters;
            if(funcDeclare.Profile.ReturningParameter != null)
                parameters.Add(funcDeclare.Profile.ReturningParameter);

            foreach (var dataEntry in parameters)
            {
                TypeReferencer(dataEntry, dataEntry.SymbolTable);
            }

            return base.Visit(funcDeclare);
        }

        private void TypeReferencer(DataDescription dataEntry, SymbolTable symbolTable)
        {
            if (symbolTable == null) return;
            var types = symbolTable.GetType(dataEntry.DataType);
            if (types.Count != 1)
            {
                //Check the existing children, if they use a type
                foreach (var child in dataEntry.Children) 
                {
                    if (child is DataDescription childDataDesc)
                        TypeReferencer(childDataDesc, symbolTable);
                }
                return;
            }
            var type = types.First();
            dataEntry.TypeDefinition = type; //Set the TypeDefinition on DataDefinition Node so to avoid symbolTable access

            //Check circular type reference
            if (dataEntry.IsPartOfATypeDef)
            {
                var circularRefInsideChildren = type.Children.Any(c =>
                {
                    DataDefinition dataChild = (DataDefinition)c;
                    var childrenType = symbolTable.GetType(dataChild.DataType).FirstOrDefault();
                    if (childrenType == null) return false;
                    return dataEntry.ParentTypeDefinition == childrenType; //Circular reference detected will return true
                });


                if (type == dataEntry.ParentTypeDefinition || circularRefInsideChildren)
                {
                    DiagnosticUtils.AddError(dataEntry, "Type circular reference detected",
                        dataEntry.CodeElement, code: MessageCode.SemanticTCErrorInParser);
                    return; //Do not continue to prevent further work/crash with circular references
                }
            }

            if (dataEntry.CodeElement.IsGlobal)
                symbolTable = symbolTable.GetTableFromScope(SymbolTable.Scope.Global);

            
            if (symbolTable.TypesReferences.ContainsKey(type)) //If datatype already exists, add ref to the list
            {
                if (!symbolTable.TypesReferences[type].Contains(dataEntry))
                    symbolTable.TypesReferences[type].Add(dataEntry);
            }
            else
            {
                symbolTable.TypesReferences.Add(type, new List<DataDefinition> {dataEntry});
            }

            //Also add all the typedChildren to reference list
            foreach (var dataDescTypeChild in type.Children.Where(c => c is DataDescription))
            {
                TypeReferencer(dataDescTypeChild as DataDescription, symbolTable);
            }
        }
      
    }
}
