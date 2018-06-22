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
            return base.Visit(dataEntry);
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
            if (types.Count != 1) return;
            var type = types.First();


            var circularRefInsideChildren = type.Children.Any(c =>
            {
                var dataChild = c as DataDescription;
                if (dataChild == null) return false;
                var childrenType = symbolTable.GetType(dataChild.DataType).FirstOrDefault();
                if (childrenType == null) return false;
                return dataEntry.GetParentTypeDefinition == childrenType; //Circular reference detected will return true
            });
            if (type == dataEntry.GetParentTypeDefinition || circularRefInsideChildren) 
            {
                DiagnosticUtils.AddError(dataEntry, "Type circular reference detected", MessageCode.SemanticTCErrorInParser);
                return; //Do not continue to prevent further work/crash with circular references
            }

            if ((dataEntry.CodeElement as DataDescriptionEntry).IsGlobal)
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

            dataEntry.TypeDefinition = type; //Set the TypeDefinition on DataDefinition Node so to avoid symbolTable access

            //Also add all the typedChildren to reference list
            foreach (var dataDescTypeChild in type.Children.Where(c => c is DataDescription))
            {
                TypeReferencer(dataDescTypeChild as DataDescription, symbolTable);
            }
        }
      
    }
}
