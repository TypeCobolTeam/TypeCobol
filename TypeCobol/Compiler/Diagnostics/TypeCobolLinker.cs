using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;

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
            TypeReferencer(dataEntry);
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
                TypeReferencer(dataEntry);
            }

            return base.Visit(funcDeclare);
        }

        private void TypeReferencer(DataDescription dataEntry)
        {
            //Check SymbolTable for dataEntry DataType.Name
            var symbolTable = dataEntry.SymbolTable;
            var types = symbolTable.GetType(dataEntry.DataType);
            if (types.Count != 1) return;

            var type = types.First();
            
            if (symbolTable.TypesReferences.ContainsKey(type)) //If datatype already exists, add ref to the list
            {
                symbolTable.TypesReferences[type].Add(dataEntry);
            }
            else
            {
                symbolTable.TypesReferences.Add(type, new List<DataDefinition> {dataEntry});
            }
        }
      
    }
}
