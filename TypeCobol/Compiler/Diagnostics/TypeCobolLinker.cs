using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
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
            var types = dataEntry.SymbolTable.GetType(new URI(dataEntry.DataType.Name));
            if (types != null && types.Count != 1) return;

            //Add this dataEntry to type references
            var typeToUpdate = types.FirstOrDefault();
            if (typeToUpdate == null) return;

            typeToUpdate.References.Add(dataEntry);
        }
      
    }
}
