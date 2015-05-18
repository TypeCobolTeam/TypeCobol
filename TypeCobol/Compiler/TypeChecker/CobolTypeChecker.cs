using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.TypeChecker
{
    /// <summary>
    /// Builds a code model and check all semantic rules by walking the code elements
    /// </summary>
    public class CobolTypeChecker
    {
        /// <summary>
        /// Code model for a SourceProgram
        /// </summary>
        public SourceProgram Program { get; private set; }

        /// <summary>
        /// List of errors found by checking the program
        /// </summary>
        public IList<TypeDiagnostic> Diagnostics { get; private set; }

        public CobolTypeChecker()
        {
            Program = new SourceProgram();
            Diagnostics = new List<TypeDiagnostic>();
        }

        /*

        // --- Currently implement only data item type checks ---

        private DataItem currentDataItem;

        public override void EnterDataDescriptionEntry(CobolParser.DataDescriptionEntryContext context)
        {
            if (context.dataName() != null)
            {
                currentDataItem = new DataItem()
                {
                    Level = Int32.Parse(context.levelNumber().IntegerLiteral().GetText()),
                    Name = context.dataName().UserDefinedWord().GetText()
                };
            }
        }

        public override void EnterPictureClause(CobolParser.PictureClauseContext context)
        {
            if(currentDataItem != null)
            {
                currentDataItem.PictureString = context.PictureCharacterString().GetText();
            }
        }

        /// <summary>
        /// TypeCobol extension
        /// </summary>
        public override void EnterTypeCobolExt_typeClause(CobolParser.TypeCobolExt_typeClauseContext context)
        {
            if (currentDataItem != null)
            {
                currentDataItem.Type = ((AlphanumericLiteralValue)((Token)context.AlphanumericLiteral().Symbol).LiteralValue).Text;
            }
        }

        public override void ExitDataDescriptionEntry(CobolParser.DataDescriptionEntryContext context)
        {
            if (currentDataItem != null)
            {
                Program.DataItems.Add(currentDataItem.Name, currentDataItem);
                currentDataItem = null;
            }
        }

        public override void EnterMoveStatement(CobolParser.MoveStatementContext context)
        {
            try
            {
                string dataItemName1 = context.identifier(0).qualifiedDataName().dataName(0).UserDefinedWord().GetText();
                string dataItemName2 = context.identifier(1).qualifiedDataName().dataName(0).UserDefinedWord().GetText();

                if (!Program.DataItems.ContainsKey(dataItemName1))
                {
                    string message = "Data item name " + dataItemName1 + " was not declared";
                    TypeDiagnostic diagnostic = new TypeDiagnostic() { StartToken = context.identifier(0).Start, EndToken = context.identifier(0).Stop, Message = message };
                    Diagnostics.Add(diagnostic);
                    return;
                }
                DataItem dataItem1 = Program.DataItems[dataItemName1];
                if (!Program.DataItems.ContainsKey(dataItemName2))
                {
                    string message = "Data item name " + dataItemName2 + " was not declared";
                    TypeDiagnostic diagnostic = new TypeDiagnostic() { StartToken = context.identifier(1).Start, EndToken = context.identifier(1).Stop, Message = message };
                    Diagnostics.Add(diagnostic);
                    return;
                }
                DataItem dataItem2 = Program.DataItems[dataItemName2];

                // TypeCobol extension
                if (dataItem1.Type != null && dataItem2.Type != null && dataItem1.Type != dataItem2.Type)
                {
                    string message = "Data item " + dataItemName2 + " of type " + dataItem2.Type + " can not be moved into data item " + dataItemName1 + " of type " + dataItem1.Type;
                    TypeDiagnostic diagnostic = new TypeDiagnostic() { StartToken = context.Start, EndToken = context.Stop, Message = message };
                    Diagnostics.Add(diagnostic);
                }
            }
            catch (Exception e) { } // Temporary : for the demo not all kind of identifiers are supported
        }*/
    } 

    /// <summary>
    /// Temporary diagnostic class for the demo
    /// </summary>
    public class TypeDiagnostic
    {
        public IToken StartToken { get; set; }
        public IToken EndToken { get; set; }

        public string Message { get; set; }
    }
}
