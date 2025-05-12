using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

using Range = TypeCobol.LanguageServer.VsCodeProtocol.Range;

namespace TypeCobol.LanguageServer.Processor
{
    public class CompletionProcessor
    {
        private readonly SignatureCompletionContext _signatureCompletionContext;

        public CompletionProcessor(SignatureCompletionContext signatureCompletionContext)
        {
            _signatureCompletionContext = signatureCompletionContext;
        }

        public List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, Position position)
        {
            List<CompletionItem> items;

            var wrappedCodeElements = TypeCobolServer.CodeElementFinder(compilationUnit, position);
            if (wrappedCodeElements == null)
                return null;

            //Try to get a significant token for completion and return the codeelement containing the matching token.
            CodeElement matchingCodeElement = CodeElementMatcher.MatchCompletionCodeElement(position,
                wrappedCodeElements,
                out var userFilterToken, out var lastSignificantToken); //Magic happens here

            if (lastSignificantToken != null)
            {
                switch (lastSignificantToken.TokenType)
                /*
                 * WARNING: when adding completion support for a new keyword, do not forget
                 * to reference the new keyword in CodeElementMatcher static class!
                 */
                {
                    case TokenType.PERFORM:
                        items = new CompletionAfterPerform(userFilterToken).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.CALL:
                        _signatureCompletionContext.Candidates.Clear(); //Clear to avoid key collision
                        items = new CompletionForProcedure(userFilterToken, _signatureCompletionContext.Candidates).ComputeProposals(compilationUnit, matchingCodeElement);
                        items.AddRange(new CompletionForLibrary(userFilterToken).ComputeProposals(compilationUnit, matchingCodeElement));
                        break;
                    case TokenType.TYPE:
                        items = new CompletionForType(userFilterToken).ComputeProposals(compilationUnit, matchingCodeElement);
                        items.AddRange(new CompletionForLibrary(userFilterToken).ComputeProposals(compilationUnit, matchingCodeElement));
                        break;
                    case TokenType.QualifiedNameSeparator:
                        items = new CompletionForQualifiedName(userFilterToken, lastSignificantToken, position, _signatureCompletionContext.Candidates).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.INPUT:
                    case TokenType.OUTPUT:
                    case TokenType.IN_OUT:
                        items = new CompletionForProcedureParameter(userFilterToken, lastSignificantToken, position, _signatureCompletionContext.BestMatch).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.DISPLAY:
                        Predicate<DataDefinition> excludeNonDisplayable = dataDefinition =>
                            dataDefinition.Usage != DataUsage.ProcedurePointer // invalid usages in DISPLAY statement
                            && dataDefinition.Usage != DataUsage.FunctionPointer
                            && dataDefinition.Usage != DataUsage.ObjectReference
                            && dataDefinition.Usage != DataUsage.Index
                            && dataDefinition.CodeElement?.LevelNumber != null
                            && dataDefinition.CodeElement.LevelNumber.Value < 88;
                        // Ignore level 88. Note that dataDefinition.CodeElement != null condition also filters out IndexDefinition which is invalid in the context of DISPLAY
                        // Filtering dataDefinition without LevelNumber also excludes FileDescription which are invalid for a DISPLAY
                        items = new CompletionForVariable(userFilterToken, excludeNonDisplayable).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.MOVE:
                        Predicate<DataDefinition> excludeLevel88 = dataDefinition =>
                            (dataDefinition.CodeElement?.LevelNumber != null && dataDefinition.CodeElement.LevelNumber.Value < 88)
                            ||
                            (dataDefinition.CodeElement == null && dataDefinition is IndexDefinition);
                        //Ignore 88 level variable
                        items = new CompletionForVariable(userFilterToken, excludeLevel88).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.TO:
                        items = new CompletionForTo(userFilterToken, lastSignificantToken).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.INTO:
                        Predicate<DataDefinition> onlyAlpha = dataDefinition => dataDefinition.CodeElement != null &&
                                                                   (dataDefinition.DataType == DataType.Alphabetic ||
                                                                    dataDefinition.DataType == DataType.Alphanumeric ||
                                                                    dataDefinition.DataType == DataType.AlphanumericEdited);
                        items = new CompletionForVariable(userFilterToken, onlyAlpha).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.SET:
                        Predicate<DataDefinition> keepCompatibleTypes = dataDefinition => dataDefinition.CodeElement?.Type == CodeElementType.DataConditionEntry //Level 88 Variable
                                                                           || dataDefinition.DataType == DataType.Numeric //Numeric Integer Variable
                                                                           || dataDefinition.Usage == DataUsage.Pointer
                                                                           || dataDefinition.Usage == DataUsage.Pointer32; //Or usage is pointer/pointer-32
                        items = new CompletionForVariable(userFilterToken, keepCompatibleTypes).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    case TokenType.OF:
                        items = new CompletionForOf(userFilterToken, position).ComputeProposals(compilationUnit, matchingCodeElement);
                        break;
                    default:
                        // Unable to suggest anything
                        items = new List<CompletionItem>();
                        break;
                }
            }
            else
            {
                //If no known keyword has been found, let's try to get the context and return available variables. 
                if (matchingCodeElement == null && wrappedCodeElements.Any())
                {
                    userFilterToken =
                        wrappedCodeElements.First().ArrangedConsumedTokens.FirstOrDefault(
                            t =>
                                position.character <= t.StopIndex + 1 && position.character > t.StartIndex
                                && t.Line == position.line + 1
                                && t.TokenType == TokenType.UserDefinedWord); //Get the userFilterToken to filter the results
                    items = new CompletionForVariable(userFilterToken, _ => true).ComputeProposals(compilationUnit, wrappedCodeElements.First());
                }
                else
                {
                    //Return a default text to inform the user that completion is not available after the given token
                    items = new List<CompletionItem>(1)
                        {
                            new CompletionItem() { label = "Completion is not available in this context", insertText = string.Empty }
                        };
                }
            }

            if (userFilterToken != null)
            {
                //Add the range object to let the client know the position of the user filter token
                var range = Range.FromPositions(userFilterToken.Line - 1, userFilterToken.StartIndex, userFilterToken.Line - 1, userFilterToken.StopIndex + 1);
                //-1 on line to 0 based / +1 on stop index to include the last character
                items.ForEach(c =>
                {
                    if (c.data != null && c.data.GetType().IsArray)
                        ((object[])c.data)[0] = range;
                    else
                        c.data = range;
                });
            }

            return items;
        }
    }
}
