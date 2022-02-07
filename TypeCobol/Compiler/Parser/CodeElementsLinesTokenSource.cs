using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.SqlCodeElements.Statement;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Implementation of the Antlr ITokenSource interface on top of a ISearchableReadOnlyList<ICodeElementsLine>
    /// </summary>
    public class CodeElementsLinesTokenSource : ITokenSource
    {
        private string sourceFileName;
        private ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines;       

        public CodeElementsLinesTokenSource(string sourceFileName, ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines)
        {
            this.sourceFileName = sourceFileName;
            this.codeElementsLines = codeElementsLines;
            this.codeElementsLinesEnumerator = codeElementsLines.GetEnumerator();
        }

        IEnumerator<ICodeElementsLine> codeElementsLinesEnumerator;
        int currentCodeElementIndexInLine = -1;

        public CodeElement CurrentToken { get; private set; }

        public int Column
        {
            get 
            {
                if (CurrentToken != null)
                {
                    return CurrentToken.Column;
                }
                else
                {
                    return 0;
                }
            }
        }

        public ICharStream InputStream
        {
            get 
            {
                if (CurrentToken != null)
                {
                    return CurrentToken.InputStream;
                }
                else
                {
                    return null;
                }
            }
        }

        public int Line
        {
            get
            {
                if (CurrentToken != null)
                {
                    return CurrentToken.Line;
                }
                else
                {
                    return 0;
                }
            }
        }

        public IToken NextToken()
        {
            if(currentCodeElementIndexInLine >= 0)
            {
                currentCodeElementIndexInLine++;
                if(currentCodeElementIndexInLine < codeElementsLinesEnumerator?.Current?.CodeElements.Count)
                {
                    CurrentToken = codeElementsLinesEnumerator.Current.CodeElements[currentCodeElementIndexInLine];
                    return CurrentToken;
                }
                else
                {
                    currentCodeElementIndexInLine = -1;
                }
            }
            
            if (currentCodeElementIndexInLine < 0)
            {
                while(codeElementsLinesEnumerator != null && codeElementsLinesEnumerator.MoveNext())
                {
                    if(codeElementsLinesEnumerator?.Current?.CodeElements != null &&
                        codeElementsLinesEnumerator.Current.CodeElements.Count > 0)
                    {
                        currentCodeElementIndexInLine = 0;
                        CurrentToken = codeElementsLinesEnumerator.Current.CodeElements[currentCodeElementIndexInLine];
                        return CurrentToken;
                    }
                }
            }

            return Token.EndOfFile();
        }

        public string SourceName
        {
            get 
            { 
                return sourceFileName; 
            }
        }

        private ITokenFactory _tokenFactory = new CobolTokenFactory();

        public ITokenFactory TokenFactory
        {
            get
            {
                return _tokenFactory;
            }
            set
            {
                _tokenFactory = value;
            }
        }

        private class CobolTokenFactory : ITokenFactory
        {
            public IToken Create(int type, string text)
            {
                throw new NotImplementedException();
            }

            public IToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
            {
                if (text == null) text = String.Empty;
                var lastConsumedCodeElement = ((CodeElementsLinesTokenSource)source.Item1).CurrentToken;
                Token lastTokenBeforeError = null;
                if(lastConsumedCodeElement != null && lastConsumedCodeElement.ConsumedTokens != null && lastConsumedCodeElement.ConsumedTokens.Count > 0)
                {
                    lastTokenBeforeError = lastConsumedCodeElement.ConsumedTokens[lastConsumedCodeElement.ConsumedTokens.Count - 1];
                }
                IToken codeElement = BuildCodeElementFromType((CodeElementType)type, lastTokenBeforeError, text);
                return codeElement;
            }

            private static IToken BuildCodeElementFromType(CodeElementType type, Token lastTokenBeforeError, string informationText)
            {
                CodeElement codeElement = null;
                switch(type)
                {
                    case CodeElementType.ProgramIdentification:
                        codeElement = new ProgramIdentification();
                        break;
                    case CodeElementType.ProgramEnd:
                        codeElement = new ProgramEnd();
                        break;
                    case CodeElementType.ClassIdentification:
                        codeElement = new ClassIdentification();
                        break;
                    case CodeElementType.ClassEnd:
                        codeElement = new ClassEnd();
                        break;
                    case CodeElementType.FactoryIdentification:
                        codeElement = new FactoryIdentification();
                        break;
                    case CodeElementType.FactoryEnd:
                        codeElement = new FactoryEnd();
                        break;
                    case CodeElementType.ObjectIdentification:
                        codeElement = new ObjectIdentification();
                        break;
                    case CodeElementType.ObjectEnd:
                        codeElement = new ObjectEnd();
                        break;
                    case CodeElementType.MethodIdentification:
                        codeElement = new MethodIdentification();
                        break;
                    case CodeElementType.MethodEnd:
                        codeElement = new MethodEnd();
                        break;
                    case CodeElementType.EnvironmentDivisionHeader:
                        codeElement = new EnvironmentDivisionHeader();
                        break;
                    case CodeElementType.DataDivisionHeader:
                        codeElement = new DataDivisionHeader();
                        break;
                    case CodeElementType.ProcedureDivisionHeader:
                        codeElement = new ProcedureDivisionHeader();
                        break;
                    case CodeElementType.DeclarativesHeader:
                        codeElement = new DeclarativesHeader();
                        break;
                    case CodeElementType.DeclarativesEnd:
                        codeElement = new DeclarativesEnd();
                        break;
                    case CodeElementType.SectionHeader:
                        codeElement = new SectionHeader();
                        break;
                    case CodeElementType.ConfigurationSectionHeader:
                        codeElement = new ConfigurationSectionHeader();
                        break;
                    case CodeElementType.InputOutputSectionHeader:
                        codeElement = new InputOutputSectionHeader();
                        break;
                    case CodeElementType.FileSectionHeader:
                        codeElement = new FileSectionHeader();
                        break;
                    case CodeElementType.GlobalStorageSectionHeader:
                        codeElement = new GlobalStorageSectionHeader();
                        break;
                    case CodeElementType.WorkingStorageSectionHeader:
                        codeElement = new WorkingStorageSectionHeader();
                        break;
                    case CodeElementType.LocalStorageSectionHeader:
                        codeElement = new LocalStorageSectionHeader();
                        break;
                    case CodeElementType.LinkageSectionHeader:
                        codeElement = new LinkageSectionHeader();
                        break;
                    case CodeElementType.ParagraphHeader:
                        codeElement = new ParagraphHeader();
                        break;
                    case CodeElementType.FileControlParagraphHeader:
                        codeElement = new FileControlParagraphHeader();
                        break;
                    case CodeElementType.IOControlParagraphHeader:
                        codeElement = new IOControlParagraphHeader();
                        break;
                    case CodeElementType.SentenceEnd:
                        codeElement = new SentenceEnd();
                        break;
                    case CodeElementType.FileDescriptionEntry:
                        codeElement = new FileDescriptionEntry();
                        break;
                    case CodeElementType.DataDescriptionEntry:
                        codeElement = new DataDescriptionEntry();
                        break;
                    case CodeElementType.DataRedefinesEntry:
                        codeElement = new DataRedefinesEntry();
                        break;
                    case CodeElementType.DataRenamesEntry:
                        codeElement = new DataRenamesEntry();
                        break;
                    case CodeElementType.DataConditionEntry:
                        codeElement = new DataConditionEntry();
                        break;
                    case CodeElementType.FileControlEntry:
                        codeElement = new FileControlEntry();
                        break;
                    case CodeElementType.IOControlEntry:
                        codeElement = new RerunIOControlEntry();
                        break;
                    case CodeElementType.SourceComputerParagraph:
                        codeElement = new SourceComputerParagraph();
                        break;
                    case CodeElementType.ObjectComputerParagraph:
                        codeElement = new ObjectComputerParagraph();
                        break;
                    case CodeElementType.SpecialNamesParagraph:
                        codeElement = new SpecialNamesParagraph();
                        break;
                    case CodeElementType.RepositoryParagraph:
                        codeElement = new RepositoryParagraph();
                        break;
                    case CodeElementType.AcceptStatement:
                        codeElement = new AcceptFromInputDeviceStatement();
                        break;
                    case CodeElementType.AddStatement:
                        codeElement = new AddSimpleStatement();
                        break;
                    case CodeElementType.AllocateStatement:
                        codeElement = new AllocateStatement();
                        break;
                    case CodeElementType.AlterStatement:
                        codeElement = new AlterStatement();
                        break;
                    case CodeElementType.CallStatement:
                        codeElement = new CallStatement();
                        break;
                    case CodeElementType.CancelStatement:
                        codeElement = new CancelStatement();
                        break;
                    case CodeElementType.CloseStatement:
                        codeElement = new CloseStatement();
                        break;
                    case CodeElementType.ComputeStatement:
                        codeElement = new ComputeStatement();
                        break;
                    case CodeElementType.ContinueStatement:
                        codeElement = new ContinueStatement();
                        break;
                    case CodeElementType.DeleteStatement:
                        codeElement = new DeleteStatement();
                        break;
                    case CodeElementType.DisplayStatement:
                        codeElement = new DisplayStatement();
                        break;
                    case CodeElementType.DivideStatement:
                        codeElement = new DivideSimpleStatement();
                        break;
                    case CodeElementType.EntryStatement:
                        codeElement = new EntryStatement();
                        break;
                    case CodeElementType.EvaluateStatement:
                        codeElement = new EvaluateStatement();
                        break;
                    case CodeElementType.ExecStatement:
                        codeElement = new ExecStatement();
                        break;
                    case CodeElementType.ExecStatementText:
                        codeElement = new ExecStatementText();
                        break;
                    case CodeElementType.ExitMethodStatement:
                        codeElement = new ExitMethodStatement();
                        break;
                    case CodeElementType.ExitProgramStatement:
                        codeElement = new ExitProgramStatement();
                        break;
                    case CodeElementType.ExitStatement:
                        codeElement = new ExitStatement();
                        break;
                    case CodeElementType.FreeStatement:
                        codeElement = new FreeStatement();
                        break;
                    case CodeElementType.GobackStatement:
                        codeElement = new GobackStatement();
                        break;
                    case CodeElementType.GotoStatement:
                        codeElement = new GotoSimpleStatement();
                        break;
                    case CodeElementType.IfStatement:
                        codeElement = new IfStatement();
                        break;
                    case CodeElementType.InitializeStatement:
                        codeElement = new InitializeStatement();
                        break;
                    case CodeElementType.InspectStatement:
                        codeElement = new InspectTallyingStatement();
                        break;
                    case CodeElementType.InvokeStatement:
                        codeElement = new InvokeStatement();
                        break;
                    case CodeElementType.JsonGenerateStatement:
                        codeElement = new JsonGenerateStatement();
                        break;
                    case CodeElementType.JsonParseStatement:
                        codeElement = new JsonParseStatement();
                        break;
                    case CodeElementType.MergeStatement:
                        codeElement = new MergeStatement();
                        break;
                    case CodeElementType.MoveStatement:
                        codeElement = new MoveSimpleStatement(null, null, null);
                        break;
                    case CodeElementType.MultiplyStatement:
                        codeElement = new MultiplySimpleStatement();
                        break;
                    case CodeElementType.NextSentenceStatement:
                        codeElement = new NextSentenceStatement();
                        break;
                    case CodeElementType.OpenStatement:
                        codeElement = new OpenStatement();
                        break;
                    case CodeElementType.PerformProcedureStatement:
                        codeElement = new PerformProcedureStatement();
                        break;
                    case CodeElementType.PerformStatement:
                        codeElement = new PerformStatement();
                        break;
                    case CodeElementType.ReadStatement:
                        codeElement = new ReadStatement();
                        break;
                    case CodeElementType.ReleaseStatement:
                        codeElement = new ReleaseStatement();
                        break;
                    case CodeElementType.ReturnStatement:
                        codeElement = new ReturnStatement();
                        break;
                    case CodeElementType.RewriteStatement:
                        codeElement = new RewriteStatement();
                        break;
                    case CodeElementType.SearchStatement:
                        codeElement = new SearchSerialStatement();
                        break;
                    case CodeElementType.SetStatement:
                        codeElement = new SetStatementForAssignment();
                        break;
                    case CodeElementType.SortStatement:
                        codeElement = new SortStatement();
                        break;
                    case CodeElementType.StartStatement:
                        codeElement = new StartStatement();
                        break;
                    case CodeElementType.StopStatement:
                        codeElement = new StopStatement();
                        break;
                    case CodeElementType.StringStatement:
                        codeElement = new StringStatement();
                        break;
                    case CodeElementType.SubtractStatement:
                        codeElement = new SubtractSimpleStatement();
                        break;
                    case CodeElementType.UnstringStatement:
                        codeElement = new UnstringStatement();
                        break;
                    case CodeElementType.UseStatement:
                        codeElement = new UseAfterIOExceptionStatement();
                        break;
                    case CodeElementType.WriteStatement:
                        codeElement = new WriteStatement();
                        break;
                    case CodeElementType.XmlGenerateStatement:
                        codeElement = new XmlGenerateStatement();
                        break;
                    case CodeElementType.XmlParseStatement:
                        codeElement = new XmlParseStatement();
                        break;
                    case CodeElementType.AtEndCondition:
                        codeElement = new AtEndCondition();
                        break;
                    case CodeElementType.NotAtEndCondition:
                        codeElement = new NotAtEndCondition();
                        break;
                    case CodeElementType.AtEndOfPageCondition:
                        codeElement = new AtEndOfPageCondition();
                        break;
                    case CodeElementType.NotAtEndOfPageCondition:
                        codeElement = new NotAtEndOfPageCondition();
                        break;
                    case CodeElementType.OnExceptionCondition:
                        codeElement = new OnExceptionCondition();
                        break;
                    case CodeElementType.NotOnExceptionCondition:
                        codeElement = new NotOnExceptionCondition();
                        break;
                    case CodeElementType.OnOverflowCondition:
                        codeElement = new OnOverflowCondition();
                        break;
                    case CodeElementType.NotOnOverflowCondition:
                        codeElement = new NotOnOverflowCondition();
                        break;
                    case CodeElementType.InvalidKeyCondition:
                        codeElement = new InvalidKeyCondition();
                        break;
                    case CodeElementType.NotInvalidKeyCondition:
                        codeElement = new NotInvalidKeyCondition();
                        break;
                    case CodeElementType.OnSizeErrorCondition:
                        codeElement = new OnSizeErrorCondition();
                        break;
                    case CodeElementType.NotOnSizeErrorCondition:
                        codeElement = new NotOnSizeErrorCondition();
                        break;
                    case CodeElementType.ElseCondition:
                        codeElement = new ElseCondition();
                        break;
                    case CodeElementType.WhenCondition:
                        codeElement = new WhenCondition();
                        break;
                    case CodeElementType.WhenOtherCondition:
                        codeElement = new WhenOtherCondition();
                        break;
                    case CodeElementType.WhenSearchCondition:
                        codeElement = new WhenSearchCondition();
                        break;
                    case CodeElementType.AddStatementEnd:
                        codeElement = new AddStatementEnd();
                        break;
                    case CodeElementType.CallStatementEnd:
                        codeElement = new CallStatementEnd();
                        break;
                    case CodeElementType.ComputeStatementEnd:
                        codeElement = new ComputeStatementEnd();
                        break;
                    case CodeElementType.DeleteStatementEnd:
                        codeElement = new DeleteStatementEnd();
                        break;
                    case CodeElementType.DivideStatementEnd:
                        codeElement = new DivideStatementEnd();
                        break;
                    case CodeElementType.EvaluateStatementEnd:
                        codeElement = new EvaluateStatementEnd();
                        break;
                    case CodeElementType.ExecStatementEnd:
                        codeElement = new ExecStatementEnd();
                        break;
                    case CodeElementType.IfStatementEnd:
                        codeElement = new IfStatementEnd();
                        break;
                    case CodeElementType.InvokeStatementEnd:
                        codeElement = new InvokeStatementEnd();
                        break;
                    case CodeElementType.JsonStatementEnd:
                        codeElement = new JsonStatementEnd();
                        break;
                    case CodeElementType.MultiplyStatementEnd:
                        codeElement = new MultiplyStatementEnd();
                        break;
                    case CodeElementType.PerformStatementEnd:
                        codeElement = new PerformStatementEnd();
                        break;
                    case CodeElementType.ReadStatementEnd:
                        codeElement = new ReadStatementEnd();
                        break;
                    case CodeElementType.ReturnStatementEnd:
                        codeElement = new ReturnStatementEnd();
                        break;
                    case CodeElementType.RewriteStatementEnd:
                        codeElement = new RewriteStatementEnd();
                        break;
                    case CodeElementType.SearchStatementEnd:
                        codeElement = new SearchStatementEnd();
                        break;
                    case CodeElementType.StartStatementEnd:
                        codeElement = new StartStatementEnd();
                        break;
                    case CodeElementType.StringStatementEnd:
                        codeElement = new StringStatementEnd();
                        break;
                    case CodeElementType.SubtractStatementEnd:
                        codeElement = new SubtractStatementEnd();
                        break;
                    case CodeElementType.UnstringStatementEnd:
                        codeElement = new UnstringStatementEnd();
                        break;
                    case CodeElementType.WriteStatementEnd:
                        codeElement = new WriteStatementEnd();
                        break;
                    case CodeElementType.XmlStatementEnd:
                        codeElement = new XmlStatementEnd();
                        break;
                    case CodeElementType.LibraryCopy:
                        codeElement = new LibraryCopyCodeElement();
                        break;
                    case CodeElementType.FunctionDeclarationHeader:
                        codeElement = new FunctionDeclarationHeader();
                        break;
                    case CodeElementType.FunctionDeclarationEnd:
                        codeElement = new FunctionDeclarationEnd();
                        break;
                    case CodeElementType.CommitStatement:
                        codeElement = new CommitStatement();
                        break;
                    default:
                        throw new NotImplementedException();
                }
                if (lastTokenBeforeError != null)
                {
                    var missingToken = new MissingToken(TokenType.InvalidToken, informationText, lastTokenBeforeError.TokensLine, lastTokenBeforeError.StopIndex);
                    codeElement.ConsumedTokens.Add(missingToken);
                }
                return codeElement;
            }
        }
    }    
}
