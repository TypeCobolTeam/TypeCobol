
using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using Object = TypeCobol.Compiler.Nodes.Object;
using String = TypeCobol.Compiler.Nodes.String;

namespace TypeCobol.Compiler.CodeElements
{
    public interface IVisitable {
        bool AcceptASTVisitor([NotNull] IASTVisitor astVisitor);
    }

    public static class VisitableExtension
    {

        public static bool ContinueVisitToChildren(this IVisitable mainVisitable, IASTVisitor astVisitor, IEnumerable<IVisitable> visitables)
        {
            bool continueVisit = true;
            if (visitables != null)
            {
                foreach (var visitable in visitables)
                {
                    if (!continueVisit) break;
                    continueVisit = visitable.AcceptASTVisitor(astVisitor);
                }
            }
            return continueVisit;
        }

        public static bool ContinueVisitToChildren(this IVisitable mainVisitable, IASTVisitor astVisitor, params IVisitable[] visitables) {
            bool continueVisit = true;
            if (visitables != null) {
                foreach (var visitable in visitables) {
                    if (!continueVisit) break;
                    if (visitable != null) {
                        continueVisit = visitable.AcceptASTVisitor(astVisitor);
                    }
                }
            }
            return continueVisit;
        }

        public static bool ContinueVisitToChildren(this IVisitable mainVisitable, IASTVisitor astVisitor, params IEnumerable<IVisitable>[] visitablesList) {
            bool continueVisit = true;
            if (visitablesList != null) {
                foreach (var visitables in visitablesList) {
                    if (!continueVisit) break;
                    if (visitables != null) {
                        foreach (var visitable in visitables) {
                            if (!continueVisit) break;
                            if (visitable != null) {
                                continueVisit = visitable.AcceptASTVisitor(astVisitor);
                            }
                        }
                    }
                }
            }
            return continueVisit;
        }
    }

    public  interface IASTVisitor {

        /// <summary>
        /// Is this Visitor Allow visiting SymbolInformationForTokens?
        /// </summary>
        bool IsSymbolInformationForTokensEnabled
        {
            get;
        }

        /// <summary>
        /// Is this Visitor Stop visiting Children when one children has asked to stop ?
        /// </summary>
        bool IsStopVisitingChildren
        {
            get;
        }

        bool BeginNode([NotNull] Node node);
        void EndNode([NotNull] Node node);

        bool BeginCodeElement([NotNull] CodeElement codeElement);
        void EndCodeElement([NotNull] CodeElement codeElement);


        bool Visit([NotNull] AcceptStatement acceptStatement);
        bool Visit([NotNull] AcceptFromInputDeviceStatement acceptFromInputDeviceStatement);
        bool Visit([NotNull] AcceptFromSystemDateStatement acceptFromSystemDateStatement);
        bool Visit([NotNull] AddSimpleStatement addSimpleStatement);
        bool Visit([NotNull] AddGivingStatement addGivingStatement);
        bool Visit([NotNull] AddCorrespondingStatement addCorrespondingStatement);
        bool Visit([NotNull] AlterStatement alterStatement);
        bool Visit([NotNull] CallStatement callStatement);
        bool Visit([NotNull] CancelStatement cancelStatement);
        bool Visit([NotNull] CloseStatement closeStatement);
        bool Visit([NotNull] ComputeStatement computeStatement);
        bool Visit([NotNull] ContinueStatement continueStatement);
        bool Visit([NotNull] DeleteStatement deleteStatement);
        bool Visit([NotNull] DisplayStatement displayStatement);
        bool Visit([NotNull] DivideStatement divideStatement);
        bool Visit([NotNull] EntryStatement entryStatement);
        bool Visit([NotNull] EvaluateStatement evaluateStatement);
        bool Visit([NotNull] ExecStatement execStatement);
        bool Visit([NotNull] ExitMethodStatement exitMethodStatement);
        bool Visit([NotNull] ExitProgramStatement exitProgramStatement);
        bool Visit([NotNull] ExitStatement exitStatement);
        bool Visit([NotNull] GobackStatement gobackStatement);
        bool Visit([NotNull] GotoStatement gotoStatement);
        bool Visit([NotNull] IfStatement ifStatement);
        bool Visit([NotNull] InitializeStatement initializeStatement);
        bool Visit([NotNull] InspectStatement inspectStatement);
        bool Visit([NotNull] InvokeStatement invokeStatement);
        bool Visit([NotNull] MergeStatement mergeStatement);
        bool Visit([NotNull] MoveStatement moveStatement);
        bool Visit([NotNull] MoveSimpleStatement moveStatement);
        bool Visit([NotNull] MoveCorrespondingStatement moveStatement);
                   
        bool Visit([NotNull] MultiplyStatement multiplyStatement);
        bool Visit([NotNull] OpenStatement openStatement);
        bool Visit([NotNull] PerformProcedureStatement performProcedureStatement);
        bool Visit([NotNull] PerformStatement performStatement);
        bool Visit([NotNull] ReadStatement readStatement);
        bool Visit([NotNull] ReleaseStatement releaseStatement);
        bool Visit([NotNull] ReturnStatement returnStatement);
        bool Visit([NotNull] RewriteStatement rewriteStatement);
        bool Visit([NotNull] SearchStatement searchStatement);
        bool Visit([NotNull] SetStatement setStatement);
        bool Visit([NotNull] SetStatementForAssignment setStatement);
        bool Visit([NotNull] SetStatementForIndexes setStatement);
                   
        bool Visit([NotNull] SortStatement sortStatement);
        bool Visit([NotNull] StartStatement startStatement);
        bool Visit([NotNull] StopStatement stopStatement);
        bool Visit([NotNull] StringStatement stringStatement);
        bool Visit([NotNull] SubtractStatement substractStatement);
        bool Visit([NotNull] UnstringStatement unstringStatement);
        bool Visit([NotNull] WriteStatement writeStatement);
        bool Visit([NotNull] XmlGenerateStatement xmlGenerateStatement);
        bool Visit([NotNull] XmlParseStatement xmlParseStatement);


        //Expressions
        bool Visit([NotNull] ReceivingStorageArea receivingStorageArea);

        bool Visit([NotNull] NumericVariable numericVariable);
        bool Visit([NotNull] NumericVariable[] numericVariable);

        bool Visit([NotNull] NumericValue numericValue);
        bool Visit([NotNull] GeneratedNumericValue generatedNumericValue);
//        bool Visit(StorageDataType storageDataType);
        bool Visit([NotNull] StorageArea storageArea);
        bool Visit([NotNull] FunctionCallResult functionCallResult);
        bool Visit([NotNull] FilePropertySpecialRegister filePropertySpecialRegister);
        bool Visit([NotNull] IndexStorageArea indexStorageArea);
        bool Visit([NotNull] IntrinsicStorageArea intrinsicStorageArea);
        bool Visit([NotNull] StorageAreaPropertySpecialRegister storageAreaPropertySpecialRegister);
        bool Visit([NotNull] DataOrConditionStorageArea storageArea);
        bool Visit([NotNull] SymbolReference symbolReference);
        bool Visit([NotNull] AmbiguousSymbolReference ambiguousSymbolReference);
        bool Visit([NotNull] ReferenceModifier referenceModifier);
        bool Visit([NotNull] ExternalNameOrSymbolReference externalNameOrSymbolReference);

        bool Visit([NotNull] FunctionCall functionCall);
        bool Visit([NotNull] IntrinsicFunctionCall functionCall);
        bool Visit([NotNull] UserDefinedFunctionCall functionCall);
        bool Visit([NotNull] ProcedureCall functionCall);
        bool Visit([NotNull] BooleanValue booleanValue);
        bool Visit([NotNull] GeneratedBooleanValue generatedBooleanValue);
        bool Visit([NotNull] Variable variable);
        bool Visit([NotNull] VariableOrExpression variableOrExpression);
        bool Visit([NotNull] Token token);
        bool Visit([NotNull] CallSiteParameter callSiteParameter);
        bool Visit([NotNull] ExternalName externalName);
        bool Visit([NotNull] QualifiedTextName qualifiedTextName);
        bool Visit([NotNull] SyntaxProperty<bool> syntaxProperty);
        bool Visit([NotNull] SymbolInformation symbolInformation);
        bool Visit([NotNull] SymbolDefinitionOrReference symbolDefinitionOrReference);
        bool Visit([NotNull] SymbolDefinition symbolDefinition);
        bool Visit([NotNull] QualifiedSymbolReference qualifiedSymbolReference);
        bool Visit([NotNull] TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference);
        bool Visit([NotNull] SyntaxValue<string> syntaxValue);
        bool Visit([NotNull] AlphanumericValue alphanumericValue);
        bool Visit([NotNull] GeneratedAlphanumericValue generatedAlphanumericValue);
        bool Visit([NotNull] GeneratedSymbolName generatedSymbolName);
        bool Visit([NotNull] EnumeratedValue enumeratedValue);
        bool Visit([NotNull] CharacterValue characterValue);
        bool Visit([NotNull] RepeatedCharacterValue repeatedCharacterValue);
        bool Visit([NotNull] SymbolType symbolType);
        bool Visit([NotNull] Expression expression);
        bool Visit([NotNull] ArithmeticExpression arithmeticExpression);
        bool Visit([NotNull] NumericVariableOperand numericVariableOperand);
        bool Visit([NotNull] ArithmeticOperation arithmeticOperation);
        bool Visit([NotNull] ConditionalExpression conditionalExpression);
        bool Visit([NotNull] SignCondition signCondition);
        bool Visit([NotNull] RelationCondition relationCondition);
        bool Visit([NotNull] LogicalOperation logicalOperation);
        bool Visit([NotNull] ClassCondition classCondition);
        bool Visit([NotNull] ConditionNameConditionOrSwitchStatusCondition conditionNameConditionOrSwitchStatusCondition);
        bool Visit([NotNull] VariableBase variableBase);
        bool Visit([NotNull] SymbolReferenceVariable symbolReferenceVariable);
        bool Visit([NotNull] AlphanumericVariable alphanumericVariable);
        bool Visit([NotNull] CharacterVariable characterVariable);
        bool Visit([NotNull] IntegerVariable integerVariable);
        bool Visit<T>([NotNull] SyntaxValue<T> syntaxValue);
        bool Visit([NotNull] NullPointerValue nullPointerValue);
        bool Visit([NotNull] IntegerValue integerValue);
        bool Visit([NotNull] GeneratedIntegerValue generatedIntegerValue);
        bool Visit<T>([NotNull] SyntaxProperty<T> syntaxProperty);

        bool Visit([NotNull] CallSite callSite);
        bool Visit([NotNull] CallTarget callTarget);
        bool Visit([NotNull] CallTargetParameter callTargetParameter);
        bool Visit([NotNull] SubscriptExpression subscriptExpression);
        bool Visit([NotNull] Value value);
        bool Visit([NotNull] GroupCorrespondingImpact groupCorrespondingImpact);
        bool Visit([NotNull] BooleanValueOrExpression booleanValueOrExpression);
        bool Visit([NotNull] DataDefinitionEntry dataDefinitionEntry);
        bool Visit([NotNull] DataDescriptionEntry dataDescriptionEntry);
        bool Visit([NotNull] SpecialRegisterDescriptionEntry specialRegisterDescriptionEntry);
        bool Visit([NotNull] FunctionCallResultDescriptionEntry functionCallResultDescriptionEntry);
        bool Visit([NotNull] ParameterDescriptionEntry parameterDescriptionEntry);
        bool Visit([NotNull] DataTypeDescriptionEntry dataTypeDescriptionEntry);
        bool Visit([NotNull] DataRedefinesEntry dataRedefinesEntry);
        bool Visit([NotNull] DataRenamesEntry dataRenamesEntry);
        bool Visit([NotNull] DataConditionEntry dataConditionEntry);
        bool Visit([NotNull] DataType dataType);
        bool Visit([NotNull] TableSortingKey tableSortingKey);
        bool Visit([NotNull] ValuesRange valuesRange);
        bool Visit([NotNull] CodeElement codeElement);
        bool Visit([NotNull] FunctionDeclarationEnd functionDeclarationEnd);
        bool Visit([NotNull] FunctionDeclarationHeader functionDeclarationHeader);
        bool Visit([NotNull] StatementElement statementElement);
        bool VisitVariableWriter([NotNull] VariableWriter variableWriter);
        bool VisitFunctionCaller([NotNull] FunctionCaller functionCaller);
        bool Visit([NotNull] SetSendingVariable setSendingVariable);
        bool Visit([NotNull] SetUPSISwitchInstruction setUPSISwitchInstruction);
        bool Visit([NotNull] ParametersProfile parametersProfile);


        //Nodes
        bool Visit([NotNull] Section section);
        bool Visit([NotNull] Paragraph paragraph);
        bool Visit([NotNull] Perform perform);
        bool Visit([NotNull] PerformProcedure performProcedure);
        bool Visit([NotNull] SourceFile root);
        bool Visit([NotNull] LibraryCopy libraryCopy);
        bool Visit([NotNull] Nodes.Class classNode);
        bool Visit([NotNull] Factory factory);
        bool Visit([NotNull] Nodes.Method method);
        bool Visit([NotNull] Object objectNode);
        bool Visit([NotNull] End end);

        bool Visit([NotNull] Accept accept);
        bool Visit([NotNull] Alter alter);
        bool Visit([NotNull] Call call);
        bool Visit([NotNull] ProcedureStyleCall procedureStyleCall);
        bool Visit([NotNull] Cancel cancel);
        bool Visit([NotNull] Continue continueNode);
        bool Visit([NotNull] Delete delete);
        bool Visit([NotNull] Display display);
        bool Visit([NotNull] Entry entry);
        bool Visit([NotNull] Exec exec);
        bool Visit([NotNull] Exit exit);
        bool Visit([NotNull] ExitMethod exitMethod);
        bool Visit([NotNull] ExitProgram exitProgram);
        bool Visit([NotNull] Goback goback);
        bool Visit([NotNull] Goto gotoNode);
        bool Visit([NotNull] Initialize initialize);
        bool Visit([NotNull] Inspect inspect);
        bool Visit([NotNull] Invoke invoke);
        bool Visit([NotNull] Merge merge);
        bool Visit([NotNull] Move move);
        bool Visit([NotNull] Release release);
        bool Visit([NotNull] Return returnNode);
        bool Visit([NotNull] Set set);
        bool Visit([NotNull] Sort sort);
        bool Visit([NotNull] Start start);
        bool Visit([NotNull] Stop stop);
        bool Visit([NotNull] String stringNode);
        bool Visit([NotNull] Unstring unstring);
        bool Visit([NotNull] XmlGenerate xmlGenerate);
        bool Visit([NotNull] XmlParse xmlParse);
        bool Visit([NotNull] Add add);
        bool Visit([NotNull] Subtract subtract);
        bool Visit([NotNull] Multiply multiply);
        bool Visit([NotNull] Divide divide);
        bool Visit([NotNull] Compute compute);
        bool Visit([NotNull] Open open);
        bool Visit([NotNull] Close close);
        bool Visit([NotNull] Read read);
        bool Visit([NotNull] Write write);
        bool Visit([NotNull] Rewrite rewrite);
        bool Visit([NotNull] If ifNode);
        bool Visit([NotNull] Then then);
        bool Visit([NotNull] Else elseNode);
        bool Visit([NotNull] NextSentence nextSentence);
        bool Visit([NotNull] Evaluate evaluate);
        bool Visit([NotNull] WhenGroup whenGroup);
        bool Visit([NotNull] When when);
        bool Visit([NotNull] WhenOther whenOther);
        bool Visit([NotNull] Search search);
        bool Visit([NotNull] WhenSearch whenSearch);

        bool Visit([NotNull] DataSection dataSection);
        bool Visit([NotNull] DataDivision dataDivision);
        bool Visit([NotNull] FileSection fileSection);
        bool Visit([NotNull] FileDescriptionEntryNode fileDescriptionEntryNode);
        bool Visit([NotNull] WorkingStorageSection workingStorageSection);
        bool Visit([NotNull] LocalStorageSection localStorageSection);
        bool Visit([NotNull] LinkageSection linkageSection);
        bool Visit([NotNull] DataDefinition dataDefinition);
        bool Visit([NotNull] DataDescription dataDescription);
        bool Visit([NotNull] DataCondition dataCondition);
        bool Visit([NotNull] DataRedefines dataRedefines);
        bool Visit([NotNull] DataRenames dataRenames);
        bool Visit([NotNull] TypeDefinition typeDefinition);
        bool Visit([NotNull] ParameterDescription parameterDescription);

        bool Visit([NotNull] EnvironmentDivision environmentDivision);
        bool Visit([NotNull] ConfigurationSection configurationSection);
        bool Visit([NotNull] SourceComputer sourceComputer);
        bool Visit([NotNull] ObjectComputer objectComputer);
        bool Visit([NotNull] SpecialNames specialNames);
        bool Visit([NotNull] Repository repository);
        bool Visit([NotNull] InputOutputSection inputOutputSection);
        bool Visit([NotNull] FileControlParagraphHeaderNode fileControlParagraphHeaderNode);
        bool Visit([NotNull] FileControlEntryNode fileControlEntryNode);

        bool Visit([NotNull] OnSizeError onSizeError);
        bool Visit([NotNull] NoSizeError noSizeError);
        bool Visit([NotNull] OnAtEnd onAtEnd);
        bool Visit([NotNull] NoAtEnd noAtEnd);
        bool Visit([NotNull] OnException onException);
        bool Visit([NotNull] NoException noException);
        bool Visit([NotNull] OnInvalidKey onInvalidKey);
        bool Visit([NotNull] NoInvalidKey noInvalidKey);
        bool Visit([NotNull] OnOverflow onOverflow);
        bool Visit([NotNull] NoOverflow noOverflow);
        bool Visit([NotNull] ProcedureDivision procedureDivision);
        bool Visit([NotNull] FunctionDeclaration functionDeclaration);
        bool Visit([NotNull] FunctionEnd functionEnd);
        bool Visit([NotNull] Sentence sentence);

        bool Visit([NotNull] RoundedResult roundedResult);
        bool Visit([NotNull] CloseFileInstruction closeFileInstruction);

        bool Visit([NotNull] CodeModel.Program program);

        bool Visit([NotNull] ParametersProfileNode profile);
        bool Visit ([NotNull] IndexDefinition indexDefinition);
    }



    public abstract class AbstractAstVisitor : IASTVisitor {
        public virtual bool IsSymbolInformationForTokensEnabled
        {
            get
            {
                return true;
            }
        }

        public virtual bool IsStopVisitingChildren
        {
            get
            {
                return true;
            }
        }

        public virtual bool BeginNode(Node node) {
            return true;
        }

        public virtual void EndNode(Node node) {
            
        }

        public virtual bool BeginCodeElement(CodeElement codeElement) {
            return true;
        }

        public virtual void EndCodeElement(CodeElement codeElement) {
            
        }

        public virtual bool Visit(AcceptStatement acceptStatement) {
            return true;
        }

        public virtual bool Visit(AcceptFromInputDeviceStatement acceptFromInputDeviceStatement) {
            return true;
        }

        public virtual bool Visit(AcceptFromSystemDateStatement acceptFromSystemDateStatement) {
            return true;
        }

        public virtual bool Visit(AddSimpleStatement addSimpleStatement) {
            return true;
        }

        public virtual bool Visit(AddGivingStatement addGivingStatement) {
            return true;
        }

        public virtual bool Visit(AddCorrespondingStatement addCorrespondingStatement) {
            return true;
        }

        public virtual bool Visit(AlterStatement alterStatement) {
            return true;
        }

        public virtual bool Visit(CallStatement callStatement) {
            return true;
        }

        public virtual bool Visit(CancelStatement cancelStatement) {
            return true;
        }

        public virtual bool Visit(CloseStatement closeStatement) {
            return true;
        }

        public virtual bool Visit(ComputeStatement computeStatement) {
            return true;
        }

        public virtual bool Visit(ContinueStatement continueStatement) {
            return true;
        }

        public virtual bool Visit(DeleteStatement deleteStatement) {
            return true;
        }

        public virtual bool Visit(DisplayStatement displayStatement) {
            return true;
        }

        public virtual bool Visit(DivideStatement divideStatement) {
            return true;
        }

        public virtual bool Visit(EntryStatement entryStatement) {
            return true;
        }

        public virtual bool Visit(EvaluateStatement evaluateStatement) {
            return true;
        }

        public virtual bool Visit(ExecStatement execStatement) {
            return true;
        }

        public virtual bool Visit(ExitMethodStatement exitMethodStatement) {
            return true;
        }

        public virtual bool Visit(ExitProgramStatement exitProgramStatement) {
            return true;
        }

        public virtual bool Visit(ExitStatement exitStatement) {
            return true;
        }

        public virtual bool Visit(GobackStatement gobackStatement) {
            return true;
        }

        public virtual bool Visit(GotoStatement gotoStatement) {
            return true;
        }

        public virtual bool Visit(IfStatement ifStatement) {
            return true;
        }

        public virtual bool Visit(InitializeStatement initializeStatement) {
            return true;
        }

        public virtual bool Visit(InspectStatement inspectStatement) {
            return true;
        }

        public virtual bool Visit(InvokeStatement invokeStatement) {
            return true;
        }

        public virtual bool Visit(MergeStatement mergeStatement) {
            return true;
        }

        public virtual bool Visit(MoveStatement moveStatement) {
            return true;
        }

        public virtual bool Visit(MoveSimpleStatement moveStatement) {
            return true;
        }

        public virtual bool Visit(MoveCorrespondingStatement moveStatement) {
            return true;
        }

        public virtual bool Visit(MultiplyStatement multiplyStatement) {
            return true;
        }

        public virtual bool Visit(OpenStatement openStatement) {
            return true;
        }

        public virtual bool Visit(PerformProcedureStatement performProcedureStatement) {
            return true;
        }

        public virtual bool Visit(PerformStatement performStatement) {
            return true;
        }

        public virtual bool Visit(ReadStatement readStatement) {
            return true;
        }

        public virtual bool Visit(ReleaseStatement releaseStatement) {
            return true;
        }

        public virtual bool Visit(ReturnStatement returnStatement) {
            return true;
        }

        public virtual bool Visit(RewriteStatement rewriteStatement) {
            return true;
        }

        public virtual bool Visit(SearchStatement searchStatement) {
            return true;
        }

        public virtual bool Visit(SetStatement setStatement) {
            return true;
        }

        public virtual bool Visit(SetStatementForAssignment setStatement) {
            return true;
        }

        public virtual bool Visit(SetStatementForIndexes setStatement) {
            return true;
        }

        public virtual bool Visit(SortStatement sortStatement) {
            return true;
        }

        public virtual bool Visit(StartStatement startStatement) {
            return true;
        }

        public virtual bool Visit(StopStatement stopStatement) {
            return true;
        }

        public virtual bool Visit(StringStatement stringStatement) {
            return true;
        }

        public virtual bool Visit(SubtractStatement substractStatement) {
            return true;
        }

        public virtual bool Visit(UnstringStatement unstringStatement) {
            return true;
        }

        public virtual bool Visit(WriteStatement writeStatement) {
            return true;
        }

        public virtual bool Visit(XmlGenerateStatement xmlGenerateStatement) {
            return true;
        }

        public virtual bool Visit(XmlParseStatement xmlParseStatement) {
            return true;
        }

        public virtual bool Visit(ReceivingStorageArea receivingStorageArea) {
            return true;
        }

        public virtual bool Visit(NumericVariable numericVariable) {
            return true;
        }

        public virtual bool Visit(NumericVariable[] numericVariable) {
            return true;
        }

        public virtual bool Visit(NumericValue numericValue) {
            return true;
        }

        public virtual bool Visit(GeneratedNumericValue generatedNumericValue) {
            return true;
        }

        public virtual bool Visit(StorageArea storageArea) {
            return true;
        }

        public virtual bool Visit(FunctionCallResult functionCallResult) {
            return true;
        }

        public virtual bool Visit(FilePropertySpecialRegister filePropertySpecialRegister) {
            return true;
        }

        public virtual bool Visit(IndexStorageArea indexStorageArea) {
            return true;
        }

        public virtual bool Visit(StorageAreaPropertySpecialRegister storageAreaPropertySpecialRegister) {
            return true;
        }

        public virtual bool Visit(DataOrConditionStorageArea storageArea) {
            return true;
        }

        public virtual bool Visit(SymbolReference symbolReference) {
            return true;
        }

        public virtual bool Visit(AmbiguousSymbolReference ambiguousSymbolReference) {
            return true;
        }

        public virtual bool Visit(ReferenceModifier referenceModifier) {
            return true;
        }

        public virtual bool Visit(ExternalNameOrSymbolReference externalNameOrSymbolReference) {
            return true;
        }

        public virtual bool Visit(FunctionCall functionCall) {
            return true;
        }

        public virtual bool Visit(IntrinsicFunctionCall functionCall) {
            return true;
        }

        public virtual bool Visit(UserDefinedFunctionCall functionCall) {
            return true;
        }

        public virtual bool Visit(ProcedureCall functionCall) {
            return true;
        }

        public virtual bool Visit(BooleanValue booleanValue) {
            return true;
        }

        public virtual bool Visit(GeneratedBooleanValue generatedBooleanValue) {
            return true;
        }

        public virtual bool Visit(Variable variable) {
            return true;
        }

        public virtual bool Visit(VariableOrExpression variableOrExpression) {
            return true;
        }

        public virtual bool Visit(Token token) {
            return true;
        }

        public virtual bool Visit(CallSiteParameter callSiteParameter) {
            return true;
        }

        public virtual bool Visit(ExternalName externalName) {
            return true;
        }

        public virtual bool Visit(QualifiedTextName qualifiedTextName) {
            return true;
        }

        public virtual bool Visit(SyntaxProperty<bool> syntaxProperty) {
            return true;
        }

        public virtual bool Visit(SymbolInformation symbolInformation) {
            return true;
        }

        public virtual bool Visit(SymbolDefinitionOrReference symbolDefinitionOrReference) {
            return true;
        }

        public virtual bool Visit(SymbolDefinition symbolDefinition) {
            return true;
        }

        public virtual bool Visit(QualifiedSymbolReference qualifiedSymbolReference) {
            return true;
        }

        public virtual bool Visit(TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference) {
            return true;
        }

        public virtual bool Visit(SyntaxValue<string> syntaxValue) {
            return true;
        }

        public virtual bool Visit(AlphanumericValue alphanumericValue) {
            return true;
        }

        public virtual bool Visit(GeneratedAlphanumericValue generatedAlphanumericValue) {
            return true;
        }

        public virtual bool Visit(GeneratedSymbolName generatedSymbolName) {
            return true;
        }

        public virtual bool Visit(EnumeratedValue enumeratedValue) {
            return true;
        }

        public virtual bool Visit(CharacterValue characterValue) {
            return true;
        }

        public virtual bool Visit(RepeatedCharacterValue repeatedCharacterValue) {
            return true;
        }

        public virtual bool Visit(SymbolType symbolType) {
            return true;
        }

        public virtual bool Visit(Expression expression) {
            return true;
        }

        public virtual bool Visit(ArithmeticExpression arithmeticExpression) {
            return true;
        }

        public virtual bool Visit(NumericVariableOperand numericVariableOperand) {
            return true;
        }

        public virtual bool Visit(ArithmeticOperation arithmeticOperation) {
            return true;
        }

        public virtual bool Visit(ConditionalExpression conditionalExpression) {
            return true;
        }

        public virtual bool Visit(SignCondition signCondition) {
            return true;
        }

        public virtual bool Visit(RelationCondition relationCondition) {
            return true;
        }

        public virtual bool Visit(LogicalOperation logicalOperation) {
            return true;
        }

        public virtual bool Visit(ClassCondition classCondition) {
            return true;
        }

        public virtual bool Visit(ConditionNameConditionOrSwitchStatusCondition conditionNameConditionOrSwitchStatusCondition) {
            return true;
        }

        public virtual bool Visit(VariableBase variableBase) {
            return true;
        }

        public virtual bool Visit(SymbolReferenceVariable symbolReferenceVariable) {
            return true;
        }

        public virtual bool Visit(AlphanumericVariable alphanumericVariable) {
            return true;
        }

        public virtual bool Visit(CharacterVariable characterVariable) {
            return true;
        }

        public virtual bool Visit(IntegerVariable integerVariable) {
            return true;
        }

        public virtual bool Visit<T>(SyntaxValue<T> syntaxValue) {
            return true;
        }

        public virtual bool Visit(SyntaxValue<object> syntaxValue) {
            return true;
        }

        public virtual bool Visit(NullPointerValue nullPointerValue) {
            return true;
        }

        public virtual bool Visit(IntegerValue integerValue) {
            return true;
        }

        public virtual bool Visit(GeneratedIntegerValue generatedIntegerValue) {
            return true;
        }

        public virtual bool Visit<T>(SyntaxProperty<T> syntaxProperty) {
            return true;
        }

        public virtual bool Visit(CallSite callSite) {
            return true;
        }

        public virtual bool Visit(CallTarget callTarget) {
            return true;
        }

        public virtual bool Visit(CallTargetParameter callTargetParameter) {
            return true;
        }

        public virtual bool Visit(SubscriptExpression subscriptExpression) {
            return true;
        }

        public virtual bool Visit(Value value) {
            return true;
        }

        public virtual bool Visit(GroupCorrespondingImpact groupCorrespondingImpact) {
            return true;
        }

        public virtual bool Visit(BooleanValueOrExpression booleanValueOrExpression) {
            return true;
        }

        public virtual bool Visit(DataDefinitionEntry dataDefinitionEntry) {
            return true;
        }

        public virtual bool Visit(DataDescriptionEntry dataDescriptionEntry) {
            return true;
        }

        public virtual bool Visit(SpecialRegisterDescriptionEntry specialRegisterDescriptionEntry) {
            return true;
        }

        public virtual bool Visit(FunctionCallResultDescriptionEntry functionCallResultDescriptionEntry) {
            return true;
        }

        public virtual bool Visit(ParameterDescriptionEntry parameterDescriptionEntry) {
            return true;
        }

        public virtual bool Visit(DataTypeDescriptionEntry dataTypeDescriptionEntry) {
            return true;
        }

        public virtual bool Visit(DataRedefinesEntry dataRedefinesEntry) {
            return true;
        }

        public virtual bool Visit(DataRenamesEntry dataRenamesEntry) {
            return true;
        }

        public virtual bool Visit(DataConditionEntry dataConditionEntry) {
            return true;
        }

        public virtual bool Visit(DataType dataType) {
            return true;
        }

        public virtual bool Visit(TableSortingKey tableSortingKey) {
            return true;
        }

        public virtual bool Visit(ValuesRange valuesRange) {
            return true;
        }

        public virtual bool Visit(CodeElement codeElement) {
            return true;
        }

        public virtual bool Visit(FunctionDeclarationEnd functionDeclarationEnd) {
            return true;
        }

        public virtual bool Visit(FunctionDeclarationHeader functionDeclarationHeader) {
            return true;
        }

        public virtual bool Visit(StatementElement statementElement) {
            return true;
        }

        public virtual bool VisitVariableWriter(VariableWriter variableWriter) {
            return true;
        }

        public virtual bool VisitFunctionCaller(FunctionCaller functionCaller) {
            return true;
        }

        public virtual bool Visit(SetSendingVariable setSendingVariable) {
            return true;
        }

        public virtual bool Visit(SetUPSISwitchInstruction setUPSISwitchInstruction) {
            return true;
        }

        public virtual bool Visit(ParametersProfile parametersProfile) {
            return true;
        }

        public virtual bool Visit(Section section) {
            return true;
        }

        public virtual bool Visit(Paragraph paragraph) {
            return true;
        }

        public virtual bool Visit(Perform perform) {
            return true;
        }

        public virtual bool Visit(PerformProcedure performProcedure) {
            return true;
        }

        public virtual bool Visit(SourceFile root) {
            return true;
        }

        public virtual bool Visit(LibraryCopy libraryCopy) {
            return true;
        }

        public virtual bool Visit(Factory factory) {
            return true;
        }
        public virtual bool Visit(Object objectNode) {
            return true;
        }

        public virtual bool Visit(End end) {
            return true;
        }

        public virtual bool Visit(Accept accept) {
            return true;
        }

        public virtual bool Visit(Alter alter) {
            return true;
        }

        public virtual bool Visit(Call call) {
            return true;
        }

        public virtual bool Visit(ProcedureStyleCall procedureStyleCall) {
            return true;
        }

        public virtual bool Visit(Cancel cancel) {
            return true;
        }

        public virtual bool Visit(Continue continueNode) {
            return true;
        }

        public virtual bool Visit(Delete delete) {
            return true;
        }

        public virtual bool Visit(Display display) {
            return true;
        }

        public virtual bool Visit(Entry entry) {
            return true;
        }

        public virtual bool Visit(Exec exec) {
            return true;
        }

        public virtual bool Visit(Exit exit) {
            return true;
        }

        public virtual bool Visit(ExitMethod exitMethod) {
            return true;
        }

        public virtual bool Visit(ExitProgram exitProgram) {
            return true;
        }

        public virtual bool Visit(Goback goback) {
            return true;
        }

        public virtual bool Visit(Goto gotoNode) {
            return true;
        }

        public virtual bool Visit(Initialize initialize) {
            return true;
        }

        public virtual bool Visit(Inspect inspect) {
            return true;
        }

        public virtual bool Visit(Invoke invoke) {
            return true;
        }

        public virtual bool Visit(Merge merge) {
            return true;
        }

        public virtual bool Visit(Move move) {
            return true;
        }

        public virtual bool Visit(Release release) {
            return true;
        }

        public virtual bool Visit(Return returnNode) {
            return true;
        }

        public virtual bool Visit(Set set) {
            return true;
        }

        public virtual bool Visit(Sort sort) {
            return true;
        }

        public virtual bool Visit(Start start) {
            return true;
        }

        public virtual bool Visit(Stop stop) {
            return true;
        }

        public virtual bool Visit(String stringNode) {
            return true;
        }

        public virtual bool Visit(Unstring unstring) {
            return true;
        }

        public virtual bool Visit(XmlGenerate xmlGenerate) {
            return true;
        }

        public virtual bool Visit(XmlParse xmlParse) {
            return true;
        }

        public virtual bool Visit(Add add) {
            return true;
        }

        public virtual bool Visit(Subtract subtract) {
            return true;
        }

        public virtual bool Visit(Multiply multiply) {
            return true;
        }

        public virtual bool Visit(Divide divide) {
            return true;
        }

        public virtual bool Visit(Compute compute) {
            return true;
        }

        public virtual bool Visit(Open open) {
            return true;
        }

        public virtual bool Visit(Close close) {
            return true;
        }

        public virtual bool Visit(Read read) {
            return true;
        }

        public virtual bool Visit(Write write) {
            return true;
        }

        public virtual bool Visit(Rewrite rewrite) {
            return true;
        }

        public virtual bool Visit(If ifNode) {
            return true;
        }

        public virtual bool Visit(Then then) {
            return true;
        }

        public virtual bool Visit(Else elseNode) {
            return true;
        }

        public virtual bool Visit(NextSentence nextSentence) {
            return true;
        }

        public virtual bool Visit(Evaluate evaluate) {
            return true;
        }

        public virtual bool Visit(WhenGroup whenGroup) {
            return true;
        }

        public virtual bool Visit(When when) {
            return true;
        }

        public virtual bool Visit(WhenOther whenOther) {
            return true;
        }

        public virtual bool Visit(Search search) {
            return true;
        }

        public virtual bool Visit(WhenSearch whenSearch) {
            return true;
        }

        public virtual bool Visit(DataSection dataSection) {
            return true;
        }

        public virtual bool Visit(DataDivision dataDivision) {
            return true;
        }

        public virtual bool Visit(FileSection fileSection) {
            return true;
        }

        public virtual bool Visit(FileDescriptionEntryNode fileDescriptionEntryNode) {
            return true;
        }

        public virtual bool Visit(WorkingStorageSection workingStorageSection) {
            return true;
        }

        public virtual bool Visit(LocalStorageSection localStorageSection) {
            return true;
        }

        public virtual bool Visit(LinkageSection linkageSection) {
            return true;
        }

        public virtual bool Visit(DataDefinition dataDefinition) {
            return true;
        }

        public virtual bool Visit(DataDescription dataDescription) {
            return true;
        }

        public virtual bool Visit(DataCondition dataCondition) {
            return true;
        }

        public virtual bool Visit(DataRedefines dataRedefines) {
            return true;
        }

        public virtual bool Visit(DataRenames dataRenames) {
            return true;
        }

        public virtual bool Visit(TypeDefinition typeDefinition) {
            return true;
        }

        public virtual bool Visit(ParameterDescription parameterDescription) {
            return true;
        }

        public virtual bool Visit(EnvironmentDivision environmentDivision) {
            return true;
        }

        public virtual bool Visit(ConfigurationSection configurationSection) {
            return true;
        }

        public virtual bool Visit(SourceComputer sourceComputer) {
            return true;
        }

        public virtual bool Visit(ObjectComputer objectComputer) {
            return true;
        }

        public virtual bool Visit(SpecialNames specialNames) {
            return true;
        }

        public virtual bool Visit(Repository repository) {
            return true;
        }

        public virtual bool Visit(InputOutputSection inputOutputSection) {
            return true;
        }

        public virtual bool Visit(FileControlParagraphHeaderNode fileControlParagraphHeaderNode) {
            return true;
        }

        public virtual bool Visit(FileControlEntryNode fileControlEntryNode) {
            return true;
        }

        public virtual bool Visit(OnSizeError onSizeError) {
            return true;
        }

        public virtual bool Visit(NoSizeError noSizeError) {
            return true;
        }

        public virtual bool Visit(OnAtEnd onAtEnd) {
            return true;
        }

        public virtual bool Visit(NoAtEnd noAtEnd) {
            return true;
        }

        public virtual bool Visit(OnException onException) {
            return true;
        }

        public virtual bool Visit(NoException noException) {
            return true;
        }

        public virtual bool Visit(OnInvalidKey onInvalidKey) {
            return true;
        }

        public virtual bool Visit(NoInvalidKey noInvalidKey) {
            return true;
        }

        public virtual bool Visit(OnOverflow onOverflow) {
            return true;
        }

        public virtual bool Visit(NoOverflow noOverflow) {
            return true;
        }

        public virtual bool Visit(ProcedureDivision procedureDivision) {
            return true;
        }

        public virtual bool Visit(FunctionDeclaration functionDeclaration) {
            return true;
        }

        public virtual bool Visit(FunctionEnd functionEnd) {
            return true;
        }

        public virtual bool Visit(Sentence sentence) {
            return true;
        }

        public virtual bool Visit(RoundedResult roundedResult) {
            return true;
        }

        public virtual bool Visit(CloseFileInstruction closeFileInstruction) {
            return true;
        }

        public virtual bool Visit(IntrinsicStorageArea intrinsicStorageArea) {
            return true;
        }

        public virtual bool Visit(Program program)
        {
            return true;
        }

        public virtual bool Visit(Nodes.Class classNode)
        {
            return true;
        }

        public virtual bool Visit(Nodes.Method method)
        {
            return true;
        }

        public virtual bool Visit(ParametersProfileNode profile)
        {
            return true;
        }

        public virtual bool Visit([NotNull] IndexDefinition indexDefinition)
        {
            return true;
        }
    }




    public  class Cobol85Visitor : AbstractAstVisitor {
        protected internal bool NeedGeneration { get; set; }
        private Node CurrentNode { get; set; }

        public override bool BeginNode(Node node) {
            NeedGeneration = false;
            CurrentNode = node;
            return !node.NeedGeneration;
        }

        public override void EndNode(Node node) {
            CurrentNode.NeedGeneration = NeedGeneration;
        }


        public override bool Visit(MoveStatement moveStatement) {
            return VisitVariableWriter(moveStatement);
        }

        public override bool VisitVariableWriter(VariableWriter variableWriter) {
            if (variableWriter.IsUnsafe) {
                NeedGeneration = true;
            }
            return !NeedGeneration;
        }

        public override bool Visit(FunctionCall functionCall) {
            if (functionCall is UserDefinedFunctionCall || functionCall is ProcedureCall) {
                NeedGeneration = true;
            }
            return !NeedGeneration;
        }

        public override bool Visit(Token token) {
            var tokenLanguageLevel = TokenConst.GetCobolLanguageLevel(token.TokenType);
            if (tokenLanguageLevel > CobolLanguageLevel.Cobol85)
            {
                NeedGeneration = true;
            }
            return !NeedGeneration;
        }

        public override bool Visit(SymbolInformation symbolInformation) {
            var symbolInfoLanguageLevel = SymbolTypeUtils.GetCobolLanguageLevel(symbolInformation.Type);
            if (symbolInfoLanguageLevel > CobolLanguageLevel.Cobol85)
            {
                NeedGeneration = true;
            }
            return !NeedGeneration;
        }

        public override bool Visit(TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference)
        {
            NeedGeneration = true;
            return false;
        }

        public override bool Visit(DataType dataType) {
            if (dataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85)
            {
                NeedGeneration = true;
            }
            return !NeedGeneration;
        }
    }

    /// <summary>
    /// Visitor which mark all node that must be generated to target the IBM Z/OS Cobol V5 compiler
    /// </summary>
    public class IBMZOSCobolV5Compiler : AbstractAstVisitor
    {
        protected internal bool NeedGeneration { get; set; }
        private Node CurrentNode { get; set; }

        public override bool BeginNode(Node node) {
            NeedGeneration = false;
            CurrentNode = node;
            return !node.NeedGeneration;
        }

        public override void EndNode(Node node)
        {
            CurrentNode.NeedGeneration = NeedGeneration;
        }


        public override bool BeginCodeElement(CodeElement codeElement) {
            //TODO incremental mode: find a way to store the information into the CodeElement
            //Determine if we allo to generate the same Nodes to multiple target: 
            //Cobol 85, Cobol 2002, IBM Z/OS Cobol V6 Compiler, ...
            return base.BeginCodeElement(codeElement);
        }

        public override bool VisitVariableWriter(VariableWriter variableWriter)
        {
            if (variableWriter.IsUnsafe) {
                NeedGeneration = true;
                return false;
            }
            //TODO analyse variables written
            return true;
        }

        public override bool Visit(FunctionCall functionCall)
        {
            if (functionCall is UserDefinedFunctionCall || functionCall is ProcedureCall) {
                NeedGeneration = true;
                return false;
            }
            return true;
        }

        public override bool Visit(Token token)
        {
            var tokenLanguageLevel = TokenConst.GetCobolLanguageLevel(token.TokenType);
            if (tokenLanguageLevel >= CobolLanguageLevel.Cobol2002) {
                NeedGeneration = true;
                return false;
            }
            return true;
        }

        public override bool Visit(SymbolInformation symbolInformation)
        {
            var symbolInfoLanguageLevel = SymbolTypeUtils.GetCobolLanguageLevel(symbolInformation.Type);
            if (symbolInfoLanguageLevel >= CobolLanguageLevel.Cobol2002)
            {
                NeedGeneration = true;
                return false;
            }
            return true;
        }

        public override bool Visit(TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference)
        {
            NeedGeneration = true;
            return false;
        }

        public override bool Visit(DataType dataType)
        {
            if (dataType.CobolLanguageLevel >= CobolLanguageLevel.Cobol2002)
            {
                NeedGeneration = true;
                return false;
            }
            return true;
        }

        public override bool Visit(FunctionDeclarationEnd functionDeclarationEnd) {
            NeedGeneration = true;
            return false;
        }

        public override bool Visit(FunctionDeclarationHeader functionDeclarationHeader) {
            NeedGeneration = true;
                return false;
        }
    }
}

