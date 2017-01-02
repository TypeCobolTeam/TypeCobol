
using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

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

        bool BeginNode([NotNull] Node node);
        void EndNode([NotNull] Node node);

        bool BeginCodeElement([NotNull] CodeElement codeElement);
        void EndCodeElement([NotNull] CodeElement codeElement);


        bool Visit(AcceptStatement acceptStatement);
        bool Visit(AcceptFromInputDeviceStatement acceptFromInputDeviceStatement);
        bool Visit(AcceptFromSystemDateStatement acceptFromSystemDateStatement);
        bool Visit(AddSimpleStatement addSimpleStatement);
        bool Visit(AddGivingStatement addGivingStatement);
        bool Visit(AddCorrespondingStatement addCorrespondingStatement);
        bool Visit(AlterStatement alterStatement);
        bool Visit(CallStatement callStatement);
        bool Visit(CancelStatement cancelStatement);
        bool Visit(CloseStatement closeStatement);
        bool Visit(ComputeStatement computeStatement);
        bool Visit(ContinueStatement continueStatement);
        bool Visit(DeleteStatement deleteStatement);
        bool Visit(DisplayStatement displayStatement);
        bool Visit(DivideStatement divideStatement);
        bool Visit(EntryStatement entryStatement);
        bool Visit(EvaluateStatement evaluateStatement);
        bool Visit(ExecStatement execStatement);
        bool Visit(ExitMethodStatement exitMethodStatement);
        bool Visit(ExitProgramStatement exitProgramStatement);
        bool Visit(ExitStatement exitStatement);
        bool Visit(GobackStatement gobackStatement);
        bool Visit(GotoStatement gotoStatement);
        bool Visit(IfStatement ifStatement);
        bool Visit(InitializeStatement initializeStatement);
        bool Visit(InspectStatement inspectStatement);
        bool Visit(InvokeStatement invokeStatement);
        bool Visit(MergeStatement mergeStatement);
        bool Visit(MoveStatement moveStatement);
        bool Visit(MoveSimpleStatement moveStatement);

        bool Visit(MoveCorrespondingStatement moveStatement);

        bool Visit(MultiplyStatement multiplyStatement);
        bool Visit(OpenStatement openStatement);
        bool Visit(PerformProcedureStatement performProcedureStatement);
        bool Visit(PerformStatement performStatement);
        bool Visit(ReadStatement readStatement);
        bool Visit(ReleaseStatement releaseStatement);
        bool Visit(ReturnStatement returnStatement);
        bool Visit(RewriteStatement rewriteStatement);
        bool Visit(SearchStatement searchStatement);
        bool Visit(SetStatement setStatement);
        bool Visit(SortStatement sortStatement);
        bool Visit(StartStatement startStatement);
        bool Visit(StopStatement stopStatement);
        bool Visit(StringStatement stringStatement);
        bool Visit(SubtractStatement substractStatement);
        bool Visit(UnstringStatement unstringStatement);
        bool Visit(WriteStatement writeStatement);
        bool Visit(XmlGenerateStatement xmlGenerateStatement);
        bool Visit(XmlParseStatement xmlParseStatement);


        //Expressions
        bool Visit(ReceivingStorageArea receivingStorageArea);

        bool Visit(NumericVariable numericVariable);
        bool Visit(NumericVariable[] numericVariable);

        bool Visit(NumericValue numericValue);
        bool Visit(GeneratedNumericValue generatedNumericValue);
//        bool Visit(StorageDataType storageDataType);
        bool Visit(StorageArea storageArea);
        bool Visit(FunctionCallResult functionCallResult);
        bool Visit(FilePropertySpecialRegister filePropertySpecialRegister);
        bool Visit(IndexStorageArea indexStorageArea);
        bool Visit(StorageAreaPropertySpecialRegister storageAreaPropertySpecialRegister);
        bool Visit(DataOrConditionStorageArea storageArea);
        bool Visit(SymbolReference symbolReference);
        bool Visit(AmbiguousSymbolReference ambiguousSymbolReference);
        bool Visit(ReferenceModifier referenceModifier);
        bool Visit(ExternalNameOrSymbolReference externalNameOrSymbolReference);

        bool Visit(FunctionCall functionCall);
        bool Visit(IntrinsicFunctionCall functionCall);
        bool Visit(UserDefinedFunctionCall functionCall);
        bool Visit(ProcedureCall functionCall);
        bool Visit(BooleanValue booleanValue);
        bool Visit(GeneratedBooleanValue generatedBooleanValue);
        bool Visit(Variable variable);
        bool Visit(VariableOrExpression variableOrExpression);
        bool Visit(Token token);
        bool Visit(CallSiteParameter callSiteParameter);
        bool Visit(ExternalName externalName);
        bool Visit(QualifiedTextName qualifiedTextName);
        bool Visit(SyntaxProperty<bool> syntaxProperty);
        bool Visit(SymbolInformation symbolInformation);
        bool Visit(SymbolDefinitionOrReference symbolDefinitionOrReference);
        bool Visit(SymbolDefinition symbolDefinition);
        bool Visit(QualifiedSymbolReference qualifiedSymbolReference);
        bool Visit(TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference);
        bool Visit(SyntaxValue<string> syntaxValue);
        bool Visit(AlphanumericValue alphanumericValue);
        bool Visit(GeneratedAlphanumericValue generatedAlphanumericValue);
        bool Visit(GeneratedSymbolName generatedSymbolName);
        bool Visit(EnumeratedValue enumeratedValue);
        bool Visit(CharacterValue characterValue);
        bool Visit(RepeatedCharacterValue repeatedCharacterValue);
        bool Visit(SymbolType symbolType);
        bool Visit(Expression expression);
        bool Visit(ArithmeticExpression arithmeticExpression);
        bool Visit(NumericVariableOperand numericVariableOperand);
        bool Visit(ArithmeticOperation arithmeticOperation);
        bool Visit(ConditionalExpression conditionalExpression);
        bool Visit(SignCondition signCondition);
        bool Visit(RelationCondition relationCondition);
        bool Visit(LogicalOperation logicalOperation);
        bool Visit(ClassCondition classCondition);
        bool Visit(ConditionNameConditionOrSwitchStatusCondition conditionNameConditionOrSwitchStatusCondition);
        bool Visit(VariableBase variableBase);
        bool Visit(SymbolReferenceVariable symbolReferenceVariable);
        bool Visit(AlphanumericVariable alphanumericVariable);
        bool Visit(CharacterVariable characterVariable);
        bool Visit(IntegerVariable integerVariable);
        bool Visit<T>(SyntaxValue<T> syntaxValue);
        bool Visit(NullPointerValue nullPointerValue);
        bool Visit(IntegerValue integerValue);
        bool Visit(GeneratedIntegerValue generatedIntegerValue);
        bool Visit<T>(SyntaxProperty<T> syntaxProperty);

        bool Visit(CallSite callSite);
        bool Visit(CallTarget callTarget);
        bool Visit(CallTargetParameter callTargetParameter);
        bool Visit(SubscriptExpression subscriptExpression);
        bool Visit(Value value);
        bool Visit(GroupCorrespondingImpact groupCorrespondingImpact);
        bool Visit(BooleanValueOrExpression booleanValueOrExpression);
        bool Visit(DataDefinitionEntry dataDefinitionEntry);
        bool Visit(DataDescriptionEntry dataDescriptionEntry);
        bool Visit(SpecialRegisterDescriptionEntry specialRegisterDescriptionEntry);
        bool Visit(FunctionCallResultDescriptionEntry functionCallResultDescriptionEntry);
        bool Visit(ParameterDescriptionEntry parameterDescriptionEntry);
        bool Visit(DataTypeDescriptionEntry dataTypeDescriptionEntry);
        bool Visit(DataRedefinesEntry dataRedefinesEntry);
        bool Visit(DataRenamesEntry dataRenamesEntry);
        bool Visit(DataConditionEntry dataConditionEntry);
        bool Visit(DataType dataType);
        bool Visit(TableSortingKey tableSortingKey);
        bool Visit(ValuesRange valuesRange);
        bool Visit(CodeElement codeElement);
        bool Visit(FunctionDeclarationEnd functionDeclarationEnd);
        bool Visit(FunctionDeclarationHeader functionDeclarationHeader);
        bool Visit(StatementElement statementElement);
        bool VisitVariableUser(VariableUser variableUser);
        bool VisitVariableWriter(VariableWriter variableWriter);
        bool VisitFunctionCaller(FunctionCaller functionCaller);
        bool Visit(SetSendingVariable setSendingVariable);
        bool Visit(SetUPSISwitchInstruction setUPSISwitchInstruction);
        bool Visit(ParametersProfile parametersProfile);
    }



    public abstract class AbstractAstVisitor : IASTVisitor {
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

        public virtual bool VisitVariableUser(VariableUser variableUser) {
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

        public override bool VisitVariableUser(VariableUser variableUser) {
            //TODO
            return base.VisitVariableUser(variableUser);
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
