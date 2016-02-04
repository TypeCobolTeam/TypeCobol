using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;
using System.Linq;

namespace TypeCobol.Compiler.Parser
{
    class LogicalExpressionBuilder
    {

        public LogicalExpression createCondition(CobolCodeElementsParser.ConditionalExpressionContext context)
        {
            if (context.simpleCondition() != null)
                return createCondition(context.simpleCondition());
            if (context.complexCondition() != null)
                return createCondition(context.complexCondition());
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ComplexConditionContext context)
        {
            if (context.andCondition() != null)
            {
                var conditions = context.andCondition();
                if (conditions.Length > 0)
                {
                    LogicalExpression left = createCondition(conditions[0]);
                    if (conditions.Length > 1)
                    {
                        LogicalExpression right = createCondition(conditions[1]);
                        return new OR(left, right);
                    }
                    return left;
                }
            }
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.AndConditionContext context)
        {
            if (context.notCondition() != null)
            {
                var conditions = context.notCondition();
                if (conditions.Length > 0)
                {
                    LogicalExpression left = createCondition(conditions[0]);
                    if (conditions.Length > 1)
                    {
                        LogicalExpression right = createCondition(conditions[1]);
                        return new AND(left, right);
                    }
                    return left;
                }
            }
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.NotConditionContext context)
        {
            if (context.conditionBase() != null)
            {
                if (context.NOT() != null)
                {
                    return new NOT(createCondition(context.conditionBase()));
                }
                else
                {
                    return createCondition(context.conditionBase());
                }
            }
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ConditionBaseContext context)
        {
            if (context.simpleCondition() != null)
                return createCondition(context.simpleCondition());
            if (context.complexCondition() != null)
                return createCondition(context.complexCondition());
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.SimpleConditionContext context)
        {
            if (context.classCondition() != null)
                return createCondition(context.classCondition());
            if (context.conditionNameCondition() != null)
                return createCondition(context.conditionNameCondition());
            if (context.relationCondition() != null)
                return createCondition(context.relationCondition());
            if (context.signCondition() != null)
                return createCondition(context.signCondition());
            if (context.switchStatusCondition() != null)
                return createCondition(context.switchStatusCondition());
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ClassConditionContext context)
        {
            Symbol type = null;
            if (context.charsetClassName() != null) type = new CharsetClassName(ParseTreeUtils.GetFirstToken(context.charsetClassName()));
            if (context.NUMERIC() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.NUMERIC()));
            if (context.ALPHABETIC() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.ALPHABETIC()));
            if (context.ALPHABETIC_LOWER() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.ALPHABETIC_LOWER()));
            if (context.ALPHABETIC_UPPER() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.ALPHABETIC_UPPER()));
            if (context.DBCS() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.DBCS()));
            if (context.KANJI() != null) type = new ClassName(ParseTreeUtils.GetFirstToken(context.KANJI()));
            LogicalExpression condition = new ClassCondition(SyntaxElementBuilder.CreateIdentifier(context.identifier()), type);
            if (context.NOT() != null) condition = new NOT(condition);
            return condition;
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ConditionNameConditionContext context)
        {
            if (context.conditionNameReference() == null) return new Empty();
            return createCondition(context.conditionNameReference());
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ConditionNameReferenceContext context)
        {
            if (context == null) return new Empty();
            QualifiedName conditionname = SyntaxElementBuilder.CreateQualifiedName(context.qualifiedConditionName());
            IList<Subscript> subscripts = SyntaxElementBuilder.CreateSubscripts(context.subscript());
            return new Condition(conditionname, subscripts);
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.QualifiedConditionNameContext context)
        {
            if (context == null) return new Empty();
            return new Condition(SyntaxElementBuilder.CreateQualifiedName(context));
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.RelationConditionContext context)
        {
            if (context.generalRelationCondition() != null)
                return createCondition(context.generalRelationCondition());
            if (context.dataPointerRelationCondition() != null)
                return createCondition(context.dataPointerRelationCondition());
            if (context.programPointerRelationCondition() != null)
                return createCondition(context.programPointerRelationCondition());
            if (context.objectReferenceRelationCondition() != null)
                return createCondition(context.objectReferenceRelationCondition());
            return new Empty();
        }

        internal char CreateOperator(CobolCodeElementsParser.RelationalOperatorContext context)
        {
            if (context == null) return '?';
            var simple = context.simpleRelation();
            if (simple != null)
            {
                if (simple.GreaterThanOrEqualOperator() != null) return '≥';
                if (simple.LessThanOrEqualOperator() != null) return '≤';
                if (simple.GREATER() != null) return '≥';
                if (simple.LESS() != null) return '≤';
            }
            bool inverted = context.NOT() != null;
            var strict = context.strictRelation();
            if (strict != null) {
                if (!inverted)
                {
                    if (strict.GreaterThanOperator() != null) return '>';
                    if (strict.LessThanOperator() != null) return '<';
                    if (strict.GREATER() != null) return '>';
                    if (strict.LESS() != null) return '<';
                    if (strict.EqualOperator() != null) return '=';
                    if (strict.EQUAL() != null) return '=';
                }
                else
                {
                    if (strict.GreaterThanOperator() != null) return '≤';
                    if (strict.LessThanOperator() != null) return '≥';
                    if (strict.GREATER() != null) return '≤';
                    if (strict.LESS() != null) return '≥';
                    if (strict.EqualOperator() != null) return '!';
                    if (strict.EQUAL() != null) return '!';
                }
            }
            return '?';
        }

        internal char invertOperator(char op) {
            switch(op) {
                case '>': return '≤';
                case '≥': return '<';
                case '<': return '≥';
                case '≤': return '>';
                case '=': return '!';
                case '!': return '=';
                default: return '?';
            }
        }

        private Expression createOperand(CobolCodeElementsParser.OperandContext context)
        {
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return SyntaxElementBuilder.CreateLiteral(context.literal());
            if (context.arithmeticExpression() != null) return new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression());
            if (context.indexName() != null) return new Index(new IndexName(ParseTreeUtils.GetTokenFromTerminalNode(context.indexName().UserDefinedWord())));
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.GeneralRelationConditionContext context)
        {
            if (context == null) return new Empty();
            Expression left = createOperand(context.operand());
            char op = CreateOperator(context.relationalOperator());
            return CreateOperation(left, op, context.abbreviatedOR());
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.DataPointerRelationConditionContext context)
        {
            if (context.dataPointer() != null && context.dataPointer().Length > 0)
            {
                var first = context.dataPointer().ElementAt(0);
                Expression left = createOperand(context.dataPointer().ElementAt(0));
                if (first.ADDRESS() != null) left = new Pointer((IdentifierOld)left);

                char op = createOperator(context.relationConditionEquality());

                Expression right = null;
                if (context.dataPointer().Length > 1)
                {
                    var second = context.dataPointer().ElementAt(1);
                    right = createOperand(second);
                    if (second.ADDRESS() != null) right = new Pointer((IdentifierOld)right);
                }
                return LogicOperation.Create(left, op, right);
            }
            return new Empty();
        }

        private char createOperator(CobolCodeElementsParser.RelationConditionEqualityContext context)
        {
            if (context == null) return '?';
            if (context.NOT() != null) return '!';
            if (context.EqualOperator() != null || context.EQUAL() != null) return '=';
            return '?';
        }

        private Expression createOperand(CobolCodeElementsParser.DataPointerContext context)
        {
            if (context.NULL() != null || context.NULLS() != null) return new Null();
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ProgramPointerRelationConditionContext context)
        {
            if (context.procedureOrFunctionPointer() != null && context.procedureOrFunctionPointer().Length > 0)
            {
                Expression left = createOperand(context.procedureOrFunctionPointer().ElementAt(0));

                char op = createOperator(context.relationConditionEquality());

                Expression right = null;
                if (context.procedureOrFunctionPointer().Length > 1)
                {
                    right = createOperand(context.procedureOrFunctionPointer().ElementAt(1));
                }
                return LogicOperation.Create(left, op, right);
            }
            return new Empty();
        }

        private Expression createOperand(CobolCodeElementsParser.ProcedureOrFunctionPointerContext context)
        {
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.NULL() != null || context.NULLS() != null) return new Null();
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ObjectReferenceRelationConditionContext context)
        {
            if (context.objectReference() != null && context.objectReference().Length > 0)
            {
                Expression left = createOperand(context.objectReference().ElementAt(0));

                char op = createOperator(context.relationConditionEquality());

                Expression right = null;
                if (context.objectReference().Length > 1)
                {
                    right = createOperand(context.objectReference().ElementAt(1));
                }
                return LogicOperation.Create(left, op, right);
            }
            return new Empty();
        }

        private Expression createOperand(CobolCodeElementsParser.ObjectReferenceContext context)
        {
            if (context == null) return new Empty();
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.NULL() != null || context.NULLS() != null) return new Null();
            if (context.SELF() != null) return new Self();
            return new Empty();
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.SignConditionContext context)
        {
            if (context == null) return new Empty();
            Expression operand = createOperand(context.operand());
            bool not = context.NOT() != null;
            if (context.ZERO() != null) return new SignCondition(operand, not, SignCondition.Type.ZERO);
            if (context.POSITIVE() != null) return new SignCondition(operand, not, SignCondition.Type.POSITIVE);
            if (context.NEGATIVE() != null) return new SignCondition(operand, not, SignCondition.Type.NEGATIVE);
            return new SignCondition(operand, not, SignCondition.Type.UNKNOWN);
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.SwitchStatusConditionContext context)
        {
            return createCondition(context.qualifiedConditionName());
        }





        internal LogicOperation CreateOperation(Expression left, char op, CobolCodeElementsParser.AbbreviatedORContext context)
        {
            if (context == null) return null;
            LogicOperation operation = null;
            int length = context.abbreviatedAND().Length;
            if (length > 0) {
                operation = createOperation(left, op, context.abbreviatedAND()[0]);
                for (int c = 1; c < length; c++) {
                    LogicOperation current = createOperation(left, op, context.abbreviatedAND()[c]);
                    operation = new OR(operation, current);
                }
            }
            return operation;
        }

        private LogicOperation createOperation(Expression left, char op, CobolCodeElementsParser.AbbreviatedANDContext context)
        {
            if (context == null) return null;
            LogicOperation operation = null;
            int length = context.abbreviatedNOT().Length;
            if (length > 0) {
                operation = createOperation(left, op, context.abbreviatedNOT()[0]);
                for (int c = 1; c < length; c++) {
                    LogicOperation current = createOperation(left, op, context.abbreviatedNOT()[c]);
                    operation = new AND(operation, current);
                }
            }
            return operation;
        }

        private LogicOperation createOperation(Expression left, char op, CobolCodeElementsParser.AbbreviatedNOTContext context)
        {
            return createOperation(left, (context.NOT() != null), op, context.abbreviatedExpression());
        }

        private LogicOperation createOperation(Expression left, bool not, char op, CobolCodeElementsParser.AbbreviatedExpressionContext context)
        {
            if (context == null) return null;
            if (context.abbreviatedOperand() != null ) return createOperand(left, not, op, context.abbreviatedOperand());
            if (context.abbreviatedOR() != null) return CreateOperation(left, op, context.abbreviatedOR());
            return null;
        }

        private Relation createOperand(Expression left, bool not, char op, CobolCodeElementsParser.AbbreviatedOperandContext context)
        {
            if (context.relationalOperator() != null) op = CreateOperator(context.relationalOperator());
            if (not) op = invertOperator(op);
            Expression right = createOperand(context.operand());
            return new Relation(left, op, right);
        }
    }
}