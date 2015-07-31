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
                if (conditions.Count > 0)
                {
                    LogicalExpression left = createCondition(conditions[0]);
                    if (conditions.Count > 1)
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
                if (conditions.Count > 0)
                {
                    LogicalExpression left = createCondition(conditions[0]);
                    if (conditions.Count > 1)
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
            System.Console.WriteLine("TODO: IMPLEMENT CLASS CONDITIONS");
            throw new System.NotImplementedException("ClassCondition not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ConditionNameConditionContext context)
        {
            // we can take first, as conditionName is a UserDefinedWord
            return new Condition(ParseTreeUtils.GetFirstToken(context.conditionName()));
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

        private char createOperator(CobolCodeElementsParser.RelationalOperatorContext context)
        {
            var simple = context.simpleRelation();
            if (simple != null)
            {
                if (simple.GreaterThanOrEqualOperator() != null) return '≥';
                if (simple.LessThanOrEqualOperator() != null) return '≤';
                if (simple.OR() == null) ;//TODO diagnostic about missing word
                if (simple.EQUAL() == null) ;//TODO diagnostic about missing word
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

        private Expression createOperand(CobolCodeElementsParser.OperandContext context)
        {
            if (context.identifier() != null) return new Identifier(context.identifier());
            System.Console.WriteLine("TODO: IMPLEMENT NON-IDENTIFIER OPERANDS "+(context.identifier() != null) + " " + (context.literal() != null) + " " + (context.functionIdentifier() != null) + " " + (context.arithmeticExpression() != null) + " " + (context.indexName() != null));
            throw new System.NotImplementedException("operands not implemented");
        }

        private LogicalExpression createAbbreviatedRelation(LogicalExpression relation, CobolCodeElementsParser.AbbreviatedRelationContext[] relations)
        {
            System.Console.WriteLine("TODO: "+ relations.Length + " abbreviated relations");
            throw new System.NotImplementedException("abbreviated relations not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.GeneralRelationConditionContext context)
        {
            LogicalExpression relation = null;
            char op = createOperator(context.relationalOperator());
            var operands = context.operand();
            if (operands != null && operands.Count > 0)
            {
                Expression left = createOperand(operands[0]);
                if (operands.Count > 1)
                {
                    Expression right = createOperand(operands[1]);
                    relation = new Relation(left, op, right);
                }
                else
                {
                    LogicalExpression logical = left as LogicalExpression;
                    if (logical != null)
                    {
                        relation = logical;
                    }
                    else
                    {
                        System.Console.WriteLine("TODO: Illogical expression \""+left+"\" sole part of a LogicalExpression !");
                        throw new System.NotImplementedException("Illogical expression \"" + left + "\" sole part of a LogicalExpression");
                    }
                }
            }
            var relations = context.abbreviatedRelation();
            if (relations != null && relations.Count > 0)
            {
                return createAbbreviatedRelation(relation, relations.ToArray());
            }
            return relation;
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.DataPointerRelationConditionContext context)
        {
            System.Console.WriteLine("TODO: IMPLEMENT DATA POINTER RELATION CONDITIONS");
            throw new System.NotImplementedException("RelationCondition not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ProgramPointerRelationConditionContext context)
        {
            System.Console.WriteLine("TODO: IMPLEMENT PROGRAM POINTER RELATION CONDITIONS");
            throw new System.NotImplementedException("RelationCondition not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.ObjectReferenceRelationConditionContext context)
        {
            System.Console.WriteLine("TODO: IMPLEMENT OBJECT REFERENCE RELATION CONDITIONS");
            throw new System.NotImplementedException("RelationCondition not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.SignConditionContext context)
        {
            System.Console.WriteLine("TODO: IMPLEMENT SIGN CONDITIONS");
            throw new System.NotImplementedException("SignCondition not implemented");
        }

        private LogicalExpression createCondition(CobolCodeElementsParser.SwitchStatusConditionContext context)
        {
            System.Console.WriteLine("TODO: IMPLEMENT SWITCH STATUS CONDITIONS");
            throw new System.NotImplementedException("SwitchStatusCondition not implemented");
        }
    }
}