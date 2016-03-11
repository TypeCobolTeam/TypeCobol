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

        public LogicalExpression createCondition(CodeElementsParser.ConditionalExpressionContext context) {
            var conditions = new List<LogicalExpression>();
            foreach(var terminal in context.conditionalExpression()) conditions.Add(createCondition(terminal));
            if (conditions.Count < 1) {
                // simple condition
                if (context.classCondition()           != null) return createCondition(context.classCondition());
                if (context.conditionNameConditionOrSwitchStatusCondition()   != null) return createCondition(context.conditionNameConditionOrSwitchStatusCondition());
                if (context.generalRelationCondition() != null) return createCondition(context.generalRelationCondition());
                if (context.pointerRelationCondition() != null) return createCondition(context.pointerRelationCondition());
                if (context.signCondition()            != null) return createCondition(context.signCondition());
            } else // complex condition
            if (conditions.Count == 1) {
                if (context.NOT() == null) return conditions[0]; // (x)
                return new NOT(conditions[0]); // NOT x
            } else
            if (conditions.Count > 1 ) {
                if (context.AND() != null) return new AND(conditions[0], conditions[1]);
                return new OR(conditions[0], conditions[1]);
            }
            throw new System.NotSupportedException("Uh-oh!");
        }

        //private LogicalExpression createCondition(CodeElementsParser.ComplexConditionContext context)
        //{
        //    if (context.andCondition() != null)
        //    {
        //        var conditions = context.andCondition();
        //        if (conditions.Length > 0)
        //        {
        //            LogicalExpression left = createCondition(conditions[0]);
        //            if (conditions.Length > 1)
        //            {
        //                LogicalExpression right = createCondition(conditions[1]);
        //                return new OR(left, right);
        //            }
        //            return left;
        //        }
        //    }
        //    return new Empty();
        //}

        //private LogicalExpression createCondition(CodeElementsParser.AndConditionContext context)
        //{
        //    if (context.notCondition() != null)
        //    {
        //        var conditions = context.notCondition();
        //        if (conditions.Length > 0)
        //        {
        //            LogicalExpression left = createCondition(conditions[0]);
        //            if (conditions.Length > 1)
        //            {
        //                LogicalExpression right = createCondition(conditions[1]);
        //                return new AND(left, right);
        //            }
        //            return left;
        //        }
        //    }
        //    return new Empty();
        //}

        //private LogicalExpression createCondition(CodeElementsParser.NotConditionContext context)
        //{
        //    if (context.conditionBase() != null)
        //    {
        //        if (context.NOT() != null)
        //        {
        //            return new NOT(createCondition(context.conditionBase()));
        //        }
        //        else
        //        {
        //            return createCondition(context.conditionBase());
        //        }
        //    }
        //    return new Empty();
        //}

        //private LogicalExpression createCondition(CodeElementsParser.ConditionBaseContext context)
        //{
        //    if (context.simpleCondition() != null)
        //        return createCondition(context.simpleCondition());
        //    if (context.complexCondition() != null)
        //        return createCondition(context.complexCondition());
        //    return new Empty();
        //}

        //private LogicalExpression createCondition(CodeElementsParser.SimpleConditionContext context)
        //{
        //    if (context.classCondition() != null)
        //        return createCondition(context.classCondition());
        //    if (context.conditionNameCondition() != null)
        //        return createCondition(context.conditionNameCondition());
        //    if (context.relationCondition() != null)
        //        return createCondition(context.relationCondition());
        //    if (context.signCondition() != null)
        //        return createCondition(context.signCondition());
        //    if (context.switchStatusCondition() != null)
        //        return createCondition(context.switchStatusCondition());
        //    return new Empty();
        //}

        private LogicalExpression createCondition(CodeElementsParser.ClassConditionContext context)
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

        private LogicalExpression createCondition(CodeElementsParser.ConditionNameConditionOrSwitchStatusConditionContext context) {
            if (context == null) return null;
            return createCondition(context.conditionReference());
        }

        private LogicalExpression createCondition(CodeElementsParser.ConditionReferenceContext context) {
            if (context == null) return null;
            QualifiedName conditionname = SyntaxElementBuilder.CreateQualifiedName(context.qualifiedConditionName());
            IList<Subscript> subscripts = SyntaxElementBuilder.CreateSubscripts(context.subscript());
            return new Condition(conditionname, subscripts);
        }

        private LogicalExpression createCondition(CodeElementsParser.QualifiedConditionNameContext context) {
            if (context == null) return null;
            return new Condition(SyntaxElementBuilder.CreateQualifiedName(context));
        }

        internal char CreateOperator(CodeElementsParser.RelationalOperatorContext context)
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

        private Expression createOperand(CodeElementsParser.OperandContext context) {
            if (context == null) return null;
            if (context.identifier() != null) return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return SyntaxElementBuilder.CreateLiteral(context.literal());
            if (context.arithmeticExpression() != null) return new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression());
            // indexName cannot be distinguished from identifier at the parsing stage
            return null;
        }

        private LogicalExpression createCondition(CodeElementsParser.GeneralRelationConditionContext context) {
            if (context == null)  return null;
            var left = createOperand(context.operand());
            char op = CreateOperator(context.relationalOperator());
            return CreateExpression(left, op, context.abbreviatedExpression());
        }

        /// <summary>
        /// issue #151: this method only manages to return correct results with very simple abbreviated relations.
        /// it returns WRONG results with the majority of what can be written with this horrid syntax.
        /// </summary>
        private LogicalExpression CreateExpression(Expression left, char op, CodeElementsParser.AbbreviatedExpressionContext context) {
            char op2 = '?';
            var operand = createOperand(context.operand());
            if (operand != null) {
                if (context.relationalOperator() != null) {
                    op2 = CreateOperator(context.relationalOperator());
                    return new Relation(left, op2, operand);
                }
                return new Relation(left, op, operand);
            }
            var tail = context.abbreviatedExpression();
            if (tail.Length == 1) {
                var expr  = CreateExpression(left, op, tail[0]);
                if (context.NOT() != null) expr = new NOT(expr);
                return expr;
            }
            if (context.AND() != null) op2 = '&';
            if (context.OR()  != null) op2 = '|';
            if (op2 != '?') {
                var aleft  = CreateExpression(left, op, tail[0]);
                var aright = CreateExpression(left, op, tail[1]);
                return LogicOperation.Create(aleft, op2, aright);
            }
            return null;
        }

        private LogicalExpression createCondition(CodeElementsParser.PointerRelationConditionContext context) {
            var op = createOperator(context.relationConditionEquality());
            var operands = context.specificPointerOperand();
            var left = createOperand(operands[0]);
            var right = createOperand(operands[1]);
            return LogicOperation.Create(left, op, right);
        }

        private char createOperator(CodeElementsParser.RelationConditionEqualityContext context)
        {
            if (context == null) return '?';
            if (context.NOT() != null) return '!';
            if (context.EqualOperator() != null || context.EQUAL() != null) return '=';
            return '?';
        }

        private Expression createOperand(CodeElementsParser.SpecificPointerOperandContext context) {
            if (context == null) return null;

            if (context.identifier() != null) {
                var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());
                if (context.ADDRESS() != null || context.OF() != null) return new Pointer(identifier);
                return identifier;
            }
            if (context.NULL() != null || context.NULLS() != null) return new Null();
            if (context.SELF() != null) return new Self();
            return null;
        }

        private LogicalExpression createCondition(CodeElementsParser.SignConditionContext context) {
            if (context == null) return null;
            Expression operand = createOperand(context.operand());
            bool not = context.NOT() != null;
            if (context.ZERO() != null) return new SignCondition(operand, not, SignCondition.Type.ZERO);
            if (context.POSITIVE() != null) return new SignCondition(operand, not, SignCondition.Type.POSITIVE);
            if (context.NEGATIVE() != null) return new SignCondition(operand, not, SignCondition.Type.NEGATIVE);
            return new SignCondition(operand, not, SignCondition.Type.UNKNOWN);
        }





        //internal LogicOperation CreateOperation(Expression left, char op, CodeElementsParser.AbbreviatedORContext context)
        //{
        //    if (context == null) return null;
        //    LogicOperation operation = null;
        //    int length = context.abbreviatedAND().Length;
        //    if (length > 0) {
        //        operation = createOperation(left, op, context.abbreviatedAND()[0]);
        //        for (int c = 1; c < length; c++) {
        //            LogicOperation current = createOperation(left, op, context.abbreviatedAND()[c]);
        //            operation = new OR(operation, current);
        //        }
        //    }
        //    return operation;
        //}

        //private LogicOperation createOperation(Expression left, char op, CodeElementsParser.AbbreviatedANDContext context)
        //{
        //    if (context == null) return null;
        //    LogicOperation operation = null;
        //    int length = context.abbreviatedNOT().Length;
        //    if (length > 0) {
        //        operation = createOperation(left, op, context.abbreviatedNOT()[0]);
        //        for (int c = 1; c < length; c++) {
        //            LogicOperation current = createOperation(left, op, context.abbreviatedNOT()[c]);
        //            operation = new AND(operation, current);
        //        }
        //    }
        //    return operation;
        //}

        //private LogicOperation createOperation(Expression left, char op, CodeElementsParser.AbbreviatedNOTContext context)
        //{
        //    return createOperation(left, (context.NOT() != null), op, context.abbreviatedExpression());
        //}

        //private LogicOperation createOperation(Expression left, bool not, char op, CodeElementsParser.AbbreviatedExpressionContext context)
        //{
        //    if (context == null) return null;
        //    if (context.abbreviatedOperand() != null ) return createOperand(left, not, op, context.abbreviatedOperand());
        //    if (context.abbreviatedOR() != null) return CreateOperation(left, op, context.abbreviatedOR());
        //    return null;
        //}

        //private Relation createOperand(Expression left, bool not, char op, CodeElementsParser.AbbreviatedOperandContext context)
        //{
        //    if (context.relationalOperator() != null) op = CreateOperator(context.relationalOperator());
        //    if (not) op = invertOperator(op);
        //    Expression right = createOperand(context.operand());
        //    return new Relation(left, op, right);
        //}
    }
}