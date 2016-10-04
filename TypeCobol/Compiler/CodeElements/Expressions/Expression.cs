﻿using System;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// Base class for all Cobol expression nodes
	/// </summary>
	public abstract class Expression
	{
		public Expression(ExpressionNodeType nodeType) {
			NodeType = nodeType;
		}

		public ExpressionNodeType NodeType { get; private set; }
	}

	public enum ExpressionNodeType
	{
		// Conditional expression
		LogicalOperation,
		ClassCondition,
		ConditionNameConditionOrSwitchStatusCondition,
		RelationCondition,        
		SignCondition,
		ConditionOperand,

		// Arithmetic expression
		ArithmeticOperation,
		NumericVariable
	}

	/// <summary>
	/// Base class for all conditional expression nodes
	/// </summary>
	public abstract class ConditionalExpression : Expression {
		public ConditionalExpression(ExpressionNodeType nodeType) : base(nodeType) { }
	}

	/// <summary>
	/// Complex conditions
	/// A complex condition is formed by combining simple conditions, combined
	/// conditions, or complex conditions with logical operators, or negating those
	/// conditions with logical negation.
	/// </summary>
	public class LogicalOperation : ConditionalExpression
	{
		public LogicalOperation(ConditionalExpression leftOperand, SyntaxProperty<LogicalOperator> logicalOperator, ConditionalExpression rightOperand) :
			base(ExpressionNodeType.LogicalOperation) {
			LeftOperand = leftOperand;
			Operator = logicalOperator;
			RightOperand = rightOperand;
		}

		public ConditionalExpression LeftOperand   { get; private set; }

		public SyntaxProperty<LogicalOperator> Operator { get; private set;  }

		public ConditionalExpression RightOperand  { get; private set; }

		public override string ToString() {
			// reverse polish notation
			return new StringBuilder(LeftOperand != null ? LeftOperand.ToString() : "<?>").Append(" ").Append(RightOperand != null ? RightOperand.ToString() : "<?>").Append(" ").Append(Operator).ToString();
		}
	}

	/// <summary>
	/// Logical operators and their meanings
	/// AND | Logical conjunction | The truth value is true when both conditions are true.
	/// OR | Logical inclusive OR | The truth value is true when either or both conditions are true.
	/// NOT | Logical negation | Reversal of truth value (the truth value is true if the condition is false)
	/// </summary>
	public enum LogicalOperator
	{
		NOT,
		AND,
		OR
	}

	/// <summary>
	///  Class condition
	/// The class condition determines whether the content of a data item is alphabetic,
	/// alphabetic-lower, alphabetic-upper, numeric, DBCS, KANJI, or contains only the
	/// characters in the set of characters specified by the CLASS clause as defined in the
	/// SPECIAL-NAMES paragraph of the ENVIRONMENT DIVISION.
	/// </summary>
	public class ClassCondition : ConditionalExpression
	{
		public ClassCondition(StorageArea dataItem, SymbolReference characterClassNameReference, SyntaxProperty<bool> invertResult) :
			base(ExpressionNodeType.ClassCondition) {
			DataItem = dataItem;
			CharacterClassNameReference = characterClassNameReference;
			InvertResult = invertResult;
		}

		public ClassCondition(StorageArea dataItem, SyntaxProperty<DataItemContentType> dataItemContentType, SyntaxProperty<bool> invertResult) :
			base(ExpressionNodeType.ClassCondition) {
			DataItem = dataItem;
			DataItemContentType = dataItemContentType;
			InvertResult = invertResult;
		}

		public StorageArea DataItem { get; private set; }

		public SymbolReference CharacterClassNameReference { get; private set; }

		public SyntaxProperty<DataItemContentType> DataItemContentType { get; private set; }

		public SyntaxProperty<bool> InvertResult { get; private set; }

		public override string ToString() {
			return new StringBuilder(DataItem.ToString()).Append(InvertResult.Value?" NOT ":" ").Append(CharacterClassNameReference != null ? CharacterClassNameReference.ToString() : DataItemContentType.ToString()).Append(" ?").ToString();
		}
	}

	public enum DataItemContentType
	{
		Numeric,
		Alphabetic,
		AlphabeticLower,
		AlphabeticUpper,
		DBCS,
		Kanji
	}

	/// <summary>
	/// Condition-name condition
	/// A condition-name condition tests a conditional variable to determine whether its
	/// value is equal to any values that are associated with the condition-name.
	/// A condition-name is used in conditions as an abbreviation for the relation
	/// condition. The rules for comparing a conditional variable with a condition-name
	/// value are the same as those specified for relation conditions.
	/// </summary>
	public class ConditionNameConditionOrSwitchStatusCondition : ConditionalExpression
	{
		public ConditionNameConditionOrSwitchStatusCondition(DataOrConditionStorageArea conditionReference) :
			base(ExpressionNodeType.ConditionNameConditionOrSwitchStatusCondition) {
			ConditionReference = conditionReference;
		}

		public DataOrConditionStorageArea ConditionReference { get; private set; }


		public override string ToString() {
			return ConditionReference.ToString();
		}
	}

	/// <summary>
	/// General relation conditions
	/// A general relation condition compares two operands, either of which can be an
	/// identifier, literal, arithmetic expression, or index-name.
	/// </summary>
	public class RelationCondition : ConditionalExpression
	{
		public RelationCondition(ConditionOperand leftOperand, SyntaxProperty<RelationalOperator> relationalOperator, ConditionOperand rightOperand) :
			base(ExpressionNodeType.RelationCondition) {
			LeftOperand = leftOperand;
			Operator = relationalOperator;
			RightOperand = rightOperand;
		}

		public ConditionOperand LeftOperand { get; private set; }

		public SyntaxProperty<RelationalOperator> Operator { get; private set; }

		public ConditionOperand RightOperand { get; private set; }


		public override string ToString() {
			// reverse polish notation
			return new StringBuilder(LeftOperand != null ? LeftOperand.ToString() : "<?>").Append(" ").Append(RightOperand != null ? RightOperand.ToString() : "<?>").Append(" ").Append(Operator).ToString();
		}
	}

	/// <summary>
	/// Relational operators and their meanings
	/// </summary>
	public enum RelationalOperator
	{
		EqualTo,
		GreaterThan,
		GreaterThanOrEqualTo,
		LessThan,
		LessThanOrEqualTo,
		NotEqualTo
	}

	/// <summary>
	/// Sign condition
	/// The sign condition determines whether the algebraic value of a numeric operand is
	/// greater than, less than, or equal to zero.
	/// </summary>
	public class SignCondition : ConditionalExpression
	{
		public SignCondition(ConditionOperand operand, SyntaxProperty<SignComparison> signComparison, SyntaxProperty<bool> invertResult) :
			base(ExpressionNodeType.SignCondition) {
			Operand = operand;
			SignComparison = signComparison;
			InvertResult = invertResult;
		}

		public ConditionOperand Operand { get; private set; }
			
		public SyntaxProperty<SignComparison> SignComparison { get; private set; }

		public SyntaxProperty<bool> InvertResult { get; private set; }

		public override string ToString() {
			var sb = new StringBuilder(Operand.ToString()).Append(" IS ");
			if (InvertResult != null && InvertResult.Value) sb.Append("NOT ");
			sb.Append(SignComparison.Value.ToString()).Append(" ?");
			return sb.ToString();
		}
	}

	/// <summary>
	/// The operand is:
	/// - POSITIVE if its value is greater than zero
	/// - NEGATIVE if its value is less than zero
	/// - ZERO if its value is equal to zero
	/// An unsigned operand is either POSITIVE or ZERO.
	/// </summary>
	public enum SignComparison
	{
		Positive,
		Negative,
		Zero
	}

	/// <summary>
	/// The subject of a general relation condition can be : identifier, literal, function-identifier (already included in identifier), arithmetic expression, or index-name.
	/// dataPointer : ADDRESS OF identifier (already included in identifier) | identifier | NULL | NULLS
	/// programPointer : identifier | NULL | NULLS
	/// objectReference : identifier | SELF | NULL | NULLS
	/// </summary>
	public class ConditionOperand : ConditionalExpression
	{
		public ConditionOperand(ArithmeticExpression arithmeticExpression) :
			base(ExpressionNodeType.ConditionOperand) {
			ArithmeticExpression = arithmeticExpression;
		}

		public ConditionOperand(Variable variable) :
			base(ExpressionNodeType.ConditionOperand) {
			Variable = variable;
		}

		public ConditionOperand(NullPointerValue nullPointerValue) :
			base(ExpressionNodeType.ConditionOperand) {
			NullPointerValue = nullPointerValue;
		}

		public ConditionOperand(Token selfObjectIdentifier) :
			base(ExpressionNodeType.ConditionOperand) {
			SelfObjectIdentifier = selfObjectIdentifier;
		}

		public ArithmeticExpression ArithmeticExpression { get; private set; }

		public Variable Variable { get; private set; }

		public NullPointerValue NullPointerValue { get; private set; }

		public Token SelfObjectIdentifier { get; private set; }

		public override string ToString() {
			if (ArithmeticExpression != null)
				return ArithmeticExpression.ToString();
			if(Variable != null)
				return Variable.ToString();
			if(NullPointerValue != null)
				return NullPointerValue.ToString();
			return SelfObjectIdentifier.ToString();
		}
	}

	/// <summary>
	/// Base class for all arithmetic expression nodes
	/// </summary>
	public abstract class ArithmeticExpression: Expression {
		public ArithmeticExpression(ExpressionNodeType nodeType): base(nodeType) { }
	}

	/// <summary>
	/// When parentheses are not used, or parenthesized expressions are at the same level
	/// of inclusiveness, the following hierarchic order is implied:
	/// 1. Unary operator
	/// 2. Exponentiation
	/// 3. Multiplication and division
	/// 4. Addition and subtraction
	/// </summary>
	public enum ArithmeticOperator
	{
		UnaryPlus = '+',
		UnaryMinus= '-',
		Round   = '°',
		Power   = '^',
		Multiply= '×',
		Divide  = '÷',
		Remainder='/',
		Plus    = '+',
		Minus   = '-',
	}
	static class ArithmeticOperatorExtension {
		public static ArithmeticOperation CreateOperation(this ArithmeticOperator op, ArithmeticExpression left, ArithmeticExpression right = null) {
			return new ArithmeticOperation(left, new SyntaxProperty<ArithmeticOperator>(op, null), right);
		}
	}

	public class ArithmeticOperation: ArithmeticExpression {
		public ArithmeticOperation(ArithmeticExpression left, SyntaxProperty<ArithmeticOperator> op, ArithmeticExpression right = null)
			: base(ExpressionNodeType.ArithmeticOperation) {
			LeftOperand = left;
			Operator = op;
			RightOperand = right;
		}

		public ArithmeticExpression LeftOperand { get; private set; }
		public SyntaxProperty<ArithmeticOperator> Operator { get; private set; }
		public ArithmeticExpression RightOperand { get; private set; }

		/// <summary>reverse polish notation</summary>
		public override string ToString() {
			var str = new StringBuilder();
			if (LeftOperand == null) str.Append('0');
			else str.Append(LeftOperand);
			bool maybeunary =  Operator == null
							|| Operator.Value == ArithmeticOperator.UnaryPlus
							|| Operator.Value == ArithmeticOperator.UnaryMinus
							|| Operator.Value == ArithmeticOperator.Round;
			if (RightOperand == null)
				if (maybeunary) { }
				else str.Append(' ').Append('0');
			else str.Append(' ').Append(RightOperand);
			str.Append(' ').Append((char)Operator.Value);
			return str.ToString();
		}
	}

	/// <summary>
	/// Wrapper to use an integer or numeric variable in an arithmetic expression tree
	/// </summary>
	public class NumericVariableOperand: ArithmeticExpression {
		public NumericVariableOperand(IntegerVariable variable): base(ExpressionNodeType.NumericVariable) { IntegerVariable = variable; }
		public NumericVariableOperand(NumericVariable variable): base(ExpressionNodeType.NumericVariable) { NumericVariable = variable; }

		public IntegerVariable IntegerVariable { get; private set; }
		public NumericVariable NumericVariable { get; private set; }

		public override string ToString() {
			if (IntegerVariable != null) return IntegerVariable.ToString();
			return NumericVariable.ToString();
		}
	}





	/// <summary>TODO</summary>
	public class Literal { }

	/// <summary>TODO</summary>
	public class FigurativeConstant { }
}
