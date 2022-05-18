using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql;

namespace TypeCobol.Compiler.Sql.Model
{
    public abstract class SqlExpression : SqlObject
    {

    }

    public enum SqlConstantType
    {
        Null,
        Integer,
        FloatingPoint,
        Decimal,
        DecimalFloatingPoint,
        CharacterString,
        BinaryString,
        Datetime,
        GraphicString
    }

    public class SqlConstant : SqlExpression
    {
        public SqlConstant(Token literal)
        {
            Literal = literal;
            switch (literal.TokenType)
            {
                case (TokenType.SQL_NULL):
                    Type = SqlConstantType.Null;
                    break;
                case (TokenType.IntegerLiteral):
                    Type = SqlConstantType.Integer;
                    break;
                case (TokenType.DecimalLiteral):
                    Type = SqlConstantType.Decimal;
                    break;
                case (TokenType.FloatingPointLiteral):
                    Type = SqlConstantType.FloatingPoint;
                    break;
                case (TokenType.SQL_DecimalFloatingPointLiteral):
                    Type = SqlConstantType.DecimalFloatingPoint;
                    break;
                case (TokenType.SQL_BinaryStringLiteral):
                    Type = SqlConstantType.BinaryString;
                    break;
                case (TokenType.SQL_GraphicStringLiteral):
                    Type = SqlConstantType.GraphicString;
                    break;
            }
        }

        public virtual SqlConstantType Type { get; } //based on Literal.TokenType, can be any SqlConstantType except Datetime
     /*
     * Literal.TokenType =   SQL_NULL (sql-specific keyword)
     *                     | IntegerLiteral
     *                     | FloatingPointLiteral
     *                     | DecimalLiteral
     *                     | DecimalFloatingPointLiteral (sql-specific)
     *                     | AlphanumericLiteral / HexadecimalAlphanumericLiteral
     *                     | BinaryStringLiteral (sql-specific)
     *                     | GraphicStringLiteral (sql-specific)
     */

        public Token Literal { get; }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public enum DatetimeConstantKind
    {
        Date,
        Time,
        Timestamp
    }

    public class DatetimeConstant : SqlConstant
    {
        public DatetimeConstant(Token literal, DatetimeConstantKind kind, Token tokenKind) : base(literal)
        {
            TokenKind = tokenKind;
            Kind = kind;

        }

        public override SqlConstantType Type => SqlConstantType.Datetime;

        public DatetimeConstantKind Kind { get; } 

        public Token TokenKind { get; } 
    }
}