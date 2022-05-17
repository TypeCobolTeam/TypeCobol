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
        public SqlConstant(Token literal, SqlConstantType type)
        {
            Literal = literal;
            if (type != SqlConstantType.Datetime)
            {
                Type = type;
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
        public DatetimeConstant(Token literal, SqlConstantType type, Token tokenKind) : base(literal,type)
        {
            switch (tokenKind.TokenType)
            {
                case (TokenType.SQL_CURRENT_DATE):
                    Kind = DatetimeConstantKind.Date;
                    break;
                case (TokenType.SQL_CURRENT_TIME):
                    Kind = DatetimeConstantKind.Time;
                    break;
                case (TokenType.SQL_CURRENT_TIMESTAMP):
                    Kind = DatetimeConstantKind.Timestamp;
                    break;
            }
            TokenKind = tokenKind;
        }

        public override SqlConstantType Type => SqlConstantType.Datetime;

        public DatetimeConstantKind Kind { get; } //based on TokenKind.TokenType

        public Token TokenKind { get; } //TokenKind.TokenType = SQL_DATE | SQL_TIME | SQL_TIMESTAMP
    }
}