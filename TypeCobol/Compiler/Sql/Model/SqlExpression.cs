using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql;
using TypeCobol.Compiler.Sql.Model;

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
        throw new System.NotImplementedException();
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
    public DatetimeConstant(Token literal,DatetimeConstantKind kind, Token tokenKind) : base(literal)
    {
        Kind = kind;
        TokenKind = tokenKind;
    }

    public override SqlConstantType Type => SqlConstantType.Datetime;

    public DatetimeConstantKind Kind { get; } //based on TokenKind.TokenType

    public Token TokenKind { get; } //TokenKind.TokenType = SQL_DATE | SQL_TIME | SQL_TIMESTAMP
}