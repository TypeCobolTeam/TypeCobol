using System;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{

    public interface Expression { }

    public class Literal : Expression
    {
        private SyntaxNumber numberValue;
        internal SyntaxString stringValue;
        public bool All = false;

        public Literal(SyntaxNumber numberValue)
        {
            this.numberValue = numberValue;
        }

        public Literal(SyntaxString stringValue)
        {
            this.stringValue = stringValue;
        }

        /// <summary>
        /// Numeric or string value defined by the Token
        /// </summary>
        public object Value
        {
            get
            {
                if (numberValue != null) return numberValue.Value;
                if (stringValue != null) return stringValue.Value;
                throw new InvalidOperationException("Malformed literal");
            }
        }

		public bool IsNumeric {
			get {
				switch (Type.GetTypeCode(Value.GetType())) {
					case TypeCode.Byte:
					case TypeCode.SByte:
					case TypeCode.UInt16:
					case TypeCode.UInt32:
					case TypeCode.UInt64:
					case TypeCode.Int16:
					case TypeCode.Int32:
					case TypeCode.Int64:
					case TypeCode.Decimal:
					case TypeCode.Double:
					case TypeCode.Single:
						return true;
					default:
						return false;
				}
			}
		}

		public override string ToString() {
			return Value.ToString();
		}
    }

    public class FigurativeConstant : Literal
    {
        public FigurativeConstant(SyntaxString value) : base(value) { }
    }

    public class New : Expression { }

}
