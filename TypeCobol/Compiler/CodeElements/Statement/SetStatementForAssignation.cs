using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Statement
{
    class SetStatementForAssignation : SetStatement
    {
        /// <summary>
        /// index-name, identifier(numeric integer item), pointer, procedure-pointer, function-pointer, object reference id
        /// </summary>
        public List<Expression> ReceivingFields { get; set; } 
        /// <summary>
        /// index-name, identifier, positive integer, address of, null, nulls, entry identifier|literal, object reference id, pointer,
        /// procedure-pointer, function-pointer, 
        /// </summary>
        public Expression SendingField { get; set; }            //


        ///Format 1: SET for basic table handling
//        public List<Expression> ReceivingFields { get; set; }   //index-name or identifier(numeric integer item)
//        public Expression SendingField { get; set; }            //index-name or identifier(numeric integer item) or positive integer

        //Format 5: SET for USAGE IS POINTER data items
        //public List<Identifier> ReceivingIdentifiers { get; set; }    //identifier(pointer)
        //public Expression SendingField { get; set; }      //only allow identifier, address of, null, nulls


        //Format 6: SET for procedure-pointer and function-pointer data items
        //public List<Pointer> ReceivingPointers { get; set; }
        //public Expression SendingExpression { get; set; }


        //Format 7: SET for USAGE OBJECT REFERENCE data items
        //        public ObjectReference ReceiverObjectReference { get; set; }
        //        public Expression SendingExpression { get; set; }


        public override string ToString()
        {
            if (ReceivingFields == null && SendingField == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder("Set ");
            if (ReceivingFields != null)
            {
                foreach (Expression receivingField in ReceivingFields)
                {
                    sb.Append(' ');
                    sb.Append(receivingField);
                }
            }
            sb.Append(" TO ");
            if (SendingField != null)
            {
                sb.AppendLine(SendingField.ToString());
            }
            return sb.ToString();
        }
    }
}
