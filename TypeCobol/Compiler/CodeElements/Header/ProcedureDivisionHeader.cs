using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Functions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The program procedure division consists of optional declaratives, and
    /// procedures that contain sections, paragraphs, sentences, and statements.
    /// </summary>
    public class ProcedureDivisionHeader : CodeElement
    {
        public ProcedureDivisionHeader() : base(CodeElementType.ProcedureDivisionHeader) { }

        /// <summary>
        /// The USING phrase specifies the parameters that a program or method receives
        /// when the program is called or the method is invoked.
        /// Each USING identifier must be defined as a level-01 or level-77 item in the
        /// LINKAGE SECTION of the called subprogram or invoked method.
        /// The argument receiving mode can be : BY REFERENCE or BY VALUE
        /// </summary>
        public IList<InputParameter> UsingParameters { get; set; }

        /// <summary>
        /// The RETURNING phrase specifies a data item that is to receive the program or
        /// method result.
        /// The RETURNING data item must be a level-01 or level-77 item in the LINKAGE SECTION.
        /// The RETURNING data item is an output-only parameter.
        /// </summary>
        public DataName ReturningParameter { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (UsingParameters == null && ReturningParameter == null)
            {
                return base.ToString();
            }
            else
            {
                StringBuilder sb = new StringBuilder(base.ToString());
                if (UsingParameters != null)
                {
                    sb.Append("- InputParameters =");
                    foreach (InputParameter inputParam in UsingParameters)
                    {
                        sb.Append(' ');
                        if (inputParam.ReceivingMode != null)
                        {
                            sb.Append(inputParam.ReceivingMode);
                            sb.Append(':');
                        }
                        sb.Append(inputParam.DataName);
                    }
                    sb.AppendLine();
                }                
                if (ReturningParameter != null)
                {
                    sb.AppendLine("- ReturningDataName = " + ReturningParameter);
                }                
                return sb.ToString();
            }
        }
    }

    /// <summary>
    /// The USING phrase specifies the parameters that a program or method receives
    /// when the program is called or the method is invoked.
    /// </summary>
    public class InputParameter
    {
        /// <summary>
        /// Argument receiving mode : BY REFERENCE or BY VALUE
        /// </summary>
        public SyntaxProperty<ReceivingMode> ReceivingMode { get; set; }

        /// <summary>
        /// Each USING identifier must be defined as a level-01 or level-77 item in the
        /// LINKAGE SECTION of the called subprogram or invoked method.
        /// </summary>
        public DataName DataName { get; set; }

		public InputParameter(DataName name, SyntaxProperty<ReceivingMode> mode) {
			this.DataName = name;
			this.ReceivingMode = mode;
		}
    }

    /// <summary>
    /// Argument receiving mode for InputParameter
    /// </summary>
    public enum ReceivingMode
    {
        /// <summary>
        /// BY REFERENCE (for programs only)
        /// When an argument is passed BY CONTENT or BY REFERENCE, BY
        /// REFERENCE must be specified or implied for the corresponding formal
        /// parameter on the PROCEDURE or ENTRY USING phrase.
        /// BY REFERENCE is the default if neither BY REFERENCE nor BY VALUE is
        /// specified.
        /// If the reference to the corresponding data item in the CALL statement
        /// declares the parameter to be passed BY REFERENCE (explicit or implicit),
        /// the program executes as if each reference to a USING identifier in the
        /// called subprogram is replaced by a reference to the corresponding USING
        /// identifier in the calling program.
        /// If the reference to the corresponding data item in the CALL statement
        /// declares the parameter to be passed BY CONTENT, the value of the item is
        /// moved when the CALL statement is executed and placed into a
        /// system-defined storage item that possesses the attributes declared in the
        /// LINKAGE SECTION for data-name-1. The data description of each
        /// parameter in the BY CONTENT phrase of the CALL statement must be the
        /// same, meaning no conversion or extension or truncation, as the data
        /// description of the corresponding parameter in the USING phrase of the
        /// header.
        /// </summary>
        ByReference,
        /// <summary>
        /// BY VALUE
        /// When an argument is passed BY VALUE, the value of the argument is
        /// passed, not a reference to the sending data item. The receiving subprogram
        /// or method has access only to a temporary copy of the sending data item.
        /// Any modifications made to the formal parameters that correspond to an
        /// argument passed BY VALUE do not affect the argument.
        /// Parameters specified in the USING phrase of a method procedure division
        /// header must be passed to the method BY VALUE. See Passing data in the
        /// Enterprise COBOL Programming Guide for examples that illustrate these
        /// concepts.
        /// </summary>
        ByValue
    }
}
