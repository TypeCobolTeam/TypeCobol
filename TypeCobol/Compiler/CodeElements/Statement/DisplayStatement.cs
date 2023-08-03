using System;
using System.Collections.Generic;
using System.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p322: DISPLAY statement
    /// DISPLAY statement transfers the contents of each operand to the output device. 
    /// The contents are displayed on the output device in the order, left to right, in which the operands are listed.
    /// </summary>
    public class DisplayStatement : StatementElement
    {
        public DisplayStatement() : base(CodeElementType.DisplayStatement, StatementType.DisplayStatement)
        { }

        /// <summary>
        /// identifier-1 
        /// Identifier-1 references the data that is to be displayed. 
        /// Identifier-1 can reference any data item except an item of usage PROCEDURE-POINTER, FUNCTION-POINTER, OBJECT REFERENCE, or INDEX. Identifier-1 cannot be an index-name.
        /// If identifier-1 is a binary, internal decimal, or internal floating-point data item, identifier-1 is converted automatically to external format as follows: 
        /// - Binary and internal decimal items are converted to zoned decimal. Negative signed values cause a low-order sign overpunch. 
        /// - Internal floating-point numbers are converted to external floating-point numbers for display such that: 
        ///    – A COMP-1 item will display as if it had an external floating-point PICTURE clause of -.9(8)E-99. 
        ///    – A COMP-2 item will display as if it had an external floating-point PICTURE clause of -.9(17)E-99. 
        /// Data items defined with USAGE POINTER are converted to a zoned decimal number that has an implicit PICTURE clause of PIC 9(10). 
        /// If the output is directed to CONSOLE, data items described with usage NATIONAL are converted from national character representation to EBCDIC. 
        /// The conversion uses the EBCDIC code page that was specified in the CODEPAGE compiler option when the source code was compiled. 
        /// National characters without EBCDIC counterparts are converted to default substitution characters; no exception condition is indicated or raised. 
        /// If the output is not directed to CONSOLE, data items described with usage NATIONAL are written without conversion and without data validation. 
        /// No other categories of data require conversion.
        /// DBCS data items, explicitly or implicitly defined as USAGE DISPLAY-1, are transferred to the sending field of the output device. 
        /// For proper results, the output device must have the capability to recognize DBCS shift-out and shift-in control characters. 
        /// Both DBCS and non-DBCS operands can be specified in a single DISPLAY statement. 
        /// literal-1 
        /// Can be any literal or any figurative constant as specified in “Figurative constants” on page 13. 
        /// When a figurative constant is specified, only a single occurrence of that figurative constant is displayed
        /// </summary>
        public Variable[] Variables { get; set; }
        
        /// <summary>
        /// UPON 
        /// environment-name-1 or the environment name associated with mnemonic-name-1 must be associated with an output device. See “SPECIAL-NAMES paragraph” on page 112. 
        /// A default logical record size is assumed for each device, as follows: 
        /// - The system logical output device 120 characters 
        /// - The system punch device 80 characters
        /// - The console 100 characters
        /// A maximum logical record size is allowed for each device, as follows: 
        /// - The system logical output device 255 characters 
        /// - The system punch device 255 characters 
        /// - The console 100 characters
        /// On the system punch device, the last eight characters are used for PROGRAM-ID name. 
        /// When the UPON phrase is omitted, the system's logical output device is assumed. The list of valid environment-names in a DISPLAY statement is shown in Table 5 on page 114. 
        /// For details on routing DISPLAY output to stdout, see Displaying values on a screen or in a file (
        /// </summary>
        public AmbiguousSymbolReference OutputDeviceName { get; set; }

        /// <summary>
        /// WITH NO ADVANCING 
        /// When specified, the positioning of the output device will not be changed in any way following the display of the last operand.
        /// If the WITH NO ADVANCING phrase is not specified, after the last operand has been transferred to the output device, the positioning of the output device will be reset to the leftmost position of the next line of the device.
        /// Enterprise COBOL does not support output devices that are capable of positioning to a specific character position. See Displaying values on a screen or in a file (DISPLAY) in the Enterprise COBOL Programming Guide for more information about the DISPLAY statement.
        /// The DISPLAY statement transfers the data in the sending field to the output device. The size of the sending field is the total byte count of all operands listed. 
        /// If the output device is capable of receiving data of the same size as the data item being transferred, then the data item is transferred. 
        /// If the output device is not capable of receiving data of the same size as the data item being transferred, then one of the following applies: 
        /// - If the total count is less than the device maximum, the remaining rightmost positions are padded with spaces. 
        /// - If the total count exceeds the maximum, as many records are written as are needed to display all operands. Any operand being printed or displayed when the end of a record is reached is continued in the next record.
        /// ... more details on DBCS operands p324 ...
        /// </summary>
        public SyntaxProperty<bool> WithNoAdvancing { get; set; }

        public bool IsWithNoAdvancing
        {
            get { return WithNoAdvancing != null && WithNoAdvancing.Value; }
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, OutputDeviceName, WithNoAdvancing)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) Variables);
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (Variables == null && WithNoAdvancing == null && OutputDeviceName == null )
            {
                return base.ToString();
            }
            else
            {
                var sb = new StringBuilder(base.ToString());
                if (Variables != null)
                {
                    sb.Append("- variables =");
                    foreach (var varToDisplay in Variables)
                    {
                        sb.Append(' ');
                        sb.Append(varToDisplay);
                    }
                    sb.AppendLine();
                }

                if (OutputDeviceName != null)
                {
                    sb.AppendLine("- OutputDeviceName = " + OutputDeviceName.Name);
                }

                if (WithNoAdvancing != null) sb.AppendLine("- WithNoAdvancing");
                return sb.ToString();
            }
        }
    }
}
