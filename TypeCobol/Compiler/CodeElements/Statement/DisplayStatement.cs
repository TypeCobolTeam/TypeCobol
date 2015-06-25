using System;

namespace TypeCobol.Compiler.CodeElements
{
    public class DisplayStatement : CodeElement
    {
        public DisplayStatement() : base(CodeElementType.DisplayStatement)
        { }


        /// <summary>
        /// / UPON 
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
        public MnemonicForEnvironmentName UponMnemonicForEnvironmentName { get; set; }

        /// <summary>
        /// / UPON 
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
        public EnvironmentName UponEnvironmentName { get; set; }

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
        public SyntaxBoolean IsWithNoAdvancing { get; set; }
    }
}
