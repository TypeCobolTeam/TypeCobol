using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CupCommon;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// Interface for a Compiler Directive Builder.
    /// </summary>
    public interface ICompilerDirectiveBuilder
    {
        /// <summary>
        /// Enter a Basic Compiler Statement
        /// </summary>
        /// <param name="textName">The Text Name Token</param>
        void EnterBasisCompilerStatement(TypeCobol.Compiler.Scanner.Token textName);

        /// <summary>
        /// Enter an Asterisk Control or Cbl Compiler Statement option.
        /// </summary>
        /// <param name="controlCbl"></param>
        void EnterControlCblCompilerStatement(TypeCobol.Compiler.Scanner.Token controlCbl);

        /// <summary>
        /// Enter a COPY compiler statement.
        /// </summary>
        /// <param name="copy">The COPY token</param>
        void EnterCopyCompilerStatement(TypeCobol.Compiler.Scanner.Token copy);

        /// <summary>
        /// Enters the Body a Copy Statement
        /// </summary>
        /// <param name="isCopy">true if the body is for a Copy, false otherwise (i.e Exec body)</param>
        /// <param name="qualifiedTextName">The Qualified name</param>
        /// <param name="suppress">The Suppress Token if any null otherwise</param>
        /// <param name="replacingOperands">The Replacing operands</param>
        void EnterCopyCompilerStatementBody(bool isCopy, QualifiedTextName qualifiedTextName,
            TypeCobol.Compiler.Scanner.Token suppress, PairTokenListList replacingOperands);

        /// <summary>
        /// Enter a Delete Compiler directive
        /// </summary>
        /// <param name="delToken">The DELETE Token</param>
        void EnterDeleteCompilerStatement(TypeCobol.Compiler.Scanner.Token delToken);

        /// <summary>
        /// Enter a sequence of number field.
        /// </summary>
        /// <param name="seqNumField">The sequence to be entered</param>
        void EnterSequenceNumberField(List<TypeCobol.Compiler.Scanner.Token> seqNumField);

        /// <summary>
        /// Enter an EJECT Compiler Directive
        /// </summary>
        /// <param name="ejectToken"></param>
        void EnterEjectCompilerStatement(TypeCobol.Compiler.Scanner.Token ejectToken);

        /// <summary>
        /// Enter an ENTER Compiler directive.
        /// </summary>
        /// <param name="enterToken"></param>
        /// <param name="languageName"></param>
        /// <param name="routineName"></param>
        void EnterEnterCompilerStatement(TypeCobol.Compiler.Scanner.Token enterToken, TypeCobol.Compiler.Scanner.Token languageName, TypeCobol.Compiler.Scanner.Token routineName);
    }
}
