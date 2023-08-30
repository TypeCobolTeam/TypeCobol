using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CupCommon;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// Interface for a Compiler Directive Builder.
    /// </summary>
    public interface ICompilerDirectiveBuilder
    {
        /// <summary>
        /// Start a Basic Compiler Statement
        /// </summary>
        void StartBasisCompilerStatement();
        /// <summary>
        /// Enter a Basic Compiler Statement
        /// </summary>
        /// <param name="textName">The Text Name Token</param>
        void EnterBasisCompilerStatement(TypeCobol.Compiler.Scanner.Token textName);

        /// <summary>
        /// Enter a CONTROL CBL Option.
        /// </summary>
        /// <param name="optionToken"></param>
        void EnterControlCblOption(TypeCobol.Compiler.Scanner.Token optionToken);

        /// <summary>
        /// Start an Asterisk Control or Cbl Compiler Statement option.
        /// </summary>
        /// <param name="type"></param>
        void StartControlCblCompilerStatement(CompilerDirectiveType type);

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
        /// <param name="qualifiedTextName">The Qualified name</param>
        /// <param name="suppress">The Suppress Token if any null otherwise</param>
        /// <param name="replacingOperands">The Replacing operands</param>
        void EnterCopyCompilerStatementBody(QualifiedTextName qualifiedTextName,
            TypeCobol.Compiler.Scanner.Token suppress, PairTokenListList replacingOperands);

        /// <summary>
        /// Start a Delete Compiler directive
        /// </summary>
        void StartDeleteCompilerStatement();

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
        /// Starts the EJECT Compiler Directive
        /// </summary>
        void StartEjectCompilerStatement();

        /// <summary>
        /// Enter an EJECT Compiler Directive
        /// </summary>
        /// <param name="ejectToken"></param>
        void EnterEjectCompilerStatement(TypeCobol.Compiler.Scanner.Token ejectToken);

        /// <summary>
        /// Start an ENTER Compiler directive.
        /// </summary>
        void StartEnterCompilerStatement();

        /// <summary>
        /// Enter an ENTER Compiler directive.
        /// </summary>
        /// <param name="enterToken"></param>
        /// <param name="languageName"></param>
        /// <param name="routineName"></param>
        void EnterEnterCompilerStatement(TypeCobol.Compiler.Scanner.Token enterToken, TypeCobol.Compiler.Scanner.Token languageName, TypeCobol.Compiler.Scanner.Token routineName);

        /// <summary>
        /// Enter an Exec Sql Include statement
        /// </summary>
        /// <param name="execToken">the EXEC token</param>
        void EnterExecSqlIncludeStatement(TypeCobol.Compiler.Scanner.Token execToken);

        /// <summary>
        /// Start an Inser Compiler Directive
        /// </summary>
        void StartInsertCompilerStatement();

        /// <summary>
        /// Enter an Insert Compiler Directive
        /// </summary>
        /// <param name="insertToken"></param>
        /// <param name="sequenceNumber"></param>
        void EnterInsertCompilerStatement(TypeCobol.Compiler.Scanner.Token insertToken, TypeCobol.Compiler.Scanner.Token sequenceNumber);

        /// <summary>
        /// Starts a READY or RESET Compiler directive.
        /// <param name="type">the Type READY or RESET</param>
        /// </summary>
        void StartReadyOrResetTraceCompilerStatement(CompilerDirectiveType type);

        /// <summary>
        /// Enter a READY or RESET Compiler directive.
        /// </summary>
        /// <param name="readyOrResetToken"></param>
        void EnterReadyOrResetTraceCompilerStatement(TypeCobol.Compiler.Scanner.Token readyOrResetToken);

        /// <summary>
        /// Start a replace compiler directive.
        /// </summary>
        /// <param name="type"></param>
        void StartReplaceCompilerStatement(CompilerDirectiveType type);

        /// <summary>
        /// Enter a replace compiler directive.
        /// </summary>
        /// <param name="replaceTokn"></param>
        /// <param name="offToken"></param>
        /// <param name="cupReplacingOperands"></param>
        void EnterReplaceCompilerStatement(TypeCobol.Compiler.Scanner.Token replaceTokn,
            TypeCobol.Compiler.Scanner.Token offToken, CupReplaceOperations cupReplacingOperands);

        /// <summary>
        /// Start a SERVICE LABEL compiler directive.
        /// </summary>
        void StartServiceLabelCompilerStatement();

        /// <summary>
        /// Enter a SERVICE LABEL compiler directive
        /// </summary>
        /// <param name="serviceToken"></param>
        /// <param name="labelToken"></param>
        void EnterServiceLabelCompilerStatement(TypeCobol.Compiler.Scanner.Token serviceToken, TypeCobol.Compiler.Scanner.Token labelToken);

        /// <summary>
        /// Start a SERVICE RELOAD compiler directive.
        /// </summary>
        void StartServiceReloadCompilerStatement();

        /// <summary>
        /// Enter a SERVICE RELOAD compiler directive.
        /// </summary>
        /// <param name="serviceToken"></param>
        /// <param name="reloadToken"></param>
        /// <param name="userDefinedWord"></param>
        void EnterServiceReloadCompilerStatement(TypeCobol.Compiler.Scanner.Token serviceToken,
            TypeCobol.Compiler.Scanner.Token reloadToken, TypeCobol.Compiler.Scanner.Token userDefinedWord);

        /// <summary>
        /// StartSKIP compiler directive
        /// <param name="type">The SKIP token type</param>
        /// </summary>
        void StartSkipCompilerStatement(CompilerDirectiveType type);

        /// <summary>
        /// Enter a SKIP compiler directive.
        /// </summary>
        /// <param name="skipTolen"></param>
        void EnterSkipCompilerStatement(TypeCobol.Compiler.Scanner.Token skipTolen);

        /// <summary>
        /// Start TITLE compiler directive
        /// </summary>
        void StartTitleCompilerStatement();

        /// <summary>
        /// Enter TITLE compiler directive
        /// </summary>
        /// <param name="titleToken"></param>
        /// <param name="title"></param>
        void EnterTitleCompilerStatement(TypeCobol.Compiler.Scanner.Token titleToken, TypeCobol.Compiler.Scanner.Token title);
    }
}
