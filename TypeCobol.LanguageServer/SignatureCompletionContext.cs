using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Keeps track of current signature completion context.
    /// 
    /// Candidates for signature completion are selected explicitly using a "textDocument/signatureHelp" request
    /// or implicitly during completion request after a CALL keyword.
    /// The best match for signature completion is either selected directly by the client when multiple
    /// procedures are available or automatically set when a single procedure is available.
    /// Finally the best match procedure is used to provide results when completion is requested
    /// after INPUT or IN-OUT or OUTPUT TypeCobol keywords.
    /// </summary>
    public class SignatureCompletionContext
    {
        public Dictionary<SignatureInformation, FunctionDeclaration> Candidates { get; }

        public FunctionDeclaration BestMatch { get; set; }

        public SignatureCompletionContext()
        {
            Candidates = new Dictionary<SignatureInformation, FunctionDeclaration>();
            BestMatch = null;
        }
    }
}
