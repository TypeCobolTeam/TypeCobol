using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements.
    /// A CodeElement produced during the first parsing phase is also a token consumed by the second parsing phase. 
    /// </summary>
    public abstract partial class CodeElement: IToken, IVisitable, IEquatable<CodeElement>
    {
        protected CodeElement(CodeElementType type)
        {
            Type = type;
            ConsumedTokens = new List<Token>();
            SymbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
        }

        /// <summary>
        /// The Cobol syntax can be decomposed in 116 elementary code elements
        /// </summary>
        public CodeElementType Type { get; }

        /// <summary>
        /// Describe how the CodeElement is debugged
        /// Takes in account if CodeElement is spanned across lines
        /// </summary>
        public DebugType DebugMode
        {
            get
            {
                if (!_debugMode.HasValue) ComputeDebugMode();
                Debug.Assert(_debugMode.HasValue);
                return _debugMode.Value;

                void ComputeDebugMode()
                {
                    var consumedTokensCount = ConsumedTokens.Count;
                    if (consumedTokensCount == 0)
                    {
                        // CodeElement is invalid, we assume DebugMode is None but it could be wrong
                        _debugMode = DebugType.None;
                        return;
                    }

                    bool atLeastOneDebug = false, atLeastOneWithoutDebug = false;
                    if (ConsumedTokens.First().Line == ConsumedTokens.Last().Line)
                    {
                        // CodeElement is on one line
                        atLeastOneDebug = ConsumedTokens[0].TokensLine.Type == CobolTextLineType.Debug;
                        atLeastOneWithoutDebug = !atLeastOneDebug;
                    }
                    else
                    {
                        // CodeElement is on multiple lines
                        for (var i = 0; i < consumedTokensCount; i++)
                        {
                            var isDebugType = ConsumedTokens[i].TokensLine.Type == CobolTextLineType.Debug;
                            atLeastOneDebug |= isDebugType;
                            atLeastOneWithoutDebug |= !isDebugType;
                            if (atLeastOneDebug && atLeastOneWithoutDebug)
                            {
                                break;
                            }
                        }
                    }

                    if (atLeastOneDebug && !atLeastOneWithoutDebug)
                    {
                        // Only debug lines
                        _debugMode = DebugType.All;
                    }
                    else if (!atLeastOneDebug && atLeastOneWithoutDebug)
                    {
                        // Only not debug lines
                        _debugMode = DebugType.None;
                    }
                    else
                    {
                        // Some debug lines and some not debug lines
                        _debugMode = DebugType.Mix;
                    }
                }
            }
        }

        private DebugType? _debugMode;

        private IList<Token> _consumedTokens;
        /// <summary>
        /// All significant tokens consumed in the source document to build this code element
        /// </summary>
        public IList<Token> ConsumedTokens
        {
            get { return this._consumedTokens; }
            set {
                this._consumedTokens = value;
                ResetLazyProperties();
            } 
        }

        /// <summary>
        /// Is the token is a UserDefinedWord or a literal, it could be a symbol definition or a symbol reference.
        /// Keywords can also be symbol references (special registers).
        /// This property enables to retrieve symbol information attached to this token at a later stage.
        /// </summary>
        public IDictionary<Token,SymbolInformation> SymbolInformationForTokens { get; set; }
        
        /// <summary>
        /// Storage area definitions (explicit data definitions AND compiler generated storage area allocations)
        /// </summary>
        public IDictionary<SymbolDefinition,DataDescriptionEntry> StorageAreaDefinitions { get; set; }

        /// <summary>
        /// List of storage areas read from by this CodeElement
        /// </summary>
        public IList<StorageArea> StorageAreaReads { get; set; }

        /// <summary>
        /// List of storage areas written to by this CodeElement
        /// </summary>
        public IList<ReceivingStorageArea> StorageAreaWrites { get; set; }

        /// <summary>
        /// Impacts which we will need to resolve at the next stage between two group items
        /// because of MOVE CORRESPONDING, ADD CORRESPONDING, and SUBTRACT CORRESPONDING statements
        /// </summary>
        public GroupCorrespondingImpact StorageAreaGroupsCorrespondingImpact { get; set; }

        /// <summary>
        /// Program, method, or function entry point which can be targeted by call instructions (with shared storage areas)
        /// </summary>
        public CallTarget CallTarget { get; set; }

        /// <summary>
        /// List of program, method, or function call instructions (with shared sotrage areas)
        /// </summary>
        public IList<CallSite> CallSites { get; set; }

        /// <summary>
        /// List of errors found on this CodeElement
        /// </summary>

        public List<Diagnostic> Diagnostics { get; set; }

        /// <summary>
        /// Return true if code element has diagnostics.
        /// </summary>
        public bool IsInError
        {
            get { return Diagnostics != null && Diagnostics.Count > 0; }
        }


        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            bool continueVisit = astVisitor.BeginCodeElement(this) && VisitCodeElement(astVisitor);
            astVisitor.EndCodeElement(this);
            return continueVisit;
        }

        public virtual bool VisitCodeElement(IASTVisitor astVisitor) {
            var continueVisit = this.ContinueVisitToChildren(astVisitor, CallTarget, StorageAreaGroupsCorrespondingImpact)
                                && this.ContinueVisitToChildren(astVisitor, CallSites, ConsumedTokens, StorageAreaReads, StorageAreaWrites);
            if (continueVisit && StorageAreaDefinitions != null)
            {
                continueVisit = this.ContinueVisitToChildren(astVisitor, StorageAreaDefinitions.Keys,
                                                                            StorageAreaDefinitions.Values);
            }
            if (astVisitor.IsSymbolInformationForTokensEnabled && continueVisit && SymbolInformationForTokens != null)
            {
                continueVisit = this.ContinueVisitToChildren(astVisitor, SymbolInformationForTokens.Keys,
                                                                         SymbolInformationForTokens.Values);
            }
            return continueVisit;
        }

        /// <summary>
        /// Apply propperties of the current CodeElement to the specified one.
        /// </summary>
        /// <param name="ce"></param>
        public void ApplyPropertiesToCE([NotNull] CodeElement ce)
        {
            ce.ConsumedTokens = this.ConsumedTokens;
            ce.Diagnostics = this.Diagnostics;
            ce.SymbolInformationForTokens = this.SymbolInformationForTokens;
            ce.StorageAreaReads = this.StorageAreaReads;
        }
        
        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder("[[");
            sb.Append(Type);
            sb.Append("]] ");
            if (ConsumedTokens == null || ConsumedTokens.Count < 1) {
                sb.Append("No Consumed Tokens").AppendLine();
            } else {
                sb.Append(ConsumedTokens[0]).ToString();
                sb.Append(" --> ");
                sb.Append(ConsumedTokens[ConsumedTokens.Count - 1].ToString());
                bool displayLineNumbers = false;
                if (displayLineNumbers) {
                    int first = ConsumedTokens[0].Line;
                    int last  = ConsumedTokens[ConsumedTokens.Count-1].Line;
                    sb.Append(" on lines ");
                    sb.Append(first);
                    sb.Append(">");
                    sb.Append(last);
                }
            }
            sb.AppendLine(); //TODO: is the newline really necessary here ? ToString returns shouldn't end with a newline, should they ?
            return sb.ToString();
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as CodeElement);
        }

        public bool Equals(CodeElement codeElement)
        {
            if (Object.ReferenceEquals(this, codeElement)) return true;
            if (Object.ReferenceEquals(null, codeElement)) return false;

            if (ConsumedTokens != null && codeElement.ConsumedTokens != null)
            {
                if (ConsumedTokens.Count != codeElement.ConsumedTokens.Count)
                {
                    return false;
                }

                if (ConsumedTokens.Count > 0 && codeElement.ConsumedTokens.Count > 0)
                {
                    return Type == codeElement.Type && ConsumedTokens[0].Equals(codeElement.ConsumedTokens[0]);
                }

                // ConsumedTokens collections are both empty
                Debug.Fail("CodeElement.Equals: cannot compare 2 CodeElements having both no Consumed Tokens.");
            }

            return false;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = 13;
                hashCode = (hashCode * 397) ^ Type.GetHashCode();
                if (ConsumedTokens != null)
                {
                    var consumedTokensHashCode = ConsumedTokens.Count > 0
                        ? ConsumedTokens[0].GetHashCode()
                        : ConsumedTokens.GetHashCode();
                    hashCode = (hashCode * 397) ^ consumedTokensHashCode;
                }

                return hashCode;
            }
        }

        private bool? _isInsideCopy = null; 
        public CopyDirective FirstCopyDirective { get; private set; }

        /// <summary>
        /// Return true if this CodeElement is inside a COPY
        /// </summary>
        /// <returns></returns>
        public bool IsInsideCopy() {
            CalculateIsAcrossSourceFile();
            return _isInsideCopy.Value;
        }

        private bool? _isAcrossSourceFile = null;

        private void CalculateIsAcrossSourceFile() {
            if (_isAcrossSourceFile == null || _isInsideCopy == null) {
                FirstCopyDirective = null;
                CopyDirective firstSource = null; //null = in the main source file

                if (ConsumedTokens != null && ConsumedTokens.Count > 0)
                {
                    //Get CopyDirective of first ConsumedToken
                    var firstConsumedToken = ConsumedTokens[0] as ImportedToken;
                    if (firstConsumedToken != null)
                    {
                        firstSource = firstConsumedToken.CopyDirective;
                    }

                    foreach (var consumedToken in ConsumedTokens)
                    {
                        CopyDirective copyDirective = null;
                        switch (consumedToken)
                        {
                            case MissingToken _:
                            case ReplacedToken _:
                                continue;
                            case ImportedToken importedToken:
                                copyDirective = importedToken.CopyDirective;
                                break;
                        }

                        if (copyDirective != firstSource)
                        {
                            _isAcrossSourceFile = true;
                            _isInsideCopy = true;
                            FirstCopyDirective = copyDirective ?? firstSource;
                            return;
                        }
                    }
                    _isAcrossSourceFile = false;
                    _isInsideCopy = firstSource != null;
                    FirstCopyDirective = firstSource;
                } else {
                    _isInsideCopy = false;
                    _isAcrossSourceFile = false;
                }
            }
        }


        /// <summary>
        /// Return true if this CodeElement is across 2 source file.
        /// Eg: 
        ///  - in a program source file and a copy
        ///  - In 2 different copy
        /// </summary>
        /// <returns></returns>
        public bool IsAcrossSourceFile() {
            CalculateIsAcrossSourceFile();
            return _isAcrossSourceFile.Value;
        }


        protected void ResetLazyProperties() {
            _isInsideCopy = null;
            _isAcrossSourceFile = null;
        }

        public string SourceText {
            get {
                var str = new StringBuilder();
                ITokensLine previous = null;
                int end = -1;
                foreach(var token in ConsumedTokens) {
                    var line = token.TokensLine;
                    string whitespace = "";
                    if (previous == null) { // first line
                        whitespace = GetIndent(line, token.StartIndex);
                    } else
                    if (previous == line) { // same line
                        whitespace = line.Text.Substring(end, token.StartIndex-end);
                    } else { // new line
                        str.AppendLine();
                        whitespace = GetIndent(line, token.StartIndex);
                    }
                    previous = line;
                    string text = line.Text.Substring(token.StartIndex, token.Length);
                    str.Append(whitespace+text);
                    end = token.StartIndex + token.Length;
                }
                return str.ToString();
            }
        }

        private string GetIndent(ITokensLine line, int firstTokenStartIndex) {
            var lineStartIndex = line.Indicator.StartIndex + 1;
            return new string(' ',  firstTokenStartIndex - lineStartIndex);
        }

        // --- Antlr4.Runtime.IToken implementation ---
        // ... used by the ProgramClassParser  ...

        public string Text
        {
            get
            {
                StringBuilder sb = new StringBuilder();
                foreach(Token elementToken in ConsumedTokens)
                {
                    sb.Append(elementToken.Text);
                }
                return sb.ToString();
            }
        }

        int IToken.Type
        {
            get
            {
                return (int)Type;
            }
        }

        public int Line
        {
            get
            {
                if (ConsumedTokens.Count < 1) return -1;
                return ConsumedTokens[0].Line;
            }
        }

        public int LineEnd
        {
            get
            {
                if (ConsumedTokens.Count < 1) return -1;
                return ConsumedTokens[ConsumedTokens.Count - 1].Line;
            }
        }

        public int Channel
        {
            get
            {
                return Token.CHANNEL_SourceTokens;
            }
        }

        public int Column {
            get {
                if (ConsumedTokens.Count < 1) return -1;// ISSUE #204
                return ConsumedTokens[0].Column;
            }
        }

        public int TokenIndex {
            get {
                if (ConsumedTokens.Count < 1) return -1;
                return ConsumedTokens[0].TokenIndex;
            }
        }

        public int StartIndex {
            get {
                if (ConsumedTokens.Count < 1) return -1;
                return ConsumedTokens[0].StartIndex;
            }
        }

        public int StopIndex {
            get {
                if (ConsumedTokens.Count < 1) return -1;// ISSUE #204
                return ConsumedTokens[ConsumedTokens.Count-1].StopIndex;
            }
        }

        public ITokenSource TokenSource {
            get {
                if (ConsumedTokens.Count < 1) return null;
                return ConsumedTokens[0].TokenSource;
            }
        }

        public ICharStream InputStream {
            get {
                if (ConsumedTokens.Count < 1) return null;
                return ConsumedTokens[0].InputStream;
            }
        }

        /// <summary>
        /// Get the line number in the main source for this CodeElement.
        /// If it belongs to an imported COPY, the line is the one declaring the COPY directive in the main source
        /// </summary>
        /// <returns>The line number</returns>
        public int GetLineInMainSource()
        {
            if (ConsumedTokens.Count > 0)
            {
                var firstToken = ConsumedTokens[0];
                if (firstToken is ImportedToken importedToken)
                {
                    return importedToken.CopyDirective.TextNameSymbol.TokensLine.LineIndex;
                }

                return firstToken.TokensLine.LineIndex;
            }

            return Line;
        }

        /// <summary>
        /// Describe how a CodeElement debugging is set
        /// </summary>
        public enum DebugType
        {
            /// <summary>
            /// Contain no debugging at all
            /// </summary>
            None,
            /// <summary>
            /// Contain some elements in debug, some without debug
            /// </summary>
            Mix,
            /// <summary>
            /// Contain only elements in debug
            /// </summary>
            All
        }
    }

    public interface INamedCodeElement
    {
        string Name { get; }
    }

    public interface ITypedCodeElement
    {
        DataType DataType { get; }
    }
}
