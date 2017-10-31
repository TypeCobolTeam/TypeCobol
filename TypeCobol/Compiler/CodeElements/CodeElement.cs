using Antlr4.Runtime;
using System.Collections.Generic;
using System.Text;
using JetBrains.Annotations;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements.
    /// A CodeElement produced during the first parsing phase is also a token consumed by the second parsing phase. 
    /// </summary>
    public abstract partial class CodeElement: IToken, IVisitable
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
        public CodeElementType Type { get; private set; }

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

        public IList<Diagnostic> Diagnostics { get; set; }

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
            var codeElement = obj as CodeElement;
            if (codeElement == null)
                return false;

            return this.Line == codeElement.Line &&
                   this.Type == codeElement.Type &&
                   this.Column == codeElement.Column &&
                   this.StartIndex == codeElement.StartIndex &&
                   this.StopIndex == codeElement.StopIndex &&
                   this.Text == codeElement.Text;
        }

        private bool? _isInsideCopy = null; 

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
            if (_isAcrossSourceFile == null || _isInsideCopy == null)
            {
                CopyDirective firstSource = null; //null = in the main source file

                if (ConsumedTokens != null && ConsumedTokens.Count > 1)
                {
                    //Get CopyDirective of first ConsumedToken
                    var firstConsumedToken = ConsumedTokens[0] as ImportedToken;
                    if (firstConsumedToken != null)
                    {
                        firstSource = firstConsumedToken.CopyDirective;
                    }

                    foreach (var consumedToken in ConsumedTokens)
                    {
                        var it = consumedToken as ImportedToken;
                        CopyDirective copyDirective = it != null ? it.CopyDirective : null;
                        if (copyDirective != firstSource)
                        {
                            _isAcrossSourceFile = true;
                            _isInsideCopy = true;
                            return;
                        }
                    }
                    _isAcrossSourceFile = false;
                    _isInsideCopy = firstSource != null;
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
			var lineStartIndex = line.SequenceNumberText.Length + 1;// +1 for line.IndicatorChar
			return line.SourceText.Substring(0, firstTokenStartIndex-lineStartIndex);
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

       
    }

    // --- Temporary base classes for data definition code elements ---

    public abstract class NamedCodeElement : CodeElement
    {
        protected NamedCodeElement(CodeElementType type) : base(type) { }

        public abstract string Name { get; }
    }

    public interface ITypedCodeElement
    {
        DataType DataType { get; }
        int Length { get; }
    }
}
