using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements.
    /// A CodeElement produced during the first parsing phase is also a token consumed by the second parsing phase. 
    /// </summary>
    public abstract partial class CodeElement: IToken
    {
        public CodeElement(CodeElementType type)
        {
            Type = type;
            SymbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
            Diagnostics = new List<Diagnostic>();
        }

        /// <summary>
        /// The Cobol syntax can be decomposed in 116 elementary code elements
        /// </summary>
        public CodeElementType Type { get; private set; }

        /// <summary>
        /// All significant tokens consumed in the source document to build this code element
        /// </summary>
        public IList<Token> ConsumedTokens { get; set; }

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
        public IList<Diagnostic> Diagnostics { get; private set; }
        
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
			var lineStartIndex = line.SequenceNumberText.Length + (line.IndicatorChar != null? 1:0);
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
                return -1;
                //return ConsumedTokens[0].Line;
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
        public NamedCodeElement(CodeElementType type) : base(type) { }

        public abstract string Name { get; }
    }

    public interface ITypedCodeElement
    {
        DataType DataType { get; }
        int Length { get; }
    }
}
