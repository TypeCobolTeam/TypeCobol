using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Snapshot of a  MultilineScanState
    /// </summary>
    public class MultilineScanStateSnapshot : MultilineScanState
    {
        public MultilineScanStateSnapshot(MultilineScanState currentState) : base(currentState)
        {
            _AfterExecSql = currentState.AfterExecSql;
            _AfterExec = currentState.AfterExec;
            _AfterExecTranslatorName = currentState.AfterExecTranslatorName;
            _AfterExecStatementText = currentState.AfterExecStatementText;
            _AfterPicture = currentState.AfterPicture;
            _AfterCommentEntryKeyword = currentState.AfterCommentEntryKeyword;
            _AfterCommentEntryKeywordPeriod = currentState.AfterCommentEntryKeywordPeriod;
            _AfterCommentEntry = currentState.AfterCommentEntry;
            _AfterFUNCTION = currentState.AfterFUNCTION;
            _AtBeginningOfSentence = currentState.AtBeginningOfSentence;
        }

        private bool _AfterExecSql;
        public override bool AfterExecSql
        {
            get
            {
                return AfterExecSql;
            }
        }

        private bool _AfterExec;
        public override bool AfterExec
        {
            get
            {
                return _AfterExec;
            }
        }

        private bool _AfterExecTranslatorName;
        public override bool AfterExecTranslatorName
        {
            get
            {
                return _AfterExecTranslatorName;
            }
        }

        private bool _AfterExecStatementText;
        public override bool AfterExecStatementText
        {
            get
            {
                return _AfterExecStatementText;
            }
        }

        private bool _AfterPicture;
        public override bool AfterPicture
        {
            get
            {
                return _AfterPicture;
            }
        }

        private bool _AfterCommentEntryKeyword;
        public override bool AfterCommentEntryKeyword
        {
            get
            {
                return _AfterCommentEntryKeyword;
            }
        }

        private bool _AfterCommentEntryKeywordPeriod;
        public override bool AfterCommentEntryKeywordPeriod
        {
            get
            {
                return _AfterCommentEntryKeywordPeriod;
            }
        }

        private bool _AfterCommentEntry;
        public override bool AfterCommentEntry
        {
            get
            {
                return _AfterCommentEntry;
            }
        }

        private bool _AfterFUNCTION;
        public override bool AfterFUNCTION
        {
            get
            {
                return _AfterFUNCTION;
            }
        }

        private bool _AtBeginningOfSentence;
        public override bool AtBeginningOfSentence
        {
            get
            {
                return _AtBeginningOfSentence;
            }
        }

    }
}
