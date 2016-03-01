using System.Collections.Generic;
using System.Diagnostics;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    class StatementsBuilder
    {
          //////////////////////
         // ACCEPT STATEMENT //
        //////////////////////

        internal AcceptStatement CreateAcceptStatement(CodeElementsParser.AcceptStatementContext context)
        {
            var statement = new AcceptStatement();
            statement.Receiving = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            statement.Input = SyntaxElementBuilder.CreateMnemonic(context.mnemonicOrEnvironmentName());
            statement.Mode = CreateDateMode(context);
            return statement;
        }

        private DateMode CreateDateMode(CodeElementsParser.AcceptStatementContext context)
        {
            if (context.YYYYMMDD() != null) return DateMode.DATE_YYYYMMDD;
            if (context.DATE() != null) return DateMode.DATE_YYMMDD;
            if (context.YYYYDDD() != null) return DateMode.DAY_YYYYDDD;
            if (context.DAY() != null) return DateMode.DAY_YYDDD;
            if (context.DAY_OF_WEEK() != null) return DateMode.DAY_OF_WEEK;
            if (context.TIME() != null) return DateMode.TIME;
            return DateMode.UNKNOWN;
        }



          ////////////////////
         // CALL STATEMENT //
        ////////////////////

        internal CallStatement CreateCallStatement(CodeElementsParser.CallStatementContext context)
        {
            var statement = new CallStatement();
            statement.Subprogram = CreateProgram(context);
            foreach (var call in context.callBy()) AddCallUsings(call, statement);
            if (context.callReturning() != null)
                statement.Returning = SyntaxElementBuilder.CreateIdentifier(context.callReturning().identifier());
            return statement;
        }

        private CallStatement.Program CreateProgram(CodeElementsParser.CallStatementContext context)
        {
            if (context.identifier() != null)
            {
                var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());
                if (identifier != null) return new CallStatement.Program(identifier);
            }
            if (context.literal() != null)
            {
                var literal = SyntaxElementBuilder.CreateLiteral(context.literal());
                if (literal != null) return new CallStatement.Program(literal);
            }
            if (context.procedurePointer() != null)
            {
                var pointer = SyntaxElementBuilder.CreateDataName(context.procedurePointer().dataName());
                if (pointer != null) return new CallStatement.Program(pointer);
            }
            if (context.functionPointer() != null)
            {
                var pointer = SyntaxElementBuilder.CreateDataName(context.functionPointer().dataName());
                if (pointer != null) return new CallStatement.Program(pointer);
            }
            return null;
        }

        private void AddCallUsings(CodeElementsParser.CallByContext context, CallStatement statement)
        {
            if (context == null) return;
            CallStatement.Using.Mode mode = CreateCallMode(context, statement);
            foreach (var e in context.identifier())
            {
                var identifier = SyntaxElementBuilder.CreateIdentifier(e);
                if (identifier != null) statement.Usings.Add(new CallStatement.Using(mode, identifier));

                if (identifier is FunctionReference) //p306: identifier-2 cannot be a function-identifier.
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", e);
                if (identifier is LinageCounter)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE COUNTER", e);
                if (mode == CallStatement.Using.Mode.REFERENCE && identifier is Length)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", e);

                //TODO ISSUE#183 here can be filenames, too. the following check must then be made:
                //    if (mode == CallStatement.Using.Mode.CONTENT || mode == CallStatement.Using.Mode.VALUE)
                //        DiagnosticUtils.AddError(statement, "CALL .. USING: <filename> only allowed in BY REFERENCE phrase", e);

                //TODO what about special registers ?
            }
            foreach (var e in context.literal())
            {
                var literal = SyntaxElementBuilder.CreateLiteral(e);
                if (literal != null) statement.Usings.Add(new CallStatement.Using(mode, literal));

                if (mode == CallStatement.Using.Mode.REFERENCE)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", e);
            }
            foreach (var e in context.OMITTED())
            {
                var token = TypeCobol.Compiler.AntlrUtils.ParseTreeUtils.GetTokenFromTerminalNode(e);
                statement.Usings.Add(new CallStatement.Using(mode, new Omitted(token)));
                if (mode == CallStatement.Using.Mode.VALUE)
                {
                    var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal OMITTED in BY VALUE phrase", token, rulestack);
                }
            }
        }

        private CallStatement.Using.Mode CreateCallMode(CodeElementsParser.CallByContext context, CallStatement statement)
        {
            if (context.VALUE() != null) return CallStatement.Using.Mode.VALUE;
            if (context.CONTENT() != null) return CallStatement.Using.Mode.CONTENT;
            if (context.REFERENCE() != null) return CallStatement.Using.Mode.REFERENCE;
            if (context.BY() == null) return CallStatement.Using.Mode.REFERENCE;

            // This error is already covered by the grammar ; no use of sending the same error twice
            //var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
            //var token = TypeCobol.Compiler.AntlrUtils.ParseTreeUtils.GetTokenFromTerminalNode(context.BY());
            //DiagnosticUtils.AddError(statement, "CALL .. USING: Required REFERENCE, VALUE, or CONTENT after BY", token, rulestack);
            return CallStatement.Using.Mode.UNKNOWN;
        }



          ////////////////////
         // GOTO STATEMENT //
        ////////////////////

        internal GotoStatement CreateGotoStatement(CodeElementsParser.GotoStatementContext context)
        {
            var statement = new GotoStatement();
            foreach (var procedure in context.procedureName())
            {
                QualifiedProcedureName procedurename = SyntaxElementBuilder.CreateProcedureName(procedure);
                if (procedurename != null) statement.Procedures.Add(procedurename);
            }
            if (context.identifier() != null)
                statement.DependingOn = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (statement.Procedures.Count > 1 && statement.DependingOn == null)
                DiagnosticUtils.AddError(statement, "GO TO: Required only one <procedure name> or DEPENDING phrase", context);
            if (statement.Procedures.Count < 1 && statement.DependingOn != null)
                DiagnosticUtils.AddError(statement, "Conditional GO TO: Required <procedure name>", context);
            if (statement.Procedures.Count > 255)
                DiagnosticUtils.AddError(statement, "Conditional GO TO: Maximum 255 <procedure name> allowed", context);
            return statement;
        }



          //////////////////////////
         // INITIALIZE STATEMENT //
        //////////////////////////

        internal InitializeStatement CreateInitializeStatement(CodeElementsParser.InitializeStatementContext context)
        {
            var statement = new InitializeStatement();
            statement.Receiving = SyntaxElementBuilder.CreateIdentifiers(context.identifier());
            foreach (var sending in context.initializeReplacing())
            {
                var expression = SyntaxElementBuilder.CreateIdentifierOrLiteral(sending.identifierOrLiteral());
                statement.Sending.Add(new InitializeStatement.Replacing(expression, CreateInitializeMode(sending)));
            }
            return statement;
        }

        private InitializeStatement.Replacing.Mode CreateInitializeMode(CodeElementsParser.InitializeReplacingContext context)
        {
            if (context.ALPHABETIC() != null) return InitializeStatement.Replacing.Mode.ALPHABETIC;
            if (context.ALPHANUMERIC() != null) return InitializeStatement.Replacing.Mode.ALPHANUMERIC;
            if (context.ALPHANUMERIC_EDITED() != null) return InitializeStatement.Replacing.Mode.ALPHANUMERIC_EDITED;
            if (context.NATIONAL() != null) return InitializeStatement.Replacing.Mode.NATIONAL;
            if (context.NATIONAL_EDITED() != null) return InitializeStatement.Replacing.Mode.NATIONAL_EDITED;
            if (context.NUMERIC() != null) return InitializeStatement.Replacing.Mode.NUMERIC;
            if (context.NUMERIC_EDITED() != null) return InitializeStatement.Replacing.Mode.NUMERIC_EDITED;
            if (context.DBCS() != null) return InitializeStatement.Replacing.Mode.DBCS;
            if (context.EGCS() != null) return InitializeStatement.Replacing.Mode.EGCS;
            return InitializeStatement.Replacing.Mode.UNKNOWN;
        }



          ///////////////////////
         // INSPECT STATEMENT //
        ///////////////////////

        internal CodeElement CreateInspectStatement(CodeElementsParser.InspectStatementContext context)
        {
            var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());

            // CONVERTING
            if (context.inspectConverting() != null)
                return CreateInspectConverting(context.inspectConverting());

            var statement = new InspectStatement();
            statement.Item = identifier;

            //TALLYING
            foreach (var t in context.inspectTallying())
            {
                var tallying = new InspectStatement.Tallying();
                tallying.Count = SyntaxElementBuilder.CreateIdentifier(t.identifier());
                if (t.inspectCharacters() != null)
                    tallying.CharactersPhrase = CreateInspectSubject(t.inspectCharacters(), statement, false);
                if (t.inspectIdentifiers() != null)
                    tallying.IdentifiersPhrase = CreateInspectIdentifier(t.inspectIdentifiers(), statement, false, false);
                statement.TallyingList.Add(tallying);
            }

            // REPLACING
            foreach (var c in context.inspectCharacters())
                statement.ReplacingCharacters.Add(CreateInspectSubject(c, statement, true));
            foreach (var c in context.inspectIdentifiers())
                statement.ReplacingIdentifiers.Add(CreateInspectIdentifier(c, statement, true, true));
            return statement;
        }

        private InspectConvertingStatement CreateInspectConverting(CodeElementsParser.InspectConvertingContext context)
        {
            var statement = new InspectConvertingStatement();
            if (context.identifierOrLiteral().Length > 0)
            {
                int c = 0;
                foreach (var i in context.identifierOrLiteral())
                {
                    if (c == 0) statement.Replaced = SyntaxElementBuilder.CreateIdentifierOrLiteral(i);
                    if (c == 1) statement.Replacing = SyntaxElementBuilder.CreateIdentifierOrLiteral(i);
                    c++;
                }
            }
            statement.Delimiters = CreateDelimiters(context.inspectPhrase1(), statement);
            return statement;
        }

        private InspectStatement.Subject CreateInspectSubject(CodeElementsParser.InspectByIdentifiersContext context, InspectStatement statement, bool meWantsBy)
        {
            var result = new InspectStatement.Subject();
            result.SubstitutionField = CreateInspectBy(context.inspectBy());
            if (!meWantsBy && result.SubstitutionField != null)
                DiagnosticUtils.AddError(statement, "INSPECT  TALLYING: illegal CHARACTERS BY <identifier> or <literal>", context.inspectBy());
            if (meWantsBy && result.SubstitutionField == null)
                DiagnosticUtils.AddError(statement, "INSPECT REPLACING: Missing CHARACTERS BY <identifier> or <literal>", context);
            result.Delimiters = CreateDelimiters(context.inspectPhrase1(), statement);
            return result;
        }

        private InspectStatement.Subject CreateInspectSubject(CodeElementsParser.InspectCharactersContext context, InspectStatement statement, bool meWantsBy)
        {
            var result = new InspectStatement.Subject();
            result.SubstitutionField = CreateInspectBy(context.inspectBy());
            if (!meWantsBy && result.SubstitutionField != null)
                DiagnosticUtils.AddError(statement, "INSPECT TALLYING: illegal CHARACTERS BY <identifier> or <literal>", context.inspectBy());
            if (meWantsBy && result.SubstitutionField == null)
                DiagnosticUtils.AddError(statement, "INSPECT REPLACING: Missing CHARACTERS BY <identifier> or <literal>", context);
            result.Delimiters = CreateDelimiters(context.inspectPhrase1(), statement);
            return result;
        }

        private InspectStatement.ALF CreateInspectIdentifier(CodeElementsParser.InspectIdentifiersContext context, InspectStatement statement, bool meWantsBy, bool isFirstAllowed)
        {
            var alf = new InspectStatement.ALF();
            alf.All = context.ALL() != null;
            alf.Leading = context.LEADING() != null;
            alf.First = context.FIRST() != null;
            if (alf.First && !isFirstAllowed)
                DiagnosticUtils.AddError(statement, "INSPECT TALLYING: illegal FIRST", context);
            foreach (var c in context.inspectByIdentifiers())
            {
                var sub = CreateInspectSubject(c, statement, meWantsBy);
                if (sub != null) alf.Subjects.Add(sub);
            }
            return alf;
        }

        private IList<Delimiter> CreateDelimiters(IReadOnlyList<CodeElementsParser.InspectPhrase1Context> context, CodeElement e)
        {
            var delimiters = new List<Delimiter>();
            bool seenBefore = false, seenAfter = false;
            foreach (var phrase in context)
            {
                var delimiter = CreateDelimiter(phrase);
                if (delimiter.Before)
                {
                    if (seenBefore) DiagnosticUtils.AddError(e, "INSPECT: Maximum one BEFORE phrase for any one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase", phrase);
                    seenBefore = true;
                }
                if (delimiter.After)
                {
                    if (seenAfter) DiagnosticUtils.AddError(e, "INSPECT: Maximum one AFTER phrase for any one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase", phrase);
                    seenAfter = true;
                }
                delimiters.Add(delimiter);
            }
            return delimiters;
        }

        private Delimiter CreateDelimiter(CodeElementsParser.InspectPhrase1Context context)
        {
            var delimiter = new Delimiter();
            delimiter.Initial = context.INITIAL() != null;
            delimiter.Before = context.BEFORE() != null;
            delimiter.After = context.AFTER() != null;
            delimiter.Item = SyntaxElementBuilder.CreateIdentifierOrLiteral(context.identifierOrLiteral());
            return delimiter;
        }

        private Expression CreateInspectBy(CodeElementsParser.InspectByContext context)
        {
            if (context == null) return null;
            return SyntaxElementBuilder.CreateIdentifierOrLiteral(context.identifierOrLiteral());
        }



          //////////////////////
         // INVOKE STATEMENT //
        //////////////////////

        internal InvokeStatement CreateInvokeStatement(CodeElementsParser.InvokeStatementContext context)
        {
            var statement = new InvokeStatement();

            // object name
            if (context.invokeObject() != null)
            {
                statement.Instance = SyntaxElementBuilder.CreateIdentifier(context.invokeObject().identifier());
                statement.ClassName = SyntaxElementBuilder.CreateClassName(context.invokeObject().className());
                statement.IsSelf = context.invokeObject().SELF() != null;
                statement.IsSuper = context.invokeObject().SUPER() != null;
            }

            // method name
            if (context.NEW() != null)
                statement.MethodName = new New();
            else
            if (context.identifier() != null)
                statement.MethodName = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            else
            if (context.alphanumOrNationalLiteral() != null)
                statement.MethodName = SyntaxElementBuilder.CreateLiteral(context.alphanumOrNationalLiteral());

            // usings
            statement.Usings = new List<Expression>();
            if (context.invokeUsing() != null)
                foreach (var use in context.invokeUsing())
                {
                    foreach (var c in use.literal())
                    {
                        var e = SyntaxElementBuilder.CreateLiteral(c);
                        if (e != null) statement.Usings.Add(e);
                    }
                    foreach (var c in use.identifier())
                    {
                        var e = SyntaxElementBuilder.CreateIdentifier(c);
                        // TODO: "(LENGTH OF)? identifier" only
                        if (e != null) statement.Usings.Add(e);
                    }
                }

            // returning
            if (context.invokeReturning() != null)
            {
                statement.Returning = SyntaxElementBuilder.CreateIdentifier(context.invokeReturning().identifier());
                if (IdentifierUtils.IsReferenceModified(statement.Returning))
                    DiagnosticUtils.AddError(statement, "INVOKE: Illegal <identifier> reference modification", context.invokeReturning().identifier());
            }

            return statement;
        }



          ////////////////////
         // MOVE STATEMENT //
        ////////////////////

        internal MoveStatement CreateMoveStatement(Generated.CodeElementsParser.MoveStatementContext context)
        {
            var sending   = SyntaxElementBuilder.CreateIdentifierOrLiteral(context.identifierOrLiteral());
            var receiving = SyntaxElementBuilder.CreateIdentifiers(context.identifier());
            var statement = new MoveStatement(sending, receiving, context.corresponding() != null);
            if (context.corresponding() != null)
            {
                if (sending is Literal)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: illegal <literal> before TO", context.identifierOrLiteral());
                if (receiving != null && receiving.Count > 1)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: maximum 1 group item after TO", context.identifierOrLiteral());
            }
            Debug.Assert(receiving != null, "receiving != null");
            foreach (var identifier in receiving)
            {
                var function = identifier as FunctionReference;
                if (function != null)
                {
                    var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
                    DiagnosticUtils.AddError(statement, "MOVE: illegal <intrinsic function> after TO", function.Symbol.NameToken, rulestack);
                }
            }
            return statement;
        }



          //////////////////////
         // RETURN STATEMENT //
        //////////////////////

        internal ReturnStatement CreateReturnStatement(Generated.CodeElementsParser.ReturnStatementContext context)
        {
            if (context == null) return null;
            var filename = SyntaxElementBuilder.CreateFileName(context.fileName());
            var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            var statement = new ReturnStatement(filename, identifier);
            return statement;
        }



          //////////////////////
         // SEARCH STATEMENT //
        //////////////////////

        internal SearchStatement CreateSearchStatement(CodeElementsParser.SearchStatementContext context)
        {
            var statement = new SearchStatement();
            statement.All = context.ALL() != null;
            var identifiers = context.identifier();
            if (identifiers != null)
            {
                int c = 0;
                foreach (var identifier in identifiers)
                {
                    if (c == 0) // SEARCH ALL? identifier
                    {
                        statement.Element = SyntaxElementBuilder.CreateIdentifier(identifier);
                        if (IdentifierUtils.IsSubscripted(statement.Element))
                            DiagnosticUtils.AddError(statement, "SEARCH: Illegal subscripted identifier", context);
                        if (IdentifierUtils.IsReferenceModified(statement.Element))
                            DiagnosticUtils.AddError(statement, "SEARCH: Illegal reference-modified identifier", context);
                    } else
                    if (c == 1) // SEARCH ... VARYING identifier
                    {
                        statement.VaryingIdentifier = SyntaxElementBuilder.CreateIdentifier(identifier);
                    } else
                        DiagnosticUtils.AddError(statement, "SEARCH: wtf identifier?", context);
                    c++;
                }
            }
            if (context.indexName() != null) statement.VaryingIndex = SyntaxElementBuilder.CreateIndexName(context.indexName());
            if (statement.All && statement.IsVarying)
                DiagnosticUtils.AddError(statement, "Illegal VARYING after SEARCH ALL", context);
            return statement;
        }



          /////////////////////
         // MERGE STATEMENT //
        /////////////////////

        internal MergeStatement CreateMergeStatement(CodeElementsParser.MergeStatementContext context)
        {
            var statement = new MergeStatement();
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileName());
            statement.Keys = CreateKeyDataItems(context.onAscendingDescendingKey());
            statement.CollatingSequence = SyntaxElementBuilder.CreateAlphabetName(context.alphabetName());
            if (context.usingFilenames() != null)
            {
                statement.Using = SyntaxElementBuilder.CreateFileNames(context.usingFilenames().fileName());
                if (statement.Using.Count == 1)
                    DiagnosticUtils.AddError(statement, "MERGE: USING <filename> <filename>+", context.usingFilenames());
            }
            if (context.givingFilenames() != null) statement.Giving = SyntaxElementBuilder.CreateFileNames(context.givingFilenames().fileName());
            if (context.outputProcedure() != null) statement.Output = SyntaxElementBuilder.CreateProcedureNames(context.outputProcedure().procedureName());
            return statement;
        }

          ////////////////////
         // SORT STATEMENT //
        ////////////////////

        internal SortStatement CreateSortStatement(CodeElementsParser.SortStatementContext context)
        {
            var statement = new SortStatement();
            statement.FileName = SyntaxElementBuilder.CreateFileName(context.fileName());
            statement.Keys = CreateKeyDataItems(context.onAscendingDescendingKey());
            statement.IsDuplicates = context.DUPLICATES() != null// each of these words is only
                                  || context.WITH() != null     // used for DUPLICATES phrase,
                                  || context.IN() != null      // so the presence of any one
                                  || context.ORDER() != null; // shows us the writer's intent
            statement.CollatingSequence = SyntaxElementBuilder.CreateAlphabetName(context.alphabetName());
            if (context.usingFilenames()  != null) statement.Using  = SyntaxElementBuilder.CreateFileNames(context.usingFilenames().fileName());
            if (context.givingFilenames() != null) statement.Giving = SyntaxElementBuilder.CreateFileNames(context.givingFilenames().fileName());
            if (context.inputProcedure()  != null) statement.Input  = SyntaxElementBuilder.CreateProcedureNames(context.inputProcedure().procedureName());
            if (context.outputProcedure() != null) statement.Output = SyntaxElementBuilder.CreateProcedureNames(context.outputProcedure().procedureName());
            return statement;
        }

        private IList<KeyDataItem> CreateKeyDataItems(IReadOnlyList<CodeElementsParser.OnAscendingDescendingKeyContext> context)
        {
            var keys = new List<KeyDataItem>();
            foreach (var key in context)
            {
                KeyDataItem x = CreateKeyDataItem(key);
                if (x != null) keys.Add(x);
            }
            return keys;
        }

        private KeyDataItem CreateKeyDataItem(CodeElementsParser.OnAscendingDescendingKeyContext context)
        {
            if (context == null) return null;
            var key = new KeyDataItem();
            key.IsAscending = context.DESCENDING() == null;
            key.Data = SyntaxElementBuilder.CreateQualifiedNames(context.qualifiedDataName());
            return key;
        }



          ///////////////////
         // USE STATEMENT //
        ///////////////////

        internal UseErrorsStatement CreateUseStatement(CodeElementsParser.UseStatementForExceptionDeclarativeContext context)
        {
            var statement = new UseErrorsStatement();
            if (context.fileName() != null)
            {
                foreach (var file in context.fileName())
                {
                    var filename = SyntaxElementBuilder.CreateFileName(file);
                    if (filename != null) statement.FileNames.Add(filename);
                }
            }
            statement.Mode = CreateOpenMode(context, statement);
            return statement;
        }

        private OpenMode CreateOpenMode(CodeElementsParser.UseStatementForExceptionDeclarativeContext context, UseErrorsStatement statement)
        {
            if (context.INPUT() != null)
            {
                // already covered by grammar
                //if (statement.FileNames.Count > 0) AddError(statement, "INPUT", context, context.INPUT());
                return OpenMode.INPUT;
            }
            if (context.OUTPUT() != null)
            {
                // already covered by grammar
                //if (statement.FileNames.Count > 0) AddError(statement, "OUTPUT", context, context.OUTPUT());
                return OpenMode.OUTPUT;
            }
            if (context.I_O() != null)
            {
                // already covered by grammar
                //if (statement.FileNames.Count > 0) AddError(statement, "I-O", context, context.I_O());
                return OpenMode.IO;
            }
            if (context.EXTEND() != null)
            {
                // already covered by grammar
                //if (statement.FileNames.Count > 0) AddError(statement, "EXTEND", context, context.EXTEND());
                return OpenMode.EXTEND;
            }
            return OpenMode.NONE;
        }
        /*
        private void AddError(UseErrorsStatement statement, string mode, CodeElementsParser.UseStatementForExceptionDeclarativeContext context, Antlr4.Runtime.Tree.ITerminalNode node)
        {
            var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
            var token = TypeCobol.Compiler.AntlrUtils.ParseTreeUtils.GetTokenFromTerminalNode(node);
            DiagnosticUtils.AddError(statement, "USE AFTER ERROR: Illegal <filename list> with "+mode, token, rulestack);
        }
        */
        internal UseDebuggingStatement CreateUseStatement(CodeElementsParser.UseStatementForDebuggingDeclarativeContext context)
        {
            var statement = new UseDebuggingStatement();
            if (context.ALL() != null) statement.AllProcedures = true;
            if (context.procedureName() != null)
            {
                foreach (var procedure in context.procedureName())
                {
                    QualifiedProcedureName procedurename = SyntaxElementBuilder.CreateProcedureName(procedure);
                    if (procedurename != null) statement.Procedures.Add(procedurename);
                }
            }
            // already covered by grammar
            //if (statement.AllProcedures && statement.procedures.Count > 0)
            //{
            //    var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
            //    var token = TypeCobol.Compiler.AntlrUtils.ParseTreeUtils.GetTokenFromTerminalNode(context.ALL());
            //    DiagnosticUtils.AddError(statement, "USE FOR DEBUGGING: Illegal <procedure list> with ALL PROCEDURES", token, rulestack);
            //}
            return statement;
        }



          ////////////////////////////
         // XML GENERATE STATEMENT //
        ////////////////////////////

        internal XmlGenerateStatement CreateXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context)
        {
            var statement = new XmlGenerateStatement();
            statement.Encoding = SyntaxElementBuilder.CreateEncoding(context.codepage());
            int c = 0;
            foreach (var identifier in context.identifier()) // context.identifier().Count is 2 outside of syntax errors
            {
                if (c == 0) statement.Receiving = SyntaxElementBuilder.CreateIdentifier(identifier);
                if (c == 1) statement.Data = SyntaxElementBuilder.CreateIdentifier(identifier);
                c++;
            }
            if (context.xmlCount() != null) statement.Count = SyntaxElementBuilder.CreateIdentifier(context.xmlCount().identifier());
            statement.Encoding = SyntaxElementBuilder.CreateEncoding(context.codepage());
            statement.IsXMLDeclaration = context.XML_DECLARATION() != null;
            statement.IsAttributes = context.ATTRIBUTES() != null;
            if (context.xmlNamespace() != null)
            {
                statement.Namespace = CreateIdentifierOrAlphanumericOrNationalLiteral(
                    context.xmlNamespace().identifier(), context.xmlNamespace().alphanumOrNationalLiteral());
            }
            if (context.xmlNamespacePrefix() != null)
            {
                statement.NamespacePrefix = CreateIdentifierOrAlphanumericOrNationalLiteral(
                    context.xmlNamespacePrefix().identifier(), context.xmlNamespacePrefix().alphanumOrNationalLiteral());
            }
            foreach (var namecontext in context.xmlName())
            {
                try {
                    var x = CreateXmlName(namecontext);
                    if (x != null) statement.Names.Add(x);
                } catch(System.InvalidOperationException ex) {
                    DiagnosticUtils.AddError(statement, "Expected: Alphanumeric or National literal", namecontext);
                }
            }
            foreach (var typecontext in context.xmlType())
            {
                var x = CreateXmlType(typecontext);
                if (x != null) statement.Types.Add(x);
            }
            foreach (var suppresscontext in context.xmlSuppress())
            {
                var x = CreateXmlSuppression(statement, suppresscontext);
                if (x != null) statement.Suppressions.Add(x);
            }
            return statement;
        }

        private static Expression CreateIdentifierOrAlphanumericOrNationalLiteral(CodeElementsParser.IdentifierContext identifier, 
                                                                                  CodeElementsParser.AlphanumOrNationalLiteralContext literal) {
            Expression result = SyntaxElementBuilder.CreateIdentifier(identifier);
            if (result != null) return result;
            return SyntaxElementBuilder.CreateLiteral(literal);
        }

        private XmlGenerateStatement.Name CreateXmlName(CodeElementsParser.XmlNameContext context)
        {
            if (context == null) return null;
            var result = new XmlGenerateStatement.Name();
            result.Old = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            result.New = SyntaxElementBuilder.CreateLiteral(context.alphanumOrNationalLiteral());
            return result;
        }

        private XmlGenerateStatement.Type CreateXmlType(CodeElementsParser.XmlTypeContext context)
        {
            if (context == null) return null;
            var result = new XmlGenerateStatement.Type();
            result.Data = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            if (context.ATTRIBUTE() != null) result.DataType = XmlGenerateStatement.Type.Mode.ATTRIBUTE;
            if (context.ELEMENT()   != null) result.DataType = XmlGenerateStatement.Type.Mode.ELEMENT;
            if (context.CONTENT()   != null) result.DataType = XmlGenerateStatement.Type.Mode.CONTENT;
            return result;
        }

        private XmlGenerateStatement.Suppression CreateXmlSuppression(CodeElement e, CodeElementsParser.XmlSuppressContext context)
        {
            XmlGenerateStatement.Suppression result = new XmlGenerateStatement.Suppression();
            result.Specific = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            result.Generic = CreateXmlSuppressionMode(context.xmlSuppressGeneric());
            result.When = new List<FigurativeConstant>();
            if (context.whenPhrase() != null)
            {
                string[] allowed = { "ZERO", "ZEROES", "ZEROS", "SPACE", "SPACES", "LOW-VALUE", "LOW-VALUES", "HIGH-VALUE", "HIGH-VALUES" };
                foreach (var c in context.whenPhrase().figurativeConstant())
                {
                    var constant = SyntaxElementBuilder.CreateFigurativeConstant(c);
                    if (constant != null)
                    {
                        if (System.Array.IndexOf(allowed, constant.Value) < 0)
                            DiagnosticUtils.AddError(e, "XML GENERATE: Illegal figurative constant " + constant.Value, c);
                        result.When.Add(constant);
                    }
                }
            }
            return result;
        }

        private XmlGenerateStatement.Suppression.Mode CreateXmlSuppressionMode(CodeElementsParser.XmlSuppressGenericContext context)
        {
            if (context == null) return XmlGenerateStatement.Suppression.Mode.UNKNOWN;
            var result = XmlGenerateStatement.Suppression.Mode.UNKNOWN;
            if (context.ATTRIBUTE() != null)
            {
                if (context.NUMERIC() != null) {
                    result = XmlGenerateStatement.Suppression.Mode.NUMERIC_ATTRIBUTE;
                }
                else
                if (context.NONNUMERIC() != null) {
                    result = XmlGenerateStatement.Suppression.Mode.NONNUMERIC_ATTRIBUTE;
                }
                else result = XmlGenerateStatement.Suppression.Mode.ATTRIBUTE;
            }
            if (context.ELEMENT() != null)
            {
                if (context.NUMERIC() != null) {
                    result = XmlGenerateStatement.Suppression.Mode.NUMERIC_ELEMENT;
                }
                else
                if (context.NONNUMERIC() != null) {
                    result = XmlGenerateStatement.Suppression.Mode.NONNUMERIC_ELEMENT;
                }
                else result = XmlGenerateStatement.Suppression.Mode.ELEMENT;
            }
            return result;
        }



          /////////////////////////
         // XML PARSE STATEMENT //
        /////////////////////////

        internal XmlParseStatement CreateXmlParseStatement(CodeElementsParser.XmlParseStatementContext context)
        {
            var statement = new XmlParseStatement();
            int c = 0;
            foreach (var identifier in context.identifier())
            {
                if (c == 0) statement.Identifier = SyntaxElementBuilder.CreateIdentifier(identifier);
                if (c == 1) statement.ValidatingIdentifier = SyntaxElementBuilder.CreateIdentifier(identifier);
                c++;
            }
            statement.Encoding = SyntaxElementBuilder.CreateEncoding(context.codepage());
            statement.IsReturningNational = context.RETURNING() != null;
            statement.ValidatingFile = SyntaxElementBuilder.CreateXmlSchemaName(context.xmlSchemaName());
            foreach (var procedure in context.procedureName())
            {
                var procedurename = SyntaxElementBuilder.CreateProcedureName(procedure);
                if (procedurename != null) statement.Procedures.Add(procedurename);
            }
            return statement;
        }
    }
}
