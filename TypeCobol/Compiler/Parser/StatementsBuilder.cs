using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    class StatementsBuilder
    {
          ////////////////////
         // CALL STATEMENT //
        ////////////////////

        internal CallStatement CreateCallStatement(CobolCodeElementsParser.CallStatementContext context)
        {
            var statement = new CallStatement();
            statement.Subprogram = CreateProgram(context);
            foreach (var call in context.callBy()) AddCallUsings(call, statement);
            if (context.callReturning() != null)
            {
                statement.Returning = SyntaxElementBuilder.CreateIdentifier(context.callReturning().identifier());
                if (statement.Returning == null)
                    DiagnosticUtils.AddError(statement, "CALL .. RETURNING: Missing identifier", context.callReturning());
            }
            return statement;
        }

        private CallStatement.Program CreateProgram(CobolCodeElementsParser.CallStatementContext context)
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

        private void AddCallUsings(CobolCodeElementsParser.CallByContext context, CallStatement statement)
        {
            if (context == null) return;
            CallStatement.Using.Mode mode = CreateCallMode(context, statement);
            foreach (var e in context.identifier())
            {
                var identifier = SyntaxElementBuilder.CreateIdentifier(e);
                if (identifier != null) statement.Usings.Add(new CallStatement.Using(mode, identifier));

                if (identifier as FunctionReference != null) //p306: identifier-2 cannot be a function-identifier.
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", e);
                if (identifier as LinageCounter != null)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE COUNTER", e);
                if (mode == CallStatement.Using.Mode.REFERENCE && identifier as Length != null)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", e);
                //TODO what about special registers ?
            }
            foreach (var e in context.literal())
            {
                var literal = SyntaxElementBuilder.CreateLiteral(e);
                if (literal != null) statement.Usings.Add(new CallStatement.Using(mode, literal));

                if (mode == CallStatement.Using.Mode.REFERENCE)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", e);
            }
            foreach (var e in context.fileName())
            {
                var filename = SyntaxElementBuilder.CreateFileName(e);
                if (filename != null) statement.Usings.Add(new CallStatement.Using(mode, filename));

                if (mode == CallStatement.Using.Mode.CONTENT || mode == CallStatement.Using.Mode.VALUE)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: <filename> only allowed in BY REFERENCE phrase", e);
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

        private CallStatement.Using.Mode CreateCallMode(CobolCodeElementsParser.CallByContext context, CallStatement statement)
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



          //////////////////////////
         // INITIALIZE STATEMENT //
        //////////////////////////

        internal InitializeStatement CreateInitializeStatement(CobolCodeElementsParser.InitializeStatementContext context)
        {
            var statement = new InitializeStatement();
            statement.Receiving = SyntaxElementBuilder.CreateIdentifiers(context.identifier());
            foreach (var sending in context.initializeReplacing())
            {
                var expression = SyntaxElementBuilder.CreateIdentifierOrLiteral(sending.identifierOrLiteral());
                statement.Sending.Add(new InitializeStatement.Replacing(expression));
            }
            return statement;
        }



          ////////////////////
         // MOVE STATEMENT //
        ////////////////////

        internal MoveStatement CreateMoveStatement(Generated.CobolCodeElementsParser.MoveStatementContext context)
        {
            var sending   = SyntaxElementBuilder.CreateIdentifierOrLiteral(context.identifierOrLiteral());
            var receiving = SyntaxElementBuilder.CreateIdentifiers(context.identifier());
            var statement = new MoveStatement(sending, receiving, context.corresponding() != null);
            if (context.corresponding() != null)
            {
                if (sending as Literal != null)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: illegal <literal> before TO", context.identifierOrLiteral());
                if (receiving != null && receiving.Count > 1)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: maximum 1 group item after TO", context.identifierOrLiteral());
            }
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

        internal ReturnStatement CreateReturnStatement(Generated.CobolCodeElementsParser.ReturnStatementContext context)
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

        internal SearchStatement CreateSearchStatement(CobolCodeElementsParser.SearchStatementContext context)
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

    }
}
