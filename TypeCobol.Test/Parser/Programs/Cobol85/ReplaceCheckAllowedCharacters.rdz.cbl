       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ReplaceCheckAllowedCharacters.
       DATA DIVISION.
       working-storage section.

       01  Group1.
       replace ==:_:== by ==OK-UNDERSCORE==.
           05 :_:-Var pic X.
       replace ==:>:== by ==OK-GREATER==.
           05 :>:-Var pic X.
       replace ==:=:== by ==OK-EQUAL==.
           05 :=:-Var pic X.
       replace ==:<:== by ==OK-LESS==.
           05 :<:-Var pic X.
       replace ==:;:== by ==OK-SEMICOLON==.
           05 :;:-Var pic X.
       replace ==:/:== by ==OK-SLASH==.
           05 :/:-Var pic X.
       replace ==:.:== by ==OK-DOT==.
           05 :.:-Var pic X.
       replace ==:-:== by ==OK-HYPHEN==.
           05 :-:-Var pic X.
       replace ==:,:== by ==OK-COLON==.
           05 :,:-Var pic X.
       replace ==:+:== by ==OK-PLUS==.
           05 :+:-Var pic X.
       replace ==:*:== by ==OK-STAR==.
           05 :*:-Var pic X.
       replace ==:$:== by ==OK-DOLLAR==.
           05 :$:-Var pic X.
       replace ==: :== by ==OK-SPACE==.
           05 : :-Var pic X.

      *Ok
       replace ==:'':== by ==OK-2-QUOTE==.
           05 :'':-Var pic X.
      *Ok
       replace ==:"":== by ==OK-2-DOUBLEQUOTE==.
           05 :"":-Var pic X.

      *Ok for CobolEditorE-I
      *But NOT OK in IBM because string is not terminated correctly ?
      *An invalid"REPLACE"statement was found.  Scanning was resumed at
      *  the period terminating the"REPLACE"statement.
       replace ==:':== by ==OK-QUOTE==.
           05 :':-Var pic X.
      *Ok for CobolEditorE-I
      *But NOT OK because string is not terminated correctly ?
      *An invalid"REPLACE"statement was found.  Scanning was resumed at
      *  the period terminating the"REPLACE"statement.
       replace ==:":== by ==OK-DOUBLE-QUOTE==.
           05 :":-Var pic X.

      *KO for CobolEditorE-I
       replace ==:):== by ==KO-OPEN-PARENTHESIS==.
           05 :):-Var pic X.
       replace ==:(:== by ==KO-CLOSE-PARENTHESIS==.
           05 :(:-Var pic X.


      *Non-COBOL character"@"was found in column 19.
      *  The character was accepted.
       replace ==:@:== by ==KO-AROBASE==.
      *Non-COBOL character(s) were found starting with"@"in column 16.
      *  The characters were discarded.
           05 :@:-Var pic X.


      *Ko Non-COBOL character"€"was found in column 19.
      *The character was accepted.
       replace ==:€:== by ==KO-EURO==.
           05 :€:-Var pic X.

      *Ko Non-COBOL character"£"was found in column 19.
      *The character was accepted.
       replace ==:£:== by ==KO-GBR==.
           05 :£:-Var pic X.

      *Ko Non-COBOL character"#"was found in column 19.
      *The character was accepted.
       replace ==:#:== by ==KO-SHARP==.
           05 :#:-Var pic X.

      *Ko Non-COBOL character"é"was found in column 19.
      *The character was accepted.
       replace ==:é:== by ==KO-E-ACUTE==.
           05 :é:-Var pic X.

      *Ko Non-COBOL character"&"was found in column 19.
      *The character was accepted.
       replace ==:&:== by ==KO-AND==.
           05 :&:-Var pic X.

      *Ko Non-COBOL character"["was found in column 19.
      *The character was accepted.
       replace ==:[:== by ==KO-OPEN-BRACKET==.
           05 :[:-Var pic X.
      *Ko Non-COBOL character"]"was found in column 19.
      *The character was accepted.
       replace ==:]:== by ==KO-CLOSE-BRACKET==.
           05 :]:-Var pic X.
      *Ko Non-COBOL character"^"was found in column 19.
      *The character was accepted.
       replace ==:^:== by ==KO-CIRCUMFLEX ==.
           05 :^ :-Var pic X.
      *Ko Non-COBOL character"{"was found in column 19.
      *The character was accepted.
       replace ==:{:== by ==KO-OPEN-BRACE==.
           05 :{:-Var pic X.
      *Ko Non-COBOL character"}"was found in column 19.
      *The character was accepted.
       replace ==:}:== by ==KO-CLOSE-BRACE==.
           05 :}:-Var pic X.

       END PROGRAM ReplaceCheckAllowedCharacters.