000001 REPLACE == TOTO1 TOTO2 TOTO3 == BY == TOTO ==
000002         == TOTO1 TOTO2 == BY == TOTO2 TOTO 3 ==
000003         == TOTO2 TOTO4 == BY == ==
000004         == TOTO2 TOTO3 == BY == TOTO1 TOTO2 TOTO3 ==.
000005 TOTO1 TOTO2 TOTO3.
000006 TOTO1 TOTO2 TOTO4.
000007 TOTO2 TOTO3 TOTO2 TOTO4.
000008 TOTO2 TOTO2 TOTO3.

      *SingleToken -> SingleToken
       REPLACE ==ADD== BY ==SUBTRACT==
               ==SUBTRACT== BY ==ADD==.
      *OK  SUBTRACT 1 FROM var1
           ADD 1 FROM var1.
      
      *PartialWord -> SingleToken
       REPLACE ==:partial-word:== BY ==DISPLAY==
               ==DISPLAY== BY ==MOVE==.
      *OK  DISPLAY var1
           :partial-word: var1.
      
      *SingleToMultipleTokens -> SingleToken
       REPLACE ==ADD== BY ==MOVE 1 TO==
               ==MOVE== BY ==SUBTRACT==.
      *OK  MOVE 1 TO var1
           ADD var1.
      
      *MultipleTokens -> SingleToken
       REPLACE ==MOVE 1 FROM== BY ==DISPLAY==
               ==DISPLAY== BY ==ADD==.
      *OK  DISPLAY var1
           MOVE 1 FROM var1.
      
      *SingleToken -> PartialWord
       REPLACE ==ADD== BY ==:partial-word:==
               ==:partial-word:== BY ==DISPLAY==.
      *KO  :partial-word: var1 is invalid
           ADD var1.
      
      *PartialWord -> PartialWord
       REPLACE ==:partial-word1:== BY ==:partial-word2:==
               ==:partial-word2:== BY ==:partial-word3:==.
      *KO  :partial-word2: var1 is invalid
           :partial-word1: var1.
      
      *SingleToMultipleTokens -> PartialWord
       REPLACE ==ADD== BY ==:partial-word: 1 TO==
               ==:partial-word:== BY ==MOVE==.
      *KO  :partial-word: 1 TO var1 is invalid
           ADD var1.
      
      *MultipleTokens -> PartialWord
       REPLACE ==MOVE 1 FROM== BY ==:partial-word:==
               ==:partial-word:== BY ==DISPLAY==.
      *KO  :partial-word: var1  is invalid
           MOVE 1 FROM var1.
      
      *SingleToken -> SingleToMultipleTokens
       REPLACE ==ADD== BY ==DISPLAY==
               ==DISPLAY== BY ==SUBTRACT 1 TO==.
      *OK  DISPLAY var1
           ADD var1.
      
      *PartialWord -> SingleToMultipleTokens
       REPLACE ==:partial-word:== BY ==DISPLAY==
               ==DISPLAY== BY ==ADD==.
      *OK  DISPLAY var1
           :partial-word: var1.
      
      *SingleToMultipleTokens -> SingleToMultipleTokens
       REPLACE ==ADD== BY ==MOVE 1 TO==
               ==MOVE== BY ==CALL var2. SUBTRACT==.
      *OK  MOVE 1 TO var1
           ADD var1.
      
      *MultipleTokens -> SingleToMultipleTokens
       REPLACE ==MOVE 1 FROM== BY ==DISPLAY==
               ==DISPLAY== BY ==SUBTRACT 1 TO==.
      *OK  DISPLAY var1
           MOVE 1 FROM var1.
      
      *SingleToken -> MultipleTokens
       REPLACE ==SUBTRACT== BY ==MOVE==
               ==MOVE 1 TO== BY ==ADD==.
      *OK  MOVE 1 TO var1
           SUBTRACT 1 TO var1.
      
      *PartialWord -> MultipleTokens
       REPLACE ==:partial-word:== BY ==SUBTRACT==
               ==SUBTRACT 1 FROM== BY ==ADD==.
      *OK  SUBTRACT 1 FROM var1
           :partial-word: 1 FROM var1.
      
      *SingleToMultipleTokens -> MultipleTokens
       REPLACE ==ADD== BY ==MOVE 1 TO==
               ==MOVE 1 TO== BY ==ADD==.
      *OK  MOVE 1 TO var1
           ADD var1.
      
      *MultipleTokens -> MultipleTokens
       REPLACE ==SUBTRACT 1== BY ==MOVE 9==
               ==9 TO== BY ==1 FROM==.
      *OK  MOVE 9 TO var1
           SUBTRACT 1 TO var1.
