*Issue #2163, would previously throw NullReferenceException
IF (Var1 NOT SPACE) DISPLAY 'hello' END-IF.
IF (Var1 NOT SPACE OR LOW-VALUE) DISPLAY 'hello' END-IF.