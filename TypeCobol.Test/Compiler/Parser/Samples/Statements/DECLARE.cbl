* OK: variable FunctionType
DECLARE           fun PUBLIC.
DECLARE FUNCTION  fun PUBLIC.
DECLARE PROCEDURE pro PRIVATE.
* KO: missing visibility
DECLARE           NoVisibility.
DECLARE FUNCTION  NoVisibility.
DECLARE PROCEDURE NoVisibility.

* OK: standard function declarations
DECLARE FUNCTION   fun PRIVATE
    INPUT     i PIC 9.
DECLARE FUNCTION   fun PUBLIC
    INPUT     i TYPE Wherever
              j PIC 9
              k TYPE Whenever.
DECLARE FUNCTION   fun PUBLIC
    RETURNING k PIC 9.
DECLARE FUNCTION   fun PRIVATE
    INPUT     i PIC 9
              j TYPE Wherever
    RETURNING k TYPE Whenever.
* OK: could be Function or Procedure
DECLARE           FunctionOrProcedure PRIVATE
    INPUT     i PIC 9.
* KO: illegal RETURNING clause in procedure
DECLARE PROCEDURE ProcedureRETURNING PRIVATE
    RETURNING i PIC 9.

* KO: cannot know if function or procedure
DECLARE FUNCTION  Unknown PUBLIC
    INOUT     b PIC 9
    RETURNING d PIC 9.
DECLARE PROCEDURE Unknown PUBLIC
    OUTPUT    c PIC 9
    RETURNING d PIC 9.
DECLARE           Unknown PUBLIC
    INPUT     a PIC 9
    INOUT     b PIC 9
    OUTPUT    c PIC 9
    RETURNING d PIC 9.