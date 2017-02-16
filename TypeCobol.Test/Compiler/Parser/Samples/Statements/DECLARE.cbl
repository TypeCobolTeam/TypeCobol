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
* KO: TCRFUN_NO_INOUT_OR_OUTPUT_FOR_FUNCTIONS
DECLARE Function ProcedureINOUT PRIVATE
    IN-OUT  i PIC 9.
DECLARE Function ProcedureOUTPUT PUBLIC
    OUTPUT i PIC 9.
DECLARE Function ProcedureINOUTAndOUTPUT PUBLIC
    INPUT     i PIC 9
    IN-OUT     j PIC 9
    OUTPUT    k PIC 9
    RETURNING l PIC 9.

* OK: standard procedure declarations
DECLARE PROCEDURE   pro PRIVATE
   INPUT    i PIC 9.
DECLARE PROCEDURE   pro PUBLIC
   INPUT    i TYPE Wherever
            j TYPE Whenever
   IN-OUT    k PIC 9
            j TYPE Were
            l TYPE Meant
   OUTPUT   m TYPE ToBe
            n PIC 9
            o TYPE Together.
* KO: TCRFUN_NO_RETURNING_FOR_PROCEDURES
DECLARE PROCEDURE ProcedureRETURNING PRIVATE
    RETURNING i PIC 9.
* KO: cannot know if function or procedure
DECLARE FUNCTION  Unknown PUBLIC
    IN-OUT     b PIC 9
    RETURNING d PIC 9.
DECLARE PROCEDURE Unknown PUBLIC
    OUTPUT    c PIC 9
    RETURNING d PIC 9.
DECLARE           Unknown PUBLIC
    INPUT     a PIC 9
    IN-OUT     b PIC 9
    OUTPUT    c PIC 9
    RETURNING d PIC 9.