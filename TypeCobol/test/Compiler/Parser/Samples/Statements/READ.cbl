﻿PERFORM test after UNTIL x = y
READ filename INTO W-PARAM
  AT END
    MOVE y TO x
  NOT AT END
    ADD 1 TO z END-ADD
    MOVE space TO x
END-READ
END-PERFORM