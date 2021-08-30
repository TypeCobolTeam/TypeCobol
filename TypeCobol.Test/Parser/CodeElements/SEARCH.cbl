SEARCH x
SEARCH ALL x
SEARCH x VARYING y
* ERROR: Use either ALL or VARYING
SEARCH ALL x VARYING y
* ERROR p409: identifier-1 must not be subscripted or reference-modified.
SEARCH identifier1 (ALL)
SEARCH identifier1 (1:1)
SEARCH identifier1 (ALL)(1:1)
* explicit scope
SEARCH ALL someIdentifier
  AT END
    SET fatalError    TO TRUE
    SET otherVariable TO TRUE
    PERFORM errorProcedure
  WHEN condition
    MOVE x TO y
END-SEARCH