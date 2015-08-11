SEARCH ALL someIdentifier
  AT END
    SET fatalError    TO TRUE
    SET otherVariable TO TRUE
    PERFORM errorProcedure
  WHEN condition
    MOVE x TO y
END-SEARCH.