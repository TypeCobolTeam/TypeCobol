EXEC SQL
  UPDATE TIDCA01
  SET x = :y
    WHERE a = :b
      AND c = :d
      AND e = :f
      AND g = :h
END-EXEC.