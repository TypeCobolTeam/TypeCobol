*KO missing INTO
JSON PARSE x.

*OK
JSON PARSE x INTO y.
JSON PARSE x INTO y DETAIL.
JSON PARSE x INTO y WITH DETAIL.

*KO
JSON PARSE x INTO y NAME.
JSON PARSE x INTO y NAME a.
JSON PARSE x INTO y NAME OF a.

*OK
JSON PARSE x INTO y NAME data-item 'newName'.
JSON PARSE x INTO y NAME OF data-item 'newName'.
*OK
JSON PARSE x INTO y NAME OF data-item IS 'newName'.
JSON PARSE x INTO y NAME OF data-item IS OMITTED.
*OK multiple name mappings
JSON PARSE x INTO y NAME OF o1 IS 'n1' o2 'n2' o3 IS 'n3'.

*KO missing data name to suppress
JSON PARSE x INTO y SUPPRESS.

*OK
JSON PARSE x INTO y SUPPRESS t.
*OK multiple suppress directives
JSON PARSE x INTO y SUPPRESS t1 t2 t3.
