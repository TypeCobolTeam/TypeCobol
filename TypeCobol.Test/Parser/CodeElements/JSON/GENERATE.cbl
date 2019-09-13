﻿*KO missing FROM
JSON GENERATE x.

*OK
JSON GENERATE x FROM y.
*OK
JSON GENERATE x FROM y COUNT a.
*OK
JSON GENERATE x FROM y COUNT IN a.

*KO missing json name mapping 
JSON GENERATE x FROM y NAME.
*KO incomplete name mapping
JSON GENERATE x FROM y NAME t.

*OK
JSON GENERATE x FROM y NAME data-item 'newName'.

*KO incomplete name mapping
JSON GENERATE x FROM y NAME OF t.

*OK
JSON GENERATE x FROM y NAME OF data-item 'newName'.
*OK
JSON GENERATE x FROM y NAME OF data-item IS 'newName'.
*OK multiple name mappings
JSON GENERATE x FROM y NAME OF o1 IS 'n1' o2 'n2' o3 IS 'n3'.

*KO missing data name to suppress
JSON GENERATE x FROM y SUPPRESS.

*OK
JSON GENERATE x FROM y SUPPRESS t.
*OK multiple suppress directives
JSON GENERATE x FROM y SUPPRESS t1 t2 t3.
